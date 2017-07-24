%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2017 15:21
%%%-------------------------------------------------------------------
-module(pladder).

-behaviour(gen_server).

%% API
-export([start_link/0,
         check_on_ladders/0,
         clean_ladders/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([clean_ladder/1,
         get_update_ladders/0]).

-define(SERVER, ?MODULE).

-include("pladder.hrl").

-record(state, {
    check_on_ladders_timer :: timer:tref(),
    clean_ladders_timer :: timer:tref(),
    workers :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec check_on_ladders() -> ok.
check_on_ladders() ->
    gen_server:cast(?MODULE, {check_on_ladders}).

-spec clean_ladders() -> ok.
clean_ladders() ->
    gen_server:cast(?MODULE, {clean_ladders}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    Workers = application:get_env(pladder, workers, 100),
    init_ladders(Workers),
    {ok, CheckOnLaddersTref} = timer:apply_interval(?CheckOnLaddersTimer,
                                                    ?MODULE,
                                                    check_on_ladders, []),
    {ok, CleanLaddersTref} = timer:apply_interval(?CleanLaddersTimer, ?MODULE,
                                                  clean_ladders, []),
    {ok, #state{check_on_ladders_timer=CheckOnLaddersTref,
                clean_ladders_timer=CleanLaddersTref,
                workers=Workers}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{},
                      timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(),
                      NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({check_on_ladders}, #state{workers=Workers}=State) ->
    DBLadders = sumo:find_by(ladders, []),
    F = fun(#{active := Active}) -> Active end,
    {ActiveDBLadders, InactiveDBLadders} = lists:splitwith(F, DBLadders),
    ActiveLadders = [ sumo_utils:to_list(Ladder) ||
        #{id := Ladder} <- ActiveDBLadders],
    InactiveLadders = [ sumo_utils:to_list(Ladder) ||
        #{id := Ladder} <- InactiveDBLadders],
    Ladders = get_update_ladders(),
    stop_ladders(InactiveLadders, Ladders),
    WorkersPerLadder = workers_per_ladder(Workers, length(ActiveLadders)),
    start_ladders(ActiveLadders, Ladders, WorkersPerLadder),
    {noreply, State};

handle_cast({clean_ladders}, State) ->
    Ladders = get_update_ladders(),
    clean_ladders(Ladders),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(Info, State) ->
    lager:error("[~p] Unhandled info msg received:~p~n", [?SERVER, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(Reason, #state{check_on_ladders_timer=CheckOnLaddersTimer,
                          clean_ladders_timer=CleanLaddersTimer}) ->
    lager:warning("[~p] Shuting down with reason:~p~n", [?SERVER, Reason]),
    Ladders = get_update_ladders(),
    stop_ladders(Ladders, Ladders),
    timer:cancel(CheckOnLaddersTimer),
    timer:cancel(CleanLaddersTimer),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_ladders(Workers) ->
    ActiveLadders = sumo:find_by(ladders, [{active, 1}]),
    Ladders = [ sumo_utils:to_list(Id) || #{id := Id} <- ActiveLadders],
    WorkersPerLadder = workers_per_ladder(Workers, length(Ladders)),
    lager:info("[~p] Initating ladders:~p~n", [?SERVER, Ladders]),
    start_ladders(Ladders, [], WorkersPerLadder).

workers_per_ladder(_Workers, 0) ->
    0;
workers_per_ladder(Workers, Ladders) ->
    trunc(Workers / Ladders).

start_ladders([], _OnlineLadders, _WorkersPerLadder) ->
    ok;
start_ladders([Ladder|Rest], OnlineLadders, WorkersPerLadder) ->
    case lists:member(Ladder, OnlineLadders) of
        false ->
            start_ladder(Ladder, WorkersPerLadder);
        true ->
            pladder_ladder_update:set_workers(Ladder, WorkersPerLadder),
            ok
    end,
    start_ladders(Rest, OnlineLadders, WorkersPerLadder).

start_ladder(Ladder, WorkersPerLadder) ->
    lager:info("[~p] Starting ladder_update for ~p~n", [?SERVER, Ladder]),
    case pladder_update_sup:start_ladder_update(Ladder, WorkersPerLadder) of
        {error, Reason} ->
            lager:error("[~p] Unable to start ladder_update for ~p "
                        "with reason:~p~n", [?SERVER, Ladder, Reason]),
            pladder_ladder_update:set_workers(Ladder, WorkersPerLadder);
        _ ->
            ok
    end.

stop_ladders([], _OnlineLadders) ->
    ok;
stop_ladders([Ladder|Rest], OnlineLadders) ->
    case lists:member(Ladder, OnlineLadders) of
        false ->
            ok;
        true ->
            stop_ladder(Ladder)
    end,
    stop_ladders(Rest, OnlineLadders).

stop_ladder(Ladder) ->
    lager:info("[~p] Stopping ladder_update for ~p~n", [?SERVER, Ladder]),
    case pladder_update_sup:stop_ladder_update(Ladder) of
        {error, Reason} ->
            lager:error("[~p] Unable to stop ladder_update for ~p "
                        "with reason:~p~n", [?SERVER, Ladder, Reason]);
        _ ->
            ok
    end.

clean_ladders([]) ->
    ok;
clean_ladders([Ladder|Rest]) ->
    clean_ladder(Ladder),
    clean_ladders(Rest).

clean_ladder(Ladder) ->
    Now = calendar:datetime_to_gregorian_seconds(
        calendar:now_to_datetime(erlang:timestamp())),
    OldDate = calendar:gregorian_seconds_to_datetime(Now - (3600*2)),
    try
        Conditions = {'and', [{ladder, Ladder}, {rank, '<', 15001},
                              {last_updated, '<', OldDate}]},
        Total = sumo:count_by(characters, {ladder, Ladder}),
        Characters = sumo:find_by(characters, Conditions),
        lists:foreach(
            fun(Character) ->
                sumo:persist(characters, Character#{rank => Total + 1})
            end,
            Characters)
    catch
        _:Exception  ->
            lager:error("[~p] Exception caught while cleaning ~p exception:~p~n",
                        [?SERVER, Ladder, Exception])
    end.

get_update_ladders() ->
    OnlineLadders = supervisor:which_children(pladder_update_sup),
    F =
    fun({pladder_worker_pool, _, _, _}, Ladders) ->
        Ladders;
       ({L, _, _, _}, Ladders) ->
           "pladder_ladder_update_" ++ Ladder = atom_to_list(L),
           Ladders ++ [Ladder]
    end,
    lists:foldl(F, [], OnlineLadders).