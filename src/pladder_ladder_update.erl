%%%-------------------------------------------------------------------
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 15. Jul 2017 12:55
%%%-------------------------------------------------------------------
-module(pladder_ladder_update).


-behaviour(gen_server).

%% API
-export([start_link/2,
         update_ladder/1,
         name/1,
         set_workers/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER(Ladder), list_to_atom(lists:concat([?MODULE, "_", Ladder]))).

-include("pladder.hrl").

-record(state, {
    rest_pid :: pid(),
    ladder :: string(),
    stream_ref :: reference() | undefined,
    offset = 0 :: integer(),
    temp_data = <<>> :: binary(),
    timer_ref :: timer:tref(),
    last_rest_call :: calendar:datetime() | undefined,
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
-spec(start_link(Ladder :: string(), Workers :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Ladder, Workers) ->
    gen_server:start_link({local, ?SERVER(Ladder)}, ?MODULE,
                          [Ladder, Workers], []).

-spec update_ladder(Ladder :: string()) -> ok.
update_ladder(Ladder) ->
    gen_server:cast(?SERVER(Ladder), {update_ladder, Ladder}).

-spec name(Ladder :: string()) -> atom().
name(Ladder) ->
    ?SERVER(Ladder).

-spec set_workers(Ladder :: string(), Workers :: integer()) -> ok.
set_workers(Ladder, Workers) ->
    gen_server:call(?SERVER(Ladder), {set_workers, Workers}, 20000).

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
init([Ladder, Workers]) ->
    {ok, RestPid} = gun:open(?RestURL, ?RestPort, #{protocols=>[http]}),
    {ok, Tref} = timer:apply_interval(?StartUpdateTimer, ?MODULE,
                                      update_ladder, [Ladder]),
    %% Start updating ladder directly
    update_ladder(Ladder),
    {ok, #state{rest_pid=RestPid, ladder=Ladder, timer_ref=Tref,
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
handle_call({set_workers, Workers}, _From, State) ->
    {reply, ok, State#state{workers=Workers}};
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
handle_cast({update_ladder, Ladder},
            #state{ladder=Ladder, offset=0=Offset,
                   rest_pid=RestPid}=State) ->
    Path = pladder_util:build_path(Ladder, ?Limit, Offset),
    lager:debug("Rest call for more entries for ~p with path ~p~n",
               [Ladder, Path]),
    StreamRef = gun:get(RestPid, Path),
    TempData = <<>>,
    Now = calendar:now_to_datetime(erlang:timestamp()),
    {noreply, State#state{offset=Offset, stream_ref=StreamRef,
                          temp_data=TempData, last_rest_call=Now}};
handle_cast({update_ladder, Ladder},
            #state{ladder=Ladder, offset=Offset,
                rest_pid=RestPid, last_rest_call=undefined}=State) ->
    Path = pladder_util:build_path(Ladder, ?Limit, Offset),
    lager:debug("Rest call for more entries for ~p with path ~p~n",
               [Ladder, Path]),
    StreamRef = gun:get(RestPid, Path),
    TempData = <<>>,
    Now = calendar:now_to_datetime(erlang:timestamp()),
    {noreply, State#state{offset=Offset, stream_ref=StreamRef,
                          temp_data=TempData, last_rest_call=Now}};
handle_cast({update_ladder, Ladder},
            #state{ladder=Ladder, offset=Offset,
                   rest_pid=RestPid, last_rest_call=LastRestCall}=State) ->
    Now = calendar:now_to_datetime(erlang:timestamp()),
    Diff =
    try
        NowSec = calendar:datetime_to_gregorian_seconds(Now),
        LastSec = calendar:datetime_to_gregorian_seconds(LastRestCall),
        NowSec - LastSec
    catch
        _:Exception ->
            lager:error("Exception caught(~p) in handle_cast update_ladder"
                        " for ~p~n", [Exception, Ladder]),
            0
    end,
    if
        Diff > 60 ->
            lager:warning("Last rest call for ~p was ~p seconds ago, "
                          "resting offset and updating~n", [Ladder, Diff]),
            Path = pladder_util:build_path(Ladder, ?Limit, Offset),
            lager:debug("Rest call for more entries for ~p with path ~p~n",
                       [Ladder, Path]),
            StreamRef = gun:get(RestPid, Path),
            TempData = <<>>,
            {noreply, State#state{offset=Offset, stream_ref=StreamRef,
                                  temp_data=TempData, last_rest_call=Now}};
        true ->
            lager:warning("Last rest call for ~p was ~p seconds ago, "
                          "skipping this update cycle~n", [Ladder, Diff]),
            {noreply, State}
    end;
handle_cast({update_ladder, Ladder}, #state{}=State) ->
         lager:error("Trying to update ~p which isn't in this state:~p~n",
                     [Ladder, State]),
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
handle_info({gun_up, _RestPid, _Protocol},
            #state{ladder=Ladder}=State) ->
    lager:debug("Rest for ~p online~n", [Ladder]),
    {noreply, State};

handle_info({gun_down, _RestPid, _Protocol, _Reason,
             _KilledStream, _UnProcessedStreams},
            #state{ladder=Ladder}=State) ->
    lager:debug("Rest for ~p offline~n", [Ladder]),
    {noreply, State};

handle_info({gun_response, RestPid, _StreamRef, nofin, 200, _Headers},
            #state{rest_pid=RestPid}=State) ->
    {noreply, State};
handle_info({gun_response, RestPid, _StreamRef, nofin, Status, Headers},
            #state{rest_pid=RestPid, ladder=Ladder}=State) ->
    lager:error("Received no end response for ~p with status(~p) "
                "and headers:~p~n",
                [Ladder, Status, Headers]),
    {noreply, State};
handle_info({gun_response, RestPid, _StreamRef, fin, 200, _Headers},
            #state{rest_pid=RestPid}=State) ->
    {noreply, State};
handle_info({gun_response, RestPid, _StreamRef, fin, Status, Headers},
            #state{rest_pid=RestPid, ladder=Ladder}=State) ->
    lager:error("Received end response for ~p with status(~p) "
                "and headers:~p~n",
                [Ladder, Status, Headers]),
    {noreply, State};

handle_info({gun_data, RestPid, StreamRef, nofin, Data},
            #state{rest_pid=RestPid,
                   stream_ref=StreamRef, temp_data=TempData}=State) ->
    UpdatedTempData = <<TempData/binary, Data/binary>>,
    {noreply, State#state{temp_data=UpdatedTempData}};
handle_info({gun_data, RestPid, StreamRef, nofin, _Data},
            #state{rest_pid=RestPid, ladder=Ladder}=State) ->
    lager:error("No streamref(~p) found for current ladder:~p~n",
                [StreamRef, Ladder]),
    {noreply, State};
handle_info({gun_data, RestPid, StreamRef, fin, Data},
            #state{rest_pid=RestPid, ladder=Ladder, stream_ref=StreamRef,
                   offset=Offset, temp_data=TempData, workers=Workers}=State) ->
    NewTempData = <<>>,
    UpdatedTempData = <<TempData/binary, Data/binary>>,
    try
        DecodedData = jiffy:decode(UpdatedTempData, [return_maps]),
        %%lager:info("Updating ~p for ~p offset~n", [Ladder, Offset]),
        Total = pladder_util:update_ladder(DecodedData, Ladder, Workers),
        %%lager:info("~p updated for ~p offset~n", [Ladder, Offset]),
        NewOffset = pladder_util:update_offset(Offset, Total),
        if
            NewOffset == 0 ->
                %% Do nothing but wait for next update cycle
                lager:debug("Update cycle for ~p done, waiting for next~n", [Ladder]),
                {noreply, State#state{offset=NewOffset, temp_data=NewTempData,
                                      stream_ref=undefined,
                                      last_rest_call=undefined}};
            true ->
                Path = pladder_util:build_path(Ladder, ?Limit, NewOffset),
                lager:debug("Rest call for more entries for ~p with path ~p~n",
                           [Ladder, Path]),
                NewStreamRef = gun:get(RestPid, Path),
                Now = calendar:now_to_datetime(erlang:timestamp()),
                {noreply, State#state{stream_ref=NewStreamRef, offset=NewOffset,
                                      temp_data=NewTempData,
                                      last_rest_call=Now}}
        end
    catch %% Most likely POE servers are down
        _:Exception  ->
            lager:error("Exception thrown:~p~n", [Exception]),
            lager:warning("Resting offset to 0 as Exception occured "
                          "at receiving rest data for ~p~n", [Ladder]),
            {noreply, State#state{stream_ref=undefined, offset=0,
                                  temp_data=NewTempData,
                                  last_rest_call=undefined}}
    end;


handle_info({gun_data, ConnPid, _StreamRef, IsFin, Data}=_Msg,
            #state{ladder=Ladder}=State) ->
    lager:error("Unhandled gun_data for ~p recieved for pid:~p finish state:~p "
                "and data:~n~p~n", [Ladder, ConnPid, IsFin, Data]),
    {noreply, State};

handle_info({gun_error, RestPid, StreamRef, {closed, Reason}},
            #state{rest_pid=RestPid, ladder=Ladder, offset=Offset,
                   stream_ref=StreamRef}=State) ->
    lager:error("Connection closed for streamref:~p "
                "belonging to ~p at ~p offset with reason:~p~n",
                [StreamRef, Ladder, Offset, Reason]),
    lager:warning("Current update cycle for ~p canceled, waiting on next~n",
                  [Ladder]),
    NewTempData = <<>>,
    {noreply, State#state{temp_data=NewTempData, offset=0,
                          last_rest_call=undefined,
                          stream_ref=undefined}};
handle_info({gun_error, RestPid, StreamRef, {closed, Reason}},
            #state{rest_pid=RestPid, ladder=Ladder}=State) ->
    lager:error("Connection closed for unknown streamref:~p "
                 "belonging to ~p with reason:~p~n",
                 [StreamRef, Ladder, Reason]),
    {noreply, State};

handle_info(Info, #state{ladder=Ladder}=State) ->
    lager:error("Unhandled info for ~p received:~p~n", [Ladder, Info]),
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
terminate(Reason, #state{ladder=Ladder, rest_pid=RestPid,
                          timer_ref=Tref, stream_ref=StreamRef}) ->
    timer:cancel(Tref),
    case StreamRef of
        undefined -> ok;
        StreamRef -> gun:cancel(RestPid, StreamRef)
    end,
    gun:close(RestPid),
    lager:warning("Shuting down ~p for ~p with reason:~p~n",
                  [?MODULE, Ladder, Reason]),
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
