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
         get_by_char_name/1,
         get_by_acc_name/1,
         get_by_char_class/1,
         get_stats/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("pladder.hrl").

-record(state, {
    rest_pid :: pid(),
    ladder :: string(),
    offset :: integer(),
    stream_ref :: reference(),
    temp_data = <<>> :: binary()
}).

-define(RestURL, "api.pathofexile.com").
-define(Ladders, "/ladders").
-define(RestPort, 80).
-define(Limit, 200).
-define(StartUpdateTimer, 60000).

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

get_by_char_name(CharacterName) ->
    CharacterNameLowerCase = string:to_lower(CharacterName),
    F = fun() ->
        mnesia:index_read(ladder_entry, CharacterNameLowerCase,
                          #ladder_entry.character_name_lower_case)
        end,
    mnesia:transaction(F).

get_by_acc_name(AccountName) ->
    AccountNameLowerCase = string:to_lower(AccountName),
    F = fun() ->
        mnesia:index_read(ladder_entry, AccountNameLowerCase,
                          #ladder_entry.account_name_lower_case)
        end,
    mnesia:transaction(F).

get_by_char_class(ClassName) ->
    F = fun() ->
        mnesia:index_read(ladder_entry, ClassName,
                          #ladder_entry.character_class)
        end,
    mnesia:transaction(F).

get_stats() ->
    F = fun() ->
        mnesia:foldl(
            fun(#ladder_entry{dead=IsDead, online=IsOnline},
                #ladder_stats{dead=Dead, alive=Alive, online=Online}) ->
                case {IsDead, IsOnline} of
                    {true, true} ->
                        #ladder_stats{dead=Dead+1, alive=Alive,
                                      online=Online+1};
                    {true, false} ->
                        #ladder_stats{dead=Dead+1, alive=Alive,
                                      online=Online};
                    {false, true} ->
                        #ladder_stats{dead=Dead, alive=Alive+1,
                                      online=Online+1};
                    {false, false} ->
                        #ladder_stats{dead=Dead+1, alive=Alive,
                                      online=Online}
                end
            end,
            #ladder_stats{},
            ladder_entry)
        end,
    mnesia:transaction(F).


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
    application:ensure_all_started(gun),
    mnesia:create_schema([node()]),
    mnesia:start(),
    case mnesia:create_table(ladder_entry,
                             [{attributes,
                               record_info(fields, ladder_entry)},
                              {disc_copies, [node()]},
                              {index, [character_name_lower_case,
                                       account_name_lower_case,
                                       rank, character_class]}]) of
        {aborted, {already_exists, ladder_entry}} ->
            {ok, RestPid} = gun:open(?RestURL, ?RestPort, #{protocols=>[http]}),
            Offset = 0,
            Ladder = "2 Week Mayhem HC Solo (JRE093)",
            Path = build_path(Ladder, ?Limit, Offset),
            io:format("path:~p~n", [Path]),
            io:format("pids self:~p rest:~p~n", [self(), RestPid]),
            StreamRef = gun:get(RestPid, Path),
            {ok, #state{rest_pid=RestPid, offset=Offset, ladder=Ladder,
                        stream_ref=StreamRef}};
        {aborted, Reason} ->
            io:format("Mnesia table failed to create with reason:~p~n",
                      [Reason]),
            {stop, Reason};
        _ ->
            {ok, RestPid} = gun:open(?RestURL, ?RestPort, #{protocols=>[http]}),
            Offset = 0,
            Ladder = "2 Week Mayhem HC Solo (JRE093)",
            Path = build_path(Ladder, ?Limit, Offset),
            io:format("path:~p~n", [Path]),
            io:format("pids self:~p rest:~p~n", [self(), RestPid]),
            StreamRef = gun:get(RestPid, Path),
            {ok, #state{rest_pid=RestPid, offset=Offset, ladder=Ladder,
                        stream_ref=StreamRef}}
    end.

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

handle_info({gun_up, RestPid, _Protocol},
            #state{rest_pid=RestPid}=State) ->
    io:format("Rest up~n", []),
    {noreply, State};
handle_info({gun_up, _ConnPid, _Protocol}, State)  ->
    io:format("test~n", []),
    {noreply, State};

handle_info({gun_down, RestPid, _Protocol, _Reason,
             _KilledStream, _UnProcessedStreams},
            #state{rest_pid=RestPid}=State) ->
    io:format("Rest down:~n", []),
    {noreply, State};
handle_info({gun_down, _ConnPid, _Protocol, _Reason,
             _KilledStreams, _UnProcessedStreams}=_Msg, State) ->
    io:format("test~n", []),
    {noreply, State};

handle_info({gun_response, RestPid, _StreamRef, nofin, _Status, _Headers},
            #state{rest_pid=RestPid}=State) ->
    %%io:format("Received no end response with status(~p) and headers:~p~n",
    %%          [Status, Headers]),
    {noreply, State};
handle_info({gun_response, RestPid, _StreamRef, fin, _Status, _Headers},
            #state{rest_pid=RestPid}=State) ->
    %%io:format("Received end response with status(~p) and headers:~p~n",
    %%          [Status, Headers]),
    {noreply, State};
handle_info({gun_response, _ConnPid, _StreamRef,
             _IsFin, _Status, _Headers}=_Msg,
            #state{}=State) ->
    io:format("test~n", []),
    {noreply, State};

handle_info({gun_data, RestPid, StreamRef, nofin, Data},
            #state{rest_pid=RestPid, stream_ref=StreamRef,
                   temp_data=TempData}=State) ->
    %%io:format("Received data:~p~n", [Data]),
    UpdatedTempData = <<TempData/binary, Data/binary>>,
    {noreply, State#state{temp_data=UpdatedTempData}};
handle_info({gun_data, RestPid, StreamRef, fin, Data},
            #state{rest_pid=RestPid, stream_ref=StreamRef,
                temp_data=TempData, offset=Offset, ladder=Ladder}=State) ->
    %%io:format("Received data:~p~n", [Data]),
    UpdatedTempData = <<TempData/binary, Data/binary>>,
    DecodedData = jiffy:decode(UpdatedTempData, [return_maps]),
    Total = update_ladder(DecodedData),
    NewOffset = update_offset(Offset, Total),
    NewTempData = <<>>,
    if
        NewOffset == 0 ->
            timer:send_after(?StartUpdateTimer, {start_update}),
            {noreply, State#state{offset=NewOffset, stream_ref=undefined,
                temp_data=NewTempData}};
        true ->
            Path = build_path(Ladder, ?Limit, NewOffset),
            NewStreamRef = gun:get(RestPid, Path),
            {noreply, State#state{offset=NewOffset, stream_ref=NewStreamRef,
                                  temp_data=NewTempData}}
    end;
handle_info({gun_data, _ConnPid, _StreamRef, _IsFin, _Data}=_Msg, State) ->
    io:format("test~n", []),
    {noreply, State};

handle_info({start_update}, #state{ladder=Ladder, offset=Offset,
                                   rest_pid=RestPid}=State) ->
    Path = build_path(Ladder, ?Limit, Offset),
    StreamRef = gun:get(RestPid, Path),
    TempData = <<>>,
    {noreply, State#state{stream_ref=StreamRef, temp_data=TempData}};

handle_info(_Info, State) ->
    io:format("test~n", []),
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
terminate(_Reason, _State) ->
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
build_path(Ladder, Limit, Offset) ->
    EncodedLadder = http_uri:encode(Ladder),
    lists:concat([?Ladders, "/", EncodedLadder, "?", "limit=", Limit,
                  "&", "offset=", Offset, "&track=true"]).
update_ladder(#{<<"total">> := Total, <<"entries">> := Entries}) ->
    update_entry(Entries),
    Total;
update_ladder(LadderEntries) ->
    io:format("~p~n", [LadderEntries]),
    ok.

update_entry([]) ->
    ok;
update_entry([#{<<"rank">> := Rank, <<"dead">> := Dead, <<"online">> := Online,
                <<"character">> := #{<<"name">> := CharacterName,
                                     <<"id">> := Id,
                                     <<"level">> := CharacterLevel,
                                     <<"experience">> := CharacterExp,
                                     <<"class">> := CharacterClass},
                <<"account">> := #{<<"name">> := AccountName}=AccountInfo}
                 |Rest]) ->
    TwitchName =
    case AccountInfo of
        #{<<"twitch">> := #{<<"name">> := AccountTwitchName}} ->
            bitstring_to_list(AccountTwitchName);
        _ ->
            ""
    end,
    Now = calendar:local_time(),
    LadderEntry =
    #ladder_entry{
        id=bitstring_to_list(Id),
        rank=Rank,
        dead=Dead,
        online=Online,
        character_class=bitstring_to_list(CharacterClass),
        character_exp=CharacterExp,
        character_level=CharacterLevel,
        character_name=bitstring_to_list(CharacterName),
        character_name_lower_case=string:to_lower(
            bitstring_to_list(CharacterName)),
        account_name=bitstring_to_list(AccountName),
        account_name_lower_case=string:to_lower(bitstring_to_list(AccountName)),
        twitch_name=TwitchName,
        last_update=Now
    },
    ExpPerHour =
    case get_old_entry(LadderEntry, Dead) of
        {atomic, []} ->
            "0 XPH";
        {atomic, [#ladder_entry{last_update=LastUpdate,
                                character_exp=OldCharacterExp}]} ->
            NowSeconds = calendar:datetime_to_gregorian_seconds(Now),
            LastUpdateSeconds =
            calendar:datetime_to_gregorian_seconds(LastUpdate),
            UpdatedDiffSeconds = NowSeconds - LastUpdateSeconds,
            ExpDiff = CharacterExp - OldCharacterExp,
            NewExpPerHour = trunc(ExpDiff * (3600 / UpdatedDiffSeconds)),
            round_exp(NewExpPerHour);
        _ ->
            "0 XPH"
    end,
    F = fun() ->
        mnesia:write(LadderEntry#ladder_entry{exp_per_hour=ExpPerHour})
        end,
    case mnesia:transaction(F) of
        {aborted, Reason} ->
            io:format("Transaction aborted with reason:~p~n",[Reason]);
        {atomic, _} ->
            ok
    end,
    update_entry(Rest).

update_offset(Offset, Total) ->
    if
        (Offset + ?Limit) >= Total ->
            0;
        true ->
            Offset + ?Limit
    end.

get_old_entry(_Entry, true) ->
    [];
get_old_entry(Entry, false) ->
    get_entry(Entry).

get_entry(#ladder_entry{id=Id}) ->
    F = fun() ->
        mnesia:read({ladder_entry, Id})
        end,
    mnesia:transaction(F).

round_exp(ExpPerHour) ->
    if
        ExpPerHour < 1000000 ->
            lists:concat([trunc(ExpPerHour / 1000), "K XPH"]);
        true ->
            lists:concat([trunc(ExpPerHour / 1000000), "M XPH"])
    end.