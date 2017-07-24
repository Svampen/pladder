%%%-------------------------------------------------------------------
%% @doc pladder public API
%% @end
%%%-------------------------------------------------------------------

-module(pladder_app).

-behaviour(application).

%% Application callbacks
-export([start/0,
         stop/0,
         start/2,
         stop/1,
         start_phase/3]).

%%====================================================================
%% API
%%====================================================================
start() ->
    application:ensure_all_started(pladder).
start(_StartType, _StartArgs) ->
    ok = sumo:create_schema(),
    pladder_sup:start_link().

%%--------------------------------------------------------------------
stop() ->
    application:stop(pladder).

stop(_State) ->
    ok.

-spec start_phase(atom(), StartType::application:start_type(), []) ->
    ok | {error, _}.
start_phase(start_cowboy_listeners, _StartType, []) ->
    Port = application:get_env(pladder, http_port, 8383),
    ListenerCount = application:get_env(pladder, http_listener_count, 10),

    Handlers =
    [pladder_accounts_handler,
     pladder_experiences_handler,
     pladder_characters_handler,
     pladder_ladders_handler,
     pladder_character_handler,
     pladder_ladder_handler,
     pladder_stats_handler,
     cowboy_swagger_handler],
    Routes = trails:trails(Handlers),
    trails:store(Routes),
    Dispatch = trails:single_host_compile(Routes),

    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}, {compress, true}]}],
    case cowboy:start_http(pladder_server, ListenerCount,
                           TransOpts, ProtoOpts) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.
%%====================================================================
%% Internal functions
%%====================================================================
