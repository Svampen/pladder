%%%-------------------------------------------------------------------
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 15. Jul 2017 15:29
%%%-------------------------------------------------------------------
-module(pladder_update_sup).


-behaviour(supervisor).

%% API
-export([start_link/0,
         start_ladder_update/2,
         stop_ladder_update/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_ladder_update(Ladder :: string(), WorkersPerLadder :: integer()) ->
    supervisor:startchild_ret().
start_ladder_update(Ladder, WorkersPerLadder) ->
    Child =
    #{
        id => pladder_ladder_update:name(Ladder),
        start => {pladder_ladder_update, start_link,
                  [Ladder, WorkersPerLadder]},
        restart => permanent,
        shutdown => 5000
    },
    supervisor:start_child(?SERVER, Child).

-spec stop_ladder_update(Ladder :: string()) -> ok | {error, Error}
                            when Error :: not_found | simple_one_for_one.
stop_ladder_update(Ladder) ->
    supervisor:terminate_child(?MODULE, pladder_ladder_update:name(Ladder)).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
                       MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
          [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    PladderWorker =
    #{
        id => pladder_worker_pool,
        start => {pladder_ladder_update_worker, start_link, []},
        restart => permanent,
        shutdown => 2000
    },
    {ok, {SupFlags, [PladderWorker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
