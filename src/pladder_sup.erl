%%%-------------------------------------------------------------------
%% @doc pladder top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pladder_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).



%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    PladderUpdateSup =
    #{
        id => pladder_update_sup,
        start => {pladder_update_sup, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor
    },
    Pladder =
    #{
        id => pladder,
        start => {pladder, start_link, []},
        restart => permanent,
        shutdown => 2000

    },
    {ok, {{one_for_one, 10, 3600}, [PladderUpdateSup, Pladder]}}.

%%====================================================================
%% Internal functions
%%====================================================================
