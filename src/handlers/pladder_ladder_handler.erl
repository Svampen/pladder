%%%-------------------------------------------------------------------
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2017 18:13
%%%-------------------------------------------------------------------
-module(pladder_ladder_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler,
          [ init/3,
            rest_init/2,
            allowed_methods/2,
            content_types_accepted/2,
            content_types_provided/2,
            resource_exists/2,
            handle_get/2,
            handle_put/2,
            handle_patch/2,
            delete_resource/2
          ]
        }]).

-export([trails/0]).

-spec trails() -> trails:trails().
trails() ->
    RequestBody =
    #{
        name => <<"request body">>,
        in => body,
        description => <<"request body (as json)">>,
        required => true
    },
    Id =
    #{
        name => id,
        in => path,
        description => <<"ladder name">>,
        required => true,
        type => string
    },
    Metadata =
    #{
        patch =>
        #{
            tags => ["ladders"],
            description => <<"Updates an element">>,
            consumes => ["application/json", "application/json; charset=utf-8"],
            produces => ["application/json"],
            parameters => [RequestBody, Id]
        },
        put =>
        #{
            tags => ["ladders"],
            description => <<"Updates or creates a new element">>,
            consumes => ["application/json", "application/json; charset=utf-8"],
            produces => ["application/json"],
            parameters => [RequestBody, Id]
        }
    },
    Path = "/ladders/:id",
    Opts = #{ path => Path, model => ladders, verbose => true
    },
    [trails:trail(Path, ?MODULE, Opts, Metadata)].
