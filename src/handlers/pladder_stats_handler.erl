%%%-------------------------------------------------------------------
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2017 18:13
%%%-------------------------------------------------------------------
-module(pladder_stats_handler).

-behaviour(trails_handler).

-include("pladder.hrl").

-include_lib("mixer/include/mixer.hrl").

-mixin([{ sr_entities_handler,
          [ init/3,
            rest_init/2,
            allowed_methods/2,
            content_types_accepted/2,
            content_types_provided/2,
            resource_exists/2,
            handle_put/2,
            handle_patch/2,
            delete_resource/2
          ]
        }]).

-export([trails/0,
         handle_get/2]).

-type state() :: sr_single_entity_handler:state().

-spec trails() -> trails:trails().
trails() ->
    Ladder =
    #{
        name => <<"ladder">>,
        in => query,
        description => <<"Ladder to get stats for">>,
        required => true,
        type => string
    },
    Metadata =
    #{
        get => #{
            tags => ["stats"],
            description => "Returns stats",
            consumes => ["application/json"],
            produces => ["application/json"],
            parameters => [Ladder]
        }
    },
    Path = "/stats",
    Opts = #{ path => Path, model => characters, verbose => true
    },
    [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec handle_get(cowboy_req:req(), state()) ->
    {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
    {Qs, Req1} = cowboy_req:qs_vals(Req),
    Conditions = [ {binary_to_atom(Name, unicode),
                    Value} || {Name, Value} <- Qs ],
    Reply =
    case lists:keyfind(ladder, 1, Conditions) of
        {ladder, Ladder} ->
            Characters = sumo:count_by(characters, {ladder, Ladder}),
            AliveCharacters = sumo:count_by(characters,
                                            {'and', [{ladder, Ladder},
                                                     {dead, 0}]}),
            DeadCharacters = sumo:count_by(characters,
                                           {'and', [{ladder, Ladder},
                                                    {dead, 1}]}),
            Classes =
            lists:foldl(
                fun(Class, Acc) ->
                    ClassCount = sumo:count_by(characters,
                                               {'and', [{ladder, Ladder},
                                                        {class, Class}]}),
                    ClassCountDead = sumo:count_by(characters,
                                                   {'and', [{ladder, Ladder},
                                                            {class, Class},
                                                            {dead, 1}]}),
                    ClassCountAlive = sumo:count_by(characters,
                                                    {'and', [{ladder, Ladder},
                                                             {class, Class},
                                                             {dead, 0}]}),
                    Map = #{total => ClassCount, dead => ClassCountDead,
                            alive => ClassCountAlive},
                    maps:put(Class, Map, Acc)
                end,
                #{},
                ?Classes
            ),
            #{total => Characters, alive => AliveCharacters,
              dead => DeadCharacters, class_stats => Classes};
        false ->
            #{total => 0, alive => 0, dead => 0, class_stats => null}
    end,
    JSON = sr_json:encode(Reply),
    {JSON, Req1, State}.