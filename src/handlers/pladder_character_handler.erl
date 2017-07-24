%%%-------------------------------------------------------------------
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2017 18:13
%%%-------------------------------------------------------------------
-module(pladder_character_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler,
          [ init/3,
            rest_init/2,
            allowed_methods/2,
            content_types_accepted/2,
            content_types_provided/2,
            handle_put/2,
            handle_patch/2,
            delete_resource/2
          ]
        }]).

-export([trails/0,
         resource_exists/2,
         handle_get/2]).

-type state() :: sr_single_entity_handler:state().

-spec trails() -> trails:trails().
trails() ->
    Id =
    #{
        name => id,
        in => path,
        description => <<"character id">>,
        required => true,
        type => string
    },
    Metadata =
    #{
        get => #{
            tags => ["characters"],
            description => "Returns character information",
            produces => ["application/json"],
            parameters => [Id]
        }
    },
    Path = "/characters/:id",
    Opts = #{ path => Path, model => characters, verbose => true
    },
    [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec resource_exists(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
    Id = sr_state:id(State),
    #{model := Model} = sr_state:opts(State),
    case sumo:fetch(Model, Id) of
        notfound -> {false, Req, State};
        Entity ->
            UpdatedEntity = add_class_rank(Entity),
            {true, Req, sr_state:entity(State, UpdatedEntity)}
    end.

-spec handle_get(cowboy_req:req(), state()) ->
    {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
    Entity = sr_state:entity(State),
    Module = sr_state:module(State),
    ResBody = sr_json:encode(Module:to_json(Entity)),
    {ResBody, Req, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
add_class_rank(#{ladder := Ladder, class := Class, rank := Rank}=Character) ->
    LadderCond = {ladder, Ladder},
    ClassCond = {class, Class},
    RankCond = {rank, '<', Rank},
    Conditions = {'and', [LadderCond, ClassCond, RankCond]},
    ClassRank = sumo:count_by(characters, Conditions),
    MultiClasses = [{class, MClass} ||
        MClass <- pladder_util:get_classes(Class)],
    MultiClassCondition = {'or', MultiClasses},
    MultiClassRankConditions = {'and', [LadderCond, RankCond,
                                        MultiClassCondition]},
    MultiClassRank = sumo:count_by(characters, MultiClassRankConditions),
    Character#{class_rank => ClassRank, multi_class_rank => MultiClassRank}.
