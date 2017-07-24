%%%-------------------------------------------------------------------
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2017 16:51
%%%-------------------------------------------------------------------
-module(pladder_ladders).


-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-type ladder() :: #{
    id => binary(),
    type => binary(),
    active => boolean()
}.

%% API
-export([new/3]).

%% sumo_doc callbacks
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).
%% sumo_rest_dock callbacks
-export([to_json/1,
         from_json/1,
         update/2,
         location/2,
         duplication_conditions/1,
         id_from_binding/1]).


%%%=============================================================================
%%% sumo_doc callbacks
%%%=============================================================================

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
    sumo:new_schema(ladders, [
        sumo:new_field(id,     string,  [id, {length, 100}, not_null]),
        sumo:new_field(type,   string,  [{length, 100}, not_null]),
        sumo:new_field(active, boolean, [not_null])
    ]).

-spec sumo_sleep(ladder()) -> sumo:model().
sumo_sleep(#{id := Id, type := Type, active := Active}) ->
    #{
        id => Id,
        type => Type,
        active => Active
    }.

-spec sumo_wakeup(sumo:model()) -> ladder().
sumo_wakeup(Doc) ->
    #{
        id => maps:get(id, Doc),
        type => maps:get(type, Doc),
        active => maps:get(active, Doc)
    }.

%%%=============================================================================
%%% sumo_rest_doc callbacks
%%%=============================================================================
-spec to_json(ladder()) -> sumo_rest_doc:json().
to_json(Ladder) ->
    Ladder.

-spec from_json(sumo_rest_doc:json()) -> {ok, ladder()} | {error, iodata()}.
from_json(Json) ->
    try
        {ok, #{
            id => maps:get(<<"id">>, Json),
            type => maps:get(<<"type">>, Json),
            active => maps:get(<<"active">>, Json)
        }}
    catch
        _:{badkey, Key} ->
            {error, <<"missing field: ", Key/binary>>}
    end.

-spec update(ladder(), sumo_rest_doc:json()) ->
    {ok, ladder()} | {error, iodata()}.
update(Ladder, Json) ->
    try
        UpdatedLadder =
        case {maps:is_key(<<"type">>, Json), maps:is_key(<<"active">>, Json)} of
            {true, true} ->
                Type = maps:get(<<"type">>, Json),
                Active = maps:get(<<"active">>, Json),
                Ladder#{type := Type, active := Active};
            {true, false} ->
                Type = maps:get(<<"type">>, Json),
                Ladder#{type := Type};
            {false, true} ->
                Active = maps:get(<<"active">>, Json),
                Ladder#{active := Active};
            {false, false} ->
                throw(no_keys)
        end,
        {ok, UpdatedLadder}
    catch
        _:no_keys ->
            {error, <<"missing fields">>};
        _:{badkey, Key} ->
            {error, <<"missing field: ", Key/binary>>}
    end.

-spec location(ladder(), sumo_rest_doc:path()) -> binary().
location(#{id := Id}, Path) ->
    iolist_to_binary([Path, "/", Id]).

-spec duplication_conditions(ladder()) ->
    sumo_rest_doc:duplication_conditions().
duplication_conditions(#{id := Id}) ->
    [{id, '==', Id}].

-spec id_from_binding(term()) -> binary().
id_from_binding(Id) when is_binary(Id) ->
    Id;
id_from_binding(Id) ->
    sumo_utils:to_bin(Id).

%%%=============================================================================
%%% API
%%%=============================================================================
-spec new(Id :: binary(), Type :: binary(), Active :: boolean()) -> ladder().
new(Id, Type, Active) ->
    #{
        id => Id,
        type => Type,
        active => Active
    }.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

