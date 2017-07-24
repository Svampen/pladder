%%%-------------------------------------------------------------------
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2017 22:38
%%%-------------------------------------------------------------------
-module(pladder_experiences).


-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-type experience() :: #{
    id => binary(),
    experience => integer(),
    date => calendar:datetime()
}.


%% API
-export([new/2]).

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
    sumo:new_schema(experiences, [
        sumo:new_field(id,         string,   [id, {length, 100}, not_null]),
        sumo:new_field(experience, integer,  [bigint, not_null]),
        sumo:new_field(date,       datetime, [not_null])
    ]).

-spec sumo_sleep(experience()) -> sumo:model().
sumo_sleep(#{id := Id, experience := Experience, date := Date}) ->
%%    DateSleep =
%%    pladder_util:to_mysql_datetime(sr_json:encode_date(Date)),
    #{
        id => Id,
        experience => Experience,
        date => Date
    }.

-spec sumo_wakeup(sumo:model()) -> experience().
sumo_wakeup(Doc) ->
    Date = maps:get(date, Doc),
%%    DateWakeup =
%%    sr_json:decode_date(pladder_util:from_mysql_datetime(Date)),
    #{
        id => maps:get(id, Doc),
        experience => maps:get(experience, Doc),
        date => Date
    }.

%%%=============================================================================
%%% sumo_rest_doc callbacks
%%%=============================================================================
-spec to_json(experience()) -> sumo_rest_doc:json().
to_json(#{date := Date}=Experience) ->
%%    DateJson = pladder_util:from_mysql_datetime(Date),
    DateJson = sr_json:encode_date(Date),
    Experience#{date => DateJson}.

-spec from_json(sumo_rest_doc:json()) -> {ok, experience()} | {error, iodata()}.
from_json(Json) ->
    try
        Now = calendar:now_to_datetime(erlang:timestamp()),
        {ok, #{
            id => maps:get(id, Json),
            experience => maps:get(experience, Json),
            date => Now
        }}
    catch
        _:{badkey, Key} ->
            {error, <<"missing field: ", Key/binary>>}
    end.

-spec update(experience(), sumo_rest_doc:json()) ->
    {ok, experience()} | {error, iodata()}.
update(_Character, _Json) ->
    {error, <<"unsupported feature">>}.

-spec location(experience(), sumo_rest_doc:path()) -> binary().
location(#{id := Id}, Path) ->
    iolist_to_binary([Path, "/", Id]).

-spec duplication_conditions(experience()) ->
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
-spec new(Id :: binary(), Exp :: integer()) -> experience().
new(Id, Exp) ->
    Date = calendar:now_to_datetime(erlang:timestamp()),
    ExpId = <<Id/bitstring, <<"_">>/bitstring,
              (sr_json:encode_date(Date))/bitstring>>,
    #{
        id => ExpId,
        experience => Exp,
        date => calendar:now_to_datetime(erlang:timestamp())
    }.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
