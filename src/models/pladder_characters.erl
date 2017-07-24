%%%-------------------------------------------------------------------
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2017 21:39
%%%-------------------------------------------------------------------
-module(pladder_characters).


-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-type character() :: # {
    id => binary(),
    name => binary(),
    name_lower_case => binary(),
    level => integer(),
    class => binary(),
    exp => integer(),
    rank => integer(),
    dead => boolean(),
    online => boolean(),
    ladder => binary(),
    account => binary(),
    last_updated => calendar:datetime()
}.

%% API
-export([new/11]).

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
    sumo:new_schema(characters, [
        sumo:new_field(id,              string,   [id, {length, 100},
                                                   not_null]),
        sumo:new_field(name,            string,   [{length, 100}, not_null]),
        sumo:new_field(name_lower_case, string,   [{length, 100}, not_null]),
        sumo:new_field(level,           integer,  [not_null]),
        sumo:new_field(class,           string,   [{length, 100}, not_null]),
        sumo:new_field(exp,             integer,  [bigint, not_null]),
        sumo:new_field(rank,            integer,  [not_null]),
        sumo:new_field(dead,            boolean,  [not_null]),
        sumo:new_field(online,          boolean,  [not_null]),
        sumo:new_field(ladder,          string,   [{length, 100}, not_null]),
        sumo:new_field(account,         string,   [{length, 100}, not_null]),
        sumo:new_field(last_updated,    datetime, [not_null])
    ]).

-spec sumo_sleep(character()) -> sumo:model().
sumo_sleep(#{id := Id, name := Name, name_lower_case := NameLowerCase,
             level := Level, class := Class, exp := Exp,
             rank := Rank, dead := Dead, online := Online, ladder := Ladder,
             account := Account, last_updated := LastUpdated}) ->
%%    LastUpdatedSleep =
%%    pladder_util:to_mysql_datetime(sr_json:encode_date(LastUpdated)),
    #{
        id => Id,
        name => Name,
        name_lower_case => NameLowerCase,
        level => Level,
        class => Class,
        exp => Exp,
        rank => Rank,
        dead => Dead,
        online => Online,
        ladder => Ladder,
        account => Account,
        last_updated => LastUpdated
    }.

-spec sumo_wakeup(sumo:model()) -> character().
sumo_wakeup(Doc) ->
    LastUpdated = maps:get(last_updated, Doc),
%%    LastUpdatedWakeup =
%%    sr_json:decode_date(pladder_util:from_mysql_datetime(LastUpdated)),
    #{
        id => maps:get(id, Doc),
        name => maps:get(name, Doc),
        name_lower_case => maps:get(name_lower_case, Doc),
        level => maps:get(level, Doc),
        class => maps:get(class, Doc),
        exp => maps:get(exp, Doc),
        rank => maps:get(rank, Doc),
        dead => maps:get(dead, Doc),
        online => maps:get(online, Doc),
        ladder => maps:get(ladder, Doc),
        account => maps:get(account, Doc),
        last_updated => LastUpdated
    }.

%%%=============================================================================
%%% sumo_rest_doc callbacks
%%%=============================================================================
-spec to_json(character()) -> sumo_rest_doc:json().
to_json(#{last_updated := LastUpdated}=Character) ->
%%    LastUpdatedJson = pladder_util:from_mysql_datetime(LastUpdated),
    LastUpdatedJson = sr_json:encode_date(LastUpdated),
    Character#{last_updated => LastUpdatedJson}.

-spec from_json(sumo_rest_doc:json()) -> {ok, character()} | {error, iodata()}.
from_json(Json) ->
    try
        Now = calendar:now_to_datetime(erlang:timestamp()),
        {ok, #{
            id => maps:get(id, Json),
            name => maps:get(name, Json),
            name_lower_case => maps:get(name_lower_case, Json),
            level => maps:get(level, Json),
            class => maps:get(class, Json),
            exp => maps:get(exp, Json),
            rank => maps:get(rank, Json),
            dead => maps:get(dead, Json),
            online => maps:get(online, Json),
            ladder => maps:get(ladder, Json),
            account => maps:get(account, Json),
            last_updated => Now
        }}
    catch
        _:{badkey, Key} ->
            {error, <<"missing field: ", Key/binary>>}
    end.

-spec update(character(), sumo_rest_doc:json()) ->
    {ok, character()} | {error, iodata()}.
update(_Character, _Json) ->
    {error, <<"unsupported feature">>}.

-spec location(character(), sumo_rest_doc:path()) -> binary().
location(#{id := Id}, Path) ->
    iolist_to_binary([Path, "/", Id]).

-spec duplication_conditions(character()) ->
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
-spec new(Id :: binary(), Name :: binary(), NameLowerCase :: binary(),
          Level :: integer(), Class :: binary(), Exp :: integer(),
          Rank :: integer(), Dead :: boolean(), Online :: boolean(),
          Ladder :: binary(), Account :: binary())
         -> character().
new(Id, Name, NameLowerCase, Level, Class, Exp, Rank, Dead,
    Online, Ladder, Account) ->
    Now = calendar:now_to_datetime(erlang:timestamp()),
    #{
        id => Id,
        name => Name,
        name_lower_case => NameLowerCase,
        level => Level,
        class => Class,
        exp => Exp,
        rank => Rank,
        dead => Dead,
        online => Online,
        ladder => Ladder,
        account => Account,
        last_updated => Now
    }.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================