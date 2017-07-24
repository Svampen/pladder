%%%-------------------------------------------------------------------
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2017 22:23
%%%-------------------------------------------------------------------
-module(pladder_accounts).


-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-type account() :: #{
    name => binary(),
    name_lower_case => binary(),
    twitch => binary()
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
    sumo:new_schema(accounts, [
        sumo:new_field(name,            string, [id, {length, 100}, not_null]),
        sumo:new_field(name_lower_case, string, [{length, 100}, not_null]),
        sumo:new_field(twitch,          string, [{length, 100}, not_null])
    ]).

-spec sumo_sleep(account()) -> sumo:model().
sumo_sleep(#{name := Name, name_lower_case := NameLowerCase,
             twitch := Twitch}) ->
    #{
        name => Name,
        name_lower_case => NameLowerCase,
        twitch => Twitch
    }.

-spec sumo_wakeup(sumo:model()) -> account().
sumo_wakeup(Doc) ->
    #{
        name => maps:get(name, Doc),
        name_lower_case => maps:get(name_lower_case, Doc),
        twitch => maps:get(twitch, Doc)
    }.

%%%=============================================================================
%%% sumo_rest_doc callbacks
%%%=============================================================================
-spec to_json(account()) -> sumo_rest_doc:json().
to_json(Account) ->
    Account.

-spec from_json(sumo_rest_doc:json()) -> {ok, account()} | {error, iodata()}.
from_json(Json) ->
    try
        {ok, #{
            name => maps:get(name, Json),
            name_lower_case => maps:get(name_lower_case, Json),
            twitch => maps:get(twitch, Json)
        }}
    catch
        _:{badkey, Key} ->
            {error, <<"missing field: ", Key/binary>>}
    end.

-spec update(account(), sumo_rest_doc:json()) ->
    {ok, account()} | {error, iodata()}.
update(_Character, _Json) ->
    {error, <<"unsupported feature">>}.

-spec location(account(), sumo_rest_doc:path()) -> binary().
location(#{name := Name}, Path) ->
    iolist_to_binary([Path, "/", Name]).

-spec duplication_conditions(account()) ->
    sumo_rest_doc:duplication_conditions().
duplication_conditions(#{name := Name}) ->
    [{name, '==', Name}].

-spec id_from_binding(term()) -> binary().
id_from_binding(Id) when is_binary(Id) ->
    Id;
id_from_binding(Id) ->
    sumo_utils:to_bin(Id).

%%%=============================================================================
%%% API
%%%=============================================================================
-spec new(Name :: binary(), NameLowerCase :: binary(),
          Twitch :: binary()) -> account().
new(Name, NameLowerCase, Twitch) ->
    #{
        name => Name,
        name_lower_case => NameLowerCase,
        twitch => Twitch
    }.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================