%%%-------------------------------------------------------------------
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2017 22:12
%%%-------------------------------------------------------------------
-module(pladder_util).

-include("poe_exp.hrl").
-include("pladder.hrl").

%% API
-export([convert_boolean/1,
         get_default_ladders/0,
         add_padding/2,
         get_classes/1,
         build_path/3,
         update_entry/2,
         update_ladder/3,
         update_offset/2,
         to_mysql_datetime/1,
         from_mysql_datetime/1]).

convert_boolean(false) ->
    <<"false">>;
convert_boolean(true) ->
    <<"true">>;
convert_boolean(<<"false">>) ->
    false;
convert_boolean(<<"true">>) ->
    true;
convert_boolean(Active)
    when is_atom(Active) ->
    <<"false">>;
convert_boolean(Active)
    when is_list(Active) ->
    false.

get_default_ladders() ->
    case application:get_env(pladder, default_ladders) of
        {ok, Ladders} ->
            Ladders;
        undefined ->
            []
    end.

add_padding(exp, Exp) ->
    add_integer_padding(Exp, ?MaxExpLength(?MaxLevel));
add_padding(rank, Rank) ->
    add_integer_padding(Rank, ?MaxRankLength);
add_padding(level, Level) ->
    add_integer_padding(Level, ?MaxLevelLength);
add_padding(_Type, Integer) ->
    list_to_bitstring(integer_to_list(Integer)).

add_integer_padding(Integer, MaxLength) ->
    IntegerString = integer_to_list(Integer),
    IntegerLength = length(IntegerString),
    PaddedIntegerString =
    if
        IntegerLength < MaxLength ->
            PaddingLength = MaxLength - IntegerLength,
            add_zeros(IntegerString, lists:seq(1, PaddingLength));
        true ->
            IntegerString
    end,
    list_to_bitstring(PaddedIntegerString).


add_zeros(String, []) ->
    String;
add_zeros(String, [_|Rest]) ->
    add_zeros("0" ++ String, Rest).

get_classes(<<"Templar">>) ->
    ?Templar;
get_classes(<<"Inquisitor">>) ->
    ?Inquisitor;
get_classes(<<"Hierophant">>) ->
    ?Hierophant;
get_classes(<<"Guardian">>) ->
    ?Guardian;
get_classes(<<"Duelist">>) ->
    ?Duelist;
get_classes(<<"Slayer">>) ->
    ?Slayer;
get_classes(<<"Gladiator">>) ->
    ?Gladiator;
get_classes(<<"Champion">>) ->
    ?Champion;
get_classes(<<"Shadow">>) ->
    ?Shadow;
get_classes(<<"Assassin">>) ->
    ?Assassin;
get_classes(<<"Saboteur">>) ->
    ?Saboteur;
get_classes(<<"Trickster">>) ->
    ?Trickster;
get_classes(<<"Marauder">>) ->
    ?Marauder;
get_classes(<<"Juggernaut">>) ->
    ?Juggernaut;
get_classes(<<"Chieftain">>) ->
    ?Chieftain;
get_classes(<<"Witch">>) ->
    ?Witch;
get_classes(<<"Necromancer">>) ->
    ?Necromancer;
get_classes(<<"Elementalist">>) ->
    ?Elementalist;
get_classes(<<"Occultist">>) ->
    ?Occultist;
get_classes(<<"Ranger">>) ->
    ?Ranger;
get_classes(<<"Deadeye">>) ->
    ?Deadeye;
get_classes(<<"Raider">>) ->
    ?Raider;
get_classes(<<"Pathfinder">>) ->
    ?Pathfinder;
get_classes(<<"Scion">>) ->
    ?Scion;
get_classes(<<"Ascendant">>) ->
    ?Ascendant.

build_path(Ladder, Limit, Offset) ->
    EncodedLadder = http_uri:encode(Ladder),
    lists:concat([?Ladders, "/", EncodedLadder, "?", "limit=", Limit,
                  "&", "offset=", Offset, "&track=true"]).
update_ladder(#{<<"total">> := Total, <<"entries">> := _Entries}, Ladder,
              0) ->
    lager:warning("[~p] 0 available workers for ~p to update entries~n",
                  [?MODULE, Ladder]),
    Total;
update_ladder(#{<<"total">> := Total, <<"entries">> := Entries}, Ladder,
              Workers) ->
    request_entry_updates(Entries, Ladder, Workers),
    Total;
update_ladder(LadderEntries, Ladder, _Workers) ->
    lager:error("[~p] Ladder entries for ~p didn't match function clause:~p~n",
                [?MODULE, Ladder, LadderEntries]),
    0.

request_entry_updates([], _Ladder, _Workers) ->
    ok;
request_entry_updates(Entries, Ladder, Workers) ->
    {Rest, Updates} = request_entry_update(Entries, Ladder, Workers, 0),
    wait_for_entry_updates(Updates),
    request_entry_updates(Rest, Ladder, Workers).

request_entry_update(Entries, _Ladder, 0, Updates) ->
    {Entries, Updates};
request_entry_update([], _Ladder, _Workers, Updates) ->
    {[], Updates};
request_entry_update([Entry|Entries], Ladder, Workers, Updates) ->
    pladder_ladder_update_worker:update_entry(Entry, Ladder, self()),
    request_entry_update(Entries, Ladder, Workers - 1, Updates + 1).

wait_for_entry_updates(0) ->
    ok;
wait_for_entry_updates(UpdatesLeft) ->
    receive
        {entry_updated} ->
            wait_for_entry_updates(UpdatesLeft - 1);
        {entry_failed} ->
            wait_for_entry_updates(UpdatesLeft - 1)
    after
        30000 ->
            lager:warning("[~p] Timedout while waiting for entry "
                          "updates with ~p updates left~n",
                          [?MODULE, UpdatesLeft]),
            ok
    end.

update_entry([], _Ladder) ->
    ok;
update_entry([#{<<"rank">> := Rank, <<"dead">> := Dead, <<"online">> := Online,
                <<"character">> := #{<<"name">> := CharacterName,
                                     <<"id">> := Id,
                                     <<"level">> := CharacterLevel,
                                     <<"experience">> := Exp,
                                     <<"class">> := Class},
                <<"account">> := #{<<"name">> := AccountName}=AccountInfo}
                 |Rest], Ladder) ->
    TwitchName =
    case AccountInfo of
        #{<<"twitch">> := #{<<"name">> := AccountTwitchName}} ->
            AccountTwitchName;
        _ ->
            <<"">>
    end,
    CharacterNameLowerCaseBitString = <<"">>,
    Character = pladder_characters:new(Id, CharacterName,
                                       CharacterNameLowerCaseBitString,
                                       CharacterLevel,
                                       Class, Exp, Rank, Dead, Online,
                                       list_to_bitstring(Ladder), AccountName),
    sumo:persist(characters, Character),
    Account =
    pladder_accounts:new(AccountName,
                         <<"">>,
                         TwitchName),
    sumo:persist(accounts, Account),
    if
        Dead == true orelse Online == false orelse CharacterLevel == 100 ->
            ok;
        true ->
            Experience = pladder_experiences:new(Id, Exp),
            sumo:persist(experiences, Experience)
    end,
    update_entry(Rest, Ladder).

update_offset(Offset, Total) ->
    if
        (Offset + ?Limit) >= Total ->
            0;
        true ->
            Offset + ?Limit
    end.

to_mysql_datetime(DateTimeBin) when is_bitstring(DateTimeBin) ->
    DateTime = sumo_utils:to_list(DateTimeBin),
    [Date, Time] = string:tokens(DateTime, "TZ"),
    lists:concat([Date, " ", Time]).
from_mysql_datetime(DateTime) ->
    [Date, Time] = string:tokens(DateTime, " "),
    sumo_utils:to_bin(lists:concat([Date, "T", Time, "Z"])).