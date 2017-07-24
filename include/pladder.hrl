%%%-------------------------------------------------------------------
%%% @copyright (C) 2017>>, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2017 15:43
%%%-------------------------------------------------------------------

-define(RestURL, "api.pathofexile.com").
-define(Ladders, "/ladders").
-define(RestPort, 80).
-define(Limit, 200).
-define(StartUpdateTimer, 5*60*1000).
-define(CheckOnLaddersTimer, 60*60*1000).
-define(CleanLaddersTimer, 5*60*60*1000).

-define(MaxLevel, 100).
-define(MaxLevelLength, length(integer_to_list(?MaxLevel))).
-define(MaxRank, 15000).
-define(MaxRankLength, length(integer_to_list(?MaxRank))).

-define(Templar, [<<"Templar">>, <<"Inquisitor">>,
                  <<"Hierophant">>, <<"Guardian">>]).
-define(Inquisitor, [<<"Inquisitor">>, <<"Templar">>]).
-define(Hierophant, [<<"Hierophant">>, <<"Templar">>]).
-define(Guardian, [<<"Templar">>, <<"Guardian">>]).

-define(Duelist, [<<"Duelist">>, <<"Slayer">>,
                  <<"Gladiator">>, <<"Champion">>]).
-define(Slayer, [<<"Slayer">>, <<"Duelist">>]).
-define(Gladiator, [<<"Duelist">>, <<"Gladiator">>]).
-define(Champion, [<<"Duelist">>, <<"Champion">>]).

-define(Shadow, [<<"Shadow">>, <<"Assassin">>,
                 <<"Saboteur">>, <<"Trickster">>]).
-define(Assassin, [<<"Shadow">>, <<"Assassin">>]).
-define(Saboteur, [<<"Shadow">>, <<"Saboteur">>]).
-define(Trickster, [<<"Shadow">>, <<"Trickster">>]).

-define(Marauder, [<<"Marauder">>, <<"Juggernaut">>,
                   <<"Berserker">>, <<"Chieftain">>]).
-define(Juggernaut, [<<"Marauder">>, <<"Juggernaut">>]).
-define(Berserker, [<<"Marauder">>, <<"Berserker">>]).
-define(Chieftain, [<<"Marauder">>, <<"Chieftain">>]).

-define(Witch, [<<"Witch">>, <<"Necromancer">>,
                <<"Elementalist">>, <<"Occultist">>]).
-define(Necromancer, [<<"Witch">>, <<"Necromancer">>]).
-define(Elementalist, [<<"Witch">>, <<"Elementalist">>]).
-define(Occultist, [<<"Witch">>, <<"Occultist">>]).

-define(Ranger, [<<"Ranger">>, <<"Deadeye">>,
                 <<"Raider">>, <<"Pathfinder">>]).
-define(Deadeye, [<<"Ranger">>, <<"Deadeye">>]).
-define(Raider, [<<"Ranger">>, <<"Raider">>]).
-define(Pathfinder, [<<"Ranger">>, <<"Pathfinder">>]).

-define(Scion, [<<"Scion">>, <<"Ascendant">>]).
-define(Ascendant, [<<"Scion">>, <<"Ascendant">>]).

-define(Classes, ?Templar ++
                 ?Duelist ++
                 ?Shadow ++
                 ?Marauder ++
                 ?Witch ++
                 ?Ranger ++
                 ?Scion).