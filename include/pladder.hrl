%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2017 15:43
%%%-------------------------------------------------------------------

%%"rank": 2,
%%"dead": false,
%%"online": false,
%%"character": {
%%"name": "TaylorSwiftVEVO",
%%"level": 100,
%%"class": "Scion",
%%"experience": 4250334444
%%},
%%"account": {
%%"name": "PeakingDuck",
%%"challenges": {
%%"total": 0
%%},
%%"twitch": {
%%"name": "peakingduck"
%%}
%%}
-record(ladder_entry, {
    id :: string(),
    character_name :: string(),
    character_name_lower_case :: string(),
    character_level :: integer(),
    character_class :: string(),
    character_exp :: integer(),
    account_name :: string(),
    account_name_lower_case :: string(),
    twitch_name :: string(),
    rank :: integer(),
    dead :: boolean(),
    online :: boolean(),
    last_update :: calendar:datetime(),
    exp_per_hour :: string()
}).