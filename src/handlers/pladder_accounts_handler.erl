%%%-------------------------------------------------------------------
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2017 19:29
%%%-------------------------------------------------------------------
-module(pladder_accounts_handler).

-define(DefaultPageSize, 20).
-define(MaxPageSize, 200).
-define(DefaultPage, 1).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").

-mixin([{sr_entities_handler,
         [init/3,
          rest_init/2,
          allowed_methods/2,
          resource_exists/2,
          content_types_accepted/2,
          content_types_provided/2,
          handle_post/2
         ]
        }]).

-export([trails/0,
         handle_get/2]).

-type state() :: sr_entities_handler:state().

-spec trails() -> trails:trails().
trails() ->
    AdvFilterExample = <<"{\"and\": [{ \"field_name\": \"rank\","
                         " \"operator\": \">\", \"value\": 10}]}">>,
    Filter =
    #{
        name => <<"advfilter">>,
        in => body,
        description => <<"Complex filtering using "
                         "https://github.com/inaka/sumo_db/wiki/"
                         "Conditional-Logic-Syntax "
                         "Example: ", AdvFilterExample/bitstring>>,
        required => false
    },
    PageSize =
    #{
        name => <<"page_size">>,
        in => query,
        description => <<"Number of accounts per page. "
                         "Default: 20. Maximum: 200">>,
        required => false,
        type => integer
    },
    Page =
    #{
        name => <<"page">>,
        in => query,
        description => <<"Page to retrieve where page 1 will "
                         "return total amount of pages. Default: 1">>,
        required => false,
        type => integer
    },
    Metadata =
    #{
        get => #{
            tags => ["accounts"],
            description => "Returns accounts",
            produces => ["application/json"],
            parameters => [Page, PageSize, Filter]
        }
    },
    Path = "/accounts",
    Opts = #{ path => Path, model => accounts, verbose => true
    },
    [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec handle_get(cowboy_req:req(), state()) ->
    {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
    #{model := Model} = sr_state:opts(State),
    Module = sr_state:module(State),
    {Qs, Req1} = cowboy_req:qs_vals(Req),
    {ok, Body, _} = cowboy_req:body(Req1),
    io:format("JsonBody:~p~n", [Body]),
    Conditions = [ {binary_to_atom(Name, unicode),
                    Value} || {Name, Value} <- Qs ],
    MaxPageSize = pladder_handler_utils:page_size(Conditions, ?MaxPageSize,
                                                  ?DefaultPageSize),
    Page = pladder_handler_utils:page(Conditions, ?DefaultPage),
    Sort = pladder_handler_utils:qs_sort(Conditions, []),
    Req2 = pladder_handler_utils:update_header(Req1),
    Reply =
    case pladder_handler_utils:validate_conditions(Conditions, Body, Model) of
        {ok, ValidConditions} ->
            io:format("Final conditions:~p~n", [ValidConditions]),
            Entities = sumo:find_by(Model, ValidConditions, Sort,
                                    MaxPageSize, (Page - 1) * MaxPageSize),
            JsonEntities = [Module:to_json(Entity) || Entity <- Entities],
            PageInfo =
            case Page of
                1 ->
                    Total = sumo:count_by(Model, ValidConditions),
                    Pages =
                    if
                        (Total rem MaxPageSize) == 0 ->
                            trunc(Total / MaxPageSize);
                        true ->
                            trunc(Total / MaxPageSize) + 1
                    end,
                    pladder_handler_utils:page_info(Pages, Page,
                                                    MaxPageSize,
                                                    length(JsonEntities));
                _ ->
                    pladder_handler_utils:page_info(0, Page, MaxPageSize,
                                                    length(JsonEntities))
            end,
            #{entities => JsonEntities, error => #{type => "", reason => ""},
              page_info => PageInfo};
        {error, #{type := Type, reason := Reason, action := error_reply}} ->
            PageInfo = pladder_handler_utils:page_info(0, 0, 0, 0),
            #{entities => [], error => #{type => Type, reason => Reason},
              page_info => PageInfo};
        {error, #{type := Type, reason := Reason,
                  action := return_no_results}} ->
            PageInfo = pladder_handler_utils:page_info(0, 0, 0, 0),
            #{entities => [], error => #{type => Type, reason => Reason},
              page_info => PageInfo};
        {error, #{type := Type, reason := Reason, action := Action}} ->
            PageInfo = pladder_handler_utils:page_info(0, 0, 0, 0),
            lager:error("Received unknown error action:~p~n", [Action]),
            #{entties => [], error => #{type => Type, reason => Reason},
              page_info => PageInfo}
    end,
    JSON = sr_json:encode(Reply),
    {JSON, Req2, State}.