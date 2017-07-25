%%%-------------------------------------------------------------------
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 13. Jul 2017 18:52
%%%-------------------------------------------------------------------
-module(pladder_handler_utils).


%% API
-export([page_info/4,
         update_header/1,
         page_size/3,
         page/2,
         qs_sort/2,
         validate_conditions/3]).


-type error_action() :: return_no_results | no_error_action | error_reply.
-type error() :: #{type => string(), reason => string(),
                   action => error_action()}.

%%%=============================================================================
%%% API
%%%=============================================================================
page_info(Pages, Page, MaxPageSize, PageSize) ->
    #{pages => Pages, current_page => Page, max_items_on_page => MaxPageSize,
      items_on_page => PageSize}.

update_header(Req) ->
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
                                      <<"GET, OPTIONS">>, Req),
    cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
                               <<"*">>, Req2).

page_size(Qs, MaxPageSize, DefaultPageSize) ->
    case lists:keyfind(page_size, 1, Qs) of
        {page_size, Value} ->
            try
                PageSize = list_to_integer(bitstring_to_list(Value)),
                if
                    PageSize == 0 ->
                        MaxPageSize;
                    PageSize > MaxPageSize ->
                        MaxPageSize;
                    true ->
                        PageSize
                end
            catch
                _:_ ->
                    DefaultPageSize
            end;
        false ->
            DefaultPageSize
    end.

page(Qs, DefaultPage) ->
    case lists:keyfind(page, 1, Qs) of
        {page, Value} ->
            try
                Page = list_to_integer(bitstring_to_list(Value)),
                if
                    Page =< 0 -> DefaultPage;
                    true -> Page
                end
            catch
                _:_ ->
                    DefaultPage
            end;
        false ->
            DefaultPage
    end.

qs_sort(Qs, DefaultSort) ->
    case lists:keyfind(sort_by, 1, Qs) of
        {sort_by, Value} ->
            try
                list_to_atom(bitstring_to_list(Value))
            catch
                _:_ ->
                    DefaultSort
            end;
        false ->
            DefaultSort
    end.

validate_conditions(Conditions, Body, Model) ->
    Schema = sumo_internal:get_schema(Model),
    Fields = [ sumo_internal:field_name(Field) ||
        Field <- sumo_internal:schema_fields(Schema) ],
    case adv_filter(Body, Fields) of
        {ok, AdvFilterConditions} ->
            case qs_valid_conditions(Conditions, Fields) of
                {ok, QsValidConditions} ->
                    {ok, QsValidConditions ++ AdvFilterConditions};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
-spec get_special_fields(Conditions :: term(), Acc :: [term()]) ->
    {ok, [term()]} | {error, error()}.
get_special_fields(Conditions, Acc) ->
    try
        {ok, qs_fields(Conditions, Acc)}
    catch
        _:{error, Error} when is_map(Error) ->
            lager:error("[~p] Exception catch in get_special_fields:~p~n",
                        [?MODULE, Error]),
            {error, Error};
        _:Exception ->
            lager:error("[~p] Exception catch in get_special_fields:~p~n",
                        [?MODULE, Exception]),
            {error, #{type => internal_server_error,
                      reason => <<"see server error log">>,
                      action => error_reply}}
    end.
qs_fields([], ValidConditions) ->
    ValidConditions;
qs_fields([{character_name, Value}|Rest], ValidConditions) ->
    qs_fields(Rest, ValidConditions ++
                    [{name, 'like', <<Value/binary, <<"%">>/binary>>}]);
qs_fields([{account_name, " " ++ _Value}|Rest], ValidConditions) ->
    qs_fields(Rest, ValidConditions);
qs_fields([{account_name, Value}|Rest], ValidConditions) ->
    Name = string:to_lower(bitstring_to_list(Value)),
    case sumo:find_by(accounts, {name, 'like',
                                 <<Value/binary, <<"%">>/binary>>}) of
        [] ->
            Error = lists:concat(["No accounts found using name: ",
                                  Name]),
            throw({error, #{type => no_results,
                            reason => sumo_utils:to_bin(Error),
                            action => return_no_results}});
        [#{name := AccountName}] ->
            qs_fields(Rest, ValidConditions ++ [{account, AccountName}]);
        Docs ->
            Accounts = [ {account, AccountName} ||
                #{name := AccountName} <- Docs],
            case Accounts of
                [] ->
                    Error = lists:concat(["No accounts found using name: ",
                                          Name]),
                    throw({error, #{type => no_results,
                                    reason => sumo_utils:to_bin(Error),
                                    action => return_no_results}});
                Accounts ->
                    qs_fields(Rest, ValidConditions ++ [{'or', Accounts}])
            end
    end;
qs_fields([{character_id, Value}|Rest], ValidConditions) ->
    try
        Id = <<Value/bitstring, <<"_%">>/bitstring>>,
        qs_fields(Rest, ValidConditions ++ [{id, 'like', Id}])
    catch
        _:Exception ->
            lager:error("[~p] Exception in qs_fields for character_id: ~p~n",
                        [?MODULE, Exception]),
            Error = <<"Concatination error with "
                      "bitstring in qs_fields for character_id">>,
            throw({error, #{type => no_results,
                            reason => Error,
                            action => return_no_results}})
    end;
qs_fields([{classes, Classes}|Rest], ValidConditions) ->
    try
        ClassesList = string:tokens(sumo_utils:to_list(Classes), ","),
        ClassesRule = [{class, sumo_utils:to_bin(Class)} || Class <- ClassesList],
        qs_fields(Rest, ValidConditions ++ [{'or', ClassesRule}])
    catch
        _:Exception ->
            lager:error("[~p] Exception in qs_fields for classes: ~p~n",
                        [?MODULE, Exception]),
            Error = <<"Tokenizing error with "
                      "bitstring in qs_fields for classes">>,
            throw({error, #{type => no_results,
                            reason => Error,
                            action => return_no_results}})
    end;
qs_fields([_|Rest], ValidConditions) ->
    qs_fields(Rest, ValidConditions).

adv_filter(<<"">>, _Fields) ->
    {ok, []};
adv_filter(Json, Fields) ->
    try
        CLSMap = jiffy:decode(Json, [return_maps]),
        {ok, decode_cls(CLSMap, Fields)}
    catch
        _:{error, #{reason := Reason}=Error} ->
            lager:error("[~p] Exception catch in adv_filter:~p~n",
                        [?MODULE, Reason]),
            {error, Error};
        _:Exception ->
            lager:error("[~p] Exception catch in adv_filter:~p~n",
                        [?MODULE, Exception]),
            {error, #{type => internal_server_error,
                      reason => <<"see server error log">>,
                      action => error_reply}}
    end.

decode_cls(#{<<"and">> := ExpressionList}, Fields)
    when is_list(ExpressionList) ->
    EList = decode_cls(ExpressionList, [], Fields),
    {'and', EList};
decode_cls(#{<<"or">> := ExpressionList}, Fields)
    when is_list(ExpressionList) ->
    EList = decode_cls(ExpressionList, [], Fields),
    {'or', EList};
decode_cls(#{<<"not">> := Expression}, Fields)
    when is_map(Expression) ->
    [E] = decode_cls([Expression], [], Fields),
    {'not', E};
decode_cls(CLS, _Fields) ->
    lager:error("[~p] Unsupported CLS to decode:~p~n", [?MODULE, CLS]),
    throw({error, #{type => internal_server_error,
                    reason => <<"Unsupported CLS to decode">>,
                    action => return_no_results}}).

decode_cls([], Acc, _Fields) ->
    Acc;
decode_cls([#{<<"field_name">> := FieldName, <<"operator">> := Operator,
              <<"value">> := Value}|Rest], Acc, Fields) ->
    FieldNameAtom = sumo_utils:to_atom(FieldName),
    case verify_fields([FieldNameAtom], Fields) of
        ok ->
            E = {FieldNameAtom, sumo_utils:to_atom(Operator), Value},
            decode_cls(Rest, Acc ++ [E], Fields);
        {error, Error} ->
            throw({error, #{type => internal_server_error,
                            reason => sumo_utils:to_bin(Error),
                            action => return_no_results}})
    end;
decode_cls([#{<<"field_name_a">> := FieldNameA, <<"operator">> := Operator,
              <<"field_name_b">> := FieldNameB}|Rest], Acc, Fields) ->
    FieldNameAAtom = sumo_utils:to_atom(FieldNameA),
    FieldNameBAtom = sumo_utils:to_atom(FieldNameB),
    case verify_fields([FieldNameAAtom, FieldNameBAtom], Fields) of
        ok ->
            E = {FieldNameAAtom, sumo_utils:to_atom(Operator), FieldNameBAtom},
            decode_cls(Rest, Acc ++ [E], Fields);
        {error, Error} ->
            throw({error, #{type => internal_server_error,
                            reason => Error,
                            action => return_no_results}})
    end;
decode_cls([#{<<"field_name">> := FieldName, <<"value">> := Value}|Rest],
           Acc, Fields) ->
    FieldNameAtom = sumo_utils:to_atom(FieldName),
    case verify_fields([FieldNameAtom], Fields) of
        ok ->
            E = {FieldNameAtom, Value},
            decode_cls(Rest, Acc ++ [E], Fields);
        {error, Error} ->
            throw({error, #{type => internal_server_error,
                            reason => Error,
                            action => return_no_results}})
    end;
decode_cls([#{<<"and">> := _}=E|Rest], Acc, Fields) ->
    decode_cls(Rest, Acc ++ [decode_cls(E, Fields)]);
decode_cls([#{<<"or">> := _}=E|Rest], Acc, Fields) ->
    decode_cls(Rest, Acc ++ [decode_cls(E, Fields)]);
decode_cls([#{<<"not">> := _}=E|Rest], Acc, Fields) ->
    decode_cls(Rest, Acc ++ [decode_cls(E, Fields)]);
decode_cls([CLS|_Rest], _Acc, _Fields) ->
    lager:error("[~p] Unsupported CLS to decode:~p~n", [?MODULE, CLS]),
    throw({error, #{type => internal_server_error,
                    reason => <<"Unsupported CLS to decode">>,
                    action => return_no_results}}).



qs_valid_conditions(Conditions, Fields) ->
    CompareFun =
    fun({Name, _}) ->
        true =:= lists:member(Name, Fields)
    end,
    ValidConditions = lists:filter(CompareFun, Conditions),
    get_special_fields(Conditions, ValidConditions).

verify_fields([], _Fields) ->
    ok;
verify_fields([Field|Rest], Fields) ->
    case lists:member(Field, Fields) of
        true ->
            verify_fields(Rest, Fields);
        false ->
            Error = lists:concat([Field, " field not found in model"]),
            {error, Error}
    end.
