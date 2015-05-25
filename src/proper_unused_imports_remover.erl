-module(proper_unused_imports_remover).
-export([parse_transform/2]).

-include("proper_internal.hrl").

-define(imported_modules, [proper, proper_types, proper_symb, proper_statem]).

-spec parse_transform([abs_form()], [compile:option()]) -> [abs_form()].
parse_transform(Forms, Options) ->
    case lists:member(warn_unused_import, Options) of
        true  -> parse(Forms, [], []);
        false -> Forms
    end.

-spec parse([abs_form()], list(), list()) -> [abs_form()].
parse([{attribute, _L, import, {?MODULE, []}} | Rest], Imports, Acc) ->
    lists:reverse(Acc) ++ use_new_imports(to_dict(Imports), Rest);
parse([{attribute, _L, import, {Mod, _Funs}} = A | Rest], Imports, Acc) ->
    case lists:member(Mod, ?imported_modules) of
        true  -> parse(Rest, [A | Imports], Acc);
        false -> parse(Rest, Imports, [A | Acc])
    end;
parse([Form | Rest], Imports, Acc) ->
    parse(Rest, Imports, [Form | Acc]).

-spec use_new_imports(dict(), [abs_form()]) -> [abs_form()].
use_new_imports(Dict0, Forms) ->
    Dict = mark_used_imports(Dict0, Forms),
    new_import_attributes(Dict) ++ Forms.

-spec mark_used_imports(dict(), [abs_form()]) -> dict().
mark_used_imports(Dict, Forms) ->
    lists:foldl(fun scan_forms/2, Dict, Forms).

-spec scan_forms(abs_form(), dict()) -> dict().
scan_forms({function, _L, _F, _A, Clauses}, Dict) ->
    lists:foldl(fun brutal_scan/2, Dict, Clauses);
scan_forms(_, Dict) ->
    Dict.

-spec brutal_scan(abs_form(), dict()) -> dict().
brutal_scan({'fun', _L, {function, Name, Arity}}, Dict) ->
    maybe_update_dict({Name, Arity}, Dict);
brutal_scan({call, _L1, Call, Args}, Dict0) ->
    case Call of
        {atom, _L2, Name} ->
            Dict = maybe_update_dict({Name, length(Args)}, Dict0),
            brutal_scan(Args, Dict);
        _ ->
            brutal_scan([Call | Args], Dict0)
    end;
brutal_scan(Other, Dict) when is_list(Other) ->
    lists:foldl(fun brutal_scan/2, Dict, Other);
brutal_scan(Other, Dict) when is_tuple(Other) ->
    brutal_scan(tuple_to_list(Other), Dict);
brutal_scan(_Other, Dict) ->
    Dict.

-spec maybe_update_dict({atom(), non_neg_integer()}, dict()) -> dict().
maybe_update_dict(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, {Line, Mod, false}} ->
            dict:store(Key, {Line, Mod, true}, Dict);
        _Other ->
            Dict
    end.

-spec to_dict([abs_form()]) -> dict().
to_dict(Imports) -> to_dict(Imports, dict:new()).

-spec to_dict([abs_form()], dict()) -> dict().
to_dict([], Dict) ->
    Dict;
to_dict([{attribute, Line, import, {Mod, FunL}} | Rest], Dict0) ->
    to_dict(Rest, lists:foldl(fun(Fun, Dict) ->
                                  dict:store(Fun, {Line, Mod, false}, Dict)
                              end, Dict0, FunL)).

-spec new_import_attributes(dict()) -> [abs_form()].
new_import_attributes(Dict) ->
    Imports = lists:keysort(1, [{Line, Mod, Fun} || {Fun, {Line, Mod, true}}
                                                    <- dict:to_list(Dict)]),
    lists:reverse(lists:foldl(fun add_new_attribute/2, [], Imports)).

-spec add_new_attribute({non_neg_integer(), atom(), atom()},
                        [abs_form()]) -> [abs_form()].
add_new_attribute({Line, Mod, Fun}, [{_, Line, _, {Mod, FunL}} | Attributes]) ->
    [{attribute, Line, import, {Mod, [Fun | FunL]}} | Attributes];
add_new_attribute({Line, Mod, Fun}, Attributes) ->
    [{attribute, Line, import, {Mod, [Fun]}} | Attributes].

