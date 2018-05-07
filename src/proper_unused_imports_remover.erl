%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2015-2017 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2015-2017 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Zaiming Shi (modifications and update by Kostis Sagonas)

-module(proper_unused_imports_remover).
-export([parse_transform/2]).

-include("proper_internal.hrl").

-ifdef(USE_ERL_SCAN_LINE).
-define(LINE_MOD, erl_scan).
-else.
-define(LINE_MOD, erl_anno).
-endif.

-type key() :: {fun_name(), arity()}.

-type val() :: {?LINE_MOD:line(), mod_name(), boolean()}.
-type imp_dict() :: dict:dict(key(), val()).

-define(IMP_MODULES,
	[proper, proper_statem, proper_symb, proper_types, proper_unicode]).

-spec parse_transform([abs_form()], [compile:option()]) -> [abs_form()].
parse_transform(Forms, Options) ->
    case lists:member(warn_unused_import, Options) of
        true  -> parse(Forms, [], []);
        false -> Forms
    end.

-spec parse([abs_form()], [abs_form()], [abs_form()]) -> [abs_form()].
parse([{attribute, _L, import, {?MODULE, []}} | Rest], Imports, Acc) ->
    lists:reverse(Acc) ++ use_new_imports(to_dict(Imports), Rest);
parse([{attribute, _L, import, {Mod, _Funs}} = A | Rest], Imports, Acc) ->
    case lists:member(Mod, ?IMP_MODULES) of
        true  -> parse(Rest, [A | Imports], Acc);
        false -> parse(Rest, Imports, [A | Acc])
    end;
parse([Form | Rest], Imports, Acc) ->
    parse(Rest, Imports, [Form | Acc]).

-spec use_new_imports(imp_dict(), [abs_form()]) -> [abs_form()].
use_new_imports(Dict0, Forms) ->
    Dict = mark_used_imports(Dict0, Forms),
    new_import_attributes(Dict) ++ Forms.

-spec mark_used_imports(imp_dict(), [abs_form()]) -> imp_dict().
mark_used_imports(Dict, Forms) ->
    lists:foldl(fun scan_forms/2, Dict, Forms).

-spec scan_forms(abs_form(), imp_dict()) -> imp_dict().
scan_forms({function, _L, _F, _A, Clauses}, Dict) ->
    lists:foldl(fun brutal_scan/2, Dict, Clauses);
scan_forms(_, Dict) ->
    Dict.

-spec brutal_scan(abs_form() | [abs_form()], imp_dict()) -> imp_dict().
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

-spec maybe_update_dict(key(), imp_dict()) -> imp_dict().
maybe_update_dict(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, {Line, Mod, false}} ->
            dict:store(Key, {Line, Mod, true}, Dict);
        _Other ->
            Dict
    end.

-spec to_dict([abs_form()]) -> imp_dict().
to_dict(Imports) -> to_dict(Imports, dict:new()).

-spec to_dict([abs_form()], imp_dict()) -> imp_dict().
to_dict([], Dict) ->
    Dict;
to_dict([{attribute, Line, import, {Mod, FunL}} | Rest], Dict0) ->
    to_dict(Rest, lists:foldl(fun(Fun, Dict) ->
                                  dict:store(Fun, {Line, Mod, false}, Dict)
                              end, Dict0, FunL)).

-spec new_import_attributes(imp_dict()) -> [abs_form()].
new_import_attributes(Dict) ->
    LMFs = [{Line, Mod, Fun} || {Fun, {Line, Mod, true}} <- dict:to_list(Dict)],
    Imports = lists:keysort(1, LMFs),
    lists:reverse(lists:foldl(fun add_new_attribute/2, [], Imports)).

-type lmf() :: {?LINE_MOD:line(), mod_name(), fun_name()}.
-spec add_new_attribute(lmf(), [abs_form()]) -> [abs_form()].
add_new_attribute({Line, Mod, Fun}, [{_, Line, _, {Mod, FunL}} | Attributes]) ->
    [{attribute, Line, import, {Mod, [Fun | FunL]}} | Attributes];
add_new_attribute({Line, Mod, Fun}, Attributes) ->
    [{attribute, Line, import, {Mod, [Fun]}} | Attributes].
