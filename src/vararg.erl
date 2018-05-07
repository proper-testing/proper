%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2010-2016 Manolis Papadakis <manopapad@gmail.com>,
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

%%% @copyright 2010-2016 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Manolis Papadakis
%%% @doc This module contains a helper parse transform that allows the creation
%%%      of functions of arbitrary arity.
%%% @private

-module(vararg).
-export([parse_transform/2]).

-include("proper_internal.hrl").

%% 'MAKE_FUN'(Arity,Handler,Err) will be replaced with this case clause:
%%    case Arity of
%%        0 -> fun() -> Handler([]) end;
%%        1 -> fun(X1) -> Handler([X1]) end;
%%        2 -> fun(X2,X1) -> Handler([X2,X1]) end;
%%        3 -> fun(X3,X2,X1) -> Handler([X3,X2,X1]) end;
%%        ...
%%        k -> fun(Xk,...,X1) -> Handler([Xk,...,X1]) end;
%%        _ -> Err()
%%    end
%% where k = ?MAX_ARITY

%% CAUTION: This conversion works on a syntactic level:
%% 'Arity' will usually be a variable or a function call (it certainly doesn't
%% make sense for it to be a simple numeric value).
%% 'Handler' gets copied many times, therefore it should not be a complex
%% expression. It will usually be a variable, an external fun declaration, or
%% even simply the name of a local function.
%% 'Err' can be anything that evaluates to a 0-arity fun value.


%%------------------------------------------------------------------------------
%% Top-level functions
%%------------------------------------------------------------------------------

-spec parse_transform([abs_form()], [compile:option()]) -> [abs_form()].
parse_transform(Forms, _Options) ->
    process(Forms).

-spec process(term()) -> term().
process({call,_,{atom,_,'MAKE_FUN'},[Arity,Handler,Err]}) ->
    add_vararg_wrapper(Arity, Handler, Err);
process(List) when is_list(List) ->
    [process(X) || X <- List];
process(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(process(tuple_to_list(Tuple)));
process(Other) ->
    Other.

-spec add_vararg_wrapper(abs_expr(), abs_expr(), abs_expr()) -> abs_expr().
add_vararg_wrapper(Arity, Handler, Err) ->
    RevClauses = wrapper_clauses(?MAX_ARITY, Handler),
    L = ?anno(0),
    CatchAll = {clause,L,[{var,L,'_'}],[],[{call,L,Err,[]}]},
    Clauses = lists:reverse([CatchAll | RevClauses]),
    {'case',L,Arity,Clauses}.

-spec wrapper_clauses(arity(), abs_expr()) -> [abs_clause(),...].
wrapper_clauses(MaxArity, Handler) ->
    L = ?anno(0),
    wrapper_clauses(0, MaxArity, Handler, [], [], {nil,L}).

-spec wrapper_clauses(arity(), arity(), abs_expr(), [abs_clause()],
		      [abs_expr()], abs_expr()) -> [abs_clause(),...].
wrapper_clauses(MaxArity, MaxArity, Handler, Clauses, Args, ArgsList) ->
    FinalClause = wrapper_clause(MaxArity, Handler, Args, ArgsList),
    [FinalClause | Clauses];
wrapper_clauses(N, MaxArity, Handler, Clauses, Args, ArgsList) ->
    NewClause = wrapper_clause(N, Handler, Args, ArgsList),
    NewClauses = [NewClause | Clauses],
    L = ?anno(0),
    NewArg = {var,L,list_to_atom("X" ++ integer_to_list(N+1))},
    NewArgs = [NewArg | Args],
    NewArgsList = {cons,L,NewArg,ArgsList},
    wrapper_clauses(N+1, MaxArity, Handler, NewClauses, NewArgs, NewArgsList).

-spec wrapper_clause(arity(), abs_expr(), [abs_expr()], abs_expr()) ->
	  abs_clause().
wrapper_clause(N, Handler, Args, ArgsList) ->
    L = ?anno(0),
    Body = [{call,L,Handler,[ArgsList]}],
    Fun = {'fun',L,{clauses,[{clause,L,Args,[],Body}]}},
    {clause,L,[{integer,L,N}],[],[Fun]}.
