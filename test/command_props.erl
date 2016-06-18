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
%%% @author Eirini Arvaniti

-module(command_props).

-include_lib("proper/include/proper.hrl").

-define(MOD, ets_counter).
-define(MOD1, pdict_statem).

ne_nd_list(ElemType) ->
    ?LET(L, non_empty(list(ElemType)), lists:usort(L)).

short_ne_nd_list(ElemType) ->
    ?LET(L, resize(8, non_empty(list(ElemType))), lists:usort(L)).

no_duplicates(L) ->
    length(L) =:= length(lists:usort(L)).

prop_index() ->
    ?FORALL(List, ne_nd_list(integer()),
	    ?FORALL(X, union(List),
		    lists:nth(proper_statem:index(X, List), List) =:= X)).

prop_all_insertions() ->
    ?FORALL(List, list(integer()),
	    begin
		Len = length(List),
		?FORALL(Limit, range(1,Len+1),
			?FORALL(X, integer(),
				begin
				    AllIns = proper_statem:all_insertions(X, Limit, List),
				    length(AllIns) =:= Limit
				end))
	    end).

prop_insert_all() ->
    ?FORALL(List, short_ne_nd_list(integer()),
	    begin
		Len = length(List),
		{L1, L2} = lists:split(Len div 2, List),
		AllIns = proper_statem:insert_all(L1, L2),
		?WHENFAIL(io:format("~nList: ~w, L1: ~w, L2: ~w~nAllIns: ~w~n",
				    [List,L1,L2,AllIns]),
			  lists:all(fun(L) ->
					    length(L) =:= Len andalso
						no_duplicates(L) andalso
						lists:subtract(L,L2) =:= L1
				    end, AllIns))
	    end).

prop_zip() ->
    ?FORALL({X, Y}, {list(), list()},
	    begin
		LenX = length(X),
		LenY = length(Y),
		Res = if LenX < LenY ->
			      lists:zip(X, lists:sublist(Y, LenX));
			 LenX =:= LenY ->
			      lists:zip(X, Y);
			 LenX > LenY ->
			      lists:zip(lists:sublist(X, LenY), Y)
		      end,
		equals(zip(X, Y), Res)
	    end).

prop_state_after() ->
    ?FORALL(Cmds, proper_statem:commands(?MOD1),
	    begin
		SymbState = proper_statem:state_after(?MOD1, Cmds),
		{_, S, ok} = proper_statem:run_commands(?MOD1, Cmds),
		?MOD1:clean_up(),
		equals(proper_symb:eval(SymbState), S)
	    end).

prop_parallel_ets_counter() ->
    ?FORALL({_Seq, [P1, P2]}, proper_statem:parallel_commands(?MOD),
	    begin
		Len1 = length(P1),
		Len2 = length(P2),
		Len1 =:= Len2 orelse (Len1 + 1) =:= Len2
	    end).

prop_check_true() ->
    ?FORALL({Seq, Par}, proper_statem:parallel_commands(?MOD),
	    begin
		?MOD:clean_up(),
		?MOD:set_up(),
		{{_, State, ok}, Env} = proper_statem:run(?MOD, Seq, []),
		Res = [proper_statem:execute(C, Env, ?MOD) || C <- Par],
		V = proper_statem:check(?MOD, State, Env, false, [], Res),
		equals(V, true)
	    end).
