%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2017, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                and  Konstantinos Sagonas <kostis@it.uu.se>
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

%%% @copyright 2017 Andreas Löscher and Kostis Sagonas
%%% @version {@version}
%%% @author Andreas Löscher

-module(target_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPER_OPTIONS, [quiet, {numtests, 1000}]).

-spec sa_test() -> 'ok'.
sa_test() ->
  true = proper:quickcheck(prop_sa(), ?PROPER_OPTIONS),
  ok.

prop_sa() ->
  ?STRATEGY(proper_sa,
	    ?FORALL({I, J}, {?TARGET(proper_sa:integer()), ?TARGET(proper_sa:integer())},
		    begin
		      ?MAXIMIZE(-(I+J)),
		      true
		    end)).

-spec shrinking_test() -> 'ok'.
shrinking_test() ->
  false = proper:quickcheck(prop_shrinking(), ?PROPER_OPTIONS),
  [1000] = proper:counterexample(),
  ok.

prop_shrinking() ->
  ?STRATEGY(proper_sa,
	    proper:numtests(1000,
			    ?FORALL(I, ?TARGET(proper_sa:integer()),
				    begin
				      ?MAXIMIZE(I),
				      check(I)
				    end))).

check(I) ->
  I < 1000.

-spec integer_test() -> 'ok'.
integer_test() ->
  put(proper_sa_testing, true),
  proper:global_state_init_size(10),
  Gen = proper_types:integer(),
  #{next := TG} = proper_sa_gen:from_proper_generator(Gen),
  %% apply the generator 100 times and check that nothing crashes
  appl(TG, 0, 100),
  ok.

-spec list_test() -> 'ok'.
list_test() ->
  put(proper_sa_testing, true),
  proper:global_state_init_size(10),
  Gen = proper_types:list(atom),
  #{next := TG} = proper_sa_gen:from_proper_generator(Gen),
  %% apply the generator 100 times and check that nothing crashes
  appl(TG, [], 100),
  ok.

-spec combine_test() -> 'ok'.
combine_test() ->
  put(proper_sa_testing, true),
  proper:global_state_init_size(10),
  Gen = proper_types:list(proper_types:list(proper_types:integer())),
  #{next := TG} = proper_sa_gen:from_proper_generator(Gen),
  %% apply the generator 100 times and check that nothing crashes
  appl(TG, [], 100),
  ok.

appl(_, A, 0) -> A;
appl(TG, A, X) -> appl(TG, TG(A, 0.5), X - 1).


-spec basic1_test() -> 'ok'.
basic1_test() ->
  put(proper_sa_testing, true),
  false = proper:quickcheck(prop_big_list(), ?PROPER_OPTIONS),
  [L] = proper:counterexample(),
  ?assertMatch(48,length(L)).

prop_big_list() ->
  Gen = proper_types:list(atom),
  ?FORALL_SA(List, ?TARGET(proper_sa_gen:from_proper_generator(Gen)),
             begin
               L = length(List),
               ?MAXIMIZE(-abs(L - 50)),
               abs(L - 50) > 2
             end).


-spec basic2_test() -> 'ok'.
basic2_test() ->
  put(proper_sa_testing, true),
  false = proper:quickcheck(prop_use_ngenerator(), ?PROPER_OPTIONS),
  [L] = proper:counterexample(),
  ?assertMatch(48,length(L)).

prop_use_ngenerator() ->
  ?FORALL_SA(List, ?TARGET(#{gen => proper_types:list(atom)}),
             begin
               L = length(List),
               ?MAXIMIZE(-abs(L - 50)),
               abs(L - 50) > 2
             end).


-spec let_test() -> 'ok'.
let_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_let(), ?PROPER_OPTIONS)).

prop_let() ->
  ?FORALL_SA(V, ?TARGET(#{gen => even_int()}),
             begin
               ?MAXIMIZE(-V),
               V rem 2 =:= 0
             end).

even_int() ->
  ?LET(I, integer(), I*2).


-spec suchthat_test() -> 'ok'.
suchthat_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_suchthat(), ?PROPER_OPTIONS)).

prop_suchthat() ->
  ?FORALL_SA(V, ?TARGET(#{gen => suchthat_gen()}),
             begin
               ?MAXIMIZE(V),
               V rem 2 =:= 0
             end).

suchthat_gen() ->
  ?SUCHTHAT(I, integer(), I rem 2 =:= 0).


-spec union_test() -> 'ok'.
union_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_union(), ?PROPER_OPTIONS)).

prop_union() ->
  L = [a, b, c],
  ?FORALL_SA(X, ?TARGET(#{gen => proper_types:union(L)}), lists:member(X, L)).


-spec weighted_union_test() -> 'ok'.
weighted_union_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_weighted_union(), ?PROPER_OPTIONS)).

prop_weighted_union() ->
  Gen = proper_types:weighted_union([{1, a}, {2, b}, {3, c}]),
  ?FORALL_SA(X, ?TARGET(#{gen => Gen}), lists:member(X, [a, b, c])).


-spec tuple_test() -> 'ok'.
tuple_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_tuple(), ?PROPER_OPTIONS)).

prop_tuple() ->
  ?FORALL_SA({L, R}, ?TARGET(#{gen => tuple_type_res()}), L > R).

tuple_type_res() ->
  ?SUCHTHAT({V1, V2}, tuple_type(), V1 > V2).

tuple_type() ->
  proper_types:tuple([integer(), integer()]).


-spec let_union_test() -> 'ok'.
let_union_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_union_let(), ?PROPER_OPTIONS)).

prop_union_let() ->
  C = lists:seq(0,1),
  ?FORALL_SA(_E, ?TARGET(#{gen => union_let_type(C)}), true).

union_let_type(C) ->
  ?LET(E, union(C), E).

-spec lazy_test() -> 'ok'.
lazy_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_lazy(), ?PROPER_OPTIONS)).

prop_lazy() ->
  Gen = ?LAZY(?LET(I, integer(), I * 2)),
  ?FORALL_SA(I, ?TARGET(#{gen => Gen}), I rem 2 =:= 0).


-spec sized_test() -> 'ok'.
sized_test() ->
  put(proper_sa_testing, true),
  false = proper:quickcheck(prop_sized(), ?PROPER_OPTIONS),
  [C] = proper:counterexample(),
  ?assert(42 =< length(C)).

prop_sized() ->
  ?FORALL_SA(L, ?TARGET(#{gen => sized_type()}),
             begin
               ?MAXIMIZE(lists:sum(L)),
               length(L) < 42
             end).

sized_type() ->
  ?SIZED(S, lists:seq(0, S)).


-spec edge_test() -> 'ok'.
edge_test() ->
  ?assert(proper:quickcheck(prop_edge(), ?PROPER_OPTIONS)).

prop_edge() ->
  Gen = simple_edge([1,2,3,4,5,6,7,8,9]),
  ?FORALL_SA({L, R}, ?TARGET(proper_sa_gen:from_proper_generator(Gen)), L > R).


-spec graph_test() -> 'ok'.
graph_test() ->
  put(proper_sa_tempfunc, default),
  put(proper_sa_acceptfunc, default),
  ?assert(proper:quickcheck(prop_graph(), ?PROPER_OPTIONS)).

prop_graph() ->
  ?FORALL_SA({V, E},
             ?TARGET(proper_sa_gen:from_proper_generator(simple_graph())),
             begin
               ?MAXIMIZE((length(E) - length(V))),
               true
             end).

%% simple generator for a graph
simple_graph() ->
  ?LET(RawV, non_empty(list(integer(1, inf))),
       begin
         V = lists:usort(RawV),
         case length(V) > 1 of
           true ->
             ?LET(E, simple_edges(V), {V, E});
           _ ->
             {V, []}
         end
       end).

simple_edges(V) ->
  ?LET(Edges, list(simple_edge(V)), lists:usort(Edges)).

simple_edge(V) ->
  ?SUCHTHAT({V1, V2}, {oneof(V), oneof(V)}, V1 > V2).

%% improper lists
il_type() ->
  ?LET(I, integer(), [I|42]).

prop_il() ->
  ?FORALL_SA(_L, ?TARGET(#{gen => il_type()}), true).

-spec improper_list_test() -> 'ok'.
improper_list_test() ->
  put(proper_sa_tempfunc, default),
  put(proper_sa_acceptfunc, default),
  ?assert(proper:quickcheck(prop_il(), ?PROPER_OPTIONS)).


prop_reset() ->
  ?STRATEGY(hill_climbing,
	    ?FORALL(I, ?TARGET(stepint()),
		    begin
		      ?MAXIMIZE(I),
		      case I < 10 of
			true -> ok;
			false -> proper_sa:reset()
		      end,
		      I =< 10
		    end)).

stepint() ->
  #{first => 0,
    next=> fun (Base, _) -> Base + 1 end}.

reset_test() ->
  ?assert(proper:quickcheck(prop_reset(), ?PROPER_OPTIONS)).
