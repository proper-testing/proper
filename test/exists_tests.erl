%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2018, Andreas Löscher <andreas.loscher@it.uu.se>
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

-module(exists_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPER_OPTIONS, [quiet, {search_steps, 1000}, noshrink]).
-define(PROPER_OPTIONS_SHRINKING, [quiet, {search_steps, 1000}]).
-define(timeout(Timeout, Tests), {timeout, Timeout, Tests}).

%% Backwards Compatibility Test
prop_strategy() ->
  ?STRATEGY(proper_sa,
            ?FORALL(X, ?TARGET(#{gen => integer()}),
                    begin
                      ?MAXIMIZE(X),
                      X < 10
                    end)).

prop_forall_sa() ->
  ?FORALL_SA(X, ?TARGET(#{gen => integer()}),
             begin
               ?MAXIMIZE(X),
               X < 10
             end).

strategy_test() ->
  false = proper:quickcheck(prop_strategy(), ?PROPER_OPTIONS_SHRINKING),
  [10] = proper:counterexample(),
  ok.

forall_sa_test() ->
  false = proper:quickcheck(prop_forall_sa(), ?PROPER_OPTIONS_SHRINKING),
  [10] = proper:counterexample(),
  ok.

%% Macros Test
prop_exists() ->
  ?FORALL(X, integer(),
          ?EXISTS(I, integer(),
                  begin
                    ?MAXIMIZE(I),
                    I > X
                  end)).

prop_not_exists() ->
  ?NOT_EXISTS(I, integer(),
              begin
                ?MAXIMIZE(I),
                I >= 10
              end).

prop_forall_targeted() ->
  ?FORALL_TARGETED(I, integer(),
                   begin
                     ?MAXIMIZE(I),
                     I < 10
                   end).

exists_test() ->
  ?assert(proper:quickcheck(prop_exists(), ?PROPER_OPTIONS)).

not_exists_test() ->
  false = proper:quickcheck(prop_not_exists(), ?PROPER_OPTIONS_SHRINKING),
  [10] = proper:counterexample(),
  ok.

forall_targeted_test() ->
  false = proper:quickcheck(prop_forall_targeted(), ?PROPER_OPTIONS_SHRINKING),
  [10] = proper:counterexample(),
  ok.

%% configuration tests
not_exists_const_temp_test() ->
  put(proper_sa_tempfunc, fun (_, _, _, K, _, _) -> {1.0, K} end),
  false = proper:quickcheck(prop_not_exists(), ?PROPER_OPTIONS_SHRINKING),
  erase(proper_sa_tempfunc),
  [10] = proper:counterexample(),
  ok.

hillclimbing_test() ->
  false = proper:quickcheck(prop_not_exists(), [{search_strategy, hill_climbing} | ?PROPER_OPTIONS_SHRINKING]),
  [10] = proper:counterexample(),
  ok.

hillclimbing2_test() ->
  put(proper_sa_acceptfunc, hillclimbing),
  false = proper:quickcheck(prop_not_exists(), ?PROPER_OPTIONS_SHRINKING),
  [10] = proper:counterexample(),
  erase(proper_sa_acceptfunc),
  ok.

hillclimbing3_test() ->
  put(proper_sa_acceptfunc, fun (Old, New, _) -> New > Old end),
  false = proper:quickcheck(prop_not_exists(), ?PROPER_OPTIONS_SHRINKING),
  [10] = proper:counterexample(),
  erase(proper_sa_acceptfunc),
  ok.

default_accept_function_test() ->
  put(proper_sa_acceptfunc, wrong_value),
  false = proper:quickcheck(prop_not_exists(), ?PROPER_OPTIONS_SHRINKING),
  [10] = proper:counterexample(),
  erase(proper_sa_acceptfunc),
  ok.



%% Generator
-spec integer_test() -> 'ok'.
integer_test() ->
  put(proper_sa_testing, true),
  proper:global_state_init_size(10),
  proper_gen_next:init(),
  Gen = proper_types:integer(),
  #{next := TG} = proper_gen_next:from_proper_generator(Gen),
  %% apply the generator 100 times and check that nothing crashes
  appl(TG, 0, 100),
  proper_gen_next:cleanup(),
  ok.

-spec list_test() -> 'ok'.
list_test() ->
  put(proper_sa_testing, true),
  proper:global_state_init_size(10),
  proper_gen_next:init(),
  Gen = proper_types:list(atom),
  #{next := TG} = proper_gen_next:from_proper_generator(Gen),
  %% apply the generator 100 times and check that nothing crashes
  appl(TG, [], 100),
  proper_gen_next:cleanup(),
  ok.

-spec combine_test() -> 'ok'.
combine_test() ->
  put(proper_sa_testing, true),
  proper:global_state_init_size(10),
  proper_gen_next:init(),
  Gen = proper_types:list(proper_types:list(proper_types:integer())),
  #{next := TG} = proper_gen_next:from_proper_generator(Gen),
  %% apply the generator 100 times and check that nothing crashes
  appl(TG, [], 100),
  ok.

appl(_, A, 0) -> A;
appl(TG, A, X) -> appl(TG, TG(A, 0.5), X - 1).

-spec biglist_test() -> 'ok'.
biglist_test() ->
  put(proper_sa_testing, true),
  false = proper:quickcheck(prop_big_list(), ?PROPER_OPTIONS_SHRINKING),
  [L] = proper:counterexample(),
  ?assertMatch(49,length(L)).

prop_big_list() ->
  Gen = proper_types:list(atom),
  ?NOT_EXISTS(List, Gen,
              begin
                L = length(List),
                ?MINIMIZE(abs(L - 50)),
                abs(L - 50) < 2
              end).

-spec let_test() -> 'ok'.
let_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_let(), ?PROPER_OPTIONS)).

prop_let() ->
  ?NOT_EXISTS(V, even_int(),
              begin
                ?MINIMIZE(V),
                V rem 2 =/= 0
              end).

even_int() ->
  ?LET(I, integer(), I*2).

-spec suchthat_test() -> 'ok'.
suchthat_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_suchthat(), ?PROPER_OPTIONS)).

prop_suchthat() ->
  ?NOT_EXISTS(V, suchthat_gen(),
              begin
                ?MAXIMIZE(V),
                V rem 2 =/= 0
              end).

suchthat_gen() ->
  ?SUCHTHAT(I, integer(), I rem 2 =:= 0).

-spec union_test() -> 'ok'.
union_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_union(), ?PROPER_OPTIONS)).

prop_union() ->
  L = [a, b, c],
  ?NOT_EXISTS(X, proper_types:union(L), not lists:member(X, L)).

-spec weighted_union_test() -> 'ok'.
weighted_union_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_weighted_union(), ?PROPER_OPTIONS)).

prop_weighted_union() ->
  Gen = proper_types:weighted_union([{1, a}, {2, b}, {3, c}]),
  ?NOT_EXISTS(X, Gen, not lists:member(X, [a, b, c])).

-spec tuple_test() -> 'ok'.
tuple_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_tuple(), ?PROPER_OPTIONS)).

prop_tuple() ->
  ?NOT_EXISTS({L, R}, tuple_type_res(), L =< R).

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
  ?NOT_EXISTS(_E, union_let_type(C), false).

union_let_type(C) ->
  ?LET(E, union(C), E).

-spec lazy_test() -> 'ok'.
lazy_test() ->
  put(proper_sa_testing, true),
  ?assert(proper:quickcheck(prop_lazy(), ?PROPER_OPTIONS)).

prop_lazy() ->
  Gen = ?LAZY(?LET(I, integer(), I * 2)),
  ?NOT_EXISTS(I, Gen, I rem 2 =/= 0).

-spec sized_test() -> 'ok'.
sized_test() ->
  put(proper_sa_testing, true),
  false = proper:quickcheck(prop_sized(), ?PROPER_OPTIONS),
  [C] = proper:counterexample(),
  ?assert(42 =< length(C)).

prop_sized() ->
  ?NOT_EXISTS(L, sized_type(),
              begin
                ?MAXIMIZE(lists:sum(L)),
                length(L) >= 42
              end).

sized_type() ->
  ?SIZED(S, lists:seq(0, S)).

-spec edge_test() -> 'ok'.
edge_test() ->
  ?assert(proper:quickcheck(prop_edge(), ?PROPER_OPTIONS)).

prop_edge() ->
  Gen = simple_edge([1,2,3,4,5,6,7,8,9]),
  ?NOT_EXISTS({L, R}, Gen, L =< R).

-spec graph_test_() -> 'ok'.
graph_test_() ->
  ?timeout(10, ?_assert(proper:quickcheck(prop_graph(), ?PROPER_OPTIONS))).

prop_graph() ->
  ?NOT_EXISTS(_, simple_graph(), false).

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
  ?NOT_EXISTS(_L, il_type(), false).

-spec improper_list_test() -> 'ok'.
improper_list_test() ->
  put(proper_sa_tempfunc, default),
  put(proper_sa_acceptfunc, default),
  ?assert(proper:quickcheck(prop_il(), ?PROPER_OPTIONS)).

prop_reset() ->
  ?NOT_EXISTS(I, ?USERNF(exactly(0), fun (Base, _) -> Base + 1 end),
              begin
                ?MAXIMIZE(I),
                case I < 10 of
                  true -> ok;
                  false -> proper_sa:reset()
                end,
                %% I will be 10 when resetting
                %% and then 0 in the next run
                I > 10
              end).

reset_test() ->
  ?assert(proper:quickcheck(prop_reset(), ?PROPER_OPTIONS)).

%% graph with matching
graph_duplicated_edges() ->
  ?LET(Vn, integer(2, 42),
       begin
         Vs = lists:seq(1, Vn),
         {Vs, list(simple_edge(Vs))}
       end).

matching_graph() ->
  ?LET({Vs, Es}, graph_duplicated_edges(),
       {Vs, lists:usort(Es)}).

-spec graph_match1_test_() -> 'ok'.
graph_match1_test_() ->
  Opts = ?PROPER_OPTIONS,
  ?timeout(60, ?_assert(proper:quickcheck(prop_graph_match_corr(), Opts))).


-spec graph_match2_test_() -> 'ok'.
graph_match2_test_() ->
  Opts = ?PROPER_OPTIONS,
  ?timeout(60, ?_assert(proper:quickcheck(prop_graph_match_perf(), Opts))).

prop_graph_match_perf() ->
  ?EXISTS({V, E}, matching_graph(),
          begin
            UV = length(V) - length(E),
            ?MAXIMIZE(UV),
            UV =:= 42
          end).

prop_graph_match_corr() ->
  ?NOT_EXISTS({V, E},
              matching_graph(),
              begin
                UV = length(V) - length(E),
                ?MAXIMIZE(UV),
                CorrectEdges = lists:foldr(fun ({L, R}, AccIn) ->
                                               AccIn andalso
                                                 lists:member(L, V) andalso
                                                 lists:member(R, V)
                                           end, true, E),
                not CorrectEdges
              end).

-spec whenfail_test() -> 'ok'.
whenfail_test() ->
  put(test_token, false),
  false = proper:quickcheck(prop_whenfail(), ?PROPER_OPTIONS),
  ?assert(get(test_token)).

prop_whenfail() ->
  ?NOT_EXISTS(_, integer(),
              ?WHENFAIL(put(test_token, true), true)).

-spec shrink1_test() -> 'ok'.
shrink1_test() ->
  false = proper:quickcheck(prop_shrink1(), ?PROPER_OPTIONS_SHRINKING),
  ?assertMatch([1050], proper:counterexample()).

prop_shrink1() ->
  ?FORALL(I, integer(1000, 1100),
          ?EXISTS(J, integer(1000, 1050),
                  begin
                    ?MAXIMIZE(J),
                    J > I
                  end)).

-spec shrink2_test() -> 'ok'.
shrink2_test() ->
  false = proper:quickcheck(prop_shrink2(), ?PROPER_OPTIONS_SHRINKING),
  ?assertMatch([0, 1], proper:counterexample()).

prop_shrink2() ->
  ?FORALL(I, integer(0, 100),
          ?NOT_EXISTS(J, integer(-50, 50),
                      begin
                        ?MAXIMIZE(J),
                        J > I
                      end)).

-spec shrink3_test() -> 'ok'.
shrink3_test() ->
  false = proper:quickcheck(prop_shrink3(), ?PROPER_OPTIONS_SHRINKING),
  ?assertMatch(undefined, proper:counterexample()).

prop_shrink3() ->
  ?EXISTS(_, integer(), false).

-spec shrink4_test() -> 'ok'.
shrink4_test() ->
  false = proper:quickcheck(prop_shrink4(), ?PROPER_OPTIONS_SHRINKING),
  ?assertMatch([0], proper:counterexample()).

prop_shrink4() ->
  ?NOT_EXISTS(I, integer(0, 20),
              begin
                ?MINIMIZE(I),
                I < 10
              end).

matching_type() ->
  ?LET(I, integer(), I).

prop_match() ->
  ?FORALL_TARGETED(I, ?USERMATCHER(matching_type(), fun proper_gen_next:match/3),
                   begin
                     ?MAXIMIZE(I),
                     I < 10
                   end).

-spec match_test() -> 'ok'.
match_test() ->
  false = proper:quickcheck(prop_match(), ?PROPER_OPTIONS_SHRINKING),
  ?assertMatch([10], proper:counterexample()).

let_integer() ->
  ?LET(I, integer(), I+1).

let_list_type() ->
  ?LET(L, vector(10, let_integer()), L ++ [0]).

prop_match_and_shrink() ->
  ?FORALL_TARGETED(L, let_list_type(),
                   begin
                     I = lists:sum(L),
                     ?MAXIMIZE(I),
                     I < 100
                   end).

-spec match_and_shrink_test() -> 'ok'.
match_and_shrink_test() ->
  false = proper:quickcheck(prop_match_and_shrink(), ?PROPER_OPTIONS_SHRINKING),
  [L] = proper:counterexample(),
  ?assertEqual(100, lists:sum(L)).

let_nf() ->
  fun (PrevList, _) ->
      ?LET(I, exactly(ok), PrevList ++ [I])
  end.

prop_let_nf() ->
  ?FORALL_TARGETED(L, ?USERNF(list(ok), let_nf()),
  begin
    ?MAXIMIZE(length(L)),
    length(L) < 20
  end).

-spec let_in_nf_test() -> 'ok'.
let_in_nf_test() ->
  false = proper:quickcheck(prop_let_nf(), ?PROPER_OPTIONS_SHRINKING),
  [L] = proper:counterexample(),
  ?assertEqual(20, length(L)).

prop_not_exists_crash() ->
  ?NOT_EXISTS(_, integer(), error(crash)).

-spec count_crash_in_not_exists_as_failure_test() -> 'ok'.
count_crash_in_not_exists_as_failure_test() ->
  ?assertEqual(false, proper:quickcheck(prop_not_exists_crash(), ?PROPER_OPTIONS)).
