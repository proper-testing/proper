%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2010-2020 Manolis Papadakis <manopapad@gmail.com>,
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

%%% @copyright 2020 Spiros Dontas and Kostis Sagonas
%%% @version {@version}
%%% @author Spiros Dontas

%%% @doc This module contains the examples tests.

-module(examples_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


%% -----------------------------------------------------------------------------
%% Macros
%% -----------------------------------------------------------------------------


-define(_passes(Test), ?_passes(Test, [])).

-define(_passes(Test, Opts),
        ?_assertEqual(true, proper:quickcheck(Test, Opts))).

-define(_fails(Test), ?_fails(Test, [])).

-define(_fails(Test, Opts),
        ?_test(
           begin
             Result = proper:quickcheck(Test, Opts),
             CExm = proper:counterexample(),
             proper:clean_garbage(),
             ?assertEqual(false, Result),
             ?checkCExm(CExm, Test, Opts)
           end)).

-define(_failsWith(ExpectedCExm, Test), ?_failsWith(ExpectedCExm, Test, [])).

-define(_failsWith(ExpectedCExm, Test, Opts),
        ?_test(
           begin
             Result = proper:quickcheck(Test, Opts),
             CExm = proper:counterexample(),
             proper:clean_garbage(),
             ?assertEqual(false, Result),
             ?assertMatch(ExpectedCExm, CExm),
             ?checkCExm(CExm, Test, Opts)
           end)).

-define(checkCExm(CExm, Test, Opts),
        ?assertEqual(false, proper:check(Test, CExm, Opts))).


%%------------------------------------------------------------------------------
%% Test that the examples work
%%------------------------------------------------------------------------------


%% test the properties of the `magic' example.
example_magic_props_test_() ->
  %% no point shrinking testx executed only for checking that they fail
  FailOpts = [{numtests,10000}, noshrink],
  [?_passes(magic:prop_spells_random(), [500]),  % let's hope we are unlucky
   {timeout, 180, ?_fails(magic:prop_spells_targeted_auto(), FailOpts)},
   {timeout, 180, ?_fails(magic:prop_spells_targeted_user(), FailOpts)}].

%% test the unary properties of the `labyrinth' example.
example_labyrinth_props_test_() ->
  FailOpts = [{numtests,7500}, noshrink],        % see comment above
  M0 = labyrinth:maze(0), M1 = labyrinth:maze(1), M2 = labyrinth:maze(2),
  [?_failsWith([[left,left,left,left,left,left]],       % run 500 tests,
               labyrinth:prop_exit_random(M0), [500]),  % for safety
   ?_failsWith([[left,left,left,left,left,left]],
               labyrinth:prop_exit_targeted_user(M0)),
   {timeout, 42, ?_fails(labyrinth:prop_exit_targeted_user(M1), FailOpts)},
   {timeout, 42, ?_fails(labyrinth:prop_exit_targeted_user(M2), FailOpts)},
   {timeout, 42, ?_fails(labyrinth:prop_exit_targeted_auto(M2), FailOpts)}].

%% test the unary properties of the `mastermind' example.
example_mastermind_props_test_() ->
  Properties = [prop_all_produced_solutions_are_valid,
                %% prop_secret_combination_is_not_discarded,
                prop_invalidated_instances_reject_original_secret],
  Strategies = [heur, simple, stream],
  [?_passes(mastermind:prop_secret_combination_is_not_discarded(heur)),
   {timeout, 20,
    ?_passes(mastermind:prop_secret_combination_is_not_discarded(simple))}
   |[{timeout, 10,
      ?_passes(mastermind:Prop(S))} || Prop <- Properties, S <- Strategies]].

%% test the properties of `car_statem' example.
example_car_statem_props_test_() ->
  FailOpts = [{numtests,1000}, {constraint_tries,1000}, noshrink],
  [{timeout, 42, ?_passes(car_statem:prop_distance(), [500])},
   {timeout, 600, ?_fails(car_statem:prop_distance_targeted(), FailOpts)},
   {timeout, 600, ?_fails(car_statem:prop_distance_targeted_init(), FailOpts)}].

%% test the properties of `car_fsm' example.
example_car_fsm_props_test_() ->
  FailOpts = [{numtests,1000}, {constraint_tries,1000}, noshrink],
  [{timeout, 42, ?_passes(car_fsm:prop_distance(), [500])},
   {timeout, 600, ?_fails(car_fsm:prop_distance_targeted(), FailOpts)},
   {timeout, 600, ?_fails(car_fsm:prop_distance_targeted_init(), FailOpts)}].
