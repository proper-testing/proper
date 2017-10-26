%%% coding: latin-1
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

%%% @doc This module defines the top-level behaviour for targeted
%%% property-based testing (TPBT). Using TPBT the input generation
%%% is no longer random, but guided by a search strategy to increase
%%% the probability of finding failing input. For this to work the user
%%% has to specify a search strategy and also needs to extract
%%% utility-values from the system under test that the search strategy
%%% then tries to maximize.
%%%
%%% The typical structure of a property using TPBT looks as follows:
%%% ```prop_target() ->
%%%      ?TARGET_STRATEGY(SearchStrategy,
%%%      ?FORALL(Input, ?TARGET(Params),
%%%              begin                  % for the input generation.
%%%                UV = SUT:run(Input), % Do so by running SUT with Input
%%%                ?MAXIMIZE(UV),       % and maximize its Utility Value
%%%                UV < Threshold       % up to some Threshold.
%%%              end)).'''
%%%
%%% == Macros ==
%%%
%%% <dl>
%%%   <dt>`?TARGET_STRATEGY(<Strategy>, <Prop>)'</dt>
%%%   <dd>This macro defines that `<Strategy>' should be used as search strategy
%%%       to produce input for `<Prop>'. The currently available search strategies
%%%       are `simulated_annealing' and `hill_climbing'. Alternatively a users can
%%%       define their own strategy. In this case the module name containing the
%%%       implementation should be given as argument.</dd>
%%%   <dt>`?TARGET(<Options>)'</dt>
%%%   <dd>This macro specifies a targeted generator that is under the control of the search strategy.
%%%       The `<Options>' are specific to the search strategy.</dd>
%%%   <dt>`?FORALL_SA(<Xs>, <targeted_gen>, <Prop>)'</dt>
%%%   <dd>equivalent to `?TARGET_STRATEGY(simulated_annealing, ?FORALL(<Xs>, <targeted_gen>, <Prop>))'</dd>
%%%   <dt>`?MAXIMIZE(UV)'</dt>
%%%   <dd>This tells the search strategy to maximize the value `UV'.</dd>
%%%   <dt>`?MINIMIZE(UV)'</dt>
%%%   <dd>equivalent to `?MAXIMIZE(-UV)'</dd>
%%% </dl>

-module(proper_target).

-export([targeted/3, update_target_uvs/2, update_target_uvs/3, use_strategy/3, cleanup_strategy/0]).

-include_lib("proper_common.hrl").

-export_type([key/0, fitness/0, tmap/0]).
-export_type([target_state/0, next_func/0, fitness_func/0,
              target/0, options/0]).

%% -----------------------------------------------------------------------------
%% Type declarations
%% -----------------------------------------------------------------------------

-type key()     :: nonempty_string() | reference().
-type fitness() :: number().
-type tmap()    :: #{atom() => term()}.

-type threshold() :: fitness() | 'inf'.

-type target_state() :: term().
-type next_func()    :: fun ((target_state()) -> {target_state(), any()}).
-type fitness_func() :: fun ((target_state(), fitness()) -> target_state()).

-type target()    :: {target_state(), next_func(), fitness_func()}.
-type options()   :: [{atom(), term()}].
-type strategy()  :: module().

%% -----------------------------------------------------------------------------
%% proper_target callback functions for defining strategies
%% ----------------------------------------------------------------------------
%% strategy global initializer
-callback init_strategy(proper:outer_test(), proper:setup_opts()) -> proper:outer_test().
%%
-callback cleanup() -> 'ok'.
%% target (one variable) initializer
-callback init_target(tmap()) -> target().
%% generator for shrinking
-callback get_shrinker(tmap()) -> proper_types:type().
%% store, and retrieve state
-callback store_target(key(), target_state()) -> 'ok'.
-callback retrieve_target(key()) -> target() | 'undefined'.
%% update the strategy with the fitness
-callback update_global_fitness(fitness()) -> 'ok'.

%% @private
-spec targeted(key(), proper_types:type(), tmap()) -> proper_types:type().
targeted(Key, Gen, TMap) ->
  ?SHRINK(proper_types:exactly(?LAZY(targeted_gen(Key, Gen, TMap))),
          [shrink_gen(TMap)]).

%% @private
targeted_gen(Key, Gen, TMap) ->
  {State, NextFunc, _FitnessFunc} = get_target(Key, TMap),
  {NewState, NextValue} = NextFunc(State),
  update_target(Key, NewState),
  Gen(NextValue).

%% @private
-spec update_target_uvs(fitness(), threshold()) -> boolean().
update_target_uvs(Fitness, Threshold) ->
  set_fitness(Fitness),
  check_threshold(Threshold, Fitness).

%% @private
-spec update_target_uvs(fitness(), threshold(), key()) -> boolean().
update_target_uvs(Fitness, Threshold, Key) ->
  set_fitness(Fitness, Key),
  check_threshold(Threshold, Fitness).

%% @private
check_threshold(Threshold, Fitness) ->
  case Threshold of
    inf -> true;
    _ -> Fitness < Threshold
  end.

%% @private
set_fitness(Fitness, Key) ->
  {State, _NextFunc, FitnessFunc} = get_target(Key, []),
  NewState = FitnessFunc(State, Fitness),
  update_target(Key, NewState).

%% @private
set_fitness(Fitness) ->
  update_global(Fitness).

%% target_strategy

%% access to the current strategy
-define(STRATEGY, get(target_strategy)).

%% store the used strategy into the process dictionary
%% @private
-spec use_strategy(strategy(), any(), proper:setup_opts()) -> proper:outer_test().
use_strategy(Strat, Prop, Opts) ->
  Strategy = case Strat of
               simulated_annealing ->
                 proper_sa;
               hill_climbing ->
                 put(target_sa_acceptfunc, hillclimbing),
                 proper_sa;
               _ ->
                 Strat
             end,
  put(target_strategy, Strategy),
  Strategy:init_strategy(Prop, Opts).

%% @private
-spec cleanup_strategy() -> ok.
cleanup_strategy() ->
  (?STRATEGY):cleanup().

%% @private
-spec get_target(key(), options()) -> target().
get_target(Key, Opts) ->
  Strategy = ?STRATEGY,
  case Strategy:retrieve_target(Key) of
    undefined ->
      FreshTarget = Strategy:init_target(Opts),
      Strategy:store_target(Key, FreshTarget),
      FreshTarget;
    StoredTarget ->
      StoredTarget
  end.

%% @private
-spec update_target(key(), target_state()) -> 'ok'.
update_target(Key, State) ->
  {_, N, F} = (?STRATEGY):retrieve_target(Key),
  (?STRATEGY):store_target(Key, {State, N, F}).

%% @private
-spec update_global(fitness()) -> 'ok'.
update_global(Fitness) ->
  (?STRATEGY):update_global_fitness(Fitness).

%% @private
-spec shrink_gen(options()) -> proper_types:type().
shrink_gen(Opts) ->
  (?STRATEGY):get_shrinker(Opts).
