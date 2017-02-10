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

-module(proper_target).

-export([targeted/3, update_target_uvs/2, update_target_uvs/3, use_strategy/3, cleanup_strategy/0]).

-include_lib("proper_common.hrl").

-export_type([key/0, fitness/0, tmap/0]).
-export_type([target_state/0, next_func/0, fitness_func/0,
              target/0, options/0]).

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


%% behaviour for strategies
%% strategy global initializer
-callback init_strategy(proper:outer_test(), proper:setup_opts()) -> proper:outer_test().
-callback cleanup() -> 'ok'.
%% target (one variable) initializer
-callback init_target(tmap()) -> target().
%% %% generator for shrinking
-callback get_shrinker(tmap()) -> proper_types:type().
%% store, and retrieve state
-callback store_target(key(), target_state()) -> 'ok'.
-callback retrieve_target(key()) -> target() | 'undefined'.
%% global update
-callback update_global_fitness(fitness()) -> 'ok'.

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

-spec update_target_uvs(fitness(), threshold()) -> boolean().
update_target_uvs(Fitness, Threshold) ->
  set_fitness(Fitness),
  check_threshold(Threshold, Fitness).

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

-spec cleanup_strategy() -> ok.
cleanup_strategy() ->
  (?STRATEGY):cleanup().


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

-spec update_target(key(), target_state()) -> 'ok'.
update_target(Key, State) ->
  {_, N, F} = (?STRATEGY):retrieve_target(Key),
  (?STRATEGY):store_target(Key, {State, N, F}).

-spec update_global(fitness()) -> 'ok'.
update_global(Fitness) ->
  (?STRATEGY):update_global_fitness(Fitness).

-spec shrink_gen(options()) -> proper_types:type().
shrink_gen(Opts) ->
  (?STRATEGY):get_shrinker(Opts).
