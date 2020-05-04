%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2017, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                and  Kostis Sagonas <kostis@it.uu.se>
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

%%% @doc This module provides simulated annealing (SA) as search strategy
%%% for targeted property-based testing. SA is a local search meta-heuristic
%%% that can be used to address discrete and continuous optimization problems.
%%%
%%% SA starts with a random initial input. It then produces a random input in
%%% the neighborhood of the previous one and compares the fitness of both. If
%%% the new input has a higher fitness than the previous one, it is accepted
%%% as new best input. SA can also accepts worse inputs with a certain
%%% probability.
%%% (<a target="_blank" href="https://en.wikipedia.org/wiki/Simulated_annealing">more information</a>)

-module(proper_sa).

-behaviour(proper_target).

-include("proper_internal.hrl").

%% -----------------------------------------------------------------------------
%% Exports
%% -----------------------------------------------------------------------------

-export([init_strategy/1, init_target/2, next/2,
         get_shrinker/2, update_fitness/3, reset/2]).

%% -----------------------------------------------------------------------------
%% Macros
%% -----------------------------------------------------------------------------

-define(RANDOM_PROBABILITY, (?RANDOM_MOD:uniform())).
-define(TEMP_FUN, fun(_, _, _, _, _) -> 1.0 end).

%% -----------------------------------------------------------------------------
%% Types
%% -----------------------------------------------------------------------------

-type k() :: non_neg_integer().
-type temp_fun() :: fun((%% old temperature
                         proper_gen_next:temperature(),
                         %% old energy level
                         proper_target:fitness(),
                         %% new energy level
                         proper_target:fitness(),
                         %% k_current
                         k(),
                         %% k_max
                         k(),
                         %% accepted or not
                         boolean()) -> {proper_gen_next:temperature(), k()}).
-type accept_fun() :: fun((proper_target:fitness(), proper_target:fitness(),
                           proper_gen_next:temperature()) -> boolean()).

%% -----------------------------------------------------------------------------
%% Records
%% -----------------------------------------------------------------------------

-record(sa_target,
        {first             = null :: proper_types:type(),
         next              = null :: proper_target:next_fun(),
         current_generated = null :: proper_gen:instance(),
         last_generated    = null :: proper_gen:instance()}).
-type sa_target() :: #sa_target{}.

-record(sa_data,
        {%% search steps
         k_max = 0                      :: k(),
         %% current step
         k_current = 0                  :: k(),
         %% acceptance function
         p = fun (_, _, _) -> false end :: accept_fun(),
         %% fitness
         last_energy = null             :: proper_target:fitness() | null,
         last_update = 0                :: integer(),
         %% temperature
         temperature = 1.0              :: proper_gen_next:temperature(),
         temp_func = ?TEMP_FUN          :: temp_fun()}).
-type sa_data() :: #sa_data{}.

%% -----------------------------------------------------------------------------
%% proper_target callbacks
%% -----------------------------------------------------------------------------

%% Initialize the strategy data based on the
%% number of the search steps and the strategy.

%% @private
-spec init_strategy(proper_target:search_steps()) -> sa_data().
init_strategy(Steps) ->
  #sa_data{k_max = Steps,
           p = get_acceptance_function(),
           temp_func = get_temperature_function()}.

%% Initialize target state based on the initial generator
%% and the neighbourhood function.

%% @private
-spec init_target(proper_types:type(), proper_target:next_fun()) -> sa_target().
init_target(First, Next) ->
  {ok, InitialValue} = proper_gen:safe_generate(First),
  #sa_target{first = First, next = Next, last_generated = InitialValue}.

%% The function which generates the next instances of
%% the targeted generator. It also updates the target state.

%% @private
-spec next(sa_target(), sa_data()) ->
        {proper_gen:instance(), sa_target(), sa_data()}.
next(#sa_target{next = Next, last_generated = LastGen} = Target, Data) ->
  NextGenerator = Next(LastGen, Data#sa_data.temperature),
  {ok, Generated} = proper_gen:safe_generate(NextGenerator),
  {Generated, Target#sa_target{current_generated = Generated}, Data}.

%% The function which returns the generator to use when shrinking.

%% @private
-spec get_shrinker(sa_target(), sa_data()) -> proper_types:type().
get_shrinker(#sa_target{first = Type, current_generated = Generated}, Data) ->
  CleanGenerated = proper_gen:clean_instance(Generated),
  case proper_types:find_prop(user_nf, Type) of
    {ok, NF} ->
      NextType = NF(CleanGenerated, {1, Data#sa_data.temperature}),
      %% Check for shrinkers provided by user with ?SHRINK macro.
      case proper_types:find_prop(alt_gens, NextType) of
        %% User provided ?SHRINK, so we keep it.
        {ok, _} -> NextType;
        %% Try to find which is the best shrinker.
        %% We try to keep the original generator whenever possible.
        error ->
          case proper_types:safe_is_instance(Generated, Type) of
            false ->
              case proper_types:safe_is_instance(CleanGenerated, Type) of
                true -> Type;
                false -> NextType
              end;
            true -> Type
          end
      end;
    error ->
      Type
  end.

%% Update state and data based on current fitness.
%% The current generated value is accepted based on the
%% simulated annealing acceptance function, which always
%% accepts better fitnesses, while accepting worst fitnesses
%% based on the acceptance probability.

%% @private
-spec update_fitness(proper_target:fitness(), sa_target(), sa_data()) ->
        {sa_target(), sa_data()}.
update_fitness(Fitness, Target, Data) ->
  #sa_data{k_current = K_Current,
           k_max = K_Max,
           last_energy = Energy,
           temperature = Temperature,
           temp_func = TempFunc,
           p = P} = Data,
  case (Energy =:= null) orelse P(Energy, Fitness, Temperature) of
    true ->
      %% accept new state
      proper_gen_next:update_caches(accept),
      %% calculate new temperature
      {NewTemperature, AdjustedK} =
        TempFunc(Temperature,
                 Energy,
                 Fitness,
                 K_Max,
                 K_Current,
                 true),
      NewTarget =
        Target#sa_target{last_generated = Target#sa_target.current_generated},
      {NewTarget, Data#sa_data{last_energy = Fitness,
                               last_update = 0,
                               k_current = AdjustedK,
                               temperature = NewTemperature}};
    false ->
      %% reject new state
      proper_gen_next:update_caches(reject),
      %% calculate new temperature
      {NewTemperature, AdjustedK} =
        TempFunc(Temperature,
                 Energy,
                 Fitness,
                 K_Max,
                 K_Current,
                 false),
      {Target, Data#sa_data{last_update = Data#sa_data.last_update + 1,
                            k_current = AdjustedK,
                            temperature = NewTemperature}}
  end.

%% Restart the search strategy from a random input.

%% @private
-spec reset(sa_target(), sa_data()) -> {sa_target(), sa_data()}.
reset(Target, Data) ->
  {ok, ResetValue} = proper_gen:safe_generate(Target#sa_target.first),
  {Target#sa_target{last_generated = ResetValue},
   Data#sa_data{last_energy = null,
                last_update = 0,
                k_max = Data#sa_data.k_max - Data#sa_data.k_current,
                k_current = 0}}.

%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------

acceptance_function_standard(EnergyCurrent, EnergyNew, Temperature) ->
  case EnergyNew > EnergyCurrent of
    true ->
      %% always accept better results
      true;
    false ->
      %% probabilistic acceptance (always between 0.0 and 0.5)
      AcceptanceProbability =
        try
          %%  1 / (1 + math:exp(abs(EnergyCurrent - EnergyNew) / Temperature))
          math:exp(-(EnergyCurrent - EnergyNew) / Temperature)
        catch
          error:badarith -> 0.0
        end,
      %% if random probability is less, accept
      ?RANDOM_PROBABILITY < AcceptanceProbability
  end.

acceptance_function_hillclimbing(EnergyCurrent, EnergyNew, _Temperature) ->
  %% Hill-Climbing
  EnergyNew > EnergyCurrent.

temperature_function_standard_sa(_OldTemperature,
                                 _OldEnergyLevel,
                                 _NewEnergyLevel,
                                 K_Max,
                                 K_Current,
                                 _Accepted) ->
  {1.0 - min(1, K_Current / K_Max), K_Current + 1}.

get_temperature_function() ->
  case get(proper_sa_tempfunc) of
    default -> fun temperature_function_standard_sa/6;
    Fun when is_function(Fun) ->
      case proplists:lookup(arity, erlang:fun_info(Fun)) of
        {arity, 6} -> Fun;
        _ -> fun temperature_function_standard_sa/6
      end;
    undefined -> fun temperature_function_standard_sa/6;
    _ -> fun temperature_function_standard_sa/6
  end.

get_acceptance_function() ->
  case get(proper_sa_acceptfunc) of
    default -> fun acceptance_function_standard/3;
    hillclimbing -> fun acceptance_function_hillclimbing/3;
    Fun when is_function(Fun) ->
      case proplists:lookup(arity, erlang:fun_info(Fun)) of
        {arity, 3} -> Fun;
        _ -> fun acceptance_function_standard/3
      end;
    undefined -> fun acceptance_function_standard/3;
    _ -> fun acceptance_function_standard/3
  end.
