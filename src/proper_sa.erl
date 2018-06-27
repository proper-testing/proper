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
%%% the neighborhood of the previous one and compares the fitnessof both. If
%%% the new input has a higher fitness than the previous one, it is accepted
%%% as new best input. SA can also accepts worse inputs with a certain
%%% probbability.
%%% (<a target="_blank" href="https://en.wikipedia.org/wiki/Simulated_annealing">more information</a>)

-module(proper_sa).

-behaviour(proper_target).

%% callbacks
-export([init_strategy/1,
         init_target/1,
         cleanup/0,
         store_target/2,
         retrieve_target/1,
         update_global_fitness/1,
         get_shrinker/1
        ]).
%% lib
-export([reset/0, get_last_fitness/0]).

-include("proper_internal.hrl").

%% macros and configuration parameters
-define(REHEAT_THRESHOLD, 5).
-define(RESTART_THRESHOLD, 100).

-define(RANDOM_PROBABILITY, (?RANDOM_MOD:uniform())).

-define(SA_DATA, proper_sa_data).
-define(SA_REHEAT_COUNTER, proper_sa_reheat_counter).

%% types
-type k() :: integer().
-type temp_fun() :: fun(( %% old temperature
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
-type accept_fun() :: fun((proper_target:fitness(), proper_target:fitness(), proper_gen_next:temperature()) -> boolean()).
-type output_fun() :: fun((string(), [term()]) -> 'ok').

%% records
-record(sa_target,
        {first = null :: proper_types:type(),
         next  = null :: fun((_, _) -> proper_types:type()),
         current_generated = null :: proper_gen:instance(),
         last_generated    = null :: proper_gen:instance()
        }).
-type sa_target() :: #sa_target{}.

-record(sa_data,
        {state = dict:new()                          :: dict:dict(proper_target:key(), sa_target()),
         %% max runs
         k_max = 0                                   :: k(),
         %% run number
         k_current = 0                               :: k(),
         %% acceptance probability
         p = fun (_, _, _) -> false end              :: accept_fun(),
         %% energy level
         last_energy = null                          :: proper_target:fitness() | null,
         last_update = 0                             :: integer(),
         %% temperature function
         temperature = 1.0                           :: proper_gen_next:temperature(),
         temp_func = fun(_, _, _, _, _) -> 1.0 end   :: temp_fun(),
         %% output function
         output_fun = fun (_, _) -> ok end            :: output_fun()}).

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
  {1.0 - (K_Current / K_Max), K_Current + 1}.

get_temperature_function(_) ->
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

get_acceptance_function(_) ->
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

%% @doc returns the fitness of the last accepted solution and how many tests old the fitness is
-spec get_last_fitness() -> {integer(), proper_target:fitness()}.
get_last_fitness() ->
  State = get(?SA_DATA),
  {State#sa_data.last_update, State#sa_data.last_energy}.

%% @doc restart the search starting from a random input
-spec reset() -> ok.
reset() ->
  Data = get(?SA_DATA),
  put(?SA_DATA,
      Data#sa_data{state = reset_all_targets(Data#sa_data.state),
                   last_energy = null,
                   last_update = 0,
                   k_max = Data#sa_data.k_max - Data#sa_data.k_current,
                   k_current = 0}).

reset_all_targets(TargetDict) ->
  reset_all_targets(TargetDict, dict:fetch_keys(TargetDict)).

reset_all_targets(Dict,  []) ->
  Dict;
reset_all_targets(Dict, [K|T]) ->
  {S, N, F} = dict:fetch(K, Dict),
  {ok, ResetValue} = proper_gen:safe_generate(S#sa_target.first),
  NewVal = {S#sa_target{last_generated = ResetValue}, N, F},
  reset_all_targets(dict:store(K, NewVal, Dict), T).

%% @private
-spec init_strategy(proper:setup_opts()) -> 'ok'.
init_strategy(#{numtests:=Steps, output_fun:=OutputFun}) ->
  proper_gen_next:init(),
  SA_Data = #sa_data{k_max = Steps,
                     p = get_acceptance_function(OutputFun),
                     temp_func = get_temperature_function(OutputFun)},
  put(?SA_DATA, SA_Data), ok.

%% @private
-spec cleanup() -> ok.
cleanup() ->
  erase(?SA_DATA),
  erase(?SA_REHEAT_COUNTER),
  proper_gen_next:cleanup(),
  ok.

%% @private
-spec init_target(proper_target:tmap()) -> proper_target:target().
init_target(#{gen := Gen}) ->
  init_target(proper_gen_next:from_proper_generator(Gen));
init_target(#{first := First, next := Next}) ->
  create_target(#sa_target{first = First, next = Next}).

create_target(SATarget) ->
  {ok, InitialValue} = proper_gen:safe_generate(SATarget#sa_target.first),
  {SATarget#sa_target{last_generated = InitialValue},
   fun next_func/1,
   %% no local fitness function
   none}.

%% generating next element and updating the target state
next_func(SATarget) ->
  %% retrieving temperature
  GlobalData = get(?SA_DATA),
  Temperature = GlobalData#sa_data.temperature,
  %% calculating the max generated size
  NextGenerator = (SATarget#sa_target.next)(SATarget#sa_target.last_generated, Temperature),
  %% generate the next element
  {ok, Generated} = proper_gen:safe_generate(NextGenerator),
  %% return according to interface
  {SATarget#sa_target{current_generated = Generated}, Generated}.

%% @private
-spec store_target(proper_target:key(), proper_target:target()) -> 'ok'.
store_target(Key, Target) ->
  Data = get(?SA_DATA),
  NewData = Data#sa_data{state = dict:store(Key, Target, (Data#sa_data.state))},
  put(?SA_DATA, NewData),
  ok.

%% @private
-spec retrieve_target(proper_target:key()) -> proper_target:target() | 'undefined'.
retrieve_target(Key) ->
  Dict = (get(?SA_DATA))#sa_data.state,
  case dict:is_key(Key, Dict) of
    true ->
      dict:fetch(Key, Dict);
    false ->
      undefined
  end.

%% @private
-spec update_global_fitness(proper_target:fitness()) -> 'ok'.
update_global_fitness(Fitness) ->
  case get(?SA_DATA) of
    Data = #sa_data{k_current = K_CURRENT,
                    k_max = K_MAX,
                    temperature = Temperature,
                    temp_func = TempFunc} ->
      NewData = case (Data#sa_data.last_energy =:= null)
		  orelse (Data#sa_data.p)(Data#sa_data.last_energy,
					  Fitness,
					  Temperature) of
                  true ->
                    %% accept new state
                    proper_gen_next:update_caches(accept),
                    NewState = update_all_targets(Data#sa_data.state),
                    %% calculate new temperature
                    {NewTemperature, AdjustedK} =
                      TempFunc(Temperature,
                               Data#sa_data.last_energy,
                               Fitness,
                               K_MAX,
                               K_CURRENT,
                               true),
                    Data#sa_data{state = NewState,
                                 last_energy = Fitness,
                                 last_update = 0,
                                 k_current = AdjustedK,
                                 temperature = NewTemperature};
                  false ->
                    %% reject new state
                    proper_gen_next:update_caches(reject),
                    %% calculate new temperature
                    {NewTemperature, AdjustedK} =
                      TempFunc(Temperature,
                               Data#sa_data.last_energy,
                               Fitness,
                               K_MAX,
                               K_CURRENT,
                               false),
                    Data#sa_data{last_update = Data#sa_data.last_update + 1,
                                 k_current = AdjustedK,
                                 temperature = NewTemperature}
		end,
      put(?SA_DATA, NewData),
      ok;
    _ ->
      %% no search strategy or shrinking
      ok
  end.

%% update the last generated value with the current generated value
%% (hence accepting new state)
update_all_targets(TargetDict) ->
  update_all_targets(TargetDict, dict:fetch_keys(TargetDict)).

update_all_targets(Dict,  []) ->
  Dict;
update_all_targets(Dict, [K|T]) ->
  {S, N, F} = dict:fetch(K, Dict),
  NewVal = {S#sa_target{last_generated = S#sa_target.current_generated}, N, F},
  update_all_targets(dict:store(K, NewVal, Dict), T).

%% @private
-spec get_shrinker(proper_target:tmap()) -> proper_types:type().
get_shrinker(#{gen := Gen}) -> Gen.
