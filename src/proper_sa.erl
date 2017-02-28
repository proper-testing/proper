%%% coding: latin-1
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

-module(proper_sa).

-behaviour(proper_target).

%% callbacks
-export([init_strategy/2,
         init_target/1,
         cleanup/0,
         store_target/2,
         retrieve_target/1,
         update_global_fitness/1,
         get_shrinker/1
        ]).
%% lib
-export([reset/0, get_last_fitness/0]).
%% standard types
-export([integer/0, integer/2, float/0, float/2, list/1]).

-export_type([first_next/0]).

-include("proper_internal.hrl").

%% macros and configuration parameters
-define(DEFAULT_STEPS, 1000).
-define(MAX_SIZE, 10000).
-define(REHEAT_THRESHOLD, 5).

-define(RANDOM_PROBABILITY, (?RANDOM_MOD:uniform())).

-define(SA_DATA, proper_sa_data).
-define(SA_REHEAT_COUNTER, proper_sa_reheat_counter).

%% types
-type k() :: integer().
-type temperature() :: float().
-type temp_fun() :: fun(( %% old temperature
                          temperature(),
                          %% old energy level
                          proper_target:fitness(),
                          %% new energy level
                          proper_target:fitness(),
                          %% k_current
                          k(),
                          %% k_max
                          k(),
                          %% accepted or not
                          boolean()) -> {temperature(), k()}).
-type accept_fun() :: fun((proper_target:fitness(), proper_target:fitness(), temperature()) -> boolean()).
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
         %% temperature function
         temperature = 1.0                           :: temperature(),
         temp_func = fun(_, _, _, _, _) -> 1.0 end   :: temp_fun(),
         %% output function
         output_fun = fun (_,_) -> ok end            :: output_fun()}).

print_accepted(State, Utility, Temperature) ->
  case get(target_print_accepted) of
    Printer when is_function(Printer) -> Printer(State, Utility);
    true -> io:format("Accepted at Fitness ~p and Temperature ~p ~n", [Utility, Temperature]);
    _ -> ok
  end.

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

acceptance_function_normalized(EnergyCurrent, EnergyNew, Temperature) ->
  case EnergyNew > EnergyCurrent of
    true ->
      %% always accept better results
      true;
    false ->
      %% probabilistic acceptance (always between 0.0 and 0.5)
      AcceptanceProbability =
        try
          1 / (1 + math:exp( (1 -  (EnergyNew/EnergyCurrent)) / Temperature))
        catch
          error:badarith -> 0.0
        end,
      %% if random probability is less, accept
      ?RANDOM_PROBABILITY < AcceptanceProbability
  end.

acceptance_function_hillclimbing(EnergyCurrent, EnergyNew, _Temperature) ->
  %% Hill-Climbing
  EnergyNew > EnergyCurrent.

temperature_function_fast_sa(_OldTemperature,
                             _OldEnergyLevel,
                             _NewEnergyLevel,
                             _K_Max,
                             K_Current,
                             Accepted) ->
  AdjustedK = case Accepted of
                false ->
                  case get(?SA_REHEAT_COUNTER) of
                    undefined ->
                      put(?SA_REHEAT_COUNTER, 1),
                      K_Current + 1;
                    N when N >= ?REHEAT_THRESHOLD->
                      put(?SA_REHEAT_COUNTER, 0),
                      max(1, K_Current - trunc(1.4 * ?REHEAT_THRESHOLD));
                    N ->
                      put(?SA_REHEAT_COUNTER, N + 1),
                      K_Current + 1
                  end;
                true -> K_Current + 1
              end,
  {1 / max((AdjustedK / 4.0), 1.0), AdjustedK}.

temperature_function_fast2_sa(_OldTemperature,
                              _OldEnergyLevel,
                              _NewEnergyLevel,
                              K_Max,
                              K_Current,
                              _Accepted) ->
  {1.0 - math:sqrt(K_Current / K_Max), K_Current + 1}.

temperature_function_reheat_sa(OldTemperature,
                               OldEnergyLevel,
                               NewEnergyLevel,
                               K_Max,
                               K_Current,
                               Accepted) when is_integer(K_Current) ->
  temperature_function_reheat_sa(OldTemperature,
                                 OldEnergyLevel,
                                 NewEnergyLevel,
                                 K_Max,
                                 {K_Current, K_Current},
                                 Accepted);
temperature_function_reheat_sa(_OldTemperature,
                               _OldEnergyLevel,
                               _NewEnergyLevel,
                               K_Max,
                               {K_Current, K_Counter},
                               Accepted) ->
  Scaling = 1.0 - (K_Counter / K_Max),
  AdjustedK = case Accepted of
                false ->
                  case get(?SA_REHEAT_COUNTER) of
                    undefined ->
                      put(?SA_REHEAT_COUNTER, 1),
                      K_Current + 1;
                    N when N >= ?REHEAT_THRESHOLD->
                      put(?SA_REHEAT_COUNTER, 0),
                      max(1, K_Current - trunc(Scaling * 5 * ?REHEAT_THRESHOLD));
                    N ->
                      put(?SA_REHEAT_COUNTER, N + 1),
                      K_Current + 1
                  end;
                true -> K_Current + 1
              end,
  {1 / max((AdjustedK / 4.0), 1.0), {AdjustedK, K_Counter + 1}}.

temperature_function_standard_sa(_OldTemperature,
                                 _OldEnergyLevel,
                                 _NewEnergyLevel,
                                 K_Max,
                                 K_Current,
                                 _Accepted) ->
  {1.0 - (K_Current / K_Max), K_Current + 1}.

get_temperature_function(OutputFun) ->
  OutputFun("Temperature Function: \t", []),
  case get(proper_sa_tempfunc) of
    default ->
      OutputFun("default~n", []),
      fun temperature_function_standard_sa/6;
    fast ->
      OutputFun("fast~n", []),
      fun temperature_function_fast_sa/6;
    very_fast ->
      OutputFun("very fast~n", []),
      fun temperature_function_fast2_sa/6;
    reheat ->
      OutputFun("decreasing reheating~n", []),
      fun temperature_function_reheat_sa/6;
    Fun when is_function(Fun) ->
      case proplists:lookup(arity, erlang:fun_info(Fun)) of
        {arity, 6} ->
          OutputFun("configured ~p~n", [Fun]),
          Fun;
        _ ->
          OutputFun("wrong arity of configured temperature function; using default instead~n", []),
          fun temperature_function_standard_sa/6
      end;
    undefined ->
      OutputFun("default~n", []),
      fun temperature_function_standard_sa/6;
    _ ->
      OutputFun("undefined configured temperature function; using default instead~n", []),
      fun temperature_function_standard_sa/6
  end.

get_acceptance_function(OutputFun) ->
  OutputFun("Acceptance Function: \t", []),
  case get(proper_sa_acceptfunc) of
    default ->
      OutputFun("default~n", []),
      fun acceptance_function_standard/3;
    hillclimbing ->
      OutputFun("hillclimbing~n", []),
      fun acceptance_function_hillclimbing/3;
    normalized ->
      OutputFun("normalized~n", []),
      fun acceptance_function_normalized/3;
    Fun when is_function(Fun) ->
      case proplists:lookup(arity, erlang:fun_info(Fun)) of
        {arity, 3} ->
          OutputFun("configured ~p~n", [Fun]),
          Fun;
        _ ->
          OutputFun("wrong arity of configured acceptance function; using default instead~n", []),
          fun acceptance_function_standard/3
      end;
    undefined ->
      OutputFun("default~n", []),
      fun acceptance_function_standard/3;
    _ ->
      OutputFun("undefined configured acceptance function; using default instead~n", []),
      fun acceptance_function_standard/3
  end.

-spec get_last_fitness() -> proper_target:fitness().
get_last_fitness() ->
  State = get(?SA_DATA),
  State#sa_data.last_energy.

-spec reset() -> ok.
reset() ->
  Data = get(?SA_DATA),
  put(?SA_DATA,
      Data#sa_data{state = reset_all_targets(Data#sa_data.state),
                   last_energy = null,
                   k_max = Data#sa_data.k_max - Data#sa_data.k_current,
                   k_current = 0}).

reset_all_targets(TargetDict) ->
  reset_all_targets(TargetDict, dict:fetch_keys(TargetDict)).

reset_all_targets(Dict,  []) ->
  Dict;
reset_all_targets(Dict, [K|T]) ->
  {S, N, F} = dict:fetch(K, Dict),
  {ok, ResetValue} = proper_gen:clean_instance(proper_gen:safe_generate(S#sa_target.first)),
  NewVal = {S#sa_target{last_generated = ResetValue}, N, F},
  reset_all_targets(dict:store(K, NewVal, Dict), T).

-spec init_strategy(proper:outer_test(), proper:setup_opts()) -> proper:outer_test().
init_strategy(Prop, #{numtests:=Steps, output_fun:=OutputFun}) ->
  OutputFun("-- Simulated Annealing Search Strategy --~n", []),
  SA_Data = #sa_data{k_max = Steps,
		     p = get_acceptance_function(OutputFun),
		     temp_func = get_temperature_function(OutputFun)},
  put(?SA_DATA, SA_Data),
  Prop.

-spec cleanup() -> ok.
cleanup() ->
  erase(?SA_DATA),
  erase(?SA_REHEAT_COUNTER),
  ok.

-spec init_target(proper_target:tmap()) -> proper_target:target().
init_target(TMap) when map_size(TMap) =:= 0 ->
  init_target(?MODULE:integer());
init_target(#{gen := Gen}) ->
  init_target(proper_sa_gen:from_proper_generator(Gen));
init_target(#{first := First, next := Next}) ->
  create_target(#sa_target{first = First, next = Next}).

create_target(SATarget) ->
  {ok, InitialValue} = proper_gen:clean_instance(proper_gen:safe_generate(SATarget#sa_target.first)),
  {SATarget#sa_target{last_generated = InitialValue},
   fun next_func/1,
   %% dummy local fitness function
   fun (S, _) -> S end}.

%% generating next element and updating the target state
next_func(SATarget) ->
  %% retrieving temperature
  GlobalData = get(?SA_DATA),
  Temperature = GlobalData#sa_data.temperature,
  %% calculating the max generated size
  NextGenerator = (SATarget#sa_target.next)(SATarget#sa_target.last_generated, Temperature),
  %% generate the next element
  {ok, Generated} = proper_gen:clean_instance(proper_gen:safe_generate(NextGenerator)),
  %% return according to interface
  {SATarget#sa_target{current_generated = Generated}, Generated}.


-spec store_target(proper_target:key(), proper_target:target()) -> 'ok'.
store_target(Key, Target) ->
  Data = get(?SA_DATA),
  NewData = Data#sa_data{state = dict:store(Key, Target, (Data#sa_data.state))},
  put(?SA_DATA, NewData),
  ok.

-spec retrieve_target(proper_target:key()) -> proper_target:target() | 'undefined'.
retrieve_target(Key) ->
  Dict = (get(?SA_DATA))#sa_data.state,
  case dict:is_key(Key, Dict) of
    true ->
      dict:fetch(Key, Dict);
    false ->
      undefined
  end.

-spec update_global_fitness(proper_target:fitness()) -> 'ok'.
update_global_fitness(Fitness) ->
  Data = get(?SA_DATA),
  K_CURRENT = (Data#sa_data.k_current),
  K_MAX = (Data#sa_data.k_max),
  Temperature = Data#sa_data.temperature,
  NewData = case (Data#sa_data.last_energy =:= null)
              orelse
              (Data#sa_data.p)(Data#sa_data.last_energy,
                               Fitness,
                               Temperature) of
              true ->
                %% accept new state
                proper_sa_gen:update_caches(accept),
                print_accepted(Data, Fitness, Temperature),
                NewState = update_all_targets(Data#sa_data.state),
                %% calculate new temperature
                {NewTemperature, AdjustedK} =
                  (Data#sa_data.temp_func)(Temperature,
                                           Data#sa_data.last_energy,
                                           Fitness,
                                           K_MAX,
                                           K_CURRENT,
                                           true),
                Data#sa_data{state = NewState,
                             last_energy=Fitness,
                             k_current = AdjustedK,
                             temperature = NewTemperature};
              false ->
                %% reject new state
                %% calculate new temperature
                proper_sa_gen:update_caches(reject),
                {NewTemperature, AdjustedK} =
                  (Data#sa_data.temp_func)(Temperature,
                                           Data#sa_data.last_energy,
                                           Fitness,
                                           K_MAX,
                                           K_CURRENT,
                                           false),
                Data#sa_data{k_current = AdjustedK, temperature = NewTemperature}
            end,
  put(?SA_DATA, NewData),
  ok.

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

-spec get_shrinker(proper_target:tmap()) -> proper_types:type().
get_shrinker(#{first := First}) -> First;
get_shrinker(#{gen := Gen}) -> Gen.

%%--------------------------------------------------------------------------
%% library
%%--------------------------------------------------------------------------

-type first_next() :: proper_target:tmap().

-spec integer() -> first_next().
integer() ->
  ?MODULE:integer(inf, inf).

-spec integer(proper_types:extint(), proper_types:extint()) -> first_next().
integer(L, R) ->
  #{first => proper_types:integer(L, R), next => integer_next(L, R)}.

integer_next(L, R) ->
  fun (OldInstance, Temperature) ->
      {LL, LR} = case L =:= inf orelse R =:= inf of
                   true ->
                     {inf, inf};
                   false ->
                     Limit = trunc(abs(L - R) * Temperature * 0.1) + 1,
                     {-Limit, Limit}
                 end,
      ?LET(X, proper_types:integer(LL, LR), make_inrange(OldInstance, X, L, R))
  end.

-spec float() -> first_next().
float() ->
  ?MODULE:float(inf, inf).

-spec float(proper_types:extnum(), proper_types:extnum()) -> first_next().
float(L, R) ->
  #{first => proper_types:float(L, R), next => float_next(L, R)}.

float_next(L, R) ->
  fun (OldInstance, Temperature) ->
      {LL, LR} = case L =:= inf orelse R =:= inf of
                   true ->
                     {inf, inf};
                   false ->
                     Limit = abs(L - R) * Temperature * 0.1,
                     {-Limit, Limit}
                 end,
      ?LET(X, proper_types:float(LL, LR), make_inrange(OldInstance, X, L, R))
  end.

make_inrange(Val, L, R) when (R =:= inf orelse Val =< R) andalso (L =:= inf orelse Val >= L) -> Val;
make_inrange(Val, L, _R) when Val < L -> L;
make_inrange(Val, _L, R) when Val > R -> R.

make_inrange(Val, Offset, L, R) when L =/= inf, Val + Offset < L ->
  make_inrange(Val - Offset, L, R);
make_inrange(Val, Offset, L, R) when R =/= inf, Val + Offset > R ->
  make_inrange(Val - Offset, L, R);
make_inrange(Val, Offset, L, R) -> make_inrange(Val + Offset, L, R).


%% list
-spec list(proper_types:type()) -> first_next().
list(Type) ->
  #{first => proper_types:list(Type),
    next => list_next(Type)}.

list_next(Type) ->
  fun (Base, _T) ->
      GrowthCoefficient = (?RANDOM_MOD:uniform() * 0.8) + 0.1,
      list_gen_internal(Base, 0.5, Type, GrowthCoefficient)
  end.

list_gen_internal([], Temp, InternalType, GrowthCoefficient) ->
  %% chance to add an element
  case list_choice(empty, Temp) of
    add ->
      {ok, New} = proper_gen:clean_instance(proper_gen:safe_generate(InternalType)),
      [New | list_gen_internal([], Temp, InternalType, GrowthCoefficient)];
    nothing -> []
  end;
list_gen_internal(L=[H|T], Temp, InternalType, GrowthCoefficient) ->
  %% chance to delete current element
  %% chance to add element in front of current element
  case list_choice(GrowthCoefficient, Temp) of
    add ->
      {ok, New} = proper_gen:clean_instance(proper_gen:safe_generate(InternalType)),
      [New | list_gen_internal(L, Temp, InternalType, GrowthCoefficient)];
    del ->
      list_gen_internal(T, Temp, InternalType, GrowthCoefficient);
    nothing ->
      [H | list_gen_internal(T, Temp, InternalType, GrowthCoefficient)]
  end.

list_choice(empty, Temp) ->
  C = ?RANDOM_MOD:uniform(),
  C_Add = 0.5 * Temp,
  if
    C < C_Add -> add;
    true      -> nothing
  end;
list_choice(GrowthCoefficient, Temp) ->
  C = ?RANDOM_MOD:uniform(),
  AddCoefficient = 0.6 * GrowthCoefficient,
  DelCoefficient = 0.6 * (1- GrowthCoefficient),
  C_Add =          AddCoefficient * Temp,
  C_Del = C_Add + (DelCoefficient * Temp),
  if
    C < C_Add -> add;
    C < C_Del -> del;
    true      -> nothing
  end.
