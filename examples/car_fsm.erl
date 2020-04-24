%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2020-     Manolis Papadakis <manopapad@gmail.com>,
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

-module(car_fsm).
-behaviour(gen_statem).
-behaviour(proper_fsm).

-include_lib("proper/include/proper.hrl").


%% -----------------------------------------------------------------------------
%% Exports
%% -----------------------------------------------------------------------------


%% API
-export([start_link/0, stop/0, accelerate/1, brake/1, travel/1, refuel/1]).
%% gen_statem callbacks
-export([init/1, callback_mode/0, stopped/3, slow/3, normal/3, fast/3,
         too_fast/3, terminate/3, code_change/4]).
%% proper_fsm callbacks
-export([initial_state/0, initial_state_data/0, next_state_data/5,
         precondition/4, postcondition/5, weight/3]).
-export([stopped/1, slow/1, normal/1, fast/1, too_fast/1]).


%% -----------------------------------------------------------------------------
%% Definitions
%% -----------------------------------------------------------------------------


-define(DISTANCE, 1000).
-define(CONSUMPTION, 10).
-define(HOUR, 3600).
-define(ACCELERATION, 5).
-define(DECELERATION, 20).
-define(MAX_FUEL, 70).
-define(MAX_SPEED, 200).
-define(AVG(X), lists:sum(X) / length(X)).
-define(SLOW_THRESHOLD, 50).
-define(NORMAL_THRESHOLD, 100).
-define(FAST_THRESHOLD, 150).
-define(CALL(C, A), {call, ?MODULE, C, A}).
-define(NAME, car).


%% -----------------------------------------------------------------------------
%% Records
%% -----------------------------------------------------------------------------


-record(gen_state,
        {fuel  :: float(),
         speed :: non_neg_integer()}).

-record(state,
        {fuel     :: float(),
         speed    :: non_neg_integer(),
         distance :: float(),
         burnt    :: float()}).


%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------


start_link() ->
  gen_statem:start_link({local, ?NAME}, ?MODULE, [], []).

stop() ->
  gen_statem:stop(?NAME).

accelerate(Value) ->
  gen_statem:call(?NAME, {accelerate, Value}).

brake(Value) ->
  gen_statem:call(?NAME, {brake, Value}).

travel(Distance) ->
  gen_statem:call(?NAME, {travel, Distance}).

refuel(Amount) ->
  gen_statem:call(?NAME, {refuel, Amount}).


%% -----------------------------------------------------------------------------
%% gen_statem callbacks
%% -----------------------------------------------------------------------------


init([]) ->
  {ok, stopped, #gen_state{fuel = ?MAX_FUEL, speed = 0}}.

callback_mode() ->
  state_functions.

stopped({call, From}, {accelerate, Value}, S) ->
  accelerate_helper(From, Value, S);
stopped({call, From}, {refuel, Value}, S) ->
  refuel_helper(From, Value, S).

slow({call, From}, {accelerate, Value}, S) ->
  accelerate_helper(From, Value, S);
slow({call, From}, {brake, Value}, S) ->
  accelerate_helper(From, -Value, S);
slow({call, From}, {travel, Value}, S) ->
  travel_helper(From, Value, S);
slow({call, From}, {refuel, Value}, S) ->
  refuel_helper(From, Value, S).

normal({call, From}, {accelerate, Value}, S) ->
  accelerate_helper(From, Value, S);
normal({call, From}, {brake, Value}, S) ->
  accelerate_helper(From, -Value, S);
normal({call, From}, {travel, Value}, S) ->
  travel_helper(From, Value, S);
normal({call, From}, {refuel, Value}, S) ->
  refuel_helper(From, Value, S).

fast({call, From}, {accelerate, Value}, S) ->
  accelerate_helper(From, Value, S);
fast({call, From}, {brake, Value}, S) ->
  accelerate_helper(From, -Value, S);
fast({call, From}, {travel, Value}, S) ->
  travel_helper(From, Value, S);
fast({call, From}, {refuel, Value}, S) ->
  refuel_helper(From, Value, S).

too_fast({call, From}, {accelerate, Value}, S) ->
  accelerate_helper(From, Value, S);
too_fast({call, From}, {brake, Value}, S) ->
  accelerate_helper(From, -Value, S);
too_fast({call, From}, {travel, Value}, S) ->
  travel_helper(From, Value, S);
too_fast({call, From}, {refuel, Value}, S) ->
  refuel_helper(From, Value, S).

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.


%% -----------------------------------------------------------------------------
%% gen_statem helpers
%% -----------------------------------------------------------------------------


accelerate_helper(From, Value, S) ->
  #gen_state{fuel = Fuel, speed = Speed} = S,
  {Distance, Acceleration, Burnt} =
    acceleration_calculations({Speed, Value}, Fuel),
  StateName = state_name(Speed + Acceleration),
  gen_statem:reply(From, {Distance, Burnt}),
  {next_state, StateName, S#gen_state{fuel = Fuel - Burnt,
                                      speed = Speed + Acceleration}}.

travel_helper(From, Value, S) ->
  #gen_state{fuel = Fuel, speed = Speed} = S,
  {RealDistance, Burnt} = travel_calculations(Value, Speed, Fuel),
  StateName = state_name(Speed),
  gen_statem:reply(From, {RealDistance, Burnt}),
  {next_state, StateName, S#gen_state{fuel = Fuel - Burnt}}.

refuel_helper(From, Value, S) ->
  #gen_state{fuel = Fuel, speed = Speed} = S,
  {Distance, _Deceleration, Burnt} =
    acceleration_calculations({Speed, -Speed}, Fuel),
  gen_statem:reply(From, {Distance, Burnt}),
  {next_state, state_name(0),
   S#gen_state{fuel = min(?MAX_FUEL, Fuel - Burnt + Value),speed = 0}}.


%% -----------------------------------------------------------------------------
%% Generators
%% -----------------------------------------------------------------------------


accelerator(Speed, slow) ->
  integer(1, ?SLOW_THRESHOLD - Speed);
accelerator(Speed, normal) ->
  integer(max(1, ?SLOW_THRESHOLD - Speed + 1), ?NORMAL_THRESHOLD - Speed);
accelerator(Speed, fast) ->
  integer(max(1, ?NORMAL_THRESHOLD - Speed + 1), ?FAST_THRESHOLD - Speed);
accelerator(Speed, too_fast) ->
  integer(max(1, ?FAST_THRESHOLD - Speed + 1), ?MAX_SPEED - Speed).

braker(Speed, stopped) ->
  exactly(Speed);
braker(Speed, slow) ->
  integer(max(1, Speed - ?SLOW_THRESHOLD + 1), Speed - 1);
braker(Speed, normal) ->
  integer(max(1, Speed - ?NORMAL_THRESHOLD + 1), Speed - ?SLOW_THRESHOLD - 1);
braker(Speed, fast) ->
  integer(max(1, Speed - ?FAST_THRESHOLD + 1), Speed - ?NORMAL_THRESHOLD - 1);
braker(Speed, too_fast) ->
  integer(1, Speed - ?FAST_THRESHOLD - 1).

traveler() -> integer(1, 100).

refueler(Fuel) -> integer(0, round(?MAX_FUEL - Fuel)).


%% -----------------------------------------------------------------------------
%% proper_fsm callbacks
%% -----------------------------------------------------------------------------


initial_state() -> stopped.

initial_state_data() ->
  #state{fuel     = ?MAX_FUEL,
         speed    = 0,
         burnt    = 0,
         distance = 0}.

stopped(S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  accelerate_commands(Speed) ++
    [{history, {call, ?MODULE, refuel, [refueler(Fuel)]}}
     || Fuel < ?MAX_FUEL - 1].

slow(S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  accelerate_commands(Speed) ++
    brake_commands(Speed) ++
    [{history, ?CALL(travel, [traveler()])},
     {stopped, ?CALL(refuel, [refueler(Fuel)])}].

normal(S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  accelerate_commands(Speed) ++
    brake_commands(Speed) ++
    [{history, ?CALL(travel, [traveler()])},
     {stopped, ?CALL(refuel, [refueler(Fuel)])}].

fast(S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  accelerate_commands(Speed) ++
    brake_commands(Speed) ++
    [{history, ?CALL(travel, [traveler()])},
     {stopped, ?CALL(refuel, [refueler(Fuel)])}].

too_fast(S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  accelerate_commands(Speed) ++
    brake_commands(Speed) ++
    [{history, ?CALL(travel, [traveler()])},
     {stopped, ?CALL(refuel, [refueler(Fuel)])}].

precondition(stopped, stopped, _S, {call, _, refuel, _}) ->
  true;
precondition(_, NextState, S, {call, _, accelerate, [Value]}) ->
  #state{fuel = Fuel, speed = Speed} = S,
  Speed < ?MAX_SPEED andalso
    Fuel > ?MAX_FUEL * 0.1 andalso
    state_name(Speed + Value) =:= NextState;
precondition(_, NextState, S, {call, _, brake, [Value]}) ->
  #state{speed = Speed} = S,
  state_name(Speed - Value) =:= NextState;
precondition(PrevState, NextState, S, {call, _, travel, _}) ->
  #state{speed = Speed} = S,
  Speed > 20 andalso PrevState =:= NextState;
precondition(_, stopped, S, {call, _, refuel, _}) ->
  #state{fuel = Fuel} = S,
  Fuel < ?MAX_FUEL * 0.8;
precondition(_, _, _, _) ->
  false.

next_state_data(_, _, S, _, {call, _, accelerate, [Value]}) ->
  #state{fuel = Fuel,
         speed = Speed,
         distance = Distance,
         burnt = B} = S,
  {Travelled, Acceleration, Burnt} =
    acceleration_calculations({Speed, Value}, Fuel),
  S#state{fuel = Fuel - Burnt,
          speed = Speed + Acceleration,
          distance = Distance + Travelled,
          burnt = B + Burnt};
next_state_data(_, _, S, _, {call, _, brake, [Value]}) ->
  #state{fuel = Fuel,
         speed = Speed,
         distance = Distance,
         burnt = B} = S,
  {Travelled, Acceleration, Burnt} =
    acceleration_calculations({Speed, -Value}, Fuel),
  S#state{fuel = Fuel - Burnt,
          speed = Speed + Acceleration,
          distance = Distance + Travelled,
          burnt = B + Burnt};
next_state_data(_, _, S, _, {call, _, travel, [Value]}) ->
  #state{fuel = Fuel,
         speed = Speed,
         distance = Distance,
         burnt = B} = S,
  {Travelled, Burnt} = travel_calculations(Value, Speed, Fuel),
  S#state{fuel = Fuel - Burnt,
          distance = Distance + Travelled,
          burnt = B + Burnt};
next_state_data(_, _, S, _, {call, _, refuel, [Value]}) ->
  #state{fuel = Fuel,
         speed = Speed,
         distance = Distance,
         burnt = B} = S,
  {Travelled, Acceleration, Burnt} =
    acceleration_calculations({Speed, -Speed}, Fuel),
  S#state{fuel = Fuel - Burnt + Value,
          speed = Speed + Acceleration,
          distance = Distance + Travelled,
          burnt = B + Burnt}.

postcondition(stopped, stopped, _, {call, _, refuel, _}, {0.0, 0.0}) ->
  true;
postcondition(_, _, _, _, {Distance, Burnt}) ->
  Distance >= 0.0 andalso Burnt >= 0.0.

%% Uncomment the following line to make the search faster.
% weight(_, _, {call, _, travel, _}) -> 10;
weight(_, _, _) -> 1.


%% -----------------------------------------------------------------------------
%% proper_fsm helpers
%% -----------------------------------------------------------------------------


accelerate_commands(Speed) ->
  [{slow, ?CALL(accelerate, [accelerator(Speed, slow)])}
   || Speed < ?SLOW_THRESHOLD] ++
    [{normal, ?CALL(accelerate, [accelerator(Speed, normal)])}
     || Speed < ?NORMAL_THRESHOLD] ++
    [{fast, ?CALL(accelerate, [accelerator(Speed, fast)])}
     || Speed < ?FAST_THRESHOLD] ++
    [{too_fast, ?CALL(accelerate, [accelerator(Speed, too_fast)])}
     || Speed < ?MAX_SPEED].

brake_commands(Speed) ->
  [{stopped, ?CALL(brake, [braker(Speed, stopped)])}] ++
    [{slow, ?CALL(brake, [braker(Speed, slow)])} || Speed > 1] ++
    [{normal, ?CALL(brake, [braker(Speed, normal)])}
     || Speed > ?SLOW_THRESHOLD + 1] ++
    [{fast, ?CALL(brake, [braker(Speed, fast)])}
     || Speed > ?NORMAL_THRESHOLD + 1] ++
    [{too_fast, ?CALL(brake, [braker(Speed, too_fast)])}
     || Speed > ?FAST_THRESHOLD + 1].


%% -----------------------------------------------------------------------------
%% Properties
%% -----------------------------------------------------------------------------


%% This should "never" fail:<br/>
%% `proper:quickcheck(car_fsm:prop_normal_distance(), 1000).'
prop_distance() ->
  ?FORALL(Cmds, proper_fsm:commands(?MODULE),
          ?TRAPEXIT(
             begin
               start_link(),
               {_H, {_, S}, R} = proper_fsm:run_commands(?MODULE, Cmds),
               stop(),
               #state{distance = Distance, burnt = Burnt} = S,
               Consumption = case Distance > 0 of
                               true -> 100 * Burnt / Distance;
                               false -> 0
                             end,
               ?WHENFAIL(
                  io:format("Distance: ~p~nConsumption: ~p~n",
                            [Distance, Consumption]),
                  aggregate(command_names(Cmds),
                            R =:= ok andalso (Distance < ?DISTANCE orelse
                                              Consumption > ?CONSUMPTION)))
             end)).

%% This should fail most of the times with:<br/>
%% `proper:quickcheck(car_fsm:prop_targeted_distance(), 1000).'
prop_distance_targeted() ->
  ?FORALL_TARGETED(
     Cmds, more_commands(2, proper_fsm:targeted_commands(?MODULE)),
     ?TRAPEXIT(
        begin
          start_link(),
          {_H, {_, S}, R} = proper_fsm:run_commands(?MODULE, Cmds),
          stop(),
          #state{distance = Distance, burnt = Burnt} = S,
          Consumption = case Distance > 0 of
                          true -> 100 * Burnt / Distance;
                          false -> 0
                        end,
          UV = case Consumption > ?CONSUMPTION of
                 true -> Distance * 0.1;
                 false -> Distance
               end,
          ?MAXIMIZE(UV),
          ?WHENFAIL(
             io:format("Distance: ~p~nConsumption: ~p~n",
                       [Distance, Consumption]),
             aggregate(command_names(Cmds),
                       R =:= ok andalso (Distance < ?DISTANCE orelse
                                         Consumption > ?CONSUMPTION)))
        end)).

%% This should fail most of the times with:<br/>
%% `proper:quickcheck(car_fsm:prop_targeted_distance_init(), 1000).'
prop_distance_targeted_init() ->
  State = {initial_state(), initial_state_data()},
  ?FORALL_TARGETED(
     Cmds, more_commands(2, proper_fsm:targeted_commands(?MODULE, State)),
     ?TRAPEXIT(
        begin
          start_link(),
          {_H, {_, S}, R} = proper_fsm:run_commands(?MODULE, Cmds),
          stop(),
          #state{distance = Distance, burnt = Burnt} = S,
          Consumption = case Distance > 0 of
                          true -> 100 * Burnt / Distance;
                          false -> 0
                        end,
          UV = case Consumption > ?CONSUMPTION of
                 true -> Distance * 0.1;
                 false -> Distance
               end,
          ?MAXIMIZE(UV),
          ?WHENFAIL(
             io:format("Distance: ~p~nConsumption: ~p~n",
                       [Distance, Consumption]),
             aggregate(command_names(Cmds),
                       R =:= ok andalso (Distance < ?DISTANCE orelse
                                         Consumption > ?CONSUMPTION)))
        end)).

%% -----------------------------------------------------------------------------
%% Calculation Functions
%% -----------------------------------------------------------------------------


travel_calculations(Distance, Speed, Fuel) when Speed > 0 ->
  Consumption = fuel_consumption(Speed),
  Burn = Consumption * Distance / 100,
  case Burn > Fuel of
    true -> {Fuel * 100 / Consumption, Fuel};
    false -> {Distance, Burn}
  end;
travel_calculations(_D, _S, _F) ->
  {0, 0.0}.

acceleration_calculations({Speed, Accel}, Fuel) when Accel > 0 ->
  Acceleration = case Speed + Accel > ?MAX_SPEED of
                   true -> ?MAX_SPEED - Speed;
                   false -> Accel
                 end,
  Consumption = fuel_consumption(Speed, Acceleration),
  Distance = calculate_distance(Speed, Acceleration),
  Burn = Consumption * Distance / 100,
  case Burn > Fuel of
    true -> acceleration_calculations({Speed, Acceleration - ?ACCELERATION},
                                      Fuel);
    false -> {Distance, Acceleration, Burn}
  end;
acceleration_calculations({Speed, Accel}, Fuel) ->
  Acceleration = case Speed + Accel < 0 of
                   true -> -Speed;
                   false -> Accel
                 end,
  Consumption = fuel_consumption(Speed, Acceleration),
  Distance = calculate_distance(Speed, Acceleration),
  Burn = Consumption * Distance / 100,
  case Burn > Fuel of
    true -> {Distance, Acceleration, Fuel};
    false -> {Distance, Acceleration, Burn}
  end.


%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------


%% Calculate distance driven when accelerating - decelerating.
calculate_distance(Speed, Acceleration) when Acceleration > 0 ->
  T = Acceleration / ?ACCELERATION,
  Speed / ?HOUR * T + 1 / 2 * ?ACCELERATION / ?HOUR * T * T;
calculate_distance(Speed, Acceleration)->
  T = -Acceleration / ?DECELERATION,
  Speed / ?HOUR * T - 1 / 2 * ?DECELERATION / ?HOUR * T * T.

%% Low speeds give rewards to consumption.
%% High speed give penalty to consumption.
fuel_speed_penalty(Speed) when Speed =< ?SLOW_THRESHOLD -> 0.7;
fuel_speed_penalty(Speed) when Speed =< ?NORMAL_THRESHOLD -> 0.9;
fuel_speed_penalty(Speed) when Speed =< ?FAST_THRESHOLD -> 1.1;
fuel_speed_penalty(_) -> 1.5.

%% Acceleration penalty.
%% Deceleration reward.
fuel_acceleration_penalty(Acceleration) when Acceleration > 0 -> 2.0;
fuel_acceleration_penalty(_) -> 0.1.

%% Fuel Consumption (stable speed).
fuel_consumption(Speed) ->
  Speed * fuel_speed_penalty(Speed) / 10.

%% Fuel Consumption (acc - dec).
fuel_consumption(Speed, Acceleration) ->
  Consumptions = [fuel_consumption(S) *
                    fuel_acceleration_penalty(Acceleration)
                  || S <- intermediate_speeds(Speed, Acceleration)],
  ?AVG(Consumptions).

%% Intermediate speeds from accelerating - decelerating.
intermediate_speeds(Speed, Acceleration) when Acceleration > 0 ->
  T = Acceleration / ?ACCELERATION,
  [Speed + X / 10 * ?ACCELERATION || X <- lists:seq(0, round(T * 10))];
intermediate_speeds(Speed, Acceleration) ->
  T = -Acceleration / ?DECELERATION,
  [Speed - X / 10 * ?DECELERATION || X <- lists:seq(0, round(T * 10))].

%% Get state names based on speed.
state_name(0) -> stopped;
state_name(S) when S =< ?SLOW_THRESHOLD -> slow;
state_name(S) when S =< ?NORMAL_THRESHOLD -> normal;
state_name(S) when S =< ?FAST_THRESHOLD -> fast;
state_name(_S) -> too_fast.
