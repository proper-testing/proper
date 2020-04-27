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


%% -----------------------------------------------------------------------------
%% Macros
%% -----------------------------------------------------------------------------


-define(HOUR, 3600).
-define(ACCELERATION, 5).
-define(DECELERATION, 20).
-define(MAX_FUEL, 70).
-define(MAX_SPEED, 200).
-define(AVG(X), lists:sum(X) / length(X)).
-define(NAME, car).
-define(CALL(C, A), {call, ?MODULE, C, A}).


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

acceleration_calculations(Speed, Accel, Fuel) when Accel > 0 ->
  Acceleration = case Speed + Accel > ?MAX_SPEED of
                   true -> ?MAX_SPEED - Speed;
                   false -> Accel
                 end,
  Consumption = fuel_consumption(Speed, Acceleration),
  Distance = calculate_distance(Speed, Acceleration),
  Burn = Consumption * Distance / 100,
  case Burn > Fuel of
    true ->
      acceleration_calculations(Speed, Acceleration - ?ACCELERATION, Fuel);
    false ->
      {Distance, Acceleration, Burn}
  end;
acceleration_calculations(Speed, Accel, Fuel) ->
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


calculate_consumption(0, _Burnt) -> 0;
calculate_consumption(Distance, Burnt) -> 100 * Burnt / Distance.


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
fuel_speed_penalty(Speed) when Speed =< 50 -> 0.7;
fuel_speed_penalty(Speed) when Speed =< 100 -> 0.9;
fuel_speed_penalty(Speed) when Speed =< 150 -> 1.1;
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
  Penalty = fuel_acceleration_penalty(Acceleration),
  Intermediate = intermediate_speeds(Speed, Acceleration),
  Consumptions = [fuel_consumption(S) * Penalty || S <- Intermediate],
  ?AVG(Consumptions).

%% Intermediate speeds from accelerating - decelerating.
intermediate_speeds(Speed, Acceleration) when Acceleration > 0 ->
  T = Acceleration / ?ACCELERATION,
  [Speed + X / 10 * ?ACCELERATION || X <- lists:seq(0, round(T * 10))];
intermediate_speeds(Speed, Acceleration) ->
  T = -Acceleration / ?DECELERATION,
  [Speed - X / 10 * ?DECELERATION || X <- lists:seq(0, round(T * 10))].
