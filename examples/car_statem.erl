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

-module(car_statem).
-behaviour(gen_server).
-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").


%% -----------------------------------------------------------------------------
%% Exports
%% -----------------------------------------------------------------------------


%% api
-export([start_link/0, stop/0, accelerate/1, brake/1, travel/1, refuel/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
%% proper_statem callbacks
-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).


%% -----------------------------------------------------------------------------
%% Definitions
%% -----------------------------------------------------------------------------


-define(DISTANCE, 1500).


%% -----------------------------------------------------------------------------
%% Records
%% -----------------------------------------------------------------------------


-record(state,
        {fuel  :: float(),
         speed :: non_neg_integer()}).

-record(test_state,
        {fuel     :: float(),
         speed    :: non_neg_integer(),
         distance :: float(),
         burnt    :: float()}).


%% -----------------------------------------------------------------------------
%% Common Imports
%% -----------------------------------------------------------------------------


-include("car.inc").


%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------


start_link() ->
  gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?NAME).

accelerate(Value) ->
  gen_server:call(?NAME, {accelerate, Value}).

brake(Value) ->
  gen_server:call(?NAME, {brake, Value}).

travel(Distance) ->
  gen_server:call(?NAME, {travel, Distance}).

refuel(Amount) ->
  gen_server:call(?NAME, {refuel, Amount}).


%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------


init([]) ->
  {ok, #state{fuel = ?MAX_FUEL, speed = 0}}.

handle_call({accelerate, Value}, _From, S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  Calc = acceleration_calculations(Speed, Value, Fuel),
  {Distance, Acceleration, Burnt} = Calc,
  NS = S#state{fuel = Fuel - Burnt, speed = Speed + Acceleration},
  {reply, {Distance, Burnt}, NS};

handle_call({brake, Value}, _From, S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  Calc = acceleration_calculations(Speed, -Value, Fuel),
  {Distance, Deceleration, Burnt} = Calc,
  NS = S#state{fuel = Fuel - Burnt, speed = Speed + Deceleration},
  {reply, {Distance, Burnt}, NS};

handle_call({travel, Distance}, _From, S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  {RealDistance, Burnt} = travel_calculations(Distance, Speed, Fuel),
  NS = S#state{fuel = Fuel - Burnt},
  {reply, {RealDistance, Burnt}, NS};

handle_call({refuel, Value}, _From, S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  Calc = acceleration_calculations(Speed, -Speed, Fuel),
  {Distance, _, Burnt} = Calc,
  NS = S#state{fuel = min(?MAX_FUEL, Fuel - Burnt + Value), speed = 0},
  {reply, {Distance, Burnt}, NS}.

handle_cast(_Msg, S) ->
  {noreply, S}.

handle_info(_Msg, S) ->
  {noreply, S}.

terminate(_Reason, _S) ->
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.


%% -----------------------------------------------------------------------------
%% Generators
%% -----------------------------------------------------------------------------


accelerator(Speed) ->
  integer(0, ?MAX_SPEED - Speed).

braker(Speed) ->
  integer(0, Speed).

traveler() ->
  integer(1, 100).

refueler(Fuel) ->
  integer(0, round(?MAX_FUEL - Fuel)).


%% -----------------------------------------------------------------------------
%% proper_statem callbacks
%% -----------------------------------------------------------------------------


initial_state() ->
  #test_state{fuel     = ?MAX_FUEL,
              speed    = 0,
              burnt    = 0,
              distance = 0}.

command(#test_state{fuel = Fuel, speed = Speed}) ->
  oneof([{call, ?MODULE, accelerate, [accelerator(Speed)]},
         {call, ?MODULE, brake, [braker(Speed)]},
         {call, ?MODULE, travel, [traveler()]},
         {call, ?MODULE, refuel, [refueler(Fuel)]}]).

precondition(State, {call, _, accelerate, [Value]}) ->
  #test_state{fuel = Fuel, speed = Speed} = State,
  Fuel > ?MAX_FUEL * 0.1 andalso Speed < ?MAX_SPEED andalso Value > 0;
precondition(#test_state{speed = Speed}, {call, _, brake, [Value]}) ->
  Speed > 0 andalso Value > 0;
precondition(#test_state{speed = Speed}, {call, _, travel, _}) ->
  Speed > 20;
precondition(#test_state{fuel = Fuel}, {call, _, refuel, _}) ->
  Fuel < ?MAX_FUEL * 0.8;
precondition(_, _) ->
  true.

postcondition(S, _, {D, B}) when D >= 0, B >= 0 ->
  #test_state{distance = Distance} = S,
  Distance + D < ?DISTANCE;
postcondition(_S, _, _R) ->
  false.

next_state(S, _V, {call, _, accelerate, [Value]}) ->
  #test_state{fuel = Fuel,
              speed = Speed,
              distance = Distance,
              burnt = B} = S,
  Calc = acceleration_calculations(Speed, Value, Fuel),
  {Travelled, Acceleration, Burnt} = Calc,
  S#test_state{fuel = Fuel - Burnt,
               speed = Speed + Acceleration,
               distance = Distance + Travelled,
               burnt = B + Burnt};
next_state(S, _V, {call, _, brake, [Value]}) ->
  #test_state{fuel = Fuel,
              speed = Speed,
              distance = Distance,
              burnt = B} = S,
  Calc = acceleration_calculations(Speed, -Value, Fuel),
  {Travelled, Acceleration, Burnt} = Calc,
  S#test_state{fuel = Fuel - Burnt,
               speed = Speed + Acceleration,
               distance = Distance + Travelled,
               burnt = B + Burnt};
next_state(S, _V, {call, _, travel, [Value]}) ->
  #test_state{fuel = Fuel,
              speed = Speed,
              distance = Distance,
              burnt = B} = S,
  {Travelled, Burnt} = travel_calculations(Value, Speed, Fuel),
  S#test_state{fuel = Fuel - Burnt,
               distance = Distance + Travelled,
               burnt = B + Burnt};
next_state(S, _V, {call, _, refuel, [Value]}) ->
  #test_state{fuel = Fuel,
              speed = Speed,
              distance = Distance,
              burnt = B} = S,
  Calc = acceleration_calculations(Speed, -Speed, Fuel),
  {Travelled, Acceleration, Burnt} = Calc,
  S#test_state{fuel = min(?MAX_FUEL, Fuel - Burnt + Value),
               speed = Speed + Acceleration,
               distance = Distance + Travelled,
               burnt = B + Burnt}.


%% -----------------------------------------------------------------------------
%% Properties
%% -----------------------------------------------------------------------------


%% Vanilla property based testing. This should not fail consistently.
prop_distance() ->
  ?FORALL(Cmds, more_commands(2, commands(?MODULE)),
          ?TRAPEXIT(
             begin
               start_link(),
               {_H, _S, R} = run_commands(?MODULE, Cmds),
               stop(),
               ?WHENFAIL(
                  on_failure(R, Cmds),
                  aggregate(command_names(Cmds), R =:= ok))
             end)).

%% Targeted property based testing, where maximizing the distance travelled
%% provides failing command sequencies more consistently.
prop_distance_targeted() ->
  ?FORALL_TARGETED(
     Cmds, more_commands(2, targeted_commands(?MODULE)),
     ?TRAPEXIT(
        begin
          start_link(),
          {_H, S, R} = run_commands(?MODULE, Cmds),
          stop(),
          #test_state{distance = Distance} = S,
          ?MAXIMIZE(Distance),
          ?WHENFAIL(
             on_failure(R, Cmds),
             aggregate(command_names(Cmds), R =:= ok))
        end)).

%% This is the same as the above but manually providing the initial state.
prop_distance_targeted_init() ->
  State = initial_state(),
  ?FORALL_TARGETED(
     Cmds, more_commands(2, targeted_commands(?MODULE, State)),
     ?TRAPEXIT(
        begin
          start_link(),
          {_H, S, R} = run_commands(?MODULE, Cmds),
          stop(),
          #test_state{distance = Distance} = S,
          ?MAXIMIZE(Distance),
          ?WHENFAIL(
             on_failure(R, Cmds),
             aggregate(command_names(Cmds), R =:= ok))
        end)).


%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------


%% Function to be called when the property fails.
on_failure({postcondition, false}, Cmds) ->
  #test_state{distance = Distance, burnt = Burnt} = state_after(?MODULE, Cmds),
  Consumption = calculate_consumption(Distance, Burnt),
  io:format("Distance: ~w~nConsumption: ~w~n", [Distance, Consumption]);
on_failure(Result, _Cmds) ->
  io:format("Result: ~w~n", [Result]).
