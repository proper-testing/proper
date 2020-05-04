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
-export([init/1, callback_mode/0, stopped/3, slow/3, fast/3, terminate/3,
         code_change/4]).
%% proper_fsm callbacks
-export([initial_state/0, initial_state_data/0, next_state_data/5,
         precondition/4, postcondition/5]).
-export([stopped/1, slow/1, fast/1]).


%% -----------------------------------------------------------------------------
%% Definitions
%% -----------------------------------------------------------------------------


-define(DISTANCE, 1000).
-define(SLOW_THRESHOLD, 100).


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
  {ok, stopped, #state{fuel = ?MAX_FUEL, speed = 0}}.

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

fast({call, From}, {accelerate, Value}, S) ->
  accelerate_helper(From, Value, S);
fast({call, From}, {brake, Value}, S) ->
  accelerate_helper(From, -Value, S);
fast({call, From}, {travel, Value}, S) ->
  travel_helper(From, Value, S);
fast({call, From}, {refuel, Value}, S) ->
  refuel_helper(From, Value, S).

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.


%% -----------------------------------------------------------------------------
%% gen_statem helpers
%% -----------------------------------------------------------------------------


accelerate_helper(From, Value, S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  Calc = acceleration_calculations(Speed, Value, Fuel),
  {Distance, Acceleration, Burnt} = Calc,
  StateName = state_name(Speed + Acceleration),
  gen_statem:reply(From, {Distance, Burnt}),
  NS = S#state{fuel = Fuel - Burnt, speed = Speed + Acceleration},
  {next_state, StateName, NS}.

travel_helper(From, Value, S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  {RealDistance, Burnt} = travel_calculations(Value, Speed, Fuel),
  StateName = state_name(Speed),
  gen_statem:reply(From, {RealDistance, Burnt}),
  NS = S#state{fuel = Fuel - Burnt},
  {next_state, StateName, NS}.

refuel_helper(From, Value, S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  Calc = acceleration_calculations(Speed, -Speed, Fuel),
  {Distance, _, Burnt} = Calc,
  gen_statem:reply(From, {Distance, Burnt}),
  NS = S#state{fuel = min(?MAX_FUEL, Fuel - Burnt + Value), speed = 0},
  {next_state, state_name(0), NS}.


%% -----------------------------------------------------------------------------
%% Generators
%% -----------------------------------------------------------------------------


accelerator(Speed, slow) ->
  integer(1, ?SLOW_THRESHOLD - Speed);
accelerator(Speed, fast) ->
  integer(max(1, ?SLOW_THRESHOLD - Speed + 1), ?MAX_SPEED - Speed).

braker(Speed, stopped) ->
  exactly(Speed);
braker(Speed, slow) ->
  integer(max(1, Speed - ?SLOW_THRESHOLD + 1), Speed - 1);
braker(Speed, fast) ->
  integer(1, Speed - ?SLOW_THRESHOLD - 1).

traveler() -> integer(1, 100).

refueler(Fuel) -> integer(0, round(?MAX_FUEL - Fuel)).


%% -----------------------------------------------------------------------------
%% proper_fsm callbacks
%% -----------------------------------------------------------------------------


initial_state() -> stopped.

initial_state_data() ->
  #test_state{fuel     = ?MAX_FUEL,
              speed    = 0,
              burnt    = 0,
              distance = 0}.

stopped(S) ->
  #test_state{fuel = Fuel, speed = Speed} = S,
  accelerate_commands(Speed) ++
    [{history, ?CALL(refuel, [refueler(Fuel)])}
     || Fuel < ?MAX_FUEL - 1].

slow(S) ->
  #test_state{fuel = Fuel, speed = Speed} = S,
  accelerate_commands(Speed) ++
    brake_commands(Speed) ++
    [{history, ?CALL(travel, [traveler()])},
     {stopped, ?CALL(refuel, [refueler(Fuel)])}].

fast(S) ->
  #test_state{fuel = Fuel, speed = Speed} = S,
  accelerate_commands(Speed) ++
    brake_commands(Speed) ++
    [{history, ?CALL(travel, [traveler()])},
     {stopped, ?CALL(refuel, [refueler(Fuel)])}].

precondition(stopped, stopped, _S, ?CALL(refuel, _)) ->
  true;
precondition(_, NextState, S, ?CALL(accelerate, [Value])) ->
  #test_state{fuel = Fuel, speed = Speed} = S,
  Speed < ?MAX_SPEED andalso
    Fuel > ?MAX_FUEL * 0.1 andalso
    state_name(Speed + Value) =:= NextState;
precondition(_, NextState, S, ?CALL(brake, [Value])) ->
  #test_state{speed = Speed} = S,
  state_name(Speed - Value) =:= NextState;
precondition(PrevState, NextState, S, ?CALL(travel, _)) ->
  #test_state{speed = Speed} = S,
  Speed > 20 andalso PrevState =:= NextState;
precondition(_, stopped, S, ?CALL(refuel, _)) ->
  #test_state{fuel = Fuel} = S,
  Fuel < ?MAX_FUEL * 0.8;
precondition(_, _, _, _) ->
  false.

next_state_data(_, _, S, _, ?CALL(accelerate, [Value])) ->
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
next_state_data(_, _, S, _, ?CALL(brake, [Value])) ->
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
next_state_data(_, _, S, _, ?CALL(travel, [Value])) ->
  #test_state{fuel = Fuel,
              speed = Speed,
              distance = Distance,
              burnt = B} = S,
  {Travelled, Burnt} = travel_calculations(Value, Speed, Fuel),
  S#test_state{fuel = Fuel - Burnt,
               distance = Distance + Travelled,
               burnt = B + Burnt};
next_state_data(_, _, S, _, ?CALL(refuel, [Value])) ->
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

postcondition(_, _, S, _, {D, B}) when D >= 0, B >= 0 ->
  #test_state{distance = Distance} = S,
  Distance + D < ?DISTANCE;
postcondition(_, _, _S, _, _R) ->
  false.


%% -----------------------------------------------------------------------------
%% proper_fsm helpers
%% -----------------------------------------------------------------------------


accelerate_commands(Speed) ->
  [{slow, ?CALL(accelerate, [accelerator(Speed, slow)])}
   || Speed < ?SLOW_THRESHOLD] ++
    [{fast, ?CALL(accelerate, [accelerator(Speed, fast)])}
     || Speed < ?MAX_SPEED].

brake_commands(Speed) ->
  [{stopped, ?CALL(brake, [braker(Speed, stopped)])}] ++
    [{slow, ?CALL(brake, [braker(Speed, slow)])} || Speed > 1] ++
    [{fast, ?CALL(brake, [braker(Speed, fast)])}
     || Speed > ?SLOW_THRESHOLD + 1].


%% -----------------------------------------------------------------------------
%% Properties
%% -----------------------------------------------------------------------------


%% Vanilla property based testing. This should not fail consistently.
prop_distance() ->
  ?FORALL(
     Cmds, more_commands(2, proper_fsm:commands(?MODULE)),
     ?TRAPEXIT(
        begin
          start_link(),
          {H, {_, S}, R} = proper_fsm:run_commands(?MODULE, Cmds),
          stop(),
          ?WHENFAIL(
             on_failure(H, S, R),
             aggregate(zip(proper_fsm:state_names(H), command_names(Cmds)),
                       R =:= ok))
        end)).

%% Targeted property based testing, where maximizing the distance travelled
%% provides failing command sequencies more consistently.
prop_distance_targeted() ->
  ?FORALL_TARGETED(
     Cmds, more_commands(2, proper_fsm:targeted_commands(?MODULE)),
     ?TRAPEXIT(
        begin
          start_link(),
          {H, {_, S}, R} = proper_fsm:run_commands(?MODULE, Cmds),
          stop(),
          #test_state{distance = Distance} = S,
          ?MAXIMIZE(Distance),
          ?WHENFAIL(
             on_failure(H, S, R),
             aggregate(zip(proper_fsm:state_names(H), command_names(Cmds)),
                       R =:= ok))
        end)).

%% This is the same as the above but manually providing the initial state.
prop_distance_targeted_init() ->
  State = {initial_state(), initial_state_data()},
  ?FORALL_TARGETED(
     Cmds, more_commands(2, proper_fsm:targeted_commands(?MODULE, State)),
     ?TRAPEXIT(
        begin
          start_link(),
          {H, {_, S}, R} = proper_fsm:run_commands(?MODULE, Cmds),
          stop(),
          #test_state{distance = Distance} = S,
          ?MAXIMIZE(Distance),
          ?WHENFAIL(
             on_failure(H, S, R),
             aggregate(zip(proper_fsm:state_names(H), command_names(Cmds)),
                       R =:= ok))
        end)).


%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------


%% Get state names based on speed.
state_name(0) -> stopped;
state_name(S) when S =< ?SLOW_THRESHOLD -> slow;
state_name(_S) -> fast.

%% Function to be called when the property fails.
on_failure(History, State, {postcondition, false}) ->
  [{_, {D, B}} | _] = lists:reverse(History),
  Distance = State#test_state.distance + D,
  Burnt = State#test_state.burnt + B,
  Consumption = calculate_consumption(Distance, Burnt),
  io:format("Distance: ~w~nConsumption: ~w~n", [Distance, Consumption]);
on_failure(_History, _State, Result) ->
  io:format("Result: ~p~n", [Result]).
