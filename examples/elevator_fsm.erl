%%% -*- coding: utf-8; erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2010-2021 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>,
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

%%% @copyright 2010-2021 Manolis Papadakis, Eirini Arvaniti, and Kostis Sagonas
%%% @version {@version}
%%% @author Eirini Arvaniti

-module(elevator_fsm).
-behaviour(gen_statem).
-behaviour(proper_fsm).

%% gen_statem mandatory callbacks and state-handling functions
-export([init/1, callback_mode/0, basement/3, floor/3]).
%% proper_fsm callbacks
-export([initial_state/0, initial_state_data/0, precondition/4,
	 next_state_data/5, postcondition/5]).
%% functions used as proper_fsm commands
-export([up/0, down/0, which_floor/0, get_on/1, get_off/1,
	 fsm_basement/1, fsm_floor/2]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {floor  = 0 :: non_neg_integer(), %% current floor
		people = 0 :: non_neg_integer(), %% people inside the elevator
		num_floors :: non_neg_integer(), %% number of building floors
		limit      :: pos_integer()}).   %% max number of people allowed

-record(test_state, {people     = 0  :: non_neg_integer(),
		     num_floors = 5  :: non_neg_integer(),
		     max_people = 10 :: pos_integer()}).

-define(NAME, elevator).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link(Info) ->
    gen_statem:start_link({local,?NAME}, ?MODULE, Info, []).

stop() ->
    gen_statem:call(?NAME, stop).

up() ->
    gen_statem:cast(?NAME, up).

down() ->
    gen_statem:cast(?NAME, down).

which_floor() ->
    gen_statem:call(?NAME, which_floor).

%% N people try to get on the elevator
get_on(N) ->
    gen_statem:call(?NAME, {get_on,N}).

%% N people get off the elevator (assuming at least N people are inside)
get_off(N) ->
    gen_statem:cast(?NAME, {get_off,N}).


%%--------------------------------------------------------------------
%%% gen_statem callbacks
%%--------------------------------------------------------------------

init(Info) ->
    {NumFloors, Limit} = Info,
    {ok, basement, #state{num_floors = NumFloors, limit = Limit}}.

callback_mode() ->
    state_functions.

%%--------------------------------------------------------------------
%%% State-handling functions
%%--------------------------------------------------------------------

basement(cast, up, S) ->
    case S#state.num_floors > 0 of
	true ->
	    {next_state, floor, S#state{floor = 1}};
	false ->
	    {next_state, basement, S}
    end;
basement(cast, down, S) ->
    {next_state, basement, S};
basement(cast, {get_off,N}, S) ->
    People = S#state.people,
    {next_state, basement, S#state{people = People - N}};
basement({call,From}, which_floor, S) ->
    gen_statem:reply(From, S#state.floor),
    {next_state, basement, S};
basement({call,From}, {get_on,N}, S) ->
    People = S#state.people,
    MorePeople = People + N,
    case MorePeople =< S#state.limit of
	true ->
	    gen_statem:reply(From, MorePeople),
	    {next_state, basement, S#state{people = MorePeople}};
	false ->
	    gen_statem:reply(From, People),
	    {next_state, basement, S}
    end;
basement({call,From}, Msg, Data) ->
    handle_call(From, Msg, Data);
basement({info,Msg}, StateName, Data) ->
    handle_info(Msg, StateName, Data).

floor(cast, up, S) ->
    Floor = S#state.floor,
    NumFloors = S#state.num_floors,
    case NumFloors > Floor of
	true ->
	    {next_state, floor, S#state{floor = Floor + 1}};
	false ->
	    {next_state, floor, S}
    end;
floor(cast, down, S) ->
    case S#state.floor of
	1 ->
	    {next_state, basement, S#state{floor = 0}};
	Floor when Floor > 1 ->
	    {next_state, floor, S#state{floor = Floor-1}}
    end;
floor(cast, {get_off,N}, S) ->
    People = S#state.people,
    {next_state, floor, S#state{people = People - N}};
floor({call,From}, which_floor, S) ->
    gen_statem:reply(From, S#state.floor),
    {next_state, floor, S};
floor({call,From}, Msg, Data) ->
    handle_call(From, Msg, Data);
floor({info,Msg}, StateName, Data) ->
    handle_info(Msg, StateName, Data).

handle_call(From, stop, Data) ->
    {stop_and_reply, normal,  {reply, From, ok}, Data}.

handle_info(Info, StateName, Data) ->
    {stop, {shutdown, {unexpected, Info, StateName}}, StateName, Data}.

%%--------------------------------------------------------------------
%%% PropEr elevator specification
%%--------------------------------------------------------------------

initial_state() -> fsm_basement.

initial_state_data() -> #test_state{}.

fsm_basement(S) ->
    [{history,{call,?MODULE,down,[]}},
     {history,{call,?MODULE,which_floor,[]}},
     {history,{call,?MODULE,get_on,[people(S)]}},
     {history,{call,?MODULE,get_off,[people(S)]}},
     {{fsm_floor,1},{call,?MODULE,up,[]}},
     {history,{call,?MODULE,up,[]}}].

fsm_floor(N, S) ->
    [{{fsm_floor,N-1},{call,?MODULE,down,[]}} || N > 1] ++
    [{fsm_basement,{call,?MODULE,down,[]}} || N =:= 1] ++
    [{history,{call,?MODULE,which_floor,[]}},
     {history,{call,?MODULE,get_off,[people(S)]}},
     {{fsm_floor,N+1},{call,?MODULE,up,[]}},
     {history,{call,?MODULE,up,[]}}].

precondition(fsm_basement, {fsm_floor,1}, S, {call,_,up,[]}) ->
    S#test_state.num_floors > 0;
precondition(fsm_basement, fsm_basement, S, {call,_,up,[]}) ->
    S#test_state.num_floors =:= 0;
precondition({fsm_floor,N}, {fsm_floor,M}, S, {call,_,up,[]})
  when M =:= N + 1 ->
    S#test_state.num_floors > N;
precondition({fsm_floor,N}, {fsm_floor,N}, S, {call,_,up,[]}) ->
    S#test_state.num_floors =:= N;
precondition({fsm_floor,_}, {fsm_floor,_}, _S, {call,_,up,[]}) ->
    false;
precondition(_, _, S, {call,_,get_off,[N]}) ->
    N =< S#test_state.people;
precondition(_, _, _, _) ->
    true.

next_state_data(_, _, S, _, {call,_,get_off,[N]}) ->
    S#test_state{people = S#test_state.people - N};
next_state_data(_, _, S, _, {call,_,get_on,[N]}) ->
    People = S#test_state.people,
    case S#test_state.max_people < People + N of
	true -> S;
	false -> S#test_state{people = People + N}
    end;
next_state_data(_, _, S, _, _) ->
    S.

postcondition(_, _, S, {call,_,get_on,[N]}, R) ->
    People = S#test_state.people,
    case S#test_state.max_people < People + N of
	true -> R =:= People;
	false -> R =:= N + People
    end;
postcondition(fsm_basement, fsm_basement, _, {call,_,which_floor,[]}, 0) ->
    true;
postcondition({fsm_floor,N}, {fsm_floor,N}, _, {call,_,which_floor,[]}, N) ->
    true;
postcondition(_, _, _, {call,_,which_floor,[]}, _) ->
    false;
postcondition(_, _, _, _, R) ->
    R =:= ok.


%%%-------------------------------------------------------------------
%%% Properties
%%%-------------------------------------------------------------------

prop_elevator() ->
    ?FORALL(
       {NumFloors,MaxPeople}, {num_floors(),max_people()},
       begin
	   Initial = {initial_state(),
		      #test_state{num_floors = NumFloors,
				  max_people = MaxPeople,
				  people = 0}},
	   ?FORALL(
	      Cmds, more_commands(5, proper_fsm:commands(?MODULE, Initial)),
	      begin
		  {ok,_} = start_link({NumFloors,MaxPeople}),
		  {H,S,Res} = proper_fsm:run_commands(?MODULE, Cmds),
		  stop(),
		  ?WHENFAIL(
		     io:format("H: ~w~nS: ~w~nR: ~w~n", [H,S,Res]),
		     aggregate(zip(proper_fsm:state_names(H),
				   command_names(Cmds)),
			       Res =:= ok))
	      end)
       end).

people(S) ->
    ?SUCHTHAT(X, pos_integer(), X =< S#test_state.max_people).

max_people() ->
    noshrink(integer(5, 20)).

num_floors() ->
    noshrink(integer(1, 4)).


%%%-------------------------------------------------------------------
%%% EUnit tests
%%%-------------------------------------------------------------------

-define(WRAP(T), proper:test_to_outer_test(T)).

elevator_test_() ->
  elevator_test_(100).

elevator_test_(N) ->
  {"Elevator FSM "++integer_to_list(N),
   ?_assert(proper:quickcheck(?WRAP(prop_elevator()), [{numtests,N}]))}.
