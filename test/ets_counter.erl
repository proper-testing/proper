%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2010-2019 Manolis Papadakis <manopapad@gmail.com>,
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

%%% @copyright 2010-2019 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Eirini Arvaniti

-module(ets_counter).

-export([ets_inc/2]).
-export([set_up/0, clean_up/0]).  % needed by proper_tests
-export([command/1, precondition/2, postcondition/3,
 	 initial_state/0, next_state/3]).

-include_lib("proper/include/proper.hrl").

%% ---------------------------------------------------------------------
%% Under _reasonable_ schedulings, the following property is expected
%% to *fail* due to races between the ets:lookup/2 and ets:insert/2
%% calls.
%%
%% In Erlang/OTP releases 22.0 and prior, a call to erlang:yield/0 was
%% sufficient to provoke the race when executing commands in parallel.
%% In a pre-release of 23.0, this seems not to be the case any longer
%% but substituting the yield() call with a sleep() seems to do the
%% trick. How robust is that to ensure the failure of the property
%% remains to be investigated.
%% ---------------------------------------------------------------------

prop_ets_counter() ->
    ?FORALL(Commands, parallel_commands(?MODULE),
	    begin
		set_up(),
		{Seq,P,Result} = run_parallel_commands(?MODULE, Commands),
		clean_up(),
		?WHENFAIL(io:format("Seq: ~w\nPar: ~w\nRes: ~w\n",
				    [Seq, P, Result]),
			  Result =:= ok)
	    end).

%% ---------------------------------------------------------------------

-define(KEYS, lists:seq(1, 10)).

ets_inc(Key, Inc) ->
    case ets:lookup(counter, Key) of
	[] ->
	    my_yield(), % attempts to schedule out the process here
	    ets:insert(counter, {Key,Inc}),
	    Inc;
	[{Key,OldValue}] ->
	    NewValue = OldValue + Inc,
	    ets:insert(counter, {Key,NewValue}),
	    NewValue
    end.

my_yield() ->
   timer:sleep(1).	% for OTP < 22.1 sufficed: erlang:yield().

set_up() ->
    counter = ets:new(counter, [public, named_table]),
    ok.

clean_up() ->
    catch ets:delete(counter).

key() ->
    elements(?KEYS).

initial_state() ->
    [].  % an empty proplist

precondition(_S, _C) ->
    true.

command(_S) ->
    {call,?MODULE,ets_inc,[key(),pos_integer()]}.

postcondition(S, {call,_,ets_inc,[Key, Inc]}, Res) ->
    case proplists:is_defined(Key, S) of
	true ->
	    OldValue = proplists:get_value(Key, S),
	    Res =:= OldValue + Inc;
	false ->
	    Res =:= Inc
    end.

next_state(S, _Res, {call,_,ets_inc,[Key, Inc]}) ->
    case proplists:is_defined(Key, S) of
	 true ->
	     OldValue = proplists:get_value(Key, S),
	     NewValue = OldValue + Inc,
	     [{Key,NewValue}|proplists:delete(Key, S)];
	 false ->
	     [{Key,Inc}|S]
    end.
