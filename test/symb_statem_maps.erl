%%% Copyright 2016-2017 Manolis Papadakis <manopapad@gmail.com>,
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

%%% @copyright 2016-2017 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Pierre Fenoll (adapted from the code of test/symb_statem.erl)

-module(symb_statem_maps).

-include("compile_flags.hrl").

-include_lib("proper/include/proper.hrl").

-export([command/1,
	 initial_state/0,
	 next_state/3,
	 precondition/2,
	 postcondition/3]).

-export([qux/1]).

-record(state, {qux = #{key => []} :: map()}).

initial_state() ->
    #state{}.

command(_S) ->
    oneof([{call,?MODULE,qux,[integer()]}]).

precondition(_, _) ->
    true.

next_state(S = #state{qux=Qux}, V, {call,?MODULE,qux,[_Arg]}) ->
    Values = maps:get(key, Qux),
    NewValues = {call,maps,get,[key,V]},
    NewQux = Qux#{key => [{call,erlang,hd,[NewValues]} | Values]},
    S#state{qux = NewQux}.

postcondition(S=#state{qux=#{key:=Values}}, {call,?MODULE,qux,[_Arg]}, Res)
  when is_map(Res) ->
    lists:all(fun is_integer/1, Values);
postcondition(_, _, _) ->
    false.

qux(I) when is_integer(I) ->
    #{key => lists:duplicate(3, I)}.

prop_simple() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{H,S,Res} = run_commands(?MODULE, Cmds),
		?WHENFAIL(
		   io:format("H: ~w\nState: ~p\n:Res: ~w\n", [H,S,Res]),
		   Res =:= ok)
	    end).

prop_parallel_simple() ->
    ?FORALL(Cmds, parallel_commands(?MODULE),
	    begin
		{S,P,Res} = run_parallel_commands(?MODULE, Cmds),
		?WHENFAIL(
		   io:format("Seq: ~w\nParallel: ~p\n:Res: ~w\n", [S,P,Res]),
		   Res =:= ok)
	    end).
