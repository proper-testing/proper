-module(symb_statem).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

-record(state, {foo = [],
		bar = []}).

initial_state() ->
    #state{}.

command(_S) ->
    oneof([{call,?MODULE,foo,[integer()]},
	   {call,?MODULE,bar,[integer()]}]).

precondition(_, _) ->
    true.

next_state(S = #state{foo=Foo}, V, {call,_,foo,[_Arg]}) ->
    V1 = {call,erlang,element,[1,V]},
    S#state{foo = [V1|Foo]};
next_state(S = #state{bar=Bar}, V, {call,_,bar,[_Arg]}) ->
    V1 = {call,erlang,hd,[V]},
    S#state{foo = [V1|Bar]}.

postcondition(S, {call,_,foo,[_Arg]}, Res) when is_tuple(Res) ->
    lists:all(fun is_integer/1, S#state.foo);
postcondition(S, {call,_,bar,[_Arg]}, Res) when is_list(Res) ->
    lists:all(fun is_integer/1, S#state.bar);
postcondition(_, _, _) ->
    false.

foo(I) when is_integer(I) ->
    erlang:make_tuple(3, I).

bar(I) when is_integer(I) ->
    lists:duplicate(3, I).

prop_simple() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{H,S,Res} = run_commands(?MODULE, Cmds),
		?WHENFAIL(
		   io:format("H: ~w\nState: ~w\n:Res: ~w\n", [H,S,Res]),
		   Res =:= ok)
	    end).

prop_parallel_simple() ->
    ?FORALL(Cmds, parallel_commands(?MODULE),
	    begin
		{S,P,Res} = run_parallel_commands(?MODULE, Cmds),
		?WHENFAIL(
		   io:format("Seq: ~w\nParallel: ~w\n:Res: ~w\n", [S,P,Res]),
		   Res =:= ok)
	    end).
