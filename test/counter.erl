-module(counter).
-include_lib("proper/include/proper.hrl").

-compile(export_all).

start() ->
    register(num_server,spawn_link(fun() -> loop(0) end)).

stop() ->
    num_server ! stop.

loop(N) ->
    %%io:format("N=~w~n", [N]),
    receive
	{get_counter, Pid} ->
	    Pid ! N,
	    loop(N);
	{set_counter,C} ->
	    loop(C);
	stop ->
	    ok
    end.

take_action() ->
    num_server ! {get_counter, self()},
    receive
	N -> num_server ! {set_counter, N+1},
	     N+1
    end.
   
prop_num_server() ->
    ?FORALL(Commands, parallel_commands(?MODULE),
	    begin
		clean_up(),
		start(),
		{Seq,P,Result} = run_parallel_commands(?MODULE, Commands),
		?WHENFAIL(io:format("Seq: ~w\nPar: ~w\nRes: ~w\n",
				    [Seq, P, Result]),
			  Result =:= ok)
	    end).

clean_up() ->
    catch stop().

initial_state() -> 0.

precondition(_S, _C) ->
    true.

command(_S) ->
    {call,?MODULE,take_action,[]}.

postcondition(N, {call,_,take_action,_}, R) ->
    R =:= N+1.

next_state(_N, R, _C) -> R.
   
