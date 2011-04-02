-module(fibo_statem).
-compile(export_all).

-include_lib("proper/include/proper.hrl").


start() ->
    register(fibo_server, spawn(fun init/0)).

stop() -> fibo_server ! stop.

fibo(0) -> 0;
fibo(1) -> 1;
fibo(N) when N > 1 -> fibo_tr(N, 0, 1).

fibo_tr(0, Result, _Next) -> Result;
fibo_tr(Iter, Result, Next) when Iter > 0 ->
    fibo_tr(Iter -1, Next, Result + Next).

init() -> loop({2,1,2}).

loop({Index,Value,Next}) ->
    receive
	{Pid,request} ->
	    Pid ! {self(),Value},
	    loop({Index+1, Next, Value+Next});
	{Pid,set_index,I} ->
	    Pid ! {self(),ok},
	    loop({I, fibo(I), fibo(I+1)});
	stop ->
	    exit(normal)
    end.

get_fibo() ->
    fibo_server ! {self(),request},
    Pid = whereis(fibo_server),
    receive
	{Pid,N} -> N
    end.

set_index(I) ->
    fibo_server ! {self(),set_index,I},
    Pid = whereis(fibo_server),
    receive
	{Pid,ok} -> ok
    end.
    

prop_fibo() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin 
		set_up(),
		{H,S,Res} = run_commands(?MODULE, Cmds),
		clean_up(), 
		?WHENFAIL(
		   io:format("H: ~w\nS: ~w\nRes: ~w\n", [H,S,Res]),
		   Res == ok)
	    end).

prop_parallel_fibo() ->
    ?FORALL(Cmds, parallel_commands(?MODULE),
	    begin
		set_up(),
		{Seq,P,Res} = run_parallel_commands(?MODULE, Cmds),
		clean_up(),
		?WHENFAIL(
		   io:format("Seq: ~w\nParallel: ~w\nRes: ~w\n",
			     [Seq,P,Res]),
		   Res == ok)
	    end).

set_up() ->
    start().

clean_up() ->
    stop(),
    catch unregister(fibo_server).

initial_state() ->
    {2, 0, 1}. %% {index, fib(n-2),fib(n-1)}

precondition(_, _) ->
    true.

command(_State) ->
    oneof([{call,?MODULE,get_fibo,[]},
	   {call,?MODULE,set_index,[index()]}]).
	     
index() ->
    elements(lists:seq(2,20)).

postcondition({_,Fib0,Fib1}, {call,?MODULE,get_fibo,[]}, Res) ->
    Res =:= Fib0 + Fib1;
postcondition({_,_,_}, {call,?MODULE,set_index,[_I]}, ok) ->
    true;
postcondition(_, _, _) ->
    false.

next_state({I,_Fib0,Fib1}, Res, {call,?MODULE,get_fibo,[]}) ->
    {I+1, Fib1, Res};
next_state({_,_,_}, _Res, {call,?MODULE,set_index,[I]}) ->
    {I, fibo(I-2), fibo(I-1)}.
