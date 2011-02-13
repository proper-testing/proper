-module(freq_statem).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

-define(FREQUENCIES, lists:seq(1,20)).

%% Frequency server
start() ->
    register(frequency, spawn(fun init/0)).

init() ->
    Frequencies = {?FREQUENCIES, []},
    loop(Frequencies).

stop() ->
    call(stop).

allocate(Freq) ->
    call({allocate,Freq}).

deallocate(Freq) ->
    call({deallocate,Freq}).

call(Message) ->
    frequency ! {request, self(), Message},
    receive 
	{reply, Reply} -> Reply
    end.

loop(Frequencies) ->
    receive
	{request, Pid, {allocate,Freq}} ->
	    {NewFreqs, Reply} = allocate(Freq,Frequencies, Pid),
	    reply(Pid, Reply),
	    loop(NewFreqs);
	{request, Pid, {deallocate,Freq}} ->
	    NewFreqs = deallocate(Frequencies, Freq),
	    reply(Pid, ok),
	    loop(NewFreqs);
	{request, Pid, stop} ->
	    reply(Pid, ok)
     end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

allocate(_Freq,{[],Allocated}, _Pid) ->
    {{[],Allocated}, {error, no_frequency}};
allocate(Freq,{Free, Allocated}, Pid) ->
    case lists:member(Freq, Free) of
	false -> 
	     {{Free,Allocated}, {error, not_available}};
	true ->
	    NewFree=lists:delete(Freq, Free),
	    {{NewFree, [{Freq, Pid}|Allocated]}, ok}
    end.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Properties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	   
freq() ->
    oneof(?FREQUENCIES).

initial_state() -> {?FREQUENCIES,[]}.    

command(_) ->
    weighted_union([{1,{call,?MODULE,allocate,[freq()]}},
		    {1,{call,?MODULE,deallocate,[freq()]}}]
		  ).

precondition({_Free,Alloc},{call,_,deallocate,[Freq]}) ->
    lists:member(Freq, Alloc);
precondition(_,_) ->
    true.

next_state({Free,Alloc}=S, _V, {call,_,allocate,[Freq]}) ->
    case lists:member(Freq, Free) of
	false -> 
	    S;
	true ->
	    NewFree=lists:delete(Freq, Free),
	    {NewFree, [Freq|Alloc]}
    end; 
next_state({Free,Alloc},_V,{call,_,deallocate,[Freq]}) ->
    {[Freq|Free],lists:delete(Freq,Alloc)}.


postcondition({[],_Alloc},{call,_,allocate,[]},Res) ->
    Res =:= {error,no_frequency} ;
postcondition({Free,Alloc},{call,_,allocate,[Freq]},Res) ->
    case {lists:member(Freq,Free), lists:member(Freq,Alloc)} of
	{true, false} ->
	    Res =:= ok;
	{false, true} ->
	    Res =:= {error, not_available};
	_Other ->
	    false
    end;
postcondition(_,{call,_,deallocate,[_Freq]}, ok) ->
    true;
postcondition(_,_,_) -> false.    

prop_freq() ->
    ?FORALL(Cmds,commands(?MODULE),
	    begin
		start(),
		{H,S,Res} = run_commands(?MODULE,Cmds),
		stop(),
		?WHENFAIL(
		   io:format("H: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
		   Res =:= ok)
	    end).

prop_parallel() ->
    ?FORALL(Cmds, parallel_commands(?MODULE),
	    begin
		start(),
		{Seq,P,Res} = run_parallel_commands(?MODULE,Cmds),
		stop(),
		io:format("Sequential: ~p\nParallel: ~p\nRes: ~p\n",[Seq,P,Res]),
		equals(Res, ok)
	    end).
