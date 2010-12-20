-module(frequency).
-export([start/0,stop/0,allocate/0,deallocate/1]).
-export([get_frequencies/0]).
-export([init/0]).

start() ->
    register(frequency, spawn(frequency, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

get_frequencies() ->
     [10,11,12,13,14,15].

stop() ->
     call(stop).
allocate() ->
    call(allocate).
deallocate(Freq) ->
    call({deallocate,Freq}).

call(Message) ->
    frequency ! {request, self(), Message},
    receive 
	{reply, Reply} -> Reply
    end.

loop(Frequencies) ->
    receive
	{request, Pid, allocate} ->
	    {NewFreqs, Reply} = allocate(Frequencies, Pid),
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

allocate({[],Allocated}, _Pid) ->
    {{[],Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, Freq}.
deallocate({Free, Allocated}, Freq) ->
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.


	     

