-module(freq_statem).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

-define(FREQUENCIES,frequency:get_frequencies()). 
frequency() ->
    oneof(?FREQUENCIES).

initial_state() -> {?FREQUENCIES,[]}.    

command(_) -> oneof([{call,frequency,allocate,[]},
		     {call,frequency,deallocate,[frequency()]}]).

precondition(_Freqs,{call,frequency,allocate,[]}) ->
    true;
precondition({_Free,Alloc},{call,frequency,deallocate,[Freq]}) ->
    lists:member(Freq,Alloc);
precondition(_,_) ->
    true.

next_state({[],Alloc},_V,{call,frequency,allocate,[]}) ->
    {[],Alloc};
next_state({[Freq|Free],Alloc},_V,{call,frequency,allocate,[]}) ->
    {Free,[Freq|Alloc]};
next_state({Free,Alloc},_V,{call,frequency,deallocate,[Freq]}) ->
    {[Freq|Free],lists:delete(Freq,Alloc)}.

postcondition({[],_Alloc},{call,frequency,allocate,[]},{error,no_frequency}) ->
    false;
postcondition({Free,Alloc},{call,frequency,allocate,[]},Freq) ->
    lists:member(Freq,Free) andalso (not lists:member(Freq,Alloc));
postcondition(_,{call,frequency,deallocate,[_Freq]},_Res) ->
    true;
postcondition(_,_,_) -> false.    

prop_freq() ->
    ?FORALL(Cmds,commands(?MODULE),
	    begin
		frequency:start(),
		{H,S,Res} = run_commands(?MODULE,Cmds),
		frequency:stop(),
		?WHENFAIL(
		   io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
		   Res == ok)
	    end).
