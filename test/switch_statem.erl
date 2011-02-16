-module(switch_statem).

-export([initial_state/0, command/1, precondition/2, postcondition/3,
	next_state/3]).
-export([set_up/0, clean_up/0]).
-export([press/0, release/0]).

-include_lib("proper/include/proper.hrl").

press() -> 
    on.

release() -> 
    off.

set_up() ->
    ok.

clean_up() ->
    ok.

initial_state() -> on.

command(S) ->
    frequency([{1,stop},
	       {10,{call,?MODULE,press,[]}},
	       {10,{call,?MODULE,release,[]}}]).

precondition(S,{call,_,press,_}) -> 
    S == off;
precondition(S,{call,_,release,_}) -> 
    S == on.

postcondition(_,{call,_,press,_},R) -> 
    R == on;
postcondition(_,{call,_,release,_},R) -> 
    R == off.

next_state(_,_,{call,_,press,_}) -> 
    on;
next_state(_,_,{call,_,release,_}) -> 
    off.

prop_switch() ->
    ?FORALL(Cmds,commands(?MODULE,initial_state()),
	    begin
		{_,_,R} = run_commands(?MODULE,Cmds),
		R == ok
	    end).

