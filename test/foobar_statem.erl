-module(foobar_statem).

-export([initial_state/0, command/1, precondition/2, next_state/3, postcondition/3]).
-export([foo/0, bar/0, history/0]).
-export([set_up/0, clean_up/0]).

-include_lib("proper/include/proper.hrl").

foo() -> ok.
bar() -> ok.
history() -> ok.

initial_state() -> foo_state.

command(_S) ->
    frequency([{100,{call,?MODULE,foo,[]}},
	       {100,{call,?MODULE,bar,[]}},
	       {1,{call,?MODULE,history,[]}}]).

precondition(S, {call,_,foo,_}) -> 
    S =:= foo_state;
precondition(S, {call,_,bar,_}) -> 
    S =:= bar_state;
precondition(_, _) ->
    true.

next_state(_, _, {call,_,foo,_}) -> 
    bar_state;
next_state(_, _, {call,_,bar,_}) -> 
    foo_state;
next_state(S, _, {call,_,history,_}) ->
    S.

set_up() -> ok.
clean_up() -> ok.

postcondition(_, {call,_,foo,_}, R) -> 
    R =:= ok;
postcondition(_, {call,_,bar,_}, R) -> 
    R =:= ok;
postcondition(_, _, _) ->
    false.


prop_foobar() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{_,_,R} = run_commands(?MODULE, Cmds),
		R == ok
	    end).

