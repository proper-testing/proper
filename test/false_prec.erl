-module(false_prec).

-export([initial_state/0, command/1, precondition/2, next_state/3]).
-export([foo/0, bar/0]).

-include_lib("proper/include/proper.hrl").

foo() -> ok.

bar() -> ok.

initial_state() -> oneof([foo_state, bar_state]).

command(S) ->
    oneof([{call,?MODULE,foo,[]} || S == foo_state] ++
	  [{call,?MODULE,bar,[]} || S == bar_state]).

precondition(S,{call,_,foo,_}) -> 
    S == bar_state;
precondition(S,{call,_,bar,_}) -> 
    S == foo_state.

next_state(_,_,{call,_,foo,_}) -> 
    bar_state;
next_state(_,_,{call,_,bar,_}) -> 
    foo_state.


