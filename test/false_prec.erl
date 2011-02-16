-module(false_prec).

-export([initial_state/0, command/1, precondition/2, next_state/3]).
-export([press/0, release/0]).

-include_lib("proper/include/proper.hrl").

press() -> 
    on.

release() -> 
    off.

initial_state() -> on.

command(S) ->
    frequency([{10,{call,?MODULE,press,[]}} || S == off] ++
	      [{10,{call,?MODULE,release,[]}} || S == on]).

precondition(S,{call,_,press,_}) -> 
    S == on;
precondition(S,{call,_,release,_}) -> 
    S == off.

next_state(_,_,{call,_,press,_}) -> 
    on;
next_state(_,_,{call,_,release,_}) -> 
    off.


