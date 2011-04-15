-module(nogen_statem).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

initial_state() -> [].

command(_S) ->
    oneof([{call,?MODULE,foo,[impossible_arg()]},
	   {call,?MODULE,bar,[]}]).

impossible_arg() ->
    ?SUCHTHAT(X, non_neg_integer(), X < 0).

precondition(_, _) -> true.

next_state(S, _, _) -> S.

postcondition(_, _, _) -> true.

foo(_) -> ok.
bar() -> 42.

prop_simple() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{_H,_S,Res} = run_commands(?MODULE, Cmds),
		equals(Res, ok)
	    end).
