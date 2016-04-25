-module(lists_statem).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

initial_state() ->
    state.

command(_State) ->
    return({call, ?MODULE, foo, [[a|b]]}).

precondition(_, _) ->
    true.

next_state(State, _, _) ->
    State.

postcondition(_, _, _) ->
    true.

foo(_Something) ->
    ok.

prop_simple() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{_H,_S,Res} = run_commands(?MODULE, Cmds),
		equals(Res, ok)
	    end).
