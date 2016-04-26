%%---------------------------------------------------------------------
%% From Matthias Kretschmer
%%
%% I encountered the problem that I cannot have improper lists in my
%% symbolic state. As I like to use a dictionary which might include
%% improper lists, I made this little fix (#102).
%%---------------------------------------------------------------------
-module(improper_lists_statem).
-export([command/1, initial_state/0, next_state/3,
	 precondition/2, postcondition/3, foo/1]).

-include_lib("proper/include/proper.hrl").

command(_State) ->
    return({call, ?MODULE, foo, [[a|b]]}).

initial_state() ->
    state.

next_state(State, _, _) ->
    State.

precondition(_, _) ->
    true.

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
