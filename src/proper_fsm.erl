-module(proper_fsm).
-export([commands/1, commands/2]).
-export([run_commands/2, run_commands/3]).
-export([state_names/1]).
-export([initial_state/0, command/1, precondition/2, next_state/3,
	 postcondition/3]).

-include("proper_internal.hrl").


%% -----------------------------------------------------------------------------
%% Type declarations
%% -----------------------------------------------------------------------------

-type symb_var()   :: proper_statem:symb_var().
-type symb_call()  :: proper_statem:symb_call().
-type fsm_result() :: proper_statem:statem_result().

-type state_name() :: atom() | tuple().
-type state_data() :: term().
-type fsm_state()  :: {state_name(),state_data()}.
-type transition() :: {state_name(),symb_call()}.
-type result()     :: term().
-type command()    ::   {'init',fsm_state()}
		      | {'set',symb_var(),symb_call()}.
-type command_list() :: [command()].
-type history()    :: [{fsm_state(),result()}].

-record(state, {name :: state_name(),
		data :: state_data(),
		mod  :: mod_name()}).
-type state() :: #state{}.


%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

-spec commands(mod_name()) -> proper_types:type().
commands(Module) ->
    proper_types:with_parameter('$callback_mod', Module,
				proper_statem:commands(?MODULE)).

-spec commands(mod_name(), fsm_state()) -> proper_types:type().
commands(Module, {Name,Data}) ->
    State = #state{name=Name, data=Data, mod=Module},
    proper_statem:commands(?MODULE, State).

-spec run_commands(mod_name(), command_list()) ->
			  {history(),fsm_state(),fsm_result()}.
run_commands(Module, Cmds) ->
    run_commands(Module, Cmds, []).

%%TODO: should take Module into account in case its different?
-spec run_commands(mod_name(), command_list(), proper_symb:var_values()) ->
			  {history(),fsm_state(),fsm_result()}.
run_commands(_Module, Cmds, Env) ->
    {H,S,Res} = proper_statem:run_commands(?MODULE, Cmds, Env),
    History = [{{Name,Data},R} || {#state{name=Name, data=Data},R} <- H],
    State = {S#state.name, S#state.data},
    {History, State, Res}.

-spec state_names(history()) -> [state_name()].
state_names(History) ->
    [SName || {{SName,_},_Res} <- History].

		      
%% -----------------------------------------------------------------------------
%% State machine specification for fsm commands
%% -----------------------------------------------------------------------------

-spec initial_state() -> state().
initial_state() ->
    Mod = proper_types:parameter('$callback_mod'),
    S_name = Mod:initial_state(),
    S_data = Mod:initial_state_data(),
    #state{name=S_name, data=S_data, mod=Mod}.

-spec command(state()) -> proper_types:type().
command(S) ->
    Mod = S#state.mod,
    Data = S#state.data,
    case S#state.name of
	Fun when is_atom(Fun) ->
	    choose_transition(apply(Mod, Fun, [Data]));
	Name when is_tuple(Name) ->
	    Fun = element(1, Name),
	    Args = tl(tuple_to_list(Name)),
	    choose_transition(apply(Mod, Fun, Args ++ [Data]))
    end.

-spec precondition(state(), symb_call()) -> boolean().
precondition(S, Call) ->
    Mod = S#state.mod,
    From = S#state.name,
    Data = S#state.data,
    try target_state(Mod, From, Data, Call) of
	history ->
	    Mod:precondition(From, From, Data, Call);
	To ->
	    Mod:precondition(From, To, Data, Call)
    catch
	throw:'$no_target' -> false
    end.

-spec next_state(state(), symb_var() | result(), symb_call()) -> state().
next_state(S, Var, Call) ->
    Mod = S#state.mod,
    From = S#state.name,
    Data = S#state.data,
    To = target_state(Mod, From, Data, Call),
    case To of
	history ->
	    D = Mod:next_state_data(From, From, Data, Var, Call),
	    S#state{data=D};
	_ ->
	    D = Mod:next_state_data(From, To, Data, Var, Call),
	    S#state{name=To, data=D}
    end.

-spec postcondition(state(), symb_call(), result()) -> boolean().
postcondition(S, Call, Res) ->
    Mod = S#state.mod,
    From = S#state.name,
    Data = S#state.data,
    To = target_state(Mod, From, Data, Call),
    case To of
	history ->
	    Mod:postcondition(From, From, Data, Call, Res);
	_ ->
	    Mod:postcondition(From, To, Data, Call, Res)
    end.

    
%% -----------------------------------------------------------------------------
%% Utility functions
%% -----------------------------------------------------------------------------

-spec choose_transition([transition()]) -> proper_types:type().
choose_transition(T_list) ->
    ?LET({_To,Call}, proper_types:oneof(T_list), Call).

-spec target_state(mod_name(), state_name(), state_data(), symb_call()) ->
			  state_name().
target_state(Module, From, StateData, Call) ->
    Transitions = apply(Module, From, [StateData]),
    case find_target(Transitions, Call) of
	'$no_target' -> throw('$no_target');
	Target -> Target
    end.

-spec find_target([transition()], symb_call()) -> state_name() | '$no_target'. 
find_target([], _) -> '$no_target';
find_target(Transitions, Call) ->
    [{Target,CallGen}|Rest] = Transitions,
    case is_instance_call(Call, CallGen) of
	true -> Target;
	false -> find_target(Rest, Call)
    end.

-spec is_instance_call(term(), term()) -> boolean().
is_instance_call({call,M,F,A}, {call,M,F,ArgsGen}) ->
    is_instance_args(A, ArgsGen);
is_instance_call(_, _) ->
    false.

-spec is_instance_args([term()], [term()]) -> boolean().
is_instance_args([], []) -> true;
is_instance_args([], _) -> false;
is_instance_args(_, []) -> false;
is_instance_args([X|Rest1], [Gen|Rest2]) ->
    case proper_types:is_instance(X, Gen) of
	true -> is_instance_args(Rest1, Rest2); 
	false -> false
    end.
