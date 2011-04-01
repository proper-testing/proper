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
	From when is_atom(From) ->
	    choose_transition(Mod, From, apply(Mod, From, [Data]));
	From when is_tuple(From) ->
	    Fun = element(1, From),
	    Args = tl(tuple_to_list(From)),
	    choose_transition(Mod, From, apply(Mod, Fun, Args ++ [Data]))
    end.

-spec precondition(state(), symb_call()) -> boolean().
precondition(S, Call) ->
    Mod = S#state.mod,
    From = S#state.name,
    Data = S#state.data,
    case target_states(Mod, From, Data, Call) of
	Targets when is_list(Targets) ->
	    case [T || T <- Targets, fsm_precondition(Mod, From, T, Data, Call)] of
		[]   -> false;
		[_T] -> true
	    end;
	'$no_target' -> io:format("no_target!\n"),false
    end.

-spec fsm_precondition(mod_name(), state_name(), state_name(), state_data(),
		       symb_call()) -> boolean().
fsm_precondition(Mod, From, To, Data, Call) ->
    case To of
	history ->
	    Mod:precondition(From, From, Data, Call);
	To ->
	    Mod:precondition(From, To, Data, Call)
    end.

-spec next_state(state(), symb_var() | result(), symb_call()) -> state().
next_state(S, Var, Call) ->
    Mod = S#state.mod,
    From = S#state.name,
    Data = S#state.data,
    case transition_target(Mod, From, Data, Call) of
	history ->
	    D = Mod:next_state_data(From, From, Data, Var, Call),
	    S#state{data=D};
	To ->
	    D = Mod:next_state_data(From, To, Data, Var, Call),
	    S#state{name=To, data=D}
    end.

-spec postcondition(state(), symb_call(), result()) -> boolean().
postcondition(S, Call, Res) ->
    Mod = S#state.mod,
    From = S#state.name,
    Data = S#state.data,
    case transition_target(Mod, From, Data, Call) of
	history ->
	    Mod:postcondition(From, From, Data, Call, Res);
	To ->
	    Mod:postcondition(From, To, Data, Call, Res)
    end.


%% -----------------------------------------------------------------------------
%% Utility functions
%% -----------------------------------------------------------------------------

-spec choose_transition(mod_name(), state_name(), [transition()]) ->
			       proper_types:type().
choose_transition(Mod, From, T_list) ->
    case is_exported(Mod, {weight,3}) of
	false ->
	    choose_uniform_transition(T_list);
	true ->
	    W_list = [{Mod:weight(From, To, Call), Call} || {To,Call} <- T_list],
	    choose_weighted_transition(W_list)
    end.

-spec choose_uniform_transition([transition()]) -> proper_types:type().
choose_uniform_transition(T_list) ->
    List = [Call || {_,Call} <- T_list, can_generate(Call)],
    proper_types:oneof(List).

-spec choose_weighted_transition([{frequency(),symb_call()}]) ->
					proper_types:type().
choose_weighted_transition(W_list) ->
    List = [T || {_,Call} = T <- W_list, can_generate(Call)],
    proper_types:frequency(List).

-spec is_exported(mod_name(), {fun_name(),arity()}) -> boolean().
is_exported(Mod, Fun) ->
    lists:member(Fun, Mod:module_info(exports)).

-spec can_generate(proper_types:type()) -> boolean().
can_generate(Type) ->
    try proper_gen:safe_generate(Type) of
	{ok,_Instance} -> true;
	{error,_Error} -> false
    catch
	_Exception:_Reason -> false
    end.

-spec transition_target(mod_name(), state_name(), state_data(), symb_call()) ->
			       state_name().
transition_target(Mod, From, Data, Call) ->
    Targets = target_states(Mod, From, Data, Call),
    [To] = [T || T <- Targets, fsm_precondition(Mod, From, T, Data, Call)],
    To.

-spec target_states(mod_name(), state_name(), state_data(), symb_call()) ->
			   [state_name()] | '$no_target'.
target_states(Module, From, StateData, Call) ->
    Transitions = apply(Module, From, [StateData]),
    find_target(Transitions, Call, []).

-spec find_target([transition()], symb_call(), [transition()]) ->
			 [state_name()] | '$no_target'.
find_target([], _, []) -> '$no_target';
find_target([], _, Accum) -> Accum;
find_target(Transitions, Call, Accum) ->
    [{Target,CallGen}|Rest] = Transitions,
    case is_instance_call(Call, CallGen) of
	true  -> find_target(Rest, Call, [Target|Accum]);
	false -> find_target(Rest, Call, Accum)
    end.

-spec is_instance_call(term(), term()) -> boolean().
is_instance_call({call,M,F,A}, {call,M,F,ArgsGen})
  when is_list(A), is_list(ArgsGen) ->
    length(A) =:= length(ArgsGen);
is_instance_call(_, _) ->
    false.
