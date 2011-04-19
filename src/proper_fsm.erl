-module(proper_fsm).
-export([commands/1, commands/2]).
-export([run_commands/2, run_commands/3]).
-export([state_names/1]).
-export([initial_state/0, command/1, precondition/2, next_state/3,
	 postcondition/3]).

%% Exported for testing purposes
-export([target_states/4]).

-include("proper_internal.hrl").


%% -----------------------------------------------------------------------------
%% Type declarations
%% -----------------------------------------------------------------------------

-type symb_var()   :: proper_statem:symb_var().
-type symb_call()  :: proper_statem:symb_call().
-type fsm_result() :: proper_statem:statem_result().

-type state_name()     :: atom() | tuple().
-type state_data()     :: term().
-type fsm_state()      :: {state_name(),state_data()}.
-type symb_call_gen()  :: {'call',mod_name(),fun_name(),[proper_types:type()]}.
-type transition_gen() :: {state_name(),symb_call_gen()}.
-type result()         :: term().
-type command()        :: {'init',fsm_state()}
		          | {'set',symb_var(),symb_call()}.
-type command_list()   :: [command()].
-type history()        :: [{fsm_state(),result()}].

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

-spec run_commands(mod_name(), command_list(), proper_symb:var_values()) ->
         {history(),fsm_state(),fsm_result()}.
run_commands(Module, [{init,Init}|Rest], Env) ->
    Cmds = [{init,Init#state{mod = Module}}|Rest],
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
    From = S#state.name,
    choose_transition(Mod, From, get_transitions(Mod, From, Data)).

-spec precondition(state(), symb_call()) -> boolean().
precondition(S, Call) ->
    Mod = S#state.mod,
    From = S#state.name,
    Data = S#state.data,
    case target_states(Mod, From, Data, Call) of
	Targets when is_list(Targets) ->
	    case [To || To <- Targets,
			Mod:precondition(From, cook_history(From, To),
					 Data, Call)] of
		[]   -> false;
		[_T] -> true
	    end;
	'$no_target' -> false
    end.

-spec next_state(state(), symb_var() | result(), symb_call()) -> state().
next_state(S, Var, Call) ->
    Mod = S#state.mod,
    From = S#state.name,
    Data = S#state.data,
    To = cook_history(From, transition_target(Mod, From, Data, Call)),
    S#state{name=To, data=Mod:next_state_data(From, To, Data, Var, Call)}.

-spec postcondition(state(), symb_call(), result()) -> boolean().
postcondition(S, Call, Res) ->
    Mod = S#state.mod,
    From = S#state.name,
    Data = S#state.data,
    To = cook_history(From, transition_target(Mod, From, Data, Call)),
    Mod:postcondition(From, To, Data, Call, Res).


%% -----------------------------------------------------------------------------
%% Utility functions
%% -----------------------------------------------------------------------------

-spec get_transitions(mod_name(), state_name(), state_data()) ->
         [transition_gen()].
get_transitions(Mod, StateName, Data) ->
    case StateName of
	From when is_atom(From) ->
	    Mod:From(Data);
	From when is_tuple(From) ->
	    Fun = element(1, From),
	    Args = tl(tuple_to_list(From)),
	    apply(Mod, Fun, Args ++ [Data])
    end.

-spec choose_transition(mod_name(), state_name(), [transition_gen()]) ->
         proper_types:type().
choose_transition(Mod, From, T_list) ->
    case is_exported(Mod, {weight,3}) of
	false ->
	    choose_uniform_transition(T_list);
	true ->
	    choose_weighted_transition(Mod, From, T_list)
    end.

-spec choose_uniform_transition([transition_gen()]) -> proper_types:type().
choose_uniform_transition(T_list) ->
    List = [CallGen || {_,CallGen} <- T_list],
    safe_union(List).

-spec choose_weighted_transition(mod_name(), state_name(),
         [transition_gen()]) -> proper_types:type().
choose_weighted_transition(Mod, From, T_list) ->
    List = [{Mod:weight(From, cook_history(From, To), CallGen), CallGen}
	    || {To,CallGen} <- T_list],
    safe_weighted_union(List).

-spec cook_history(state_name(), state_name()) -> state_name().
cook_history(From, history) -> From;
cook_history(_, To)         -> To.

-spec is_exported(mod_name(), {fun_name(),arity()}) -> boolean().
is_exported(Mod, Fun) ->
    lists:member(Fun, Mod:module_info(exports)).

-spec transition_target(mod_name(), state_name(), state_data(), symb_call()) ->
         state_name().
transition_target(Mod, From, Data, Call) ->
    Targets = target_states(Mod, From, Data, Call),
    hd([T || T <- Targets,
	     Mod:precondition(From, cook_history(From, T), Data, Call)]).

-spec target_states(mod_name(), state_name(), state_data(), symb_call()) ->
         [state_name()] | '$no_target'.
target_states(Module, From, StateData, Call) ->
    find_target(get_transitions(Module, From, StateData), Call, []).

-spec find_target([transition_gen()], symb_call(), [transition_gen()]) ->
         [state_name()] | '$no_target'.
find_target([], _, []) -> '$no_target';
find_target([], _, Accum) -> Accum;
find_target(Transitions, Call, Accum) ->
    [{Target,CallGen}|Rest] = Transitions,
    case is_compatible(Call, CallGen) of
	true  -> find_target(Rest, Call, [Target|Accum]);
	false -> find_target(Rest, Call, Accum)
    end.

-spec is_compatible(symb_call(), symb_call_gen()) -> boolean().
is_compatible({call,M,F,A1}, {call,M,F,A2})
  when is_list(A1), is_list(A2) ->
    length(A1) =:= length(A2);
is_compatible(_, _) ->
    false.


%% -----------------------------------------------------------------------------
%% Special types and generators
%% -----------------------------------------------------------------------------

%% @private
-spec safe_union([proper_types:raw_type(),...]) -> proper_types:type().
safe_union(RawChoices) ->
    Choices = [proper_types:cook_outer(C) || C <- RawChoices],
    proper_types:subtype(
      [{generator, fun() -> safe_union_gen(Choices) end}],
      proper_types:union(Choices)).

%% @private
-spec safe_weighted_union([{frequency(),proper_types:raw_type()},...]) ->
         proper_types:type().
safe_weighted_union(RawFreqChoices) ->
    CookFreqType = fun({Freq,RawType}) ->
			   {Freq,proper_types:cook_outer(RawType)} end,
    FreqChoices = lists:map(CookFreqType, RawFreqChoices),
    Choices = [T || {_F,T} <- FreqChoices],
    proper_types:subtype(
      [{generator, fun() -> safe_weighted_union_gen(FreqChoices) end}],
      proper_types:union(Choices)).

%% @private
-spec safe_union_gen([proper_types:type(),...]) -> proper_gen:imm_instance().
safe_union_gen(Choices) ->
    {Choice,Type} = proper_arith:rand_choose(Choices),
    try proper_gen:generate(Type) of
	Instance -> Instance
    catch
	_:_ ->
	    safe_union_gen(proper_arith:list_remove(Choice, Choices))
    end.

%% @private
-spec safe_weighted_union_gen([{frequency(),proper_types:type()},...]) ->
         proper_gen:imm_instance().
safe_weighted_union_gen(FreqChoices) ->
    {Choice,Type} = proper_arith:freq_choose(FreqChoices),
    try proper_gen:generate(Type) of
	Instance -> Instance
    catch
	_:_ ->
	    safe_weighted_union_gen(proper_arith:list_remove(Choice,
							     FreqChoices))
    end.
