-module(proper_fsm).
-export([commands/1, commands/2]).
-export([run_commands/2, run_commands/3]).
-export([state_names/1]).
-export([analyze/1]).
-export([initial_state/0, command/1, precondition/2, next_state/3,
	 postcondition/3]).

%% Exported for testing purposes
-export([target_states/4]).

-include("proper_internal.hrl").
-define(TIMES, 100000).


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
-type transition()     :: {state_name(), state_name(), mfa()}.
-type flow()           :: {float(), transition()}.
-type result()         :: term().
-type command()        :: {'init',fsm_state()}
		          | {'set',symb_var(),symb_call()}.
-type command_list()   :: [command()].
-type history()        :: [{fsm_state(),result()}].

-record(state, {name :: state_name(),
		data :: state_data(),
		mod  :: mod_name()}).
-type state() :: #state{}.

-record (state_info, {prob        :: float(),
		      transitions :: [{transition_gen(),pos_integer(),float()}],
		      norm        :: float()}).
-type state_info() :: #state_info{}.


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

%%TODO: _Module??
-spec run_commands(mod_name(), command_list(), proper_symb:var_values()) ->
			  {history(),fsm_state(),fsm_result()}.
run_commands(_Module, Cmds, Env) ->
    {H,S,Res} = proper_statem:run_commands(?MODULE, Cmds, Env),
    History = [{{Name,Data},R} ||
		  {#state{name=Name, data=Data},R} <- H],
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
    S_name = S#state.name,
    S_data = S#state.data,
    choose_transition(Mod, S_name, get_transitions(Mod, S_name, S_data)).

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
	'$no_target' -> io:format("no_target!\n"), false
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

-spec get_transitions(mod_name(), state_name(), state_data()) ->
			     [transition_gen()].
get_transitions(Module, StateName, Data) ->
    case StateName of
	From when is_atom(From) ->
	    Module:From(Data);
	From when is_tuple(From) ->
	    Fun = element(1, From),
	    Args = tl(tuple_to_list(From)),
	    apply(Module, Fun, Args ++ [Data])
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
    List = [CallGen || {_,CallGen} <- T_list, can_generate(CallGen)],
    proper_types:oneof(List).

-spec choose_weighted_transition(mod_name(), state_name(), [transition_gen()]) ->
					proper_types:type().
choose_weighted_transition(Mod, From, T_list) ->
    List = [{weight(Mod, From, To, CallGen), CallGen}
	    || {To,CallGen} <- T_list, can_generate(CallGen)],
    proper_types:frequency(List).

-spec weight(mod_name(), state_name(), state_name(), symb_call_gen()) ->
		    pos_integer().
weight(Mod, From, To, CallGen) ->
    case To of
	history ->
	    Mod:weight(From, From, CallGen);
	_ ->
	    Mod:weight(From, To, CallGen)
    end.

-spec precondition_probability(mod_name(), state_name(), state_name(),
			       symb_call_gen()) -> float().
precondition_probability(Mod, From, To, CallGen) ->
    case To of
	history ->
	    Mod:precondition_probability(From, From, CallGen);
	_ ->
	    Mod:precondition_probability(From, To, CallGen)
    end.

-spec is_exported(mod_name(), {fun_name(),arity()}) -> boolean().
is_exported(Mod, Fun) ->
    lists:member(Fun, Mod:module_info(exports)).

-spec can_generate(symb_call_gen()) -> boolean().
can_generate(CallGen) ->
    try proper_gen:safe_generate(CallGen) of
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
target_states(Module, From, StateData, Call) when is_atom(From) ->
    Transitions = apply(Module, From, [StateData]),
    find_target(Transitions, Call, []);
target_states(Module, From, StateData, Call) when is_tuple(From) ->
    Fun = element(1, From),
    Args = tl(tuple_to_list(From)),
    Transitions = apply(Module, Fun, Args ++ [StateData]),
    find_target(Transitions, Call, []).

-spec find_target([transition_gen()], symb_call(), [transition_gen()]) ->
			 [state_name()] | '$no_target'.
find_target([], _, []) -> '$no_target';
find_target([], _, Accum) -> Accum;
find_target(Transitions, Call, Accum) ->
    [{Target,CallGen}|Rest] = Transitions,
    case is_instance_call(Call, CallGen) of
	true  -> find_target(Rest, Call, [Target|Accum]);
	false -> find_target(Rest, Call, Accum)
    end.

-spec is_instance_call(symb_call(), symb_call_gen()) -> boolean().
is_instance_call(Call, CallGen) ->
    case can_generate(CallGen) of
	true ->
	    is_compatible(Call, CallGen);
	false ->
	    false
    end.

-spec is_compatible(symb_call(), symb_call_gen()) -> boolean().
is_compatible({call,M,F,A1}, {call,M,F,A2})
  when is_list(A1), is_list(A2) ->
    length(A1) =:= length(A2);
is_compatible(_, _) ->
    false.


%% -----------------------------------------------------------------------------
%% State space analysis
%% -----------------------------------------------------------------------------

-spec analyze(mod_name()) -> [flow()].
analyze(Mod) ->
    S_name = Mod:initial_state(),
    S_data = Mod:initial_state_data(),
    Transitions = cook(Mod, S_name, get_transitions(Mod, S_name, S_data)),
    Info = #state_info{prob = 1.0,
		       transitions = Transitions,
		       norm = normalize(Transitions)},
    R = orddict:to_list(
	  update_all_states(Mod, ?TIMES,
			    orddict:store(S_name, Info, orddict:new()))),
    %%io:format("Dict: ~w\n", [R]),
    Flow = [{Prob*Pre*Weight/N, {State, Target, get_mfa(Call)}}
	    || {State, #state_info{prob=Prob, transitions=Tr, norm=N}} <- R,
	       {{Target,Call},Weight,Pre} <- Tr],
    Sum = lists:foldl(fun({F,_}, Acc) -> F + Acc end, 0, Flow),
    [{F/Sum,Transition} || {F,Transition} <- Flow].

-spec get_mfa(symb_call()) -> mfa().
get_mfa({call,M,F,Args}) -> {M,F,length(Args)}.

-spec normalize([{transition_gen(),pos_integer(),float()}]) -> float().
normalize(Transitions) ->
    lists:foldl(fun({_,W,Pre}, Accum) -> W * Pre + Accum end, 0,
		Transitions).

-spec cook(mod_name(), state_name(), [transition_gen()]) ->
		  [{transition_gen(),pos_integer(),float()}].
cook(Module, From, Transitions) ->
    case is_exported(Module, {weight,3}) of
	true ->
	    case is_exported(Module, {precondition_probability,3}) of
		true ->
		    [{{cook_history(From, To),CallGen},
		      weight(Module, From, To, CallGen),
		      precondition_probability(Module, From, To, CallGen)}
		     || {To,CallGen} <- Transitions];
		false ->
		    [{{cook_history(From, To),CallGen},
		      weight(Module, From, To, CallGen),1}
		     || {To,CallGen} <- Transitions]
	    end;
	false ->
	    case is_exported(Module, {precondition_probability,3}) of
		true ->
		    [{{cook_history(From, To),CallGen},
		      1,precondition_probability(Module, From, To, CallGen)}
		     || {To,CallGen} <- Transitions];
		false ->
		    [{{cook_history(From, To),CallGen},1,1}
		     || {To,CallGen} <- Transitions]
	    end
    end.

-spec cook_history(state_name(), state_name()) -> state_name().
cook_history(From, history) -> From;
cook_history(_, To) -> To.

-spec update_all_states(mod_name(), non_neg_integer(), orddict:orddict()) ->
			       orddict:orddict().
update_all_states(_Module, 0, Dict) -> Dict;
update_all_states(Module, N, Dict) ->
    update_all_states(Module, N-1, update_all_states(Module, Dict)).

-spec update_all_states(mod_name(), orddict:orddict()) -> orddict:orddict().
update_all_states(Module, Dict) ->
    AllStates =
	orddict:fold(
	  fun(_S, Info, Accum) ->
		  begin
		      Prior = Info#state_info.prob,
		      Norm  = Info#state_info.norm,
		      case [{To,Prior * W * P / Norm}
			    || {{To,_},W,P} <- Info#state_info.transitions,
			       To =/= history,
			       not orddict:is_key(To, Accum)] of
			  []        -> Accum;
			  NewStates -> add_states(Module, NewStates, Accum)
		      end
		  end
	  end,
	  Dict, Dict),
    orddict:map(fun(S,Info) -> set_state_prob(S, Info, AllStates) end,
		AllStates).

-spec add_states(mod_name(), [{state_name(),float()}], orddict:orddict()) ->
			orddict:orddict().
add_states(Module, States, Dict) ->
    lists:foldl(
      fun(S, Accum) -> add_state(Module, S, Accum) end,
      Dict,
      States).

-spec add_state(mod_name(), {state_name(),float()}, orddict:orddict()) ->
		       orddict:orddict().
add_state(Mod, {S,P}, Dict) ->
    Data = Mod:initial_state_data(),
    Transitions = cook(Mod, S, get_transitions(Mod, S, Data)),
    Info = #state_info{prob = P,
		       transitions = Transitions,
		       norm = normalize(Transitions)},
    orddict:store(S, Info, Dict).

-spec set_state_prob(state_name(), state_info(), orddict:orddict()) ->
			    state_info().
set_state_prob(S, S_info, AllStates) ->
    V = orddict:fold(
	  fun(_State, Info, Acc) ->
		  Prior = Info#state_info.prob,
		  Norm = Info#state_info.norm,
		  C = compute(Info#state_info.transitions, S, Norm),
		  Acc + Prior*C
	  end,
	  0,
	  AllStates),
    S_info#state_info{prob = V}.

-spec compute([{transition_gen(),pos_integer(),float()}], state_name(),
	      float()) -> float().
compute(List, S, Norm) ->
    compute_tr(List, S, Norm, 0.0).

-spec compute_tr([{transition_gen(),pos_integer(),float()}], state_name(),
		 float(), float()) -> float().
compute_tr([], _, _, C) -> C;
compute_tr([{{S,_},W,P}|Rest], S, Norm, C) ->
    compute_tr(Rest, S, Norm, C + P*W/Norm);
compute_tr([_|Rest], S, Norm, C) ->
    compute_tr(Rest, S, Norm, C).
