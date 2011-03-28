-module(proper_fsm).

-export([commands/1, commands/2]).
-export([run_commands/2, run_commands/3]).
-export([state_names/1]).

-include("proper_internal.hrl").


%% -----------------------------------------------------------------------------
%% Type declarations
%% -----------------------------------------------------------------------------

-type state_name() :: atom() | tuple().
-type symbolic_state_data() :: term().
-type dynamic_state_data() :: term().
-type symbolic_state() :: {state_name(),symbolic_state_data()}.
-type dynamic_state() :: {state_name(),dynamic_state_data()}.

-type symb_var() :: {'var',proper_symb:var_id()}.
-type symb_call() :: {'call',mod_name(),fun_name(),[term()]}.
-type transition() :: {state_name(), symb_call()}.
-type command() :: {'init',symbolic_state()}
		   | {'set',symb_var(),symb_call()}.

-type command_list() :: [command()].
-type history() :: [{dynamic_state(),term()}].

%%TODO: import these from proper.erl 
-type exc_kind() :: 'throw' | 'error' | 'exit'.
-type exc_reason() :: term().
-type stacktrace() :: [{atom(),atom(),arity() | [term()]}].
-type exception() ::  {'exception',exc_kind(),exc_reason(),stacktrace()}.
-type statem_result() :: 'ok'
			 | 'initialization_error'
			 | {'precondition', boolean() | exception()}
			 | {'postcondition', boolean() | exception()}
			 | exception().

		      
%% -----------------------------------------------------------------------------
%% Command generation
%% -----------------------------------------------------------------------------

-define(COMMANDS(PropList), proper_types:new_type(PropList,commands)).

-spec commands(mod_name()) -> proper_types:type().
commands(Module) ->
    ?COMMANDS(
       [{generator,
	 fun(Size) -> gen_commands(Size, Module,
				   {Module:initial_state(),Module:initial_state_data()},
				   false)
	 end},
	{is_instance, fun commands_test/1}]).

-spec commands(mod_name(), symbolic_state()) -> proper_types:type().
commands(Module, InitialState) ->
    ?COMMANDS(
       [{generator, fun(Size) -> gen_commands(Size, Module, InitialState, true) end},
	{is_instance, fun commands_test/1}]).

-spec gen_commands(size(), mod_name(), symbolic_state(), boolean()) -> command_list().
gen_commands(Size, Mod, InitialState, InitFlag) ->
    Len = proper_arith:rand_int(0,Size),
    erlang:put('$initial_state', InitialState),
    {StateName,StateData} = InitialState,
    CmdList = gen_commands(Mod, StateName, StateData, [], Len, Len,
			   get('$constraint_tries')),
    case InitFlag of
	true -> [{init,InitialState}|CmdList];
	false -> CmdList
    end.

-spec gen_commands(mod_name(), state_name(), symbolic_state_data(), command_list(),
		   size(), non_neg_integer(), non_neg_integer()) -> command_list().
gen_commands(_, _, _, _, _, _, 0) ->
    exit:error();
gen_commands(_, _, _, Commands, _, 0, _) ->
    lists:reverse(Commands);
gen_commands(Module, From, StateData, Commands, Len, Count, Tries) ->
    TransitionGen = 
	case From of
	    State when is_atom(State) ->
		proper_types:oneof(apply(Module, From, [StateData]));
	    State when is_tuple(State) -> 
		StateName = element(1, From),
		Args = tl(tuple_to_list(From)),
		proper_types:oneof(apply(Module, StateName, [StateData|Args]))
	end,
    try proper_gen:clean_instance(proper_gen:safe_generate(TransitionGen)) of
	{ok,{To,Call}} ->
	    case safe_precondition(Module, From, To, StateData, Call) of
		true ->
		    Var = {var, Len-Count+1},
		    Command= {set,Var,Call},
		    case To of
			history ->
			    NextStateData =
				Module:next_state_data(From, From, StateData, Var, Call),
			    gen_commands(Module, From, NextStateData,
					 [Command|Commands], Len, Count-1, Tries);
			_ ->
			    NextStateData =
				Module:next_state_data(From, To, StateData, Var, Call),
			    gen_commands(Module, To, NextStateData,
					 [Command|Commands], Len, Count-1, Tries)
		    end;
		_ ->  
		    gen_commands(Module, From, StateData, Commands, Len, Count, Tries-1)
	    end
    catch
	_:_ ->
	    gen_commands(Module, From, StateData, Commands, Len, Count, Tries-1)
    end.

-spec safe_precondition(mod_name(), state_name(), state_name(), symbolic_state_data(),
			symb_call()) -> boolean().
safe_precondition(Module, From, history, StateData, Call) ->
    Module:precondition(From, From, StateData, Call);
safe_precondition(Module, From, To, StateData, Call) ->
    Module:precondition(From, To, StateData, Call).


%% -----------------------------------------------------------------------------
%% Command execution
%% -----------------------------------------------------------------------------

-spec run_commands(mod_name(), command_list()) ->
			  {history(),dynamic_state(),statem_result()}.
run_commands(Module, Cmds) ->
    run_commands(Module, Cmds, []).

-spec run_commands(mod_name(), command_list(), proper_symb:var_values()) ->
			  {history(),dynamic_state(),statem_result()}.
run_commands(Module, Commands, Env) ->
    InitialState = get_initial_state(Commands),
    case safe_eval_init(Env, InitialState) of
	{ok,DynState} ->
	    do_run_command(Commands, Env, Module, [], DynState);
	{error,Reason} ->
	    {[], [], Reason}
    end.

-spec safe_eval_init(proper_symb:var_values(), symbolic_state()) ->
			    {'ok',dynamic_state()} | {'error',statem_result()}.
safe_eval_init(Env, SymbState) ->
    try proper_symb:eval(Env, SymbState) of
	DynState ->
	    {ok,DynState}
    catch
	_Exception:_Reason ->
	    {error,initialization_error}
    end.

-spec do_run_command(command_list(), proper_symb:var_values(), mod_name(),
		     history(), dynamic_state()) ->
			    {history(),dynamic_state(),statem_result()}.
do_run_command(Commands, Env, Module, History, State) ->
    {From,StateData} = State,
    case Commands of
	[] ->
     	    {lists:reverse(History), State, ok};
	[{init,_S}|Rest] ->
	    do_run_command(Rest, Env, Module, History, State);
 	[{set,{var,V},{call,M,F,A}=SymbCall}|Rest] ->
	    NextState = target_state(Module, From, StateData, SymbCall),
	    To = case NextState of
		     history -> From;
		     _ -> NextState
		 end,
	    M2 = proper_symb:eval(Env, M),
	    F2 = proper_symb:eval(Env, F),
	    A2 = proper_symb:eval(Env, A),
	    Call = {call,M2,F2,A2},
	    case check_precondition(Module, From, To, StateData, Call) of
		true ->
		    case safe_apply(M2, F2, A2) of
			{ok,Res} ->
			    StateData2 = Module:next_state_data(From, To, StateData,
								Res, Call),
			    State2 = {To,StateData2},
			    History2 = [{State,Res}|History],
			    case check_postcondition(Module, From, To,
						     StateData, Call, Res) of
				true ->
				    Env2 = [{V,Res}|Env],
				    do_run_command(Rest, Env2, Module, History2,
						   State2);
				false ->
				    {lists:reverse(History2), State2,
				     {postcondition, false}};
			   	{exception,_,_,_} = Exception ->
				    {lists:reverse(History2), State2, 
				     {postcondition,Exception}}
			    end;
			{error,Exception} ->
			    {lists:reverse(History),State,Exception}
		    end;
		false ->
		    {lists:reverse(History),State,{precondition,false}};
		{exception,_,_,_} = Exception ->
		    {lists:reverse(History),State,{precondition,Exception}}
	    end
    end.

-spec check_precondition(mod_name(), state_name(), state_name(),
			 dynamic_state_data(), symb_call()) ->
				boolean() | exception().
check_precondition(Module, From, To, StateData, Call) ->
    try Module:precondition(From, To, StateData, Call) of
	Result -> Result
    catch
	Kind:Reason ->
	    {exception,Kind,Reason,erlang:get_stacktrace()}
    end.

-spec check_postcondition(mod_name(), state_name(), state_name(),
			 dynamic_state_data(), symb_call(), term()) ->
				 boolean() | exception().
check_postcondition(Module, From, To, StateData, Call, Res) ->
    try Module:postcondition(From, To, StateData, Call, Res) of
	Result -> Result
    catch
	Kind:Reason ->
	    {exception,Kind,Reason,erlang:get_stacktrace()}
    end.

-spec safe_apply(mod_name(), fun_name(), [term()]) ->
			{ok,term()} | {error,exception()}.
safe_apply(M, F, A) ->    
    try apply(M, F, A) of
	Result -> {ok,Result}
    catch
	Kind:Reason ->
	    {error,{exception,Kind,Reason,erlang:get_stacktrace()}}
    end.


%% -----------------------------------------------------------------------------
%% Utility functions
%% -----------------------------------------------------------------------------

-spec state_names(history()) -> [state_name()].
state_names(History) ->
    [SName || {{SName,_},_Res} <- History].

-spec get_initial_state(command_list()) -> symbolic_state().
get_initial_state(Cmds) ->
    case Cmds of
	[{init,S}|_] -> S;
	_ -> erlang:get('$initial_state')
    end.

-spec target_state(mod_name(), state_name(), symbolic_state_data(), symb_call()) ->
			  state_name().
target_state(Module, From, StateData, Call) ->
    AllTransitions = apply(Module, From, [StateData]),
    find_target(AllTransitions, Call).

-spec find_target([transition(),...], symb_call()) -> state_name().
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

-spec commands_test(proper_gen:imm_instance()) -> boolean().
commands_test(X) when is_list(X) ->
    lists:all(fun is_command/1, X);
commands_test(_X) -> false.

-spec is_command(proper_gen:imm_instance()) -> boolean().
is_command({set,{var,V},{call,M,F,A}})
  when is_integer(V), is_atom(M), is_atom(F), is_list(A) -> true;
is_command({init,_S}) -> true;
is_command(_Other) -> false.
