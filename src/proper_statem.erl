-module(proper_statem).

-export([gen_commands/2, gen_commands/3, run_commands/2, run_commands/3, state_after/2,
	 remove_shrinker/5, split_shrinker/5, command_names/1, zip/2, commands_test/1]).

-export_type([symbolic_state/0]).

-include("proper_internal.hrl").

-type var_count() :: non_neg_integer().
-type tries() :: non_neg_integer().

-type symbolic_state() :: term().
-type dynamic_state() :: term().
-type history() :: [{dynamic_state(),term()}].

%%TODO: import these from proper.erl 
-type exc_kind() :: 'throw' | 'error' | 'exit'.
-type exc_reason() :: term().
-type stacktrace() :: [{atom(),atom(),arity() | [term()]}].
-type result() :: 'ok'
		  |'initialization_error'
		  |{'precondition',boolean()}
		  |{'postcondition',boolean()}
		  |{'exception',exc_kind(),exc_reason(),stacktrace()}.

-spec gen_commands(mod_name(),symbolic_state(),size()) -> [proper_gen:imm_instance()].
gen_commands(Module,StartState,Size) ->
    [{init,StartState}|gen_commands(Module, StartState,[], Size, Size, 
				    get('$constraint_tries'))].

-spec gen_commands(mod_name(),size()) -> [proper_gen:imm_instance()].
gen_commands(Module,Size) ->
    gen_commands(Module, Module:initial_state(),[], Size, Size, get('$constraint_tries')).

-spec gen_commands(mod_name(),symbolic_state(),[proper_gen:imm_instance()],size(),
		   var_count(),tries()) -> [proper_gen:imm_instance()].
gen_commands(_Module,_,Commands,_,0,_) ->
        lists:reverse(Commands);

gen_commands(Module,State,_,_,_,0) ->
    erlang:error({cannot_generate_statem,Module,State});

gen_commands(Module,State,Commands,Size,Count,Tries) ->    
    CmdDom = Module:command(State), 
    case proper_gen:safe_generate(CmdDom) of
	{ok,ImmInstance} ->
	    CleanInstance = proper_gen:clean_instance(ImmInstance),
	    case Module:precondition(State, CleanInstance) of
		true ->
		    Var = {var, Size-Count},
		    NextState = Module:next_state(State, Var, CleanInstance),
		    Command = {set, Var, CleanInstance},
		    gen_commands(Module,
				 NextState, 
				 [Command|Commands],
				 Size, Count-1, get('$constraint_tries'));
		_ ->
		    gen_commands(Module,State,Commands,Size,Count,Tries-1)
	    end;
	{error,_Reason} = Error ->
	    Error
    end.

-spec safe_eval_init(proper_symb:var_values(),symbolic_state()) -> 
			    {'ok',dynamic_state()}|{'error',result()}.
safe_eval_init(Env,SymbState) ->
    try proper_symb:eval(Env,SymbState) of
	DynState -> 
	    {ok,DynState}
    catch
	_Exception:_Reason ->
	    {error,'initialization_error'}
    end. 

-spec run_commands(mod_name(),[proper_gen:imm_instance()]) -> 
			  {history(),dynamic_state(),result()}.	  
run_commands(Module,Cmds) -> 
    run_commands(Module,Cmds,[]).

-spec run_commands(mod_name(),[proper_gen:imm_instance()],proper_symb:var_values()) ->
			  {history(),dynamic_state(),result()}.
run_commands(Module,Commands,Env) ->
    case safe_eval_init(Env,Module:initial_state()) of
	{ok,DynState} -> 
	    Self = self(),
	    _Child = proper:spawn_link_migrate(
		      fun() -> Self ! {result,
				       do_run_command(Commands,Env,Module,[],DynState)} 
		      end),
	    receive
		{result,Result} -> Result
	    end;	    
	{error,Reason} ->
	    {[],[],Reason}
    end.					       
		
-spec do_run_command([proper_gen:imm_instance()],proper_symb:var_values(),mod_name(),
		     [{dynamic_state(),term()}],dynamic_state()) -> 
			    {history(),dynamic_state(),result()}.	   
do_run_command(Commands, Env, Module, History, State) ->
    case Commands of
	[] -> 
	    {lists:reverse(History), State, ok};

	[{init,S}|Rest] ->
	    case safe_eval_init(Env,S) of
		{ok,DynState} ->
	   	    do_run_command(Rest, Env, Module, History, DynState);
		{error,Reason} ->
		    {[],[],Reason}	   
	    end; 
	
	[{set, {var,V}, {call,M,F,A}}|Rest] ->
	    M2=proper_symb:eval(Env,M), 
	    F2=proper_symb:eval(Env,F), 
	    A2=proper_symb:eval(Env,A),
	    try apply(M2,F2,A2) of
		Res ->
		    Call = {call, M2,F2,A2},
		    case Module:precondition(State,Call) of
			true ->
			    case Module:postcondition(State,Call,Res) of
				true ->
				    Env2 = [{V,Res}|proplists:delete(V,Env)],
				    State2 = Module:next_state(State,Res,Call),
				    History2 = [{State,Res}|History],
				    do_run_command(Rest, Env2, Module, History2, State2);
				Other ->
				    {lists:reverse(History), State, {postcondition, Other}}
			    end;
			_Other ->
			    {lists:reverse(History), State, {precondition, _Other}}
		    end
	    catch
		ExcKind:ExcReason ->
		    {lists:reverse(History), State, 
		     {exception,ExcKind,ExcReason,erlang:get_stacktrace()}}
	    end
    end.

-spec split_shrinker(mod_name(),symbolic_state(),proper_gen:imm_instance(), 
		     proper_types:type(),proper_shrink:state()) ->
			    {[proper_gen:imm_instance()],proper_shrink:state()}. 
split_shrinker(Module, StartState, [{init,StartState}|Commands], Type,State) ->
    {Slices,NewState} =  proper_shrink:split_shrinker(Commands,Type,State),
    IsValid= fun (CommandSeq) -> validate(Module,StartState,CommandSeq,[]) end,
    {lists:map(fun(L) -> [{init,StartState}|L] end,lists:filter(IsValid,Slices)),
     NewState};

split_shrinker(Module, StartState, Commands, Type,State) ->
    {Slices,NewState} =  proper_shrink:split_shrinker(Commands,Type,State),
    IsValid= fun (CommandSeq) -> validate(Module,StartState,CommandSeq,[]) end,
    {lists:filter(IsValid,Slices),NewState}.

-spec remove_shrinker(mod_name(),symbolic_state(),proper_gen:imm_instance(),
		      proper_types:type(),proper_shrink:state()) ->
			     {[proper_gen:imm_instance()],proper_shrink:state()}. 
remove_shrinker(Module,StartState,[{init,StartState}|Commands]=Cmds,Type,State) ->
   {CommandList,NewState} =  proper_shrink:remove_shrinker(Commands,Type,State),
   case CommandList of
       [] -> {[],NewState};
       [NewCommands] ->
	   case validate(Module,StartState,NewCommands,[]) of
	       true -> 
		   {[[{init,StartState}|NewCommands]],NewState};
	        _ ->
		   remove_shrinker(Module,StartState,Cmds,Type,NewState)
	   end
   end;

remove_shrinker(Module,StartState,Commands,Type,State) ->
   {CommandList,NewState} =  proper_shrink:remove_shrinker(Commands,Type,State),
   case CommandList of
       [] -> {[],NewState};
       [NewCommands] ->
	   case validate(Module,StartState,NewCommands,[]) of
	       true -> {[NewCommands],NewState};
	        _ ->
		   remove_shrinker(Module,StartState,Commands,Type,NewState)
	   end
   end.
 
-spec validate(mod_name(),symbolic_state(),[proper_gen:imm_instance()],
	       [proper_symb:symb_var()]) -> boolean().
validate(_Mod,_State,[],_Env) -> true;

validate(Module,_State,[{init,S}|Commands],_Env) ->
    validate(Module,S,Commands,_Env);

validate(Module,State,[{set,Var,{call,_M,_F,A}=Call}|Commands],Env) ->
    case Module:precondition(State,Call) of
	true ->
	    case args_defined(A,Env) of
		true ->
		    NextState = Module:next_state(State, Var, Call),
		    validate(Module,NextState,Commands,[Var|Env]);
		_ -> false
	    end;
	_ -> false
    end.

-spec args_defined([term()],[proper_symb:symb_var()]) -> boolean().			  
args_defined(A,Env) ->
    lists:all(fun ({var,I}) when is_integer(I) -> lists:member({var,I},Env);
		  (_V) -> true 
	      end, A).      

-spec state_after(mod_name(),[proper_gen:imm_instance()]) -> symbolic_state().		   
state_after(Module,Commands) ->
    NextState = fun(S,V,C) -> Module:next_state(S,V,C) end,
    lists:foldl(fun({init,S}, _) ->
			S;
		   ({set,Var,Call},S) ->
			NextState(S,Var,Call)
		end,
		Module:initial_state(),
		Commands).

-spec command_names([proper_gen:imm_instance()]) -> 
			   [{mod_name(),fun_name(),non_neg_integer()}].
command_names(Cmds) ->
    GetName = fun({set,_Var,{call,M,F,Args}}) -> {M,F,length(Args)} end,
    lists:map(GetName,Cmds).
    

-spec zip([A],[B]) -> [{A,B}].
zip(X,Y) ->
    Lx = length(X),
    Ly = length(Y),
    if Lx < Ly ->
	    lists:zip(X,lists:sublist(Y,Lx));
       Lx == Ly ->
	    lists:zip(X,Y);
       true ->
	    lists:zip(lists:sublist(X,Ly),Y)
    end.
    
-spec commands_test(proper_gen:imm_instance()) -> boolean().
commands_test(X) when is_list(X) ->
    lists:all(fun is_command/1,X);
commands_test(_X) -> false.

-spec is_command(proper_gen:imm_instance()) -> boolean().			
is_command({set,{var,V},{call,M,F,A}}) 
  when is_integer(V) andalso is_atom(M) andalso is_atom(F) andalso is_list(A) ->
    true;
is_command({init,_S}) ->
    true;
is_command(_Other) ->
    false.
