-module(proper_statem).

-define(TRIES,100).

-export([gen_commands/2, gen_commands/3, run_commands/2, state_after/2,
	 remove_shrinker/5, split_shrinker/5, command_names/1, zip/2]).

-export_type([symbolic_state/0]).

-include("proper_internal.hrl").

%%TODO: type refinement
-type var_count() :: non_neg_integer().
-type tries() :: non_neg_integer().
%%TODO: abstract datatype for states
-type symbolic_state() :: any().
-type dynamic_state() :: any().
-type fsm_state() :: symbolic_state() | dynamic_state().
-type result() :: any().

-spec gen_commands(mod_name(),symbolic_state(),size()) -> [proper_gen:imm_instance()].
gen_commands(Module,StartState,Size) ->
    [{init,StartState}|gen_commands(Module, StartState,[], Size, Size, ?TRIES)].

-spec gen_commands(mod_name(),size()) -> [proper_gen:imm_instance()].
gen_commands(Module,Size) ->
    gen_commands(Module, Module:initial_state(),[], Size, Size, ?TRIES).

-spec gen_commands(mod_name(),symbolic_state(),[proper_gen:imm_instance()],size(),
		   var_count(),tries()) -> [proper_gen:imm_instance()].
gen_commands(_Module,_,Commands,_,0,_) ->
        lists:reverse(Commands);

gen_commands(Module,State,_,_,_,0) ->
    erlang:error({cannot_generate_statem,Module,State});

gen_commands(Module,State,Commands,Size,Count,Tries) ->
    
    CmdDom = Module:command(State),  
    {ok,ImmInstance} = proper_gen:safe_generate(CmdDom),
    CleanInstance = proper_gen:clean_instance(ImmInstance),
    case Module:precondition(State, CleanInstance) of
	true ->
	    Var = {var, Size-Count},
	    NextState = Module:next_state(State, Var, CleanInstance),
	    Command = {set, Var, CleanInstance},
	    gen_commands(Module,
			 NextState, 
			 [Command|Commands],
			 Size, 
			 Count-1, ?TRIES);
	_ ->
	    %% try again, up to Tries times...
	    gen_commands(Module,State,Commands,Size,Count,Tries-1)
    end.

-spec run_commands(mod_name(),[proper_gen:imm_instance()]) -> 
			   {[{dynamic_state(),result()}],dynamic_state(),
			    'ok'|{'postcondition',boolean()}}.			  
run_commands(Module,Cmds) ->
    run_commands(Module,Cmds,[]).

-spec run_commands(mod_name(),[proper_gen:imm_instance()],[any()]) ->
			   {[{dynamic_state(),result()}],dynamic_state(),
			    'ok'|{'postcondition',boolean()}}.
run_commands(Module,Commands,Env) ->
    do_run_command(Commands,
		   Env,
		   Module,
		   [],
		   proper_symb:eval(Env,Module:initial_state())).

-spec do_run_command([proper_gen:imm_instance()],[any()],mod_name(),
		     [{dynamic_state(),result()}],dynamic_state()) -> 
			    {[{dynamic_state(),result()}],dynamic_state(),
			     'ok'|{'postcondition',boolean()}}.			    
do_run_command(Commands, Env, Module, History, State) ->
    case Commands of
	[] -> 
	    {lists:reverse(History), State, ok};

	[{init,S}|Rest] ->
	    State2 = proper_symb:eval(Env, S),
	    do_run_command(Rest, Env, Module, History, State2);
	
	[{set, {var,V}, {call,M,F,A}}|Rest] ->
	    M2=proper_symb:eval(Env,M), 
	    F2=proper_symb:eval(Env,F), 
	    A2=proper_symb:eval(Env,A),
	    
	    Res = apply(M2,F2,A2),
	    Call = {call, M2,F2,A2},
   
	    case Module:postcondition(State,Call,Res) of
		true ->
		    Env2 = [{V,Res}|proplists:delete(V,Env)],
		    State2 = Module:next_state(State,Res,Call),
		    History2 = [{State,Res}|History],
		    do_run_command(Rest, Env2, Module, History2, State2);
			   
		Other ->
		    {lists:reverse(History), State, {postcondition, Other}}
	    end
    end.

-spec split_shrinker(mod_name(),symbolic_state(),proper_gen:imm_instance(), 
		     proper_types:type(),proper_shrink:state()) ->
			    {[proper_gen:imm_instance()],proper_shrink:state()}. 
split_shrinker(Module, StartState, [{init,StartState}|Commands], Type,State) ->
    {Slices,NewState} =  proper_shrink:split_shrinker(Commands,Type,State),
    IsValid= fun (CommandSeq) -> validate(Module,StartState,CommandSeq) end,
    {lists:map(fun(L) -> [{init,StartState}|L] end,lists:filter(IsValid,Slices)),
     NewState};

split_shrinker(Module, StartState, Commands, Type,State) ->
    {Slices,NewState} =  proper_shrink:split_shrinker(Commands,Type,State),
    IsValid= fun (CommandSeq) -> validate(Module,StartState,CommandSeq) end,
    {lists:filter(IsValid,Slices),NewState}.

-spec remove_shrinker(mod_name(),symbolic_state(),proper_gen:imm_instance(),
		      proper_types:type(),proper_shrink:state()) ->
			     {[proper_gen:imm_instance()],proper_shrink:state()}. 
remove_shrinker(Module,StartState,[{init,StartState}|Commands]=Cmds,Type,State) ->
   {CommandList,NewState} =  proper_shrink:remove_shrinker(Commands,Type,State),
   case CommandList of
       [] -> {[{init,StartState}],NewState};
       [NewCommands] ->
	   case validate(Module,StartState,NewCommands) of
	       true -> 
		   {[{init,StartState}|NewCommands],NewState};
	        _ ->
		   remove_shrinker(Module,StartState,Cmds,Type,NewState)
	   end
   end;

remove_shrinker(Module,StartState,Commands,Type,State) ->
   {CommandList,NewState} =  proper_shrink:remove_shrinker(Commands,Type,State),
   case CommandList of
       [] -> {[],NewState};
       [NewCommands] ->
	   case validate(Module,StartState,NewCommands) of
	       true -> {[NewCommands],NewState};
	        _ ->
		   remove_shrinker(Module,StartState,Commands,Type,NewState)
	   end
   end.
 
-spec validate(mod_name(),symbolic_state(),[proper_gen:imm_instance()]) -> boolean().
validate(_Mod,_State,[]) -> true;

validate(Module,_State,[{init,S}|Commands]) ->
    validate(Module,S,Commands);

validate(Module,State,[{set,Var,Call}|Commands]) ->
    case Module:precondition(State,Call) of
	true ->
	    case Module:next_state(State, Var, Call) of
		NextState -> 
		    validate(Module,NextState, Commands)
	    end;
	_ -> false
    end.

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

-spec command_names([proper_gen:imm_instance()]) -> [{mod_name(),fun_name(),integer()}].
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
    
		 
