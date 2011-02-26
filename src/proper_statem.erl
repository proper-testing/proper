-module(proper_statem).

-export([commands/1, commands/2, parallel_commands/1, parallel_commands/2,
	 more_commands/2]).
-export([gen_commands/2, gen_commands/3, run_commands/2, run_commands/3,
	 gen_parallel_commands/2, gen_parallel_commands/3, run_parallel_commands/2,
	 run_parallel_commands/3]).
-export([state_after/2, command_names/1, zip/2, 
	 commands_test/1, parallel_commands_test/1]).

-export([all_selections/2, index/2, all_insertions/3, insert_all/2]).
-export([validate/4]).

-export_type([symbolic_state/0]).

-include("proper_internal.hrl").


%% -----------------------------------------------------------------------------
%% Type declarations
%% -----------------------------------------------------------------------------

-type symbolic_state() :: term().
-type dynamic_state() :: term().

-type command() :: {'init',symbolic_state()}
		   |{'set',proper_symb:symb_var(),{'call',mod_name(),fun_name(),[term()]}}.
-type command_list() :: [command()].
-type parallel_test_case() :: {command_list(),[command_list()]}.
-type command_history() :: [{command(),term()}].
-type history() :: [{dynamic_state(),term()}].

%%TODO: import these from proper.erl 
-type exc_kind() :: 'throw' | 'error' | 'exit'.
-type exc_reason() :: term().
-type stacktrace() :: [{atom(),atom(),arity() | [term()]}].
-type statem_result() :: 'ok'
			 |'initialization_error'
			 |{'precondition',boolean()}
			 |{'postcondition',boolean()}
			 |{'exception',exc_kind(),exc_reason(),stacktrace()}
			 |'no_possible_interleaving'.

%%-type p_size() :: 2..
			 


%% -----------------------------------------------------------------------------
%% Sequential command generation
%% -----------------------------------------------------------------------------


-define(COMMANDS(PropList), proper_types:new_type(PropList,commands)).

-spec commands(mod_name()) -> proper_types:type().		       
commands(Module) ->
    ?COMMANDS([
	{generator, fun(Size) -> gen_commands(Module,Size) end},
	{is_instance, fun(X) -> commands_test(X) end},    
	{get_indices, fun proper_types:list_get_indices/1},
	{get_length, fun erlang:length/1},
	{split, fun lists:split/2},
	{join, fun lists:append/2},
	{remove, fun proper_arith:list_remove/2},
	{shrinkers, [fun(Cmds,T,S) -> 
			     split_shrinker(Module,Cmds,T,S) end,
		     fun(Cmds,T,S) -> 
			     remove_shrinker(Module,Cmds,T,S) end]}
	      ]).

-spec commands(mod_name(),symbolic_state()) -> proper_types:type().
commands(Module,StartState) ->
    ?COMMANDS([
	{generator, fun(Size) -> gen_commands(Module,StartState,Size) end},
	{is_instance, fun(X) -> commands_test(X) end},
	{get_indices, fun proper_types:list_get_indices/1},
	{get_length, fun erlang:length/1},
	{split, fun lists:split/2},
	{join, fun lists:append/2},
	{remove, fun proper_arith:list_remove/2},
	{shrinkers,[fun(Cmds,T,S) -> 
			    split_shrinker(Module,Cmds,T,S) end,
		    fun(Cmds,T,S) -> 
			    remove_shrinker(Module,Cmds,T,S) end]}
	      ]).

-spec more_commands(integer(),proper_types:type()) ->  proper_types:type().
more_commands(N,Gen) ->			    
    ?SIZED(Size, proper_types:resize(Size * N, Gen)).

-spec gen_commands(mod_name(),symbolic_state(),size()) -> command_list().
gen_commands(Mod,StartState,Size) ->
    Len = proper_arith:rand_int(0,Size),
    try gen_commands(Mod, StartState,[],Len,Len,get('$constraint_tries')) of
	{ok, CmdList} ->
	    [{init,StartState}|CmdList];
	{false_prec, State} ->
	    throw({'$cant_generate_commands',State})
    catch
	_Exc:Reason ->
	    throw({'$gen_commands',{_Exc,Reason,erlang:get_stacktrace()}})
    end.

-spec gen_commands(mod_name(),size()) -> command_list().
gen_commands(Mod,Size) ->
    Len = proper_arith:rand_int(0,Size),
    try gen_commands(Mod,Mod:initial_state(),[],Len,Len,get('$constraint_tries')) of 
	{ok,CmdList} ->
	    CmdList;
	{false_prec, State} ->
	    throw({'$cant_generate_commands',State})
    catch
	Exc:Reason ->
	    throw({'$gen_commands',{Exc,Reason,erlang:get_stacktrace()}})
    end.

-spec gen_commands(mod_name(),symbolic_state(),command_list(),size(),non_neg_integer(),
		   non_neg_integer()) -> 
			  {'ok', command_list()}
			  |{'false_prec', symbolic_state()}.
gen_commands(_Module,_,Commands,_,0,_) ->
    {ok, lists:reverse(Commands)};

gen_commands(_Module,State,_,_,_,0) ->
    {false_prec, State};

gen_commands(Module,State,Commands,Len,Count,Tries) ->  
    Cmd = Module:command(State),
    {ok,Instance} = proper_gen:clean_instance(proper_gen:safe_generate(Cmd)), 
    if Instance =/= stop ->
	    case Module:precondition(State,Instance) of
		true ->
		    Var = {var,Len-Count+1},
		    NextState = Module:next_state(State,Var,Instance),
		    Command= {set,Var,Instance},
		    gen_commands(Module,NextState,[Command|Commands],
				 Len, Count-1, get('$constraint_tries'));
		_ ->
		    gen_commands(Module,State,Commands,Len,Count,Tries-1)
	    end;
       true ->
	    {ok, lists:reverse(Commands)}
    end.    
	    

%% -----------------------------------------------------------------------------
%% Parallel command generation
%% -----------------------------------------------------------------------------


-spec parallel_commands(mod_name()) -> proper_types:type().		       
parallel_commands(Module) ->
    ?COMMANDS([
	{generator, fun(Size) -> gen_parallel_commands(Module,Size+2) end},
	{is_instance, fun(X) -> parallel_commands_test(X) end},    
	{get_indices, fun proper_types:list_get_indices/1},
	{get_length, fun erlang:length/1},
	{split, fun lists:split/2},
	{join, fun lists:append/2},
	{remove, fun proper_arith:list_remove/2},
	{shrinkers, 
	 lists:map(fun(I) ->
		      fun(Parallel_Cmds,T,S) -> 
			      split_parallel_shrinker(I,Module,Parallel_Cmds,T,S) 
		      end
		   end, lists:seq(1,2)) ++
	 lists:map(fun(I) ->
		      fun(Parallel_Cmds,T,S) -> 
			      remove_parallel_shrinker(I,Module,Parallel_Cmds,T,S) 
		      end
		   end, lists:seq(1,2)) ++
	 [fun(Parallel_Cmds,T,S) -> 
		  split_seq_shrinker(Module,Parallel_Cmds,T,S) end,
	  fun(Parallel_Cmds,T,S) -> 
		  remove_seq_shrinker(Module,Parallel_Cmds,T,S) end,
	  fun move_shrinker/3]}
	      ]).

-spec parallel_commands(mod_name(),symbolic_state()) -> proper_types:type(). 
parallel_commands(Module, StartState) ->
    ?COMMANDS([
	{generator,fun(Size) -> 
			   gen_parallel_commands(Module,StartState,Size+2) 
		   end},
	{is_instance, fun(X) -> parallel_commands_test(X) end},    
	{get_indices, fun proper_types:list_get_indices/1},
	{get_length, fun erlang:length/1},
	{split, fun lists:split/2},
	{join, fun lists:append/2},
	{remove, fun proper_arith:list_remove/2},
	{shrinkers, 
	 lists:map(fun(I) ->
		      fun(Parallel_Cmds,T,S) -> 
			      split_parallel_shrinker(I,Module,Parallel_Cmds,T,S) 
		      end
		   end, lists:seq(1,2)) ++
	 lists:map(fun(I) ->
		      fun(Parallel_Cmds,T,S) -> 
			      remove_parallel_shrinker(I,Module,Parallel_Cmds,T,S) 
		      end
		   end, lists:seq(1,2)) ++
	 [fun(Parallel_Cmds,T,S) -> 
		  split_seq_shrinker(Module,Parallel_Cmds,T,S) end,
	  fun(Parallel_Cmds,T,S) -> 
		  remove_seq_shrinker(Module,Parallel_Cmds,T,S) end,
	  fun move_shrinker/3]}
	      ]).

-spec gen_parallel_commands(mod_name(),symbolic_state(),size()) -> parallel_test_case().
gen_parallel_commands(Mod,StartState,Size) ->
    try gen_parallel(Mod, StartState, Size) of
	{Sequential,Parallel} -> {[{init,StartState}|Sequential],Parallel}
    catch
	Exc:Reason ->
	    throw({'$gen_commands',{Exc,Reason,erlang:get_stacktrace()}})
    end.

-spec gen_parallel_commands(mod_name(), size()) -> parallel_test_case().
gen_parallel_commands(Mod,Size) ->
    try gen_parallel(Mod, Mod:initial_state(), Size) of
	{_Sequential,_Parallel}=Res -> Res
    catch
	Exc:Reason ->
	    throw({'$gen_commands',{Exc,Reason,erlang:get_stacktrace()}})
    end.
	    
-spec gen_parallel(mod_name(),symbolic_state(),size()) -> parallel_test_case().
gen_parallel(Mod,StartState,Size) ->
    Len1 = proper_arith:rand_int(2,Size),
    {ok, CmdList} = gen_commands(Mod, StartState,[],Len1,Len1,get('$constraint_tries')),
    Len = length(CmdList),
    {Seq,P} = if Len == 2 ->
		      {[],CmdList};
		 Len =< 32 ->
		      lists:split(Len div 2,CmdList);
		 Len > 32 ->
		      lists:split(Len-16,CmdList)
	      end,
    State = state_after(Mod,Seq),
    LenSeq = length(Seq),
    LenPar = Len - LenSeq,
    Env = if LenSeq == 0 -> 
		  [];
	     true ->
		  lists:map(fun(N) -> {var,N} end, lists:seq(0,LenSeq-1))
	  end,
    {Seq,fix_gen(LenPar div 2,P,Mod,State,Env)}.

%%TODO: more efficient parallelization   
-spec fix_gen(pos_integer(),command_list(),mod_name(),symbolic_state(),
	      proper_symb:var_values()) -> [command_list()].		     
fix_gen(N,Initial,Mod,State,Env) when N>=0 ->
    Selections = all_selections(N,Initial),
    case (safe_parallelize(Selections,Initial,Mod,State,Env)) of
	{ok,Result} ->
	    tuple_to_list(Result);
	error ->
	    fix_gen(N-1,Initial,Mod,State,Env)
    end.

-spec safe_parallelize([command_list()],command_list(),mod_name(),symbolic_state(),
		       proper_symb:symb_var()) -> 
			      {'ok', {command_list(),command_list()}}|'error'.  
safe_parallelize([],_,_,_,_) ->
    error;
safe_parallelize([C1|Selections],Initial,Mod,State,Env) ->
    C2 = Initial -- C1,
    case parallelize(Mod,State,{C1,C2},Env) of
	{ok,_R}=Result -> Result;   
	error -> safe_parallelize(Selections,Initial,Mod,State,Env)
    end.	   

-spec parallelize(mod_name(),symbolic_state(),{command_list(),command_list()},
		  proper_symb:symb_var()) -> {'ok',{command_list(),command_list()}}
						 |'error'.
parallelize(Mod,S,{C1,C2},Env) ->
    Val = fun (C) -> validate(Mod,S,C,Env) end,
    case lists:all(Val, insert_all(C1,C2)) of
	true ->
	    {ok,{C1,C2}};
	false -> 
	    error
    end.			 
   
 		       
%% -----------------------------------------------------------------------------
%% Sequential command execution
%% -----------------------------------------------------------------------------


-spec run_commands(mod_name(),command_list()) -> 
			  {history(),dynamic_state(),statem_result()}.	  
run_commands(Module,Cmds) -> 
    run_commands(Module,Cmds,[]).

-spec run_commands(mod_name(),command_list(),proper_symb:var_values()) ->
			  {history(),dynamic_state(),statem_result()}.
run_commands(Module,Commands,Env) ->
    case safe_eval_init(Env,Commands,Module) of
	{ok,DynState} -> 
	    do_run_command(Commands,Env,Module,[],DynState); 
	{error,Reason} ->
	    {[],[],Reason}
    end.					       

-spec safe_eval_init(proper_symb:var_values(),command_list(),mod_name()) -> 
			    {'ok',dynamic_state()} | {'error',statem_result()}.
safe_eval_init(Env,[{init,SymbState}|_],_Module) ->
    try proper_symb:eval(Env,SymbState) of
	DynState -> 
	    {ok,DynState}
    catch
	_Exception:_Reason ->
	    {error, initialization_error}
    end; 
safe_eval_init(Env,_Cmds,Module) ->
    try proper_symb:eval(Env,Module:initial_state()) of
	DynState -> 
	    {ok,DynState}
    catch
	_Exception:_Reason ->
	    {error, initialization_error}
    end.
 
-spec do_run_command(command_list(),proper_symb:var_values(),mod_name(),
		     history(),dynamic_state()) -> 
			    {history(),dynamic_state(),statem_result()}.	   
do_run_command(Commands, Env, Module, History, State) ->
    case Commands of
	[] -> 
	    {lists:reverse(History), State, ok};
	[{init,_S}|Rest] ->
	    do_run_command(Rest,Env,Module,History,State);
 	[{set, {var,V}, {call,M,F,A}}|Rest] ->
	    M2=proper_symb:eval(Env,M), 
	    F2=proper_symb:eval(Env,F), 
	    A2=proper_symb:eval(Env,A),
	    try apply(M2,F2,A2) of
		Res ->
		    Call = {call, M2,F2,A2},
		    try Module:precondition(State,Call) of
			true ->
			    try Module:postcondition(State,Call,Res) of
				true ->
				    Env2 = [{V,Res}|Env],
				    State2 = Module:next_state(State,Res,Call),
				    History2 = [{State,Res}|History],
				    do_run_command(Rest, Env2, Module, History2, State2);
				_Other ->
				    State2 = Module:next_state(State,Res,Call),
				    History2 = [{State,Res}|History],
				    {lists:reverse(History2), State2,
				      {postcondition,_Other}}
			    catch
				Kind:Reason ->
				    State2 = Module:next_state(State,Res,Call),
				    History2 = [{State,Res}|History],
				    {lists:reverse(History2), State2, 
				     {postcondition,
				      {exception,Kind,Reason,erlang:get_stacktrace()}}}
			    end;
			_Other ->
			    {lists:reverse(History), State, {precondition, _Other}}
		    catch
			Kind:Reason ->
			    {lists:reverse(History), State, 
			     {precondition,
			      {exception,Kind,Reason,erlang:get_stacktrace()}}}
		    end
	    catch
		ExcKind:ExcReason ->
		    {lists:reverse(History), State, 
		     {exception,ExcKind,ExcReason,erlang:get_stacktrace()}}
	    end
    end.

      
%% -----------------------------------------------------------------------------
%% Parallel command execution
%% -----------------------------------------------------------------------------


-spec run_parallel_commands(mod_name(),parallel_test_case()) ->
				   {history(),[command_history()],statem_result()}.
run_parallel_commands(Module,{_Sequential,_Parallel}=Cmds) ->
    run_parallel_commands(Module,Cmds,[]).

-spec run_parallel_commands(mod_name(),parallel_test_case(),proper_symb:var_values()) ->
				   {history(),[command_history()],statem_result()}.
run_parallel_commands(Module,{Sequential,Parallel},Env) ->
     case safe_eval_init(Env,Sequential,Module) of
	{ok,DynState} -> 
	     case safe_run_sequential(Sequential,Env,Module,[],DynState) of 
		 {ok,{{Seq_history,State,ok},Env1}} -> 
		     Self = self(),
		     Parallel_test = 
			 fun(T) -> 
				 proper:spawn_link_migrate(
				   fun() -> 
					   Self ! {self(),{result,
							   safe_execute(T,Env1,Module,[])}}
				   end)
			 end,
		     Children = lists:map(Parallel_test, Parallel),
		     Parallel_history = receive_loop(Children,[],2),
		     if is_list(Parallel_history)==true ->
			     {P1,P2} = list_to_tuple(Parallel_history),
			     case check(Module,State,Env1,P1,P2,[]) of
				 true ->  
				     {Seq_history,Parallel_history,ok};
				 false ->
				     {Seq_history,Parallel_history,
				      no_possible_interleaving}
			     end;
		        %% if Parallel_history is not a list, then an exception was raised
			true ->
			     io:format("Error during parallel execution~n"),
			     exit(error)
			     %%{Seq_history,[],Parallel_history}
		     end;
			    
		 {error,_Reason} ->  
		     io:format("Error during sequential execution~n"),
		     exit(error)
		     %%{[],[],Reason}
	     end;	    
	 {error,Reason} ->
	     {[],[],Reason}
     end.

-spec receive_loop([{pid(),command_list()}],[command_history()],non_neg_integer()) -> 
			  [command_history()] | statem_result().
receive_loop(_, ResultsReceived, 0) ->
    ResultsReceived;
receive_loop(Pid_list,ResultsReceived,N) when N>0 ->
    receive
	{Pid,{result,{ok,{H,ok}}}} ->
	    case lists:member(Pid,Pid_list) of
		false ->  
		    io:format("UFO ~w sent message~n", [Pid]),
		    receive_loop(Pid_list,ResultsReceived,N);
		true -> 
		    receive_loop(lists:delete(Pid,Pid_list),
				 ResultsReceived ++ [H], N-1)
	    end;
	{Pid,{result,{error,Other}}} -> 
	    case lists:member(Pid,Pid_list) of
		false ->  
		    io:format("UFO ~w sent message~n", [Pid]),
		    receive_loop(Pid_list,ResultsReceived,N);
		true ->
		    Other
	    end	    
    end.		             

%%TODO: more efficient checking
-spec check(mod_name(),dynamic_state(),proper_symb:var_values(),
	    command_history(),command_history(),command_history()) -> boolean().
check(_Mod,_State,_Env,[],[],_Accum) -> true;

check(Mod,State,Env,[],[Head|Rest],Accum) ->
    {{set,{var,N},{call,M,F,A}},Res} = Head, 
    M2 = proper_symb:eval(Env,M), 
    F2 = proper_symb:eval(Env,F), 
    A2 = proper_symb:eval(Env,A),
    Call = {call,M2,F2,A2},
    case Mod:postcondition(State,Call,Res) of 
	true ->
	    Env2 = [{N,Res}|Env],
	    NextState = Mod:next_state(State,Res,Call),
	    check(Mod,NextState,Env2,[],Rest,[Head|Accum]);
	false -> 
	    false
    end;

check(Mod,State,Env,[Head|Rest],[],Accum) ->
    {{set,{var,N},{call,M,F,A}},Res} = Head, 
    M2 = proper_symb:eval(Env,M), 
    F2 = proper_symb:eval(Env,F), 
    A2 = proper_symb:eval(Env,A),
    Call = {call,M2,F2,A2},
    case Mod:postcondition(State,Call,Res) of 
	true ->
	    Env2 = [{N,Res}|Env],
	    NextState = Mod:next_state(State,Res,Call),
	    check(Mod,NextState,Env2,Rest,[],[Head|Accum]);
	false ->
	    false
    end;

check(Mod,State,Env,[H1|Rest1],[H2|Rest2],Accum) ->
    {{set,{var,N1},{call,M1,F1,A1}},Res1} = H1,
    {{set,{var,N2},{call,M2,F2,A2}},Res2} = H2,
    M1_ = proper_symb:eval(Env,M1), 
    F1_ = proper_symb:eval(Env,F1), 
    A1_ = proper_symb:eval(Env,A1),
    Call1 = {call,M1_,F1_,A1_},   
    M2_ = proper_symb:eval(Env,M2), 
    F2_ = proper_symb:eval(Env,F2), 
    A2_ = proper_symb:eval(Env,A2),
    Call2 = {call,M2_,F2_,A2_}, 
    case {Mod:postcondition(State,Call1,Res1),Mod:postcondition(State,Call2,Res2)} of 
	{true,false} -> 
	    Env2 = [{N1,Res1}|Env],
	    NextState = Mod:next_state(State,Res1,Call1),
	    check(Mod,NextState,Env2,Rest1,[H2|Rest2],[H1|Accum]);
	{false,true} -> 
	    Env2 = [{N2,Res2}|Env],
	    NextState = Mod:next_state(State,Res2,Call2),
	    check(Mod,NextState,Env2,[H1|Rest1],Rest2,[H1|Accum]);
	{true,true} -> 
	    NextState1 = Mod:next_state(State,Res1,Call1),
	    NextState2 = Mod:next_state(State,Res2,Call2),
	    Env1 = [{N1,Res1}|Env],
	    Env2 = [{N2,Res2}|Env],
	    case check(Mod,NextState1,Env1,Rest1,[H2|Rest2],[H1|Accum]) of
		true -> true;
		false ->
		    check(Mod,NextState2,Env2,[H1|Rest1],Rest2,[H2|Accum])
	    end;
	{false,false} -> 
	    false
    end.
		
-spec safe_run_sequential(command_list(),proper_symb:var_values(),mod_name(),
			  history(),dynamic_state()) ->
				 {'ok',{{history(),dynamic_state(),statem_result()},
					proper_symb:var_values()}}
				 |{'error', term()}.
safe_run_sequential(Commands, Env, Module, History, State) ->
    try run_sequential(Commands, Env, Module, History, State) of
	Result ->
	    {ok, Result}
    catch
	ExcKind:ExcReason ->
	   {error,{exception,ExcKind,ExcReason,erlang:get_stacktrace()}}
    end. 

-spec run_sequential(command_list(),proper_symb:var_values(),mod_name(),
		     history(),dynamic_state()) -> 
			    {{history(),dynamic_state(),statem_result()},
			     proper_symb:var_values()}.	   
run_sequential(Commands, Env, Module, History, State) ->
    case Commands of
	[] -> 
	    {{lists:reverse(History), State, ok}, Env};
	[{init,_S}|Rest] ->
	    run_sequential(Rest, Env, Module, History, State);
   	[{set, {var,V}, {call,M,F,A}}|Rest] ->
	    M2=proper_symb:eval(Env,M), 
	    F2=proper_symb:eval(Env,F), 
	    A2=proper_symb:eval(Env,A),
	    Res = apply(M2,F2,A2),
	    Call = {call, M2,F2,A2},		    
	    true = Module:precondition(State,Call),
	    true = Module:postcondition(State,Call,Res),
	    Env2 = [{V,Res}|Env],
	    State2 = Module:next_state(State,Res,Call),
	    History2 = [{State,Res}|History],
	    run_sequential(Rest, Env2, Module, History2, State2)
    end.

-spec safe_execute(command_list(),proper_symb:var_values(),mod_name(),
	      command_history()) -> 
		     {'ok',{command_history(),statem_result()}}|{'error',term()}.
safe_execute(Commands,Env,Module,History) ->
    try execute(Commands, Env, Module, History) of
	Result ->
	    {ok,Result}
    catch
	ExcKind:ExcReason ->
	   {error,{exception,ExcKind,ExcReason,erlang:get_stacktrace()}}
    end. 

-spec execute(command_list(),proper_symb:var_values(),mod_name(),
	      command_history()) -> 
		     {command_history(),statem_result()}. 
execute(Commands, Env, Module, History) ->
    case Commands of
	[] -> 
	    {lists:reverse(History), ok};
	[{set, {var,V}, {call,M,F,A}}=Cmd|Rest] ->
	    M2=proper_symb:eval(Env,M), 
	    F2=proper_symb:eval(Env,F), 
	    A2=proper_symb:eval(Env,A),
	    Res = apply(M2,F2,A2),
	    Env2 = [{V,Res}|Env],
	    History2 = [{Cmd,Res}|History],
	    execute(Rest, Env2, Module, History2)
    end.


%% -----------------------------------------------------------------------------
%% Command shrinkers
%% -----------------------------------------------------------------------------


-spec split_shrinker(mod_name(),command_list(), 
		     proper_types:type(),proper_shrink:state()) ->
			    {[command_list()],proper_shrink:state()}. 
split_shrinker(Module,[{init,StartState}|Commands], Type,State) ->
    {Slices,NewState} =  proper_shrink:split_shrinker(Commands,Type,State),
    IsValid= fun (CommandSeq) -> validate(Module,StartState,CommandSeq,[]) end,
    {lists:map(fun(L) -> [{init,StartState}|L] end,lists:filter(IsValid,Slices)),
     NewState};

split_shrinker(Module, Commands, Type,State) ->
    {Slices,NewState} =  proper_shrink:split_shrinker(Commands,Type,State),
    StartState = Module:initial_state(),
    IsValid = fun (CommandSeq) -> validate(Module,StartState,CommandSeq,[]) end,
    {lists:filter(IsValid,Slices),NewState}.

-spec remove_shrinker(mod_name(),command_list(),
		      proper_types:type(),proper_shrink:state()) ->
			     {[command_list()],proper_shrink:state()}. 
remove_shrinker(Module,[{init,StartState}|Commands]=Cmds,Type,State) ->
   {CommandList,NewState} =  proper_shrink:remove_shrinker(Commands,Type,State),
   case CommandList of
       [] -> {[],NewState};
       [NewCommands] ->
	   case validate(Module,StartState,NewCommands,[]) of
	       true -> 
		   {[[{init,StartState}|NewCommands]],NewState};
	        _ ->
		   remove_shrinker(Module,Cmds,Type,NewState)
	   end
   end;

remove_shrinker(Module,Commands,Type,State) ->
   {CommandList,NewState} =  proper_shrink:remove_shrinker(Commands,Type,State),
   case CommandList of
       [] -> {[],NewState};
       [NewCommands] ->
	   StartState = Module:initial_state(),
	   case validate(Module,StartState,NewCommands,[]) of
	       true -> {[NewCommands],NewState};
	        _ ->
		   remove_shrinker(Module,Commands,Type,NewState)
	   end
   end.

-spec split_parallel_shrinker(pos_integer(), mod_name(), parallel_test_case(), 
			      proper_types:type(), proper_shrink:state()) ->
				     {[parallel_test_case()],proper_shrink:state()}. 
split_parallel_shrinker(I, Module, {Sequential,Parallel}, Type, State) ->
    Len = length(Sequential),
    SeqEnv = lists:zip(lists:duplicate(Len, var), lists:seq(0, Len-1)),
    SymbState = state_after(Module, Sequential),
    {Slices,NewState} = proper_shrink:split_shrinker(lists:nth(I,Parallel),Type,State), 
    IsValid = fun(CommandSeq) -> validate(Module,SymbState,CommandSeq,SeqEnv) end,
    {lists:map(fun(Slice) -> {Sequential,update_list(I,Slice,Parallel)} end, 
	       lists:filter(IsValid,Slices)),
     NewState}.

-spec remove_parallel_shrinker(pos_integer(), mod_name(), parallel_test_case(),
			       proper_types:type(), proper_shrink:state()) ->
				      {[parallel_test_case()],proper_shrink:state()}. 
remove_parallel_shrinker(I, Module, {Sequential,Parallel}, Type, State) ->
    Len = length(Sequential),
    SeqEnv = lists:zip(lists:duplicate(Len, var), lists:seq(0, Len-1)),
    SymbState = state_after(Module, Sequential),
    {CommandList,NewState} =  proper_shrink:remove_shrinker(
				lists:nth(I,Parallel),Type,State),
    case CommandList of
       [] -> {[{Sequential, update_list(I,[],Parallel)}],NewState};
       [NewCommands] ->
	   case validate(Module,SymbState,NewCommands,SeqEnv) of
	       true -> {[{Sequential, update_list(I,NewCommands,Parallel)}],NewState};
	        _ ->
		   remove_parallel_shrinker(I,Module,{Sequential,Parallel},Type,NewState)
	   end
    end.

-spec split_seq_shrinker(mod_name(), parallel_test_case(), proper_types:type(),
			 proper_shrink:state()) ->
				{[parallel_test_case()],proper_shrink:state()}.
split_seq_shrinker(Module, {Sequential,[P1,P2]=Parallel}, Type, State) ->
    {Slices,NewState} = split_shrinker(Module, Sequential, Type, State),
    SymbState = case Sequential of
		    [{init,S}|_] -> S;
		    _CmdList -> Module:initial_state()
		end,
    IsValid = fun(CommandSeq) -> 
		      validate(Module,SymbState,CommandSeq ++ P1,[])
			  andalso validate(Module,SymbState,CommandSeq ++ P2,[])
	      end,
    {lists:map(fun(Slice) -> {Slice,Parallel} end, lists:filter(IsValid,Slices)),
     NewState}.
 
-spec remove_seq_shrinker(mod_name(), parallel_test_case(), proper_types:type(),
			  proper_shrink:state()) ->
				 {[parallel_test_case()],proper_shrink:state()}.
remove_seq_shrinker(_Module, TestCase, _Type, done) ->
    {[TestCase], done};
remove_seq_shrinker(Module, {Sequential,[P1,P2]=Parallel}, Type, State) ->
    {CommandList,NewState} = remove_shrinker(Module,Sequential,Type,State),
    NewCommands = case CommandList of
		      [] -> [];
		      [Cmds] -> Cmds
		  end,
    SymbState = case Sequential of
		    [{init,S}|_] -> S;
		    _CmdList -> Module:initial_state()
		end,
    case {validate(Module,SymbState,NewCommands ++ P1,[]),
	  validate(Module,SymbState,NewCommands ++ P2,[])} of
	{true,true} -> 
	    {[{NewCommands,Parallel}],NewState};
	_ ->
	    remove_seq_shrinker(Module,{Sequential,Parallel},Type,NewState)
    end. 

-spec move_shrinker(parallel_test_case(), proper_types:type(), proper_shrink:state()) ->
			   {[parallel_test_case()],proper_shrink:state()}.
move_shrinker({Sequential, [[H1|Rest1], [H2|Rest2]]}, _Type, _State) ->
    {[{Sequential ++ [H1], [Rest1, [H2|Rest2]]},
      {Sequential ++ [H2], [[H1|Rest1], Rest2]}], shrunk};
move_shrinker({_, [[],_]}=TestCase, _Type, _State) ->
    {[TestCase], done};
move_shrinker({_, [_,[]]}=TestCase, _Type, _State) ->
    {[TestCase], done}.

%% -----------------------------------------------------------------------------
%% Utility functions
%% -----------------------------------------------------------------------------


-spec validate(mod_name(),symbolic_state(),command_list(),
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

-spec state_after(mod_name(),command_list()) -> symbolic_state().		   
state_after(Module,Commands) ->
    NextState = fun(S,V,C) -> Module:next_state(S,V,C) end,
    lists:foldl(fun({init,S}, _) ->
			S;
		   ({set,Var,Call},S) ->
			NextState(S,Var,Call)
		end,
		Module:initial_state(),
		Commands).

-spec command_names(command_list()) -> 
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

-spec parallel_commands_test(proper_gen:imm_instance()) -> boolean().
parallel_commands_test({S,P}) ->
    lists:all(fun is_command/1,S) andalso 
	lists:foldl(fun(X,Y) -> X andalso Y end,
		    true,
		    lists:map(fun(Elem) -> 
				      lists:all(fun is_command/1,Elem)end,P));
parallel_commands_test(_) -> false.

-spec is_command(proper_gen:imm_instance()) -> boolean().			
is_command({set,{var,V},{call,M,F,A}}) 
  when is_integer(V) andalso is_atom(M) andalso is_atom(F) andalso is_list(A) ->
    true;
is_command({init,_S}) ->
    true;
is_command(_Other) ->
    false.

%% Returns all possible insertions of the elements of the first list, preserving their
%% order, inside the second list, i.e. all possible command interleavings

-spec insert_all([term()],[term()]) -> [[term()]].
insert_all([], List) ->
    [List];
insert_all([X], List) ->
    all_insertions(X,length(List)+1,List);

insert_all([X|[Y|Rest]], List) ->
    [L2 || L1 <- insert_all([Y|Rest], List), 
	   L2 <- all_insertions(X,index(Y,L1),L1)].

-spec all_insertions(term(),pos_integer(),[term()]) -> [[term()]].
all_insertions(X,Limit,List) ->
    all_insertions_tr(X, Limit, 0, [], List, []).

-spec all_insertions_tr(term(),pos_integer(),non_neg_integer(),[term()],[term()],
			[[term()]]) -> [[term()]].
all_insertions_tr(X, Limit, LengthFront, Front, [], Acc) ->
    case LengthFront < Limit of
	true ->
	    [Front ++ [X] | Acc];
	false ->
	    Acc
    end;
all_insertions_tr(X, Limit, LengthFront, Front, Back = [BackHead|BackTail], Acc) ->
    case LengthFront < Limit of 
	true ->
	    all_insertions_tr(X, Limit, LengthFront+1, Front ++ [BackHead], BackTail,
			      [Front ++ [X] ++ Back | Acc]);
	false -> Acc     
    end.

-spec index(term(),[term()]) -> pos_integer().
index(X,List) ->
    length(lists:takewhile(fun(Y) -> X=/=Y end,List)) + 1.

%% Returns all possible selections of 'N' elements from list 'List'.
-spec all_selections(non_neg_integer(),[term()]) -> [[term()]].		    
all_selections(0, _List) ->
    [[]];
all_selections(N, List) when N >= 1 ->
    Len = length(List),
    case N > Len of
	true ->
	    erlang:error(badarg);
	false ->
	    all_selections(N, List, Len)
    end.

-spec all_selections(pos_integer(),[term()],pos_integer()) -> [[term()]].	     
all_selections(1, List, _Len) ->
    [[X] || X <- List];
all_selections(_Len, List, _Len) ->
    [List];
all_selections(Take, [Head|Tail], Len) ->
    [[Head|Rest] || Rest <- all_selections(Take - 1, Tail, Len - 1)]
    ++ all_selections(Take, Tail, Len - 1).     

-spec update_list(pos_integer(),term(),[term()]) -> [term()].			 
update_list(I,X,List) ->	     
    array:to_list(array:set(I-1, X, array:from_list(List))).
    
