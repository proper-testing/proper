-module(proper_statem).

-export([commands/1, commands/2, parallel_commands/1, parallel_commands/2,
	 more_commands/2]).
-export([run_commands/2, run_commands/3, run_parallel_commands/2,
	 run_parallel_commands/3]).
-export([state_after/2, command_names/1, zip/2]).

-export([index/2, all_insertions/3, insert_all/2, possible_interleavings/1]).
-export([is_valid/4]).
-export([get_next/5, mk_first_comb/3, fix_gen/7, mk_dict/2]).

-include("proper_internal.hrl").

-define(WORKERS, 3).
-define(LIMIT, 16).

%% -----------------------------------------------------------------------------
%% Type declarations
%% -----------------------------------------------------------------------------

-type symbolic_state() :: term().
-type dynamic_state() :: term().

-type symb_var() :: {'var',proper_symb:var_id()}.
-type symb_call() :: {'call',mod_name(),fun_name(),[term()]}.
-type command() :: {'init',symbolic_state()}
		   | {'set',symb_var(),symb_call()}.

-type command_list() :: [command()].
-type parallel_test_case() :: {command_list(),[command_list()]}.
-type command_history() :: [{command(),term()}].
-type history() :: [{dynamic_state(),term()}].

%% TODO: import these from proper.erl
-type exc_kind() :: 'throw' | 'error' | 'exit'.
-type exc_reason() :: term().
-type stacktrace() :: [{atom(),atom(),arity() | [term()]}].
-type exception() ::  {'exception',exc_kind(),exc_reason(),stacktrace()}.
-type statem_result() :: 'ok'
			 | 'initialization_error'
			 | {'precondition', boolean() | exception()}
			 | {'postcondition', boolean() | exception()}
			 | exception()
			 | 'no_possible_interleaving'.

-type combination() :: [{pos_integer(),[pos_integer()]}].
-type lookup() :: orddict:ordered_dictionary().


%% -----------------------------------------------------------------------------
%% Sequential command generation
%% -----------------------------------------------------------------------------

-define(COMMANDS(PropList), proper_types:new_type(PropList, commands)).

-spec commands(mod_name()) -> proper_types:type().
commands(Module) ->
    commands(Module, Module:initial_state(), false).

-spec commands(mod_name(), symbolic_state()) -> proper_types:type().
commands(Module, InitialState) ->
    commands(Module, InitialState, true).

-spec commands(mod_name(), symbolic_state(), boolean()) -> proper_types:type().
commands(Module, InitialState, InitFlag) ->
    ?COMMANDS(
       [{generator, fun(Size) -> gen_commands(Size, Module, InitialState, InitFlag) end},
	{is_instance, fun commands_test/1},
	{get_indices, fun proper_types:list_get_indices/1},
	{get_length, fun erlang:length/1},
	{split, fun lists:split/2},
	{join, fun lists:append/2},
	{remove, fun proper_arith:list_remove/2},
	{shrinkers, [fun(Cmds, T, S) -> split_shrinker(Module, Cmds, T, S) end,
		     fun(Cmds, T, S) -> remove_shrinker(Module, Cmds, T, S) end]}]).

-spec more_commands(pos_integer(), proper_types:type()) -> proper_types:type().
more_commands(N, Type) ->
    ?SIZED(Size, proper_types:resize(Size * N, Type)).

-spec gen_commands(size(), mod_name(), symbolic_state(), boolean()) -> command_list().
gen_commands(Size, Mod, InitialState, InitFlag) ->
    %% TODO: choose actual length in a better way
    Len = proper_arith:rand_int(0, Size),
    try gen_commands(Mod, InitialState, [], Len, Len) of
	CmdList ->
	    case InitFlag of
		true -> 
		    [{init,InitialState}|CmdList];
		false -> 
		    erlang:put('$initial_state', InitialState),
		    CmdList
	    end
    catch
	Exc:Reason ->
	    throw({'$gen_commands', {Exc,Reason,erlang:get_stacktrace()}})
    end.

-spec gen_commands(mod_name(), symbolic_state(), command_list(), size(),
		   non_neg_integer()) -> command_list().
gen_commands(_, _, Commands, _, 0) ->
    lists:reverse(Commands);
gen_commands(Module, State, Commands, Len, Count) ->
    Call = ?SUCHTHAT(X, Module:command(State), Module:precondition(State, X)),
    {ok,Instance} = proper_gen:clean_instance(proper_gen:safe_generate(Call)),
    case Instance =/= stop of
	true ->
	    Var = {var, Len-Count+1},
	    Command = {set, Var, Instance},
	    NextState = Module:next_state(State, Var, Instance),
	    gen_commands(Module, NextState, [Command|Commands], Len, Count-1);
	false ->
	    lists:reverse(Commands)
    end.


%% -----------------------------------------------------------------------------
%% Parallel command generation
%% -----------------------------------------------------------------------------

-spec parallel_commands(mod_name()) -> proper_types:type().		       
parallel_commands(Module) ->
    parallel_commands(Module, Module:initial_state(), false).

-spec parallel_commands(mod_name(), symbolic_state()) -> proper_types:type(). 
parallel_commands(Module, InitialState) ->
    parallel_commands(Module, InitialState, true).

-spec parallel_commands(mod_name(), symbolic_state(), boolean()) -> proper_types:type().
parallel_commands(Module, InitialState, InitFlag) ->
    ?COMMANDS(
       [{generator, 
	 fun(Size) ->
		 gen_parallel_commands(Size + ?WORKERS, Module, InitialState, InitFlag)
	 end},
	{is_instance, fun parallel_commands_test/1},
	{get_indices, fun proper_types:list_get_indices/1},
	{get_length, fun erlang:length/1},
	{split, fun lists:split/2},
	{join, fun lists:append/2},
	{remove, fun proper_arith:list_remove/2},
	{shrinkers, 
	 lists:map(fun(I) ->
			   fun(Parallel_Cmds, T, S) ->
				   split_parallel_shrinker(I, Module, Parallel_Cmds, T, S)
			   end
		   end, lists:seq(1, ?WORKERS)) ++
	 lists:map(fun(I) ->
			   fun(Parallel_Cmds, T, S) ->
				   remove_parallel_shrinker(I, Module, Parallel_Cmds, T, S)
			   end
		   end, lists:seq(1, ?WORKERS)) ++
	 [fun(Parallel_Cmds, T, S) ->
		  split_seq_shrinker(Module, Parallel_Cmds, T, S) end,
	  fun(Parallel_Cmds, T, S) ->
		  remove_seq_shrinker(Module, Parallel_Cmds, T, S) end,
	  fun move_shrinker/3]}
       ]).

-spec gen_parallel_commands(size(), mod_name(), symbolic_state(), boolean()) ->
				   parallel_test_case().
gen_parallel_commands(Size, Mod, InitialState, InitFlag) ->
    try gen_parallel(Mod, InitialState, Size) of
	{Sequential,Parallel} = Res ->
	    case InitFlag of
		true ->
		    {[{init, InitialState}|Sequential], Parallel};
		false ->
		    erlang:put('$initial_state', InitialState),
		    Res
	    end
    catch
	Exc:Reason ->
	    throw({'$gen_commands', {Exc,Reason, erlang:get_stacktrace()}})
    end.

-spec gen_parallel(mod_name(), symbolic_state(), size()) -> parallel_test_case().
gen_parallel(Mod, InitialState, Size) ->
    Len1 = proper_arith:rand_int(?WORKERS, Size),
    CmdList = gen_commands(Mod, InitialState, [], Len1, Len1),
  
    %%TODO: does it make sense for Len to be different from Len1?
    Len = length(CmdList),
    {LenPar, {Seq, P}} = if Len =< ?LIMIT -> {Len, {[], CmdList}};
			    Len > ?LIMIT ->  {?LIMIT, lists:split(Len - ?LIMIT, CmdList)}
			 end,
    State = state_after(Mod, Seq),
    Env = mk_env(Seq, 1),
    Len2 = LenPar div ?WORKERS,
    Comb = mk_first_comb(LenPar, Len2, ?WORKERS),
    LookUp = orddict:from_list(mk_dict(P,1)),
    {Seq, fix_gen(LenPar, Len2, Comb, LookUp, Mod, State, Env)}.

-spec fix_gen(pos_integer(), non_neg_integer(), combination() | 'done', lookup(),
	      mod_name(), symbolic_state(), [symb_var()]) -> [command_list()].
fix_gen(_, 0, done, _, _, _, _) -> exit(error);   %% not supposed to reach here
fix_gen(MaxIndex, Len, done, LookUp, Mod, State, Env) when Len>=0 ->
    Comb = mk_first_comb(MaxIndex, Len-1, ?WORKERS),
    io:format("f"),
    fix_gen(MaxIndex, Len-1, Comb , LookUp, Mod, State, Env);	     
fix_gen(MaxIndex, Len, Comb, LookUp, Mod, State, Env) ->
    Cs = get_commands(Comb, LookUp),
    case safe_parallelize(Cs, Mod, State, Env) of
	{ok, Result} ->
	    Result;
	error ->
	    C1 = proplists:get_value(1, Comb),
	    C2 = proplists:get_value(2, Comb),
	    Next = get_next(Comb, Len, MaxIndex, lists:sort(C1 ++ C2), 2),
	    fix_gen(MaxIndex, Len, Next, LookUp, Mod, State, Env)
    end.

-spec safe_parallelize([command_list()], mod_name(), symbolic_state(), [symb_var()]) -> 
			      {'ok', [command_list()]} | 'error'.
safe_parallelize(Cs, Mod, State, Env) ->
    case lists:all(fun(C) -> is_valid(Mod, State, C, Env) end, Cs) of
	true -> 
	    case parallelize(Cs, Mod, State, Env) of
		{ok,_R} = Result -> Result;   
		error -> error
	    end;
	false -> 
	    error
    end.	   

%%TODO: produce possible interleavings in a lazy way
-spec parallelize([command_list()], mod_name(), symbolic_state(), [symb_var()]) ->
			 {'ok', [command_list()]} | 'error'.
parallelize(Cs, Mod, S, Env) ->
    Val = fun (C) -> is_valid(Mod, S, C, Env) end,
    case lists:all(Val, possible_interleavings(Cs)) of
	true ->
	    {ok, Cs};
	false -> 
	    error
    end.


%% -----------------------------------------------------------------------------
%% Sequential command execution
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
    try proper_symb:eval(Env,SymbState) of
	DynState -> 
	    {ok,DynState}
    catch
	_Exception:_Reason ->
	    {error, initialization_error}
    end.

-spec do_run_command(command_list(), proper_symb:var_values(), mod_name(),
		     history(), dynamic_state()) ->
			    {history(),dynamic_state(),statem_result()}.
do_run_command(Commands, Env, Module, History, State) ->
    case Commands of
	[] -> 
	    {lists:reverse(History), State, ok};
	[{init,_S}|Rest] ->
	    do_run_command(Rest, Env, Module, History, State);
 	[{set, {var,V}, {call,M,F,A}}|Rest] ->
	    M2 = proper_symb:eval(Env, M), 
	    F2 = proper_symb:eval(Env, F), 
	    A2 = proper_symb:eval(Env, A),
	    Call = {call, M2, F2, A2},
	    case check_precondition(Module, State, Call) of
		true ->
		    case safe_apply(M2, F2, A2) of
			{ok,Res} ->
			    case check_postcondition(Module, State, Call, Res) of
				true ->
				    Env2 = [{V,Res}|Env],
				    State2 = Module:next_state(State, Res, Call),
				    History2 = [{State,Res}|History],
				    do_run_command(Rest, Env2, Module, History2, State2);
				false ->
				    State2 = Module:next_state(State, Res, Call),
				    History2 = [{State,Res}|History],
				    {lists:reverse(History2), State2,
				     {postcondition,false}};
				{exception,_,_,_} = Exception ->
				    State2 = Module:next_state(State, Res, Call),
				    History2 = [{State,Res}|History],
				    {lists:reverse(History2), State2,
				     {postcondition,Exception}}
			    end;
			{error,Exception} ->
			    {lists:reverse(History),State,Exception}
		    end;
		false ->
		    {lists:reverse(History), State,
		     {precondition,false}};
		{exception,_,_,_} = Exception ->
		    {lists:reverse(History), State,
		     {precondition,Exception}}
	    end
    end.

-spec check_precondition(mod_name(), dynamic_state(), symb_call()) ->
				boolean() | exception().
check_precondition(Module, State, Call) ->
    try Module:precondition(State, Call) of
	Result -> Result
    catch
	Kind:Reason ->
	    {exception,Kind,Reason,erlang:get_stacktrace()}
    end.

-spec check_postcondition(mod_name(), dynamic_state(), symb_call(), term()) ->
				 boolean() | exception().
check_postcondition(Module, State, Call, Res) ->
    try Module:postcondition(State, Call, Res) of
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
%% Parallel command execution
%% -----------------------------------------------------------------------------


-spec run_parallel_commands(mod_name(), parallel_test_case()) ->
				   {history(), [command_history()], statem_result()}.
run_parallel_commands(Module, {_Sequential, _Parallel} = Cmds) ->
    run_parallel_commands(Module, Cmds, []).

-spec run_parallel_commands(mod_name(), parallel_test_case(), proper_symb:var_values()) ->
				   {history(), [command_history()], statem_result()}.
run_parallel_commands(Module, {Sequential, Parallel}, Env) ->
    InitialState = get_initial_state(Sequential),
    case safe_eval_init(Env, InitialState) of
	{ok, DynState} ->
	    case safe_run_sequential(Sequential, Env, Module, [], DynState) of
		{ok, {{Seq_history, State, ok}, Env1}} ->
		    Self = self(),
		    Parallel_test =
			fun(T) ->
				proper:spawn_link_migrate(
				  fun() ->
					  Self ! {self(), 
						  {result,
						   safe_execute(T, Env1, Module, [])}}
				  end)
			end,
		    Children = lists:map(Parallel_test, Parallel),
		    Parallel_history = receive_loop(Children, [], ?WORKERS),
		    case is_list(Parallel_history) of
			true ->
			    %% ok = init_ets_table(check_tab),
			    R = case check(Module, State, Env1, Env1, [],
					   Parallel_history, []) of
				    true ->  
					{Seq_history, Parallel_history, ok};
				    false ->
					{Seq_history, Parallel_history,
					 no_possible_interleaving}
				end,
			    %% TODO: delete the table here or after shrinking?
			    %% true = ets:delete(check_tab),
			    R;

		        %% if Parallel_history is not a list, then an
		        %% exception was raised
			false ->
			    io:format("Error during parallel execution~n"),
			    exit(error)
			    %%{Seq_history,[],Parallel_history}
		    end;
		{error, _Reason} ->
		    io:format("Error during sequential execution~n"),
		    exit(error)
		    %%{[],[],Reason}
	    end;
	{error, Reason} ->
	    {[], [], Reason}
    end.

-spec receive_loop([{pid(), command_list()}], [command_history()], non_neg_integer()) -> 
			  [command_history()] | statem_result().
receive_loop(_, ResultsReceived, 0) ->
    ResultsReceived;
receive_loop(Pid_list, ResultsReceived, N) when N > 0 ->
    receive 
	{Pid, {result, {ok, {H, ok}}}} ->
	    case lists:member(Pid, Pid_list) of
		false ->  
		    io:format("Pid ~w sent message~n", [Pid]),
		    receive_loop(Pid_list, ResultsReceived, N);
		true -> 
		    receive_loop(lists:delete(Pid, Pid_list),
				 ResultsReceived ++ [H], N-1)
	    end;
	{Pid, {result, {error, Other}}} -> 
	    case lists:member(Pid, Pid_list) of
		false ->  
		    io:format("Pid ~w sent message~n", [Pid]),
		    receive_loop(Pid_list, ResultsReceived, N);
		true ->
		    Other
	    end	    
    end.		             

%% -spec check_mem(mod_name(), dynamic_state(), proper_symb:var_values(),
%% 		command_history(), command_history(), command_history()) -> boolean().
%% check_mem(Mod, State, Env, P1, P2, Accum) ->
%%     T = {State,P1,P2},
%%     case ets:lookup(check_tab, T) of
%% 	[{T, V}] when is_boolean(V) ->
%% 	    V;
%% 	[] ->
%% 	    V = check(Mod, State, Env, P1, P2, Accum),
%% 	    memoize(check_tab, T, V),
%% 	    V
%%     end.

%% -spec check(mod_name(), dynamic_state(), proper_symb:var_values(),
%% 	    command_history(),command_history(),command_history()) -> boolean().
%% check(_Mod,_State,_Env,[],[],_Accum) ->
%%     true;
%% check(Mod,State,Env,[],[Head|Rest]=P2,Accum) ->
%%     {{set,{var,N},{call,M,F,A}},Res} = Head, 
%%     M2 = proper_symb:eval(Env,M), 
%%     F2 = proper_symb:eval(Env,F), 
%%     A2 = proper_symb:eval(Env,A),
%%     Call = {call,M2,F2,A2},
%%     case Mod:postcondition(State,Call,Res) of 
%% 	true ->
%% 	    Env2 = [{N,Res}|Env],
%% 	    NextState = Mod:next_state(State,Res,Call),
%% 	    V = check_mem(Mod,NextState,Env2,[],Rest,[Head|Accum]),
%% 	    memoize(check_tab,{NextState,[],Rest},V),
%% 	    V;
%% 	false -> 
%% 	    memoize(check_tab,{State,[],P2},false),
%% 	    false
%%     end;
%% check(Mod, State, Env, [Head|Rest]=P1, [], Accum) ->
%%     {{set,{var,N},{call,M,F,A}},Res} = Head, 
%%     M2 = proper_symb:eval(Env,M), 
%%     F2 = proper_symb:eval(Env,F), 
%%     A2 = proper_symb:eval(Env,A),
%%     Call = {call,M2,F2,A2},
%%     case Mod:postcondition(State, Call, Res) of
%% 	true ->
%% 	    Env2 = [{N,Res}|Env],
%% 	    NextState = Mod:next_state(State,Res,Call),
%% 	    V = check_mem(Mod,NextState,Env2,Rest,[],[Head|Accum]),
%% 	    memoize(check_tab, {NextState,Rest,[]}, V),
%% 	    V;
%% 	false ->
%% 	    memoize(check_tab, {State,P1,[]}, false),
%% 	    false
%%     end;
%% check(Mod,State,Env,[H1|Rest1]=P1,[H2|Rest2]=P2,Accum) ->
%%     {{set,{var,N1},{call,M1,F1,A1}},Res1} = H1,
%%     {{set,{var,N2},{call,M2,F2,A2}},Res2} = H2,
%%     M1_ = proper_symb:eval(Env,M1), 
%%     F1_ = proper_symb:eval(Env,F1), 
%%     A1_ = proper_symb:eval(Env,A1),
%%     Call1 = {call,M1_,F1_,A1_},   
%%     M2_ = proper_symb:eval(Env,M2), 
%%     F2_ = proper_symb:eval(Env,F2), 
%%     A2_ = proper_symb:eval(Env,A2),
%%     Call2 = {call,M2_,F2_,A2_}, 
%%     case {Mod:postcondition(State,Call1,Res1),Mod:postcondition(State,Call2,Res2)} of 
%% 	{true,false} -> 
%% 	    Env2 = [{N1,Res1}|Env],
%% 	    NextState = Mod:next_state(State,Res1,Call1),
%% 	    V = check_mem(Mod,NextState,Env2,Rest1,P2,[H1|Accum]),
%% 	    memoize(check_tab,{NextState,Rest1,P2},V),
%% 	    V;
%% 	{false,true} -> 
%% 	    Env2 = [{N2,Res2}|Env],
%% 	    NextState = Mod:next_state(State,Res2,Call2),
%% 	    V = check_mem(Mod,NextState,Env2,P1,Rest2,[H1|Accum]),
%% 	    memoize(check_tab,{NextState,P1,Rest2},V),
%% 	    V;
%% 	{true,true} -> 
%% 	    NextState1 = Mod:next_state(State,Res1,Call1),
%% 	    NextState2 = Mod:next_state(State,Res2,Call2),
%% 	    Env1 = [{N1,Res1}|Env],
%% 	    Env2 = [{N2,Res2}|Env],
%% 	    case check_mem(Mod,NextState1,Env1,Rest1,P2,[H1|Accum]) of
%% 		true -> 
%% 		    memoize(check_tab,{NextState1,Rest1,P2},true),
%% 		    true;
%% 		false ->
%% 		    memoize(check_tab,{NextState1,Rest1,P2},false),
%% 		    V = check_mem(Mod,NextState2,Env2,P1,Rest2,[H2|Accum]),
%% 		    memoize(check_tab,{NextState2,P1,Rest2},V),
%% 		    V
%% 	    end;
%% 	{false,false} -> 
%% 	    memoize(check_tab,{State,P1,P2},false),
%% 	    false
%%     end.

-spec check(mod_name(), dynamic_state(), proper_symb:var_values(), 
	    proper_symb:var_values(), [command_history()], [command_history()],
	    command_history()) -> boolean().
check(_Mod, _State, _OldEnv, _Env, [], [], _Accum) ->
    true;
check(_Mod, _State, Env, Env, _Tried, [], _Accum) ->
    false;
check(Mod, State, _OldEnv, Env, Tried, [], Accum) ->
    check(Mod, State, Env, Env, [], Tried, Accum);
check(Mod, State, OldEnv, Env, Tried, [P|Rest], Accum) ->
    case P of 
	[] ->
	    check(Mod, State, OldEnv, Env, Tried, Rest, Accum);
	[H|Tail] ->
	    {{set, {var, N1}, {call, M1, F1, A1}}, Res1} = H,
	    M1_ = proper_symb:eval(Env, M1), 
	    F1_ = proper_symb:eval(Env, F1), 
	    A1_ = proper_symb:eval(Env, A1),
	    Call1 = {call, M1_, F1_, A1_},
	    case Mod:postcondition(State, Call1, Res1) of
		true -> 
		    Env2 = [{N1, Res1}|Env],
		    NextState = Mod:next_state(State, Res1, Call1),
		    V = check(Mod, NextState, OldEnv, Env2, [Tail|Tried], Rest, [H|Accum]),
		    case V of
			true ->
			    true;
			false ->
			    check(Mod, State, OldEnv, Env, [P|Tried], Rest, Accum)
		    end;
		false ->
		    check(Mod, State, OldEnv, Env, [P|Tried], Rest, Accum)
	    end
    end.

-spec safe_run_sequential(command_list(), proper_symb:var_values(), mod_name(),
			  history(), dynamic_state()) ->
       {'ok', {{history(), dynamic_state(), statem_result()}, proper_symb:var_values()}} 
       | {'error', term()}.
safe_run_sequential(Commands, Env, Module, History, State) ->
    try run_sequential(Commands, Env, Module, History, State) of
	Result ->
	    {ok, Result}
    catch
	ExcKind:ExcReason ->
	    {error, {exception, ExcKind, ExcReason, erlang:get_stacktrace()}}
    end. 

-spec run_sequential(command_list(), proper_symb:var_values(), mod_name(),
		     history(), dynamic_state()) ->
       {{history(), dynamic_state(), statem_result()}, proper_symb:var_values()}.
run_sequential(Commands, Env, Module, History, State) ->
    case Commands of
	[] -> 
	    {{lists:reverse(History), State, ok}, Env};
	[{init, _S}|Rest] ->
	    run_sequential(Rest, Env, Module, History, State);
   	[{set, {var,V}, {call,M,F,A}}|Rest] ->
	    M2 = proper_symb:eval(Env, M), 
	    F2 = proper_symb:eval(Env, F),
	    A2 = proper_symb:eval(Env, A),
	    Call = {call, M2, F2, A2},
	    true = Module:precondition(State, Call),
	    Res = apply(M2, F2, A2),
	    true = Module:postcondition(State, Call, Res),
	    Env2 = [{V,Res}|Env],
	    State2 = Module:next_state(State, Res, Call),
	    History2 = [{State,Res}|History],
	    run_sequential(Rest, Env2, Module, History2, State2)
    end.

-spec safe_execute(command_list(), proper_symb:var_values(),
		   mod_name(), command_history()) -> 
			  {'ok', {command_history(), statem_result()}} | {'error', term()}.
safe_execute(Commands,Env,Module,History) ->
    try execute(Commands, Env, Module, History) of
	Result ->
	    {ok,Result}
    catch
	ExcKind:ExcReason ->
	    {error,{exception,ExcKind,ExcReason,erlang:get_stacktrace()}}
    end. 

-spec execute(command_list(), proper_symb:var_values(), mod_name(), command_history()) -> 
		     {command_history(), statem_result()}. 
execute(Commands, Env, Module, History) ->
    case Commands of
	[] -> 
	    {lists:reverse(History), ok};
	[{set, {var,V}, {call,M,F,A}} = Cmd|Rest] ->
	    M2 = proper_symb:eval(Env, M), 
	    F2 = proper_symb:eval(Env, F), 
	    A2 = proper_symb:eval(Env, A),
	    Res = apply(M2, F2, A2),
	    Env2 = [{V,Res}|Env],
	    History2 = [{Cmd,Res}|History],
	    execute(Rest, Env2, Module, History2)
    end.


%% -----------------------------------------------------------------------------
%% Command shrinkers
%% -----------------------------------------------------------------------------

-spec split_shrinker(mod_name(), command_list(), proper_types:type(),
		     proper_shrink:state()) ->
			    {[command_list()],proper_shrink:state()}.
split_shrinker(Module, [{init,InitialState}|Commands], Type, State) ->
    {Slices,NewState} = proper_shrink:split_shrinker(Commands, Type, State),
    {[[{init, InitialState}|X] || X <- Slices, is_valid(Module, InitialState, X, [])],
     NewState};

split_shrinker(Module, Commands, Type, State) ->
    {Slices,NewState} =  proper_shrink:split_shrinker(Commands, Type, State),
    InitialState = get_initial_state(Commands),
    {[X || X <- Slices, is_valid(Module, InitialState, X, [])],
     NewState}.

-spec remove_shrinker(mod_name(), command_list(), proper_types:type(),
		      proper_shrink:state()) ->
			     {[command_list()],proper_shrink:state()}.
remove_shrinker(Module, [{init,InitialState}|Commands] = Cmds, Type, State) ->
    {Slices,NewState} = proper_shrink:remove_shrinker(Commands, Type, State),
    case Slices of
	[] -> 
	    {[],NewState};
	_ ->
	    L = [[{init,InitialState}|S] || S <- Slices, 
					  is_valid(Module, InitialState, S, [])],
	    case L of
		[] -> remove_shrinker(Module, Cmds, Type, NewState);
		_ -> {L, NewState}
	    end
    end;

remove_shrinker(Module, Commands, Type, State) ->
    {Slices,NewState} = proper_shrink:remove_shrinker(Commands,Type,State),
    InitialState = get_initial_state(Commands),
    case Slices of
	[] -> 
	    {[],NewState};
	_ ->
	    L = [S || S <- Slices, is_valid(Module, InitialState, S, [])],
	    case L of
		[] -> remove_shrinker(Module, Commands, Type, NewState);
		_ -> {L, NewState}
	    end
    end.

-spec split_parallel_shrinker(pos_integer(), mod_name(), parallel_test_case(), 
			      proper_types:type(), proper_shrink:state()) ->
				     {[parallel_test_case()],proper_shrink:state()}. 
split_parallel_shrinker(I, Module, {Sequential,Parallel}, Type, State) ->
    SeqEnv = mk_env(Sequential, 1),
    SymbState = state_after(Module, Sequential),
    {Slices, NewState} = proper_shrink:split_shrinker(lists:nth(I, Parallel), Type, State), 
    {[{Sequential, update_list(I, S, Parallel)}
      || S <- Slices, is_valid(Module, SymbState, S, SeqEnv)],
     NewState}.

-spec remove_parallel_shrinker(pos_integer(), mod_name(), parallel_test_case(),
			       proper_types:type(), proper_shrink:state()) ->
				      {[parallel_test_case()],proper_shrink:state()}. 
remove_parallel_shrinker(I, Module, {Sequential,Parallel} = SP, Type, State) ->
    SeqEnv = mk_env(Sequential, 1),
    SymbState = state_after(Module, Sequential),
    {Slices,NewState} = proper_shrink:remove_shrinker(
			       lists:nth(I, Parallel), Type, State),
    case Slices of
	[] -> 
	    {[{Sequential, update_list(I, [], Parallel)}],NewState};
	[NewCommands] ->
	    case is_valid(Module, SymbState, NewCommands, SeqEnv) of
		true ->
		    {[{Sequential, update_list(I, NewCommands, Parallel)}],NewState};
	        _ ->
		    remove_parallel_shrinker(I, Module, SP, Type, NewState)
	    end
    end.

-spec split_seq_shrinker(mod_name(), parallel_test_case(), proper_types:type(),
			 proper_shrink:state()) ->
				{[parallel_test_case()],proper_shrink:state()}.
split_seq_shrinker(Module, {Sequential,Parallel}, Type, State) ->
    {Slices,NewState} = split_shrinker(Module, Sequential, Type, State),
    SymbState = get_initial_state(Sequential),
    {[{S, Parallel} || S <- Slices, P <- Parallel,
		       is_valid(Module, SymbState, S ++ P, [])],
     NewState}.

-spec remove_seq_shrinker(mod_name(), parallel_test_case(), proper_types:type(),
			  proper_shrink:state()) ->
				 {[parallel_test_case()],proper_shrink:state()}.
remove_seq_shrinker(_Module, TestCase, _Type, done) ->
    {[TestCase], done};
remove_seq_shrinker(Module, {Sequential,Parallel}, Type, State) ->
    {Slices,NewState} = remove_shrinker(Module, Sequential, Type, State),
    SymbState = get_initial_state(Sequential),
    UpdatedSlices = case Slices of
			[] -> [[]];
			_ -> Slices
		    end,
    L = [{S, Parallel} || S <- UpdatedSlices, P <- Parallel,
			  is_valid(Module, SymbState, S ++ P, [])],
    case L of
	[] -> remove_seq_shrinker(Module, {Sequential,Parallel}, Type, NewState);
	_ -> {L, NewState}
    end. 

-spec move_shrinker(parallel_test_case(), proper_types:type(), proper_shrink:state()) ->
			   {[parallel_test_case()],proper_shrink:state()}.
move_shrinker({Sequential, Parallel} = TestCase, _Type, _State) ->
    case get_first_commands(Parallel) of
	[] -> 
	    {[TestCase], done};
	List ->
	    {[{Sequential ++ [H], remove_first_command(Index, Parallel)} 
	      || {H, Index} <- List], shrunk}
    end.


%% -----------------------------------------------------------------------------
%% Utility functions
%% -----------------------------------------------------------------------------

-spec is_valid(mod_name(), symbolic_state(), command_list(), [symb_var()]) -> boolean().
is_valid(_Mod, _State, [], _Env) -> true;
is_valid(Module, _State, [{init,S}|Commands], _Env) ->
    is_valid(Module, S, Commands, _Env);
is_valid(Module, State, [{set,Var,{call,_M,_F,A}=Call}|Commands], Env) ->
    case Module:precondition(State, Call) of
	true ->
	    case args_defined(A, Env) of
		true ->
		    NextState = Module:next_state(State, Var, Call),
		    is_valid(Module, NextState, Commands, [Var|Env]);
		_ -> false
	    end;
	_ -> false
    end.

-spec args_defined([term()], [symb_var()]) -> boolean().
args_defined(A, Env) ->
    lists:all(fun ({var,I} = V) when is_integer(I) -> lists:member(V, Env);
		  (_V) -> true 
	      end, A).      

-spec state_after(mod_name(), command_list()) -> symbolic_state().
state_after(Module,Commands) ->
    NextState = fun(S,V,C) -> Module:next_state(S,V,C) end,
    lists:foldl(fun({init,S}, _) ->
			S;
		   ({set,Var,Call},S) ->
			NextState(S,Var,Call)
		end,
		get_initial_state(Commands),
		Commands).

-spec get_initial_state(command_list()) -> symbolic_state().
get_initial_state(Cmds) ->
    case Cmds of
	[{init,S}|_] -> S;
	_ -> erlang:get('$initial_state')
    end.

-spec command_names(command_list()) -> [{mod_name(),fun_name(),non_neg_integer()}].
command_names(Cmds) ->
    [{M, F, length(Args)} || {set, _Var, {call,M,F,Args}} <- Cmds].

-spec zip([A], [B]) -> [{A,B}].
zip(X, Y) ->
    zip(X, Y, []).

-spec zip([A], [B], [{A,B}]) -> [{A,B}].
zip([], _, Accum) -> lists:reverse(Accum);
zip(_, [], Accum) -> lists:reverse(Accum);
zip([X|Tail1], [Y|Tail2], Accum) ->
    zip(Tail1, Tail2, [{X,Y}|Accum]).

-spec commands_test(proper_gen:imm_instance()) -> boolean().
commands_test(X) when is_list(X) ->
    lists:all(fun is_command/1, X);
commands_test(_X) -> false.

-spec parallel_commands_test(proper_gen:imm_instance()) -> boolean().
parallel_commands_test({S,P}) ->
    commands_test(S) andalso
	lists:foldl(fun(X, Accum) -> commands_test(X) andalso Accum end, true, P);
parallel_commands_test(_) -> false.

-spec is_command(proper_gen:imm_instance()) -> boolean().
is_command({set, {var,V}, {call,M,F,A}})
  when is_integer(V), is_atom(M), is_atom(F), is_list(A) ->
    true;
is_command({init,_S}) ->
    true;
is_command(_Other) ->
    false.

-spec possible_interleavings([command_list()]) -> [command_list()].
possible_interleavings([P1,P2]) ->
    insert_all(P1, P2);
possible_interleavings([P1|Rest]) ->
    [I || L <- possible_interleavings(Rest),
	  I <- insert_all(P1, L)].

%% Returns all possible insertions of the elements of the first list,
%% preserving their order, inside the second list, i.e. all possible
%% command interleavings between two parallel processes

-spec insert_all([term()], [term()]) -> [[term()]].
insert_all([], List) ->
    [List];
insert_all([X], List) ->
    all_insertions(X, length(List) + 1, List);

insert_all([X|[Y|Rest]], List) ->
    [L2 || L1 <- insert_all([Y|Rest], List), 
	   L2 <- all_insertions(X,index(Y,L1),L1)].

-spec all_insertions(term(), pos_integer(), [term()]) -> [[term()]].
all_insertions(X, Limit, List) ->
    all_insertions_tr(X, Limit, 0, [], List, []).

-spec all_insertions_tr(term(), pos_integer(), non_neg_integer(),
			[term()], [term()], [[term()]]) -> [[term()]].
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

-spec index(term(), [term(),...]) -> pos_integer().
index(X, List) ->
    index(X, List, 1).

-spec index(term(), [term(),...], pos_integer()) -> pos_integer().
index(X, [X|_], N) -> N;
index(X, [_|Rest], N) -> index(X, Rest, N+1).

-spec mk_env(command_list(), pos_integer()) -> [{'var', pos_integer()}].
mk_env([], _) -> [];
mk_env([_|T], N) -> [{var,N}|mk_env(T, N+1)].

-spec mk_dict(command_list(), pos_integer()) -> [{pos_integer(), command()}].
mk_dict([], _) -> [];
mk_dict([H|T], N) -> [{N,H}|mk_dict(T, N+1)].

-spec mk_first_comb(pos_integer(), non_neg_integer(), pos_integer()) -> 
			   combination().
mk_first_comb(N, Len, W) ->
    mk_first_comb_tr(1, N, Len, [], W).

-spec mk_first_comb_tr(pos_integer(), pos_integer(), non_neg_integer(),
		       combination(), pos_integer()) -> 
			      combination().
mk_first_comb_tr(Start, N, _Len, Accum, 1) ->
    [{1,lists:seq(Start, N)}|Accum];
mk_first_comb_tr(Start, N, Len, Accum, W) ->
    K = Start + Len,
    mk_first_comb_tr(K, N, Len, [{W,lists:seq(Start, K-1)}|Accum], W-1).

-spec get_commands_inner([pos_integer()], lookup()) -> command_list().
get_commands_inner(Indexes, LookUp) ->
    lists:map(fun(Index) -> orddict:fetch(Index, LookUp) end, Indexes).

-spec get_commands(combination(), lookup()) -> [command_list()].
get_commands(PropList, LookUp) ->
    lists:map(fun({_,W}) -> get_commands_inner(W, LookUp) end, PropList).

-spec get_next(combination(), non_neg_integer(), pos_integer(),
	       [pos_integer()], pos_integer()) -> combination() | 'done'.
get_next(L, _Len, _MaxIndex, Available, 1) ->
    [{1,Available}|proplists:delete(1, L)];
get_next(L, Len, MaxIndex, Available, N) ->
    C = case proplists:is_defined(N, L) of
	    true ->
		next_comb(MaxIndex, proplists:get_value(N, L), Available);
	    false ->
		lists:sublist(Available, Len)
	end,
    case C of
	done -> 
	    if N =:= ?WORKERS -> 
		    done;
	       N =/= ?WORKERS -> 
		    C2 = proplists:get_value(N+1, L), 
		    NewList = [E || {M,_}=E <- L, M > N],
		    get_next(NewList, Len, MaxIndex, 
			     lists:sort(C2 ++ Available), N+1)
	    end;
	_ ->
	    get_next([{N,C}|proplists:delete(N, L)], 
		     Len, MaxIndex, Available -- C, N-1)
    end.

-spec next_comb(pos_integer(), [pos_integer()], [pos_integer()]) ->
			  [pos_integer()] | 'done'.
next_comb(MaxIndex, Comb, Available) ->
    Res = next_comb_tr(MaxIndex, lists:reverse(Comb), []),
    case is_well_defined(Res, Available) of
	true -> Res;
	false -> next_comb(MaxIndex, Res, Available)
    end.

-spec is_well_defined([pos_integer()] | 'done', [pos_integer()]) -> boolean().
is_well_defined(done, _) -> true;
is_well_defined(Comb, Available) ->	    
    lists:usort(Comb) =:= Comb andalso
	lists:all(fun(X) -> lists:member(X, Available) end, Comb).

-spec next_comb_tr(pos_integer(), [pos_integer()], [pos_integer()]) ->
			  [pos_integer()] | 'done'.
next_comb_tr(_MaxIndex, [], _Acc) ->
    done;
next_comb_tr(MaxIndex, [MaxIndex | Rest], Acc) ->
    next_comb_tr(MaxIndex, Rest, [1 | Acc]);
next_comb_tr(_MaxIndex, [X | Rest], Acc) ->
    lists:reverse(Rest) ++ [X+1] ++ Acc.			 

-spec update_list(pos_integer(), term(), [term(),...]) -> [term(),...].
update_list(I, X, List) ->
    update_list(I, X, List, [], 1).

-spec update_list(pos_integer(), term(), [term(),...], [term()], pos_integer()) -> 
			 [term(),...].
update_list(Index, X, [_H|T], Accum, Index) ->
    lists:reverse(Accum) ++ X ++ T;
update_list(Index, X, [H|T], Accum, N) ->
    update_list(Index, X, T, [H|Accum], N+1).

-spec get_first_commands([command_list()]) -> [{command(),pos_integer()}].
get_first_commands(List) ->
    get_first_commands(List, [], 1).

-spec get_first_commands([command_list()], [{command(),pos_integer()}], pos_integer()) ->
				[{command(),pos_integer()}].
get_first_commands([], Accum, _N) -> Accum;
get_first_commands([H|T], Accum, N) -> 
    case get_first_command(H) of
	none ->
	    get_first_commands(T, Accum, N+1);
	Cmd ->
	    get_first_commands(T, [{Cmd,N}|Accum], N+1)
    end.

-spec get_first_command(command_list()) -> command() | 'none'.
get_first_command([]) -> none;
get_first_command([H|_]) -> H.
    
-spec remove_first_command(pos_integer(), [command_list()]) -> [command_list()].
remove_first_command(I, List) ->
    remove_first_command(I, List, [], 1).

-spec remove_first_command(pos_integer(), [command_list(),...], [command_list()],
			   pos_integer()) -> [command_list(),...].
remove_first_command(Index, [H|T], Accum, Index) ->
    lists:reverse(Accum) ++ tl(H) ++ T;
remove_first_command(Index, [H|T], Accum, N) -> 
    remove_first_command(Index, T, [H|Accum], N+1).
    

%% -spec memoize(atom(), term(), term()) -> 'ok'.		     
%% memoize(Tab, Key, Value) ->
%%     ets:insert(Tab, {Key,Value}),
%%     ok.

%% -spec init_ets_table(atom()) -> 'ok'.
%% init_ets_table(Tab) ->
%%     case ets:info(Tab) of
%% 	undefined ->
%% 	    Tab = ets:new(Tab, [named_table]),
%% 	    ok;
%% 	_ ->
%% 	    ok
%%     end.
