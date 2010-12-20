-module(proper_statem).

-define(TRIES,100).
-export([gen_commands/2, run_commands/2, state_after/2,
	 remove_shrinker/4,split_shrinker/4]).

-include("proper_internal.hrl").

%%TODO: type refinement
-type var_count() :: non_neg_integer().
-type tries() :: non_neg_integer().
-type cmd_state() :: any().
-type result() :: any().

-spec gen_commands(mod_name(),size()) -> [proper_gen:imm_instance()].
gen_commands(Module,Size) ->
    gen_commands(Module, Module:initial_state(),
		 [], Size, Size, ?TRIES).

-spec gen_commands(mod_name(),cmd_state(),[proper_gen:imm_instance()],size(),
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
			   {[{cmd_state(),result()}],cmd_state(),'ok'|{'postcondition',any()}}.			  
run_commands(Module,Cmds) ->
    run_commands(Module,Cmds,[]).

-spec run_commands(mod_name(),[proper_gen:imm_instance()],[any()]) ->
			   {[{cmd_state(),result()}],cmd_state(),'ok'|{'postcondition',any()}}.
run_commands(Module,Commands,Env) ->

    do_run_command(Commands,
		   Env,
		   Module,
		   [],
		   proper_symb:eval(Env,Module:initial_state())).

-spec do_run_command([proper_gen:imm_instance()],[any()],mod_name(),
		     [{cmd_state(),result()}],cmd_state()) -> 
			    {[{cmd_state(),result()}],cmd_state(),'ok'|{'postcondition',any()}}.			    
do_run_command(Commands, Env, Module, History, State) ->
    case Commands of
	[] -> 
	    {lists:reverse(History), State, ok};

	[{init,S}|Rest] ->
	    State2 = proper_symb:eval(Env, S),
	    do_run_command(Rest, Env, Module, History, State2);
	
	[{set, {var,V}=Var, {call,M,F,A}}|Rest] ->
	    M2=proper_symb:eval(Env,M), 
	    F2=proper_symb:eval(Env,F), 
	    A2=proper_symb:eval(Env,A),
	    
	    Res = apply(M2,F2,A2),

	    Call = {call, M2,F2,A2},
	    History2 = [{State,Res}|History],
    
	    case Module:postcondition(State,Call,Res) of
		true ->
		    Env2 = [{V,Res}|proplists:delete(V,Env)],
		    State2 = Module:next_state(State,Var,Call),
		    do_run_command(Rest, Env2, Module, History2, State2);
			   
		Other ->
		    {lists:reverse(History2), State, {postcondition, Other}}
	    end
    end.

-spec split_shrinker(mod_name(),proper_gen:imm_instance(), proper_types:type(),proper_shrink:state()) ->
	  {[proper_gen:imm_instance()],proper_shrink:state()}.
split_shrinker(Module,Instance, Type, init) ->
    case {proper_types:find_prop(split, Type),
	  proper_types:find_prop(get_length, Type),
	  proper_types:find_prop(join, Type)} of
	{{ok,_Split}, {ok,GetLength}, {ok,_Join}} ->
	    split_shrinker(Module,Instance, Type, {slices,2,GetLength(Instance)});
	{_, _, _} ->
	    {[], done}
    end;

%% implementation of the ddmin algorithm, but stopping before the granularity
%% reaches 1, since we run a 'remove' shrinker after this
%% TODO: on success, start over with the whole testcase or keep removing slices?
split_shrinker(Module, Instance, Type, {slices,N,Len}) ->
    case Len < 2 * N of
	true ->
	    {[], done};
	false ->
	    {SmallSlices,BigSlices} = slice(Instance, Type, N, Len),
	     StartState = Module:initial_state(),
	     IsValid= fun (CommandSeq) -> validate(Module,StartState,CommandSeq) end,
	    {ValidSmall,ValidBig} = {lists:filter(IsValid,SmallSlices),
				     lists:filter(IsValid,BigSlices)},
	    {ValidSmall ++ ValidBig, {slices,2*N,Len}}
    end;
split_shrinker(Module,Instance, Type, {shrunk,Pos,{slices,DoubleN,_Len}}) ->
    N = DoubleN div 2,
    GetLength = proper_types:get_prop(get_length, Type),
    case Pos =< N of
	true ->
	    split_shrinker(Module,Instance, Type, {slices,2,GetLength(Instance)});
	false ->
	    split_shrinker(Module,Instance, Type, {slices,N-1,GetLength(Instance)})
    end.


-spec slice(proper_gen:imm_instance(), proper_types:type(), pos_integer(),
	    length()) ->
	  {[proper_gen:imm_instance()],[proper_gen:imm_instance()]}.
slice(Instance, Type, Slices, Len) ->
    BigSlices = Len rem Slices,
    SmallSlices = Slices - BigSlices,
    SmallSliceLen = Len div Slices,
    BigSliceLen = SmallSliceLen + 1,
    BigSliceTotal = BigSlices * BigSliceLen,
    WhereToSlice =
	[{1 + X * BigSliceLen, BigSliceLen}
	 || X <- lists:seq(0, BigSlices - 1)] ++
	[{BigSliceTotal + 1 + X * SmallSliceLen, SmallSliceLen}
	 || X <- lists:seq(0, SmallSlices - 1)],
    lists:unzip([take_slice(Instance, Type, From, SliceLen)
		 || {From,SliceLen} <- WhereToSlice]).

-spec take_slice(proper_gen:imm_instance(), proper_types:type(), pos_integer(),
		 length()) ->
	  {proper_gen:imm_instance(),proper_gen:imm_instance()}.
take_slice(Instance, Type, From, SliceLen) ->
    Split = proper_types:get_prop(split, Type),
    Join = proper_types:get_prop(join, Type),
    {Front,ImmBack} = Split(From - 1, Instance),
    {Slice,Back} = Split(SliceLen, ImmBack),
    {Slice, Join(Front, Back)}.

-spec remove_shrinker(mod_name(),proper_gen:imm_instance(), proper_types:type(),
		     proper_shrink:state()) ->
	  {[proper_gen:imm_instance()],proper_shrink:state()}.
remove_shrinker(Module,Commands,Type,init) ->
     case {proper_types:find_prop(get_indices, Type),
	   proper_types:find_prop(remove, Type)} of
	{{ok,_GetIndices}, {ok,_Remove}} ->
	    remove_shrinker(Module,Commands,Type,
			    {shrunk,1,{indices,ordsets:from_list([]),dummy}});
	_ ->
	    {[], done}
    end;
remove_shrinker(_Module,_Commands, _Type, {indices,_Checked,[]}) ->
    {[], done};
remove_shrinker(Module,Commands,Type,{indices,Checked,[Index | Rest]}) ->
    Remove = proper_types:get_prop(remove, Type),
    NewCommands = Remove(Index,Commands),
      
    StartState = Module:initial_state(),

    case validate(Module,StartState,NewCommands) of
	true -> 
	     {[NewCommands],
	      {indices,ordsets:add_element(Index, Checked),Rest}};
	_ ->
	    remove_shrinker(Module,Commands,Type,
			     {indices,ordsets:add_element(Index,Checked),Rest})
    end;   
   
remove_shrinker(Module,Commands,Type,{shrunk,1,{indices,Checked,_ToCheck}}) ->
    GetIndices = proper_types:get_prop(get_indices, Type),
    Indices = ordsets:from_list(GetIndices(Commands)),
    NewToCheck = ordsets:subtract(Indices, Checked),
    remove_shrinker(Module,Commands,Type,{indices,Checked,NewToCheck}).

-spec validate(mod_name(),cmd_state(),[proper_gen:imm_instance()]) -> boolean().
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

%%-----------------------------------------------------------------
%% @doc Evaluate command list, and return final state.
%%
%% Given a `Module' and `Commands', a value picked from the domain
%% `triq_statem:commands(Module)' 
%% @end
%%-----------------------------------------------------------------
-spec state_after(mod_name(),[proper_gen:imm_instance()]) -> cmd_state().		   
state_after(Module,Commands) ->
    NextState = fun(S,V,C) -> Module:next_state(S,V,C) end,
    lists:foldl(fun({init,S}, _) ->
			S;
		   ({set,Var,Call},S) ->
			NextState(S,Var,Call)
		end,
		Module:initial_state(),
		Commands).



