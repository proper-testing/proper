%%% Copyright 2010 Manolis Papadakis (manopapad@gmail.com)
%%%            and Kostis Sagonas (kostis@cs.ntua.gr)
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @author Manolis Papadakis <manopapad@gmail.com>
%%% @copyright 2010 Manolis Papadakis and Kostis Sagonas
%%% @version {@version}
%%% @doc This module contains the subsystem responsible for integration with
%%%	 Erlang's built-in type system.
%%% @private

-module(proper_typeserver).
-behaviour(gen_server).

-export([start/0, stop/0, translate_type/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).
-export([get_exp_info/1]).

-export_type([imm_type/0, mod_exp_types/0, mod_exp_funs/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------

-define(SRC_FILE_EXT, ".erl").

%% CAUTION: all these must be sorted
-define(STD_TYPES_0, [any,atom,binary,bitstring,bool,boolean,byte,char,float,
		      integer,list,neg_integer,non_neg_integer,number,
		      pos_integer,string,term,timeout,tuple]).
-define(HARD_ADTS, [{{array,0},array},{{dict,0},dict},{{digraph,0},digraph},
		    {{gb_set,0},gb_sets},{{gb_tree,0},gb_trees},
		    {{queue,0},queue},{{set,0},sets}]).
-define(HARD_ADT_MODS, [{array,[{array,0}]},{dict,[{dict,0}]},
			{digraph,[{digraph,0}]},{gb_sets,[{gb_set,0}]},
			{gb_trees,[{gb_tree,0}]},{queue,[{queue,0}]},
			{sets,[{set,0}]}]).


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type type_name() :: atom().
-type var_name() :: atom(). %% TODO: also integers?
-type field_name() :: atom().

-type type_kind() :: 'type' | 'record'.
-type type_ref() :: {type_kind(),type_name(),arity()}.
-type substs_dict() :: dict(). %% dict(field_name(),ret_type())
-type full_type_ref() :: {mod_name(),type_kind(),type_name(),
			  [ret_type()] | substs_dict()}.
-type type_repr() :: {'abs_type',abs_type(),[var_name()],boolean()}
		   | {'abs_record',[{field_name(),abs_type()}]}
		   | {'cached',fin_type()}.
-type gen_fun() :: fun((size()) -> fin_type()).
-type rec_fun() :: fun(([gen_fun()],size()) -> fin_type()).
-type rec_arg() :: {boolean() | {'list',boolean(),rec_fun()},full_type_ref()}.
-type rec_args() :: [rec_arg()].
-type ret_type() :: {'simple',fin_type()} | {'rec',rec_fun(),rec_args()}.
-type rec_fun_info() :: {pos_integer(),pos_integer(),[arity(),...],
			 [rec_fun(),...]}.

-type imm_type_ref() :: {type_name(),arity()}.
%% -type fun_ref() :: {fun_name(),arity()}.
%% -type fun_repr() :: fun_clause_repr().
-type fun_clause_repr() :: {[abs_type()],abs_type()}.
-type proc_fun_ref() :: {fun_name(),[abs_type()],abs_type()}.
-type full_imm_type_ref() :: {mod_name(),type_name(),arity()}.

-type mod_exp_types() :: set(). %% set(imm_type_ref())
-type mod_types() :: dict(). %% dict(type_ref(),type_repr())
-type mod_exp_funs() :: set(). %% set(fun_ref())
-type mod_specs() :: dict(). %% dict(fun_ref(),fun_repr())
-record(state,
	{cached    = dict:new() :: dict(),   %% dict(imm_type(),fin_type())
	 exp_types = dict:new() :: dict(),   %% dict(mod_name(),mod_exp_types())
	 types     = dict:new() :: dict()}). %% dict(mod_name(),mod_types())
-type state() :: #state{}.
-record(mod_info,
	{mod_exp_types = sets:new() :: mod_exp_types(),
	 mod_types     = dict:new() :: mod_types(),
	 mod_opaques   = sets:new() :: mod_exp_types(),
	 mod_exp_funs  = sets:new() :: mod_exp_funs(),
	 mod_specs     = dict:new() :: mod_specs()}).
-type mod_info() :: #mod_info{}.

-type stack() :: [full_type_ref() | 'tuple' | 'list' | 'union' | 'fun'].
-type var_dict() :: dict(). %% dict(var_name(),ret_type())
-type imm_type() :: {mod_name(),string()}.
-type fin_type() :: proper_types:type().
-type tagged_result(T) :: {'ok',T} | 'error'.
-type tagged_result2(T,S) :: {'ok',T,S} | 'error'.
-type rich_result(T) :: {'ok',T} | {'error',term()}.
-type rich_result2(T,S) :: {'ok',T,S} | {'error',term()}.


%%------------------------------------------------------------------------------
%% Server interface functions
%%------------------------------------------------------------------------------

-spec start() -> 'ok'.
start() ->
    %% TODO: Is this synchronous?
    {ok,_Pid} = gen_server:start({local,proper_typeserver}, ?MODULE, dummy, []),
    ok.

-spec stop() -> 'ok'.
stop() ->
    %% Ugly way to make the stopping synchronous.
    MonitorRef = erlang:monitor(process, proper_typeserver),
    gen_server:cast(proper_typeserver, stop),
    receive
	{'DOWN',MonitorRef,process,_,_} -> ok
    end.

-spec translate_type(imm_type()) -> rich_result(fin_type()).
translate_type(ImmType) ->
    gen_server:call(proper_typeserver, {translate_type,ImmType}).

-spec init(_) -> {'ok',state()}.
init(_) ->
    {ok, #state{}}.

-spec handle_call({'translate_type',imm_type()}, _, state()) ->
	  {'reply',rich_result(fin_type()),state()}.
handle_call({translate_type,ImmType}, _From, State) ->
    case translate_type(ImmType, State) of
	{ok,FinType,NewState} ->
	    {reply, {ok,FinType}, NewState};
	{error,_Reason} = Error ->
	    {reply, Error, State}
    end.

-spec handle_cast('stop', state()) -> {'stop','normal',state()}.
handle_cast(stop, State) ->
    {stop, normal, State}.

-spec handle_info(term(), state()) -> {'stop',{'received_info',term()},state()}.
handle_info(Info, State) ->
    {stop, {received_info,Info}, State}.

-spec terminate(term(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), _) -> {'ok',state()}.
code_change(_OldVsn, State, _) ->
    {ok, State}.


%%------------------------------------------------------------------------------
%% Top-level interface
%%------------------------------------------------------------------------------

-spec translate_type(imm_type(), state()) -> rich_result2(fin_type(),state()).
translate_type({Mod,Str} = ImmType, #state{cached = Cached} = State) ->
    case dict:find(ImmType, Cached) of
	{ok,Type} ->
	    {ok, Type, State};
	error ->
	    case parse_type(Str) of
		{ok,TypeForm} ->
		    case add_module(Mod, State) of
			{ok,NewState} ->
			    case convert(Mod, TypeForm, NewState) of
				{ok,FinType,
				 #state{cached = Cached} = FinalState} ->
				    NewCached = dict:store(ImmType, FinType,
							   Cached),
				    {ok, FinType,
				     FinalState#state{cached = NewCached}};
				{error,_Reason} = Error ->
				    Error
			    end;
			{error,_Reason} = Error ->
			    Error
		    end;
		{error,Reason} ->
		    {error, {parse_error,Str,Reason}}
	    end
    end.

-spec parse_type(string()) -> rich_result(abs_type()).
parse_type(Str) ->
    TypeStr = "-type mytype() :: " ++ Str ++ ".",
    case erl_scan:string(TypeStr) of
	{ok,Tokens,_EndLocation} ->
	    case erl_parse:parse_form(Tokens) of
		{ok,{attribute,_Line,type,{mytype,TypeExpr,[]}}} ->
		    {ok, TypeExpr};
		{error,_ErrorInfo} = Error ->
		    Error
	    end;
	{error,ErrorInfo,_EndLocation} ->
	    {error, ErrorInfo}
    end.

-spec add_module(mod_name(), state()) -> rich_result(state()).
add_module(Mod, #state{exp_types = ExpTypes} = State) ->
    case dict:is_key(Mod, ExpTypes) of
	true ->
	    {ok, State};
	false ->
	    case get_mod_code_and_exports(Mod) of
		{ok,AbsCode,ModExpFuns} ->
		    RawModInfo = get_mod_info(Mod, AbsCode, ModExpFuns),
		    ModInfo = process_adts(Mod, RawModInfo),
		    {ok, store_mod_info(Mod,ModInfo,State)};
		{error,Reason} ->
		    {error, {cant_load_code,Mod,Reason}}
	    end
    end.

-spec get_exp_info(mod_name()) -> rich_result2(mod_exp_types(),mod_exp_funs()).
get_exp_info(Mod) ->
    case get_mod_code_and_exports(Mod) of
	{ok,AbsCode,ModExpFuns} ->
	    ModInfo = get_mod_info(Mod, AbsCode, ModExpFuns),
	    {ok, ModInfo#mod_info.mod_exp_types, ModExpFuns};
	{error,_Reason} = Error ->
	    Error
    end.

-spec get_mod_code_and_exports(mod_name()) ->
	  rich_result2([abs_form()],mod_exp_funs()).
get_mod_code_and_exports(Mod) ->
    case code:which(Mod) of
	ObjFileName when is_list(ObjFileName) ->
	    get_chunks(ObjFileName);
	_ErrAtom when is_atom(_ErrAtom) ->
	    SrcFileName = atom_to_list(Mod) ++ ?SRC_FILE_EXT,
	    case code:where_is_file(SrcFileName) of
		FullSrcFileName when is_list(FullSrcFileName) ->
		    CompilerOpts = [binary,debug_info,{d,'PROPER_NOTRANS'}],
		    case compile:file(FullSrcFileName, CompilerOpts) of
			{ok,Mod,Binary} ->
			    get_chunks(Binary);
			error ->
			    {error, cant_compile_source_file}
		    end;
		non_existing ->
		    {error, cant_find_object_or_source_file}
	    end
    end.

-spec get_chunks(string() | binary()) ->
	  rich_result2([abs_form()],mod_exp_funs()).
get_chunks(ObjFile) ->
    case beam_lib:chunks(ObjFile, [abstract_code,exports]) of
	{ok,{_Mod,[{abstract_code,AbsCodeChunk},{exports,ExpFunsList}]}} ->
	    case AbsCodeChunk of
		{raw_abstract_v1,AbsCode} ->
		    {ok, AbsCode, sets:from_list(ExpFunsList)};
		no_abstract_code ->
		    {error, no_abstract_code};
		_ ->
		    {error, unsupported_abstract_code_format}
	    end;
	{error,beam_lib,Reason} ->
	    {error, Reason}
    end.

-spec get_mod_info(mod_name(), [abs_form()], mod_exp_funs()) -> mod_info().
get_mod_info(Mod, AbsCode, ModExpFuns) ->
    ModInfo = lists:foldl(fun add_mod_info/2,
			  #mod_info{mod_exp_funs = ModExpFuns}, AbsCode),
    case orddict:find(Mod, ?HARD_ADT_MODS) of
	{ok,ModADTs} ->
	    #mod_info{mod_exp_types = ModExpTypes, mod_types = ModTypes,
		      mod_opaques = ModOpaques} = ModInfo,
	    ModADTsSet = sets:from_list(ModADTs),
	    NewModExpTypes = sets:union(ModExpTypes, ModADTsSet),
	    NewModTypes = lists:foldl(fun store_adt/2, ModTypes, ModADTs),
	    NewModOpaques = sets:union(ModOpaques, ModADTsSet),
	    ModInfo#mod_info{mod_exp_types = NewModExpTypes,
			     mod_types = NewModTypes,
			     mod_opaques = NewModOpaques};
	error ->
	    ModInfo
    end.

-spec store_adt(imm_type_ref(), mod_types()) -> mod_types().
store_adt({Name,Arity}, ModTypes) ->
    TypeRef = {type,Name,Arity},
    TypeRepr = {abs_type,{type,0,any,[]},create_var_names(Arity),false},
    dict:store(TypeRef, TypeRepr, ModTypes).

-spec create_var_names(0..26) -> [var_name()].
create_var_names(Arity) when Arity >= 0, Arity =< 26 ->
    lists:seq($A, $A - 1 + Arity).

-spec add_mod_info(abs_form(), mod_info()) -> mod_info().
add_mod_info({attribute,_Line,export_type,TypesList},
	     #mod_info{mod_exp_types = ModExpTypes} = ModInfo) ->
    NewModExpTypes = sets:union(sets:from_list(TypesList), ModExpTypes),
    ModInfo#mod_info{mod_exp_types = NewModExpTypes};
add_mod_info({attribute,_Line,type,{{record,RecName},Fields,[]}},
	     #mod_info{mod_types = ModTypes} = ModInfo) ->
    FieldInfo = [process_rec_field(F) || F <- Fields],
    NewModTypes = dict:store({record,RecName,0}, {abs_record,FieldInfo},
			     ModTypes),
    ModInfo#mod_info{mod_types = NewModTypes};
add_mod_info({attribute,_Line,Kind,{Name,TypeForm,VarForms}},
	     #mod_info{mod_types = ModTypes,
		       mod_opaques = ModOpaques} = ModInfo)
	when Kind =:= type; Kind =:= opaque ->
    Arity = length(VarForms),
    VarNames = [V || {var,_,V} <- VarForms],
    %% TODO: No check whether variables are different, or non-'_'.
    NewModTypes = dict:store({type,Name,Arity},
			     {abs_type,TypeForm,VarNames,false}, ModTypes),
    NewModOpaques =
	case Kind of
	    type   -> ModOpaques;
	    opaque -> sets:add_element({Name,Arity}, ModOpaques)
	end,
    ModInfo#mod_info{mod_types = NewModTypes, mod_opaques = NewModOpaques};
add_mod_info({attribute,_Line,spec,{RawFunRef,[RawFirstClause | _Rest]}},
	     #mod_info{mod_specs = ModSpecs} = ModInfo) ->
    FunRef = case RawFunRef of
		 {_Mod,Name,Arity}  -> {Name,Arity};
		 {_Name,_Arity} = F -> F
	     end,
    %% TODO: We just take the first function clause.
    FirstClause = process_fun_clause(RawFirstClause),
    NewModSpecs = dict:store(FunRef, FirstClause, ModSpecs),
    ModInfo#mod_info{mod_specs = NewModSpecs};
add_mod_info(_Form, ModInfo) ->
    ModInfo.

-spec process_rec_field(abs_rec_field()) -> {field_name(),abs_type()}.
process_rec_field({record_field,_,{atom,_,FieldName}}) ->
    {FieldName, {type,0,any,[]}};
process_rec_field({record_field,_,{atom,_,FieldName},_Initialization}) ->
    {FieldName, {type,0,any,[]}};
process_rec_field({typed_record_field,RecField,FieldType}) ->
    {FieldName,_} = process_rec_field(RecField),
    {FieldName, FieldType}.

-spec process_fun_clause(abs_type()) -> fun_clause_repr().
process_fun_clause({type,_,'fun',[{type,_,product,Domain},Range]}) ->
    {Domain, Range};
process_fun_clause({type,_,bounded_fun,[MainClause,_Constraints]}) ->
    %% TODO: Spec constraints are ignored.
    process_fun_clause(MainClause).

-spec store_mod_info(mod_name(), mod_info(), state()) -> state().
store_mod_info(Mod,
	       #mod_info{mod_exp_types = ModExpTypes, mod_types = ModTypes},
	       #state{exp_types = ExpTypes, types = Types} = State) ->
    NewExpTypes = dict:store(Mod, ModExpTypes, ExpTypes),
    NewTypes = dict:store(Mod, ModTypes, Types),
    State#state{exp_types = NewExpTypes, types = NewTypes}.


%%------------------------------------------------------------------------------
%% ADT translation functions
%%------------------------------------------------------------------------------

-spec process_adts(mod_name(), mod_info()) -> mod_info().
process_adts(Mod, #mod_info{mod_exp_types = ModExpTypes,
			    mod_opaques = ModOpaques, mod_exp_funs = ModExpFuns,
			    mod_specs = ModSpecs} = ModInfo) ->
    %% TODO: No warning on unexported opaques.
    case sets:to_list(sets:intersection(ModExpTypes,ModOpaques)) of
	[] ->
	    ModInfo;
	ModADTs ->
	    ModSpecsList = dict:to_list(ModSpecs),
	    %% TODO: No warning on unexported API functions.
	    ModExpFunSpecs =
		[{Name,Domain,Range}
		 || {{Name,_Arity} = FunRef,{Domain,Range}} <- ModSpecsList,
		    sets:is_element(FunRef,ModExpFuns)],
	    AddADT = fun(ADT,Acc) -> add_adt(Mod,ADT,Acc,ModExpFunSpecs) end,
	    lists:foldl(AddADT, ModInfo, ModADTs)
    end.

-spec add_adt(mod_name(), imm_type_ref(), mod_info(), [proc_fun_ref()]) ->
	  mod_info().
add_adt(Mod, {Name,Arity}, #mod_info{mod_types = ModTypes} = ModInfo,
	ModExpFunSpecs) ->
    ADTRef = {type,Name,Arity},
    {abs_type,_InternalRepr,VarNames,false} = dict:fetch(ADTRef, ModTypes),
    FullADTRef = {Mod,Name,Arity},
    %% TODO: No warning on unsuitable range.
    SymbCalls1 = [get_symb_call(FullADTRef,Spec) || Spec <- ModExpFunSpecs],
    %% TODO: No warning on bad use of variables.
    SymbCalls2 = [fix_vars(FullADTRef,Call,RangeVars,VarNames)
		  || {ok,Call,RangeVars} <- SymbCalls1],
    case [Call || {ok,Call} <- SymbCalls2] of
	[] ->
	    %% TODO: No warning on no acceptable spec.
	    ModInfo;
	SymbCalls3 ->
	    NewADTRepr = {abs_type,{type,0,union,SymbCalls3},VarNames,true},
	    NewModTypes = dict:store(ADTRef, NewADTRepr, ModTypes),
	    ModInfo#mod_info{mod_types = NewModTypes}
    end.

-spec get_symb_call(full_imm_type_ref(), proc_fun_ref()) ->
	  tagged_result2(abs_type(),[var_name()]).
get_symb_call({Mod,_TypeName,_Arity} = FullADTRef, {FunName,Domain,Range}) ->
    BaseCall = {type,0,tuple,[{atom,0,'$call'},{atom,0,Mod},{atom,0,FunName},
			      {type,0,tuple,Domain}]},
    unwrap_range(FullADTRef, BaseCall, Range).

%% TODO: We only recurse into tuples and nonempty lists.
-spec unwrap_range(full_imm_type_ref(), abs_type() | 'dummy', abs_type()) ->
	  tagged_result2(abs_type(),[var_name()]).
unwrap_range(FullADTRef, Call, {paren_type,_,[Type]}) ->
    unwrap_range(FullADTRef, Call, Type);
unwrap_range(FullADTRef, Call, {ann_type,_,[_Var,Type]}) ->
    unwrap_range(FullADTRef, Call, Type);
unwrap_range(FullADTRef, Call, {type,_,nonempty_list,[ElemForm]}) ->
    NewCall = {type,0,tuple,[{atom,0,'$call'},{atom,0,erlang},{atom,0,hd},
			     {type,0,tuple,[Call]}]},
    unwrap_range(FullADTRef, NewCall, ElemForm);
unwrap_range(FullADTRef, Call, {type,_,tuple,ElemForms}) ->
    Translates = fun(T) -> unwrap_range(FullADTRef,dummy,T) =/= error end,
    case proper_arith:find_first(Translates, ElemForms) of
	none ->
	    error;
	{Pos,GoodElem} ->
	    NewCall = {type,0,tuple,[{atom,0,'$call'},{atom,0,erlang},
				     {atom,0,element},
				     {type,0,tuple,[{integer,0,Pos},Call]}]},
	    unwrap_range(FullADTRef, NewCall, GoodElem)
    end;
unwrap_range({_Mod,_SameName,Arity}, Call, {type,_,_SameName,ArgForms}) ->
    RangeVars = [V || {var,_,V} <- ArgForms, V =/= '_'],
    case length(ArgForms) =:= Arity andalso length(RangeVars) =:= Arity of
	true  -> {ok, Call, RangeVars};
	false -> error
    end;
unwrap_range({_SameMod,SameName,_Arity} = FullADTRef, Call,
	     {remote_type,_,[{atom,_,_SameMod},{atom,_,SameName},ArgForms]}) ->
    unwrap_range(FullADTRef, Call, {type,0,SameName,ArgForms});
unwrap_range(_FullADTRef, _Call, _Range) ->
    error.

-spec fix_vars(full_imm_type_ref(), abs_type(), [var_name()], [var_name()]) ->
	  tagged_result(abs_type()).
fix_vars(FullADTRef, Call, RangeVars, VarNames) ->
    NotAnyVar = fun(V) -> V =/= '_' end,
    case no_duplicates(VarNames) andalso lists:all(NotAnyVar,VarNames) of
	true ->
	    RawUsedVars =
		collect_vars(FullADTRef, Call, [[V] || V <- RangeVars]),
	    UsedVars = [lists:usort(L) || L <- RawUsedVars],
	    case correct_var_use(UsedVars) of
		true ->
		    PairAll = fun(L,Y) -> [{X,Y} || X <- L] end,
		    VarSubsts =
			lists:flatten(lists:zipwith(PairAll,UsedVars,VarNames)),
		    VarSubstsDict = dict:from_list(VarSubsts),
		    {ok, update_vars(Call,VarSubstsDict)};
		false ->
		    error
	    end;
	false ->
	    error
    end.

-spec no_duplicates(list()) -> boolean().
no_duplicates(L) ->
    length(lists:usort(L)) =:= length(L).

-spec correct_var_use([[var_name() | 0]]) -> boolean().
correct_var_use(UsedVars) ->
    NoNonVarArgs = fun([0|_]) -> false; (_) -> true end,
    lists:all(NoNonVarArgs, UsedVars)
    andalso no_duplicates(lists:flatten(UsedVars)).

-spec collect_vars(full_imm_type_ref(), abs_type(), [[var_name() | 0]]) ->
	  [[var_name() | 0]].
collect_vars(FullADTRef, {paren_type,_,[Type]}, UsedVars) ->
    collect_vars(FullADTRef, Type, UsedVars);
collect_vars(FullADTRef, {ann_type,_,[_Var,Type]}, UsedVars) ->
    collect_vars(FullADTRef, Type, UsedVars);
collect_vars({_Mod,_SameName,Arity} = FullADTRef, {type,_,_SameName,ArgForms},
	     UsedVars) ->
    case length(ArgForms) =:= Arity of
	true ->
	    VarArgs = [V || {var,_,V} <- ArgForms, V =/= '_'],
	    case length(VarArgs) =:= Arity of
		true ->
		    AddToList = fun(X,L) -> [X | L] end,
		    lists:zipwith(AddToList, VarArgs, UsedVars);
		false ->
		    [[0|L] || L <- UsedVars]
	    end;
	false ->
	    multi_collect_vars(FullADTRef, ArgForms, UsedVars)
    end;
collect_vars(FullADTRef, {type,_,_Name,ArgForms}, UsedVars) ->
    multi_collect_vars(FullADTRef, ArgForms, UsedVars);
collect_vars({_SameMod,SameName,_Arity} = FullADTRef,
	     {remote_type,_,[{atom,_,_SameMod},{atom,_,SameName},ArgForms]},
	     UsedVars) ->
    collect_vars(FullADTRef, {type,0,SameName,ArgForms}, UsedVars);
collect_vars(FullADTRef, {remote_type,_,ArgForms}, UsedVars) ->
    multi_collect_vars(FullADTRef, ArgForms, UsedVars);
collect_vars(_FullADTRef, _Call, UsedVars) ->
    UsedVars.

-spec multi_collect_vars(full_imm_type_ref(), [abs_type()],
			 [[var_name() | 0]]) -> [[var_name() | 0]].
multi_collect_vars({_Mod,_Name,Arity} = FullADTRef, Forms, UsedVars) ->
    NoUsedVars = lists:duplicate(Arity, []),
    MoreUsedVars = [collect_vars(FullADTRef,T,NoUsedVars) || T <- Forms],
    CombineVars = fun(L1,L2) -> lists:zipwith(fun erlang:'++'/2, L1, L2) end,
    lists:foldl(CombineVars, UsedVars, MoreUsedVars).

-spec update_vars(abs_type(), dict()) -> abs_type().
%% dict(var_name(),var_name())
update_vars({paren_type,Line,[Type]}, VarSubstsDict) ->
    {paren_type, Line, [update_vars(Type,VarSubstsDict)]};
update_vars({ann_type,Line,[Var,Type]}, VarSubstsDict) ->
    {ann_type, Line, [Var,update_vars(Type,VarSubstsDict)]};
update_vars({var,Line,VarName} = Call, VarSubstsDict) ->
    case dict:find(VarName, VarSubstsDict) of
	{ok,SubstVar} ->
	    {var, Line, SubstVar};
	error ->
	    Call
    end;
update_vars({remote_type,Line,ArgForms}, VarSubstsDict) ->
    {remote_type, Line, [update_vars(A,VarSubstsDict) || A <- ArgForms]};
update_vars({type,Line,Name,ArgForms}, VarSubstsDict) ->
    {type, Line, Name, [update_vars(A,VarSubstsDict) || A <- ArgForms]};
update_vars(Call, _VarSubstsDict) ->
    Call.


%%------------------------------------------------------------------------------
%% Type translation functions
%%------------------------------------------------------------------------------

-spec convert(mod_name(), abs_type(), state()) ->
	  rich_result2(fin_type(),state()).
convert(Mod, TypeForm, State) ->
    case convert(Mod, TypeForm, State, [], dict:new()) of
	{ok,{simple,Type},NewState} ->
	    {ok, Type, NewState};
	{ok,{rec,_RecFun,_RecArgs},_NewState} ->
	    {error, {internal,rec_returned_to_toplevel}};
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert(mod_name(), abs_type(), state(), stack(), var_dict()) ->
	  rich_result2(ret_type(),state()).
convert(Mod, {paren_type,_,[Type]}, State, Stack, VarDict) ->
    convert(Mod, Type, State, Stack, VarDict);
convert(Mod, {ann_type,_,[_Var,Type]}, State, Stack, VarDict) ->
    convert(Mod, Type, State, Stack, VarDict);
convert(_Mod, {var,_,'_'}, State, _Stack, _VarDict) ->
    {ok, {simple,proper_types:any()}, State};
convert(_Mod, {var,_,VarName}, State, _Stack, VarDict) ->
    case dict:find(VarName, VarDict) of
	%% TODO: do we need to check if we are at toplevel of a recursive?
	{ok,RetType} -> {ok, RetType, State};
	error        -> {error, {unbound_var,VarName}}
    end;
convert(Mod, {remote_type,_,[{atom,_,RemMod},{atom,_,Name},ArgForms]}, State,
	Stack, VarDict) ->
    case add_module(RemMod, State) of
	{ok,#state{exp_types = ExpTypes} = NewState} ->
	    RemModExpTypes = dict:fetch(RemMod, ExpTypes),
	    Arity = length(ArgForms),
	    case sets:is_element({Name,Arity}, RemModExpTypes) of
		true  -> convert_custom(Mod, RemMod, Name, ArgForms, NewState,
					Stack, VarDict);
		false -> {error, {type_not_exported,{RemMod,Name,Arity}}}
	    end;
	{error,_Reason} = Error ->
	    Error
    end;
convert(_Mod, {atom,_,Atom}, State, _Stack, _VarDict) ->
    {ok, {simple,proper_types:exactly(Atom)}, State};
convert(_Mod, {integer,_,_Int} = IntExpr, State, _Stack, _VarDict) ->
    convert_integer(IntExpr, State);
convert(_Mod, {op,_,_Op,_Arg} = OpExpr, State, _Stack, _VarDict) ->
    convert_integer(OpExpr, State);
convert(_Mod, {op,_,_Op,_Arg1,_Arg2} = OpExpr, State, _Stack, _VarDict) ->
    convert_integer(OpExpr, State);
convert(_Mod, {type,_,binary,[BaseExpr,UnitExpr]}, State, _Stack, _VarDict) ->
    case eval_int(BaseExpr) of
	{ok,0} ->
	    case eval_int(UnitExpr) of
		{ok,0} -> {ok, {simple,proper_types:exactly(<<>>)}, State};
		{ok,1} -> {ok, {simple,proper_types:bitstring()}, State};
		{ok,8} -> {ok, {simple,proper_types:binary()}, State};
		error  -> expr_error(invalid_unit, UnitExpr)
	    end;
	{ok,Len} when Len > 0 ->
	    case eval_int(UnitExpr) of
		{ok,0} -> {ok, {simple,proper_types:binary(Len)}, State};
		{ok,1} -> {ok, {simple,proper_types:bitstring(Len)}, State};
		{ok,8} -> {ok, {simple,proper_types:binary(Len)}, State};
		error  -> expr_error(invalid_unit, UnitExpr)
	    end;
	error ->
	    expr_error(invalid_base, BaseExpr)
    end;
convert(_Mod, {type,_,range,[LowExpr,HighExpr]}, State, _Stack, _VarDict) ->
    case {eval_int(LowExpr),eval_int(HighExpr)} of
	{{ok,Low},{ok,High}} ->
	    {ok, {simple,proper_types:integer(Low,High)}, State};
	_ ->
	    expr_error(invalid_range, LowExpr, HighExpr)
    end;
convert(_Mod, {type,_,nil,[]}, State, _Stack, _VarDict) ->
    {ok, {simple,proper_types:exactly([])}, State};
convert(Mod, {type,_,list,[ElemForm]}, State, Stack, VarDict) ->
    convert_list(Mod, false, ElemForm, State, Stack, VarDict);
convert(Mod, {type,_,nonempty_list,[ElemForm]}, State, Stack, VarDict) ->
    convert_list(Mod, true, ElemForm, State, Stack, VarDict);
convert(_Mod, {type,_,nonempty_list,[]}, State, _Stack, _VarDict) ->
    {ok, {simple,proper_types:non_empty(proper_types:list())}, State};
convert(_Mod, {type,_,nonempty_string,[]}, State, _Stack, _VarDict) ->
    {ok, {simple,proper_types:non_empty(proper_types:string())}, State};
convert(Mod, {type,_,tuple,ElemForms}, State, Stack, VarDict) ->
    convert_tuple(Mod, ElemForms, State, Stack, VarDict);
convert(Mod, {type,_,record,[{atom,_,Name}|FieldForms]}, State, Stack,
	VarDict) ->
    convert_record(Mod, Name, FieldForms, State, Stack, VarDict);
convert(Mod, {type,_,union,ChoiceForms}, State, Stack, VarDict) ->
    convert_union(Mod, ChoiceForms, State, Stack, VarDict);
convert(Mod, {type,_,'fun',[{type,_,product,Domain},Range]}, State, Stack,
	VarDict) ->
    Arity = length(Domain),
    case convert(Mod, Range, State, ['fun' | Stack], VarDict) of
	{ok,{simple,RangeType},NewState} ->
	    {ok, {simple,proper_types:function(Arity,RangeType)}, NewState};
	{ok,{rec,RecFun,RecArgs},NewState} ->
	    %% We bind the generated value by size.
	    case at_toplevel(RecArgs, Stack) of
		true ->
		    base_case_error(Stack);
		false ->
		    NewRecFun =
			fun(GenFuns,Size) ->
			    proper_types:function(Arity, RecFun(GenFuns,Size))
			end,
		    NewRecArgs = clean_rec_args(RecArgs),
		    {ok, {rec,NewRecFun,NewRecArgs}, NewState}
	    end;
	{error,_Reason} = Error ->
	    Error
    end;
convert(Mod, {type,_,Name,[]}, State, Stack, VarDict) ->
    case ordsets:is_element(Name, ?STD_TYPES_0) of
	true ->
	    {ok, {simple,apply(proper_types,Name,[])}, State};
	false ->
	    convert_maybe_hard_adt(Mod, Name, [], State, Stack, VarDict)
    end;
convert(Mod, {type,_,Name,ArgForms}, State, Stack, VarDict) ->
    convert_maybe_hard_adt(Mod, Name, ArgForms, State, Stack, VarDict);
convert(_Mod, TypeForm, _State, _Stack, _VarDict) ->
    {error, {unsupported_type,TypeForm}}.

-spec convert_list(mod_name(), boolean(), abs_type(), state(), stack(),
		   var_dict()) -> rich_result2(ret_type(),state()).
convert_list(Mod, NonEmpty, ElemForm, State, Stack, VarDict) ->
    case convert(Mod, ElemForm, State, [list | Stack], VarDict) of
	{ok,{simple,ElemType},NewState} ->
	    InnerType = proper_types:list(ElemType),
	    FinType = case NonEmpty of
			  true  -> proper_types:non_empty(InnerType);
			  false -> InnerType
		      end,
	    {ok, {simple,FinType}, NewState};
	{ok,{rec,RecFun,RecArgs},NewState} ->
	    case {at_toplevel(RecArgs,Stack), NonEmpty} of
		{true,true} ->
		    base_case_error(Stack);
		{true,false} ->
		    NewRecFun =
			fun(GenFuns,Size) ->
			    ElemGen = fun(S) -> ?LAZY(RecFun(GenFuns,S)) end,
			    proper_types:distlist(Size, ElemGen, false)
			end,
		    NewRecArgs = clean_rec_args(RecArgs),
		    {ok, {rec,NewRecFun,NewRecArgs}, NewState};
		{false,_} ->
		    {NewRecFun,NewRecArgs} =
			convert_rec_list(RecFun, RecArgs, NonEmpty),
		    {ok, {rec,NewRecFun,NewRecArgs}, NewState}
	    end;
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert_rec_list(rec_fun(), rec_args(), boolean()) ->
	  {rec_fun(),rec_args()}.
convert_rec_list(RecFun, [{true,FullTypeRef}] = RecArgs, NonEmpty) ->
    {NewRecFun,_NormalRecArgs} =
	convert_normal_rec_list(RecFun, RecArgs, NonEmpty),
    AltRecFun =
	fun([InstListGen],Size) ->
	    InstTypesList =
		proper_types:get_prop(internal_types, InstListGen(Size)),
	    proper_types:fixed_list([RecFun([fun(_Size) -> I end],0)
				     || I <- InstTypesList])
	end,
    NewRecArgs = [{{list,NonEmpty,AltRecFun},FullTypeRef}],
    {NewRecFun, NewRecArgs};
convert_rec_list(RecFun, RecArgs, NonEmpty) ->
    convert_normal_rec_list(RecFun, RecArgs, NonEmpty).

-spec convert_normal_rec_list(rec_fun(), rec_args(), boolean()) ->
	  {rec_fun(),rec_args()}.
convert_normal_rec_list(RecFun, RecArgs, NonEmpty) ->
    NewRecFun = fun(GenFuns,Size) ->
		    ElemGen = fun(S) -> RecFun(GenFuns, S) end,
		    proper_types:distlist(Size, ElemGen, NonEmpty)
		end,
    NewRecArgs = clean_rec_args(RecArgs),
    {NewRecFun, NewRecArgs}.

-spec convert_tuple(mod_name(), [abs_type()], state(), stack(), var_dict()) ->
	  rich_result2(ret_type(),state()).
convert_tuple(Mod, ElemForms, State, Stack, VarDict) ->
    case process_list(Mod, ElemForms, State, [tuple | Stack], VarDict) of
	{ok,RetTypes,NewState} ->
	    case combine_ret_types(RetTypes, tuple) of
		{simple,_FinType} = RetType ->
		    {ok, RetType, NewState};
		{rec,_RecFun,RecArgs} = RetType ->
		    case at_toplevel(RecArgs, Stack) of
			true  -> base_case_error(Stack);
			false -> {ok, RetType, NewState}
		    end
	    end;
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert_union(mod_name(), [abs_type()], state(), stack(), var_dict()) ->
	  rich_result2(ret_type(),state()).
convert_union(Mod, ChoiceForms, State, Stack, VarDict) ->
    case process_list(Mod, ChoiceForms, State, [union | Stack], VarDict) of
	{ok,RawChoices,NewState} ->
	    ProcessChoice = fun(T,A) -> process_choice(T,A,Stack) end,
	    {RevSelfRecs,RevNonSelfRecs,RevNonRecs} =
		lists:foldl(ProcessChoice, {[],[],[]}, RawChoices),
	    case {lists:reverse(RevSelfRecs),lists:reverse(RevNonSelfRecs),
		  lists:reverse(RevNonRecs)} of
		{_SelfRecs,[],[]} ->
		    base_case_error(Stack);
		{[],NonSelfRecs,NonRecs} ->
		    {ok, combine_ret_types(NonRecs ++ NonSelfRecs, union),
		     NewState};
		{SelfRecs,NonSelfRecs,NonRecs} ->
		    {BCaseRecFun,BCaseRecArgs} =
			case combine_ret_types(NonRecs ++ NonSelfRecs, union) of
			    {simple,BCaseType} ->
				{fun([],_Size) -> BCaseType end,[]};
			    {rec,BCRecFun,BCRecArgs} ->
				{BCRecFun,BCRecArgs}
			end,
		    NumBCaseGens = length(BCaseRecArgs),
		    [ParentRef | _Upper] = Stack,
		    FallbackRecFun = fun([SelfGen],_Size) -> SelfGen(0) end,
		    FallbackRecArgs = [{false,ParentRef}],
		    FallbackRetType = {rec,FallbackRecFun,FallbackRecArgs},
		    {rec,RCaseRecFun,RCaseRecArgs} =
			combine_ret_types([FallbackRetType] ++ SelfRecs
					  ++ NonSelfRecs, wunion),
		    NewRecFun =
			fun(AllGens,Size) ->
			    {BCaseGens,RCaseGens} =
				lists:split(NumBCaseGens, AllGens),
			    case Size of
				0 -> BCaseRecFun(BCaseGens,0);
				_ -> RCaseRecFun(RCaseGens,Size)
			    end
			end,
		    NewRecArgs = BCaseRecArgs ++ RCaseRecArgs,
		    {ok, {rec,NewRecFun,NewRecArgs}, NewState}
	    end;
	{error,_Reason} = Error ->
	    Error
    end.

-spec process_choice(ret_type(), {[ret_type()],[ret_type()],[ret_type()]},
		     stack()) -> {[ret_type()],[ret_type()],[ret_type()]}.
process_choice({simple,_} = RetType, {SelfRecs,NonSelfRecs,NonRecs}, _Stack) ->
    {SelfRecs, NonSelfRecs, [RetType | NonRecs]};
process_choice({rec,RecFun,RecArgs}, {SelfRecs,NonSelfRecs,NonRecs}, Stack) ->
    case at_toplevel(RecArgs, Stack) of
	true ->
	    case partition_by_toplevel(RecArgs, Stack, true) of
		{[],[],_,_} ->
		    NewRecArgs = clean_rec_args(RecArgs),
		    {[{rec,RecFun,NewRecArgs} | SelfRecs], NonSelfRecs,
		     NonRecs};
		{SelfRecArgs,SelfPos,OtherRecArgs,_OtherPos} ->
		    NumInstances = length(SelfRecArgs),
		    IsListInst = fun({true,_FTRef})                  -> false
				  ; ({{list,_NE,_AltRecFun},_FTRef}) -> true
				 end,
		    NewRecFun =
			case proper_arith:filter(IsListInst,SelfRecArgs) of
			    {[],[]} ->
				no_list_inst_rec_fun(RecFun,NumInstances,
						     SelfPos);
			    {[{{list,NonEmpty,AltRecFun},_}],[ListInstPos]} ->
				list_inst_rec_fun(AltRecFun,NumInstances,
						  SelfPos,NonEmpty,ListInstPos)
			end,
		    [{_B,SelfRef} | _] = SelfRecArgs,
		    NewRecArgs =
			[{false,SelfRef} | clean_rec_args(OtherRecArgs)],
		    {[{rec,NewRecFun,NewRecArgs} | SelfRecs], NonSelfRecs,
		     NonRecs}
	    end;
	false ->
	    NewRecArgs = clean_rec_args(RecArgs),
	    {SelfRecs, [{rec,RecFun,NewRecArgs} | NonSelfRecs], NonRecs}
    end.

-spec no_list_inst_rec_fun(rec_fun(), pos_integer(), [position()]) -> rec_fun().
no_list_inst_rec_fun(RecFun, NumInstances, SelfPos) ->
    fun([SelfGen|OtherGens], Size) ->
	?LETSHRINK(
	    Instances,
	    %% Size distribution will be a little off if both normal and
	    %% instance-accepting generators are present.
	    lists:duplicate(NumInstances, SelfGen(Size div NumInstances)),
	    begin
		InstGens = [fun(_Size) -> proper_types:exactly(I) end
			    || I <- Instances],
		AllGens = proper_arith:insert(InstGens, SelfPos, OtherGens),
		RecFun(AllGens, Size)
	    end)
    end.

-spec list_inst_rec_fun(rec_fun(), pos_integer(), [position()], boolean(),
			position()) -> rec_fun().
list_inst_rec_fun(AltRecFun, NumInstances, SelfPos, NonEmpty, ListInstPos) ->
    fun([SelfGen|OtherGens], Size) ->
	?LETSHRINK(
	    AllInsts,
	    lists:duplicate(NumInstances - 1, SelfGen(Size div NumInstances))
	    ++ proper_types:distlist(Size div NumInstances, SelfGen, NonEmpty),
	    begin
		{Instances,InstList} = lists:split(NumInstances - 1, AllInsts),
		InstGens = [fun(_Size) -> proper_types:exactly(I) end
			    || I <- Instances],
		InstTypesList = [proper_types:exactly(I) || I <- InstList],
		InstListGen =
		    fun(_Size) -> proper_types:fixed_list(InstTypesList) end,
		AllInstGens = proper_arith:list_insert(ListInstPos, InstListGen,
						       InstGens),
		AllGens = proper_arith:insert(AllInstGens, SelfPos, OtherGens),
		AltRecFun(AllGens, Size)
	    end)
    end.

-spec convert_maybe_hard_adt(mod_name(), type_name(), [abs_type()], state(),
			     stack(), var_dict()) ->
	  rich_result2(ret_type(),state()).
convert_maybe_hard_adt(Mod, Name, ArgForms, State, Stack, VarDict) ->
    Arity = length(ArgForms),
    case orddict:find({Name,Arity}, ?HARD_ADTS) of
	{ok,Mod} ->
	    convert_custom(Mod, Mod, Name, ArgForms, State, Stack, VarDict);
	{ok,ADTMod} ->
	    ADT = {remote_type,0,[{atom,0,ADTMod},{atom,0,Name},ArgForms]},
	    convert(Mod, ADT, State, Stack, VarDict);
	error ->
	    convert_custom(Mod, Mod, Name, ArgForms, State, Stack, VarDict)
    end.

-spec convert_custom(mod_name(), mod_name(), type_name(), [abs_type()], state(),
		     stack(), var_dict()) -> rich_result2(ret_type(),state()).
convert_custom(Mod, RemMod, Name, ArgForms, State, Stack, VarDict) ->
    case process_list(Mod, ArgForms, State, Stack, VarDict) of
	{ok,Args,NewState} ->
	    Arity = length(Args),
	    TypeRef = {type,Name,Arity},
	    FullTypeRef = {RemMod,type,Name,Args},
	    convert_type(TypeRef, FullTypeRef, NewState, Stack);
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert_record(mod_name(), type_name(), [abs_type()], state(), stack(),
		     var_dict()) -> rich_result2(ret_type(),state()).
convert_record(Mod, Name, RawSubsts, State, Stack, VarDict) ->
    Substs = [{N,T} || {type,_,field_type,[{atom,_,N},T]} <- RawSubsts],
    {SubstFields,SubstTypeForms} = lists:unzip(Substs),
    case process_list(Mod, SubstTypeForms, State, Stack, VarDict) of
	{ok,SubstTypes,NewState} ->
	    SubstsDict = dict:from_list(lists:zip(SubstFields, SubstTypes)),
	    TypeRef = {record,Name,0},
	    FullTypeRef = {Mod,record,Name,SubstsDict},
	    convert_type(TypeRef, FullTypeRef, NewState, Stack);
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert_type(type_ref(), full_type_ref(), state(), stack()) ->
	  rich_result2(ret_type(),state()).
convert_type(TypeRef, {Mod,_Kind,_Name,_Spec} = FullTypeRef, State, Stack) ->
    case stack_position(FullTypeRef, Stack) of
	none ->
	    ModTypes = dict:fetch(Mod, State#state.types),
	    case dict:find(TypeRef, ModTypes) of
		{ok,TypeRepr} -> convert_new_type(TypeRef, FullTypeRef,
						  TypeRepr, State, Stack);
		error         -> {error, {missing_type,Mod,TypeRef}}
	    end;
	1 ->
	    base_case_error(Stack);
	_Pos ->
	    {ok, {rec,fun([Gen],Size) -> Gen(Size) end,[{true,FullTypeRef}]},
	     State}
    end.

-spec convert_new_type(type_ref(), full_type_ref(), type_repr(), state(),
		       stack()) -> rich_result2(ret_type(),state()).
convert_new_type(_TypeRef, {_Mod,type,_Name,[]}, {cached,FinType}, State,
		 _Stack) ->
    {ok, {simple,FinType}, State};
convert_new_type(TypeRef, {Mod,type,_Name,Args} = FullTypeRef,
		 {abs_type,TypeForm,Vars,Symbolic}, State, Stack) ->
    VarDict = dict:from_list(lists:zip(Vars, Args)),
    case convert(Mod, TypeForm, State, [FullTypeRef | Stack], VarDict) of
	{ok, {simple,ImmFinType}, NewState} ->
	    FinType =
		case Symbolic of
		    true  -> proper_symb:internal_well_defined(ImmFinType);
		    false -> ImmFinType
		end,
	    FinalState =
		case Vars of
		    [] -> cache_simple_type(Mod, TypeRef, FinType, NewState);
		    _  -> NewState
		end,
	    {ok, {simple,FinType}, FinalState};
	{ok, {rec,RecFun,RecArgs}, NewState} ->
	    convert_maybe_rec(FullTypeRef, Symbolic, RecFun, RecArgs, NewState,
			      Stack);
	{error,_Reason} = Error ->
	    Error
    end;
convert_new_type(_TypeRef, {Mod,record,Name,SubstsDict} = FullTypeRef,
		 {abs_record,OrigFields}, State, Stack) ->
    Fields = [case dict:find(FieldName, SubstsDict) of
		  {ok,NewFieldType} -> NewFieldType;
		  error             -> OrigFieldType
	      end
	      || {FieldName,OrigFieldType} <- OrigFields],
    case convert_tuple(Mod, [{atom,0,Name} | Fields], State,
		       [FullTypeRef | Stack], dict:new()) of
	{ok, {simple,_FinType}, _NewState} = Result ->
	    Result;
	{ok, {rec,RecFun,RecArgs}, NewState} ->
	    convert_maybe_rec(FullTypeRef, false, RecFun, RecArgs, NewState,
			      Stack);
	{error,_Reason} = Error ->
	    Error
    end.

-spec cache_simple_type(mod_name(), type_ref(), fin_type(), state()) -> state().
cache_simple_type(Mod, TypeRef, FinType, #state{types = Types} = State) ->
    ModTypes = dict:fetch(Mod, Types),
    NewModTypes = dict:store(TypeRef, {cached,FinType}, ModTypes),
    NewTypes = dict:store(Mod, NewModTypes, Types),
    State#state{types = NewTypes}.

-spec convert_maybe_rec(full_type_ref(), boolean(), rec_fun(), rec_args(),
			state(), stack()) -> rich_result2(ret_type(),state()).
convert_maybe_rec(FullTypeRef, Symbolic, RecFun, RecArgs, State, Stack) ->
    case at_toplevel(RecArgs, Stack) of
	true  -> base_case_error(Stack);
	false -> safe_convert_maybe_rec(FullTypeRef, Symbolic, RecFun, RecArgs,
					State)
    end.

-spec safe_convert_maybe_rec(full_type_ref(), boolean(), rec_fun(), rec_args(),
			     state()) -> rich_result2(ret_type(),state()).
safe_convert_maybe_rec(FullTypeRef, Symbolic, RecFun, RecArgs, State) ->
    case partition_rec_args(FullTypeRef, RecArgs, false) of
	{[],[],_,_} ->
	    {ok, {rec,RecFun,RecArgs}, State};
	{MyRecArgs,MyPos,OtherRecArgs,_OtherPos} ->
	    case lists:all(fun({B,_T}) -> B =:= false end, MyRecArgs) of
		true  -> convert_rec_type(Symbolic, RecFun, MyPos, OtherRecArgs,
					  State);
		false -> {error, {internal,true_rec_arg_reached_type}}
	    end
    end.

-spec convert_rec_type(boolean(), rec_fun(), [position()], rec_args(),
		       state()) -> {ok, ret_type(), state()}.
convert_rec_type(Symbolic, RecFun, MyPos, [], State) ->
    NumRecArgs = length(MyPos),
    M = fun(GenFun) ->
	    fun(Size) ->
		GenFuns = lists:duplicate(NumRecArgs, GenFun),
		RecFun(GenFuns, erlang:max(0,Size - 1))
	    end
	end,
    SizedGen = y(M),
    ImmFinType = ?SIZED(Size,SizedGen(Size + 1)),
    FinType = case Symbolic of
		  true  -> proper_symb:internal_well_defined(ImmFinType);
		  false -> ImmFinType
	      end,
    {ok, {simple,FinType}, State};
convert_rec_type(_Symbolic, RecFun, MyPos, OtherRecArgs, State) ->
    NumRecArgs = length(MyPos),
    NewRecFun =
	fun(OtherGens,TopSize) ->
	    M = fun(GenFun) ->
		    fun(Size) ->
			GenFuns = lists:duplicate(NumRecArgs, GenFun),
			AllGens =
			    proper_arith:insert(GenFuns, MyPos, OtherGens),
			RecFun(AllGens, erlang:max(0,Size - 1))
		    end
		end,
	    (y(M))(TopSize)
	end,
    NewRecArgs = clean_rec_args(OtherRecArgs),
    {ok, {rec,NewRecFun,NewRecArgs}, State}.

%% Y Combinator: Read more at http://bc.tech.coop/blog/070611.html.
-spec y(fun((fun((T) -> S))  ->  fun((T) -> S))) -> fun((T) -> S).
y(M) ->
    G = fun(F) ->
	    M(fun(A) -> (F(F))(A) end)
	end,
    G(G).

-spec process_list(mod_name(), [abs_type() | ret_type()], state(), stack(),
		   var_dict()) -> rich_result2([ret_type()],state()).
process_list(Mod, RawTypes, State, Stack, VarDict) ->
    Process = fun({simple,_FinType} = Type, {ok,Types,State1}) ->
		     {ok, [Type|Types], State1};
		 ({rec,_RecFun,_RecArgs} = Type, {ok,Types,State1}) ->
		     {ok, [Type|Types], State1};
		 (TypeForm, {ok,Types,State1}) ->
		     case convert(Mod, TypeForm, State1, Stack, VarDict) of
			 {ok,Type,State2} -> {ok,[Type|Types],State2};
			 {error,_} = Err  -> Err
		     end;
		 (_RawType, {error,_} = Err) ->
		     Err
	      end,
    case lists:foldl(Process, {ok,[],State}, RawTypes) of
	{ok,RevTypes,NewState} ->
	    {ok, lists:reverse(RevTypes), NewState};
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert_integer(abs_expr(), state()) -> rich_result2(ret_type(),state()).
convert_integer(Expr, State) ->
    case eval_int(Expr) of
	{ok,Int} -> {ok, {simple,proper_types:exactly(Int)}, State};
	error    -> expr_error(invalid_int_const, Expr)
    end.

-spec eval_int(abs_expr()) -> tagged_result(integer()).
eval_int(Expr) ->
    NoBindings = erl_eval:new_bindings(),
    try erl_eval:expr(Expr, NoBindings) of
	{value,Value,_NewBindings} when is_integer(Value) ->
	    {ok, Value};
	_ ->
	    error
    catch
	error:_ ->
	    error
    end.

-spec expr_error(atom(), abs_expr()) -> {'error',term()}.
expr_error(Reason, Expr) ->
    {error, {Reason,lists:flatten(erl_pp:expr(Expr))}}.

-spec expr_error(atom(), abs_expr(), abs_expr()) -> {'error',term()}.
expr_error(Reason, Expr1, Expr2) ->
    Str1 = lists:flatten(erl_pp:expr(Expr1)),
    Str2 = lists:flatten(erl_pp:expr(Expr2)),
    {error, {Reason,Str1,Str2}}.

-spec base_case_error(stack()) -> {'error',term()}.
%% TODO: This might confuse, since it doesn't record the arguments to parametric
%%	 types or the type subsitutions of a record.
base_case_error([{Mod,type,Name,Args} | _Upper]) ->
    Arity = length(Args),
    {error, {no_base_case,{Mod,type,Name,Arity}}};
base_case_error([{Mod,record,Name,_SubstsDict} | _Upper]) ->
    {error, {no_base_case,{Mod,record,Name}}}.


%%------------------------------------------------------------------------------
%% Helper datatypes handling functions
%%------------------------------------------------------------------------------

-spec stack_position(full_type_ref(), stack()) -> 'none' | pos_integer().
stack_position(FullTypeRef, Stack) ->
    SameType = fun(A) -> same_full_type_ref(A,FullTypeRef) end,
    case proper_arith:find_first(SameType, Stack) of
	{Pos,_} -> Pos;
	none    -> none
    end.

-spec partition_by_toplevel(rec_args(), stack(), boolean()) ->
	  {rec_args(),[position()],rec_args(),[position()]}.
partition_by_toplevel(RecArgs, [], _OnlyInstanceAccepting) ->
    {[],[],RecArgs,lists:seq(1,length(RecArgs))};
partition_by_toplevel(RecArgs, [_Parent | _Upper], _OnlyInstanceAccepting)
	when is_atom(_Parent) ->
    {[],[],RecArgs,lists:seq(1,length(RecArgs))};
partition_by_toplevel(RecArgs, [Parent | _Upper], OnlyInstanceAccepting) ->
    partition_rec_args(Parent, RecArgs, OnlyInstanceAccepting).

-spec at_toplevel(rec_args(), stack()) -> boolean().
at_toplevel(RecArgs, Stack) ->
    case partition_by_toplevel(RecArgs, Stack, false) of
	{[],[],_,_} -> false;
	_           -> true
    end.

-spec partition_rec_args(full_type_ref(), rec_args(), boolean()) ->
	  {rec_args(),[position()],rec_args(),[position()]}.
partition_rec_args(FullTypeRef, RecArgs, OnlyInstanceAccepting) ->
    SameType =
	case OnlyInstanceAccepting of
	    true  -> fun({false,_T}) -> false
		      ; ({_B,T})     -> same_full_type_ref(T,FullTypeRef) end;
	    false -> fun({_B,T}) -> same_full_type_ref(T,FullTypeRef) end
	end,
    proper_arith:partition(SameType, RecArgs).

%% Tuples can be of 0 arity, unions of 1 and wunions at least of 2.
-spec combine_ret_types([ret_type()], 'tuple' | 'union' | 'wunion') ->
	  ret_type().
combine_ret_types(RetTypes, EnclosingType) ->
    case lists:all(fun is_simple_ret_type/1, RetTypes) of
	true ->
	    %% This should never happen for wunion.
	    Combine = case EnclosingType of
			  tuple -> fun proper_types:tuple/1;
			  union -> fun proper_types:union/1
		      end,
	    FinTypes = [T || {simple,T} <- RetTypes],
	    {simple, Combine(FinTypes)};
	false ->
	    NumTypes = length(RetTypes),
	    {RevRecFuns,RevRecArgsList,NumRecs} =
		lists:foldl(fun add_ret_type/2, {[],[],0}, RetTypes),
	    RecFuns = lists:reverse(RevRecFuns),
	    RecArgsList = lists:reverse(RevRecArgsList),
	    RecArgLens = [length(RecArgs) || RecArgs <- RecArgsList],
	    RecFunInfo = {NumTypes,NumRecs,RecArgLens,RecFuns},
	    FlatRecArgs = lists:flatten(RecArgsList),
	    NewRecFun =
		case EnclosingType of
		    tuple  -> tuple_rec_fun(RecFunInfo);
		    union  -> union_rec_fun(RecFunInfo);
		    wunion -> wunion_rec_fun(RecFunInfo)
		end,
	    NewRecArgs =
		case EnclosingType of
		    tuple  -> soft_clean_rec_args(FlatRecArgs, RecFunInfo);
		    union  -> clean_rec_args(FlatRecArgs);
		    wunion -> clean_rec_args(FlatRecArgs)
		end,
	    {rec, NewRecFun, NewRecArgs}
    end.

-spec tuple_rec_fun(rec_fun_info()) -> rec_fun().
tuple_rec_fun({_NumTypes,NumRecs,RecArgLens,RecFuns}) ->
    fun(AllGFs,TopSize) ->
	Size = TopSize div NumRecs,
	GFsList = proper_arith:unflatten(AllGFs, RecArgLens),
	ArgsList = [[GenFuns,Size] || GenFuns <- GFsList],
	ZipFun = fun erlang:apply/2,
	proper_types:tuple(lists:zipwith(ZipFun, RecFuns, ArgsList))
    end.

-spec union_rec_fun(rec_fun_info()) -> rec_fun().
union_rec_fun({_NumTypes,_NumRecs,RecArgLens,RecFuns}) ->
    fun(AllGFs,Size) ->
	GFsList = proper_arith:unflatten(AllGFs, RecArgLens),
	ArgsList = [[GenFuns,Size] || GenFuns <- GFsList],
	ZipFun = fun(F,A) -> ?LAZY(apply(F,A)) end,
	proper_types:union(lists:zipwith(ZipFun, RecFuns, ArgsList))
    end.

-spec wunion_rec_fun(rec_fun_info()) -> rec_fun().
wunion_rec_fun({NumTypes,_NumRecs,RecArgLens,RecFuns}) ->
    fun(AllGFs,Size) ->
	GFsList = proper_arith:unflatten(AllGFs, RecArgLens),
	ArgsList = [[GenFuns,Size] || GenFuns <- GFsList],
	ZipFun = fun(W,F,A) -> {W,?LAZY(apply(F,A))} end,
	RecWeight = Size div (NumTypes - 1) + 1,
	Weights = [1 | lists:duplicate(NumTypes - 1, RecWeight)],
	WeightedChoices = lists:zipwith3(ZipFun, Weights, RecFuns, ArgsList),
	proper_types:wunion(WeightedChoices)
    end.

-spec add_ret_type(ret_type(), {[rec_fun()],[rec_args()],non_neg_integer()}) ->
	  {[rec_fun()],[rec_args()],non_neg_integer()}.
add_ret_type({simple,FinType}, {RecFuns,RecArgsList,NumRecs}) ->
    {[fun([],_) -> FinType end | RecFuns], [[] | RecArgsList], NumRecs};
add_ret_type({rec,RecFun,RecArgs}, {RecFuns,RecArgsList,NumRecs}) ->
    {[RecFun | RecFuns], [RecArgs | RecArgsList], NumRecs + 1}.

-spec is_simple_ret_type(ret_type()) -> boolean().
is_simple_ret_type({simple,_FinType}) ->
    true;
is_simple_ret_type({rec,_RecFun,_RecArgs}) ->
    false.

-spec clean_rec_args(rec_args()) -> rec_args().
clean_rec_args(RecArgs) ->
    [{false,F} || {_B,F} <- RecArgs].

-spec soft_clean_rec_args(rec_args(), rec_fun_info()) -> rec_args().
soft_clean_rec_args(RecArgs, RecFunInfo) ->
    soft_clean_rec_args_tr(RecArgs, [], RecFunInfo, false, 1).

-spec soft_clean_rec_args_tr(rec_args(), rec_args(), rec_fun_info(),
			     boolean(), position()) -> rec_args().
soft_clean_rec_args_tr([], Acc, _RecFunInfo, _FoundListInst, _Pos) ->
    lists:reverse(Acc);
soft_clean_rec_args_tr([{{list,_NonEmpty,_AltRecFun},FTRef} | Rest], Acc,
		       RecFunInfo, true, Pos) ->
    NewArg = {false,FTRef},
    soft_clean_rec_args_tr(Rest, [NewArg | Acc], RecFunInfo, true, Pos+1);
soft_clean_rec_args_tr([{{list,NonEmpty,AltRecFun},FTRef} | Rest], Acc,
		       RecFunInfo, false, Pos) ->
    {NumTypes,NumRecs,RecArgLens,RecFuns} = RecFunInfo,
    AltRecFunPos = get_group(Pos, RecArgLens),
    AltRecFuns = proper_arith:list_update(AltRecFunPos, AltRecFun, RecFuns),
    AltRecFunInfo = {NumTypes,NumRecs,RecArgLens,AltRecFuns},
    NewArg = {{list,NonEmpty,tuple_rec_fun(AltRecFunInfo)},FTRef},
    soft_clean_rec_args_tr(Rest, [NewArg | Acc], RecFunInfo, true, Pos+1);
soft_clean_rec_args_tr([Arg | Rest], Acc, RecFunInfo, FoundListInst, Pos) ->
    soft_clean_rec_args_tr(Rest, [Arg | Acc], RecFunInfo, FoundListInst, Pos+1).

-spec get_group(pos_integer(), [non_neg_integer()]) -> pos_integer().
get_group(Pos, AllMembers) ->
    get_group_tr(Pos, AllMembers, 1).

-spec get_group_tr(pos_integer(), [non_neg_integer()], pos_integer()) ->
	  pos_integer().
get_group_tr(Pos, [Members | Rest], GroupNum) ->
    case Pos =< Members of
	true  -> GroupNum;
	false -> get_group_tr(Pos - Members, Rest, GroupNum + 1)
    end.

-spec same_full_type_ref(full_type_ref(), term()) -> boolean().
same_full_type_ref({_Mod,type,_Name,Args1}, {_Mod,type,_Name,Args2}) ->
    length(Args1) =:= length(Args2)
    andalso lists:all(fun({A,B}) -> same_ret_type(A,B) end,
		      lists:zip(Args1, Args2));
same_full_type_ref({_Mod,record,_Name,SubstsDict1},
		   {_Mod,record,_Name,SubstsDict2}) ->
    same_substs_dict(SubstsDict1, SubstsDict2);
same_full_type_ref(_, _) ->
    false.

-spec same_ret_type(ret_type(), ret_type()) -> boolean().
same_ret_type({simple,FinType1}, {simple,FinType2}) ->
    same_fin_type(FinType1, FinType2);
same_ret_type({rec,RecFun1,RecArgs1}, {rec,RecFun2,RecArgs2}) ->
    NumRecArgs = length(RecArgs1),
    length(RecArgs2) =:= NumRecArgs
    andalso lists:all(fun({A1,A2}) -> same_rec_arg(A1,A2,NumRecArgs) end,
		      lists:zip(RecArgs1,RecArgs2))
    andalso same_rec_fun(RecFun1, RecFun2, NumRecArgs);
same_ret_type(_, _) ->
    false.

%% TODO: Is this too strict?
-spec same_rec_arg(rec_arg(), rec_arg(), arity()) -> boolean().
same_rec_arg({{list,_SameBool,AltRecFun1},FTRef1},
	     {{list,_SameBool,AltRecFun2},FTRef2}, NumRecArgs) ->
    same_rec_fun(AltRecFun1, AltRecFun2, NumRecArgs)
    andalso same_full_type_ref(FTRef1, FTRef2);
same_rec_arg({true,FTRef1}, {true,FTRef2}, _NumRecArgs) ->
    same_full_type_ref(FTRef1, FTRef2);
same_rec_arg({false,FTRef1}, {false,FTRef2}, _NumRecArgs) ->
    same_full_type_ref(FTRef1, FTRef2);
same_rec_arg(_, _, _NumRecArgs) ->
    false.

-spec same_substs_dict(substs_dict(), substs_dict()) -> boolean().
same_substs_dict(SubstsDict1, SubstsDict2) ->
    SameKVPair = fun({{_K,V1},{_K,V2}}) -> same_ret_type(V1,V2);
		    (_)                 -> false
		 end,
    SubstsKVList1 = lists:sort(dict:to_list(SubstsDict1)),
    SubstsKVList2 = lists:sort(dict:to_list(SubstsDict2)),
    length(SubstsKVList1) =:= length(SubstsKVList2)
    andalso lists:all(SameKVPair, lists:zip(SubstsKVList1,SubstsKVList2)).

-spec same_fin_type(fin_type(), fin_type()) -> boolean().
same_fin_type(Type1, Type2) ->
    proper_types:equal_types(Type1, Type2).

-spec same_rec_fun(rec_fun(), rec_fun(), arity()) -> boolean().
same_rec_fun(RecFun1, RecFun2, NumRecArgs) ->
    %% It's ok that we return a type, even if there's a 'true' for use of
    %% an instance.
    GenFun = fun(_Size) -> proper_types:exactly('$dummy') end,
    GenFuns = lists:duplicate(NumRecArgs,GenFun),
    same_fin_type(RecFun1(GenFuns,0), RecFun2(GenFuns,0)).
