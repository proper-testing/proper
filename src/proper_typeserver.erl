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

-module(proper_typeserver).
-behaviour(gen_server).

-export([start/0, stop/0, translate_type/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-export_type([imm_type/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------

-define(STD_TYPES, [any,atom,binary,bitstring,bool,boolean,byte,char,float,
		    integer,list,neg_integer,non_neg_integer,number,pos_integer,
		    string,term,timeout,tuple]).


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type mod_name() :: atom().
-type type_name() :: atom().
-type var_name() :: atom(). %% TODO: also integers?
-type field_name() :: atom().

-type type_kind() :: 'type' | 'record'.
-type type_ref() :: {type_kind(),type_name(),arity()}.
-type substs_dict() :: dict(). %% dict(field_name(),ret_type())
-type full_type_ref() :: {mod_name(),type_kind(),type_name(),
			  [ret_type()] | substs_dict()}.
-type type_repr() :: {'abs_type',abs_type(),[var_name()]}
		   | {'abs_record',[{field_name(),abs_type()}]}
		   | {'cached',fin_type()}.
-type gen_fun() :: fun((size()) -> fin_type()).
-type rec_fun() :: fun(([gen_fun()],size()) -> fin_type()).
-type rec_args() :: [{boolean(),full_type_ref()}].
-type ret_type() :: {'simple',fin_type()} | {'rec',rec_fun(),rec_args()}.

-type mod_exported() :: set(). %% set({type_name(),arity()})
-type mod_types() :: dict(). %% dict(type_ref(),type_repr())
-record(state,
	{cached   = dict:new() :: dict(),   %% dict(imm_type(),fin_type())
	 exported = dict:new() :: dict(),   %% dict(mod_name(),mod_exported())
	 types    = dict:new() :: dict()}). %% dict(mod_name(),mod_types())
-type state() :: #state{}.

-type stack() :: [full_type_ref() | 'tuple' | 'list' | 'union' | 'fun'].
-type var_dict() :: dict(). %% dict(var_name(),ret_type())
-type imm_type() :: {mod_name(),string()}.
-type fin_type() :: proper_types:type().
-type tagged_result(T) :: {'ok',T} | 'error'.
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
add_module(Mod, #state{exported = ExpDict, types = TypesDict} = State) ->
    case dict:is_key(Mod, TypesDict) of
	true ->
	    {ok, State};
	false ->
	    case get_module_code(Mod) of
		{ok,AbsCode} ->
		    {ModExported,ModTypes} = get_type_info(AbsCode),
		    NewExpDict = dict:store(Mod, ModExported, ExpDict),
		    NewTypesDict = dict:store(Mod, ModTypes, TypesDict),
		    {ok, State#state{exported = NewExpDict,
				     types = NewTypesDict}};
		{error,Reason} ->
		    {error, {cant_load_code,Mod,Reason}}
	    end
    end.

-spec get_module_code(mod_name()) -> rich_result([abs_form()]).
get_module_code(Mod) ->
    case code:which(Mod) of
	ObjFileName when is_list(ObjFileName) ->
	    case beam_lib:chunks(ObjFileName, [abstract_code]) of
		{ok,{Mod,[{abstract_code,AbsCodeChunk}]}} ->
		    case AbsCodeChunk of
			{raw_abstract_v1,AbsCode} ->
			    {ok, AbsCode};
			no_abstract_code ->
			    {error, no_abstract_code};
			_ ->
			    {error, abstract_code_format}
		    end;
		{error,beam_lib,Reason} ->
		    {error, Reason}
	    end;
	ErrAtom when is_atom(ErrAtom) ->
	    {error, ErrAtom}
    end.

-spec get_type_info([abs_form()]) -> {mod_exported(),mod_types()}.
get_type_info(AbsCode) ->
    lists:foldl(fun add_type_info/2, {sets:new(),dict:new()}, AbsCode).

-spec add_type_info(abs_form(), {mod_exported(),mod_types()}) ->
	  {mod_exported(),mod_types()}.
add_type_info({attribute,_Line,export_type,TypesList},
	      {ModExported,ModTypes}) ->
    NewModExported = sets:union(sets:from_list(TypesList), ModExported),
    {NewModExported, ModTypes};
add_type_info({attribute,_Line,type,{{record,RecName},Fields,[]}},
	      {ModExported,ModTypes}) ->
    FieldInfo = [process_rec_field(F) || F <- Fields],
    NewModTypes = dict:store({record,RecName,0}, {abs_record,FieldInfo},
			     ModTypes),
    {ModExported, NewModTypes};
add_type_info({attribute,_Line,Kind,{Name,TypeForm,VarForms}},
	      {ModExported,ModTypes}) when Kind =:= type; Kind =:= opaque ->
    Arity = length(VarForms),
    VarNames = [V || {var,_,V} <- VarForms],
    NewModTypes = dict:store({type,Name,Arity}, {abs_type,TypeForm,VarNames},
			     ModTypes),
    {ModExported, NewModTypes};
add_type_info(_Form, Acc) ->
    Acc.

-spec process_rec_field(abs_rec_field()) -> {field_name(),abs_type()}.
process_rec_field({record_field,_,{atom,_,FieldName}}) ->
    {FieldName, {type,0,any,[]}};
process_rec_field({record_field,_,{atom,_,FieldName},_Initialization}) ->
    {FieldName, {type,0,any,[]}};
process_rec_field({typed_record_field,RecField,FieldType}) ->
    {FieldName,_} = process_rec_field(RecField),
    {FieldName, FieldType}.


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
	{ok,#state{exported = Exported} = NewState} ->
	    RemModExported = dict:fetch(RemMod, Exported),
	    Arity = length(ArgForms),
	    case sets:is_element({Name,Arity}, RemModExported) of
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
		    {error, no_base_case};
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
    case lists:member(Name, ?STD_TYPES) of
	true ->
	    {ok, {simple,apply(proper_types,Name,[])}, State};
	false ->
	    convert_custom(Mod, Mod, Name, [], State, Stack, VarDict)
    end;
convert(Mod, {type,_,Name,ArgForms}, State, Stack, VarDict) ->
    convert_custom(Mod, Mod, Name, ArgForms, State, Stack, VarDict);
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
	    NewRecFun =
		fun(GenFuns,Size) ->
		    ?LET(Len,
			 %% TODO: instead go up to Size div 2?
			 proper_types:integer(0,Size),
			 case Len of
			     0 -> [];
			     %% TODO: inner type needs to be lazy?
			     _ -> InnerType = RecFun(GenFuns,Size div Len),
				  proper_types:vector(Len, InnerType)
			 end)
		end,
	    NewRecArgs = clean_rec_args(RecArgs),
	    case {NonEmpty, at_toplevel(RecArgs,Stack)} of
		{true,true} ->
		    {error, no_base_case};
		{true,false} ->
		    FinalRecFun = fun(G,S) ->
				      proper_types:non_empty(NewRecFun(G,S))
				  end,
		    {ok, {rec,FinalRecFun,NewRecArgs}, NewState};
		{false,_} ->
		    {ok, {rec,NewRecFun,NewRecArgs}, NewState}
	    end;
	{error,_Reason} = Error ->
	    Error
    end.

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
			true  -> {error, no_base_case};
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
		    {error, no_base_case};
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
process_choice({simple,_FinType} = RetType, {SelfRecs,NonSelfRecs,NonRecs},
	       _Stack) ->
    {SelfRecs, NonSelfRecs, [RetType | NonRecs]};
process_choice({rec,RecFun,RecArgs}, {SelfRecs,NonSelfRecs,NonRecs} = Acc,
	       Stack) ->
    case at_toplevel(RecArgs, Stack) of
	true ->
	    case partition_by_toplevel(RecArgs, Stack, true) of
		{[],[],_,_} ->
		    NewRecArgs = clean_rec_args(RecArgs),
		    {[{rec,RecFun,NewRecArgs} | SelfRecs], NonSelfRecs,
		     NonRecs};
		{SelfRecArgs,SelfPos,OtherRecArgs,_OtherPos} ->
		    process_true_choice(RecFun, SelfRecArgs, SelfPos,
					OtherRecArgs, Acc)
	    end;
	false ->
	    NewRecArgs = clean_rec_args(RecArgs),
	    {SelfRecs, [{rec,RecFun,NewRecArgs} | NonSelfRecs], NonRecs}
    end.

-spec process_true_choice(rec_fun(), rec_args(), [position()], rec_args(),
			  {[ret_type()],[ret_type()],[ret_type()]}) ->
	  {[ret_type()],[ret_type()],[ret_type()]}.
process_true_choice(RecFun, SelfRecArgs, SelfPos, OtherRecArgs,
		    {SelfRecs,NonSelfRecs,NonRecs}) ->
    NumInstances = length(SelfRecArgs),
    NewRecFun =
	fun([SelfGen|OtherGens], Size) ->
	    ?LETSHRINK(
		Instances,
		%% Size distribution will be a little off if both normal and
		%% instance-accepting generators are present.
		%% TODO: these need to be lazy?
		lists:duplicate(NumInstances, SelfGen(Size div NumInstances)),
		begin
		    InstGens = [fun(_Size) -> proper_types:exactly(I) end
				|| I <- Instances],
		    AllGens = proper_arith:insert(InstGens, SelfPos, OtherGens),
		    RecFun(AllGens, Size)
		end
	    )
	end,
    [{true,SelfRef} | _] = SelfRecArgs,
    NewRecArgs = [{false,SelfRef} | clean_rec_args(OtherRecArgs)],
    {[{rec,NewRecFun,NewRecArgs} | SelfRecs], NonSelfRecs, NonRecs}.

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
	    {error, no_base_case};
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
		 {abs_type,TypeForm,Vars}, State, Stack) ->
    VarDict = dict:from_list(lists:zip(Vars, Args)),
    case convert(Mod, TypeForm, State, [FullTypeRef | Stack], VarDict) of
	{ok, {simple,FinType} = RetType, #state{types = Types} = NewState} ->
	    case Vars of
		[] ->
		    ModTypes = dict:fetch(Mod, Types),
		    NewModTypes = dict:store(TypeRef, {cached,FinType},
					     ModTypes),
		    NewTypes = dict:store(Mod, NewModTypes, Types),
		    {ok, RetType, NewState#state{types = NewTypes}};
		_ ->
		    {ok, RetType, NewState}
	    end;
	{ok, {rec,RecFun,RecArgs}, NewState} ->
	    convert_maybe_rec(FullTypeRef, RecFun, RecArgs, NewState, Stack);
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
	    convert_maybe_rec(FullTypeRef, RecFun, RecArgs, NewState, Stack);
	{error,_Reason} = Error ->
	    Error
    end.

-spec convert_maybe_rec(full_type_ref(), rec_fun(), rec_args(), state(),
			stack()) -> rich_result2(ret_type(),state()).
convert_maybe_rec(FullTypeRef, RecFun, RecArgs, State, Stack) ->
    case at_toplevel(RecArgs, Stack) of
	true  -> {error, no_base_case};
	false -> safe_convert_maybe_rec(FullTypeRef, RecFun, RecArgs, State)
    end.

-spec safe_convert_maybe_rec(full_type_ref(), rec_fun(), rec_args(), state()) ->
	  rich_result2(ret_type(),state()).
safe_convert_maybe_rec(FullTypeRef, RecFun, RecArgs, State) ->
    case partition_rec_args(FullTypeRef, RecArgs, false) of
	{[],[],_,_} ->
	    {ok, {rec,RecFun,RecArgs}, State};
	{MyRecArgs,MyPos,OtherRecArgs,_OtherPos} ->
	    case lists:all(fun({B,_T}) -> not B end, MyRecArgs) of
		true ->
		    convert_rec_type(RecFun, MyPos, OtherRecArgs, State);
		false ->
		    {error, {internal,true_rec_arg_reached_type}}
	    end
    end.

-spec convert_rec_type(rec_fun(), [position()], rec_args(), state()) ->
	  {ok, ret_type(), state()}.
convert_rec_type(RecFun, MyPos, [], State) ->
    NumRecArgs = length(MyPos),
    M = fun(GenFun) ->
	    fun(Size) ->
		GenFuns = lists:duplicate(NumRecArgs, GenFun),
		RecFun(GenFuns, erlang:max(0,Size - 1))
	    end
	end,
    SizedGen = y(M),
    {ok, {simple,?SIZED(Size,SizedGen(Size + 1))}, State};
convert_rec_type(RecFun, MyPos, OtherRecArgs, State) ->
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
	    true  -> fun({true,T})   -> same_full_type_ref(T,FullTypeRef)
		      ; ({false,_T}) -> false end;
	    false -> fun({_B,T}) -> same_full_type_ref(T,FullTypeRef) end
	end,
    proper_arith:partition(SameType, RecArgs).

-spec combine_ret_types([ret_type()], 'tuple' | 'union' | 'wunion') ->
	  ret_type().
combine_ret_types(RetTypes, EnclosingType) ->
    Combine = case EnclosingType of
		  tuple  -> fun proper_types:tuple/1;
		  union  -> fun proper_types:union/1;
		  wunion -> fun proper_types:wunion/1
	      end,
    case lists:all(fun is_simple_ret_type/1, RetTypes) of
	true ->
	    FinTypes = [T || {simple,T} <- RetTypes],
	    {simple, Combine(FinTypes)};
	false ->
	    NumTypes = erlang:max(1, length(RetTypes)),
	    {RevRecFuns,RevRecArgsList,NumRecs} =
		lists:foldl(fun add_ret_type/2, {[],[],0}, RetTypes),
	    RecFuns = lists:reverse(RevRecFuns),
	    RecArgsList = lists:reverse(RevRecArgsList),
	    RecArgLens = [length(RecArgs) || RecArgs <- RecArgsList],
	    FlatRecArgs = lists:flatten(RecArgsList),
	    NewRecArgs =
		case EnclosingType of
		    tuple  -> FlatRecArgs;
		    union  -> clean_rec_args(FlatRecArgs);
		    wunion -> clean_rec_args(FlatRecArgs)
		end,
	    NewRecFun =
		case EnclosingType of
		    tuple ->
			fun(AllGFs,TopSize) ->
			    Size = TopSize div NumRecs,
			    GFsList = proper_arith:unflatten(AllGFs,RecArgLens),
			    ArgsList = [[GenFuns,Size] || GenFuns <- GFsList],
			    ZipFun = fun(F,A) -> apply(F,A) end,
			    Combine(lists:zipwith(ZipFun, RecFuns, ArgsList))
			end;
		    union ->
			fun(AllGFs,Size) ->
			    GFsList = proper_arith:unflatten(AllGFs,RecArgLens),
			    ArgsList = [[GenFuns,Size] || GenFuns <- GFsList],
			    ZipFun = fun(F,A) -> ?LAZY(apply(F,A)) end,
			    Combine(lists:zipwith(ZipFun, RecFuns, ArgsList))
			end;
		    wunion ->
			fun(AllGFs,Size) ->
			    GFsList = proper_arith:unflatten(AllGFs,RecArgLens),
			    ArgsList = [[GenFuns,Size] || GenFuns <- GFsList],
			    ZipFun = fun(W,F,A) -> {W,?LAZY(apply(F,A))} end,
			    RecWeight = Size div (NumTypes - 1) + 1,
			    Weights =
				[1 | lists:duplicate(NumTypes - 1, RecWeight)],
			    Combine(lists:zipwith3(ZipFun, Weights, RecFuns,
						    ArgsList))
			end
		end,
	    {rec, NewRecFun, NewRecArgs}
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
    SameRecArg = fun({{_B,T1},{_B,T2}}) -> same_full_type_ref(T1,T2);
		    (_)                 -> false
		 end,
    NumRecArgs = length(RecArgs1),
    length(RecArgs2) =:= NumRecArgs
    andalso lists:all(SameRecArg, lists:zip(RecArgs1,RecArgs2))
    andalso same_rec_fun(RecFun1, RecFun2, NumRecArgs);
same_ret_type(_, _) ->
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

-spec same_rec_fun(rec_fun(), rec_fun(), non_neg_integer()) -> boolean().
same_rec_fun(RecFun1, RecFun2, NumRecArgs) ->
    %% It's ok that we return a type, even if there's a 'true' for use of
    %% an instance.
    GenFun = fun(_Size) -> proper_types:exactly('$dummy') end,
    GenFuns = lists:duplicate(NumRecArgs,GenFun),
    same_fin_type(RecFun1(GenFuns,0), RecFun2(GenFuns,0)).
