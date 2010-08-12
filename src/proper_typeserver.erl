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

-export([start/0, stop/0, translate_type/1]).

-export_type([imm_type/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type module_name() :: atom().
-type tagged_result(T) :: {'ok',T} | 'error'.
-type rich_result(T) :: {'ok',T} | {'error',term()}.
-type type_tag() :: 'any' | 'atom' | 'bitstring' | 'function' | 'nil' | 'list'
		  | 'tuple' | 'float' | 'integer' | 'number' | 'opaque'
		  | 'unsupported'.

%% TODO: Fill in the type for abstract syntax expressions.
%% -type module_types() :: dict({'record' | 'type' | 'opaque',atom()},
%%				[{arity(),[{atom(),_}]},...]
%%				| {module_name(),_,[atom() | integer()]}).
-type imm_type() :: {module_name(),string()}.
-record(state,
	{cached = dict:new() :: dict(), %% dict(imm_type(),proper_types:type())
	 exported = sets:new() :: set(), %% set(mfa())
	 types = dict:new() :: dict()}). %% dict(module_name(),module_types())
	 %% this is a module-keyd dict of {record|type|opaque,Name}-keyd dicts
	 %% of [{Arity, Fields}] for records
	 %% or {Module, TypeForm, ArgNames} for others
-type state() :: #state{}.


%%------------------------------------------------------------------------------
%% Server interface functions
%%------------------------------------------------------------------------------

-spec start() -> 'ok'.
start() ->
    ServerPid = spawn_link(fun init/0),
    true = register(proper_typeserver, ServerPid),
    ok.

-spec stop() -> 'ok'.
stop() ->
    ServerPid = whereis(proper_typeserver),
    unregister(proper_typeserver),
    ServerPid ! stop,
    ok.

-spec translate_type(imm_type()) -> rich_result(proper_types:type()).
translate_type(ImmType) ->
    proper_typeserver ! {self(),translate_type,ImmType},
    receive
	Result -> Result
    end.

-spec init() -> no_return().
init() ->
    loop(#state{}).

-spec loop(#state{}) -> no_return().
loop(State) ->
    receive
	stop ->
	    exit(normal);
	{From,translate_type,ImmType} ->
	    {Result,NewState} =
		case translate_type(ImmType, State) of
		    {ok,Type,NextState}     -> {{ok,Type},NextState};
		    {error,_Reason} = Error -> {Error,State}
		end,
	    From ! Result,
	    loop(NewState)
    end.


%%------------------------------------------------------------------------------
%% Type translation functions
%%------------------------------------------------------------------------------

-spec translate_type(imm_type(), state()) ->
	  {'ok',proper_types:type(),state()} | {'error',term()}.
translate_type({Mod,Str} = ImmType, #state{cached = Cached} = State) ->
    case dict:find(ImmType, Cached) of
	{ok,Type} ->
	    {ok,Type,State};
	error ->
	    case parse_type(Str) of
		{ok,TypeForm} ->
		    case add_module(Mod, State) of
			{ok,NewState} ->
			    try_convert(Mod, TypeForm, NewState);
			{error,_Reason} = Error ->
			    Error
		    end;
		{error,_Reason} = Error ->
		    Error
	    end
    end.

-spec add_module(module_name(), state()) -> rich_result(state()).
add_module(Mod, #state{exported = ExpTypes, types = TypesDict} = State) ->
    case dict:is_key(Mod, TypesDict) of
	true ->
	    {ok,State};
	false ->
	    case get_module_code(Mod) of
		{ok,AbsCode} ->
		    case dialyzer_utils:get_record_and_type_info(AbsCode) of
			{ok,ModTypes} ->
			    MoreExpTypes =
				get_exported_types_from_abs(Mod, AbsCode),
			    NewExpTypes = sets:union(ExpTypes, MoreExpTypes),
			    NewTypesDict = dict:store(Mod, ModTypes, TypesDict),
			    {ok,State#state{exported = NewExpTypes,
					    types = NewTypesDict}};
			{error,_Msg} = Error ->
			    Error
		    end;
		{error,_Reason} = Error ->
		    Error
	    end
    end.

%% TODO: Fill in the type for abstract syntax forms.
-spec try_convert(module_name(), _, state()) ->
	  {'ok',proper_types:type(),state()} | {'error',term()}.
try_convert(Mod, TypeForm,
	    #state{exported = ExpTypes, types = TypesDict} = State) ->
    ModTypes = dict:fetch(Mod, TypesDict),
    try
	ImmErlType = erl_types:t_from_form(TypeForm, ModTypes),
	erl_types:t_solve_remote(ImmErlType, ExpTypes, TypesDict)
    of
	ErlType ->
	    case collect_needed_modules() of
		[]      -> case convert_type(ErlType) of
			       {ok,Type} ->
				   {ok,Type,State};
			       error ->
				   {error,cant_convert}
			   end;
		Modules -> case add_modules(Modules, State) of
			       {ok,NewState} ->
				   try_convert(Mod, TypeForm, NewState);
			       {error,_Reason} = Error ->
				   Error
			   end
	    end
    catch
	throw:({error,_Msg} = Error) -> Error
    end.

-spec collect_needed_modules() -> [module_name()].
collect_needed_modules() ->
    collect_needed_modules([]).

-spec collect_needed_modules([module_name()]) -> [module_name()].
collect_needed_modules(Modules) ->
    receive
	{_Self,ext_types,{Mod,_Name,_ArgsLen}} ->
	    collect_needed_modules([Mod | Modules])
    after 0 ->
	lists:usort(Modules)
    end.

-spec add_modules([module_name()], state()) -> rich_result(state()).
add_modules([], State) ->
    {ok,State};
add_modules([Mod | Rest], #state{types = TypesDict} = State) ->
    case dict:is_key(Mod, TypesDict) of
	true ->
	    {error,remote_type_not_exported};
	false ->
	    case add_module(Mod, State) of
		{ok,NewState} ->
		    add_modules(Rest, NewState);
		{error,_Reason} = Error ->
		    Error
	    end
    end.


%%------------------------------------------------------------------------------
%% Parsing functions
%%------------------------------------------------------------------------------

%% TODO: Fill in the type for abstract syntax forms.
-spec parse_type(string()) -> rich_result(_).
parse_type(Str) ->
    TypeStr = "-type mytype() :: " ++ Str ++ ".",
    case erl_scan:string(TypeStr) of
	{ok,Tokens,_EndLocation} ->
	    case erl_parse:parse_form(Tokens) of
		{ok,{attribute,_,type,{mytype,TypeForm,[]}}} ->
		    {ok,TypeForm};
		{error,_ErrorInfo} = Error ->
		    Error
	    end;
	{error,ErrorInfo,_EndLocation} ->
	    {error,ErrorInfo}
    end.

%% TODO: Fill in the type for abstract syntax forms.
-spec get_module_code(module_name()) -> rich_result([_]).
get_module_code(Mod) ->
    case code:which(Mod) of
	ObjFileName when is_list(ObjFileName) ->
	    case beam_lib:chunks(ObjFileName, [abstract_code]) of
		{ok,{Mod,[{abstract_code,AbsCodeChunk}]}} ->
		    case AbsCodeChunk of
			{raw_abstract_v1,AbsCode} ->
			    {ok,AbsCode};
			no_abstract_code ->
			    {error,no_abstract_code};
			_ ->
			    {error,abstract_code_format}
		    end;
		{error,beam_lib,Reason} ->
		    {error,Reason}
	    end;
	ErrAtom when is_atom(ErrAtom) ->
	    {error,ErrAtom}
    end.

%% TODO: Fill in the type for abstract syntax forms.
-spec get_exported_types_from_abs(module_name(), [_]) -> set(). %% set(mfa())
get_exported_types_from_abs(Mod, AbsCode) ->
    lists:foldl(fun(Form, Acc) -> add_exported_types(Mod, Form, Acc) end,
		sets:new(), AbsCode).

%% TODO: Fill in the type for abstract syntax forms.
-spec add_exported_types(module_name(), _, set()) -> set(). %% set(mfa())
add_exported_types(Mod, {attribute,_,export_type,TypesList}, ExpTypes) ->
    MoreExpTypes = sets:from_list([{Mod,T,A} || {T,A} <- TypesList]),
    sets:union(MoreExpTypes, ExpTypes);
add_exported_types(_Mod, _Form, ExpTypes) ->
    ExpTypes.


%%------------------------------------------------------------------------------
%% Type conversion functions
%%------------------------------------------------------------------------------

-spec convert_type(erl_types:erl_type()) -> tagged_result(proper_types:type()).
convert_type(ErlType) ->
    Elems = erl_types:t_elements(ErlType),
    DirtyConvElems = [convert_simple_type(T) || T <- Elems],
    case clean_list(DirtyConvElems) of
	error       -> error;
	{ok,[]}     -> error;
	{ok,[Type]} -> {ok,Type};
	{ok,Types}  -> {ok,proper_types:union(Types)}
    end.

-spec convert_simple_type(erl_types:erl_type()) ->
	  tagged_result(proper_types:type()).
convert_simple_type(ErlType) ->
    TypeTag = get_type_tag(ErlType),
    convert_simple_type(ErlType, TypeTag).

-spec get_type_tag(erl_types:erl_type()) -> type_tag().
get_type_tag(ErlType) ->
    TestsAndTags = [{fun erl_types:t_is_any/1, any},
		    {fun erl_types:t_is_atom/1, atom},
		    {fun erl_types:t_is_bitstr/1, bitstring},
		    {fun erl_types:t_is_fun/1, function},
		    %% The order here is important: first test for [], then for
		    %% any list.
		    {fun erl_types:t_is_nil/1, nil},
		    {fun erl_types:t_is_list/1, list},
		    {fun erl_types:t_is_tuple/1, tuple},
		    %% The order here is important: first test for specifically
		    %% float or integer, then for generally any number.
		    {fun erl_types:t_is_float/1, float},
		    {fun erl_types:t_is_integer/1, integer},
		    {fun erl_types:t_is_number/1, number},
		    {fun erl_types:t_is_opaque/1, opaque}],
    NotThisType = fun({TypeTest,_}) -> not TypeTest(ErlType) end,
    case lists:dropwhile(NotThisType, TestsAndTags) of
	[] ->
	    unsupported;
	[{_TypeTest,Tag} | _Rest] ->
	    Tag
    end.

-spec convert_simple_type(erl_types:erl_type(), type_tag()) ->
	  tagged_result(proper_types:type()).
convert_simple_type(_T, any) ->
    {ok,proper_types:any()};
convert_simple_type(T, atom) ->
    case erl_types:t_atom_vals(T) of
	unknown                   -> {ok,proper_types:atom()};
	[Atom] when is_atom(Atom) -> {ok,proper_types:exactly(Atom)}
    end;
convert_simple_type(T, bitstring) ->
    %% TODO: bitstrings with unit size = 0?
    case {erl_types:t_bitstr_unit(T),erl_types:t_bitstr_base(T)} of
	{0,0}   -> {ok,proper_types:binary()};
	{0,Len} -> {ok,proper_types:binary(Len)};
	{8,0}   -> {ok,proper_types:binary()};
	{8,Len} -> {ok,proper_types:binary(Len)};
	{1,0}   -> {ok,proper_types:bitstring()};
	{1,Len} -> {ok,proper_types:bitstring(Len)};
	_       -> error
    end;
convert_simple_type(T, function) ->
    case erl_types:t_fun_arity(T) of
	unknown ->
	    error;
	Arity when is_integer(Arity), Arity >= 0 ->
	    case convert_type(erl_types:t_fun_range(T)) of
		error        -> error;
		{ok,RetType} -> {ok,proper_types:function(Arity, RetType)}
	    end
    end;
convert_simple_type(_T, nil) ->
    %% TODO: or proper_types:fixed_list([])?
    {ok,proper_types:exactly([])};
convert_simple_type(T, list) ->
    case convert_type(erl_types:t_list_elements(T)) of
	error ->
	    error;
	{ok,ElemType} ->
	    BaseType = proper_types:list(ElemType),
	    case erl_types:t_is_cons(T) of
		true  -> {ok,proper_types:non_empty(BaseType)};
		false -> {ok,BaseType}
	    end
    end;
convert_simple_type(T, tuple) ->
    case erl_types:t_tuple_subtypes(T) of
	unknown ->
	    {ok,proper_types:tuple()};
	[T] ->
	    Fields = erl_types:t_tuple_args(T),
	    DirtyFieldTypes = [convert_type(Field) || Field <- Fields],
	    case clean_list(DirtyFieldTypes) of
		error           -> error;
		{ok,FieldTypes} -> {ok,proper_types:tuple(FieldTypes)}
	    end
    end;
convert_simple_type(_T, float) ->
    {ok,proper_types:float()};
convert_simple_type(T, integer) ->
    case erl_types:t_number_vals(T) of
	unknown ->
	    Low = proper_int_limit(erl_types:number_min(T)),
	    High = proper_int_limit(erl_types:number_max(T)),
	    {ok,proper_types:integer(Low, High)};
	[I] when is_integer(I) ->
	    {ok,proper_types:exactly(I)}
    end;
convert_simple_type(_T, number) ->
    {ok,proper_types:number()};
convert_simple_type(T, opaque) ->
    convert_type(erl_types:t_unopaque(T));
convert_simple_type(_T, unsupported) ->
    error.

-spec clean_list([{'ok',T} | 'error']) -> {'ok',[T]} | 'error'.
clean_list(DirtyList) ->
    clean_list(DirtyList, []).

-spec clean_list([{'ok',T} | 'error'], [T]) -> {'ok',[T]} | 'error'.
clean_list([], Acc) ->
    {ok,lists:reverse(Acc)};
clean_list([error | _Rest], _Acc) ->
    error;
clean_list([{ok,X} | Rest], Acc) ->
    clean_list(Rest, [X|Acc]).

-spec proper_int_limit('neg_inf' | 'pos_inf' | integer()) ->
	  proper_arith:extint().
proper_int_limit(neg_inf) -> inf;
proper_int_limit(pos_inf) -> inf;
proper_int_limit(X) when is_integer(X) -> X.
