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
%%% @doc The generator subsystem and generators for basic types are contained in
%%%	 this module.

-module(proper_gen).
-export([pick/1, pick/2]).

-export([gen_state_get/0, gen_state_set/1, gen_state_erase/0]).
-export([safe_generate/1]).
-export([normal_gen/1, alt_gens/1, clean_instance/1, get_ret_type/2,
	 function_body/3]).
-export([integer_gen/3, float_gen/3, atom_gen/1, atom_rev/1, binary_gen/1,
	 binary_str_gen/1, binary_rev/1, binary_len_gen/1, binary_len_str_gen/1,
	 bitstring_gen/1, bitstring_rev/1, bitstring_len_gen/1, list_gen/2,
	 vector_gen/2, union_gen/1, weighted_union_gen/1, tuple_gen/1,
	 loose_tuple_gen/2, loose_tuple_rev/2, exactly_gen/1, fixed_list_gen/1,
	 function_gen/2, any_gen/1]).

-export_type([instance/0, imm_instance/0, sized_generator/0, nosize_generator/0,
	      generator/0, straight_gen/0, reverse_gen/0, combine_fun/0,
	      alt_gens/0, gen_state/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type instance() :: term().
%% TODO: update imm_instance() when adding more types: be careful when reading
%%	 anything that returns it
-type imm_instance() :: proper_types:raw_type()
		      | instance()
		      | {'$used', imm_instance(), imm_instance()}
		      | {'$to_part', pos_integer(), pos_integer(),
			 imm_instance()}.

-type sized_generator() :: fun((size()) -> imm_instance()).
-type nosize_generator() :: fun(() -> imm_instance()).
-type generator() :: sized_generator() | nosize_generator().
-type sized_straight_gen() :: fun((size()) -> {'ok',instance()} | 'error').
-type nosize_straight_gen() :: fun(() -> {'ok',instance()} | 'error').
-type straight_gen() :: sized_straight_gen() | nosize_straight_gen().
-type reverse_gen() :: fun((instance()) -> imm_instance()).
-type combine_fun() :: fun((instance()) -> imm_instance()).
-type alt_gens() :: fun(() -> [imm_instance()]).
-type fun_num() :: pos_integer().

%% TODO: fill in the type of abstract format clauses
-opaque gen_state() :: {_, fun_num()}.


%%------------------------------------------------------------------------------
%% State handling functions
%%------------------------------------------------------------------------------

%% TODO: fill in the type of abstract format clauses
-spec get_forms() -> _.
get_forms() ->
    get('$forms').

%% TODO: fill in the type of abstract format clauses
-spec set_forms(_) -> 'ok'.
set_forms(Forms) ->
    put('$forms', Forms),
    ok.

-spec get_next_fun_num() -> fun_num().
get_next_fun_num() ->
    FunNum = case get('$next_fun_num') of
		 undefined -> 1;
		 N         -> N
	     end,
    put('$next_fun_num', FunNum + 1),
    FunNum.

-spec gen_state_get() -> gen_state().
gen_state_get() ->
    {get('$forms'), get('$next_fun_num')}.

-spec gen_state_set(gen_state()) -> 'ok'.
gen_state_set({Forms, NextFunNum}) ->
    put('$forms', Forms),
    put('$next_fun_num', NextFunNum),
    case Forms of
	undefined ->
	    ok;
	_  ->
	    load_forms()
    end.

-spec gen_state_erase() -> 'ok'.
gen_state_erase() ->
    erase('$forms'),
    erase('$next_fun_num'),
    _ = code:purge('$temp_mod'),
    _ = code:delete('$temp_mod'),
    _ = code:purge('$temp_mod'),
    ok.

-spec load_forms() -> 'ok'.
load_forms() ->
    %% TODO: verbose and report options?
    {ok,'$temp_mod',Code} = compile:forms(get('$forms'), [export_all]),
    {module,_Mod} = code:load_binary('$temp_mod', "no_file", Code),
    ok.


%%------------------------------------------------------------------------------
%% Instance generation functions
%%------------------------------------------------------------------------------

-spec safe_generate(proper_types:raw_type()) -> {'ok',imm_instance()} | 'error'.
safe_generate(RawType) ->
    try generate(RawType) of
	ImmInstance -> {ok,ImmInstance}
    catch
	throw:'$cant_generate' -> error
    end.

-spec generate(proper_types:raw_type()) -> imm_instance().
generate(RawType) ->
    Type = proper_types:cook_outer(RawType),
    generate(Type, get('$constraint_tries'), none).

-spec generate(proper_types:type(), non_neg_integer(),
	       'none' | {'ok',imm_instance()}) -> imm_instance().
generate(_Type, 0, none) ->
    throw('$cant_generate');
generate(_Type, 0, {ok,Fallback}) ->
    Fallback;
generate(Type, TriesLeft, Fallback) ->
    {Instance, Result} =
	case proper_types:get_prop(kind, Type) of
	    constructed ->
		PartsType = proper_types:get_prop(parts_type, Type),
		Combine = proper_types:get_prop(combine, Type),
		ImmParts = generate(PartsType),
		Parts = clean_instance(ImmParts),
		ImmInstance1 = Combine(Parts),
		ImmInstance2 =
		    case proper_types:is_raw_type(ImmInstance1) of
			true  -> generate(ImmInstance1);
			false -> ImmInstance1
		    end,
		{clean_instance(ImmInstance2),{'$used',ImmParts,ImmInstance2}};
	    Kind ->
		ImmInstance1 =
		    case Kind of
			%% TODO: should we have an option to enable this?
			wrapper -> normal_or_str_gen(Type);
			_       -> normal_gen(Type)
		    end,
		ImmInstance2 =
		    case proper_types:is_raw_type(ImmInstance1) of
			true  -> generate(ImmInstance1);
			false -> ImmInstance1
		    end,
		CleanInstance = clean_instance(ImmInstance2),
		ImmInstance3 =
		    case Kind of
			opaque -> CleanInstance;
			_      -> ImmInstance2
		    end,
		{CleanInstance,ImmInstance3}
	end,
    case proper_types:satisfies_all(Instance, Type) of
	{_,true}      -> Result;
	{true,false}  -> generate(Type, TriesLeft - 1, {ok,Result});
	{false,false} -> generate(Type, TriesLeft - 1, Fallback)
    end.

-spec pick(proper_types:raw_type()) -> {'ok',instance()} | 'error'.
pick(RawType) ->
    pick(RawType, 10).

-spec pick(proper_types:raw_type(), size()) -> {'ok',instance()} | 'error'.
pick(RawType, Size) ->
    proper:global_state_init_size(Size),
    Result = clean_instance(safe_generate(RawType)),
    case Result of
	error ->
	    io:format("Error: couldn't produce an instance that satisfies all "
		      "strict constraints after ~b tries~n",
		      [get('$constraint_tries')]),
	    proper:global_state_erase();
	{ok,FunInstance} when is_function(FunInstance) ->
	    %% TODO: This should also cover functions in tuples etc.
	    io:format("WARNING: Some garbage has been left in the process "
		      "registry and the code server to allow for the returned "
		      "function to run normally.~nPlease run "
		      "proper:global_state_erase() when done.~n", []);
	_ ->
	    proper:global_state_erase()
    end,
    Result.


%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

-spec normal_or_str_gen(proper_types:type()) -> imm_instance().
normal_or_str_gen(Type) ->
    case proper_types:find_prop(straight_gen,Type) of
	{ok,StraightGen} ->
	    case call_gen(StraightGen, Type) of
		{ok,Instance} ->
		    ReverseGen = proper_types:get_prop(reverse_gen, Type),
		    ReverseGen(Instance);
		error ->
		    normal_gen(Type)
	    end;
	error ->
	    normal_gen(Type)
    end.

-spec normal_gen(proper_types:type()) -> imm_instance().
normal_gen(Type) ->
    call_gen(proper_types:get_prop(generator,Type), Type).

-spec call_gen(generator() | straight_gen(), proper_types:type()) ->
	  imm_instance() | {'ok',instance()} | 'error'.
call_gen(Gen, Type) ->
    if
	is_function(Gen, 0) ->
	    Gen();
	is_function(Gen, 1) ->
	    case proper:get_size(Type) of
		undefined -> throw('$need_size_info');
		Size      -> Gen(Size)
	    end
    end.

-spec alt_gens(proper_types:type()) -> [imm_instance()].
alt_gens(Type) ->
    case proper_types:find_prop(alt_gens, Type) of
	{ok, AltGens} -> ?FORCE(AltGens);
	error         -> []
    end.

-spec clean_instance(imm_instance()) -> instance().
clean_instance({'$used',_ImmParts,ImmInstance}) ->
    clean_instance(ImmInstance);
clean_instance({'$to_part',_Part,_NumParts,ImmInstance}) ->
    clean_instance(ImmInstance);
clean_instance(ImmInstance) ->
    if
	is_list(ImmInstance) ->
	    %% CAUTION: this must handle improper lists
	    proper_arith:safemap(fun clean_instance/1, ImmInstance);
	is_tuple(ImmInstance) ->
	    proper_arith:tuplemap(fun clean_instance/1, ImmInstance);
	true ->
	    ImmInstance
    end.

-spec get_ret_type(function(), arity()) -> proper_types:type().
get_ret_type(Fun, Arity) ->
    put('$get_ret_type', true),
    RetType = apply(Fun, lists:duplicate(Arity,dummy)),
    erase('$get_ret_type'),
    RetType.

-spec function_body([term()], proper_types:type() | binary(),
		    {integer(),integer()}) ->
	  proper_types:type() | instance().
function_body(Args, ImmRetType, {Seed1,Seed2}) ->
    RetType = if
		  is_binary(ImmRetType) -> binary_to_term(ImmRetType);
		  true                  -> ImmRetType
	      end,
    case get('$get_ret_type') of
	true ->
	    RetType;
	_ ->
	    SavedSeed = get(random_seed),
	    put(random_seed, {Seed1,Seed2,erlang:phash2(Args,?SEED_RANGE)}),
	    Ret = clean_instance(generate(RetType)),
	    put(random_seed, SavedSeed),
	    Ret
    end.

%%------------------------------------------------------------------------------
%% Basic type generators
%%------------------------------------------------------------------------------

-spec integer_gen(size(), proper_arith:extint(), proper_arith:extint()) ->
	  integer().
integer_gen(Size, inf, inf) ->
    proper_arith:rand_int(Size);
integer_gen(Size, inf, High) ->
    High - proper_arith:rand_non_neg_int(Size);
integer_gen(Size, Low, inf) ->
    Low + proper_arith:rand_non_neg_int(Size);
integer_gen(_Size, Low, High) ->
    proper_arith:rand_int(Low, High).

-spec float_gen(size(), proper_arith:extnum(), proper_arith:extnum()) ->
	  float().
float_gen(Size, inf, inf) ->
    proper_arith:rand_float(Size);
float_gen(Size, inf, High) ->
    High - proper_arith:rand_non_neg_float(Size);
float_gen(Size, Low, inf) ->
    Low + proper_arith:rand_non_neg_float(Size);
float_gen(_Size, Low, High) ->
    proper_arith:rand_float(Low, High).

-spec atom_gen(size()) -> proper_types:type().
%% We make sure we never clash with internal atoms by checking that the first
%% character is not '$'.
atom_gen(Size) ->
    ?LET(Str,
	 ?SUCHTHAT(X,
		   proper_types:resize(Size,
				       proper_types:list(proper_types:byte())),
		   X =:= [] orelse hd(X) =/= $$),
	 list_to_atom(Str)).

-spec atom_rev(atom()) -> imm_instance().
atom_rev(Atom) ->
    {'$used', atom_to_list(Atom), Atom}.

-spec binary_gen(size()) -> proper_types:type().
binary_gen(Size) ->
    ?LET(Bytes,
	 proper_types:resize(Size,
			     proper_types:list(proper_types:byte())),
	 list_to_binary(Bytes)).

-spec binary_str_gen(size()) -> {'ok',binary()} | 'error'.
binary_str_gen(Size) ->
    Len = proper_arith:rand_int(0, Size),
    binary_len_str_gen(Len).

-spec binary_rev(binary()) -> imm_instance().
binary_rev(Binary) ->
    {'$used', binary_to_list(Binary), Binary}.

-spec binary_len_gen(length()) -> proper_types:type().
binary_len_gen(Len) ->
    ?LET(Bytes,
	 proper_types:vector(Len, proper_types:byte()),
	 list_to_binary(Bytes)).

-spec binary_len_str_gen(length()) -> {'ok',binary()} | 'error'.
binary_len_str_gen(Len) ->
    proper_arith:rand_bytes(Len).

-spec bitstring_gen(size()) -> proper_types:type().
bitstring_gen(Size) ->
    ?LET({BytesHead, NumBits, TailByte},
	 {proper_types:resize(Size,proper_types:binary()),
	  proper_types:range(0,7), proper_types:range(0,127)},
	 <<BytesHead/binary, TailByte:NumBits>>).

-spec bitstring_rev(bitstring()) -> imm_instance().
bitstring_rev(BitString) ->
    List = bitstring_to_list(BitString),
    {BytesList, BitsTail} = lists:splitwith(fun erlang:is_integer/1, List),
    {NumBits, TailByte} = case BitsTail of
			      []     -> {0, 0};
			      [Bits] -> N = bit_size(Bits),
					<<Byte:N>> = Bits,
					{N, Byte}
			  end,
    {'$used',
     {{'$used',BytesList,list_to_binary(BytesList)}, NumBits, TailByte},
     BitString}.

-spec bitstring_len_gen(length()) -> proper_types:type().
bitstring_len_gen(Len) ->
    BytesLen = Len div 8,
    BitsLen = Len rem 8,
    ?LET({BytesHead, NumBits, TailByte},
	 {proper_types:binary(BytesLen), BitsLen,
	  proper_types:range(0, 1 bsl BitsLen - 1)},
	  <<BytesHead/binary, TailByte:NumBits>>).

-spec list_gen(size(), proper_types:type()) -> [imm_instance()].
list_gen(Size, ElemType) ->
    Len = proper_arith:rand_int(0, Size),
    vector_gen(Len, ElemType).

-spec vector_gen(length(), proper_types:type()) -> [imm_instance()].
vector_gen(Len, ElemType) ->
    vector_gen_tr(Len, ElemType, []).

-spec vector_gen_tr(length(), proper_types:type(), [imm_instance()]) ->
	  [imm_instance()].
vector_gen_tr(0, _ElemType, AccList) ->
    AccList;
vector_gen_tr(Left, ElemType, AccList) ->
    vector_gen_tr(Left - 1, ElemType, [generate(ElemType) | AccList]).

-spec union_gen([proper_types:type()]) -> imm_instance().
union_gen(Choices) ->
    {_Choice,Type} = proper_arith:rand_choose(Choices),
    generate(Type).

-spec weighted_union_gen([{frequency(),proper_types:type()}]) -> imm_instance().
weighted_union_gen(FreqChoices) ->
    {_Choice,Type} = proper_arith:freq_choose(FreqChoices),
    generate(Type).

-spec tuple_gen([proper_types:type()]) -> tuple().
tuple_gen(Fields) ->
    list_to_tuple(fixed_list_gen(Fields)).

-spec loose_tuple_gen(size(), proper_types:type()) -> proper_types:type().
loose_tuple_gen(Size, ElemType) ->
    ?LET(L,
	 proper_types:resize(Size, proper_types:list(ElemType)),
	 list_to_tuple(L)).

-spec loose_tuple_rev(tuple(), proper_types:type()) -> imm_instance().
loose_tuple_rev(Tuple, ElemType) ->
    CleanList = tuple_to_list(Tuple),
    List = case proper_types:find_prop(reverse_gen, ElemType) of
	       {ok,ReverseGen} -> [ReverseGen(X) || X <- CleanList];
	       error           -> CleanList
	   end,
    {'$used', List, Tuple}.

-spec exactly_gen(T) -> T.
exactly_gen(X) ->
    X.

-spec fixed_list_gen([proper_types:type()]) -> imm_instance()
		  ; ({[proper_types:type()],proper_types:type()}) ->
	  maybe_improper_list(imm_instance(), imm_instance()).
fixed_list_gen({ProperHead,ImproperTail}) ->
    [generate(F) || F <- ProperHead] ++ generate(ImproperTail);
fixed_list_gen(ProperFields) ->
    [generate(F) || F <- ProperFields].

-spec function_gen(arity(), proper_types:type()) -> function().
function_gen(Arity, RetType) ->
    FunSeed = {proper_arith:rand_int(0,?SEED_RANGE - 1),
	       proper_arith:rand_int(0,?SEED_RANGE - 1)},
    case Arity of
	0 ->
	    fun() -> ?MODULE:function_body([], RetType, FunSeed) end;
	1 ->
	    fun(A) -> ?MODULE:function_body([A], RetType, FunSeed) end;
	2 ->
	    fun(A,B) -> ?MODULE:function_body([A,B], RetType, FunSeed) end;
	3 ->
	    fun(A,B,C) -> ?MODULE:function_body([A,B,C], RetType, FunSeed) end;
	4 ->
	    fun(A,B,C,D) ->
		?MODULE:function_body([A,B,C,D], RetType, FunSeed)
	    end;
	_ ->
	    OldForms = case get_forms() of
			   undefined -> [{attribute,0,module,'$temp_mod'}];
			   F         -> F
		       end,
	    {FunName,FunForm} = new_function(Arity, RetType, FunSeed),
	    Forms = OldForms ++ [FunForm],
	    set_forms(Forms),
	    load_forms(),
	    erlang:make_fun('$temp_mod', FunName, Arity)
    end.

%% TODO: fill in the type of abstract format clauses
-spec new_function(arity(), proper_types:type(), {integer(),integer()}) ->
	  {atom(), _}.
new_function(Arity, RetType, FunSeed) ->
    FunNum = get_next_fun_num(),
    FunName = list_to_atom("f" ++ integer_to_list(FunNum)),
    Args = [{var,0,list_to_atom("X" ++ integer_to_list(N))}
	    || N <- lists:seq(1, Arity)],
    ArgsList = lists:foldr(fun(X,Acc) -> {cons,0,X,Acc} end, {nil,0}, Args),
    Body = [{call, 0, {remote,0,{atom,0,?MODULE},{atom,0,function_body}},
	     [ArgsList,
	      erl_parse:abstract(term_to_binary(RetType)),
	      erl_parse:abstract(FunSeed)]}],
    {FunName, {function,0,FunName,Arity,[{clause,0,Args,[],Body}]}}.

-spec any_gen(size()) -> imm_instance().
any_gen(0) ->
    SimpleTypes = [proper_types:integer(), proper_types:float(),
		   proper_types:atom()],
    union_gen(SimpleTypes);
any_gen(Size) ->
    FreqChoices = [{?ANY_SIMPLE_PROB,simple}, {?ANY_BINARY_PROB,binary},
		   {?ANY_EXPAND_PROB,expand}],
    case proper_arith:freq_choose(FreqChoices) of
	{_,simple} ->
	    any_gen(0);
	{_,binary} ->
	    generate(proper_types:resize(Size, proper_types:bitstring()));
	{_,expand} ->
	    %% TODO: statistics of produced terms?
	    NumElems = proper_arith:rand_int(0, Size - 1),
	    ElemSizes = proper_arith:distribute(Size - 1, NumElems),
	    ElemTypes = [?LAZY(any_gen(S)) || S <- ElemSizes],
	    case proper_arith:rand_int(1,2) of
		1 -> fixed_list_gen(ElemTypes);
		2 -> tuple_gen(ElemTypes)
	    end
    end.
