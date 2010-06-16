%%% Copyright 2010 Manolis Papadakis (manopapad@gmail.com)
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
%%% @copyright 2010 Manolis Papadakis
%%% @version {@version}
%%% @doc Type manipulation functions and predefined types are contained in this
%%%	module.

-module(proper_types).
-export([cook_outer/1, is_raw_type/1, get_prop/2, find_prop/2, new_type/2,
	 subtype/2, is_instance/2, unwrap/1, weakly/1, strongly/1,
	 satisfies_all/2]).
-export([lazy/1, sized/1, bind/2, shrinkwith/2, add_constraint/3]).
-export([integer/2, float/2, atom/0, binary/0, bitstring/0, list/1, vector/2,
	 union/1, weighted_union/1, tuple/1, exactly/1, fixed_list/1]).
-export([integer/0, non_neg_integer/0, pos_integer/0, neg_integer/0, range/2,
	 float/0, non_neg_float/0, number/0, boolean/0, byte/0, char/0,
	 string/0, wunion/1]).
-export([int/0, int/2, bool/0, choose/2, elements/1, oneof/1, frequency/1]).
-export([resize/2, relimit/2, non_empty/1]).

-export_type([type/0, raw_type/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type type_kind() :: 'basic' | 'wrapper' | 'constructed'
		   | 'semi_opaque' | 'opaque'.
-type instance_test() :: fun((proper_gen:imm_instance()) ->
			     proper_arith:ternary()).
-type index() :: term().
-type value() :: term().
-type constraint_fun() :: fun((proper_gen:instance()) -> boolean()).

-type type() :: {'$type', [type_prop()]}.
%% TODO: update raw_type() when adding more standard types
-type raw_type() :: type() | integer() | float() | atom() | tuple()
		  | maybe_improper_list(_,_) | <<_:_ * 1>>.
-type type_prop_name() :: 'kind' | 'generator' | 'straight_gen' | 'reverse_gen'
			| 'size_limit' | 'size_transform' | 'is_instance'
			| 'shrinkers' | 'internal_type' | 'internal_types'
			| 'get_length' | 'split' | 'join' | 'get_indices'
			| 'remove' | 'retrieve' | 'update' | 'constraints'
			| 'parts_type' | 'combine' | 'alt_gens'.
-type type_prop_value() :: term().
-type type_prop() ::
      {'kind', type_kind()}
    | {'generator', proper_gen:generator()}
    | {'straight_gen', proper_gen:straight_gen()}
    | {'reverse_gen', proper_gen:reverse_gen()}
    | {'size_limit', size()}
    | {'size_transform', fun((size()) -> size())}
    | {'is_instance', instance_test()}
    | {'shrinkers', [proper_shrink:shrinker()]}
    | {'internal_type', raw_type()}
    | {'internal_types', tuple(type()) | maybe_improper_list(type(),term())}
      %% The items returned by 'remove' must be of this type.
    | {'get_length', fun((proper_gen:imm_instance()) -> length())}
      %% If this is a container type, this should return the number of elements
      %% it contains.
    | {'split', fun((proper_gen:imm_instance()) -> [proper_gen:imm_instance()])
	      | fun((length(),proper_gen:imm_instance()) ->
		    {proper_gen:imm_instance(),proper_gen:imm_instance()})}
      %% If present, the appropriate form depends on whether get_length is
      %% defined: if get_length is undefined, this must be in the one-argument
      %% form (e.g. a tree should be split into its subtrees), else it must be
      %% in the two-argument form (e.g. a list should be split in two at the
      %% index provided).
    | {'join', fun((proper_gen:imm_instance(),proper_gen:imm_instance()) ->
		   proper_gen:imm_instance())}
    | {'get_indices', fun((proper_gen:imm_instance()) -> [index()])}
      %% If this is a container type, this should return a list of indices we
      %% can use to remove or insert elements from the given instance.
    | {'remove', fun((index(),proper_gen:imm_instance()) ->
		     proper_gen:imm_instance())}
    | {'retrieve', fun((index(),proper_gen:imm_instance()) -> value())}
    | {'update', fun((index(),value(),proper_gen:imm_instance()) ->
		     proper_gen:imm_instance())}
    | {'constraints', [{constraint_fun(), boolean()}]}
      %% A list of constraints on instances of this type: each constraint is a
      %% tuple of a fun that must return 'true' for each valid instance and a
      %% boolean field that specifies whether the condition is strict.
    | {'parts_type', type()}
    | {'combine', proper_gen:combine_fun()}
    | {'alt_gens', proper_gen:alt_gens()}.


%%------------------------------------------------------------------------------
%% Type manipulation functions
%%------------------------------------------------------------------------------

-spec cook_outer(raw_type()) -> type().
cook_outer(Type = {'$type',_Props}) ->
    Type;
cook_outer(RawType) ->
    if
	is_tuple(RawType) -> tuple(tuple_to_list(RawType));
	%% CAUTION: this must handle improper lists
	is_list(RawType)  -> fixed_list(RawType);
	%% default case (covers integers, floats, atoms, binaries, ...):
	true              -> exactly(RawType)
    end.

-spec is_raw_type(term()) -> boolean().
is_raw_type({'$type',_TypeProps}) ->
    true;
is_raw_type(X) ->
    if
	is_tuple(X) -> is_raw_type_list(tuple_to_list(X));
	is_list(X)  -> is_raw_type_list(X);
	true        -> false
    end.

-spec is_raw_type_list(term()) -> boolean().
%% CAUTION: this must handle improper lists
is_raw_type_list([]) ->
    false;
is_raw_type_list([X | Rest]) ->
    is_raw_type(X) orelse is_raw_type_list(Rest);
is_raw_type_list(X) ->
    is_raw_type(X).

-spec type_from_list([type_prop()]) -> type().
type_from_list(KeyValueList) ->
    {'$type',orddict:from_list(KeyValueList)}.

-spec add_prop(type_prop_name(), type_prop_value(), type()) -> type().
add_prop(PropName, Value, {'$type',Props}) ->
    {'$type',orddict:store(PropName, Value, Props)}.

-spec add_props([type_prop()], type()) -> type().
add_props(PropList, {'$type',OldProps}) ->
    {'$type', lists:foldl(fun({N,V},Acc) -> orddict:store(N, V, Acc) end,
			  OldProps, PropList)}.

-spec append_to_prop(type_prop_name(), type_prop_value(), type()) -> type().
append_to_prop(PropName, Value, {'$type',Props}) ->
    {'$type',orddict:append(PropName, Value, Props)}.

-spec get_prop(type_prop_name(), type()) -> type_prop_value().
get_prop(PropName, {'$type',Props}) ->
    orddict:fetch(PropName, Props).

-spec find_prop(type_prop_name(), type()) -> {'ok',type_prop_value()} | 'error'.
find_prop(PropName, {'$type',Props}) ->
    orddict:find(PropName, Props).

-spec new_type([type_prop()], type_kind()) -> type().
new_type(PropList, Kind) ->
    Type = type_from_list(PropList),
    add_prop(kind, Kind, Type).

-spec subtype([type_prop()], type()) -> type().
%% TODO: should the 'is_instance' function etc. be reset for subtypes?
subtype(PropList, Type) ->
    add_props(PropList, Type).

-spec is_instance(proper_gen:imm_instance(), type()) -> proper_arith:ternary().
is_instance(ImmInstance, Type) ->
    CleanInstance = proper_gen:clean_instance(ImmInstance),
    proper_arith:and3(
	weakly(satisfies_all(CleanInstance, Type)),
	proper_arith:or3(
	    case find_prop(is_instance, Type) of
		{ok, IsInstance} -> IsInstance(ImmInstance);
		error            -> unknown
	    end,
	    case get_prop(kind, Type) of
		wrapper     -> wrapper_test(ImmInstance, Type);
		constructed -> constructed_test(ImmInstance, Type);
		_           -> unknown
	    end
	)
    ).

-spec wrapper_test(proper_gen:imm_instance(), type()) -> proper_arith:ternary().
wrapper_test(ImmInstance, Type) ->
    proper_arith:any3([is_instance(ImmInstance, T) || T <- unwrap(Type)]).

-spec unwrap(type()) -> [type()].
%% TODO: check if it's actually a raw type that's returned?
unwrap(Type) ->
    [cook_outer(InnerType) || InnerType <- proper_gen:alt_gens(Type)
					   ++ [proper_gen:normal_gen(Type)]].

-spec constructed_test(proper_gen:imm_instance(), type()) ->
	  proper_arith:ternary().
constructed_test({'$used',ImmParts,ImmInstance}, Type) ->
    PartsType = get_prop(parts_type, Type),
    Combine = get_prop(combine, Type),
    case is_instance(ImmParts, PartsType) of
	true ->
	    %% TODO: check if it's actually a raw type that's returned?
	    %% TODO: move construction code to proper_gen
	    %% TODO: something less strict when an exactly is produced?
	    RawInnerType = Combine(proper_gen:clean_instance(ImmParts)),
	    InnerType = cook_outer(RawInnerType),
	    is_instance(ImmInstance, InnerType);
	Other ->
	    Other
    end;
constructed_test(_ImmInstance, _Type) ->
    %% TODO: can we do anything better?
    unknown.

-spec weakly({boolean(),boolean()}) -> boolean().
weakly({B1,_B2}) -> B1.

-spec strongly({boolean(),boolean()}) -> boolean().
strongly({_B1,B2}) -> B2.

-spec satisfies(proper_gen:instance(), {constraint_fun(),boolean()})
	  -> {boolean(),boolean()}.
satisfies(Instance, {Test,false}) ->
    {true,Test(Instance)};
satisfies(Instance, {Test,true}) ->
    Result = Test(Instance),
    {Result,Result}.

-spec satisfies_all(proper_gen:instance(), type()) -> {boolean(),boolean()}.
satisfies_all(Instance, Type) ->
    case find_prop(constraints, Type) of
	{ok, Constraints} ->
	    L = [satisfies(Instance, C) || C <- Constraints],
	    {L1,L2} = lists:unzip(L),
	    {proper_arith:all3(L1), proper_arith:all3(L2)};
	error ->
	    {true,true}
    end.


%%------------------------------------------------------------------------------
%% Type definition functions
%%------------------------------------------------------------------------------

-spec lazy(proper_gen:nosize_generator()) -> type().
lazy(Gen) ->
    ?WRAPPER([
	{generator, Gen}
    ]).

-spec sized(proper_gen:sized_generator()) -> type().
sized(Gen) ->
    ?WRAPPER([
	{generator, Gen}
    ]).

-spec bind(raw_type(), proper_gen:combine_fun()) -> type().
bind(RawPartsType, Combine) ->
    PartsType = cook_outer(RawPartsType),
    ?CONSTRUCTED([
	{parts_type, PartsType},
	{combine, Combine}
    ]).

-spec shrinkwith(proper_gen:nosize_generator(), proper_gen:alt_gens()) ->
	  type().
shrinkwith(Gen, DelaydAltGens) ->
    ?WRAPPER([
	{generator, Gen},
	{alt_gens, DelaydAltGens}
    ]).

-spec add_constraint(raw_type(), constraint_fun(), boolean()) -> type().
add_constraint(RawType, Condition, IsStrict) ->
    Type = cook_outer(RawType),
    append_to_prop(constraints, {Condition,IsStrict}, Type).


%%------------------------------------------------------------------------------
%% Basic types
%%------------------------------------------------------------------------------

%% TODO: bin types: specified length, base size, unit size?
%% TODO: fun (generally some fun, unspecified number of arguments, but specified
%%	 return type, specific number and types of arguments and specific return
%%	 type) ("function" keyword?)
%% TODO: pid, port, ref (it's dangerous to provide random process data to
%%	 functions - they must want it for a reason (least we can do is have a
%%	 live function with that pid))
%% TODO: any (union of all types? what are those?
%%	 number, atom, reference, fun, port, pid, tuple, list, bit string
%% TODO: (records, none, improper_list(content_type, termination_type),
%%	 maybe_improper_list)
-spec integer(proper_arith:extint(), proper_arith:extint()) -> type().
integer(Low, High) ->
    ?BASIC([
	{generator, fun(Size) -> proper_gen:integer_gen(Size, Low, High) end},
	{size_transform, fun(Size) -> Size div 3 end},
	{is_instance, fun(X) -> integer_test(X, Low, High) end},
	{shrinkers,
	 [fun(X,_T,S) -> proper_shrink:integer_shrinker(X, Low, High, S) end]}
    ]).

-spec integer_test(proper_gen:imm_instance(), proper_arith:extint(),
		   proper_arith:extint()) -> boolean().
integer_test(X, Low, High) ->
    is_integer(X)
    andalso proper_arith:le(Low, X)
    andalso proper_arith:le(X, High).

-spec float(proper_arith:extnum(), proper_arith:extnum()) -> type().
float(Low, High) ->
    ?BASIC([
	{generator, fun(Size) -> proper_gen:float_gen(Size, Low, High) end},
	{size_transform, fun(Size) -> Size div 3 end},
	{is_instance, fun(X) -> float_test(X, Low, High) end},
	{shrinkers,
	 [fun(X,_T,S) -> proper_shrink:float_shrinker(X, Low, High, S) end]}
    ]).

-spec float_test(proper_gen:imm_instance(), proper_arith:extnum(),
		 proper_arith:extnum()) -> boolean().
float_test(X, Low, High) ->
    is_float(X)
    andalso proper_arith:le(Low, X)
    andalso proper_arith:le(X, High).

-spec atom() -> type().
atom() ->
    ?WRAPPER([
	{generator, fun proper_gen:atom_gen/1},
	{reverse_gen, fun proper_gen:atom_rev/1},
	{size_limit, ?MAX_ATOM_LEN},
	{is_instance, fun atom_test/1}
    ]).

-spec atom_test(proper_gen:imm_instance()) -> boolean().
atom_test(X) ->
    is_atom(X)
    %% We return false for atoms starting with '$', since these are
    %% atoms used internally and never produced by the atom generator.
    andalso (X =:= '' orelse hd(atom_to_list(X)) =/= $$).

-spec binary() -> type().
binary() ->
    %% TODO: this is very hard for the compiler to optimize, even though the
    %%       '$crypto' flag is never changed while running
    StraightGenTail =
	case get('$crypto') of
	    true -> [{straight_gen, fun proper_gen:binary_str_gen/1}];
	    _    -> []
	end,
    ?WRAPPER([
	{generator, fun proper_gen:binary_gen/1},
	{reverse_gen, fun proper_gen:binary_rev/1},
	{size_limit, ?MAX_BINARY_LEN},
	{is_instance, fun erlang:is_binary/1}
	| StraightGenTail
    ]).

-spec bitstring() -> type().
bitstring() ->
    ?WRAPPER([
	{generator, fun proper_gen:bitstring_gen/0},
	{reverse_gen, fun proper_gen:bitstring_rev/1},
	{is_instance, fun erlang:is_bitstring/1}
    ]).

-spec list(raw_type()) -> type().
% TODO: subtyping would be useful here (list, vector, fixed_list)
list(RawElemType) ->
    ElemType = cook_outer(RawElemType),
    ?SEMI_OPAQUE([
	{generator, fun(Size) -> proper_gen:list_gen(Size, ElemType) end},
	{size_limit, ?MAX_LIST_LEN},
	{is_instance, fun(X) -> list_test(X, ElemType) end},
	{internal_type, ElemType},
	{get_length, fun erlang:length/1},
	{split, fun lists:split/2},
	{join, fun lists:append/2},
	{get_indices, fun(X) -> list_get_indices(X) end},
	{remove, fun(I,X) -> list_remove(I, X) end},
	{retrieve, fun lists:nth/2},
	{update, fun(I,V,X) -> list_update(I, V, X) end}
    ]).

-spec list_test(proper_gen:imm_instance(), type()) -> proper_arith:ternary().
list_test(X, ElemType) ->
    is_list(X)
    andalso proper_arith:all3([is_instance(E, ElemType) || E <- X]).

-spec list_get_indices(list()) -> [position()].
list_get_indices(List) ->
    lists:seq(1, length(List)).

-spec list_remove(position(), [T]) -> [T].
list_remove(Index, List) ->
    {H,[_Elem | T]} = lists:split(Index - 1, List),
    H ++ T.

-spec list_update(position(), T, [T]) -> [T].
list_update(Index, NewElem, List) ->
    {H,[_OldElem | T]} = lists:split(Index - 1, List),
    H ++ [NewElem] ++ T.

-spec vector(length(), raw_type()) -> type().
vector(Len, RawElemType) ->
    ElemType = cook_outer(RawElemType),
    Indices = lists:seq(1, Len),
    ?SEMI_OPAQUE([
	{generator, fun() -> proper_gen:vector_gen(Len, ElemType) end},
	{is_instance, fun(X) -> vector_test(X, Len, ElemType) end},
	{internal_type, ElemType},
	{get_indices, fun(_X) -> Indices end},
	{retrieve, fun lists:nth/2},
	{update, fun(I,V,X) -> list_update(I, V, X) end}
    ]).

-spec vector_test(proper_gen:imm_instance(), length(), type()) ->
	  proper_arith:ternary().
vector_test(X, Len, ElemType) ->
    is_list(X)
    andalso length(X) =:= Len
    andalso proper_arith:all3([is_instance(E, ElemType) || E <- X]).

-spec union([raw_type()]) -> type().
union(RawChoices) ->
    Choices = [cook_outer(C) || C <- RawChoices],
    ?BASIC([
	{generator, fun() -> proper_gen:union_gen(Choices) end},
	{is_instance, fun(X) -> union_test(X, Choices) end},
	{shrinkers,
	 [fun(X,_T,S) ->
	      proper_shrink:union_first_choice_shrinker(X, Choices, S)
	  end,
	  fun(X,_T,S) ->
	      proper_shrink:union_recursive_shrinker(X, Choices, S)
	  end]}
    ]).

-spec union_test(proper_gen:imm_instance(), [type()]) -> proper_arith:ternary().
union_test(X, Choices) ->
    proper_arith:any3([is_instance(X, C) || C <- Choices]).

-spec weighted_union([{frequency(),raw_type()}]) -> type().
weighted_union(RawFreqChoices) ->
    CookFreqType = fun({Freq,RawType}) -> {Freq,cook_outer(RawType)} end,
    FreqChoices = lists:map(CookFreqType, RawFreqChoices),
    Choices = [T || {_F,T} <- FreqChoices],
    ?SUBTYPE(union(Choices), [
	{generator, fun() -> proper_gen:weighted_union_gen(FreqChoices) end}
    ]).

-spec tuple([raw_type()]) -> type().
tuple(RawFields) ->
    Fields = [cook_outer(F) || F <- RawFields],
    Indices = lists:seq(1, length(Fields)),
    ?SEMI_OPAQUE([
	{generator, fun() -> proper_gen:tuple_gen(Fields) end},
	{is_instance, fun(X) -> tuple_test(X, Fields) end},
	{internal_types, list_to_tuple(Fields)},
	{get_indices, fun(_X) -> Indices end},
	{retrieve, fun erlang:element/2},
	{update, fun(I,V,X) -> tuple_update(I, V, X) end}
    ]).

-spec tuple_test(proper_gen:imm_instance(), [type()]) ->
	  proper_arith:ternary().
tuple_test(X, Fields) ->
    is_tuple(X) andalso fixed_list_test(tuple_to_list(X), Fields).

-spec tuple_update(position(), term(), tuple()) -> tuple().
tuple_update(Index, NewElem, Tuple) ->
    setelement(Index, Tuple, NewElem).

-spec exactly(term()) -> type().
exactly(E) ->
    ?BASIC([
	{generator, fun() -> proper_gen:exactly_gen(E) end},
	{is_instance, fun(X) -> X =:= E end}
    ]).

-spec fixed_list(maybe_improper_list()) -> type().
fixed_list(MaybeImproperRawFields) ->
    %% CAUTION: must handle improper lists
    {Fields, Internal, Indices, Retrieve, Update} =
	case cut_improper_tail(MaybeImproperRawFields) of
	    % TODO: have cut_improper_tail return the length and use it in test?
	    {ProperRawHead, ImproperRawTail} ->
		HeadLen = length(ProperRawHead),
		CookedHead = [cook_outer(F) || F <- ProperRawHead],
		CookedTail = cook_outer(ImproperRawTail),
		{{CookedHead,CookedTail},
		 CookedHead ++ CookedTail,
		 lists:seq(1, HeadLen + 1),
		 fun(I,L) -> improper_list_retrieve(I, L, HeadLen) end,
		 fun(I,V,L) -> improper_list_update(I, V, L, HeadLen) end};
	    ProperRawFields ->
		LocalFields = [cook_outer(F) || F <- ProperRawFields],
		{LocalFields,
		 LocalFields,
		 lists:seq(1, length(ProperRawFields)),
		 fun lists:nth/2,
		 fun(I,V,X) -> list_update(I, V, X) end}
	end,
    ?SEMI_OPAQUE([
	{generator, fun() -> proper_gen:fixed_list_gen(Fields) end},
	{is_instance, fun(X) -> fixed_list_test(X, Fields) end},
	{internal_types, Internal},
	{get_indices, fun(_X) -> Indices end},
	{retrieve, Retrieve},
	{update, Update}
    ]).

cut_improper_tail(List) ->
    cut_improper_tail_tr(List, []).

cut_improper_tail_tr([], AccList) ->
    lists:reverse(AccList);
cut_improper_tail_tr([Head | Tail], AccList) ->
    cut_improper_tail_tr(Tail, [Head | AccList]);
cut_improper_tail_tr(ImproperTail, AccList) ->
    {lists:reverse(AccList),ImproperTail}.

-spec fixed_list_test(proper_gen:imm_instance(), [type()] | {[type()],type()})
	  -> proper_arith:ternary().
fixed_list_test(X, {ProperHead,ImproperTail}) ->
    case is_list(X) andalso head_length(X) >= length(ProperHead) of
	true ->
	    {XHead,XTail} = lists:split(length(ProperHead), X),
	    proper_arith:and3(fixed_list_test(XHead, ProperHead),
			      is_instance(XTail, ImproperTail));
	false ->
	    false
    end;
fixed_list_test(X, ProperFields) ->
    is_list(X)
    andalso length(X) =:= length(ProperFields)
    andalso proper_arith:all3(lists:zipwith(fun(E,T) -> is_instance(E, T) end,
					    X, ProperFields)).

-spec head_length(maybe_improper_list()) -> length().
%% CAUTION: must handle improper lists
head_length(List) ->
    head_length_tr(List, 0).

head_length_tr([], Len) ->
    Len;
head_length_tr([_Head | Tail], Len) ->
    head_length_tr(Tail, Len + 1);
head_length_tr(_ImproperTail, Len) ->
    Len.

improper_list_retrieve(Index, List, HeadLen) ->
    case Index =< HeadLen of
	true  -> lists:nth(Index, List);
	false -> lists:nthtail(HeadLen, List)
    end.

improper_list_update(Index, Value, List, HeadLen) ->
    case Index =< HeadLen of
	true  -> list_update(Index, Value, List);
	false -> lists:sublist(List, HeadLen) ++ Value
    end.


%%------------------------------------------------------------------------------
%% Type aliases
%%------------------------------------------------------------------------------

%% TODO: term, maybe_improper_list(), maybe_improper_list(T), nonempty string,
%%	 iolist, module, mfa, node, timeout, list() => list(any()),
%%	 typle() => any tuple, no_return
-spec integer() -> type().
integer() -> integer(inf, inf).

-spec non_neg_integer() -> type().
non_neg_integer() -> integer(0, inf).

-spec pos_integer() -> type().
pos_integer() -> integer(1, inf).

-spec neg_integer() -> type().
neg_integer() -> integer(inf, -1).

-spec range(proper_arith:extint(), proper_arith:extint()) -> type().
range(Low, High) -> integer(Low, High).

-spec float() -> type().
float() -> float(inf, inf).

-spec non_neg_float() -> type().
non_neg_float() -> float(0.0, inf).

-spec number() -> type().
number() -> union([integer(), float()]).

-spec boolean() -> type().
boolean() -> union(['true', 'false']).

-spec byte() -> type().
byte() -> integer(0, 255).

-spec char() -> type().
char() -> integer(0, 16#10ffff).

-spec string() -> type().
string() -> list(char()).

-spec wunion([{frequency(),raw_type()}]) -> type().
wunion(FreqChoices) -> weighted_union(FreqChoices).


%%------------------------------------------------------------------------------
%% QuickCheck compatibility types
%%------------------------------------------------------------------------------

-spec int() -> type().
int() -> integer().

-spec int(proper_arith:extint(), proper_arith:extint()) -> type().
int(Low, High) -> integer(Low, High).

-spec bool() -> type().
bool() -> boolean().

-spec choose(proper_arith:extint(), proper_arith:extint()) -> type().
choose(Low, High) -> integer(Low, High).

-spec elements([raw_type()]) -> type().
elements(Choices) -> union(Choices).

-spec oneof([raw_type()]) -> type().
oneof(Choices) -> union(Choices).

-spec frequency([{frequency(),raw_type()}]) -> type().
frequency(FreqChoices) -> weighted_union(FreqChoices).


%%------------------------------------------------------------------------------
%% Additional type specification functions
%%------------------------------------------------------------------------------

-spec resize(size(), raw_type()) -> type().
resize(Size, RawType) ->
    add_prop(size_transform, fun(_S) -> Size end, cook_outer(RawType)).

-spec relimit(size(), raw_type()) -> type().
relimit(Limit, RawType) ->
    add_prop(size_limit, Limit, cook_outer(RawType)).

-spec non_empty(raw_type()) -> type().
non_empty(RawListType) ->
    ?SUCHTHAT(L, RawListType, L =/= []).
