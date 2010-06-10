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

%% @author Manolis Papadakis <manopapad@gmail.com>
%% @copyright 2010 Manolis Papadakis
%% @version {@version}
%% @doc Type manipulation functions and predefined types are contained in this
%%	module.

-module(proper_types).
-export([cook_outer/1, is_raw_type/1, get_prop/2, find_prop/2, new_type/2,
	 subtype/2, is_instance/2, unwrap/1, weakly/1, strongly/1,
	 satisfies_all/2]).
-export([sized/1, bind/2, shrinkwith/2, add_constraint/3]).
-export([integer/2, float/2, atom/0, list/1, vector/2, union/1,
	 weighted_union/1, tuple/1, exactly/1, fixed_list/1]).
-export([integer/0, non_neg_integer/0, pos_integer/0, neg_integer/0, range/2,
	 float/0, non_neg_float/0, number/0, boolean/0, byte/0, char/0,
	 string/0, wunion/1]).
-export([int/0, int/2, bool/0, choose/2, elements/1, oneof/1, frequency/1]).
-export([resize/2, relimit/2, non_empty/1]).

-include("proper_internal.hrl").


%% Type manipulation functions

-spec cook_outer(raw_type()) -> type().
cook_outer(Type = {'$type',_Props}) ->
    Type;
cook_outer(RawType) ->
    if
	is_tuple(RawType) -> tuple(erlang:tuple_to_list(RawType));
	% CAUTION: this must handle improper lists
	is_list(RawType)  -> fixed_list(RawType);
	% default case (covers integers, floats, atoms, binaries, ...):
	true              -> exactly(RawType)
    end.

-spec is_raw_type(term()) -> boolean().
is_raw_type({'$type',_TypeProps}) ->
    true;
is_raw_type(X) ->
    if
	is_tuple(X) -> is_raw_type_list(erlang:tuple_to_list(X));
	is_list(X)  -> is_raw_type_list(X);
	true        -> false
    end.

-spec is_raw_type_list(maybe_improper_list() | term()) -> boolean().
% CAUTION: this must handle improper lists
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
% TODO: should the 'is_instance' function etc. be reset for subtypes?
subtype(PropList, Type) ->
    add_props(PropList, Type).

-spec is_instance(imm_instance(), type()) -> ternary().
is_instance(ImmInstance, Type) ->
    CleanInstance = proper_gen:clean_instance(ImmInstance),
    proper_arith:and3(
	weakly(satisfies_all(CleanInstance, Type)),
	case find_prop(is_instance, Type) of
	    {ok, IsInstance} ->
		IsInstance(ImmInstance);
	    error ->
		case get_prop(kind, Type) of
		    wrapper     -> wrapper_test(ImmInstance, Type);
		    constructed -> constructed_test(ImmInstance, Type);
		    _           -> unknown
		end
	end
    ).

-spec wrapper_test(imm_instance(), type()) -> ternary().
wrapper_test(ImmInstance, Type) ->
    proper_arith:any3([is_instance(ImmInstance, T) || T <- unwrap(Type)]).

-spec unwrap(type()) -> [type()].
% TODO: check if it's actually a raw type that's returned?
unwrap(Type) ->
    [cook_outer(InnerType) || InnerType <- proper_gen:alt_gens(Type)
					   ++ [proper_gen:normal_gen(Type)]].

-spec constructed_test(imm_instance(), type()) -> ternary().
constructed_test({'$used',ImmParts,ImmInstance}, Type) ->
    PartsType = get_prop(parts_type, Type),
    Combine = get_prop(combine, Type),
    case is_instance(ImmParts, PartsType) of
	true ->
	    % TODO: check if it's actually a raw type that's returned?
	    % TODO: move construction code to proper_gen
	    % TODO: something less strict when an exactly is produced?
	    RawInnerType = Combine(proper_gen:clean_instance(ImmParts)),
	    InnerType = cook_outer(RawInnerType),
	    is_instance(ImmInstance, InnerType);
	Other ->
	    Other
    end;
constructed_test(_ImmInstance, _Type) ->
    % TODO: can we do anything better?
    unknown.

-spec weakly({boolean(),boolean()}) -> boolean().
weakly({B1,_B2}) -> B1.

-spec strongly({boolean(),boolean()}) -> boolean().
strongly({_B1,B2}) -> B2.

-spec satisfies(instance(), {instance_test(),boolean()})
	  -> {boolean(),boolean()}.
satisfies(Instance, {Test,false}) ->
    {true,Test(Instance)};
satisfies(Instance, {Test,true}) ->
    Result = Test(Instance),
    {Result,Result}.

-spec satisfies_all(instance(), type()) -> {boolean(),boolean()}.
satisfies_all(Instance, Type) ->
    case find_prop(constraints, Type) of
	{ok, Constraints} ->
	    L = [satisfies(Instance, C) || C <- Constraints],
	    {L1,L2} = lists:unzip(L),
	    {proper_arith:all3(L1), proper_arith:all3(L2)};
	error ->
	    {true,true}
    end.


%% Type definition functions

-spec sized(sized_generator()) -> type().
sized(Gen) ->
    ?WRAPPER([
	{generator, Gen}
    ]).

-spec bind(raw_type(), combine_fun()) -> type().
bind(RawPartsType, Combine) ->
    PartsType = cook_outer(RawPartsType),
    ?CONSTRUCTED([
	{parts_type, PartsType},
	{combine, Combine}
    ]).

-spec shrinkwith(nosize_generator(), alt_gens()) -> type().
shrinkwith(Gen, DelaydAltGens) ->
    ?WRAPPER([
	{generator, Gen},
	{alt_gens, DelaydAltGens}
    ]).

-spec add_constraint(raw_type(), instance_test(), boolean()) -> type().
add_constraint(RawType, Condition, IsStrict) ->
    Type = cook_outer(RawType),
    append_to_prop(constraints, {Condition,IsStrict}, Type).


%% Basic types

%% TODO: binary (general binary(), specified length, base size,
%%	 unit size?), bitstring
%% TODO: fun (generally some fun, unspecified number of arguments, but specified
%%	 return type, specific number and types of arguments and specific return
%%	 type) ("function" keyword?)
%% TODO: pid, port, ref (it's dangerous to provide random process data to
%%	 functions - they must want it for a reason (least we can do is have a
%%	 live function with that pid))
%% TODO: any (union of all types? what are those?)
%% TODO: (records, none, improper_list(content_type, termination_type),
%%	 maybe_improper_list)
-spec integer(extint(), extint()) -> type().
integer(Low, High) ->
    ?BASIC([
	{generator, fun(Size) -> proper_gen:integer_gen(Size, Low, High) end},
	{size_transform, fun(Size) -> Size div 3 end},
	{is_instance, fun(X) -> integer_test(X, Low, High) end},
	{shrinkers,
	 [fun(X,_T,S) -> proper_shrink:integer_shrinker(X, Low, High, S) end]}
    ]).

-spec integer_test(imm_instance(), extint(), extint()) -> boolean().
integer_test(X, Low, High) ->
    is_integer(X)
    andalso proper_arith:le(Low, X)
    andalso proper_arith:le(X, High).

-spec float(extnum(), extnum()) -> type().
float(Low, High) ->
    ?BASIC([
	{generator, fun(Size) -> proper_gen:float_gen(Size, Low, High) end},
	{size_transform, fun(Size) -> Size div 3 end},
	{is_instance, fun(X) -> float_test(X, Low, High) end},
	{shrinkers,
	 [fun(X,_T,S) -> proper_shrink:float_shrinker(X, Low, High, S) end]}
    ]).

-spec float_test(imm_instance(), extnum(), extnum()) -> boolean().
float_test(X, Low, High) ->
    is_float(X)
    andalso proper_arith:le(Low, X)
    andalso proper_arith:le(X, High).

-spec atom() -> type().
atom() ->
    ?WRAPPER([
	{generator, fun proper_gen:atom_gen/0}
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

-spec list_test(imm_instance(), type()) -> ternary().
list_test(X, ElemType) ->
    case is_list(X) of
	true  -> proper_arith:all3([is_instance(E, ElemType) || E <- X]);
	false -> false
    end.

-spec list_get_indices(list()) -> [position()].
list_get_indices(List) ->
    lists:seq(1, length(List)).

-spec list_remove(position(), [X]) -> [X].
list_remove(Index, List) ->
    {H,[_Elem | T]} = lists:split(Index - 1, List),
    H ++ T.

-spec list_update(position(), X, [X]) -> [X].
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

-spec vector_test(imm_instance(), length(), type()) -> ternary().
vector_test(X, Len, ElemType) ->
    case is_list(X) andalso length(X) =:= Len of
	true  -> proper_arith:all3([is_instance(E, ElemType) || E <- X]);
	false -> false
    end.

-spec union([raw_type()]) -> type().
union(RawChoices) ->
    Choices = lists:map(fun ?MODULE:cook_outer/1, RawChoices),
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

-spec union_test(imm_instance(), [type()]) -> ternary().
union_test(X, Choices) ->
    proper_arith:any3([is_instance(X, C) || C <- Choices]).

-spec weighted_union([{frequency(),raw_type()}]) -> type().
weighted_union(RawFreqChoices) ->
    CookFreqType = fun({Freq,RawType}) -> {Freq,cook_outer(RawType)} end,
    FreqChoices = lists:map(CookFreqType, RawFreqChoices),
    Choices = lists:map(fun({_F,T}) -> T end, FreqChoices),
    ?SUBTYPE(union(Choices), [
	{generator, fun() -> proper_gen:weighted_union_gen(FreqChoices) end}
    ]).

-spec tuple([raw_type()]) -> type().
tuple(RawFields) ->
    Fields = lists:map(fun ?MODULE:cook_outer/1, RawFields),
    Indices = lists:seq(1, length(Fields)),
    ?SEMI_OPAQUE([
	{generator, fun() -> proper_gen:tuple_gen(Fields) end},
	{is_instance, fun(X) -> tuple_test(X, Fields) end},
	{internal_types, erlang:list_to_tuple(Fields)},
	{get_indices, fun(_X) -> Indices end},
	{retrieve, fun erlang:element/2},
	{update, fun(I,V,X) -> tuple_update(I, V, X) end}
    ]).

-spec tuple_test(imm_instance(), [type()]) -> ternary().
tuple_test(X, Fields) ->
    case is_tuple(X) of
	true  -> fixed_list_test(erlang:tuple_to_list(X), Fields);
	false -> false
    end.

-spec tuple_update(position(), term(), tuple()) -> tuple().
tuple_update(Index, NewElem, Tuple) ->
    erlang:setelement(Index, Tuple, NewElem).

-spec exactly(term()) -> type().
exactly(E) ->
    ?BASIC([
	{generator, fun() -> proper_gen:exactly_gen(E) end},
	{is_instance, fun(X) -> X =:= E end}
    ]).

-spec fixed_list(maybe_improper_list(raw_type(),raw_type())) -> type().
fixed_list(MaybeImproperRawFields) ->
    % CAUTION: must handle improper lists
    {Fields, Internal, Indices, Retrieve, Update} =
	case cut_improper_tail(MaybeImproperRawFields) of
	    % TODO: have cut_improper_tail return the length and use it in test?
	    {ProperRawHead, ImproperRawTail} ->
		HeadLen = length(ProperRawHead),
		CookedHead = lists:map(fun ?MODULE:cook_outer/1, ProperRawHead),
		CookedTail = cook_outer(ImproperRawTail),
		{{CookedHead,CookedTail},
		 CookedHead ++ CookedTail,
		 lists:seq(1, HeadLen + 1),
		 fun(I,L) -> improper_list_retrieve(I, L, HeadLen) end,
		 fun(I,V,L) -> improper_list_update(I, V, L, HeadLen) end};
	    ProperRawFields ->
		LocalFields = lists:map(fun ?MODULE:cook_outer/1,
					ProperRawFields),
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

-spec cut_improper_tail([X]) -> [X]
		       % TODO: this should be improper_list(X,Y)
		     ; (maybe_improper_list(X,Y)) -> {[X],Y}.
cut_improper_tail(List) ->
    cut_improper_tail_tr(List, []).

-spec cut_improper_tail_tr(maybe_improper_list(X,Y) | Y, [X]) -> [X] | {[X],Y}.
cut_improper_tail_tr([], AccList) ->
    lists:reverse(AccList);
cut_improper_tail_tr([Head | Tail], AccList) ->
    cut_improper_tail_tr(Tail, [Head | AccList]);
cut_improper_tail_tr(ImproperTail, AccList) ->
    {lists:reverse(AccList),ImproperTail}.

-spec fixed_list_test(imm_instance(), [type()] | {[type()],type()})
	  -> ternary().
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
    case is_list(X) andalso length(X) =:= length(ProperFields) of
	true ->
	    proper_arith:all3(lists:zipwith(fun(E,T) -> is_instance(E, T) end,
					    X, ProperFields));
	false ->
	    false
    end.

-spec head_length(maybe_improper_list()) -> length().
% CAUTION: must handle improper lists
head_length(List) ->
    head_length_tr(List, 0).

-spec head_length_tr(maybe_improper_list() | term(), length()) -> length().
head_length_tr([], Len) ->
    Len;
head_length_tr([_Head | Tail], Len) ->
    head_length_tr(Tail, Len + 1);
head_length_tr(_ImproperTail, Len) ->
    Len.

-spec improper_list_retrieve(position(), [X], length()) -> X | [X]
			    % TODO: this should be improper_list(X,Y)
			  ; (position(), maybe_improper_list(X,Y), length()) ->
	  X | Y.
improper_list_retrieve(Index, List, HeadLen) ->
    case Index =< HeadLen of
	true  -> lists:nth(Index, List);
	false -> lists:nthtail(HeadLen, List)
    end.

-spec improper_list_update(position(), X | Y, maybe_improper_list(X,Y),
			   length()) ->
	  maybe_improper_list(X,Y).
improper_list_update(Index, Value, List, HeadLen) ->
    case Index =< HeadLen of
	true  -> list_update(Index, Value, List);
	false -> lists:sublist(List, HeadLen) ++ Value
    end.


%% Type aliases

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

-spec range(extint(), extint()) -> type().
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


%% QuickCheck compatibility types

-spec int() -> type().
int() -> integer().

-spec int(extint(), extint()) -> type().
int(Low, High) -> integer(Low, High).

-spec bool() -> type().
bool() -> boolean().

-spec choose(extint(), extint()) -> type().
choose(Low, High) -> integer(Low, High).

-spec elements([raw_type()]) -> type().
elements(Choices) -> union(Choices).

-spec oneof([raw_type()]) -> type().
oneof(Choices) -> union(Choices).

-spec frequency([{frequency(),raw_type()}]) -> type().
frequency(FreqChoices) -> weighted_union(FreqChoices).


%% Additional type specification functions

-spec resize(size(), raw_type()) -> type().
resize(Size, RawType) ->
    add_prop(size_transform, fun(_S) -> Size end, cook_outer(RawType)).

-spec relimit(size(), raw_type()) -> type().
relimit(Limit, RawType) ->
    add_prop(size_limit, Limit, cook_outer(RawType)).

-spec non_empty(raw_type()) -> type().
non_empty(RawListType) ->
    ?SUCHTHAT(L, RawListType, L =/= []).
