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
-export([resize/2, relimit/2]).
-export([sized/1, bind/2, shrinkwith/2, add_constraint/3]).
-export([integer/2, float/2, atom/0, list/1, vector/2, union/1,
	 weighted_union/1, tuple/1, exactly/1, fixed_list/1]).
-export([integer/0, non_neg_integer/0, pos_integer/0, neg_integer/0, float/0,
	 non_neg_float/0, number/0, boolean/0, byte/0, char/0, string/0,
	 wunion/1]).
-export([int/0, int/2, bool/0, choose/2, elements/1, oneof/1, frequency/1]).

-include("proper_internal.hrl").


%% Type manipulation functions

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

is_raw_type({'$type',_TypeProps}) ->
    true;
is_raw_type(X) ->
    if
	is_tuple(X) -> is_raw_type_list(erlang:tuple_to_list(X));
	is_list(X)  -> is_raw_type_list(X);
	true        -> false
    end.

% CAUTION: this must handle improper lists
is_raw_type_list([]) ->
    false;
is_raw_type_list([X | Rest]) ->
    is_raw_type(X) orelse is_raw_type_list(Rest);
is_raw_type_list(X) ->
    is_raw_type(X).

type_from_list(KeyValueList) ->
    {'$type',orddict:from_list(KeyValueList)}.

add_prop(PropName, Value, {'$type',Props}) ->
    {'$type',orddict:store(PropName, Value, Props)}.

add_props(PropList, {'$type',OldProps}) ->
    {'$type', lists:foldl(fun({N,V},Acc) -> orddict:store(N, V, Acc) end,
			  OldProps, PropList)}.

append_to_prop(PropName, Value, {'$type',Props}) ->
    {'$type',orddict:append(PropName, Value, Props)}.

get_prop(PropName, {'$type',Props}) ->
    orddict:fetch(PropName, Props).

find_prop(PropName, {'$type',Props}) ->
    orddict:find(PropName, Props).

new_type(PropList, Kind) ->
    Type = type_from_list(PropList),
    add_prop(kind, Kind, Type).

% TODO: should the 'is_instance' function be reset for subtypes etc.?
subtype(Type, PropList) ->
    add_props(PropList, Type).

is_instance(X, Type) ->
    Clean = proper_gen:clean_instance(X),
    proper_arith:and3(
	weakly(satisfies_all(Clean, Type)),
	case find_prop(is_instance, Type) of
	    {ok, IsInstance} ->
		IsInstance(X);
	    error ->
		case get_prop(kind, Type) of
		    wrapper     -> wrapper_test(X, Type);
		    constructed -> constructed_test(X, Type);
		    _           -> unknown
		end
	end
    ).

wrapper_test(ImmInstance, Type) ->
    proper_arith:any3(lists:map(fun(T) -> is_instance(ImmInstance, T) end,
				unwrap(Type))).

% TODO: check if it's actually a raw type that's returned?
unwrap(Type) ->
    lists:map(fun ?MODULE:cook_outer/1, proper_gen:alt_gens(Type))
    ++ [cook_outer(proper_gen:normal_gen(Type))].

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
constructed_test(_X, _Type) ->
    % TODO: can we do anything better?
    unknown.

weakly({B1,_B2}) -> B1.

strongly({_B1,B2}) -> B2.

satisfies(X, {Test, false}) ->
    {true,Test(X)};
satisfies(X, {Test, true}) ->
    Result = Test(X),
    {Result,Result}.

satisfies_all(X, Type) ->
    case find_prop(constraints, Type) of
	{ok, Constraints} ->
	    L = lists:map(fun(C) -> satisfies(X,C) end, Constraints),
	    {L1,L2} = lists:unzip(L),
	    {proper_arith:all3(L1), proper_arith:all3(L2)};
	error ->
	    {true,true}
    end.

% TODO: our resize only applies to the outer type
resize(Size, Type) ->
    add_prop(size_transform, fun(_S) -> Size end, Type).

relimit(Limit, Type) ->
    add_prop(size_limit, Limit, Type).


%% Type definition functions

sized(Gen) ->
    ?WRAPPER([
	{generator, Gen}
    ]).

bind(RawPartsType, Combine) ->
    PartsType = cook_outer(RawPartsType),
    ?CONSTRUCTED([
	{parts_type, PartsType},
	{combine, Combine}
    ]).

shrinkwith(Gen, DelaydAltGens) ->
    ?WRAPPER([
	{generator, Gen},
	{alt_gens, DelaydAltGens}
    ]).

add_constraint(RawType, Condition, Strictness) ->
    Type = cook_outer(RawType),
    append_to_prop(constraints, {Condition,Strictness}, Type).


%% Basic types

% TODO: binary (general binary(), specified length, base size,
%	unit size?), bitstring
% TODO: fun (generally some fun, unspecified number of arguments, but specified
%	return type, specific number and types of arguments and specific return
%	type) ("function" keyword?)
% TODO: pid, port, ref (it's dangerous to provide random process data to
%	functions - they must want it for a reason (least we can do is have a
%	live function with that pid))
% TODO: any (union of all types? what are those?)
% TODO: (records, none, improper_list(content_type, termination_type),
%	maybe_improper_list)
integer(Low, High) ->
    ?BASIC([
	{generator, fun(Size) -> proper_gen:integer_gen(Size, Low, High) end},
	{size_transform, fun(Size) -> Size div 3 end},
	{is_instance, fun(X) -> integer_test(X, Low, High) end},
	{shrinkers,
	 [fun(X,_T,S) -> proper_shrink:integer_shrinker(X, Low, High, S) end]}
    ]).

integer_test(X, Low, High) ->
    is_integer(X)
    andalso proper_arith:le(Low, X)
    andalso proper_arith:le(X, High).

float(Low, High) ->
    ?BASIC([
	{generator, fun(Size) -> proper_gen:float_gen(Size, Low, High) end},
	{size_transform, fun(Size) -> Size div 3 end},
	{is_instance, fun(X) -> float_test(X, Low, High) end},
	{shrinkers,
	 [fun(X,_T,S) -> proper_shrink:float_shrinker(X, Low, High, S) end]}
    ]).

float_test(X, Low, High) ->
    is_float(X)
    andalso proper_arith:le(Low, X)
    andalso proper_arith:le(X, High).

atom() ->
    ?WRAPPER([
	{generator, fun proper_gen:atom_gen/0}
    ]).

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

list_test(X, ElemType) ->
    is_list(X)
    andalso proper_arith:all3(
		lists:map(fun(E) -> is_instance(E, ElemType) end, X)).

list_get_indices(List) ->
    lists:seq(1, length(List)).

list_remove(Index, List) ->
    {H,[_Elem | T]} = lists:split(Index - 1, List),
    H ++ T.

list_update(Index, NewElem, List) ->
    {H,[_OldElem | T]} = lists:split(Index - 1, List),
    H ++ [NewElem] ++ T.

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

vector_test(X, Len, ElemType) ->
    is_list(X)
    andalso length(X) =:= Len
    andalso proper_arith:all3(
		lists:map(fun(E) -> is_instance(E, ElemType) end, X)).

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

union_test(X, Choices) ->
    proper_arith:any3(lists:map(fun(C) -> is_instance(X, C) end, Choices)).

weighted_union(RawFreqChoices) ->
    CookFreqType = fun({Freq,RawType}) -> {Freq,cook_outer(RawType)} end,
    FreqChoices = lists:map(CookFreqType, RawFreqChoices),
    Choices = lists:map(fun({_F,T}) -> T end, FreqChoices),
    ?SUBTYPE(union(Choices), [
	{generator, fun() -> proper_gen:weighted_union_gen(FreqChoices) end}
    ]).

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

tuple_test(X, Fields) ->
    is_tuple(X) andalso fixed_list_test(erlang:tuple_to_list(X), Fields).

tuple_update(Index, NewElem, Tuple) ->
    erlang:setelement(Index, Tuple, NewElem).

exactly(E) ->
    ?BASIC([
	{generator, fun() -> proper_gen:exactly_gen(E) end},
	{is_instance, fun(X) -> X =:= E end}
    ]).

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

cut_improper_tail(List) ->
    cut_improper_tail(List, []).

cut_improper_tail([], AccList) ->
    lists:reverse(AccList);
cut_improper_tail([Head | Tail], AccList) ->
    cut_improper_tail(Tail, [Head | AccList]);
cut_improper_tail(ImproperTail, AccList) ->
    {lists:reverse(AccList),ImproperTail}.

fixed_list_test(X, {ProperHead,ImproperTail}) ->
    is_list(X)
    andalso head_length(X) >= length(ProperHead)
    andalso begin
		{XHead,XTail} = lists:split(length(ProperHead), X),
		proper_arith:and3(
		    fixed_list_test(XHead, ProperHead),
		    is_instance(XTail, ImproperTail))
	    end;
fixed_list_test(X, ProperFields) ->
    is_list(X)
    andalso length(X) =:= length(ProperFields)
    andalso proper_arith:all3(
		lists:zipwith(fun(E,T) -> is_instance(E, T) end,
			      X, ProperFields)).

% CAUTION: must handle improper lists
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


%% Type aliases

% TODO: term, maybe_improper_list(), maybe_improper_list(T), nonempty string,
%	iolist, module, mfa, node, timeout, list() => list(any()),
%	typle() => any tuple, no_return
integer() -> integer(minus_inf, plus_inf).
non_neg_integer() -> integer(0, plus_inf).
pos_integer() -> integer(1, plus_inf).
neg_integer() -> integer(minus_inf, -1).
float() -> float(minus_inf, plus_inf).
non_neg_float() -> float(0.0, plus_inf).
number() -> union([integer(), float()]).
boolean() -> union(['true', 'false']).
byte() -> integer(0, 255).
char() -> integer(0, 16#10ffff).
string() -> list(char()).
wunion(FreqChoices) -> weighted_union(FreqChoices).


%% QuickCheck compatibility types

int() -> integer().
int(Low, High) -> integer(Low, High).
bool() -> boolean().
choose(Low, High) -> integer(Low, High).
elements(Choices) -> union(Choices).
oneof(Choices) -> union(Choices).
frequency(FreqChoices) -> weighted_union(FreqChoices).
