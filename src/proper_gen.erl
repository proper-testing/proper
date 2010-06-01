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
%% @doc The generator subsystem and generators for basic types are contained in
%%	this module.

-module(proper_gen).
-export([generate/1, generate/3, sample/2, normal_gen/1, alt_gens/1,
	 clean_instance/1]).
-export([integer_gen/3, float_gen/3, atom_gen/0, list_gen/2, vector_gen/2,
	 union_gen/1, weighted_union_gen/1, tuple_gen/1, exactly_gen/1,
	 fixed_list_gen/1]).

-include("proper_internal.hrl").


%% Instance generation functions

generate(Type = {'$type',_Props}) ->
    generate(Type, ?MAX_TRIES_TO_SATISFY_CONSTRAINTS, '$none');
generate(RawType) ->
    generate(proper_types:cook_outer(RawType)).

generate(_Type, 0, '$none') ->
    '$cant_generate';
generate(_Type, 0, Fallback) ->
    Fallback;
generate(Type, Tries, Fallback) ->
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
		ImmInstance1 = normal_gen(Type),
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
	{true,false}  -> generate(Type, Tries - 1, Result);
	{false,false} -> % TODO: is it okay to grow the size here?
			 proper:grow_size(),
			 generate(Type, Tries - 1, Fallback)
    end.

sample(Size, RawType) ->
    Opts = #opts{},
    proper:global_state_init(Opts),
    proper:set_size(Size),
    ImmInstance = generate(RawType),
    %io:format("~p~n~n", [ImmInstance]),
    proper:global_state_erase(Opts),
    clean_instance(ImmInstance).

% functions that produce one step
normal_gen(Type) ->
    Gen = proper_types:get_prop(generator, Type),
    if
	is_function(Gen, 0) -> Gen();
	is_function(Gen, 1) -> Size = proper:get_size(Type),
			       Gen(Size)
    end.

alt_gens(Type) ->
    case proper_types:find_prop(alt_gens, Type) of
	{ok, AltGens} -> ?FORCE(AltGens);
	error         -> []
    end.

% TODO: is it possible to have a raw_type tuple here?
clean_instance({'$used',_ImmParts,ImmInstance}) ->
    clean_instance(ImmInstance);
clean_instance({'$gen',_GenNum,ImmInstance}) ->
    clean_instance(ImmInstance);
clean_instance(ImmInstance) ->
    if
	is_list(ImmInstance) ->
	    % CAUTION: this must handle improper lists
	    safemap(fun ?MODULE:clean_instance/1, ImmInstance);
	is_tuple(ImmInstance) ->
	    tuplemap(fun ?MODULE:clean_instance/1, ImmInstance);
	true ->
	    ImmInstance
    end.

safemap(Fun, List) ->
    safemap_tr(Fun, List, []).

safemap_tr(_Fun, [], AccList) ->
    lists:reverse(AccList);
safemap_tr(Fun, [Head | Tail], AccList) ->
    safemap_tr(Fun, Tail, [Fun(Head) | AccList]);
safemap_tr(Fun, ImproperTail, AccList) ->
    lists:reverse(AccList) ++ Fun(ImproperTail).

tuplemap(Fun, Tuple) ->
    erlang:list_to_tuple(lists:map(Fun, erlang:tuple_to_list(Tuple))).


%% Basic type generators

integer_gen(Size, minus_inf, plus_inf) ->
    proper_arith:rand_int(Size);
integer_gen(Size, minus_inf, High) ->
    High - proper_arith:rand_non_neg_int(Size);
integer_gen(Size, Low, plus_inf) ->
    Low + proper_arith:rand_non_neg_int(Size);
integer_gen(_Size, Low, High) ->
    proper_arith:rand_int(Low, High).

float_gen(Size, minus_inf, plus_inf) ->
    proper_arith:rand_float(Size);
float_gen(Size, minus_inf, High) ->
    High - proper_arith:rand_non_neg_float(Size);
float_gen(Size, Low, plus_inf) ->
    Low + proper_arith:rand_non_neg_float(Size);
float_gen(_Size, Low, High) ->
    proper_arith:rand_float(Low, High).

% we make sure we never clash by checking that the first character is not '$'
atom_gen() ->
    ?LET(Str,
	 ?SUCHTHAT(X,
		   proper_types:relimit(255,
					proper_types:list(proper_types:byte())),
		   length(X) =:= 0 orelse first(X) =/= $$),
	 erlang:list_to_atom(Str)).

first([Head | _Rest]) -> Head.

list_gen(Size, ElemType) ->
    Len = proper_arith:rand_int(0, Size),
    vector_gen(Len, ElemType).

vector_gen(Len, ElemType) ->
    fixed_list_gen(lists:duplicate(Len, ElemType)).

union_gen(Choices) ->
    {_Choice,Type} = proper_arith:rand_choose(Choices),
    generate(Type).

weighted_union_gen(FreqChoices) ->
    {_Choice,Type} = proper_arith:freq_choose(FreqChoices),
    generate(Type).

tuple_gen(Fields) ->
    erlang:list_to_tuple(fixed_list_gen(Fields)).

exactly_gen(X) ->
    X.

fixed_list_gen({ProperHead,ImproperTail}) ->
    lists:map(fun ?MODULE:generate/1, ProperHead) ++ generate(ImproperTail);
fixed_list_gen(ProperFields) ->
    lists:map(fun ?MODULE:generate/1, ProperFields).
