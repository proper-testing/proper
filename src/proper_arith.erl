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
%%% @doc This module contains helper arithmetic, list handling and random
%%%	 functions.

-module(proper_arith).

-export([le/2, and3/2, or3/2, any3/1, all3/1, maybe/1, surely/1]).
-export([safemap/2, tuplemap/2, cut_improper_tail/1]).
-export([rand_start/1, rand_stop/0,
	 rand_int/1, rand_int/2, rand_non_neg_int/1,
	 rand_float/1, rand_float/2, rand_non_neg_float/1,
	 rand_bytes/1, distribute/2, jumble/1, rand_choose/1, freq_choose/1]).

-export_type([extint/0, extnum/0, ternary/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type extint()  :: integer() | 'inf'.
-type extnum()  :: number()  | 'inf'.
-type ternary() :: boolean() | 'unknown'.
-type delayed_ternary() :: fun(() -> ternary()).


%%------------------------------------------------------------------------------
%% Arithmetic functions
%%------------------------------------------------------------------------------

-spec le(extnum(), extnum()) -> boolean().
le(inf, _B) -> true;
le(_A, inf) -> true;
le(A, B)    -> A =< B.

-spec and3(delayed_ternary(), delayed_ternary()) -> ternary().
and3(A, B) ->
    case ?FORCE(A) of
	true    -> ?FORCE(B);
	false   -> false;
	unknown ->
	    case ?FORCE(B) of
		false -> false;
		_     -> unknown
	    end
    end.

-spec or3(delayed_ternary(), delayed_ternary()) -> ternary().
or3(A, B) ->
    case ?FORCE(A) of
	true    -> true;
	false   -> ?FORCE(B);
	unknown ->
	    case ?FORCE(B) of
		true -> true;
		_    -> unknown
	    end
    end.

-spec any3([ternary()]) -> ternary().
any3(TernList) ->
    any3_tr(TernList, false).

-spec any3_tr([ternary()], boolean()) -> ternary().
any3_tr([], true) ->
    unknown;
any3_tr([], false) ->
    false;
any3_tr([true | _Rest], _FoundUnknown) ->
    true;
any3_tr([false | Rest], FoundUnknown) ->
    any3_tr(Rest, FoundUnknown);
any3_tr([unknown | Rest], _FoundUnknown) ->
    any3_tr(Rest, true).

-spec all3([ternary()]) -> ternary().
all3(TernList) ->
    all3_tr(TernList, false).

-spec all3_tr([ternary()], boolean()) -> ternary().
all3_tr([], true) ->
    unknown;
all3_tr([], false) ->
    true;
all3_tr([true | Rest], FoundUnknown) ->
    all3_tr(Rest, FoundUnknown);
all3_tr([false | _Rest], _FoundUnknown) ->
    false;
all3_tr([unknown | Rest], _FoundUnknown) ->
    all3_tr(Rest, true).

-spec maybe(ternary()) -> boolean().
maybe(unknown) -> true;
maybe(Boolean) -> Boolean.

-spec surely(ternary()) -> boolean().
surely(unknown) -> false;
surely(Boolean) -> Boolean.


%%------------------------------------------------------------------------------
%% List handling functions
%%------------------------------------------------------------------------------

-spec safemap(fun((T) -> S), maybe_improper_list(T,term())) ->
	  maybe_improper_list(S,term()).
safemap(Fun, List) ->
    safemap_tr(Fun, List, []).

-spec safemap_tr(fun((T) -> S), maybe_improper_list(T,term()), [S]) ->
          maybe_improper_list(S,term()).
safemap_tr(_Fun, [], AccList) ->
    lists:reverse(AccList);
safemap_tr(Fun, [Head | Tail], AccList) ->
    safemap_tr(Fun, Tail, [Fun(Head) | AccList]);
safemap_tr(Fun, ImproperTail, AccList) ->
    lists:reverse(AccList) ++ Fun(ImproperTail).

-spec tuplemap(fun((term()) -> term()), tuple()) -> tuple().
tuplemap(Fun, Tuple) ->
    list_to_tuple(lists:map(Fun, tuple_to_list(Tuple))).

-spec cut_improper_tail(maybe_improper_list(T,term())) -> {[T],T} | [T].
cut_improper_tail(List) ->
    cut_improper_tail_tr(List, []).

-spec cut_improper_tail_tr(maybe_improper_list(T,term()) | T, [T]) ->
	  {[T],T} | [T].
cut_improper_tail_tr([], AccList) ->
    lists:reverse(AccList);
cut_improper_tail_tr([Head | Tail], AccList) ->
    cut_improper_tail_tr(Tail, [Head | AccList]);
cut_improper_tail_tr(ImproperTail, AccList) ->
    {lists:reverse(AccList), ImproperTail}.


%%------------------------------------------------------------------------------
%% Random functions
%%------------------------------------------------------------------------------

%% @doc Seeds the random number generator. This function should be run before
%% calling any random function from this module.
-spec rand_start(boolean()) -> 'ok'.
rand_start(Crypto) ->
    _ = random:seed(now()),
    %% TODO: read option for RNG bijections here
    case Crypto of
	true ->
	    case crypto:start() of
		ok ->
		    put('$crypto', true),
		    ok;
		{error, _} ->
		    ok
	    end;
	false ->
	    ok
    end.

-spec rand_stop() -> 'ok'.
rand_stop() ->
    case get('$crypto') of
	true ->
	    erase('$crypto'),
	    _ = crypto:stop(),
	    ok;
	_ ->
	    ok
    end,
    erase(random_seed),
    ok.

-spec rand_int(non_neg_integer()) -> integer().
rand_int(Const) ->
    round(rand_float(Const)).

-spec rand_non_neg_int(non_neg_integer()) -> non_neg_integer().
rand_non_neg_int(Const) ->
    trunc(rand_non_neg_float(Const)).

-spec rand_int(integer(), integer()) -> integer().
rand_int(Low, High) when is_integer(Low), is_integer(High), Low =< High ->
    case get('$crypto') of
	true ->
	    crypto:rand_uniform(Low, High + 1);
	_ ->
	    Low + random:uniform(High - Low + 1) - 1
    end.

-spec rand_float(non_neg_integer()) -> float().
rand_float(Const) ->
    X = rand_non_neg_float(Const),
    case random:uniform(2) of
	1 -> X;
	2 -> -X
    end.

-spec rand_non_neg_float(non_neg_integer()) -> float().
rand_non_neg_float(Const) when is_integer(Const), Const >= 0 ->
    case random:uniform() of
	1.0 -> rand_non_neg_float(Const);
	X   -> Const * zero_one_to_zero_inf(X)
    end.

-spec rand_float(float(), float()) -> float().
rand_float(Low, High) when is_float(Low), is_float(High), Low =< High ->
    Low + random:uniform() * (High - Low).

-spec zero_one_to_zero_inf(float()) -> float().
%% This function must return only non-negative values and map 0.0 to 0.0, but
%% may be undefined at 1.0.
%% TODO: read global options and decide here which bijection to use
zero_one_to_zero_inf(X) ->
    X / math:sqrt(1 - X*X).

-spec rand_bytes(length()) -> binary() | '$cant_generate'.
rand_bytes(Len) ->
    case get('$crypto') of
	true -> crypto:rand_bytes(Len);
	_    -> '$cant_generate'
    end.

-spec distribute(non_neg_integer(), non_neg_integer()) -> [non_neg_integer()].
distribute(_Credits, 0) ->
    [];
distribute(Credits, People) ->
    jumble(distribute_tr(Credits, People, [])).

-spec distribute_tr(non_neg_integer(), pos_integer(), [non_neg_integer()]) ->
	  [non_neg_integer()].
distribute_tr(0, PeopleLeft, AccList) ->
    lists:duplicate(PeopleLeft, 0) ++ AccList;
distribute_tr(CreditsLeft, 1, AccList) ->
    [CreditsLeft | AccList];
distribute_tr(CreditsLeft, PeopleLeft, AccList) ->
    YourCut = rand_int(0, CreditsLeft),
    distribute_tr(CreditsLeft - YourCut, PeopleLeft - 1, [YourCut | AccList]).

-spec jumble([T]) -> [T].
%% @doc Produces a random permutation of a list.
jumble(List) ->
    jumble_tr(List, length(List), []).

-spec jumble_tr([T], non_neg_integer(), [T]) -> [T].
jumble_tr([], 0, Acc) ->
    Acc;
jumble_tr(List, Len, Acc) ->
    Pos = rand_int(0, Len - 1),
    {List1, [H|List2]} = lists:split(Pos, List),
    jumble_tr(List1 ++ List2, Len - 1, [H|Acc]).

-spec rand_choose([T,...]) -> {position(),T}.
rand_choose(Choices) when Choices =/= [] ->
    Pos = rand_int(1, length(Choices)),
    {Pos, lists:nth(Pos, Choices)}.

-spec freq_choose([{frequency(),T},...]) -> {position(),T}.
freq_choose(Choices) when Choices =/= []  ->
    AddFreq = fun({Freq,_},Acc) -> Freq + Acc end,
    SumFreq = lists:foldl(AddFreq, 0, Choices),
    freq_select(rand_int(1, SumFreq), Choices, 1).

-spec freq_select(frequency(), [{frequency(),T}], position()) -> {position(),T}.
freq_select(N, [{Freq,Choice} | Rest], Pos) ->
    case N =< Freq of
	true ->
	    {Pos,Choice};
	false ->
	    freq_select(N - Freq, Rest, Pos + 1)
    end.
