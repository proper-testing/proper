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
%%% @doc This module contains helper arithmetic and random functions.

-module(proper_arith).
-export([le/2, and3/2, or3/2, any3/1, all3/1, maybe/1, surely/1]).
-export([rand_start/1, rand_stop/1]).
-export([rand_int/0, rand_int/1, rand_int/2,
	 rand_non_neg_int/0, rand_non_neg_int/1]).
-export([rand_float/0, rand_float/1, rand_float/2,
	 rand_non_neg_float/0, rand_non_neg_float/1]).
-export([jumble/1, rand_choose/1, freq_choose/1]).

-export_type([extint/0, extnum/0, ternary/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Dialyzer types
%%------------------------------------------------------------------------------

-type extint() :: integer() | 'inf'.
-type extnum() :: number() | 'inf'.
-type ternary() :: 'true' | 'false' | 'unknown'.


%%------------------------------------------------------------------------------
%% Arithmetic functions
%%------------------------------------------------------------------------------

-spec le(extnum(), extnum()) -> boolean().
le(inf, _B) -> true;
le(_A, inf) -> true;
le(A, B)    -> A =< B.

-spec and3(ternary(), ternary()) -> ternary().
and3(true, true) -> true;
and3(false, _)   -> false;
and3(_, false)   -> false;
and3(_, _)       -> unknown.

-spec or3(ternary(), ternary()) -> ternary().
or3(false, false) -> false;
or3(true, _)      -> true;
or3(_, true)      -> true;
or3(_, _)         -> unknown.

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
maybe(B)       -> B.

-spec surely(ternary()) -> boolean().
surely(unknown) -> false;
surely(B)       -> B.


%%------------------------------------------------------------------------------
%% Random functions
%%------------------------------------------------------------------------------

%% @doc Seeds the random number generator. This function should be run before
%% calling any random function from this module.
-spec rand_start(#opts{}) -> 'ok'.
rand_start(Opts) ->
    _ = random:seed(now()),
    % TODO: read option for RNG bijections here
    case Opts#opts.crypto of
	true ->
	    crypto:start(),
	    put('$crypto', true),
	    ok;
	false ->
	    ok
    end.

-spec rand_stop(#opts{}) -> 'ok'.
rand_stop(Opts) ->
    case Opts#opts.crypto of
	true ->
	    erase('$crypto'),
	    crypto:stop(),
	    ok;
	false ->
	    ok
    end,
    erase(random_seed),
    ok.

%% @doc Returns a random integer. Probability is higher for values closer to 0.
-spec rand_int() -> integer().
rand_int() ->
    round(rand_float()).

-spec rand_int(non_neg_integer()) -> integer().
rand_int(Const) ->
    round(rand_float(Const)).

-spec rand_non_neg_int() -> non_neg_integer().
rand_non_neg_int() ->
    trunc(rand_non_neg_float()).

-spec rand_non_neg_int(non_neg_integer()) -> non_neg_integer().
rand_non_neg_int(Const) ->
    trunc(rand_non_neg_float(Const)).

-spec rand_int(integer(), integer()) -> integer().
%% TODO: global option to use crypto module's rand_uniform(Lo,Hi) (caution:
%%	 produced values will be Lo le N lt Hi)
rand_int(Low, High) when is_integer(Low) andalso is_integer(High)
		    andalso Low =< High ->
    Low + random:uniform(High - Low + 1) - 1.

-spec rand_float() -> float().
rand_float() ->
    rand_float(?DEFAULT_RNG_CONST).

-spec rand_float(non_neg_integer()) -> float().
rand_float(Const) ->
    X = rand_non_neg_float(Const),
    case random:uniform(2) of
	1 -> X;
	2 -> -X
    end.

-spec rand_non_neg_float() -> float().
rand_non_neg_float() ->
    rand_non_neg_float(?DEFAULT_RNG_CONST).

-spec rand_non_neg_float(non_neg_integer()) -> float().
rand_non_neg_float(Const) when is_integer(Const) andalso Const >= 0 ->
    case random:uniform() of
	1.0 -> rand_non_neg_float(Const);
	X   -> Const * zero_one_to_zero_inf(X)
    end.

-spec rand_float(float(), float()) -> float().
rand_float(Low, High) when is_float(Low) andalso is_float(High)
		      andalso Low =< High ->
    Low + random:uniform() * (High - Low).

-spec zero_one_to_zero_inf(float()) -> float().
%% This function must return only non-negative values and map 0.0 to 0.0, but
%% may be undefined at 1.0.
%% TODO: read global options and decide here which bijection to use
zero_one_to_zero_inf(X) ->
    10 * X / math:sqrt(1 - X*X).

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
