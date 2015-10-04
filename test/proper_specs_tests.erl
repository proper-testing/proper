%%% Copyright 2010-2015 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
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

%%% @copyright 2010-2015 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>, Kostis Sagonas
%%% @doc This modules contains PropEr's Unit tests for check specs.
%%% You need the EUnit application to compile it.


-module(proper_specs_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% These are automatically exported
%% -export([check1_specs_test_/0, check2_specs_test_/0, check3_specs_test_/0]).

-export([test1_any/1,
         test2_skip/1,
         test3_fail/1,
         test4_fail_fp/2,
         test5_exc/2,
         test6_exc_fp/2,
         test7_exc_fp/2,
	 test_const_bitstrs/0,
	 t0/2, t1/2, t2/2, t3/2, tp1/2, tp2/2, tp3/2]).

check1_specs_test_() ->
    ?_test(?assert(check1_specs_test())).

check2_specs_test_() ->
    ?_test(?assert(check2_specs_test())).

check3_specs_test_() ->
    ?_test(?assert(check3_specs_test())).

%%------------------------------------------------------------------------------
%% Unit tests
%%------------------------------------------------------------------------------

check1_specs_test() ->
    Options = [quiet, long_result,
               {skip_mfas, [{?MODULE, check1_specs_test_, 0},
                            {?MODULE, check2_specs_test_, 0},
                            {?MODULE, check3_specs_test_, 0},
                            {?MODULE, t3, 2},
                            {?MODULE, tp3, 2},
                            {?MODULE, test2_skip, 1},
                            {?MODULE, test7_exc_fp, 2}]},
               {false_positive_mfas, fun check1_false_positive_mfas/3}],
    %% check for expected 1 test failure
    case proper:check_specs(?MODULE, Options) of
        [{{?MODULE, test5_exc, 2}, [_]}] ->
            true;
        Else ->
            error(failed, Else)
    end.

check2_specs_test() ->
    Options = [quiet, long_result,
               {skip_mfas, [{?MODULE, check1_specs_test_, 0},
                            {?MODULE, check2_specs_test_, 0},
                            {?MODULE, check3_specs_test_, 0},
                            {?MODULE, t3, 2},
                            {?MODULE, tp3, 2},
                            {?MODULE, test1_any, 1},
                            {?MODULE, test2_skip, 1},
                            {?MODULE, test3_fail, 1},
                            {?MODULE, test4_fail_fp, 2},
                            {?MODULE, test5_exc, 2},
                            {?MODULE, test6_exc_fp, 2}]},
               {false_positive_mfas, fun check2_false_positive_mfas/3}],
    %% check for expected 1 test failure
    case proper:check_specs(?MODULE, Options) of
        [{{?MODULE, test7_exc_fp, 2}, [[Exception,_]]}]
          when Exception =:= error; Exception =:= exit; Exception =:= throw ->
            true;
        Else ->
            error(failed, Else)
    end.

%% contains all functions whose specs are erroneous, so their check should fail
check3_specs_test() ->
    Fun = fun ({M, A}) -> false =:= proper:check_spec({?MODULE, M, A}) end,
    lists:all(Fun, [{t3, 2}, {tp3, 2}]).

%%------------------------------------------------------------------------------
%% Test helpers
%%------------------------------------------------------------------------------

-spec check1_false_positive_mfas(mfa(), Args::[term()], {fail,Result::term()} | {error | exit | throw,Reason::term()}) -> boolean().
check1_false_positive_mfas({?MODULE, test1_any, 1}, _Args, _) ->
    %% NG - should never be called
    false;
check1_false_positive_mfas({?MODULE, test2_skip, 1}, _Args, _) ->
    %% NG - should never be called
    false;
check1_false_positive_mfas({?MODULE, test3_fail, 1}, [Arg], {fail, {ng, Arg}}) ->
    %% OK
    true;
check1_false_positive_mfas({?MODULE, test4_fail_fp, 2}, [_X, _Y], {fail, _Result}) ->
    %% NG - should never match
    false;
check1_false_positive_mfas({?MODULE, test4_fail_fp, 2}, [_X, Y], {error, badarith}) ->
    %% OK
    Y == 0;
check1_false_positive_mfas({?MODULE, test5_exc, 2}, [Class, Args], {Class, Args}) ->
    %% OK
    false;
check1_false_positive_mfas({?MODULE, test6_exc_fp, 2}, [Class, Args], {Class, Args}) ->
    %% OK
    true.

-spec check2_false_positive_mfas(mfa(), Args::[term()], {fail,Result::term()} | {error | exit | throw,Reason::term()}) -> boolean().
check2_false_positive_mfas({?MODULE, test7_exc_fp, 2}, [Class, Args], {Class, Args}) ->
    %% OK
    erlang:Class(Args).

-spec test1_any(any()) -> any().
test1_any(Any) ->
    Any.

-spec test2_skip(any()) -> any().
test2_skip(Any) ->
    Any.

-spec test3_fail(any()) -> true.
test3_fail(Any) ->
    {ng, Any}.

-spec test4_fail_fp(number(), number()) -> number().
test4_fail_fp(X, Y) ->
    X / Y.

-spec test5_exc(error | exit | throw, badarg | any()) -> any().
test5_exc(Class, Any) ->
    erlang:Class(Any).

-spec test6_exc_fp(error | exit | throw, badarg | any()) -> any().
test6_exc_fp(Class, Any) ->
    erlang:Class(Any).

-spec test7_exc_fp(error | exit | throw, badarg | any()) -> any().
test7_exc_fp(Class, Any) ->
    erlang:Class(Any).

%% Tests constant (and quite weird) bitstr type specifications
-spec test_const_bitstrs() ->
    {<<_:16>>, <<_:16,_:_*0>>, <<_:16,_:_*1>>, <<_:16,_:_*3>>, <<_:8,_:_*8>>,
     <<_:17>>, <<_:17,_:_*0>>, <<_:17,_:_*1>>, <<_:11,_:_*3>>, <<_:8,_:_*3>>}.
test_const_bitstrs() ->
    Bin = <<"42">>,
    B17 = <<"42", 0:1>>,
    {Bin, Bin, Bin, Bin, Bin, B17, B17, B17, B17, B17}.

%%
%% Tests that type definitions are properly handled
%%

-spec t0(integer(), atom()) -> [{integer(), atom()}]. % only built-in types
t0(A, B) -> [{A, B}].

-type ia_pair() :: {integer(), atom()}.
-spec t1(integer(), atom()) -> [ia_pair()]. % shallow access to a type def
t1(A, B) -> [{A, B}].

-type ia_pair_list() :: [ia_pair()].
-spec t2(integer(), atom()) -> ia_pair_list(). % deep access to a type def
t2(A, B) -> [{A, B}].

-spec t3(integer(), atom()) -> [ia_pair_list()].
t3(A, B) -> [{B, A}]. % fails

-type param_pair(X, Y) :: {X, Y}. % shallow parametric type
-spec tp1(integer(), atom()) -> [param_pair(integer(), atom())].
tp1(A, B) -> [{A, B}].

-type param_pair_list(X, Y) :: [param_pair(X, Y)]. % deep parametric type
-spec tp2(integer(), atom()) -> param_pair_list(integer(), atom()).
tp2(A, B) -> [{A, B}].

-spec tp3(integer(), atom()) -> param_pair_list(integer(), atom()).
tp3(A, B) -> [{B, A}]. % fails
