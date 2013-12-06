%%% Copyright 2010-2013 Manolis Papadakis <manopapad@gmail.com>,
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

%%% @copyright 2010-2013 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>
%%% @doc This modules contains PropEr's Unit tests for check
%%% specs. You need the EUnit application to compile it.


-module(proper_specs_tests).

-include("proper.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([check1_specs_test_/0,
         check2_specs_test_/0]).

-export([test1_any/1,
         test2_skip/1,
         test3_fail/1,
         test4_fail_fp/2,
         test5_exc/2,
         test6_exc_fp/2,
         test7_exc_fp/2,
	 test_const_bitstrs/0]).

check1_specs_test_() ->
    ?_test(?assert(check1_specs_test())).

check2_specs_test_() ->
    ?_test(?assert(check2_specs_test())).

%%------------------------------------------------------------------------------
%% Unit tests
%%------------------------------------------------------------------------------

check1_specs_test() ->
    Options = [quiet, long_result,
               {skip_mfas, [{?MODULE, check1_specs_test_, 0},
                            {?MODULE, check2_specs_test_, 0},
                            {?MODULE, test2_skip, 1},
                            {?MODULE, test7_exc_fp, 2}]},
               {false_positive_mfas, fun check1_false_positive_mfas/3}],

    %% check for expected 1 test failure
    case proper:check_specs(?MODULE, Options) of
        [{{proper_specs_tests, test5_exc, 2}, [_]}] ->
            true;
        Else ->
            error(failed, Else)
    end.

check2_specs_test() ->
    Options = [quiet, long_result,
               {skip_mfas, [{?MODULE, check1_specs_test_, 0},
                            {?MODULE, check2_specs_test_, 0},
                            {?MODULE, test1_any, 1},
                            {?MODULE, test2_skip, 1},
                            {?MODULE, test3_fail, 1},
                            {?MODULE, test4_fail_fp, 2},
                            {?MODULE, test5_exc, 2},
                            {?MODULE, test6_exc_fp, 2}]},
               {false_positive_mfas, fun check2_false_positive_mfas/3}],

    %% check for expected 1 test failure
    case proper:check_specs(?MODULE, Options) of
        [{{proper_specs_tests, test7_exc_fp, 2}, [[Exception,_]]}]
          when Exception==error; Exception==exit; Exception==throw ->
            true;
        Else ->
            error(failed, Else)
    end.

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
