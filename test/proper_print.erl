%%% Copyright 2010-2011 Manolis Papadakis <manopapad@gmail.com>,
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

%%% @copyright 2010-2011 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Manolis Papadakis

%%% @doc This module contains tests to check the information printed by proper
%%% on the shell

-module(proper_print).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-spec test() -> ok.

%% Test that the stacktrace is not empty when something crashes in the property
-spec stacktrace_test_() -> any().
stacktrace_test_() ->
    [?_assertThrow({stacktrace, [_|_]},
                   proper:quickcheck(bad_proper_call_property())),
     ?_assertThrow({stacktrace, [_|_]},
                   proper:quickcheck(bad_call_property()))].

-spec set_stacktrace_thrower(any()) -> any().
set_stacktrace_thrower(Prop) ->
    proper:on_output(fun throw_stacktrace/2, Prop).

-spec throw_stacktrace(string(), list()) -> ok.
throw_stacktrace("Stacktrace: ~p.~n", [Stacktrace]) ->
    throw({stacktrace, Stacktrace});
throw_stacktrace(_, _) ->
    ok.

-spec bad_proper_call_property() -> any().
bad_proper_call_property() ->
    set_stacktrace_thrower(?FORALL(_X, proper_types:int(), proper:foo())).

-spec bad_call_property() -> any().
bad_call_property() ->
    set_stacktrace_thrower(?FORALL(_X, proper_types:int(), foo:bar())).
