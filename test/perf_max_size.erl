%%---------------------------------------------------------------------------
%% From: Jeff Hlywa
%% Subject: Increasing max_size significantly degrades performance.
%%---------------------------------------------------------------------------
%% Using the code below I get:
%% 1> timer:tc(fun() ->
%%      proper:quickcheck(perf_max_size:prop_identity(), [5, {max_size, 42}])
%%    end).
%% .....
%% OK: Passed 5 test(s).
%% {8170,true}
%%
%% 2> timer:tc(fun() ->
%%      proper:quickcheck(perf_max_size:prop_identity(),
%%                        [5, {max_size, 16#ffffffff}])
%%    end).
%% ....
%% OK: Passed 5 test(s).
%% {658751072,true}
%%
%% Not able to determine the cause of the slowdown, but it's significant.
%% ---------------------------------------------------------------------------
%% Fixed on 29/3/2013. The fix was that when increasing the size, move
%% to the next value immediately instead of trying each value one-by-one.
%% ---------------------------------------------------------------------------
-module(perf_max_size).
-export([prop_identity/0]).

-include_lib("proper/include/proper.hrl").

-record(msg, {a = 0 :: 0..16#ffffffff, b = 0 :: 0..16#f}).
-type msg() :: #msg{}.

prop_identity() ->
    ?FORALL(Msg, msg(), Msg =:= decode(encode(Msg))).

-spec encode(msg()) -> bitstring().
encode(#msg{a = A, b = B}) ->
    <<A:32, B:4>>.

-spec decode(bitstring()) -> msg().
decode(<<A:32, B:4>>) ->
    #msg{a = A, b = B}.

