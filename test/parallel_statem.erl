%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% From X4lldux
%%%
%%% When there was a crash in sequential phase of `run_parallel_commands`
%%% the returned result was `{exception,_,_,_}` tuple, but when a crash
%%% occurred in the parallel phase, then the `run_parallel_commands`
%%% also crashed.
%%%
%%% -------------------------------------------------------------------

-module(parallel_statem).

%% -compile(export_all).
-export([initial_state/0, next_state/3, precondition/2, postcondition/3]).
-export([foo/1, crash/0]).
-export([prop_parallel_crash/0, prop_sequential_crash/0]).

-include_lib("proper/include/proper.hrl").


initial_state() ->
    [].

precondition(_, _) ->
    true.

next_state(S, _V, {call,_,_,[_Arg]}) ->
    S.

postcondition(_, _, _) ->
    true.

foo(_) ->
    ok.

crash() ->
    erlang:error(boom).


parallel_crash_commands() ->
    exactly(
      {
       [{set,{var,1},{call,?MODULE,foo,[1]}}],       % seq
       [
        [{set,{var,2},{call,?MODULE,crash,[]}}],     % proc 1
        []                                           % proc 2
       ]
      }).

sequential_crash_commands() ->
    exactly(
      {
       [{set,{var,1},{call,?MODULE,crash,[]}}],      % seq
       [
        [{set,{var,2},{call,?MODULE,foo,[1]}}],      % proc 1
        []                                           % proc 2
       ]
      }).

prop_parallel_crash() ->
    ?FORALL(Cmds, parallel_crash_commands(),
            begin
                {_S,_P,Res} = run_parallel_commands(?MODULE, Cmds),
                assert_exception(Res)
            end).

prop_sequential_crash() ->
    ?FORALL(Cmds, sequential_crash_commands(),
            begin
                {_S,_P,Res} = run_parallel_commands(?MODULE, Cmds),
                assert_exception(Res)
            end).

assert_exception({exception, error, boom, _}) -> true;
assert_exception(_) -> false.
