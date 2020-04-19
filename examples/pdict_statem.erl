%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
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
%%% @author Kresten Krab Thorup, edited by Eirini Arvaniti
%%% @doc Simple statem test for a modified API to the process dictionary

-module(pdict_statem).
-behaviour(proper_statem).

-export([get/1, put/2, erase/1]).
-export([test/0, test/1]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).

-include_lib("proper/include/proper.hrl").

-define(KEYS, [a,b,c,d]).

%%% New API in which only get/1 returns an interesting value.
get(K) -> erlang:get(K).
put(K,V) ->
    erlang:put(K,V),
    ok.
erase(K) ->
    erlang:erase(K),
    ok.

%% A simple statem test for the process dictionary; tests the
%% operations ?MODULE:put/2, ?MODULE:get/1, ?MODULE:erase/1.

test() ->
    test(100).

test(N) ->
    proper:quickcheck(?MODULE:prop_pdict(), N).

prop_pdict() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                {H,S,Res} = run_commands(?MODULE, Cmds),
                clean_up(),
                ?WHENFAIL(
                   begin
                       {_, _, Msg} = Res,
                       io:format("History: ~p~nState: ~p~nMsg: ~s~n",
                                 [H, S, Msg])
                   end,
                   aggregate(command_names(Cmds), Res =:= ok))
            end).

clean_up() ->
    lists:foreach(fun(Key) -> erlang:erase(Key) end, ?KEYS).

key() ->
    elements(?KEYS).

initial_state() -> [].

command([]) ->
    {call,?MODULE,put,[key(), integer()]};
command(Props) ->
    ?LET({Key,Value}, weighted_union([{2, elements(Props)},
                                      {1, {key(),integer()}}]),
         oneof([{call,?MODULE,put,[Key,Value]},
                {call,?MODULE,get,[Key]},
                {call,?MODULE,erase,[Key]}
               ])).

precondition(_Props, {call,?MODULE,put,[_Key,_Val]}) ->
    true;
precondition(_Props, {call,?MODULE,get,[_Key]}) ->
    true;
precondition(Props, {call,?MODULE,erase,[Key]}) ->
    proplists:is_defined(Key, Props);
precondition(_, _) ->
    false.

postcondition(_Props, {call,?MODULE,put,[Key,Val]}, Result) ->
    Want = ok,
    Pred = (Result =:= Want),
    Msg = io_lib:format("Want put(~p, ~p) to return ~p, got ~p\n", [Key, Val, Want, Result]),
    {Pred, Msg};
postcondition(Props, {call,?MODULE,get,[Key]}, Result) ->
    Want = case proplists:lookup(Key, Props) of
                none -> undefined;
                {Key, Val} -> Val
           end,
    Pred = (Result =:= Want),
    Msg = io_lib:format("Want get(~p) to return ~p, got ~p\n", [Key, Want, Result]),
    {Pred, Msg};
postcondition(_Props, {call,?MODULE,erase,[_Key]}, Result) ->
    Want = ok,
    Pred = (Result =:= Want),
    Msg = io_lib:format("Want Result to be ~p, got ~p\n", [Want, Result]),
    {Pred, Msg};
postcondition(_, _, _) ->
    Msg = "Default case",
    {false, Msg}.

next_state(Props, _Var, {call,?MODULE,put,[Key,Value]}) ->
    NewProps = [{Key,Value}|proplists:delete(Key, Props)],
    NewProps;
next_state(Props, _Var, {call,?MODULE,erase,[Key]}) ->
    NewProps = proplists:delete(Key, Props),
    NewProps;
next_state(Props, _Var, {call,?MODULE,get,[_]}) ->
    Props.
