%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2020-     Manolis Papadakis <manopapad@gmail.com>,
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

%%% @copyright 2020 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Spiros Dontas

%% -----------------------------------------------------------------------------
%% Tests inspired by https://github.com/proper-testing/proper/issues/221
%% -----------------------------------------------------------------------------

-module(targeted_shrinking_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------
%% Generators
%% -----------------------------------------------------------------------------

let_int() ->
  ?LET(I, integer(), I).

nf_let_int() ->
  fun (Prev, _T) ->
      ?LET(I, integer(Prev, inf), I)
  end.

nf_int_shrink() ->
  fun (Prev, _T) ->
      ?SHRINK(integer(Prev, inf), [integer()])
  end.

int_user_nf() ->
  ?USERNF(integer(), nf_let_int()).

let_int_user_nf() ->
  ?USERNF(let_int(), nf_let_int()).

int_user_nf_shrink_inner() ->
  ?USERNF(let_int(), nf_int_shrink()).

int_user_nf_shrink_outer() ->
  ?USERNF(?SHRINK(let_int(), [integer()]),
          fun(Prev, _T) -> integer(Prev, inf) end).

normal_list() ->
  list(integer()).

let_list() ->
  ?LET(L, list(integer()), L).

nf_list() ->
  fun (Prev, _T) ->
      {Max, NewLen} = case Prev of
                        [] -> {0, 1};
                        _ -> {max(0, lists:max(Prev)), length(Prev)}
                      end,
      ?SHRINK(vector(NewLen * 2, integer(Max, inf)),
              [list(integer())])
  end.

normal_list_user_nf() ->
  ?USERNF(normal_list(), nf_list()).

let_list_user_nf() ->
  ?USERNF(let_list(), nf_list()).

%% -----------------------------------------------------------------------------
%% Properties
%% -----------------------------------------------------------------------------

property_int(I) ->
  ?MAXIMIZE(I),
  I < 500.

prop_int() ->
  ?FORALL_TARGETED(I, int_user_nf(), property_int(I)).

prop_let_int() ->
  ?FORALL_TARGETED(I, let_int_user_nf(), property_int(I)).

prop_int_shrink_outer() ->
  ?FORALL_TARGETED(I, int_user_nf_shrink_outer(), property_int(I)).

prop_int_shrink_inner() ->
  ?FORALL_TARGETED(I, int_user_nf_shrink_inner(), property_int(I)).

property_list(L) ->
  ?MAXIMIZE(lists:sum(L)),
  lists:sum(L) < 1000.

prop_normal_list() ->
  ?FORALL_TARGETED(L, normal_list_user_nf(), property_list(L)).

prop_let_list() ->
  ?FORALL_TARGETED(L, let_list_user_nf(), property_list(L)).

%% -----------------------------------------------------------------------------
%% Tests
%% -----------------------------------------------------------------------------

normal_list_test() ->
  false = proper:quickcheck(prop_normal_list()),
  [L] = proper:counterexample(),
  ?_assert(lists:sum(L) =:= 1000).

let_list_test() ->
  false = proper:quickcheck(prop_let_list()),
  [L] = proper:counterexample(),
  ?_assert(lists:sum(L) =:= 1000).
