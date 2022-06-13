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
%%% @author Kostis Sagonas

-module(shrinking_gotchas).

-include_lib("proper/include/proper.hrl").

%%
%% Test inspired by https://github.com/proper-testing/proper/issues/116
%% crash shrinking a list (length on improper list)
%%
prop_shrink_list_same_elem() ->
    ?FORALL(L, bad_gen(), length(L) < 5).

bad_gen() ->
    ?LET(L, proper_types:list(a), permutation(L)).

permutation([]) -> [];
permutation(L) ->
    ?LET(E, proper_types:elements(L), [E | permutation(lists:delete(E, L))]).
