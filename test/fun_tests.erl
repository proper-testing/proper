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

%%
%% Contains some tests for function generators
%%
-module(fun_tests).

-include_lib("proper/include/proper.hrl").

%%----------------------
%% Properties that pass
%%----------------------

prop_fun_bool() ->
  ?FORALL({F, X}, {function(1,boolean()), any()}, is_boolean(F(X))).

%%-----------------------------
%% Properties that should fail
%%-----------------------------

prop_fun_int_int() ->
  ?FORALL({F, X}, {function([integer()],integer()), integer()}, F(X) < 42).

prop_lists_map_filter() ->
  ?FORALL({MapFun, FilterFun, List},
	  {function([int()],any()), function([int()],bool()), list(int())},
	  lists:map(MapFun, lists:filter(FilterFun, List)) =:=
	    lists:filter(FilterFun, lists:map(MapFun, List))).
