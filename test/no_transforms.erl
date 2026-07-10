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
%%% @doc This module tests whether proper works with parse transforms disabled.

-module(no_transforms).
-export([prop_1/0, prop_2/0]).

-define(PROPER_NO_TRANS, true).

-include_lib("proper/include/proper.hrl").

-type local_integer() :: integer().
-type local_float() :: float().

prop_1() -> ?FORALL(X, ?TYPE(local_integer() | local_float()), is_number(X)).

prop_2() -> ?FORALL(X, native_type(), is_integer(X) orelse is_atom(X)).

native_type() ->
    ?TYPE(local_integer() | atom()).
