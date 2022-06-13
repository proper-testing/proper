%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2010-2020 Manolis Papadakis <manopapad@gmail.com>,
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

%%% @copyright 2010-2020 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Manolis Papadakis
%%% @doc This module tests a weird scenario for the parse transform.

-module(weird_types).

%% NOTE: contains export_all because it tests that the auto-export of
%% foo/0's definition takes precedence over the definition of foo()'s
%% type declaration.
-compile([export_all, nowarn_export_all]).
-compile([{no_auto_import, [hd/1]}]).

-include_lib("proper/include/proper.hrl").

-export_type([foo/0]).
%% NOTE: Possibly here temporarily until the compiler's warnings are fixed.
-export_type([hd/1]).

-type foo() :: atom().
foo() -> integer().
-type hd(T) :: {'head', T}.

prop_export_all_works() ->
    ?FORALL(X, ?MODULE:foo(), is_integer(X)).

prop_no_auto_import_works() ->
    ?FORALL(X, hd([42]), is_tuple(X)).
