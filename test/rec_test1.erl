%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2010-2022 Manolis Papadakis <manopapad@gmail.com>,
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

%%% @copyright 2010-2022 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Manolis Papadakis
%%% @doc This module contains types for testing the typeserver.

-module(rec_test1).
-export_type([a/0, aa/0, bb/0, cc/0, deeplist/0, e/0, f/0, expb/0, expc/0]).

-type a() :: 'aleaf' | b() | [{'rec', a()}] | c() | d().
-type b() :: 'bleaf' | {'bnode', b(), b()}.
-type c() :: [c()] | {'cnode1', a()} | {'cnode2',d()}.
-type d() :: [a()].
-type e() :: {'e', 'none' | e()}.
-type f() :: {'f', 'none'} | {'f', f()}.

-type deeplist() :: [deeplist()].

-type mylist(T) :: [] | {'cons', T, mylist(T)}.
-type aa() :: {} | mylist(aa()).
-type bb() :: mylist(integer()).
-type cc() :: mylist(cc()).

-record(rec, {a = 0 :: integer(), b = 'nil' :: 'nil' | #rec{}}).

-opaque expb() :: {'a', rec_test2:expa()}.

-type expc() :: 'c' | {'node', ?MODULE:expc()}.
