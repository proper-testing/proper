%%% Copyright 2010 Manolis Papadakis (manopapad@gmail.com)
%%%            and Kostis Sagonas (kostis@cs.ntua.gr)
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

%%% Internal header file
%%% This header is included in all PropEr source files.

-include("proper_common.hrl").


%%------------------------------------------------------------------------------
%% Constants
%%------------------------------------------------------------------------------

-define(MAX_LIST_LEN, 200).
-define(MAX_ATOM_LEN, 255).
-define(MAX_BINARY_LEN, 300).
-define(SEED_RANGE, 4294967296).


%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------

-define(AND3(X,Y), proper_arith:and3(?DELAY(X),?DELAY(Y))).
-define(OR3(X,Y), proper_arith:or3(?DELAY(X),?DELAY(Y))).


%%------------------------------------------------------------------------------
%% Common type aliases
%%------------------------------------------------------------------------------

-type size() :: non_neg_integer().
-type length() :: non_neg_integer().
-type position() :: pos_integer().
-type frequency() :: pos_integer().
