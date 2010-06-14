%%% Copyright 2010 Manolis Papadakis (manopapad@gmail.com)
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

%% TODO: make all these into parameters, store them in process registry
-define(DEFAULT_RNG_CONST, 10).
-define(MAX_TRIES_TO_SATISFY_CONSTRAINTS, 50).
-define(MAX_RANDOM_TRIES_WHEN_SHRINKING, 5).
-define(MAX_LIST_LEN, 200).
-define(MAX_BINARY_LEN, 300).


%%------------------------------------------------------------------------------
%% Common Dialyzer type aliases
%%------------------------------------------------------------------------------

-type size() :: non_neg_integer().
-type length() :: non_neg_integer().
-type position() :: pos_integer().
-type frequency() :: pos_integer().


%%------------------------------------------------------------------------------
%% Options record
%%------------------------------------------------------------------------------

-record(opts, {quiet       = false :: boolean(),
	       numtests    = 100   :: pos_integer(),
	       max_shrinks = 300   :: non_neg_integer(),
	       expect_fail = false :: boolean(),
	       try_shrunk  = false :: boolean(),
	       shrunk              :: proper:testcase()}).
