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

%%% User header file
%%% This should be included in each file containing user type declarations
%%% and/or properties to be tested.

-include("proper_common.hrl").


%%------------------------------------------------------------------------------
%% Usage macros
%%------------------------------------------------------------------------------

-define(FORALL(X,RawType,Prop), {'$forall',RawType,fun(X) -> Prop end}).
-define(IMPLIES(Pre,Prop), {'$implies',Pre,?DELAY(Prop)}).
-define(COLLECT(Category,Prop), {'$collect',Category,Prop}).
-define(WHENFAIL(Action,Test), {'$whenfail',?DELAY(Action),?DELAY(Test)}).
-define(NUMTESTS(N,Test), {'$numtests',N,Test}).
-define(FAILS(Test), {'$fails',Test}).
-define(TRAPEXIT(Test), {'$trapexit',?DELAY(Test)}).
%% TODO: -define(TIMEOUT(Limit,Test), {'$timeout',Limit,?DELAY(Test)}).


%%------------------------------------------------------------------------------
%% Default types
%%------------------------------------------------------------------------------

-import(proper_types, [integer/2, float/2, atom/0, binary/0, bitstring/0,
		       list/1, vector/2, union/1, weighted_union/1, tuple/1,
		       exactly/1, fixed_list/1]).


%%------------------------------------------------------------------------------
%% Type aliases
%%------------------------------------------------------------------------------

-import(proper_types, [integer/0, non_neg_integer/0, pos_integer/0,
		       neg_integer/0, range/2, float/0, non_neg_float/0,
		       number/0, boolean/0, byte/0, char/0, string/0,
		       wunion/1]).


%%------------------------------------------------------------------------------
%% QuickCheck compatibility
%%------------------------------------------------------------------------------

-import(proper_types, [int/0, int/2, bool/0, choose/2, elements/1,
		       oneof/1, frequency/1]).
-import(proper_types, [resize/2, relimit/2, non_empty/1]).
-import(proper, [numtests/2, collect/2, fails/1]).
