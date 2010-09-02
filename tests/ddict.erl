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

%%% @author Manolis Papadakis <manopapad@gmail.com>
%%% @copyright 2010 Manolis Papadakis and Kostis Sagonas
%%% @version {@version}
%%% @doc Parametric wrapper to dict module.

-module(ddict).

-export([new/0,is_key/2,to_list/1,from_list/1,size/1]).
-export([fetch/2,find/2,fetch_keys/1,erase/2]).
-export([store/3,append/3,append_list/3,update/3,update/4,update_counter/3]).
-export([fold/3,map/2,filter/2,merge/3]).

%% Parametric opaques are currently not supported by Dialyzer, thus this module
%% won't pass the analysis.
-export_type([ddict/2]).

%% This would normally contain the internal representation of the ADT.
%% This representation won't actually be used, so we could just use a dummy one.
%% As with specs, unbound type variables are not allowed in '-type' declarations
%% unless they begin with an underscore.
-opaque ddict(_K,_V) :: dict().

%% Here are some valid symbolic calls that could be automatically produced using
%% this module's exported functions, for the type ddict(atom(),integer()):
%% * {'$call',ddict,store,[aa,12,{'$call',ddict,new,[]}]}
%% * {'$call',ddict,filter,[<fun>,{'$call',ddict,from_list,[[{a,1},{b,2}]]}]}
%% * {'$call',ddict,merge,[<fun>, {'$call',ddict,from_list,[[]]},
%%			   {'$call',ddict,update,
%%			    [aa,<fun>,3,{'$call',ddict,new,[]}]}]}

%% Notice that PropEr will never produce a call like this one:
%%   {'$call',ddict,update,[aa,<fun>,{'$call',ddict,new,[]}]}
%% which would raise an exception if we tried to evaluate it.

-spec new() -> ddict(_K,_V).
new() -> dict:new().

-spec is_key(K, ddict(K,_V)) -> boolean().
is_key(Key, Dict) -> dict:is_key(Key, Dict).

-spec to_list(ddict(K,V)) -> [{K,V}].
to_list(Dict) -> dict:to_list(Dict).

-spec from_list([{A,B}]) -> ddict(A,B).
from_list(KVList) -> dict:from_list(KVList).

-spec size(ddict(_K,_V)) -> non_neg_integer().
size(Dict) -> dict:size(Dict).


-spec fetch(K, ddict(K,V)) -> V.
fetch(Key, Dict) -> dict:fetch(Key, Dict).

-spec find(K, ddict(K,V)) -> {'ok', V} | 'error'.
find(Key, Dict) -> dict:find(Key, Dict).

-spec fetch_keys(ddict(K,_V)) -> [K].
fetch_keys(Dict) -> dict:fetch_keys(Dict).

-spec erase(K, ddict(K,V)) -> ddict(K,V).
erase(Key, Dict) -> dict:erase(Key, Dict).


-spec store(K, V, ddict(K,V)) -> ddict(K,V).
store(Key, Value, Dict) -> dict:store(Key, Value, Dict).

%% TODO: This is currently unacceptable - only simple variables can be used as
%%	 ADT parameters.
-spec append(K, V, ddict(K,[V])) -> ddict(K,[V]).
append(Key, Value, Dict) -> dict:append(Key, Value, Dict).

%% TODO: This is currently unacceptable - only simple variables can be used as
%%	 ADT parameters.
-spec append_list(K, [V], ddict(K,[V])) -> ddict(K,[V]).
append_list(Key, Values, Dict) -> dict:append_list(Key, Values, Dict).

-spec update(K, fun((V) -> V), ddict(K,V)) -> ddict(K,V).
update(Key, Value, Dict) -> dict:update(Key, Value, Dict).

-spec update(K, fun((V) -> V), V, ddict(K,V)) -> ddict(K,V).
update(Key, Value, Initial, Dict) -> dict:update(Key, Value, Initial, Dict).

%% TODO: This is currently unacceptable - only simple variables can be used as
%%	 ADT parameters.
-spec update_counter(K, number(), ddict(K,number())) -> ddict(K,number()).
update_counter(Key, Number, Dict) -> dict:update_counter(Key, Number, Dict).


-spec fold(fun((K,V,A) -> A), A, ddict(K,V)) -> A.
fold(Fun, Acc0, Dict) -> dict:fold(Fun, Acc0, Dict).

-spec map(fun((K,V1) -> V2), ddict(K,V1)) -> ddict(K,V2).
map(Fun, Dict) -> dict:map(Fun, Dict).

-spec filter(fun((K,V) -> boolean()), ddict(K,V)) -> ddict(K,V).
filter(Fun, Dict) -> dict:filter(Fun, Dict).

-spec merge(fun((K,V,V) -> V), ddict(K,V), ddict(K,V)) -> ddict(K,V).
merge(Fun, Dict1, Dict2) -> dict:merge(Fun, Dict1, Dict2).
