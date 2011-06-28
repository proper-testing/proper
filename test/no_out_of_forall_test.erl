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
%%% @author Manolis Papadakis
%%% @doc This module tests whether native types are parsed outside of ?FORALLs.

-module(no_out_of_forall_test).
-export([]).

-type local_type() :: integer().
-type parametric(T) :: {0,T}.

-include_lib("proper/include/proper.hrl").

foo() -> ?LET(X, types_test1:exp1(), {42,X}).

bar2() -> ?LAZY(rec_props_test2:exp2()).
bar3() -> ?SHRINK(local_type(), [42, local_type()]).
bar4() -> ?SIZED(Size, resize(Size * 2, parametric(local_type()))).

prop_1() -> ?FORALL(_, foo(), true).
prop_2() -> ?FORALL(_, bar2(), true).
prop_3() -> ?FORALL(_, bar3(), true).
prop_4() -> ?FORALL(_, bar4(), true).
