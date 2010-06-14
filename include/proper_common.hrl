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

%%% Common parts of user and internal header files


%%------------------------------------------------------------------------------
%% Generator macros
%%------------------------------------------------------------------------------

-define(FORCE(X), (X)()).
-define(DELAY(X), fun() -> X end).
-define(SIZED(SizeArg,Gen), proper_types:sized(fun(SizeArg) -> Gen end)).
-define(LET(X,RawType,Gen), proper_types:bind(RawType,fun(X) -> Gen end)).
-define(SHRINK(Gen,AltGens),
	proper_types:shrinkwith(?DELAY(Gen),?DELAY(AltGens))).
-define(LETSHRINK(Xs,RawType,Gen),
	proper_types:bind(RawType,
			  fun(Xs) ->
			      proper_types:shrinkwith(?DELAY(Gen),?DELAY(Xs))
			  end)).
-define(SUCHTHAT(X,RawType,Condition),
	proper_types:add_constraint(RawType,fun(X) -> Condition end,true)).
-define(SUCHTHATMAYBE(X,RawType,Condition),
	proper_types:add_constraint(RawType,fun(X) -> Condition end,false)).


%%------------------------------------------------------------------------------
%% Type declaration macros
%%------------------------------------------------------------------------------

-define(BASIC(PropList), proper_types:new_type(PropList,basic)).
-define(WRAPPER(PropList), proper_types:new_type(PropList,wrapper)).
-define(CONSTRUCTED(PropList), proper_types:new_type(PropList,constructed)).
-define(SEMI_OPAQUE(PropList), proper_types:new_type(PropList,semi_opaque)).
-define(OPAQUE(PropList), proper_types:new_type(PropList,opaque)).
-define(SUBTYPE(Type,PropList), proper_types:subtype(PropList,Type)).
