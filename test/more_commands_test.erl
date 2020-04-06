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
%% Test inspired by https://github.com/proper-testing/proper/issues/192
%%
-module(more_commands_test).

-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3, foo/0]).

-include_lib("proper/include/proper.hrl").

%%
%% The two properties that show diff of commands/1 with more_commands/2.
%%

prop_commands_passes() ->	% This property, most likely, passes
  ?FORALL(Cmds, commands(?MODULE),
	  measure("Length of Cmds", length(Cmds), length(Cmds) =< 42)).

prop_more_commands_fails() ->	% This property, most likely, fails
  ?FORALL(Cmds, more_commands(17, commands(?MODULE) ),
	  measure("Length of Cmds", length(Cmds), length(Cmds) =< 42)).

%%
%% Auxiliary functions below - not important for the test
%%

command(_S) ->	% just one command suffices for this test
  exactly({call,?MODULE,foo,[]}).

-spec initial_state() -> 'ok'.
initial_state() -> ok.
 
next_state(_, _, _) -> ok.

precondition(_, _) -> true.

postcondition(_, _, _) -> true.

foo() -> 42.
