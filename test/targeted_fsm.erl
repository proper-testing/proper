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

%%% @copyright 2020 Spiros Dontas and Kostis Sagonas
%%% @version {@version}
%%% @author Spiros Dontas

%% This file is only present to make sure that the proper_fsm TPBT works
%% properly, as well as increase the coverage.

-module(targeted_fsm).
-behaviour(proper_fsm).

-export([initial_state/0, initial_state_data/0, precondition/4,
         next_state_data/5, postcondition/5, empty_ets/1, non_empty_ets/1]).

-include_lib("proper/include/proper.hrl").

-define(MAX_SIZE, 15).
-define(TAB, targeted_statem_table).


%% -----------------------------------------------------------------------------
%% proper_fsm callbacks
%% -----------------------------------------------------------------------------

unique_key(S) ->
  ?SUCHTHAT(I, integer(1, 100), not lists:keymember(I, 1, S)).

key(S) ->
  oneof([oneof([Key || {Key, _} <- S]),
         unique_key(S)]).

value() ->
  integer().

initial_state() -> empty_ets.

initial_state_data() -> [].

precondition(_, non_empty_ets, _S, {call, ets, insert, _}) -> true;
precondition(_, non_empty_ets, _S, {call, ets, lookup, _}) -> true;
precondition(_, empty_ets, _S, {call, ets, delete_all_objects, _}) -> true.

next_state_data(_, _, S, _V, {call, ets, insert, [_, {Key, Value}]}) ->
  case proplists:is_defined(Key, S) of
    true -> lists:keyreplace(Key, 1, S, {Key, Value});
    false -> [{Key, Value} | S]
  end;
next_state_data(_, _, S, _V, {call, ets, lookup, _}) -> S;
next_state_data(_, _, _S, _V, {call, ets, delete_all_objects, [_]}) -> [].

postcondition(_, _, S, {call, ets, insert, [_, {Key, _Value}]}, _R) ->
  proplists:is_defined(Key, S) orelse (length(S) + 1 < ?MAX_SIZE);
postcondition(_, _, S, {call, ets, lookup, [_, Key]}, R) ->
  proplists:lookup_all(Key, S) =:= R;
postcondition(_, _, _S, _C, _R) -> true.

empty_ets(S) ->
  [{non_empty_ets, {call, ets, insert, [?TAB, {unique_key(S), value()}]}}].

non_empty_ets(S) ->
  [{empty_ets, {call, ets, delete_all_objects, [?TAB]}},
   {history, {call, ets, insert, [?TAB, {key(S), integer()}]}},
   {history, {call, ets, lookup, [?TAB, key(S)]}}].


%% -----------------------------------------------------------------------------
%% Properties
%% -----------------------------------------------------------------------------

prop_random() ->
  ?FORALL(Cmds, proper_fsm:commands(?MODULE),
          begin
            catch ets:delete(?TAB),
            ?TAB = ets:new(?TAB, [set, public, named_table]),
            {H, _S, Res} = proper_fsm:run_commands(?MODULE, Cmds),
            ets:delete(?TAB),
            aggregate(zip(proper_fsm:state_names(H), command_names(Cmds)),
                      Res =:= ok)
          end).

prop_targeted() ->
  ?FORALL_TARGETED(
     Cmds, proper_fsm:commands(?MODULE),
     begin
       catch ets:delete(?TAB),
       ?TAB = ets:new(?TAB, [set, public, named_table]),
       {H, {_, S}, Res} = proper_fsm:run_commands(?MODULE, Cmds),
       ets:delete(?TAB),
       ?MAXIMIZE(length(S)),
       aggregate(zip(proper_fsm:state_names(H), command_names(Cmds)),
                 Res =:= ok)
     end).

prop_targeted_init() ->
  ?FORALL_TARGETED(
     Cmds, proper_fsm:commands(?MODULE, {empty_ets, []}),
     begin
       catch ets:delete(?TAB),
       ?TAB = ets:new(?TAB, [set, public, named_table]),
       {H, {_, S}, Res} = proper_fsm:run_commands(?MODULE, Cmds),
       ets:delete(?TAB),
       ?MAXIMIZE(length(S)),
       aggregate(zip(proper_fsm:state_names(H), command_names(Cmds)),
                 Res =:= ok)
     end).
