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

-module(proper_exported_types_test).
-export([not_handled/0]).

%%
%% Checks that the automatic translation of types to generators can handle all
%% types (structured + opaque) which are exported by some module of PropEr.
%% To do so, it tests that the following functions:
%%   - `proper_typeserver:demo_translate_type/2`,
%%   - `proper_typeserver:demo_is_instance/3`, and
%%   - `proper_gen:pick/1`
%% do not return errors when they are fed these types.
%%
%% OBS: This test depends on the set of exported types from PropEr modules!
%%
%% Still, the test is currently not 100% there.
%% TODOs:
%%   - Eliminate the 12 errors that `proper_typeserver:demo_translate_type/2`
%%     currently returns. (Three of these errors are due to the incomplete
%%     handling of maps.)
%%   - Handle symbolic instances (the {'$call', ...} case below).
%%

not_handled() ->
  Beams = filelib:wildcard("ebin/*.beam"),   % eunit executes from the top dir
  MTs = lists:flatmap(fun get_exported_types/1, Beams),
  R = [{M,T,A,proper_typeserver:demo_translate_type(M, stringify(T, A))}
       || {M,T,A} <- MTs],
  {OKs,Errors} = lists:partition(fun type_translation_is_ok/1, R),
  {[Inst || TGen <- OKs, (Inst = pick_instance(TGen)) =/= ok], length(Errors)}.

pick_instance({M,T,A,{ok,Gen}}) ->
  {ok,Inst} = proper_gen:pick(Gen),
  ok = proper:global_state_erase(),
  case Inst of
    {'$call',_Mod,_Fun,_Args} -> ok; % TODO: handle symbolic instances
      %% try proper_symb:eval(Inst)
      %% catch _ -> io:format("~p~n", [{M,T,A}]), Inst end;
    _ ->
      case proper_typeserver:demo_is_instance(Inst, M, stringify(T, A)) of
	true  -> ok;
        false -> {M,T,A,Inst,Gen}
      end
  end.

type_translation_is_ok({_M,_T,_A,{error,_}}) -> false;
type_translation_is_ok({_M,_T,_A,{ok,{'$type',_}}}) -> true.

%% Assumes that polymorphic types have at most two parameters.
stringify(T, 0) -> atom_to_list(T)++"()";
stringify(T, 1) -> atom_to_list(T)++"(any())";
stringify(T, 2) -> atom_to_list(T)++"(any(),any())".

get_exported_types(Beam) ->
  {ok,{M,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Beam, [abstract_code]),
  ExpTypes = lists:append([TList || {attribute,_,export_type,TList} <- AC]),
  [{M,T,A} || {T,A} <- ExpTypes].
