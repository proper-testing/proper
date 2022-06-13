%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr. If not, see <http://www.gnu.org/licenses/>.
%%%
%%% Alternatively, you may use this file under the terms of the Apache
%%% License, Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of
%%% the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% If you wish to allow use of your version of this file only under
%%% the terms of the Apache License, you should delete the provisions
%%% above and replace them with the notice and other provisions
%%% required by the Apache License; see
%%% <http://www.apache.org/licenses/LICENSE-2.0>. If you do not delete
%%% the provisions above, a recipient may use your version of this
%%% file under the terms of either the GNU General Public License or
%%% the Apache License.
%%%
%%% @doc This module is a smoke test of the Erlang abstract code generator.
-module(erlang_abstract_code_test).

-export([bits/0, expr/0, guard/0, term/0, module/0]).

-include_lib("proper/include/proper.hrl").

bits() ->
    Opts = [
            {weight, {bitstring, 20}}
           ,{weight, {blc, 10}}
           ,{weight, {blc_gen, 100}}
           ,{weight, {'receive', 0}}
           ],
    expr(Opts).

expr() ->
    expr([]).

expr(Opts) ->
    E = proper_erlang_abstract_code:expr(Opts),
    ?FORALL(X, E, check_pp(erl_pp:expr(X))).

guard() ->
    G = proper_erlang_abstract_code:guard(),
    ?FORALL(X, G, check_pp(erl_pp:guard((X)))).

term() ->
    T = proper_erlang_abstract_code:term(),
    ?FORALL(X, T, check_pp(erl_pp:expr(X))).

module() ->
    %% enable some elements which are off by default
    Opts = [{weight, {D, 1}} || D <- [type_decl, function_spec, termcall]],
    P = proper_erlang_abstract_code:module(Opts),
    ?FORALL(X, P, lists:all(fun(F) -> check_pp(erl_pp:form(F)) end, X)).

check_pp(S) ->
    string:find(S, "INVALID-FORM") =:= nomatch.
