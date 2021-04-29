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
%%% @doc PropEr generator of abstract code
%%%
%%% <p>This module is a PropEr generator for abstract code. It
%%% generates guards, expressions, programs (modules), and terms. It
%%% does not generate macros or other attributes than `function',
%%% `record', `spec', and `type'. The generated programs (guards,
%%% expressions) can be used for testing the Compiler or other modules
%%% traversing programs as abstract forms. Typical examples of the
%%% latter are <code>erl_eval</code>, <code>erl_pp</code>,
%%% <code>erl_prettypr</code> (Syntax Tools), and parse transforms.
%%% Created modules should compile without errors, but will most likely
%%% crash immediately when invoked.</p>
%%%
%%% <p>This is an example how to test the Compiler:</p>
%%%
%%% ```
%%% test() ->
%%%     ?FORALL(Abstr, proper_erlang_abstract_code:module(),
%%%             ?WHENFAIL(
%%%                begin
%%%                    io:format("~ts\n", [[erl_pp:form(F) || F <- Abstr]]),
%%%                    compile(Abstr, [report_errors])
%%%                end,
%%%                case compile(Abstr, []) of
%%%                    {error, _Es, _Ws} -> false;
%%%                    _ -> true
%%%                end)).
%%%
%%% compile(Abstr, Opts) ->
%%%     compile:noenv_forms(Abstr, Opts).
%%% '''
-module(proper_erlang_abstract_code).

-export([module/0, module/1, guard/0, guard/1, expr/0, expr/1]).

-export([term/0, term/1]).

%-compile(export_all). -compile(nowarn_export_all).

%-define(debug, true).
-ifdef(debug).
-define(DEBUG(F, As), io:format(F, As)).
-else.
-define(DEBUG(F, AS), ok).
-endif.

-include("proper_internal.hrl").

-type char_fun() :: fun(() -> proper_types:type()).
%%% A function that generates characters. The default function
%%% chooses from <code>$a..$z | $A..$Z</code>.

-type atom_fun() :: fun(() -> proper_types:type()).
%%% A function that generates atoms. The default function chooses
%%% from 100 common English words.

-type weight() :: non_neg_integer().
-type limit() :: non_neg_integer().

-type option() ::
        {'variables', [atom()]} |
        {'weight', {Key :: atom(), Weight :: weight()}} |
        {'function', [{FunctionName :: atom(), Arity :: arity()}]} |
        {'types', [{TypeName :: atom(), NumOfParms :: arity()}]} |
        {'records', [{RecordName:: atom(), [FieldName :: atom()]}]} |
        {'limit', [{Name :: atom(), Limit :: limit()}]} |
        {'char', char_fun()} |
        {'atom', atom_fun()} |
        {'set_all_weights', weight()}.
%%% See description below.

-type fa() :: {atom(), arity()}.   % function+arity
-type ta() :: {atom(), arity()}.   % type+arity
-type rec() :: {RecordName :: atom(), [FieldName :: atom()]}.

-record(gen_state,
        {
         size = 0 :: proper_gen:size(),
         result_type = 'program' :: 'program' | 'guard' | 'expr' | 'term',
         functions = [] :: [fa()],
         functions_and_auto_imported = [] :: [{weight(), fa()}],
         expr_bifs = [] :: [fa()],
         guard_bifs = [] :: [fa()],
         named_funs = [] :: [fa()],
         records = [] :: [rec()],
         guard_records = [] :: [rec()],
         types = [] :: [ta()],
         predef_types = [] :: [ta()],
         module :: module(),
         options = [] :: [option()],
         weights = #{} :: #{Key :: atom() => Weight :: weight()},
         limits = #{} :: #{Key :: atom() => Limit :: limit()},
         variables = ordsets:new() :: ordsets:ordset(atom()),
         simple_char = fun default_simple_char/0 :: char_fun(),
         atom = fun default_atom/0 :: atom_fun(),
         resize = 'false' :: boolean()
        }).

-record(post_state,
        {
         context = 'expr' :: 'expr' | 'type' | 'record' | 'pattern',
         vars = ordsets:new() :: ordsets:ordset(atom()),
         vindex = 0 :: non_neg_integer(),
         forbidden = ordsets:new() :: ordsets:ordset(atom()),
         known_functions = [] :: [fa()],
         atom = fun default_atom/0 :: atom_fun()
        }).

-define(DEFAULT_SMALL_WEIGHT_PROGRAM, 50). % Needs to be quite high.
-define(DEFAULT_SMALL_WEIGHT_TERM, 50).

-define(MAX_CALL_ARGS, 2).
-define(MAX_FUNCTION_CLAUSES, 2).

-define(MAX_QUALIFIERS, 2).
-define(MAX_IF_CLAUSES, 2).
-define(MAX_CATCH_CLAUSES, 2).
-define(MAX_CLAUSES, 2).
-define(MAX_BODY, 2).
-define(MAX_GUARD, 2).
-define(MAX_GUARD_TESTS, 2).
-define(MAX_MAP, 2).
-define(MAX_TYPE_SPECIFIER, 2).
-define(MAX_RECORD_FIELDS, 3).
-define(MAX_TUPLE, 2).
-define(MAX_BIN_ELEMENTS, 2).

-define(MAX_FUNCTION_TYPES, 2).
-define(MAX_FUNCTION_CONSTRAINTS, 2).
-define(MAX_UNION_TYPES, 4).
-define(MAX_TUPLE_TYPES, 2).

-define(MAX_LIST, 4).
-define(MAX_STRING, 4).

%%% "div 2" is just a suggestion.
-define(RESIZE(S), S#gen_state{size = S#gen_state.size div 2}).

%%% @doc Returns abstract code of a term that can be handled by
%%% <code>erl_parse:normalise/0</code>.

%%% No pid() or port().

-spec term() -> proper_types:type().

term() ->
    term([]).

%%% @doc Same as {@link term/0}, but accepts a list of options.
%%% === Options ===
%%%
%%% Many options are the same as the ones for {@link module/1}.
%%% <ul>
%%% <li><code>{atom, {@link atom_fun()}}</code> - A atom generating
%%%    function to replace the default.</li>
%%% <li><code>{char, {@link char_fun()}}</code> - A character generating
%%%    function to replace the default. The function is used when
%%%    generating strings and characters.</li>
%%% <li><code>{limit, [{Name, Limit}]}</code> - Set the limit of
%%%    <code>Name</code> to <code>Limit</code>. The limit names are:
%%%   <ul>
%%%     <li><code>bin_elements</code> - Number of segments of a bitstring.</li>
%%%     <li><code>list</code> - Number of elements of a plain list.</li>
%%%     <li><code>map</code> - Number of associations of a map.</li>
%%%     <li><code>string</code> - Number of characters of a string.</li>
%%%     <li><code>tuple</code> - Number of elements of a tuple.</li>
%%%   </ul>
%%% </li>
%%% <li><code>{resize, boolean()}</code> - Use <code>?SIZED</code>
%%%    to limit the size of the generated abstract code. With this
%%%    option set to <code>false</code> (the default) big code
%%%    may be generated among the first instances.</li>
%%% <li><code>{set_all_weights, Weight}</code> - Set the weight of
%%%    all keys to <code>Weight</code>.</li>
%%% <li><code>{weight, {Key, Weight}}</code> - Set the weight of
%%%    <code>Key</code> to weight <code>Weight</code>. A weight of zero
%%%    means that a construct is not generated. Higher weights means that
%%%    a construct i generated relatively often. Groups of weight keys
%%%    follow. Notice that the weight of a key is relative to other
%%%    keys of the same group. The weight of <code>small</code> needs
%%%    to quite high to avoid generating too deeply nested abstract
%%%    code.</li> <ul>
%%%      <li>Atomic expressions (<code>small</code>): <code>atom, boolean,
%%%        integer, string, char, float, nil</code></li>
%%%      <li>Compound terms: <code>small, bitstring, list, tuple,
%%%        map, 'fun'</code></li>
%%%      <li>Map expressions (<code>map</code>): <code>build_map</code></li>
%%%      <li>List expressions (<code>list</code>): <code>plain_list,
%%%        cons</code></li>
%%%      <li>Bitstrings (<code>bitstring</code>): <code>bits, bytes</code></li>
%%%      <li>Function expressions (<code>'fun'</code>):
%%%        <code>ext_mfa</code></li>
%%%    </ul>
%%% </ul>

-spec term(Options :: [option()]) -> proper_types:type().

term(Opts) ->
    PreOpts = [{set_all_weights, 0}],
    Tags = [compound, small, bitstring, list, tuple, map, 'fun',
            atom, boolean, integer, string, char, float, nil,
            bits, bytes,
            plain_list, cons,
            build_map,
            ext_mfa],
    WOpts = ([{weight, {small, ?DEFAULT_SMALL_WEIGHT_TERM}}]
             ++ [{weight, {T, 1}} || T <- Tags]),
    BadOpts = [Opt ||
                  {weight, {T, _}} = Opt <- Opts,
                  not lists:member(T, Tags)],
    case BadOpts =:= [] andalso options(PreOpts ++ WOpts ++ Opts) of
        false ->
            erlang:error(badarg);
        S0 ->
            S1 = S0#gen_state{result_type = term},
            S = eval_dependencies(S1),
            ?LET(E,
                 ?SIZED(Size, compound(S#gen_state{size = Size})),
                 begin
                     #gen_state{functions = Funs, atom = AtomGen} = S,
                     [Term] = post_process([E], Funs, AtomGen, []),
                     Term
                 end)
    end.

%%% @doc Returns abstract code of a module.
%%% The module has type declarations, functions, function specifications,
%%% and record declarations.

-spec module() -> proper_types:type().

module() ->
    module([]).

%%% @doc Same as {@link module/0}, but accepts a list of options.
%%% === Options ===
%%%
%%% <ul>
%%% <li><code>{atom, {@link atom_fun()}}</code> - A atom generating
%%%    function to replace the default.</li>
%%% <li><code>{char, {@link char_fun()}}</code> - A character generating
%%%    function to replace the default. The function is used when
%%%    generating strings and characters.</li>
%%% <li><code>{functions, [{Name, Arity}]}</code> - A list of FAs to
%%%    be used as names of generated functions. The default is a small
%%%    number of functions with a small number of arguments.</li>
%%% <li><code>{limit, [{Name, Limit}]}</code> - Set the limit of
%%%    <code>Name</code> to <code>Limit</code>. The limit names are:
%%%   <ul>
%%%     <li><code>bin_elements</code> - Number of segments of a bitstring.</li>
%%%     <li><code>list</code> - Number of elements of a plain list.</li>
%%%     <li><code>map</code> - Number of associations of a map.</li>
%%%     <li><code>string</code> - Number of characters of a string.</li>
%%%     <li><code>tuple</code> - Number of elements of a tuple.</li>
%%%     <li><code>body</code> - Number of clauses of a body.</li>
%%%     <li><code>call_args</code> - Number of arguments of function call.</li>
%%%     <li><code>catch_clauses</code> - Number of clauses of the
%%%       <code>catch</code> part of a <code>try/catch</code>.</li>
%%%     <li><code>clauses</code> - Number of clauses of <code>case</code>,
%%%       the <code>of</code> part of <code>try/catch</code>, and
%%%       <code>receive</code>.</li>
%%%     <li><code>function_clauses</code> - Number of clauses of
%%%       a function.</li>
%%%     <li><code>function_constraints</code> - Number of constraints of
%%%       a function specification.</li>
%%%     <li><code>function_constraints</code> - Number of constraints of
%%%       a function specification.</li>
%%%     <li><code>function_types</code> - Number of types of
%%%       an overloaded function specification.</li>
%%%     <li><code>guard</code> - Number of guards of a clause.</li>
%%%     <li><code>guard_tests</code> - Number of guard tests of a guard.</li>
%%%     <li><code>if_clauses</code> - Number of clauses of
%%%       <code>if</code>.</li>
%%%     <li><code>tuple_types</code> - Number of types (elements)
%%%       of tuple types.</li>
%%%     <li><code>qualifiers</code> - Number of qualifiers
%%%       of comprehensions.</li>
%%%     <li><code>record_fields</code> - Number of fields of record
%%%       declarations.</li>
%%%     <li><code>tsl</code> - Number of elements of
%%%       type specifier lists (of segments of bit syntax expressions).</li>
%%%     <li><code>union_types</code> - Number of types of type
%%%       union.s</li>
%%%   </ul>
%%% </li>
%%% <li><code>{records, [{Name, [Field]}]}</code> - A list
%%%    of record names with field names  to be used as names of
%%%    generated records. The default is a small number of records
%%%    with a small number of fields.</li>
%%% <li><code>{types, [{Name, NumOfParameters}]}</code> - A list
%%%    of TAs to be used as names of generated types. The default
%%%    is a small number of types.</li>
%%% <li><code>{resize, boolean()}</code> - Use <code>?SIZED</code>
%%%    to limit the size of the generated abstract code. With this
%%%    option set to <code>false</code> (the default) big code
%%%    may be generated among the first instances.</li>
%%% <li><code>{set_all_weights, Weight}</code> - Set the weight of
%%%    all keys to <code>Weight</code>.</li>
%%% <li><code>{weight, {Key, Weight}}</code> - Set the weight of
%%%    <code>Key</code> to weight <code>Weight</code>. A weight of zero
%%%    means that a construct is not generated. Higher weights means that
%%%    a construct i generated relatively often. Groups of weight keys
%%%    follow. Notice that the weight of a key is relative to other
%%%    keys of the same group. Also notice that some keys occur in
%%%    more than one group, which makes it all more complicated. The
%%%    weight of <code>small</code> needs to be quite high to avoid
%%%    generating too deeply nested abstract code.</li>
%%%    <ul>
%%%      <li>Declarations: <code>record_decl, type_decl, function_decl,
%%%        function_spec</code> (<code>type_decl</code> and
%%%        <code>function_spec</code> are off by default)</li>
%%%      <li>Atomic expressions (<code>small</code>): <code>atom, boolean,
%%%        integer, string, char, float, nil, pat_var, var</code></li>
%%%      <li>Compound expressions: <code>small, bitstring, list, tuple,
%%%        map, match, binop, unop, record, 'case', block, 'if', 'fun',
%%%        'receive', 'try', 'catch', try_of, termcall, varcall, localcall,
%%%        extcall</code> (<code>termcall</code> is off by default)</li>
%%%      <li>Map expressions (<code>map</code>): <code>build_map,
%%%        update_map</code></li>
%%%      <li>List expressions (<code>list</code>): <code>plain_list, cons,
%%%        lc</code></li>
%%%      <li>Qualifiers (of <code>lc</code>): <code>lc_gen, blc_gen,
%%%        lc_any_filter, lc_guard_filter</code></li>
%%%      <li>Bitstrings (<code>bitstring</code>): <code>bits, blc,
%%%        literal_bits</code></li>
%%%      <li>Try after (<code>'try', try_of</code>): <code>no_try_after,
%%%        try_after</code></li>
%%%      <li>Catch clause exception types (<code>'catch'</code>):
%%%        <code>no_eclass, any_eclass, lit_eclass, var_eclass,
%%%         bad_eclass</code></li>
%%%      <li>Receive timouts (<code>'receive'</code>): <code>
%%%         lit_timeout, inf_timeout, var_timeout</code></li>
%%%      <li>Function expressions (<code>'fun'</code>): <code>
%%%         lambda, rec_lambda, local_mfa, ext_mfa, any_mfa</code></li>
%%%      <li>Guards: <code>no_guard, yes_guard</code></li>
%%%      <li>Guard test: <code>small, tuple, map, cons, plain_list, bits,
%%%         binop, unop, record, guard_call, remote_guard_call</code></li>
%%%      <li>Pattern: <code>small, match, tuple, cons, plain_list, bits,
%%%         unop, binop, record, map_pattern, string_prefix</code></li>
%%%      <li>Pattern variables (<code>pat_var</code>):
%%%         <code>fresh_var, bound_var</code></li>
%%%      <li>Record field initialization (the <code>_ = V</code> syntax):
%%%         <code>yes_multi_field_init, no_multi_field_init</code></li>
%%%      <li>String prefix (<code>string_prefix</code>): <code>
%%%         nil, string, string_prefix_list</code></li>
%%%      <li>Types: <code>annotated_type, atom, bitstring, 'fun',
%%%         integer_range_type, nil, map, predefined_type, record,
%%%         remote_type, singleton_integer_type, tuple, type_union,
%%%         type_variable, user_defined_type</code></li>
%%%      <li>Function specifications: <code>yes_constrained_function_type,
%%%         no_constrained_function_type</code></li>
%%%      <li>Overloaded function specifications: <code>
%%%         no_overloaded, yes_overloaded</code></li>
%%%      <li>Singleton integer type (<code>singleton_integer_type</code>):
%%%         <code>integer, char, unop, binop</code></li>
%%%    </ul>
%%% </ul>

-spec module(Options :: [option()]) -> proper_types:type().

module(Opts)  when is_list(Opts) ->
    case options(Opts) of
        false ->
            erlang:error(badarg);
        State0 ->
            TopTags = [record_decl, type_decl, function_decl, function_spec],
            TagWeights = get_weights(TopTags, State0),
            ?DEBUG(" TagWeights ~p\n", [TagWeights]),
            State = set_up(State0),
            FormsL = [form(TW, TagWeights, State) || TW <- TagWeights],
            Fs = ([{attribute, anno(), module, State#gen_state.module}]
                  ++ lists:append(FormsL)),
            #gen_state{functions = Funs, atom = AtomGen} = State,
            true = length(Funs) > 0,
            ?SUCHTHAT(T,
                      ?LET(P,
                           Fs,
                           post_process(P, Funs, AtomGen, [])),
                      ok_by_the_linter(forms, T))
    end.

%%% @doc Returns abstract code of a guard. A guard is a sequence
%%% of guard tests.

-spec guard() -> proper_types:type().

guard() ->
    guard([]).

%%% @doc Same as {@link guard/0}, but accepts a list of options. See
%%% {@link module/1} for a description of the options.

-spec guard(Options :: [option()]) -> proper_types:type().

guard(Opts)  when is_list(Opts) ->
    case options(Opts) of
        false ->
            erlang:error(badarg);
        State0 ->
            State1 = State0#gen_state{result_type = guard},
            State = set_up(State1),
            ?LET(G,
                 ?SIZED(Size, a_guard(State#gen_state{size = Size})),
                 begin
                     #gen_state{functions = Funs,
                                atom = AtomGen,
                                variables = Vars} = State,
                     [Guard] = post_process([G], Funs, AtomGen, Vars),
                     Guard
                 end)
    end.

%%% @doc Returns abstract code of an expression.

-spec expr() -> proper_types:type().

expr() ->
    expr([]).

%%% @doc Same as {@link expr/0}, but accepts a list of options. See
%%% {@link module/1} for a description of the options.

-spec expr(Options :: list()) -> proper_types:type().

expr(Opts)  when is_list(Opts) ->
    case options(Opts) of
        false ->
            erlang:error(badarg);
        State0 ->
            State1 = State0#gen_state{result_type = expr,
                                      functions = []},
            State2 = set_up(State1),
            ?SUCHTHAT(Expr,
                      ?LET(E1,
                           ?SIZED(Size,
                                  begin
                                      State = State2#gen_state{size = Size},
                                      abstract_expr(State)
                                  end),
                           begin
                               #gen_state{functions = Funs,
                                          atom = AtomGen,
                                          variables = Vars} = State2,
                               [E2] = post_process([E1], Funs, AtomGen, Vars),
                               E2
                           end),
                      ok_by_the_linter(expr, Expr))
    end.

set_up(S0) ->
    #gen_state{functions = Fs, records = Rs, types = Ts} = S0,
    GRs = guard_records(Rs),
    case {Fs, Rs, Ts} of
        {[], [], []} ->
            erlang:error(nothing_to_work_with);
        _ ->
            %% Give local functions higher weight.
            AutoImported = auto_imported(),
            FW = case length(Fs) of
                     0 ->
                         0; % not used
                     NumFs ->
                         max(1, round(length(AutoImported) / NumFs))
                 end,
            FAWs = ([{FW, FA} || FA <- Fs]
                    ++ [{1, FA} || FA <- AutoImported]),
            S1 = S0#gen_state{functions_and_auto_imported = FAWs,
                              expr_bifs = guard_bifs() ++ expr_ops(),
                              guard_bifs = guard_bifs() ++ guard_ops(),
                              guard_records = GRs},
            State = eval_dependencies(S1),
            ?DEBUG(" records: ~p\n", [State#gen_state.records]),
            ?DEBUG(" types: ~p\n", [State#gen_state.types]),
            ?DEBUG(" functions: ~p\n", [State#gen_state.functions]),
            ?DEBUG(" weights: ~p\n", [State#gen_state.weights]),
            ?DEBUG(" limits: ~p\n", [State#gen_state.limits]),
            ?DEBUG(" resize: ~p\n", [State#gen_state.resize]),
            State
    end.

%%% The fields of the chosen record are initiated with guard
%%% expressions, which means that the record can occur in a guard
%%% expression.
guard_records([]) ->
    [];
guard_records([R | _]) ->
    [R].

form({_Tag, 0}, _, _State) ->
    [];
form({type_decl, _}, _, State) ->
    TypeNames = State#gen_state.types,
    Exports = [{attribute, anno(), export_type, TypeNames}],
    Decls = [type_decl(State, TN) || TN <- TypeNames],
    Exports ++ Decls;
form({record_decl, _}, _, State) ->
    RecordNames = State#gen_state.records,
    State = State#gen_state{records = RecordNames},
    State1 = exclude_tags([fresh_var], State),
    State2 = State1#gen_state{records = []},
    declare_recs(RecordNames, State2);
form({function_decl, _}, TagWeights, State) ->
    FunctionNames = State#gen_state.functions,
    Exports = [{attribute, anno(), export, FunctionNames}],
    State = State#gen_state{functions = FunctionNames},
    {function_spec, SpecW} = lists:keyfind(function_spec, 1, TagWeights),
    Decls = [begin
                 FD = function_decl(State, FN),
                 case SpecW of
                     0 ->
                         [FD];
                     _ ->
                         [function_spec(State, FN), FD]
                 end
             end ||
                FN <- FunctionNames],
    Exports ++ lists:append(Decls);
form({function_spec, _}, _, _State) ->
    [].

declare_recs([], _) ->
    [];
declare_recs([Rec|Recs], S0) ->
    S0_1 = exclude_tags([record], S0),
    R = record_decl(S0_1, Rec),
    S = S0#gen_state{records = [Rec|S0#gen_state.records]},
    [R | declare_recs1(Recs, S)].

declare_recs1([], _S) ->
    [];
declare_recs1([Rec|Recs], S0) ->
    R = record_decl(S0, Rec),
    S = S0#gen_state{records = [Rec|S0#gen_state.records]},
    [R | declare_recs1(Recs, S)].

record_decl(S0, {RecName, Fs}=R) ->
    ?SIZED(Size,
           begin
               S = S0#gen_state{size = Size},
               {'attribute', anno(), 'record',
                {RecName, field_decls(S, R, Fs)}}
           end).

field_decls(S, R, Fs) ->
    [field_decl(S, R, F) || F <- Fs].

field_decl(S0, R, F) ->
    S = case rec_init_guard_expr(S0, R) of
            true ->
                exclude_tags([complex_field_init], S0);
            false ->
                S0
        end,
    ?LET(Field,
         wunion([field_no_type, field_yes_type], S, ?FUNCTION_NAME),
         set_field_name(Field, F)).

rec_init_guard_expr(S, R) ->
    lists:member(R, S#gen_state.guard_records).

set_field_name({'typed_record_field', Field, AbstractType}, F) ->
    {'typed_record_field', set_field_name(Field, F), AbstractType};
set_field_name({'record_field', A, field_name}, F) ->
    {'record_field', A, lit_atom(F)};
set_field_name({'record_field', A, field_name, AbstractExpr}, F) ->
    {'record_field', A, lit_atom(F), AbstractExpr}.

field_yes_type(S) ->
    {'typed_record_field', field_no_type(S), abstract_type(S)}.

field_no_type(S) ->
    wunion([field_no_init, field_yes_init], S, ?FUNCTION_NAME).

field_no_init(_S) ->
    {'record_field', anno(), field_name()}.

field_yes_init(S) ->
    {'record_field',
     anno(),
     field_name(),
     case get_weight(complex_field_init, S) of
         0 ->
             guard_test(S);
         1 ->
             abstract_expr(S)
     end}.

field_name() ->
    field_name.

type_decl(S0, {TypeName, N}) ->
    ?SIZED(Size,
           begin
               S = S0#gen_state{size = Size},
               Parms = list_of_gen2(N, type_parameter()),
               {'attribute', anno(), type_attr(),
                %% Not affected by weight of 'variable'.
                {TypeName, abstract_type(S), Parms}}
           end).

type_parameter() ->
    a_variable(type_parameter).

type_attr() ->
    proper_types:oneof(['type', 'opaque']).

function_spec(S0, {F, N}) ->
    ?SIZED(Size,
           begin
               S = S0#gen_state{size = Size},
               {'attribute', anno(), spec_attr(),
                {{F, N}, function_type_list(S, N)}}
           end).

spec_attr() ->
    'spec'.
    %%    oneof(['callback', 'spec']).

function_decl(S0, {F, N}) ->
    ?SIZED(Size,
           begin
               S = S0#gen_state{size = Size},
               {'function', anno(), F, N, function_clause_seq(S, N)}
           end).

abstract_expr(S) ->
    compound(S).

compound(#gen_state{size = 0}=S) ->
    wunion([small], S, ?FUNCTION_NAME); % assume weight(small) > 0
compound(S) ->
    Tags = [small, bitstring, list, tuple, map, match, binop, unop,
            record, 'case', block, 'if', 'fun', 'receive', 'try',
            'catch', try_of, termcall, varcall, localcall, extcall],
    wunion(Tags, resize(S), ?FUNCTION_NAME).

a_map(S, abstract_type) ->
    map_type(S);
a_map(S, Where) ->
    wunion([build_map, update_map], S, Where).

a_list(S, Where) ->
    wunion([plain_list, cons, lc], S, Where).

%%% Assume 'plain' means 'proper' (see eqc:eqc_erlang_program).
plain_list(S, T) ->
    ?LET(L,
         list_of_gen(T, get_limit(list, S)),
         lists:foldr(fun(E, A) ->
                             {'cons', anno(), E, A}
                     end, nil(), L)).

cons(_S, T) ->
    {'cons', anno(), T, T}.

nil(_S, abstract_type) ->
    empty_list_type();
nil(_S, _Where) ->
    nil().

nil() ->
    {'nil', anno()}.

update_record(S, T) ->
    ?LET({RecName, Fields},
         known_record(S),
         {'record',
          anno(),
          abstract_expr(S),
          RecName,
          record_field_seq(S, T, Fields, update)}).

'catch'(S) ->
    {'catch', anno(), abstract_expr(S)}.

termcall(_S, T) ->
    {'tuple', anno(), [T, T]}.

varcall(S, T) ->
    {'call', anno(), T, args(S)}.

localcall(S) ->
    ?LET({F, N},
         local_function_or_auto_imported(S),
         {'call', anno(), F, n_args(S, N)}).

extcall(S) ->
    proper_types:weighted_union(
      [{1, ?LAZY(any_extcall(S))},
       {1, ?LAZY(known_extcall(S))}]).

any_extcall(S) ->
    N = random_n_args(S),
    {'call', anno(), remote_function(S), n_args(S, N)}.

known_extcall(S) ->
    ?LET({F, N},
         expr_bif(S),
         {'call', anno(),
          {'remote', anno(), lit_atom('erlang'), lit_atom(F)},
          n_args(S, N)}).

expr_bif(S) ->
    proper_types:oneof(S#gen_state.expr_bifs).

n_args(S, N) ->
    list_of_gen2(N, abstract_expr(S)).

args(S) ->
    N = random_n_args(S),
    list_of_gen2(N, abstract_expr(S)).

local_function_or_auto_imported(S) ->
    ?LET({F, N},
         proper_types:weighted_union(S#gen_state.functions_and_auto_imported),
         case lists:member({F, N}, S#gen_state.named_funs) of
             true ->
                 {a_variable(F), N};
             false ->
                 {lit_atom(F), N}
         end).

remote_function(S) ->
    {'remote', anno(), abstract_expr(S), abstract_expr(S)}.

lc(S) ->
    {'lc', anno(), template(S), qualifier_seq(S)}.

blc(S) ->
    LiteralW = get_weight(literal_bits, S),
    wunion1(
      [{1, ?LAZY({'bc', anno(), template(S), qualifier_seq(S)})},
       {LiteralW, ?LAZY(literal_bc(S))}
      ]).

literal_bc(S) ->
    %% The weight of `literal_bits' is not zero.
    SBC = set_tag_weights([{in_literal_bc, 1}], S),
    {'bc', anno(), bits(SBC, compound), qualifier_seq(SBC)}.

template(S) ->
    abstract_expr(S).

qualifier_seq(S) ->
    non_empty_list_of_gen(qualifier(S), get_limit(qualifiers, S)).

qualifier(S) ->
    Tags = [lc_gen, blc_gen, lc_any_filter, lc_guard_filter],
    wunion(Tags, S, ?FUNCTION_NAME).

lc_gen(S) ->
    {'generate', anno(), pattern(S), abstract_expr(S)}.

blc_gen(S) ->
    LiteralW = get_weight(literal_bits, S),
    WildBitsW = wild_bits_weight(S),
    ?LET({Pattern, Expr},
         {bits(S, pattern),
          wunion1(
            [{WildBitsW, ?LAZY(abstract_expr(S))},
             {LiteralW, ?LAZY(bits(S, compound))}
            ])},
         {'b_generate', anno(), Pattern, Expr}).

lc_any_filter(S) ->
    abstract_expr(S).

lc_guard_filter(S) ->
    guard_test(S).

block(S) ->
    {'block', anno(), body(S)}.

'if'(S) ->
    {'if', anno(), if_clause_seq(S)}.

if_clause_seq(S) ->
    non_empty_list_of_gen(if_clause(S), get_limit(if_clauses, S)).

if_clause(S) ->
    {'clause', anno(), [], if_guard_seq(S), body(S)}.

if_guard_seq(S) ->
    list_of_gen(a_guard(S), get_limit(guard, S)).

'case'(S) ->
    {'case', anno(), abstract_expr(S), clause_seq(S)}.

'try'(S) ->
    NESeq = non_empty_catch_clause_seq(S),
    Seq = catch_clause_seq(S),
    ?LET(After,
         wunion([no_try_after, try_after], S, ?FUNCTION_NAME),
         case After of
             [] ->
                 {'try', anno(), body(S), [], NESeq, After};
             _ ->
                 {'try', anno(), body(S), [], Seq, After}
         end).

try_of(S) ->
    NESeq = non_empty_catch_clause_seq(S),
    Seq = catch_clause_seq(S),
    ?LET(After,
         wunion([no_try_after, try_after], S, ?FUNCTION_NAME),
         case After of
             [] ->
                 {'try', anno(), body(S), clause_seq(S), NESeq, After};
             _ ->
                 {'try', anno(), body(S), clause_seq(S), Seq, After}
         end).

no_try_after(_S) ->
    [].

try_after(S) ->
    body(S).

catch_clause_seq(S) ->
    list_of_gen(catch_clause(S), get_limit(catch_clauses, S)).

non_empty_catch_clause_seq(S) ->
    non_empty_list_of_gen(catch_clause(S), get_limit(catch_clauses, S)).

catch_clause(S) ->
    Tags = [no_eclass, any_eclass, lit_eclass, var_eclass, bad_eclass],
    ?LET({EClass, St},
         {wunion(Tags, S, ?FUNCTION_NAME), stacktrace_variable(S)},
         {'clause', anno(),
          [{'tuple', anno(), [EClass, pattern(S), St]}],
          clause_guard_seq(S),
          body(S)}).

no_eclass(_S) ->
    a_variable('_').

any_eclass(_S) ->
    a_variable('_').

lit_eclass(_S) ->
    proper_types:oneof([lit_atom('exit'),
                        lit_atom('error'),
                        lit_atom('throw')]).

var_eclass(S) ->
    var(S). % atom is fallback

bad_eclass(_S) ->
    lit_atom(bad_eclass).

stacktrace_variable(S) ->
    %% weight(fresh_var) = 0 results in an anonymous variable.
    fresh_var(S).

'receive'(S) ->
    Ws = sum_weights([lit_timeout, inf_timeout, var_timeout], S),
    AfterW = min(Ws, 1),
    proper_types:weighted_union(
      [{1, ?LAZY(receive_no_after(S))},
       {AfterW, ?LAZY(receive_yes_after(S))}
      ]).

receive_no_after(S) ->
    {'receive', anno(), clause_seq(S)}.

receive_yes_after(S) ->
    ?LET(Timeout,
         wunion([lit_timeout, inf_timeout, var_timeout], S, ?FUNCTION_NAME),
         {'receive', anno(), receive_after_clause_seq(S), Timeout, body(S)}).

receive_after_clause_seq(S) ->
    list_of_gen(clause(S), get_limit(clauses, S)).

clause_seq(S) ->
    non_empty_list_of_gen(clause(S), get_limit(clauses, S)).

lit_timeout(S) ->
    an_integer(S).

inf_timeout(_S) ->
    lit_atom('infinity').

var_timeout(S) ->
    abstract_expr(S).

'fun'(S, abstract_type) ->
    fun_type(S);
'fun'(S, Where) ->
    Tags = [lambda, rec_lambda, local_mfa, ext_mfa, any_mfa],
    wunion(Tags, S, Where).

lambda(S) ->
    ?LET({_F, N},
         proper_types:oneof(S#gen_state.named_funs),
         {'fun', anno(), {'clauses', function_clause_seq(S, N)}}).

rec_lambda(S) ->
    ?LET({F, N},
         proper_types:oneof(S#gen_state.named_funs),
         begin
             FNW = {1, {F, N}}, % too low?
             Functions = [FNW | S#gen_state.functions_and_auto_imported],
             S1 = S#gen_state{functions_and_auto_imported = Functions},
             {'named_fun', anno(), F, function_clause_seq(S1, N)}
         end).

function_clause_seq(S, N) ->
    NCl = random_n_clauses(S),
    list_of_gen2(NCl, function_clause(S, N)).

random_n_clauses(S) ->
    uniform(get_limit(function_clauses, S)).

function_clause(S, N) ->
    {'clause', anno(), pattern_seq(S, N), clause_guard_seq(S), body(S)}.

local_mfa(S) ->
    ?LET({F, N},
         local_function(S),
         {'fun', anno(), {'function', F, N}}).

ext_mfa(S) ->
    LW =
        case
            (S#gen_state.result_type =:= term orelse
             get_weight(function_decl, S) > 0)
        of
            true ->
                1;
            false ->
                0
         end,
    wunion1(
      [{LW, ?LAZY(?LET({F, N},
                       local_function(S),
                       {'fun', anno(),
                        {'function', lit_atom(S#gen_state.module),
                         lit_atom(F),
                         lit_integer(N)}}))},
       {1, ?LAZY({'fun', anno(),
                  {'function', any_module(S),
                   any_function(),
                   lit_integer(proper_types:arity())}})}
      ]).

any_mfa(S) ->
    ?SUCHTHAT(Fun_MFA,
              ?LET({M, F, A},
                   {var_or_atom(S), var_or_atom(S), var_or_arity(S)},
                   {'fun', anno(), {'function', M, F, A}}),
              begin
                  {'fun', _, {'function', M, F, A}} = Fun_MFA,
                  is_var(M) orelse is_var(F) orelse is_var(A)
              end).

var_or_atom(S) ->
    var(S). % atom is fallback

var_or_arity(_S) ->
    proper_types:oneof([a_variable(bound_var_or_an_arity),
                        lit_integer(proper_types:arity())]).

is_var({'var', _, _}) -> true;
is_var(_) -> false.

local_function(S) ->
    one_of(S#gen_state.functions, no_functions).

clause(S) ->
    {'clause', anno(), [pattern(S)], clause_guard_seq(S), body(S)}.

pattern_seq(S, N) ->
    list_of_gen2(N, pattern(S)).

body(S) ->
    non_empty_list_of_gen(abstract_expr(S), get_limit(body, S)).

clause_guard_seq(S) ->
    wunion([no_guard, yes_guard], S, ?FUNCTION_NAME).

no_guard(_S) ->
    [].

yes_guard(S) ->
    non_empty_list_of_gen(a_guard(S), get_limit(guard, S)).

a_guard(S) ->
    non_empty_list_of_gen(guard_test(S), get_limit(guard_tests, S)).

guard_test(#gen_state{size = 0}=S) ->
    wunion([small], S, ?FUNCTION_NAME); % assume weight(small) > 0
guard_test(S) ->
    Tags = [small, tuple, map, cons, plain_list, bits, binop,
            unop, record, guard_call, remote_guard_call],
    wunion(Tags, resize(S), ?FUNCTION_NAME).

build_map(S, T) ->
    {'map', anno(), assoc_seq(S, 0, T)}.

update_map(S, T) ->
    {'map', anno(), T, assoc_seq(S, 1, T)}.

assoc_seq(S, ExactWeight, T) ->
    list_of_gen(assoc(ExactWeight, T), get_limit(map, S)).

assoc(ExactWeight, T) ->
    wunion1(
      [{1, ?LAZY({'map_field_assoc', anno(), T, T})},
       {ExactWeight, ?LAZY(assoc_exact(T))}]).

assoc_exact(T) ->
    {'map_field_exact', anno(), T, T}.

%%% The type test is_record() is not handled well.
guard_call(S) ->
    case has_fields(S) of
        false ->
            guard_call_1(S);
        true ->
            proper_types:weighted_union([{10, ?LAZY(guard_call_1(S))},
                                         {1, ?LAZY(guard_call_2(S))},
                                         {1, ?LAZY(guard_call_3(S))}])
    end.

guard_call_1(S) ->
    ?LET({F, N},
         guard_bif(S),
         {'call', anno(), lit_atom(F), guard_call_args(N, S)}).

guard_call_2(S) ->
    ?LET({RecName, _Fields},
         known_record_with_fields(S),
         {'call', anno(), lit_atom('is_record'),
          guard_call_args(1, S) ++ [lit_atom(RecName)]}).

guard_call_3(S) ->
    ?LET({RecName, Fields},
         known_record_with_fields(S),
         {'call', anno(),
          {'remote', anno(), lit_atom('erlang'), lit_atom('is_record')},
          (guard_call_args(1, S) ++
           [lit_atom(RecName), lit_integer(length(Fields))])}).

remote_guard_call(S) ->
    ?LET({F, N},
         guard_bif(S),
         {'call', anno(),
          {'remote', anno(), lit_atom('erlang'), lit_atom(F)},
          guard_call_args(N, S)}).

guard_bif(S) ->
    proper_types:oneof(S#gen_state.guard_bifs).

%%% Guard BIFs with arity greater than than the limit of call_args are
%%% not excluded.
guard_call_args(N, S) ->
    list_of_gen2(N, guard_test(S)).

pattern(#gen_state{size = 0}=S) ->
    wunion([small], S, ?FUNCTION_NAME); % assume weight(small) > 0
pattern(S) ->
    Tags = [small, match, tuple, cons, plain_list, bits, unop, binop,
            record, map_pattern, string_prefix],
    wunion(Tags, resize(S), ?FUNCTION_NAME).

a_record(S, abstract_type) ->
    record_type(S);
a_record(S, compound=Where) ->
    a_record2([build_record, record_field_access, record_index,
               update_record], S, Where);
a_record(S, guard_test=Where) ->
    a_record2([build_record, record_field_access, record_index], S, Where);
a_record(S, pattern=Where) ->
    a_record2([record_pattern, record_index], S, Where).

a_record2(Tags, S0, Where) ->
    S = maybe_exclude_field_access(S0),
    wunion(Tags, S, Where).

maybe_exclude_field_access(S) ->
    %% Maybe the user should fix this kind of issues.
    case has_fields(S) of
        false ->
            exclude_tags([record_field_access, record_index], S);
        true ->
            S
    end.

has_fields(S) ->
    lists:any(fun({_, Fields}) -> Fields =/= [] end, S#gen_state.records).

record_field_access(S, T) ->
    ?LET({RecName, Fields},
         known_record_with_fields(S),
         begin
             Field = lit_atom(proper_types:oneof(Fields)),
             {'record_field', anno(), T, RecName, Field}
         end).

record_index(S) ->
    ?LET({RecName, Fields},
         known_record_with_fields(S),
         begin
             Field = lit_atom(proper_types:oneof(Fields)),
             {'record_index', anno(), RecName, Field}
         end).

record_pattern(S) ->
    ?LET({RecName, Fields},
         known_guard_record(S),
         {'record',
          anno(),
          RecName,
          record_field_seq(S, pattern(S), Fields, build)}).

build_record(S, guard_test) ->
    ?LET({RecName, Fields},
         known_guard_record(S),
         {'record',
          anno(),
          RecName,
          record_field_seq(S, guard_test(S), Fields, build)});
build_record(S, compound) ->
    ?LET({RecName, Fields},
         known_record(S),
         {'record',
          anno(),
          RecName,
          record_field_seq(S, abstract_expr(S), Fields, build)}).

record_field_seq(_S, _T, [], _Context) ->
    [];
record_field_seq(S, T, Fs0, build) ->
    ?LET(IF,
         wunion([yes_multi_field_init, no_multi_field_init], S, anywhere),
         begin
             Fs = IF ++ Fs0,
             record_field_seq2(S, T, Fs)
         end);
record_field_seq(S, T, Fs, update) ->
    record_field_seq2(S, T, Fs).

record_field_seq2(S, T, Fs) ->
    N = uniform(min(length(Fs), get_limit(record_fields, S))),
    record_field(N, T, Fs).

yes_multi_field_init(_S) ->
    ['_'].

no_multi_field_init(_S) ->
    [].

record_field(0, _T, _Fs) ->
    [];
record_field(N, T, Fs0) ->
    ?LET(F,
         proper_types:oneof(Fs0),
         begin
             Name = case F of
                        '_' -> a_variable(F);
                        _ -> lit_atom(F)
                    end,
             Field = {'record_field', anno(), Name, T},
             Fs = lists:delete(F, Fs0),
             [Field | record_field(N - 1, T, Fs)]
         end).

map_pattern(S0) ->
    S = exclude_tags([record_index, string_prefix, pat_var], S0),
    {'map', anno(), assoc_pattern_seq(S)}.

assoc_pattern_seq(S) ->
    KeyW = get_weight(map_pattern_assoc, S),
    ValueW = get_weight(map_pattern_exact, S),
    %% Note that when excluding tags, more zero_weights errors are
    %% possible.
    InKey = [{fresh_var, 0},
             {map_pattern_assoc, 1},
             {map_pattern_exact, 0},
             {match, 0}],
    SKey = set_tag_weights(InKey, S), % only => in key; no =/2 in key
    G = proper_types:weighted_union(
          [{KeyW,
            %% EEP 52.
            ?LAZY({'map_field_assoc', anno(), guard_test(SKey), pattern(S)})},
           {ValueW,
            ?LAZY({'map_field_exact', anno(), pattern(SKey), pattern(S)})}]),
    non_empty_list_of_gen(G, ?MAX_MAP).

string_prefix(S) ->
    StringPrefix = wunion([nil, string, string_prefix_list], S, pattern),
    S1 = exclude_tags([record_index, string_prefix], S),
    {'op', anno(), '++', StringPrefix, pattern(S1)}.

string_prefix_list(S) ->
    plain_list(S, proper_types:union([a_char(S), an_integer(S)])).

%%% Maybe something like 'small'. Should obey S.size.
abstract_type(S) ->
    Tags = [annotated_type, atom, bitstring, 'fun',
            integer_range_type, nil, map, predefined_type, record,
            remote_type, singleton_integer_type, tuple, type_union,
            type_variable, user_defined_type],
    wunion(Tags, S, ?FUNCTION_NAME).

annotated_type(S) ->
    ?LET({Var, Type},
         {annotation(S), abstract_type(S)},
         {'ann_type', anno(), [Var, Type]}).

annotation(S) ->
    proper_types:weighted_union(
      [{20, ?LAZY(type_variable(S))},
       {1, ?LAZY(anonymous_var(S))}]).

empty_list_type() ->
    {'type', anno(), 'nil', []}.

fun_type(S) ->
    proper_types:weighted_union(
      [{1, ?LAZY({'type', anno(), 'fun', []})},
       {1, ?LAZY({'type', anno(), 'fun', [{'type', anno(), 'any'},
                                          abstract_type(S)]})},
       {2, ?LAZY(fun_type_n(S))}]).

fun_type_n(S) ->
    N = random_n_args(S),
    function_type(S, N).

random_n_args(S) ->
    uniform(get_limit(call_args, S) + 1) - 1.

integer_range_type(S) ->
    ?LET({T1, T2},
         {singleton_integer_type(S), singleton_integer_type(S)},
         {'type', anno(), 'range', [T1, T2]}).

map_type(S) ->
    proper_types:weighted_union(
      [{1, ?LAZY({'type', anno(), 'map', 'any'})},
       {1, ?LAZY({'type', anno(), 'map', assoc_type_seq(S)})}]).

assoc_type_seq(S) ->
    list_of_gen(assoc_type(S), ?MAX_MAP).

assoc_type(S) ->
    proper_types:weighted_union(
      [{1, ?LAZY({'type', anno(), 'map_field_assoc',
                  [abstract_type(S), abstract_type(S)]})},
       {1, ?LAZY({'type', anno(), 'map_field_exact',
                  [abstract_type(S), abstract_type(S)]})}]).

predefined_type(S) ->
    ?LET({TypeName, N},
         proper_types:oneof(S#gen_state.predef_types),
         {'type',
          anno(),
          TypeName,
          list_of_gen2(N, abstract_type(S))}).

record_type(S) ->
    ?LET({RecName, Fields},
         known_record(S),
         {'type',
          anno(),
          'record',
          [lit_atom(RecName) | record_field_types(S, Fields)]}).

record_field_types(_S, []) ->
    [];
record_field_types(S, Fs) ->
    N = uniform(min(length(Fs), get_limit(record_fields, S))),
    Type = abstract_type(S),
    record_field_type(N, Type, Fs).

record_field_type(0, _T, _Fs) ->
    [];
record_field_type(N, T, Fs0) ->
    ?LET(FieldName,
         proper_types:oneof(Fs0),
         begin
             Name = lit_atom(FieldName),
             Field = {'type', anno(), 'field_type', [Name, T]},
             Fs = lists:delete(FieldName, Fs0),
             [Field | record_field_type(N - 1, T, Fs)]
         end).

remote_type(S) ->
    ?LET({Module, Name, T},
         {an_atom(S), an_atom(S), abstract_type(S)},
         {'remote_type', anno(), [Module, Name, [T]]}).

tuple_type(S) ->
    proper_types:weighted_union(
      [{1, ?LAZY({'type', anno(), 'tuple', abstract_type_seq(S)})},
       {1, ?LAZY({'type', anno(), 'tuple', 'any'})}]).

abstract_type_seq(S) ->
    list_of_gen(abstract_type(S), get_limit(tuple_types, S)).

type_union(S) ->
    N = uniform(get_limit(union_types, S) - 2) + 2,
    {'type',
     anno(),
     'union',
     list_of_gen2(N, abstract_type(S))}.

user_defined_type(S) ->
    ?LET({TypeName, N},
         local_type(S),
         {'user_type', anno(), TypeName, list_of_gen2(N, abstract_type(S))}).

function_type_list(S, N) ->
    ?LET({Ft, {MinTypes, MaxTypes}},
         {function_type(S, N), n_function_types(S)},
         begin
             Tags = [yes_constrained_function_type,
                     no_constrained_function_type],
             G = wunion(Tags, S, ?FUNCTION_NAME),
             NTypes = MinTypes + uniform(MaxTypes - MinTypes + 1) - 1,
             ?LET(Ts,
                  list_of_gen2(NTypes, G),
                  [case T of
                       no_function_constraint ->
                           Ft;
                       Fc ->
                           {'type', anno(), 'bounded_fun', [Ft, Fc]}
                   end || T <- Ts])
         end).

function_type(S, N) ->
    {'type', anno(), 'fun',
     [{'type', anno(),
       'product',
       list_of_gen2(N, abstract_type(S))},
      abstract_type(S)]}.

no_constrained_function_type(_S) ->
    no_function_constraint.

yes_constrained_function_type(S) ->
    function_constraint(S).

function_constraint(S) ->
    non_empty_list_of_gen(constraint(S), get_limit(function_constraints, S)).

constraint(S) ->
    ?LET({IsSubtype, V, T},
         {lit_atom('is_subtype'), type_variable(S), abstract_type(S)},
         {'type', anno(), 'constraint', [IsSubtype, [V, T]]}).

n_function_types(S) ->
    wunion([no_overloaded, yes_overloaded], S, ?FUNCTION_NAME).

%%% Maybe function_types-limit is enough?
no_overloaded(_S) ->
    {1, 1}.

yes_overloaded(S) ->
    {2, max(get_limit(function_types, S), 2)}.

type_variable(_S) ->
    a_variable(type_variable).

singleton_integer_type(S) ->
    wunion([integer, char, unop, binop], S, abstract_type).

small(S, pattern) ->
    Tags = [atom, boolean, integer, string, char, float, nil, pat_var],
    wunion(Tags, S, ?FUNCTION_NAME);
small(S, _Where) ->
    Tags = [atom, boolean, integer, string, char, float, nil, var],
    wunion(Tags, S, ?FUNCTION_NAME).

a_boolean(_S) ->
    proper_types:union([lit_atom('true'), lit_atom('false')]).

an_integer(_S) ->
    lit_integer(proper_types:non_neg_integer()).

a_string(S) ->
    {'string', anno(), simple_string(S)}.

simple_string(S) ->
    N = uniform(get_limit(string, S) + 1) - 1,
    simple_string1(S, N).

simple_string1(_S, 0) ->
    [];
simple_string1(S, N) ->
    [simple_char(S) | simple_string1(S, N - 1)].

a_char(S) ->
    {'char', anno(), simple_char(S)}.

simple_char(S) ->
    (S#gen_state.simple_char)().

default_simple_char() ->
    proper_types:union([proper_types:integer($a, $z),
                        proper_types:integer($A, $Z)]).

default_atom() ->
    any_of(some_atoms()).

a_float(_S) ->
    ?LET(Float,
         proper_types:float(),
         {'float', anno(), abs(Float)}).

var(_S) ->
    a_variable(bound_var_or_an_atom).

pat_var(S) ->
    wunion([fresh_var, bound_var], S, ?FUNCTION_NAME).

fresh_var(S) ->
    case get_weight(fresh_var, S) of
        0 ->
            anonymous_var(S);
        _ ->
            proper_types:weighted_union(
              [{20, ?LAZY(a_variable(fresh_var))},
               {1, ?LAZY(anonymous_var(S))}])
    end.

bound_var(_S) ->
    a_variable(bound_var_or_an_integer).

anonymous_var(_S) ->
    a_variable('_').

a_variable(Name) ->
    {'var', anno(), Name}.

match(S, pattern) ->
    {'match', anno(), pattern(S), pattern(S)};
match(S, compound) ->
    {'match', anno(), pattern(S), abstract_expr(S)}.

tuple(S, abstract_type) ->
    tuple_type(S);
tuple(S, Where) ->
    T = where(S, Where),
    {'tuple', anno(), list_of_gen(T, get_limit(tuple, S))}.

non_empty_list_of_gen(G, Max) ->
    N = uniform(Max),
    list_of_gen2(N, G).

list_of_gen(G, Max) ->
    N = uniform(Max + 1) - 1,
    list_of_gen2(N, G).

list_of_gen2(0, _G) ->
    [];
list_of_gen2(N, G) ->
    [G | list_of_gen2(N - 1, G)].

bitstring(S, abstract_type) ->
    {'type', anno(), 'binary', [singleton_integer_type(S),
                                singleton_integer_type(S)]};
bitstring(S, Where) ->
    case S#gen_state.result_type of
        term ->
            wunion([bits, bytes], S, Where);
        ResType when ResType =:= program;
                     ResType =:= guard;
                     ResType =:= expr  ->
            wunion([bits, blc], S, Where)
    end.

bytes(S) ->
    {'bin', anno(), binelement_seq_term(S, bytes)}.

bits(#gen_state{result_type = term} = S, compound) ->
    {'bin', anno(), binelement_seq_term(S, bits)};
bits(S, compound=Where) ->
    LiteralW = get_weight(literal_bits, S),
    WildBitsW = wild_bits_weight(S),
    proper_types:weighted_union(
      [{WildBitsW,
        ?LAZY({'bin', anno(), binelement_seq(S, abstract_expr(S), Where)})},
       {LiteralW, ?LAZY(literal_bits(S, Where))}
      ]);
bits(S, guard_test=Where) ->
    {'bin', anno(), binelement_seq(S, guard_test(S), Where)};
bits(S, pattern=Where) ->
    LiteralW = get_weight(literal_bits, S),
    WildBitsW = wild_bits_weight(S),
    proper_types:weighted_union(
      [{WildBitsW,
        ?LAZY({'bin', anno(), binelement_seq(S, bin_pattern(S), Where)})},
       {LiteralW, ?LAZY(literal_bits(S, Where))}
      ]).

wild_bits_weight(S) ->
    case get_weight(in_literal_bc, S) of
        1 -> 0;
        _ -> 1
    end.

binelement_seq_term(S, B) ->
    N = uniform(get_limit(bin_elements, S)),
    binelements_term(N, B).

binelements_term(0, _) ->
    [];
binelements_term(N, B) ->
    Expr = lit_integer(uniform(1 bsl 32) - 1),
    Size = case B of
               bits ->
                   lit_integer(8 * uniform(4) - uniform(7));
               bytes ->
                   lit_integer(8 * uniform(4))
           end,
    TSL = default,
    [{'bin_element', anno(), Expr, Size, TSL} | binelements_term(N - 1, B)].

bin_pattern(S) ->
    Tags = [pat_var, string, integer, char, float, atom, unop, binop],
    wunion(Tags, S, pattern).

binelement_seq(S, T, Where) ->
    N = uniform(get_limit(bin_elements, S)),
    binelements(N, S, T, Where).

binelements(0, _S, _T, _W) ->
    [];
binelements(N, S, T, Where) ->
    [binelement(S, T, Where, N =:= 1) | binelements(N - 1, S, T, Where)].

binelement(S, T, Where, IsLast) ->
    ?LET(Expr,
         T,
         case {Where, Expr} of
             {pattern, {string, _, _}} ->
                 {'bin_element',
                  anno(),
                  Expr,
                  'default',
                  proper_types:union(['default',
                                      [proper_types:union(['utf8',
                                                           'utf16',
                                                           'utf32'])]])};
             _ ->
                 %% If HasUnit then Size =/= default.
                 %% If HasUtf then Size =:= default and not HasUnit
                 ?LET(TSL0,
                      type_specifier_list(S, IsLast),
                      begin
                          HasUtf = TSL0 =/= 'default'
                              andalso TSL0 -- [utf8, utf16, utf32] =/= TSL0,
                          {TSL, Size} =
                              case HasUtf of
                                  true ->
                                      {[TS || TS <- TSL0,
                                              (not is_tuple(TS) orelse
                                               element(1, TS) =/= 'unit')],
                                       'default'};
                                  false ->
                                      HasUnit =
                                          TSL0 =/= 'default'
                                          andalso
                                          lists:keymember('unit', 1, TSL0),
                                      case HasUnit of
                                          true when Where =:= pattern ->
                                              {TSL0,
                                               %% not 'default':
                                               binelement_size_pattern(S)};
                                          true ->
                                              {TSL0,
                                               T}; % not 'default'
                                          false when Where =:= pattern ->
                                              {TSL0,
                                               binelement_size
                                                 (binelement_size_pattern(S))};
                                          false ->
                                              {TSL0,
                                               binelement_size(T)} % any size
                                      end
                              end,
                          {'bin_element', anno(), Expr, Size, TSL}
                      end)
         end).

binelement_size_pattern(S0) ->
    S = exclude_tags([fresh_var], S0),
    guard_test(S). %% EEP 52

binelement_size(T) ->
    proper_types:weighted_union(
      [{1, ?LAZY('default')},
       {2, ?LAZY(T)}]).

%%% Generate simple and--most of the time--correct binary and
%%% bitstring expressions. The purpose is to cover more of the
%%% Compiler as the random code seldom passes the many tests of binary
%%% lists comprehensions.
literal_bits(S, Where) ->
    N = uniform(get_limit(bin_elements, S)),
    {'bin', anno(), literal_binelements(N, S, Where)}.

literal_binelements(0, _S, _Where) ->
    [];
literal_binelements(N, S, Where) ->
    [literal_binelement(S, N =:= 1, Where) |
     literal_binelements(N - 1, S, Where)].

literal_binelement(S, IsLast, Where) ->
    ?LET(Type,
         a_type(IsLast andalso Where =/= pattern),
         if
             Type =:= integer ->
                 %% Strings?
                 ?LET({Size, Unit},
                      {proper_types:integer(0, 8),
                       proper_types:integer(1, 32)}, % not too big...
                      {'bin_element',
                       anno(),
                       case Where of
                           compound ->
                               singleton_integer_type(S); % A bit sloppy.
                           pattern ->
                               singleton_integer_type(S) % More?
                       end,
                       lit_integer(Size),
                       [Type, {'unit', Unit}, signedness(), endianness()]});
             Type =:= utf8; Type =:= utf16; Type =:= utf32 ->
                 ?LET(CharOrString,
                      literal_unicode(S),
                      {'bin_element',
                       anno(),
                       CharOrString,
                       default,
                       [Type, signedness(), endianness()]});
             Type =:= float ->
                 ?LET({Size, Unit},
                      proper_types:oneof([{16, 1}, {32, 1}, {64, 1}]),
                      {'bin_element',
                       anno(),
                       a_float(S),
                       lit_integer(Size),
                       [Type, {'unit', Unit}, signedness(), endianness()]});
             true ->
                 ?LET({Bin, Unit0},
                      {literal_bits(S, Where), unit()},
                      begin
                          BitSize =
                              %% This could be slow if deeply nested.
                              try
                                  {value, B, _} = erl_eval:expr(Bin, []),
                                  bit_size(B)
                              catch
                                  _:_ ->
                                      1000
                              end,
                          {Unit, MaxSize} =
                              if
                                  Type =:= bytes ->
                                      {8, BitSize div 8};
                                  Type =:= binary ->
                                      Unit1 = max(min(Unit0, BitSize), 1),
                                      {Unit1, BitSize div Unit1};
                                  Type =:= bitstring; Type =:= bits ->
                                      {1, BitSize}
                              end,
                          Size = proper_types:integer(0, MaxSize),
                          {'bin_element',
                           anno(),
                           Bin,
                           lit_integer(Size),
                           [Type, {'unit', Unit}, signedness(), endianness()]}
                      end)
         end).

%%% Does not use #gen_state.simple_char. Perhaps it should.
literal_unicode(S) ->
    StringW = get_weight(string, S),
    S1 = S#gen_state{simple_char = fun unicode/0},
    wunion1([{3, ?LAZY(lit_integer(unicode()))},
             {StringW, ?LAZY(a_string(S1))}]).

unicode() ->
    proper_types:oneof([proper_types:integer(0, 16#D7FF),
                        proper_types:integer(16#E000, 16#10FFFF)]).

binop(S, abstract_type) ->
    T = singleton_integer_type(S),
    {'op', anno(), type_binop(), T, T};
binop(S, compound=Where) ->
    wunion([any_op, guard_op], S, Where);
binop(S, guard_test) ->
    guard_binop(S, guard_test(S));
binop(S, pattern) ->
    pattern_binop(pattern_expr_operand(S)).

any_binop(_S, T) ->
    {'op', anno(), any_binop(), T, T}.

guard_binop(_S, T) ->
    {'op', anno(), guard_binop(), T, T}.

pattern_binop(T) ->
    {'op', anno(), pattern_binop(), T, T}.

%%% The operators according to erl_internal. orelse/andalso added.
any_binop() ->
    proper_types:oneof(
      ['+', '-', '*', '/', 'div', 'rem', 'band', 'bor', 'bxor', 'bsl', 'bsr',
       'and', 'or', 'xor',
       '=:=', '=/=', '==', '/=', '=<', '<', '>=', '>',
       'orelse', 'andalso', % not proper operators, but handled as such
       '++', '--', '!']). % not in guards

guard_binop() ->
    proper_types:oneof(
      ['+', '-', '*', '/', 'div', 'rem', 'band', 'bor', 'bxor', 'bsl', 'bsr',
       'and', 'or', 'xor',
       '=:=', '=/=', '==', '/=', '=<', '<', '>=', '>',
       'orelse', 'andalso']). % not proper operators, but handled as such

pattern_binop() ->
    proper_types:oneof(
      ['+', '-', '*', '/', 'div', 'rem', 'band', 'bor', 'bxor', 'bsl',
       'bsr']).

type_binop() ->
    proper_types:oneof(
      ['+', '-', '*', 'div', 'rem', 'band', 'bor', 'bxor', 'bsl', 'bsr']).

unop(S, abstract_type) ->
    {'op', anno(), type_unop(), singleton_integer_type(S)};
unop(S, compound) ->
    %% any_op and guard_op: they are the same for unary operators
    {'op', anno(), any_unop(), abstract_expr(S)};
unop(S, guard_test) ->
    {'op', anno(), any_unop(), guard_test(S)};
unop(S, pattern) ->
    {'op', anno(), pattern_unop(), pattern_expr_operand(S)}.

pattern_expr_operand(S0) ->
    S = exclude_tags([record_index, string_prefix], S0),
    %% Simplified. Evaluates to an integer.
    wunion([char, float, integer, unop, binop], S, pattern).

any_unop() ->
    proper_types:oneof(['+', '-', 'bnot', 'not']).

pattern_unop() ->
    proper_types:oneof(['+', '-']).

type_unop() ->
    proper_types:oneof(['+', '-', 'bnot']).

type_specifier_list(S, IsLast) ->
    proper_types:weighted_union(
      [{1, 'default'},
       {1, ?LAZY(type_specifiers(S, IsLast))}]).

type_specifiers(S, IsLast) ->
   N = uniform(get_limit(tsl, S)),
   type_specifier(N, IsLast, []).

type_specifier(0, _IsLast, _L) ->
    [];
type_specifier(N, IsLast, L) ->
    ?LET({Tag, TS},
         ?SUCHTHAT({Tag, _},
                   a_type_specifier(IsLast, L),
                   not is_chosen(Tag, L)),
         [TS | type_specifier(N - 1, IsLast, [Tag, TS | L])]).

a_type_specifier(IsLast0, L) ->
    %% A bit sloppy. Cannot generate [{unit, 8}, binary], for example.
    %% Maybe do nothing here and everyting in post_process()?
    IsLast = IsLast0 andalso not is_chosen(unit, L),
    UnitWeight = case is_member([bitstring, bits, bytes, binary], L) of
                     true -> 0;
                     false -> 1
                 end,
    wunion1(
      [{1, ?LAZY({'type', a_type(IsLast)})},
       {1, ?LAZY({'signedness', signedness()})},
       {1, ?LAZY({'endianness', endianness()})},
       {UnitWeight, ?LAZY({unit, {'unit', unit()}})}]).

is_member([], _) ->
    false;
is_member([E|Es], L) ->
    lists:member(E, L) orelse is_member(Es, L).

is_chosen(Tag, L) ->
    lists:member(Tag, L).

a_type(IsLast) ->
    wunion1(bit_segment_types(IsLast)).

bit_segment_types(false) ->
    [{3, 'integer'},
     {2, 'float'},
     {1, 'utf8'},
     {1, 'utf16'},
     {1, 'utf32'}];
bit_segment_types(true) ->
    [{3, 'integer'},
     {2, 'float'},
     {3, 'binary'},
     {3, 'bytes'},
     {3, 'bitstring'},
     {3, 'bits'},
     {1, 'utf8'},
     {1, 'utf16'},
     {1, 'utf32'}].

signedness() ->
    proper_types:oneof(['signed', 'unsigned']).

endianness() ->
    proper_types:oneof(['big', 'little', 'native']).

unit() ->
    wunion1(
      [{10, ?LAZY(proper_types:oneof([1, 8, 16, 32]))},
       {1, proper_types:integer(1, 256)}]).

known_record_with_fields(S) ->
    ?SUCHTHAT(R, known_record(S), element(2, R) =/= []).

known_record(S) ->
    one_of(S#gen_state.records, no_records).

known_guard_record(S) ->
    [R|_] = S#gen_state.guard_records,
    R.

local_type(S) ->
    one_of(S#gen_state.types, no_types).

one_of([], Err) ->
    erlang:error(Err);
one_of(L, _Err) ->
    proper_types:oneof(L).

an_atom(_S) ->
    lit_atom(any_atom()).

any_atom() ->
    any_atom.

any_module(_S) ->
    lit_atom(any_module).

any_function() ->
    lit_atom(any_function).

lit_atom(A) ->
    {'atom', anno(), A}.

lit_integer(I) ->
    {'integer', anno(), I}.

anno() ->
    erl_anno:new(0).

resize(#gen_state{resize = false} = S) ->
    S;
resize(#gen_state{resize = true} = S) ->
    ?RESIZE(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_dependencies(State0) ->
    State = case State0#gen_state.functions of
                [] ->
                    exclude_tags([function_decl], State0);
                _ ->
                    State0
            end,
    Deps = deps(State#gen_state.result_type),
    Fun = fun(Dep, S) ->
                  {Tags, AffectedTags} = Dep,
                  eval_dep(S, Tags, AffectedTags)
          end,
    lists:foldl(Fun, State, Deps).

%%% The list is not exhaustive. Maybe the user should fix this kind of
%%% issues.
deps(term) ->
    [{[nil, string, char, integer], [string_prefix]},
     {[bits, bytes], [bitstring]},
     {[bitstring], [bits, bytes]},
     {[build_map], [map]},
     {[plain_list, cons], [list]}];
deps(ResType) when ResType =:= program; ResType =:= guard; ResType =:= expr ->
    [{[type_decl], [user_defined_type]},
     {[record_decl], [record]},
     {[nil, string, char, integer], [string_prefix]},
     {[bits, blc], [bitstring]},
     {[function_decl], [local_mfa]},
     {[bitstring], [bits, blc]},
     {[build_map, update_map], [map]},
     {[fresh_var, bound_var], [pat_var]},
     {[plain_list, cons, lc], [list]}].

eval_dep(S, Tags, AffectedTags) ->
    case sum_weights(Tags, S) of
        0 ->
            exclude_tags(AffectedTags, S);
        _ ->
            S
    end.

sum_weights(Tags, State) ->
    Ws = get_weights(Tags, State),
    lists:sum([W || {_, W} <- Ws]).

get_weights(Tags, State) ->
    [{Tag, get_weight(Tag, State)} || Tag <- Tags].

get_weight(Tag, State) ->
    maps:get(Tag, State#gen_state.weights).

exclude_tags(Tags, State) ->
    TagWeights = [{Tag, 0} || Tag <- Tags],
    set_tag_weights(TagWeights, State).

set_tag_weights(TagWeights, State) ->
    Weights = State#gen_state.weights,
    Fun = fun({Tag, Weight}, Ws) ->
                  maps:put(Tag, Weight, Ws)
          end,
    NewWeights = lists:foldl(Fun, Weights, TagWeights),
    State#gen_state{weights = NewWeights}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wunion(Tags, State, Where) ->
    FreqTypes = weights(Tags, State, Where),
    wunion2(FreqTypes, Tags).

wunion1(FreqTypes) ->
    wunion2(FreqTypes, []).

wunion2(FreqTypes0, Tags) ->
    FreqTypes = non_zero(FreqTypes0),
    case FreqTypes of
        [] ->
            erlang:error({zero_weights, Tags});
        _ ->
            ok
    end,
    proper_types:weighted_union(FreqTypes).

non_zero(FreqTypes) ->
    [FT || {F, _} = FT <- FreqTypes, F =/= 0].

weights(Tags, State, Where) ->
    Weights = State#gen_state.weights,
    [{maps:get(Tag, Weights),
      ?LAZY((pfun(Tag, Where))(State))}
      || Tag <- Tags].

pfun(annotated_type, _) -> fun annotated_type/1;
pfun(atom, _) -> fun an_atom/1;
pfun(integer_range_type, _) -> fun integer_range_type/1;
pfun(no_constrained_function_type, _) -> fun no_constrained_function_type/1;
pfun(no_overloaded, _) -> fun no_overloaded/1;
pfun(predefined_type, _) -> fun predefined_type/1;
pfun(remote_type, _) -> fun remote_type/1;
pfun(singleton_integer_type, _) -> fun singleton_integer_type/1;
pfun(type_union, _) -> fun type_union/1;
pfun(type_variable, _) -> fun type_variable/1;
pfun(user_defined_type, _) -> fun user_defined_type/1;
pfun(yes_constrained_function_type, _) -> fun yes_constrained_function_type/1;
pfun(yes_overloaded, _) -> fun yes_overloaded/1;
%%pfun(anonymous_var, _) -> fun anonymous_var/1;
pfun(any_eclass, _) -> fun any_eclass/1;
pfun(any_mfa, _) -> fun any_mfa/1;
pfun(any_op, Where) -> pfun1(fun any_binop/2, Where);
pfun(boolean, _) -> fun a_boolean/1;
pfun(bad_eclass, _) -> fun bad_eclass/1;
pfun(bitstring, Where) -> fun(S) -> bitstring(S, Where) end;
pfun(bits, Where) -> fun(S) -> bits(S, Where) end;
pfun(blc, _) -> fun blc/1;
pfun(blc_gen, _) -> fun blc_gen/1;
pfun(binop, Where) -> fun(S) -> binop(S, Where) end;
pfun(block, _) -> fun block/1;
pfun(bound_var, _) -> fun bound_var/1;
pfun(bytes, _) -> fun bytes/1;
pfun('case', _) -> fun 'case'/1;
pfun('catch', _) -> fun 'catch'/1;
pfun(char, _) -> fun a_char/1;
pfun(cons, Where) -> pfun1(fun cons/2, Where);
pfun(ext_mfa, _) -> fun ext_mfa/1;
pfun(field_no_init, _) -> fun field_no_init/1;
pfun(field_no_type, _) -> fun field_no_type/1;
pfun(field_yes_init, _) -> fun field_yes_init/1;
pfun(field_yes_type, _) -> fun field_yes_type/1;
pfun(float, _) -> fun a_float/1;
pfun(fresh_var, _) -> fun fresh_var/1;
pfun('fun', Where) -> fun(S) -> 'fun'(S, Where) end;
pfun(guard_call, _) -> fun guard_call/1;
pfun(guard_op, Where) -> pfun1(fun guard_binop/2, Where);
pfun('if', _) -> fun 'if'/1;
pfun(inf_timeout, _) -> fun inf_timeout/1;
pfun(integer, _) -> fun an_integer/1;
pfun(lambda, _) -> fun lambda/1;
pfun(lc, _) -> fun lc/1;
pfun(lc_any_filter, _) -> fun lc_any_filter/1;
pfun(lc_guard_filter, _) -> fun lc_guard_filter/1;
pfun(lc_gen, _) -> fun lc_gen/1;
pfun(list, Where) -> fun(S) -> a_list(S, Where) end;
pfun(lit_eclass, _) -> fun lit_eclass/1;
pfun(lit_timeout, _) -> fun lit_timeout/1;
pfun(localcall, _) -> fun localcall/1;
pfun(local_mfa, _) -> fun local_mfa/1;
pfun(map, Where) -> fun(S) -> a_map(S, Where) end;
pfun(build_map, Where) -> pfun1(fun build_map/2, Where);
pfun(map_pattern, _) -> fun map_pattern/1;
pfun(string_prefix, _) -> fun string_prefix/1;
pfun(string_prefix_list, _) -> fun string_prefix_list/1; % internal
pfun(match, Where) -> fun(S) -> match(S, Where) end;
pfun(nil, Where) -> fun(S) -> nil(S, Where) end;
pfun(no_eclass, _) -> fun no_eclass/1;
pfun(no_guard, _) -> fun no_guard/1;
pfun(no_multi_field_init,_) -> fun no_multi_field_init/1;
pfun(no_try_after, _) -> fun no_try_after/1;
pfun(pat_var, _) -> fun pat_var/1;
pfun(plain_list, Where) -> pfun1(fun plain_list/2, Where);
pfun('receive', _) -> fun 'receive'/1;
pfun(rec_lambda, _) -> fun rec_lambda/1;
pfun(record, Where) -> fun(S) -> a_record(S, Where) end;
pfun(build_record, Where) -> fun(S) -> build_record(S, Where) end;
pfun(record_pattern, _) -> fun record_pattern/1;
pfun(update_record, Where) -> pfun1(fun update_record/2, Where);
pfun(record_index, _) -> fun(S) -> record_index(S) end;
pfun(record_field_access, Where) -> pfun1(fun record_field_access/2, Where);
pfun(extcall, _) -> fun extcall/1;
pfun(remote_guard_call, _) -> fun remote_guard_call/1;
pfun(small, Where) -> fun(S) -> small(S, Where) end;
pfun(string, _) -> fun a_string/1;
pfun(termcall, Where) -> pfun1(fun termcall/2, Where);
pfun(tuple, Where) -> fun(S) -> tuple(S, Where) end;
pfun('try', _) -> fun 'try'/1;
pfun(try_of, _) -> fun try_of/1;
pfun(try_after, _) -> fun try_after/1;
pfun(unop, Where) -> fun(S) -> unop(S, Where) end;
pfun(update_map, Where) -> pfun1(fun update_map/2, Where);
pfun(var, _) -> fun var/1;
pfun(varcall, Where) -> pfun1(fun varcall/2, Where);
pfun(var_eclass, _) -> fun var_eclass/1;
pfun(var_timeout, _) -> fun var_timeout/1;
pfun(yes_guard, _) -> fun yes_guard/1;
pfun(yes_multi_field_init, _) -> fun yes_multi_field_init/1.

pfun1(F, Where) -> fun(S) -> F(S, where(S, Where)) end.

where(S, compound) -> abstract_expr(S);
where(S, guard_test) -> guard_test(S);
where(S, pattern) -> pattern(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

options(Options) ->
    State = create_template(),
    #gen_state{weights = Weights, limits = Limits} = State,
    (check_options(Weights, Limits, Options)
     andalso eval_options(Options, State)).

check_options(Weights, Limits, Options) ->
    lists:all(fun ({K, {V1, V2}}) -> check_option2(Weights, Limits, K, V1, V2)
                ; ({K, V}) -> check_option(K, V)
              end, Options).

check_option(set_all_weights, Value) ->
    is_integer(Value) andalso Value >= 0;
check_option(variables, Term) ->
    try
        %% true = length(Term) > 0,
        lists:all(fun(V) -> is_variable(V) end, Term)
    catch
        _:_ ->
            false
    end;
check_option(functions, Term) ->
    try
        lists:all(fun(F) -> is_fa(F) end, Term)
    catch
        _:_ ->
            false
    end;
check_option(types, Term) ->
    try
        lists:all(fun(T) -> is_fa(T) end, Term)
    catch
        _:_ ->
            false
    end;
check_option(records, Term) ->
    try
        lists:all(fun(R) -> is_record(R) end, Term)
    catch
        _:_ ->
            false
    end;
check_option(char, Term) when is_function(Term, 0) ->
    true;
check_option(atom, Term) when is_function(Term, 0) ->
    true;
check_option(resize, Term) when is_boolean(Term) ->
    true;
check_option(_, _) ->
    false.

check_option2(Weights, _Limits, weight, Term1, W) ->
    is_integer(W) andalso W >= 0 andalso
    is_map(Weights) andalso maps:is_key(Term1, Weights);
check_option2(_Weights, Limits, limit, Term1, L) ->
    %% A limit equal to zero would mean the same as setting the weight
    %% to zero.
    is_integer(L) andalso L > 0 andalso
    is_map(Limits) andalso maps:is_key(Term1, Limits) andalso
    (Term1 =/= tsl orelse L =< 3);
check_option2(_, _, _, _, _) ->
    false.

is_variable(T) ->
    case atom_to_list(T) of
        [C|_Cs] when C >= $A, C =< $Z ->
            true;
        [C|_Cs] when C =:= $_ ->
            true
    end.

is_fa({F, A}) when is_atom(F), is_integer(A), A >= 0, A < 256 -> true.

is_record({R, Fs}) when is_atom(R) ->
    true = lists:all(fun erlang:is_atom/1, Fs).

eval_options([], State) ->
    State;
eval_options([{weight, {K, V}}|Options], State0) ->
    State = set_weight(State0, K, V),
    eval_options(Options, State);
eval_options([{set_all_weights, V}|Options], State0) ->
    Weights = maps:map(fun (termcall, V0) -> V0
                         ; (map_pattern_assoc, V0) -> V0
                         ; (map_pattern_exact, V0) -> V0
                         ; (complex_field_init, V0) -> V0
                         ; (string_prefix_list, V0) -> V0
                         ; (in_literal_bc, V0) -> V0
                         ; (_, _) -> V
                       end, State0#gen_state.weights),
    State = State0#gen_state{weights = Weights},
    eval_options(Options, State);
eval_options([{limit, {K, W}}|Options], State0) ->
    State = set_limit(State0, K, W),
    eval_options(Options, State);
eval_options([{variables, Vars}|Options], State0) ->
    State = State0#gen_state{variables = ordsets:from_list(Vars)},
    eval_options(Options, State);
eval_options([{functions, Funcs}|Options], State) ->
    State1 = State#gen_state{functions = Funcs},
    eval_options(Options, State1);
eval_options([{types, Types}|Options], State) ->
    State1 = State#gen_state{types = Types},
    eval_options(Options, State1);
eval_options([{records, Records}|Options], State) ->
    State1 = State#gen_state{records = Records},
    eval_options(Options, State1);
eval_options([{char, CharGen}|Options], State) ->
    State1 = State#gen_state{simple_char = CharGen},
    eval_options(Options, State1);
eval_options([{atom, AtomGen}|Options], State) ->
    State1 = State#gen_state{atom = AtomGen},
    eval_options(Options, State1);
eval_options([{resize, Boolean}|Options], State) ->
    State1 = State#gen_state{resize = Boolean},
    eval_options(Options, State1).

set_weight(State, K, V) ->
    Weights = State#gen_state.weights,
    State#gen_state{weights = maps:update(K, V, Weights)}.

set_limit(State, Name, Value) ->
    Limits = State#gen_state.limits,
    State#gen_state{limits = maps:update(Name, Value, Limits)}.

get_limit(Tag, State) ->
    maps:get(Tag, State#gen_state.limits).

create_template() ->
    NamedFunctions = some_named_functions(),
    PredefTypes = predef_types(),
    #gen_state{module = module_name,
               functions = some_functions(),
               records = some_records(),
               types = some_types(),
               named_funs = NamedFunctions,
               predef_types = PredefTypes,
               weights = default_weights(),
               limits = default_limits()}.

default_weights() ->
    #{
     'case' => 1,
     'catch' => 1,
     'fun' => 1,
     'if' => 1,
     'receive' => 1,
     'try' => 1,
     annotated_type => 1,
     %% anonymous_var => 1,
     any_eclass => 1,
     any_mfa => 1,
     any_op => 1,
     atom => 1,
     bad_eclass => 1,
     bitstring => 1,
     bits => 1,
     bytes => 1, % term only
     blc => 1,
     blc_gen => 1,
     binop => 1,
     block => 1,
     boolean => 1,
     bound_var => 1,
     char => 1,
     compound => 1,
     cons => 1,
     default => 1,
     ext_mfa => 1,
     field => 1,
     field_no_init => 1,
     field_no_type => 1,
     field_yes_init => 1,
     field_yes_type => 1,
     file => 1,
     float => 1,
     fresh_var => 1,
     function_decl => 1,
     function_spec => 0,
     generate => 1,
     guard_op => 1,
     guard_call => 1,
     inf_timeout => 1,
     integer => 3,
     integer_range_type => 1,
     lambda => 1,
     lc => 1,
     lc_any_filter => 10,
     lc_guard_filter => 5,
     lc_gen => 5,
     list => 1,
     lit_eclass => 1,
     lit_timeout => 1,
     localcall => 1,
     local_mfa => 1,
     map => 1,
     build_map => 1,
     map_pattern => 1,
     string_prefix => 1,
     match => 1,
     nil => 1,
     no_constrained_function_type => 1,
     no_eclass => 1,
     no_guard => 1,
     no_multi_field_init => 1,
     no_overloaded => 1,
     no_try_after => 1,
     pat_var => 1,
     plain_list => 1,
     predefined_type => 1,
     rec_lambda => 1,
     record => 1,
     build_record => 1,
     record_pattern => 1,
     record_decl => 1,
     record_field_access => 1,
     record_index => 1,
     update_record => 1,
     extcall => 1,
     remote_guard_call => 1,
     remote_type => 1,
     signedness => 1,
     singleton_integer_type => 1,
     size => 1,
     small => ?DEFAULT_SMALL_WEIGHT_PROGRAM,
     string => 1,
     termcall => 0,
     try_of => 1,
     try_after => 1,
     tuple => 1,
     type => 1,
     type_decl => 0,
     type_union => 1,
     type_variable => 1,
     unop => 1,
     update_map => 1,
     user_defined_type => 1,
     varcall => 1,
     var_eclass => 1,
     var_timeout => 1,
     var => 1,
     yes_constrained_function_type => 1,
     yes_guard => 1,
     yes_multi_field_init => 1,
     yes_overloaded => 1,

     literal_bits => 1,
     %% See also eval_options().
     %% Used internally, see literal_bc().
     in_literal_bc => 0,

     %% Used internally, see assoc_pattern_seq().
     map_pattern_exact => 1,
     map_pattern_assoc => 0,

     %% Also used internally:
     complex_field_init => 1,
     string_prefix_list => 1
    }.

default_limits() ->
    #{
      %% term and program
      bin_elements => ?MAX_BIN_ELEMENTS,
      list => ?MAX_LIST,
      map => ?MAX_MAP,
      string => ?MAX_STRING,
      tuple => ?MAX_TUPLE,
      %% program
      body => ?MAX_BODY,
      call_args => ?MAX_CALL_ARGS,
      catch_clauses => ?MAX_CATCH_CLAUSES,
      clauses  => ?MAX_CLAUSES,
      function_clauses => ?MAX_FUNCTION_CLAUSES,
      function_constraints => ?MAX_FUNCTION_CONSTRAINTS,
      function_types => ?MAX_FUNCTION_TYPES,
      guard => ?MAX_GUARD,
      guard_tests => ?MAX_GUARD_TESTS,
      if_clauses => ?MAX_IF_CLAUSES,
      tuple_types => ?MAX_TUPLE_TYPES,
      qualifiers => ?MAX_QUALIFIERS,
      record_fields => ?MAX_RECORD_FIELDS,
      tsl => ?MAX_TYPE_SPECIFIER,
      union_types => ?MAX_UNION_TYPES
     }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Postprocess generated abstract format.

post_process(Abstracts, Functions, AtomGen, Variables) ->
    Known = (Functions ++ auto_imported() ++
             guard_bifs() ++ other_bifs()),
    State = #post_state{known_functions = Known,
                        atom = AtomGen,
                        vars = Variables},
    ?DEBUG("\n\n<original>\n~p\n</original>\n", [Abstracts]),
    [post1(Abstr, State) || Abstr <- Abstracts].

post1(AbstrL, State) when is_list(AbstrL) ->
    {AbstrL1, _} = post_list(AbstrL, State),
    AbstrL1;
post1(Abstr, State) ->
    {Abstr1, _} = post(Abstr, State),
    Abstr1.

%%% - unbound/unsafe linter errors are avoided, but the code is
%%%   probably a bit too complex...

post({var, A, fresh_var}, S) ->
    new_var(A, S);
post({var, A, bound_var_or_an_atom}, S) ->
    any_var(A, S, atom);
post({var, A, bound_var_or_an_integer}, S) ->
    any_var(A, S, integer);
post({var, A, bound_var_or_an_arity}, S) ->
    any_var(A, S, arity);
post({var, A, type_variable}, S) ->
    any_var(A, S, fresh_var);
post({var, A, type_parameter}, S) ->
    new_var(A, S);
post({atom, A, any_function}, S) ->
    {{atom, A, create_atom(S)}, S};
post({atom, A, any_module}, S) ->
    {{atom, A, create_atom(S)}, S};
post({atom, A, create_atom}, S) ->
    {{atom, A, create_atom(S)}, S};
post({atom, A, bad_eclass}, S) ->
    {{atom, A, create_atom(S)}, S};
post({cons, A, H, T}, S) ->
    {[H1,T1], S1} = post_expr_list([H, T], S),
    {{cons, A, H1, T1}, S1};
post({call, A1, {remote, A2, M, F}, As}, S) ->
    {[M1, F1|As1], S1} = post_expr_list([M, F|As], S),
    {{call, A1, {remote, A2, M1, F1}, As1}, S1};
post({call, A, F, As}, S) ->
    {[F1|As1], S1} = post_expr_list([F|As], S),
    case F1 of
        {atom, _, Name} = Atom ->
            KnownFunctions = S#post_state.known_functions,
            case lists:member({Name, length(As)}, KnownFunctions) of
                true ->
                    {{call, A, F1, As1}, S1};
                false ->
                    {Atom, S}
                end;
        _ -> % named fun or other
            {{call, A, F1, As1}, S1}
    end;
post({match, A, P, E}, S) ->
    {[P2, E2], S3} =
        case S#post_state.context of
            pattern -> % alias
                post_expr_list([P, E], S);
            _ ->
                {E1, S1} = post(E, S),
                {[P1], S2} = post_patterns([P], S1),
                {[P1, E1], S2}
        end,
    {{match, A, P2, E2}, S3};
post({lc, A, E, Qs}, S) ->
    {Qs1, S1} = post_qualifiers(Qs, S),
    {E1, _} = post(E, S1),
    {{lc, A, E1, Qs1}, S};
post({bc, A, E, Qs}, S) ->
    {Qs1, S1} = post_qualifiers(Qs, S),
    {E1, _} = post(E, S1),
    {{bc, A, E1, Qs1}, S};
post({op, A, Op, E}, S) ->
    {E1, S1} = post(E, S),
    E3 = case S#post_state.context of
             pattern ->
                 {E2, _} = post_round(E1, A),
                 E2;
             _ ->
                 E1
         end,
    {{op, A, Op, E3}, S1};
post({op, A, Op, L, R}, S) when Op =:= 'bsl'; Op =:= 'bsr' ->
    %% bsl and bsr can create huge integers, which is not what we want
    %% to test here.
    {[L1, R1], S1} = post_expr_list([L, R], S),
    {L3, R3} =
        case S1#post_state.context of
            expr ->
                {L1, R1};
            pattern ->
                {L2, _} = post_round(L1, A),
                {R2, V} = post_round(R1, A),
                case V of % can be slow...
                    {integer, _, I} when I > 30 ->
                        {L2, {integer, A, 30}};
                    {integer, _, I} when I < -30 ->
                        {L2, {integer, A, -30}};
                    _ ->
                        {L2, R2}
                end;
            type ->
                case erl_eval:partial_eval(R1) of % can be slow...
                    {integer, _, I} when I > 30 ->
                        {L1, {integer, A, 30}};
                    {integer, _, I} when I < -30 ->
                        {L1, {integer, A, -30}};
                    _ ->
                        {L1, R1}
                end
        end,
    {{op, A, Op, L3, R3}, S1};
post({op, A, Op, L, R}, S) when Op =:= 'rem'; Op =:= 'div'; % type and pattern
                                Op =:= '/' -> % pattern
    {[L1, R1], S1} = post_expr_list([L, R], S),
    {L3, R3} =
        case S1#post_state.context of
            expr ->
                {L1, R1};
            pattern ->
                {L2, _} = post_round(L1, A),
                {R2, V} = post_round(R1, A),
                case V of % can be slow...
                    {integer, _, 0} -> % division by zero
                        {L2, {op, A, '+', R2, {integer, A, 1}}};
                    _ ->
                        {L2, R2}
                end;
            type ->
                case erl_eval:partial_eval(R1) of % can be slow...
                    {integer, _, 0} -> % division by zero
                        {L1, {op, A, '+', R1, {integer, A, 1}}};
                    _ ->
                        {L1, R1}
                end
        end,
    {{op, A, Op, L3, R3}, S1};
post({op, A, Op, L, R}, S) when Op =:= 'orelse'; Op =:= 'andalso' ->
    {L1, S1} = post(L, S),
    {R1, S2} = post(R, S1),
    IntroducedVariables = introduced_variables(S1, S2),
    S3 = forbidden_variables(S2, IntroducedVariables),
    {{op, A, Op, L1, R1}, S3};
post({op, A, Op, L, R}, S) ->
    {[L1, R1], S1} = post_expr_list([L, R], S),
    {L3, R3} =
        case S1#post_state.context of
            pattern ->
                {L2, _} = post_round(L1, A),
                {R2, _} = post_round(R1, A),
                {L2, R2};
            _ ->
                {L1, R1}
        end,
    {{op, A, Op, L3, R3}, S1};
post({map, A, Es}, S) ->
    {Es1, S1} = post_expr_list(Es, S),
    {{map, A, Es1}, S1};
post({map, A, B, Es}, S) ->
    {[B1|Es1], S1} = post_expr_list([B|Es], S),
    {{map, A, B1, Es1}, S1};
post({map_field_assoc, A, K, V}, S) ->
    {[K2, V2], S1} =
        case S#post_state.context of
            pattern ->
                %% K is any guard expression, which the linter accepts.
                L = [post_expr(K, S), post(V, S)],
                expr_list2(L, S);
            _ ->
                post_expr_list([K, V], S)
        end,
    {{map_field_assoc, A, K2, V2}, S1};
post({map_field_exact, A, K, V}, S) ->
    {[K1, V1], S1} = post_expr_list([K, V], S),
    {{map_field_exact, A, K1, V1}, S1};
post({'if', A, Clauses}, S) ->
    {Clauses1, S1} = clauses(Clauses, S),
    {{'if', A, Clauses1}, S1};
post({'case', A, E, Clauses}, S) ->
    {E1, S1} = post(E, S),
    {Clauses1, S2} = clauses(Clauses, S1),
    {{'case', A, E1, Clauses1}, S2};
post({'try', A, B, Cls, TCls, After}, S) ->
    {B1, S1} = post_list(B, S),
    IntroVars = introduced_variables(S, S1),
    {Cls1, S2} = clauses_in_try(Cls, S1, clause),
    %% New variables in B are unsafe in TCls and After.
    S3 = forbidden_variables(S2, IntroVars),
    {TCls1, S4} = clauses_in_try(TCls, S3, catch_clause),
    {After1, S5} = post_list(After, S4),
    AllIntroVars = introduced_variables(S, S5),
    ForbiddenVars = ordsets:union(AllIntroVars, S#post_state.forbidden),
    S6 = forbidden_variables(S5, ForbiddenVars),
    {{'try', A, B1, Cls1, TCls1, After1}, S6};
post({'receive', A, Clauses}, S) ->
    {Clauses1, S1} = clauses(Clauses, S),
    {{'receive', A, Clauses1}, S1};
post({'receive', A, Clauses, E, B}, S) ->
    {Clauses1, S1} = clauses(Clauses, S),
    {E1, S2} = post(E, S),
    %% New variables in E are not visible in B:
    IntroVars0 = introduced_variables(S, S2),
    S3 = forbidden_variables(S2, IntroVars0),
    {B1, S4} = post_list(B, S3),
    %% New variables in receive are unsafe.
    IntroVars1 = introduced_variables(S, S1),
    IntroVars3 = introduced_variables(S, S4),
    IntroVars = ordsets:union(IntroVars1, IntroVars3),
    ForbiddenVars = ordsets:union(IntroVars, S4#post_state.forbidden),
    S5 = forbidden_variables(S1, ForbiddenVars),
    {{'receive', A, Clauses1, E1, B1}, S5};
post({tuple, A, Es}, S) ->
    {Es1, S1} = post_expr_list(Es, S),
    {{tuple, A, Es1}, S1};
post({'catch', A, E}, S) ->
    {E1, S1} = post(E, S),
    IntroducedVariables = introduced_variables(S, S1),
    S2 = forbidden_variables(S1, IntroducedVariables),
    {{'catch', A, E1}, S2};
post({'fun', A, {clauses, Clauses}}, S) ->
    {Clauses1, _} = clauses(Clauses, S),
    {{'fun', A, {clauses, Clauses1}}, S};
post({named_fun, A, F, Clauses}, S) ->
    {Clauses1, _} = clauses(Clauses, S),
    {{named_fun, A, F, Clauses1}, S};
post({bin, A, Es}, S) ->
    {Es1, S1} = post_expr_list(Es, S),
    {{bin, A, Es1}, S1};
post({bin_element, A, Expr, default, TSL}, S) ->
    {[Expr1], S1} = post_expr_list([Expr], S),
    {{bin_element, A, Expr1, default, TSL}, S1};
post({bin_element, A, Expr, Size, TSL}, S) ->
    {[Expr2, Size2], S1} =
        case S#post_state.context of
            pattern ->
                %% Size is any guard expression, which the linter accepts.
                L = [post(Expr, S), post_expr(Size, S)],
                expr_list2(L, S);
            _ ->
                post_expr_list([Expr, Size], S)
        end,
    {{bin_element, A, Expr2, Size2, TSL}, S1};
post({record, A, E, RecName, Fields}, S) ->
    {[E1|Fields1], S1} = post_expr_list([E|Fields], S),
    {{record, A, E1, RecName, Fields1}, S1};
post({record, A, RecName, Fields}, S) ->
    {Fields1, S1} = post_expr_list(Fields, S),
    {{record, A, RecName, Fields1}, S1};
post({type, A, binary, [_B, _U]=BU}, S) ->
    {[B1, U1], S1} = post_expr_list(BU, S),
    Check = fun(E) ->
                    case erl_eval:partial_eval(E) of
                        {integer, _, V} when V >= 0 ->
                            E;
                        {integer, _, V} when V < 0 ->
                            {op, A, '-', E};
                        (_) -> % cannot happen
                            E
                    end
            end,
    {{type, A, binary, [Check(B1), Check(U1)]}, S1};
post({type, A, range, [_L, _H]=LH}, S) ->
    {[L1, H1] = LH1, S1} = post_expr_list(LH, S),
    Low = erl_eval:partial_eval(L1),
    High = erl_eval:partial_eval(H1),
    case {Low, High} of
        {{integer, _, V}, {integer, _, V}} ->
            {{type, A, range, [L1, {op, A, '+', H1, {integer, A, 1}}]}, S1};
        {{integer, _, V1}, {integer, _, V2}} when V1 >= V2 ->
            {{type, A, range, [H1, L1]}, S1};
        {{integer, _, V1}, {integer, _, V2}} when V1 < V2 ->
            {{type, A, range, LH1}, S1};
        _ -> % cannot happen
            {{type, A, range, LH1}, S1}
    end;
post({attribute, A, Type, {TypeName, AbstrType, Parms}}, S)
                  when Type =:= 'opaque'; Type =:= 'type' ->
    in_context
      (type, S,
       fun(State) ->
               {Parms1, S1} = post_list(Parms, State),
               {AbstrType1, S2} = post(AbstrType, S1),
               {{attribute, A, Type, {TypeName, AbstrType1, Parms1}}, S2}
       end);
post({function, A, F, N, ClauseSeq}, S) ->
    in_context
      (expr, S,
       fun(State) ->
               {ClauseSeq1, State1} = function_clauses(ClauseSeq, State),
               {{function, A, F, N, ClauseSeq1}, State1}
       end);
post({attribute, A, Spec, {{_F, _N}=FN, FuncTypeList}}, S) ->
    in_context
      (type, S,
       fun(State) ->
               {FuncTypeList1, State1} = post_list(FuncTypeList, State),
               {{attribute, A, Spec, {FN, FuncTypeList1}}, State1}
       end);
post({attribute, A, record, {Name, Fields}}, S) ->
    in_context
      (record, S,
       fun(State) ->
               {Fields1, State1} = post_list(Fields, State),
               {{attribute, A, record, {Name, Fields1}}, State1}
       end);
post({record_field, A, Name, Expr}, #post_state{context = record} = S) ->
    in_context
      (expr, S,
       fun(State) ->
               {Expr1, State1} = post(Expr, State),
               {{record_field, A, Name, Expr1}, State1}
       end);
post({typed_record_field, Field, Type}, S) ->
    in_context
      (type, S,
       fun(State) ->
               {Field1, State1} = post(Field, State),
               {Type1, State2} = post(Type, State1),
               {{typed_record_field, Field1, Type1}, State2}
       end);
%%% No special handling of the following cases:
post({ann_type, A, T}, S) ->
    {T1, S1} = post_list(T, S),
    {{ann_type, A, T1}, S1};
post({atom, _, _}=A, S) ->
    {A, S};
post({attribute, _, _, _}=A, S) ->
    {A, S};
post({block, A, Body}, S) ->
    {Body1, S1} = post_list(Body, S),
    {{block, A, Body1}, S1};
post({char, _, _}=C, S) ->
    {C, S};
post({float, _, _}=F, S) ->
    {F, S};
post({'fun', _A, {function, M, N, Arity}}=F, S) when is_atom(M),
                                                     is_atom(N),
                                                     is_integer(Arity) ->
    %% cannot happen
    {F, S};
post({'fun', A, {function, M, N, Arity}}, S) ->
    {[M1, N1, Arity1], S1} = post_list([M, N, Arity], S),
    {{'fun', A, {function, M1, N1, Arity1}}, S1};
post({'fun', _A, {function, N, Arity}}=F, S) when is_atom(N),
                                                  is_integer(Arity) ->
    {F, S};
post({integer, _, _}=I, S) ->
    {I, S};
post({nil, _}=N, S) ->
    {N, S};
post({record_field, A, N}, S) ->
    {N1, S1} = post(N, S),
    {{record_field, A, N1}, S1};
post({record_field, A, F, E}, S) ->
    {[F1, E1], S1} = post_list([F, E], S),
    {{record_field, A, F1, E1}, S1};
post({record_field, A, E0, N, F}, S) ->
    {[E1, F1], S1} = post_list([E0, F], S),
    {{record_field, A, E1, N, F1}, S1};
post({record_index, A, N, F}, S) ->
    {F1, S1} = post(F, S),
    {{record_index, A, N, F1}, S1};
post({remote_type, A, [M, N, Ts]}, S) ->
    {[M1, N1], S1} = post_list([M, N], S),
    {Ts1, S2} = post_list(Ts, S1),
    {{remote_type, A, [M1, N1, Ts1]}, S2};
post({string, _, _}=Str, S) ->
    {Str, S};
post({type, _A, any}=Any, S) ->
    {Any, S};
post({type, _A, _N, any}=Any, S) ->
    {Any, S};
post({type, A, 'fun', Ts}, S) ->
    {Ts1, S1} = post_list(Ts, S),
    {{type, A, 'fun', Ts1}, S1};
post({type, A, constraint, [C, [V, T]]}, S) ->
    {[C1, V1, T1], S1} = post_list([C, V, T], S),
    {{type, A, constraint, [C1, [V1, T1]]}, S1};
post({type, A, bounded_fun, [Ft, Fcs]}, S) ->
    {Ft1, S1} = post(Ft, S),
    {Fcs1, S2} = post_list(Fcs, S1),
    {{type, A, bounded_fun, [Ft1, Fcs1]}, S2};
post({type, A, N, Ts}, S) ->
    {Ts1, S1} = post_list(Ts, S),
    {{type, A, N, Ts1}, S1};
post({user_type, A, N, Ts}, S) ->
    {Ts1, S1} = post_list(Ts, S),
    {{user_type, A, N, Ts1}, S1};
post({var, _A, '_'}=VarU, S) ->
    {VarU, S};
post({var, _A, _NamedFun}=VarNF, S) ->
    {VarNF, S}.

in_context(Context, S, Fun) ->
    S1 = S#post_state{context = Context,
                      vars = [],
                      vindex = 0,
                      forbidden = []},
    {T, _} = Fun(S1),
    {T, S}.

post_round(E, A) ->
    case erl_eval:partial_eval(E) of % can be slow if E is deep
        {float, _, F} ->
            %% Very crude. A pity guard BIFs cannot be evaluated by
            %% erl_eval:partial_eval/1 (for example erlang:guard/1).
            I = {integer, A, round(F)},
            {I, I};
        V ->
            {E, V}
    end.

post_qualifiers([], S) ->
    {[], S};
post_qualifiers([{Gen, A, P, E}|Qs], S) when Gen =:= generate;
                                             Gen =:= b_generate ->
    %% Variables introduced in E can only be used in E, and can be
    %% introduced in subsequent generators.
    {E1, _} = post(E, S),
    {[P1], S1} = post_patterns([P], S),
    {Qs1, S2} = post_qualifiers(Qs, S1),
    {[{Gen, A, P1, E1}|Qs1], S2};
post_qualifiers([F|Qs], S) ->
    {F1, S1} = post(F, S),
    {Qs1, S2} = post_qualifiers(Qs, S1),
    {[F1|Qs1], S2}.

function_clauses(Clauses, S) ->
    L = [post_clause(Cl, S) || Cl <- Clauses],
    {Clauses1, _} = lists:unzip(L),
    {Clauses1, S}.

clauses([], S) ->
    {[], S}; % receive after T -> E end
clauses(Clauses, S) ->
    L = [post_clause(Cl, S) || Cl <- Clauses],
    {Clauses1, Ss} = lists:unzip(L),
    VarsInCls = [introduced_variables(S, S1) || S1 <- Ss],
    NewVars = ordsets:union(VarsInCls),
    ExportedVars = ordsets:intersection(VarsInCls),
    ForbiddenVars = ordsets:subtract(NewVars, ExportedVars),
    ForbiddenVarsInCls = [S1#post_state.forbidden || S1 <- Ss],
    AllForbiddenVars = ordsets:union([ForbiddenVars|ForbiddenVarsInCls]),
    S1 = forbidden_variables(S, AllForbiddenVars),
    S2 = S1#post_state{vars = ordsets:union(ExportedVars, S#post_state.vars)},
    {Clauses1, S2}.

clauses_in_try([], S, _Kind) ->
    {[], S};
clauses_in_try(Clauses, S, Kind) ->
    L = [post_try_clause(Cl, S, Kind) || Cl <- Clauses],
    {Clauses1, Ss} = lists:unzip(L),
    VarsInCls = [introduced_variables(S, S1) || S1 <- Ss],
    ForbiddenVars = ordsets:union(VarsInCls),
    ForbiddenVarsInCls = [S1#post_state.forbidden || S1 <- Ss],
    AllForbiddenVars = ordsets:union([ForbiddenVars|ForbiddenVarsInCls]),
    S1 = forbidden_variables(S, AllForbiddenVars),
    {Clauses1, S1}.

post_try_clause(Clause, S, clause) ->
    post_clause(Clause, S);
post_try_clause(Clause, S, catch_clause) ->
    {clause, A1, [{'tuple', A2, [EClass, P, St]}], GuardSeq, Body} = Clause,
    {EClass1, S1} = post(EClass, S),
    {[P1], S2} = post_patterns([P], S1),
    %% Stacktrace variable must not be bound:
    SSt = forbidden_variables(S2, S2#post_state.vars),
    {St1, S3} = post(St, SSt),
    StackVars = introduced_variables(SSt, S3),
    %% The stacktrace variable cannot be used in the catch clause guard:
    S4 = forbidden_variables(S3, StackVars),
    {GuardSeq1, S5} = post_guard_seq(GuardSeq, S4),
    %% Should simplify this...
    S6 = allowed_variables(S5, StackVars),
    {Body1, S7} = post_list(Body, S6),
    S8 = forbidden_variables(S7, StackVars),
    {{clause, A1, [{'tuple', A2, [EClass1, P1, St1]}], GuardSeq1, Body1},
     S8}.

post_clause({clause, A, Patterns, GuardSeq, Body}, S0) ->
    {Patterns1, S1} = post_patterns(Patterns, S0),
    {GuardSeq1, S2} = post_guard_seq(GuardSeq, S1),
    {Body1, S3} = post_list(Body, S2),
    {{clause, A, Patterns1, GuardSeq1, Body1}, S3}.

post_patterns(Patterns, S0) ->
    Ctxt = S0#post_state.context,
    S1 = S0#post_state{context = pattern},
    {Patterns1, S2} = post_expr_list(Patterns, S1),
    {Patterns1, S2#post_state{context = Ctxt}}.

post_guard_seq(GuardSeq, S)  ->
    GuardSeq1 = [post_list(G, S) || G <- GuardSeq],
    expr_list2(GuardSeq1, S).

post_list([], S) ->
    {[], S};
post_list([E|Es], S) ->
    {E1, S1} = post(E, S),
    {Es1, S2} = post_list(Es, S1),
    {[E1|Es1], S2}.

post_expr_list(Es, S) ->
    L = [post(E, S) || E <- Es],
    expr_list2(L, S).

expr_list2(L, S) ->
    {Es1, Ss} = lists:unzip(L),
    VsInEs = [S1#post_state.vars || S1 <- Ss],
    Vs = ordsets:union([S#post_state.vars|VsInEs]),
    Forbidden = ordsets:union([S1#post_state.forbidden || S1 <- Ss]),
    S1 = S#post_state{vars = Vs},
    S2 = forbidden_variables(S1, Forbidden),
    {Es1, S2}.

post_expr(Expr, S0) ->
    Ctxt = S0#post_state.context,
    S1 = S0#post_state{context = expr},
    {Expr1, S2} = post(Expr, S1),
    {Expr1, S2#post_state{context = Ctxt}}.

introduced_variables(S0, S1) ->
    ordsets:from_list(S1#post_state.vars -- S0#post_state.vars).

forbidden_variables(S, Vs) ->
    S#post_state{forbidden = ordsets:union(S#post_state.forbidden, Vs)}.

allowed_variables(S, Vs) ->
    S#post_state{forbidden = ordsets:subtract(S#post_state.forbidden, Vs)}.

any_var(Anno, State, Fallback) ->
    #post_state{vars = Vs, forbidden = NoNo} = State,
    case find_var(Vs, NoNo, length(Vs)) of
        no ->
            case Fallback of
                arity ->
                    {{integer, Anno, uniform(256) - 1}, State};
                atom ->
                    {{atom, Anno, create_atom(State)}, State};
                fresh_var ->
                    new_var(Anno, State);
                integer ->
                    {{integer, Anno, uniform(1024) - 1}, State} % smallish
            end;
        {yes, Var} ->
            {{var, Anno, Var}, State}
    end.

find_var([], _NoNo, 0) ->
    no;
find_var(Vs, NoNo, N) ->
    V = lists:nth(uniform(N), Vs),
    case ordsets:is_element(V, NoNo) of
        true ->
            find_var(Vs -- [V], NoNo, N - 1);
        false ->
            {yes, V}
    end.

create_atom(S) ->
    (S#post_state.atom)().

any_of(L) ->
    lists:nth(uniform(length(L)), L).

new_var(Anno, S) ->
    #post_state{vars = Vs, forbidden = NoNo, vindex = I} = S,
    Seed = case S#post_state.context of
               type ->
                   '_V'; % avoid singleton_typevar error..
               expr ->
                   'V';
               pattern ->
                   'V'
           end,
    NewVar = list_to_atom(lists:concat([Seed, I])),
    S1 = S#post_state{vindex = I + 1},
    case ordsets:is_element(NewVar, NoNo) of
        true ->
            new_var(Anno, S1);
        false ->
            NewVars = ordsets:add_element(NewVar, Vs),
            S2 = S#post_state{vars = NewVars},
            {{var, Anno, NewVar}, S2}
    end.

%%% Some errors detected by the linter are not compensated for in this
%%% module. However, such errors should not occur very often, which
%%% means that bad instances can be discarded without (almost) any
%%% slow-down. But check that the errors returned are expected as
%%% unexpected errors can slow down the generator.
ok_by_the_linter(What, T) ->
    case call_linter(What, T) of
        {ok, _Ws} ->
            true;
        {error, [{_File,Errors}], _Ws} ->
            ?DEBUG("LINT ~p\n", [Errors]),
            ?DEBUG("HARD? ~p\n", [all_hard_to_fix_errors(Errors)]),
            case all_hard_to_fix_errors(Errors) of
                true ->
                    ?DEBUG("DISCARD!\n", []),
                    false % discard; try again
              %% ; false ->
              %%       io:format("The linter found an error that should not "
              %%                 "have occurred:\n ~p\n ~p\n", [Errors, T]),
              %%       exit({bug, Errors, T})
            end
    end.

call_linter(expr, Expr) ->
    erl_lint:exprs([Expr], []);
call_linter(forms, Forms) ->
    erl_lint:module(Forms).

all_hard_to_fix_errors(Errors) ->
    lists:all(fun hard_error/1, Errors).

%%% After some testing it seems harmless to treat all linter errors
%%% as "hard". The "leakage" is negligible.

%%% hard_error({_, erl_lint, illegal_bin_pattern}) -> true;
%%% hard_error({_, erl_lint, {error, bittype_unit}}) -> true;
%%% hard_error({_, erl_lint, illegal_map_key}) -> true;
%%% hard_error({_, erl_lint, unsized_binary_in_bin_gen_pattern}) -> true;
%%% Not yet seen:
%%% illegal_guard_expr
%%% utf_bittype_size_or_unit
%%% {undefined_bittype, _}
%%% unsized_binary_in_bin_gen_pattern
%%% illegal_pattern_end
%%% and probably more...
%%% Not so hard errors, should have been taken care of:
%%% ({_, erl_lint, {bittype_mismatch, _, _, _}})
%%% ({_, erl_lint, unsized_binary_not_at_end})
%%% hard_error({_, erl_lint, unsized_binary_in_bin_gen_pattern}) -> true;
hard_error({_, erl_lint, _}) -> true.

some_atoms() ->
    ['', air, area, art, back, body, book, business, car, change,
     child, city, community, company, country, day, door, education,
     eye, face, fact, family, father, force, friend, game, girl,
     government, group, guy, hand, head, health, history, home, hour,
     house, idea, information, issue, job, kid, kind, law, level,
     life, line, lot, man, member, minute, moment, money, month,
     morning, mother, name, night, number, office, others, parent,
     part, party, people, person, place, point, power, president,
     problem, program, question, reason, research, result, right,
     room, school, service, side, state, story, student, study,
     system, teacher, team, thing, time, war, water, way, week, woman,
     word, work, world, year].

some_named_functions() ->
    %% Do not include {'_', ...} since "fun _() -> _() end"
    %% results in unbound variable.
    [{'F1',1}, {'F2',0}, {'F3',2}, {'F3',3}].

some_functions() ->
    [{f1,1}, {f1,2}, {f2,0}].

some_records() ->
    %% The first one is chosen as "guard record", which means that
    %% all field initializations are guard expressions.
    [{r1,[f1,f2]}, {r2,[]}, {r3,[f1]}].

%%% An arbitrary, small, collection of BIFs.
auto_imported() ->
    [{abs,1}, {atom_to_list,1}, {ceil,1}, {erase,1},
     {exit,1}, {group_leader,2}, {is_function,2},
     %% Old BIFs:
     {check_process_code,2}, {get,0}, {is_atom,1}].

some_types() ->
    [{t1,0}, {t2,1}].

predef_types() ->
    [{any,0}, {arity,0}, {atom,0}, {binary,0}, {bitstring,0},
     {boolean,0}, {byte,0}, {char,0}, {float,0}, {function,0},
     {identifier,0}, {integer,0}, {iodata,0}, {iolist,0}, {list,0},
     {list,1}, {map,0}, {maybe_improper_list,0},
     {maybe_improper_list,2}, {mfa,0}, {module,0}, {neg_integer,0},
     {nil,0}, {no_return,0}, {node,0}, {non_neg_integer,0}, {none,0},
     {nonempty_binary, 0}, {nonempty_bitstring, 0},
     {nonempty_improper_list,2}, {nonempty_list,0}, {nonempty_list,1},
     {nonempty_maybe_improper_list,0},
     {nonempty_maybe_improper_list,2}, {nonempty_string,0},
     {number,0}, {pid,0}, {port,0}, {pos_integer,0}, {reference,0},
     {string,0}, {term,0}, {timeout,0}, {tuple,0}].

guard_bifs() ->
    [{abs,1}, {binary_part,2}, {binary_part,3}, {bit_size,1},
     {byte_size,1}, {ceil,1}, {element,2}, {float,1}, {floor,1},
     {hd,1}, {is_map_key,2}, {length,1}, {map_size,1}, {map_get,2},
     {node,0}, {node,1}, {round,1}, {self,0}, {size,1}, {tl,1},
     {trunc,1}, {tuple_size,1}, {is_atom,1}, {is_binary,1},
     {is_bitstring,1}, {is_boolean,1}, {is_float,1}, {is_function,1},
     {is_function,2}, {is_integer,1}, {is_list,1}, {is_map,1},
     {is_number,1}, {is_pid,1}, {is_port,1}, {is_reference,1},
     {is_tuple,1}].

other_bifs() ->
    [{is_record,2}, {is_record,3}].

expr_ops() ->
    %% Can be used in expressions with the "erlang:"-prefix.
    [{'++',2}, {'--',2}, {'!',2}] ++ guard_ops().

guard_ops() ->
    %% Like guard_binop() and any_unop(), but excluding
    %% andalso, orelse, ++, --, and !.
    %% Can be used in guards with the "erlang:"-prefix.
    [{'+',2}, {'+',1}, {'-',2}, {'-',1}, {'*',2}, {'/',2},
     {'div',2}, {'rem',2}, {'band',2}, {'bnot',1}, {'bor',2}, {'bxor',2},
     {'bsl',2}, {'bsr',2},
     {'and',2}, {'or',2}, {'not',1}, {'xor',2},
     {'=:=',2}, {'=/=',2}, {'==',2}, {'/=',2},
     {'=<',2}, {'<',2}, {'>=',2}, {'>',2}].

uniform(N) ->
    rand:uniform(N).
