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

%%% @author Manolis Papadakis <manopapad@gmail.com>
%%% @copyright 2010 Manolis Papadakis
%%% @version {@version}
%%% @doc This is the main PropEr module.

-module(proper).
-export([set_size/1, erase_size/0, grow_size/0, get_size/1, parse_opts/1,
	 global_state_init/1, global_state_erase/1]).
-export([numtests/2, collect/2, fails/1, equals/2]).
-export([check/1, check/2, child/4, still_fails/3, skip_to_next/1]).
-export([print/3]).

-export_type([imm_testcase/0, test/0, forall_clause/0, fail_reason/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type imm_testcase() :: [proper_gen:imm_instance()].
-type clean_testcase() :: [proper_gen:instance()].
-type category() :: term().
-type cat_dict() :: [{category(),frequency()}].
-type side_effects_fun() :: fun(() -> 'ok').
-type fail_actions() :: [side_effects_fun()].
-type time_period() :: non_neg_integer().

-type outer_test() :: test()
		    | numtests_clause()
		    | fails_clause().
-type test() :: boolean()
	      | forall_clause()
	      | implies_clause()
	      | collect_clause()
	      | whenfail_clause()
	      | trapexit_clause()
	      | timeout_clause()
	      %%| always_clause()
	      %%| sometimes_clause()
	      | apply_clause().
-type delayed_test() :: fun(() -> test()).

-type numtests_clause() :: {'$numtests', pos_integer(), outer_test()}.
-type fails_clause() :: {'$fails', outer_test()}.
-type forall_clause() :: {'$forall', proper_types:raw_type(),
			  fun((proper_gen:instance()) -> test())}.
-type implies_clause() :: {'$implies', boolean(), delayed_test()}.
-type collect_clause() :: {'$collect', category(), test()}.
-type whenfail_clause() :: {'$whenfail', side_effects_fun(), delayed_test()}.
-type trapexit_clause() :: {'$trapexit', delayed_test()}.
-type timeout_clause() :: {'$timeout', time_period(), delayed_test()}.
%%-type always_clause() :: {'$always', pos_integer(), delayed_test()}.
%%-type sometimes_clause() :: {'$sometimes', pos_integer(), delayed_test()}.
-type apply_clause() :: {'$apply', [term()], function()}.

-type opt() :: 'quiet'
	     | 'crypto'
	     | {'numtests', pos_integer()}
	     | pos_integer()
	     | {'max_shrinks', non_neg_integer()}
	     | {'constraint_tries', pos_integer()}
	     | 'fails'.
%% TODO: other ways for the user to define the extra exceptions to catch?
%% TODO: should they contain specific reasons (or '$any' for all reasons)?
%% TODO: allow errors to be caught?
-record(ctx, {catch_exits  = false :: boolean(),
	      bound        = []    :: imm_testcase(),
	      fail_actions = []    :: fail_actions(),
	      categories   = []    :: [category()]}).
-type single_run_result() :: {'passed', 'didnt_crash'}
			   | {'passed', {'categories',[category()]}}
			   | {'failed', fail_reason(), imm_testcase(),
			      fail_actions()}
			   | {'error', 'wrong_type'}
			   | {'error', 'cant_generate'}
			   | {'error', 'rejected'}.
-type exc_kind() :: 'throw' | 'exit'.
-type exc_reason() :: term().
-type fail_reason() :: 'false_property' | 'timeout' | {exc_kind(),exc_reason()}.
-type common_result() :: {'passed', pos_integer(), [cat_dict()]}
		       | {'error', 'cant_generate'}
		       | {'error', 'cant_satisfy'}
		       | {'error', {'unexpected', single_run_result()}}.
-type imm_result() :: common_result()
		    | {'failed', pos_integer(), fail_reason(), imm_testcase(),
		       fail_actions()}.
-type final_result() :: common_result()
		      | {'failed', pos_integer(), fail_reason(),
			 clean_testcase()}
		      | {'failed', pos_integer(), fail_reason(),
			 clean_testcase(), non_neg_integer(), clean_testcase()}.


%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

-spec set_size(size()) -> 'ok'.
set_size(Size) ->
    put('$size', Size),
    ok.

-spec erase_size() -> 'ok'.
erase_size() ->
    erase('$size'),
    ok.

-spec get_size() -> size().
get_size() ->
    get('$size').

-spec grow_size() -> 'ok'.
grow_size() ->
    Size = get_size(),
    set_size(Size + 1),
    ok.

-spec get_size(proper_types:type()) -> size().
get_size(Type) ->
    Size1 = get_size(),
    Size2 = case proper_types:find_prop(size_transform, Type) of
		{ok, Transform} -> Transform(Size1);
		error           -> Size1
	    end,
    %% TODO: should the size be normalized (streched or pressed)?
    case proper_types:find_prop(size_limit, Type) of
	{ok, Limit} -> erlang:min(Size2, Limit);
	error       -> Size2
    end.

-spec parse_opts([opt()] | opt()) -> #opts{}.
parse_opts(OptsList) ->
    parse_opts_tr(OptsList, #opts{}).

-spec parse_opts_tr([opt()] | opt(), #opts{}) -> #opts{}.
parse_opts_tr([], Opts) ->
    Opts;
parse_opts_tr([Opt | Rest], Opts) ->
    parse_opts_tr(Rest, parse_opt(Opt,Opts));
parse_opts_tr(Opt, Opts) ->
    parse_opt(Opt, Opts).

-spec parse_opt(opt(), #opts{}) -> #opts{}.
parse_opt(Opt, Opts) ->
    case Opt of
	quiet                        -> Opts#opts{quiet = true};
	crypto                       -> Opts#opts{crypto = true};
	{numtests,N}                 -> Opts#opts{numtests = N};
	N when is_integer(N), N > 0  -> Opts#opts{numtests = N};
	{max_shrinks,N}              -> Opts#opts{max_shrinks = N};
	{constraint_tries,N}         -> Opts#opts{constraint_tries = N};
	fails                        -> Opts#opts{expect_fail = true}
    end.


%%------------------------------------------------------------------------------
%% Test generation functions
%%------------------------------------------------------------------------------

-spec numtests(pos_integer(), outer_test()) -> numtests_clause().
numtests(N, Test) -> {'$numtests',N,Test}.

-spec collect(category(), test()) -> collect_clause().
collect(Category, Prop) -> {'$collect',Category,Prop}.

-spec fails(outer_test()) -> fails_clause().
fails(Test) -> {'$fails',Test}.

-spec equals(term(), term()) -> whenfail_clause().
equals(A, B) ->
    ?WHENFAIL(io:format("~w =/= ~w~n", [A, B]), A =:= B).


%%------------------------------------------------------------------------------
%% Main usage functions
%%------------------------------------------------------------------------------

-spec check(outer_test()) -> final_result() | 'ok'.
check(Test) ->
    check(Test, #opts{}).

-spec check(outer_test(), #opts{} | [opt()] | opt()) -> final_result() | 'ok'.
check({'$numtests',N,Test}, #opts{} = Opts) ->
    check(Test, Opts#opts{numtests = N});
%% We only allow a 'fails' to be an external wrapper, since the property
%% wrapped by a 'fails' is not delayed, and thus a failure-inducing exception
%% will cause the test to fail before the 'fails' is processed.
check({'$fails',Test}, #opts{} = Opts) ->
    check(Test, Opts#opts{expect_fail = true});
check(Test, #opts{numtests = NumTests, quiet = Quiet} = Opts) ->
    global_state_init(Opts),
    ImmResult = perform(0, NumTests, Test, none, Opts),
    report_imm_result(ImmResult, Opts),
    FinalResult = get_final_result(ImmResult, Test, Opts),
    global_state_erase(Opts),
    case Quiet of
	true  -> FinalResult;
	false -> ok
    end;
check(Test, OptsList) ->
    check(Test, parse_opts(OptsList)).

-spec get_final_result(imm_result(), test(), #opts{}) -> final_result().
get_final_result({failed,Performed,Reason,ImmFailedTestCase,_FailActions}, Test,
		 #opts{expect_fail = ExpectFail} = Opts) ->
    FailedTestCase = proper_gen:clean_instance(ImmFailedTestCase),
    case ExpectFail of
	false ->
	    {Shrinks, ImmMinTestCase} =
		proper_shrink:shrink(ImmFailedTestCase, Test, Reason, Opts),
	    NewOpts = #opts{quiet = true, try_shrunk = true,
			    shrunk = ImmMinTestCase},
	    {failed, _Reason, _Bound, MinFailActions} = run(Test, NewOpts),
	    MinTestCase = proper_gen:clean_instance(ImmMinTestCase),
	    report_shrinking(Shrinks, MinTestCase, MinFailActions, Opts),
	    {failed, Performed, Reason, FailedTestCase, Shrinks, MinTestCase};
	true ->
	    {failed, Performed, Reason, FailedTestCase}
    end;
get_final_result(ImmResult, _Test, _Opts) ->
    ImmResult.

-spec global_state_init(#opts{}) -> 'ok'.
global_state_init(Opts) ->
    put('$constraint_tries', Opts#opts.constraint_tries),
    proper_arith:rand_start(Opts),
    set_size(0),
    ok.

-spec global_state_erase(#opts{}) -> 'ok'.
global_state_erase(Opts) ->
    erase_size(),
    proper_arith:rand_stop(Opts),
    erase('$constraint_tries'),
    ok.

-spec perform(non_neg_integer(), non_neg_integer(), test(),
	      [cat_dict()] | 'none', #opts{}) -> imm_result().
perform(0, 0, _Test, _CatDicts, _Opts) ->
    {error, cant_satisfy};
perform(Performed, 0, _Test, CatDicts, _Opts) ->
    {passed, Performed, CatDicts};
perform(Performed, Left, Test, CatDicts, Opts) ->
    case run(Test, Opts) of
	{passed, {categories,Categories}} ->
	    print(".", [], Opts),
	    NewCatDicts = update_catdicts(Categories, CatDicts),
	    grow_size(),
	    perform(Performed + 1, Left - 1, Test, NewCatDicts, Opts);
	{failed, Reason, Bound, FailActions} ->
	    print("!", [], Opts),
	    {failed, Performed + 1, Reason, Bound, FailActions};
	{error, cant_generate} = Error ->
	    Error;
	{error, rejected} ->
	    print("x", [], Opts),
	    grow_size(),
	    perform(Performed, Left - 1, Test, CatDicts, Opts);
	Unexpected ->
	    {error, {unexpected, Unexpected}}
    end.

-spec update_catdicts([category()], [cat_dict()] | 'none') -> [cat_dict()].
update_catdicts(Categories, none) ->
    [orddict:from_list([{C,1}]) || C <- Categories];
update_catdicts(Categories, CatDicts) ->
    lists:zipwith(fun(C,D) -> add_to_category(C,D) end,
		  Categories, CatDicts).

-spec add_to_category(category(), cat_dict()) -> cat_dict().
add_to_category(Category, CatDict) ->
    case orddict:find(Category, CatDict) of
	{ok, Count} -> orddict:store(Category, Count + 1, CatDict);
	error       -> orddict:store(Category, 1, CatDict)
    end.

-spec run(test(), #opts{}) -> single_run_result().
run(Test, Opts) ->
    run(Test, #ctx{}, Opts).

-spec run(test(), #ctx{}, #opts{}) -> single_run_result().
run(true, Context, _Opts) ->
    {passed, {categories,lists:reverse(Context#ctx.categories)}};
run(false, Context, _Opts) ->
    {failed, false_property, lists:reverse(Context#ctx.bound),
     lists:reverse(Context#ctx.fail_actions)};
run({'$forall',RawType,Prop}, Context = #ctx{bound = Bound}, Opts) ->
    Type = proper_types:cook_outer(RawType),
    case {Opts#opts.try_shrunk, Opts#opts.shrunk} of
	{true, []} ->
	    {passed, didnt_crash};
	{true, [ImmInstance | Rest]} ->
	    case proper_arith:surely(proper_types:is_instance(ImmInstance,
							      Type)) of
		true ->
		    Instance = proper_gen:clean_instance(ImmInstance),
		    NewOpts = Opts#opts{shrunk = Rest},
		    run({'$apply',[Instance],Prop}, Context, NewOpts);
		false ->
		    {error, wrong_type}
	    end;
	{false, _} ->
	    case proper_gen:generate(Type) of
		'$cant_generate' ->
		    {error, cant_generate};
		ImmInstance ->
		    Instance = proper_gen:clean_instance(ImmInstance),
		    NewContext = Context#ctx{bound = [ImmInstance | Bound]},
		    run({'$apply',[Instance],Prop}, NewContext, Opts)
	    end
    end;
run({'$implies',Pre,Prop}, Context, Opts) ->
    case Pre of
	true  -> run({'$apply',[],Prop}, Context, Opts);
	false -> {error, rejected}
    end;
run({'$collect',NewCategory,Prop}, Context = #ctx{categories = Categories},
	Opts) ->
    NewContext = Context#ctx{categories = [NewCategory | Categories]},
    run(Prop, NewContext, Opts);
run({'$whenfail',Action,Prop}, Context = #ctx{fail_actions = FailActions},
	Opts) ->
    NewContext = Context#ctx{fail_actions = [Action | FailActions]},
    run({'$apply',[],Prop}, NewContext, Opts);
run({'$trapexit',Prop}, Context, Opts) ->
    NewContext = Context#ctx{catch_exits = true},
    run({'$apply',[],Prop}, NewContext, Opts);
run({'$timeout',Limit,Prop}, Context, Opts) ->
    Child = spawn_link(?MODULE, child, [self(), Prop, Context, Opts]),
    receive
	{result, Result} -> Result
    after Limit ->
	unlink(Child),
	exit(Child, kill),
	clear_mailbox(),
	setelement(2, run(false,Context,Opts), timeout)
    end;
run({'$apply',Args,Prop}, Context, Opts) ->
    try
	%% TODO: should we care what the code returns when trapping exits?
	%%       if we are doing that, we are probably testing code that will
	%%       run as a separate process against crashes
	run(apply(Prop,Args), Context, Opts)
    catch
	throw:ExcReason ->
	    setelement(2, run(false,Context,Opts), {throw,ExcReason});
	exit:ExcReason when Context#ctx.catch_exits ->
	    setelement(2, run(false,Context,Opts), {exit,ExcReason})
    end.

-spec child(pid(), delayed_test(), #ctx{}, #opts{}) -> 'ok'.
child(Father, Prop, Context, Opts) ->
    Result = run({'$apply',[],Prop}, Context, Opts),
    Father ! {result, Result},
    ok.

-spec clear_mailbox() -> 'ok'.
clear_mailbox() ->
    receive
	_ -> clear_mailbox()
    after 0 ->
	ok
    end.

-spec still_fails(imm_testcase(), test(), fail_reason()) -> boolean().
still_fails(TestCase, Test, OldReason) ->
    Opts = #opts{quiet = true, try_shrunk = true, shrunk = TestCase},
    case run(Test, Opts) of
	%% We check that it's the same fault that caused the crash.
	%% TODO: Should we check that the stacktrace is the same?
	{failed, Reason, _Bound, _FailActions} -> OldReason =:= Reason;
	_                                      -> false
    end.

-spec skip_to_next(test()) -> forall_clause() | 'false' | 'error'.
%% We should never encounter false ?IMPLIES, true final results or unprecedented
%% tests.
skip_to_next(true) ->
    error;
skip_to_next(false) ->
    false;
skip_to_next(Test = {'$forall',_RawType,_Prop}) ->
    Test;
skip_to_next({'$implies',true,Prop}) ->
    skip_to_next({'$apply',[],Prop});
skip_to_next({'$implies',false,_Prop}) ->
    error;
skip_to_next({'$collect',_Category,Prop}) ->
    skip_to_next(Prop);
skip_to_next({'$whenfail',_Action,Prop}) ->
    skip_to_next({'$apply',[],Prop});
skip_to_next({'$trapexit',Prop}) ->
    skip_to_next({'$apply',[],Prop});
skip_to_next({'$timeout',_Limit,_Prop}) ->
    false; % this is OK, since timeout cannot contain any ?FORALLs
skip_to_next({'$apply',Args,Prop}) ->
    try
	skip_to_next(apply(Prop, Args))
    catch
	%% TODO: should be OK to catch everything here, since we have
	%%       already tested at this point that the test still fails
	_ExcKind:_ExcReason -> false
    end.


%%------------------------------------------------------------------------------
%% Output functions
%%------------------------------------------------------------------------------

-spec print(string(), [term()], #opts{}) -> 'ok'.
print(Str, Args, Opts) ->
    case Opts#opts.quiet of
	true  -> ok;
	false -> io:format(Str, Args)
    end.

-spec report_imm_result(imm_result(), #opts{}) -> 'ok'.
report_imm_result({error,{unexpected,Unexpected}}, _Opts) ->
    io:format("~nInternal error: the last run returned an unexpected result:~n"
	      "~w~nPlease notify the maintainers about this error~n",
	      [Unexpected]),
    ok;
report_imm_result(_Result, #opts{quiet = true}) ->
    ok;
report_imm_result({passed,Performed,CatDicts}, Opts) ->
    case Opts#opts.expect_fail of
	true  -> io:format("~nError: no test failed~n", []);
	false -> io:format("~nOK, passed ~b tests~n", [Performed])
    end,
    print_categories(Performed, CatDicts),
    ok;
report_imm_result({failed,Performed,_Reason,ImmFailedTestCase,FailActions},
		  Opts) ->
    case Opts#opts.expect_fail of
	true ->
	    io:format("~nOK, failed as expected, after ~b tests.~n",
		      [Performed]);
	false ->
	    io:format("~nFailed, after ~b tests.~n", [Performed]),
	    FailedTestCase = proper_gen:clean_instance(ImmFailedTestCase),
	    print_instances(FailedTestCase),
	    execute_actions(FailActions),
	    io:format("Shrinking", [])
    end,
    ok;
report_imm_result({error,cant_generate}, _Opts) ->
    io:format("~nError: couldn't produce an instance that satisfies all strict"
	      " constraints after ~b tries~n", [get('$constraint_tries')]),
    ok;
report_imm_result({error,cant_satisfy}, _Opts) ->
    io:format("~nError: no valid test could be generated.~n", []),
    ok.

-spec print_categories(pos_integer(), [cat_dict()]) -> 'ok'.
print_categories(_Performed, []) ->
    ok;
print_categories(Performed, [CatDict]) ->
    lists:foreach(fun({C,N}) ->
		      io:format("~7.3f\% ~w~n", [100 * N / Performed,C])
		  end,
		  orddict:to_list(CatDict)),
    ok;
print_categories(Performed, [CatDict | Rest]) ->
    print_categories(Performed, [CatDict]),
    io:format("~n", []),
    print_categories(Performed, Rest).

-spec print_instances(clean_testcase()) -> 'ok'.
print_instances(Instances) ->
    lists:foreach(fun(I) -> io:format("~w~n", [I]) end, Instances),
    ok.

-spec execute_actions(fail_actions()) -> 'ok'.
execute_actions(FailActions) ->
    lists:foreach(fun(A) -> ?FORCE(A) end, FailActions),
    ok.

-spec report_shrinking(non_neg_integer(), clean_testcase(), fail_actions(),
		       #opts{}) -> 'ok'.
report_shrinking(_Shrinks, _MinTestCase, _FailActions, #opts{quiet = true}) ->
    ok;
report_shrinking(Shrinks, MinTestCase, FailActions, _Opts) ->
    io:format("(~b times)~n", [Shrinks]),
    print_instances(MinTestCase),
    execute_actions(FailActions),
    ok.
