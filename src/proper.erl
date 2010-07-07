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
%%% @doc This is the main PropEr module.

-module(proper).
-export([check/1, check/2]).
-export([global_state_erase/0, get_counterexample/0, clean_garbage/0,
	 get_fail_reason/1, get_bound/1]).

-export([get_size/1, global_state_init_size/1]).
-export([numtests/2, collect/2, aggregate/2, fails/1, on_output/2, equals/2]).
-export([still_fails/3, skip_to_next/1]).

-export_type([dependent_test/0, imm_testcase/0, test/0, fail_reason/0,
	      forall_clause/0, output_fun/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Test types
%%------------------------------------------------------------------------------

-type imm_testcase() :: [proper_gen:imm_instance()].
-type clean_testcase() :: [proper_gen:instance()].
-type sample() :: [term()].
-type side_effects_fun() :: fun(() -> 'ok').
-type fail_actions() :: [side_effects_fun()].
-type output_fun() :: fun((string(), [term()]) -> 'ok').
-type time_period() :: non_neg_integer().

-type outer_test() :: test()
		    | numtests_clause()
		    | fails_clause()
		    | on_output_clause().
-type test() :: boolean()
	      | forall_clause()
	      | implies_clause()
	      | sample_clause()
	      | whenfail_clause()
	      | trapexit_clause()
	      | timeout_clause()
	      %%| always_clause()
	      %%| sometimes_clause()
	      | apply_clause().
-type dependent_test() :: fun((proper_gen:instance()) -> test()).
-type delayed_test() :: fun(() -> test()).

-type numtests_clause() :: {'$numtests', pos_integer(), outer_test()}.
-type fails_clause() :: {'$fails', outer_test()}.
-type on_output_clause() :: {'$on_output', output_fun(), outer_test()}.

-type forall_clause() :: {'$forall', proper_types:raw_type(), dependent_test()}.
-type implies_clause() :: {'$implies', boolean(), delayed_test()}.
-type sample_clause() :: {'$sample', sample(), test()}.
-type whenfail_clause() :: {'$whenfail', side_effects_fun(), delayed_test()}.
-type trapexit_clause() :: {'$trapexit', delayed_test()}.
-type timeout_clause() :: {'$timeout', time_period(), delayed_test()}.
%%-type always_clause() :: {'$always', pos_integer(), delayed_test()}.
%%-type sometimes_clause() :: {'$sometimes', pos_integer(), delayed_test()}.
-type apply_clause() :: {'$apply', [term()], function()}.


%%------------------------------------------------------------------------------
%% Options and Context types
%%------------------------------------------------------------------------------

-type user_opt() :: 'quiet'
		  | {'to_file', file:io_device()}
		  | {'on_output', output_fun()}
		  | 'long_result'
		  | 'crypto'
		  | {'numtests', pos_integer()}
		  | pos_integer()
		  | {'start_size', size()}
		  | {'max_shrinks', non_neg_integer()}
		  | 'noshrink'
		  | {'constraint_tries', pos_integer()}
		  | 'fails'.
-record(opts, {output_fun       = fun io:format/2 :: output_fun(),
	       long_result      = false           :: boolean(),
	       crypto           = false           :: boolean(),
	       numtests         = 100             :: pos_integer(),
	       start_size       = 1               :: size(),
	       max_shrinks      = 500             :: non_neg_integer(),
	       noshrink         = false           :: boolean(),
	       constraint_tries = 50              :: pos_integer(),
	       expect_fail      = false           :: boolean()}).
-type opts() :: #opts{}.
%% TODO: other ways for the user to define the extra exceptions to catch?
%% TODO: should they contain specific reasons (or '$any' for all reasons)?
%% TODO: allow errors to be caught?
-record(ctx, {catch_exits  = false :: boolean(),
	      try_shrunk   = false :: boolean(),
	      bound        = []    :: imm_testcase(),
	      to_try       = []    :: imm_testcase(),
	      fail_actions = []    :: fail_actions(),
	      samples      = []    :: [sample()]}).
-type ctx() :: #ctx{}.


%%------------------------------------------------------------------------------
%% Result types
%%------------------------------------------------------------------------------

-type single_run_result() :: {'passed', pass_reason(), [sample()]}
			   | {'failed', counterexample(), fail_actions()}
			   | {'error', single_run_error_reason()}.
-type pass_reason() :: 'true_prop' | 'didnt_crash'.
-type fail_reason() :: 'false_prop' | 'timeout'
		     | {'exception',exc_kind(),exc_reason(),stacktrace()}.
-type exc_kind() :: 'throw' | 'exit'.
-type exc_reason() :: term().
-type stacktrace() :: [{atom(),atom(),arity() | [term()]}].
-type single_run_error_reason() :: 'wrong_type' | 'cant_generate' | 'rejected'
				 | 'type_mismatch' | 'need_size_info'
				 | 'too_many_instances'.

-type pass_result() :: {'passed', pos_integer(), [sample()]}.
-type error_result() :: {'error', error_reason()}.
-type error_reason() :: 'cant_generate' | 'cant_satisfy' | 'type_mismatch'
		      | {'unexpected', single_run_result()}.
-type common_result() :: pass_result() | error_result().
-type imm_result() :: common_result()
		    | {'failed',pos_integer(),counterexample(),fail_actions()}.
-type long_result() :: common_result()
		     | {'failed', pos_integer(), counterexample()}
		     | {'failed', pos_integer(), counterexample(),
			 non_neg_integer(), counterexample()}.
-type short_result() :: boolean() | error_result().

-record(cexm, {fail_reason :: fail_reason(),
	       bound       :: imm_testcase(),
	       size        :: size(),
	       gen_state   :: proper_gen:gen_state()}).
-type counterexample() :: #cexm{}.


%%------------------------------------------------------------------------------
%% State handling functions
%%------------------------------------------------------------------------------

-spec get_size() -> size() | 'undefined'.
get_size() ->
    get('$size').

-spec grow_size() -> 'ok'.
grow_size() ->
    Size = get('$size'),
    put('$size', Size + 1),
    ok.

-spec get_size(proper_types:type()) -> size() | 'undefined'.
get_size(Type) ->
    case get('$size') of
	undefined ->
	    undefined;
	Size1 ->
	    Size2 = case proper_types:find_prop(size_transform, Type) of
			 {ok, Transform} -> Transform(Size1);
			 error           -> Size1
		     end,
	    %% TODO: should the size be normalized (streched or pressed)?
	    case proper_types:find_prop(size_limit, Type) of
		{ok, Limit} -> erlang:min(Size2, Limit);
		error       -> Size2
	    end
    end.

-spec global_state_init_size(size()) -> 'ok'.
global_state_init_size(Size) ->
    global_state_init(#opts{start_size = Size}).

-spec global_state_init(opts()) -> 'ok'.
global_state_init(#opts{start_size = Size, constraint_tries = CTries,
			crypto = Crypto}) ->
    clean_garbage(),
    put('$size', Size),
    put('$constraint_tries', CTries),
    proper_arith:rand_start(Crypto),
    ok.

-spec global_state_erase() -> 'ok'.
global_state_erase() ->
    proper_gen:gen_state_erase(),
    proper_arith:rand_stop(),
    erase('$constraint_tries'),
    erase('$size'),
    ok.

-spec save_counterexample(counterexample()) -> 'ok'.
save_counterexample(CExm) ->
    put('$counterexample', CExm),
    ok.

-spec get_counterexample() -> counterexample() | 'undefined'.
get_counterexample() ->
    get('$counterexample').

-spec clean_garbage() -> 'ok'.
clean_garbage() ->
    erase('$counterexample'),
    ok.

-spec get_fail_reason(counterexample()) -> fail_reason().
get_fail_reason(#cexm{fail_reason = Reason}) ->
    Reason.

-spec get_bound(counterexample()) -> clean_testcase().
get_bound(#cexm{bound = ImmTestCase}) ->
    clean_testcase(ImmTestCase).


%%------------------------------------------------------------------------------
%% Options support
%%------------------------------------------------------------------------------

-spec parse_opts([user_opt()] | user_opt()) -> opts().
parse_opts(OptsList) ->
    parse_opts(OptsList, #opts{}).

-spec parse_opts([user_opt()] | user_opt(), opts()) -> opts().
parse_opts([], Opts) ->
    Opts;
parse_opts([UserOpt | Rest], Opts) ->
    parse_opts(Rest, parse_opt(UserOpt,Opts));
parse_opts(UserOpt, Opts) ->
    parse_opt(UserOpt, Opts).

-spec parse_opt(user_opt(), opts()) -> opts().
parse_opt(UserOpt, Opts) ->
    case UserOpt of
	quiet                        -> Opts#opts{output_fun =
					    fun(_,_) -> ok end};
	{to_file,IoDev}              -> Opts#opts{output_fun =
					    fun(S,F) ->
						io:format(IoDev, S, F)
					    end};
	{on_output,Print}            -> Opts#opts{output_fun = Print};
	long_result                  -> Opts#opts{long_result = true};
	crypto                       -> Opts#opts{crypto = true};
	{numtests,N}                 -> Opts#opts{numtests = N};
	N when is_integer(N), N > 0  -> Opts#opts{numtests = N};
	{start_size,Size}            -> Opts#opts{start_size = Size};
	{max_shrinks,N}              -> Opts#opts{max_shrinks = N};
	noshrink                     -> Opts#opts{noshrink = true};
	{constraint_tries,N}         -> Opts#opts{constraint_tries = N};
	fails                        -> Opts#opts{expect_fail = true};
	_                            -> Opts
    end.


%%------------------------------------------------------------------------------
%% Test declaration functions
%%------------------------------------------------------------------------------

-spec numtests(pos_integer(), outer_test()) -> numtests_clause().
numtests(N, Test) ->
    {'$numtests', N, Test}.

-spec collect(term(), test()) -> sample_clause().
collect(SingleSample, Prop) ->
    aggregate([SingleSample], Prop).

-spec aggregate(sample(), test()) -> sample_clause().
aggregate(Sample, Prop) ->
    {'$sample', Sample, Prop}.

-spec fails(outer_test()) -> fails_clause().
fails(Test) ->
    {'$fails', Test}.

-spec on_output(output_fun(), outer_test()) -> on_output_clause().
on_output(Print, Test) ->
    {'$on_output', Print, Test}.

-spec equals(term(), term()) -> whenfail_clause().
equals(A, B) ->
    ?WHENFAIL(io:format("~w =/= ~w~n",[A,B]), A =:= B).


%%------------------------------------------------------------------------------
%% Main usage functions
%%------------------------------------------------------------------------------

-spec check(outer_test()) -> long_result() | short_result().
check(Test) ->
    check(Test, #opts{}).

%% We only allow a 'fails' to be an external wrapper, since the property
%% wrapped by a 'fails' is not delayed, and thus a failure-inducing exception
%% will cause the test to fail before the 'fails' is processed.
-spec check(outer_test(), opts() | [user_opt()] | user_opt()) ->
	  long_result() | short_result().
check({'$numtests',N,Test}, #opts{} = Opts) ->
    check(Test, Opts#opts{numtests = N});
check({'$fails',Test}, #opts{} = Opts) ->
    check(Test, Opts#opts{expect_fail = true});
check({'$on_output',Print,Test}, #opts{} = Opts) ->
    check(Test, Opts#opts{output_fun = Print});
check(Test, #opts{numtests = NumTests, output_fun = Print,
		  long_result = ReturnLong} = Opts) ->
    global_state_init(Opts),
    ImmResult = perform(NumTests, Test, Print),
    report_imm_result(ImmResult, Opts),
    ShortResult = get_short_result(ImmResult, Opts),
    LongResult = get_long_result(ImmResult, Test, Opts),
    global_state_erase(),
    case ReturnLong of
	true  -> LongResult;
	false -> ShortResult
    end;
check(Test, UserOpts) ->
    check(Test, parse_opts(UserOpts)).

-spec get_short_result(imm_result(), opts()) -> short_result().
get_short_result({passed,_Passed,_Samples}, Opts) ->
    not Opts#opts.expect_fail;
get_short_result({failed,_Performed,_CExm,_Actions}, Opts) ->
    Opts#opts.expect_fail;
get_short_result({error,_Reason} = ErrorResult, _Opts) ->
    ErrorResult.

-spec get_long_result(imm_result(), test(), opts()) -> long_result().
get_long_result({failed,Performed,CExm,_Actions}, Test,
		 #opts{expect_fail = false, noshrink = false,
		       max_shrinks = MaxShrinks, output_fun = Print}) ->
    Print("Shrinking", []),
    #cexm{fail_reason = Reason, bound = ImmTestCase} = CExm,
    {Shrinks, MinImmTestCase} =
	proper_shrink:shrink(ImmTestCase, Test, Reason, MaxShrinks, Print),
    Ctx = #ctx{try_shrunk = true, to_try = MinImmTestCase},
    {failed, MinCExm, MinActions} = run(Test, Ctx),
    report_shrinking(Shrinks, MinImmTestCase, MinActions, Print),
    save_counterexample(MinCExm),
    {failed, Performed, CExm, Shrinks, MinCExm};
get_long_result({failed,Performed,CExm,_Actions}, _Test, _Opts) ->
    save_counterexample(CExm),
    {failed, Performed, CExm};
get_long_result(ImmResult, _Test, _Opts) ->
    ImmResult.

-spec perform(non_neg_integer(), test(), output_fun()) -> imm_result().
perform(NumTests, Test, Print) ->
    perform(0, NumTests, ?MAX_TRIES_FACTOR * NumTests, Test, none, Print).

-spec perform(non_neg_integer(), non_neg_integer(), non_neg_integer(), test(),
	      [sample()] | 'none', output_fun()) -> imm_result().
perform(0, _ToPass, 0, _Test, _Samples, _Print) ->
    {error, cant_satisfy};
perform(Passed, _ToPass, 0, _Test, Samples, _Print) ->
    {passed, Passed, Samples};
perform(ToPass, ToPass, _TriesLeft, _Test, Samples, _Print) ->
    {passed, ToPass, Samples};
perform(Passed, ToPass, TriesLeft, Test, Samples, Print) ->
    proper_gen:gen_state_erase(),
    case run(Test) of
	{passed, true_prop, MoreSamples} ->
	    Print(".", []),
	    NewSamples = add_samples(MoreSamples, Samples),
	    grow_size(),
	    perform(Passed+1, ToPass, TriesLeft-1, Test, NewSamples, Print);
	{failed, CExm, Actions} ->
	    Print("!", []),
	    {failed, Passed+1, CExm, Actions};
	{error, cant_generate} = Error ->
	    Error;
	{error, type_mismatch} = Error ->
	    Error;
	{error, rejected} ->
	    Print("x", []),
	    grow_size(),
	    perform(Passed, ToPass, TriesLeft-1, Test, Samples, Print);
	Unexpected ->
	    {error, {unexpected,Unexpected}}
    end.

-spec add_samples([sample()], [sample()] | 'none') -> [sample()].
add_samples(MoreSamples, none) ->
    MoreSamples;
add_samples(MoreSamples, Samples) ->
    lists:zipwith(fun erlang:'++'/2, MoreSamples, Samples).

-spec run(test()) -> single_run_result().
run(Test) ->
    run(Test, #ctx{}).

-spec run(test(), ctx()) -> single_run_result().
run(true, #ctx{to_try = [], samples = Samples}) ->
    {passed, true_prop, lists:reverse(Samples)};
run(true, _Ctx) ->
    {error, too_many_instances};
run(false, #ctx{to_try = []} = Ctx) ->
    create_failed_result(Ctx, false_prop);
run(false, _Ctx) ->
    {error, too_many_instances};
run({'$forall',RawType,Prop},
    #ctx{try_shrunk = TryShrunk, to_try = ToTry, bound = Bound} = Ctx) ->
    case {TryShrunk, ToTry} of
	{true, []} ->
	    {passed, didnt_crash, []};
	{true, [ImmInstance | Rest]} ->
	    case proper_arith:surely(proper_types:is_instance(ImmInstance,
							      RawType)) of
		true ->
		    Instance = proper_gen:clean_instance(ImmInstance),
		    NewCtx =
			Ctx#ctx{to_try = Rest, bound = [ImmInstance | Bound]},
		    run({'$apply',[Instance],Prop}, NewCtx);
		false ->
		    {error, wrong_type}
	    end;
	{false, []} ->
	    case proper_gen:generate(RawType) of
		'$cant_generate' ->
		    {error, cant_generate};
		ImmInstance ->
		    Instance = proper_gen:clean_instance(ImmInstance),
		    NewCtx = Ctx#ctx{bound = [ImmInstance | Bound]},
		    run({'$apply',[Instance],Prop}, NewCtx)
	    end
    end;
run({'$implies',true,Prop}, Ctx) ->
    run({'$apply',[],Prop}, Ctx);
run({'$implies',false,_Prop}, _Ctx) ->
    {error, rejected};
run({'$sample',NewSample,Prop}, #ctx{samples = Samples} = Ctx) ->
    NewCtx = Ctx#ctx{samples = [NewSample | Samples]},
    run(Prop, NewCtx);
run({'$whenfail',NewAction,Prop}, #ctx{fail_actions = Actions} = Ctx)->
    NewCtx = Ctx#ctx{fail_actions = [NewAction | Actions]},
    run({'$apply',[],Prop}, NewCtx);
run({'$trapexit',Prop}, Ctx) ->
    NewCtx = Ctx#ctx{catch_exits = true},
    run({'$apply',[],Prop}, NewCtx);
run({'$timeout',Limit,Prop}, Ctx) ->
    Self = self(),
    Child = spawn_link(fun() -> child(Self,Prop,Ctx) end),
    receive
	{result, Result} -> Result
    after Limit ->
	unlink(Child),
	exit(Child, kill),
	clear_mailbox(),
	create_failed_result(Ctx, timeout)
    end;
run({'$apply',Args,Prop}, Ctx) ->
    try
	%% TODO: should we care if the code returns true when trapping exits?
	%%       If we are doing that, we are probably testing code that will
	%%       run as a separate process against crashes.
	run(apply(Prop,Args), Ctx)
    catch
	error:function_clause ->
	    {error, type_mismatch};
	throw:'$cant_generate' ->
	    {error, cant_generate};
	throw:'$need_size_info' ->
	    {error, need_size_info};
	throw:ExcReason ->
	    create_failed_result(Ctx, {exception, throw, ExcReason,
				       erlang:get_stacktrace()});
	exit:ExcReason when Ctx#ctx.catch_exits ->
	    create_failed_result(Ctx, {exception, exit, ExcReason,
				       erlang:get_stacktrace()})
    end.

-spec create_failed_result(ctx(), fail_reason()) ->
	  {'failed', counterexample(), fail_actions()}.
create_failed_result(#ctx{bound = Bound, fail_actions = Actions}, Reason) ->
    CExm = #cexm{fail_reason = Reason, bound = lists:reverse(Bound),
		 size = get_size(), gen_state = proper_gen:gen_state_get()},
    {failed, CExm, lists:reverse(Actions)}.

-spec child(pid(), delayed_test(), ctx()) -> 'ok'.
child(Father, Prop, Ctx) ->
    Result = run({'$apply',[],Prop}, Ctx),
    Father ! {result, Result},
    ok.

-spec clear_mailbox() -> 'ok'.
clear_mailbox() ->
    receive
	_ -> clear_mailbox()
    after 0 ->
	ok
    end.

-spec clean_testcase(imm_testcase()) -> clean_testcase().
clean_testcase(ImmTestCase) ->
    [proper_gen:clean_instance(I) || I <- ImmTestCase].


%%------------------------------------------------------------------------------
%% Shrinking callback functions
%%------------------------------------------------------------------------------

-spec still_fails(imm_testcase(), test(), fail_reason()) -> boolean().
still_fails(ImmTestCase, Test, OldReason) ->
    Ctx = #ctx{try_shrunk = true, to_try = ImmTestCase},
    case run(Test, Ctx) of
	%% We check that it's the same fault that caused the crash.
	{failed, #cexm{fail_reason = NewReason}, _Actions} ->
	    same_fail_reason(OldReason, NewReason);
	_ ->
	    false
    end.

%% We don't consider two exceptions different if they have a different
%% stacktrace.
-spec same_fail_reason(fail_reason(), fail_reason()) -> boolean().
same_fail_reason({exception,_SameExcKind,_SameExcReason,_StackTrace1},
		 {exception,_SameExcKind,_SameExcReason,_StackTrace2}) ->
    true;
same_fail_reason(_SameReason, _SameReason) ->
    true;
same_fail_reason(_, _) ->
    false.

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
skip_to_next({'$sample',_Sample,Prop}) ->
    skip_to_next(Prop);
skip_to_next({'$whenfail',_Action,Prop}) ->
    skip_to_next({'$apply',[],Prop});
skip_to_next({'$trapexit',Prop}) ->
    skip_to_next({'$apply',[],Prop});
skip_to_next({'$timeout',_Limit,_Prop}) ->
    false; % This is OK, since timeout cannot contain any ?FORALLs.
skip_to_next({'$apply',Args,Prop}) ->
    try
	skip_to_next(apply(Prop, Args))
    catch
	%% Should be OK to catch everything here, since we have already tested
	%% at this point that the test still fails.
	_ExcKind:_ExcReason -> false
    end.


%%------------------------------------------------------------------------------
%% Output functions
%%------------------------------------------------------------------------------

-spec report_imm_result(imm_result(), opts()) -> 'ok'.
report_imm_result({passed,Passed,Samples},
		  #opts{expect_fail = ExpectF, output_fun = Print}) ->
    case ExpectF of
	true  -> Print("~nError: no test failed~n", []);
	false -> Print("~nOK, passed ~b tests~n", [Passed])
    end,
    lists:foreach(fun(S) -> print_percentages(S,Passed,Print) end, Samples),
    ok;
report_imm_result({failed,Performed,_CExm,_Actions},
		  #opts{expect_fail = true, output_fun = Print}) ->
    Print("~nOK, failed as expected, after ~b tests.~n", [Performed]);
report_imm_result({failed,Performed,#cexm{fail_reason = Reason, bound = Bound},
		   Actions},
		  #opts{expect_fail = false, output_fun = Print}) ->
    Print("~nFailed, after ~b tests.~n", [Performed]),
    case Reason of
	false_prop ->
	    ok;
	timeout ->
	    Print("Reason: timeout.~n", []);
	{exception,ExcKind,ExcReason,_StackTrace} ->
	    %% TODO: print stacktrace too?
	    Print("Reason: ~w:~w.~n", [ExcKind,ExcReason])
    end,
    print_bound(Bound, Print),
    execute_actions(Actions),
    ok;
report_imm_result({error,Reason}, #opts{output_fun = Print}) ->
    case Reason of
	cant_generate ->
	    Print("~nError: couldn't produce an instance that satisfies all "
		  "strict constraints after ~b tries~n",
		  [get('$constraint_tries')]);
	cant_satisfy ->
	    Print("~nError: no valid test could be generated.~n", []);
	type_mismatch ->
	    Print("~nError: the variables' and types' structures inside a "
		  "?FORALL don't match.~n", []);
	{unexpected,Unexpected} ->
	    Print("~nInternal error: the last run returned an unexpected result"
		  ":~n~w~nPlease notify the maintainers about this error~n",
		  [Unexpected])
    end,
    ok.

-spec print_percentages(sample(), pos_integer(), output_fun()) -> 'ok'.
print_percentages([], _Passed, _Print) ->
    ok;
print_percentages(Sample, Passed, Print) ->
    FreqDict = lists:foldl(fun add_one_to_freq/2, dict:new(), Sample),
    Freqs = dict:to_list(FreqDict),
    SortedFreqs = lists:reverse(lists:keysort(2, Freqs)),
    Print("~n", []),
    lists:foreach(fun({X,F}) -> Print("~b\% ~w~n", [100 * F div Passed,X]) end,
		  SortedFreqs),
    ok.

-spec add_one_to_freq(term(), dict()) -> dict().
add_one_to_freq(X, Dict) ->
    case dict:find(X, Dict) of
	{ok,Freq} -> dict:store(X, Freq + 1, Dict);
	error     -> dict:store(X, 1, Dict)
    end.

-spec print_bound(imm_testcase(), output_fun()) -> 'ok'.
print_bound(ImmInstances, Print) ->
    Instances = clean_testcase(ImmInstances),
    lists:foreach(fun(I) -> Print("~w~n", [I]) end, Instances),
    ok.

-spec execute_actions(fail_actions()) -> 'ok'.
execute_actions(Actions) ->
    lists:foreach(fun(A) -> ?FORCE(A) end, Actions),
    ok.

-spec report_shrinking(non_neg_integer(), imm_testcase(), fail_actions(),
		       output_fun()) -> 'ok'.
report_shrinking(Shrinks, MinImmTestCase, MinActions, Print) ->
    Print("(~b times)~n", [Shrinks]),
    print_bound(MinImmTestCase, Print),
    execute_actions(MinActions),
    ok.
