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
-export([check/1, check/2, retest/2, retest/3]).
-export([numtests/2, fails/1, on_output/2]).
-export([collect/2, collect/3, aggregate/2, aggregate/3, measure/3,
	 with_title/1, equals/2]).
-export([global_state_erase/0, get_counterexample/0, clean_garbage/0,
	 get_fail_reason/1, get_bound/1]).

-export([get_size/1, global_state_init_size/1, report_error/2]).
-export([forall/2, implies/2, whenfail/2, trapexit/1, timeout/2]).
-export([still_fails/4, force_skip/2]).

-export_type([test/0, outer_test/0, counterexample/0]).
-export_type([imm_testcase/0, stripped_test/0, fail_reason/0, output_fun/0]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------

-define(MISMATCH_MSG, "Error: the input doesn't correspond to this property: ").


%%------------------------------------------------------------------------------
%% Test types
%%------------------------------------------------------------------------------

%% @private_type imm_testcase
-type imm_testcase() :: [proper_gen:imm_instance()].
-type clean_testcase() :: [proper_gen:instance()].
-type sample() :: [term()].
-type freq_sample() :: [{term(),frequency()}].
-type side_effects_fun() :: fun(() -> 'ok').
-type fail_actions() :: [side_effects_fun()].
-type output_fun() :: fun((string(), [term()]) -> 'ok').
-type title() :: atom() | string().
-type stats_printer() :: fun((sample()) -> 'ok')
		       | fun((sample(), pos_integer()) -> 'ok')
		       | fun((sample(), pos_integer(), output_fun()) -> 'ok').
-type numeric_stat() :: number() | 'undefined'.
-type numeric_stats() :: {numeric_stat(),numeric_stat(),numeric_stat()}.
-type time_period() :: non_neg_integer().

-type outer_test() :: test() %% TODO: This should be opaque.
		    | numtests_clause()
		    | fails_clause()
		    | on_output_clause().
-type test() :: boolean() %% TODO: This should be opaque.
	      | forall_clause()
	      | implies_clause()
	      | sample_clause()
	      | whenfail_clause()
	      | trapexit_clause()
	      | timeout_clause().
	      %%| always_clause()
	      %%| sometimes_clause()
-type delayed_test() :: fun(() -> test()).
-type dependent_test() :: fun((proper_gen:instance()) -> test()).
-type lazy_test() :: delayed_test() | dependent_test().
%% @private_type stripped_test
-type stripped_test() :: 'false' | 'error' | stripped_forall().
-type stripped_forall()	:: {proper_types:type(), dependent_test()}.

-type numtests_clause() :: {'numtests', pos_integer(), outer_test()}.
-type fails_clause() :: {'fails', outer_test()}.
-type on_output_clause() :: {'on_output', output_fun(), outer_test()}.

-type forall_clause() :: {'forall', proper_types:raw_type(), dependent_test()}.
-type implies_clause() :: {'implies', boolean(), delayed_test()}.
-type sample_clause() :: {'sample', sample(), stats_printer(), test()}.
-type whenfail_clause() :: {'whenfail', side_effects_fun(), delayed_test()}.
-type trapexit_clause() :: {'trapexit', delayed_test()}.
-type timeout_clause() :: {timeout, time_period(), delayed_test()}.
%%-type always_clause() :: {'always', pos_integer(), delayed_test()}.
%%-type sometimes_clause() :: {'sometimes', pos_integer(), delayed_test()}.


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
-record(ctx, {catch_exits  = false :: boolean(),
	      try_shrunk   = false :: boolean(),
	      bound        = []    :: imm_testcase(),
	      to_try       = []    :: imm_testcase(),
	      fail_actions = []    :: fail_actions(),
	      samples      = []    :: [sample()],
	      printers     = []    :: [stats_printer()]}).
-type ctx() :: #ctx{}.


%%------------------------------------------------------------------------------
%% Result types
%%------------------------------------------------------------------------------

-type single_run_result() :: {'passed', pass_reason(), [sample()],
			      [stats_printer()]}
			   | {'failed', counterexample(), fail_actions()}
			   | {'error', single_run_error_reason()}.
-type pass_reason() :: 'true_prop' | 'didnt_crash'.
-type fail_reason() :: 'false_prop' | 'time_out'
		     | {'exception',exc_kind(),exc_reason(),stacktrace()}.
-type exc_kind() :: 'throw' | 'error' | 'exit'.
-type exc_reason() :: term().
-type stacktrace() :: [{atom(),atom(),arity() | [term()]}].
-type common_error_reason() :: 'cant_generate' | 'type_mismatch'
			     | {'typeserver',term()}.
-type single_run_error_reason() :: common_error_reason() | 'wrong_type'
				 | 'rejected' | 'too_many_instances'.

-type rerun_result() :: boolean() | {'error', rerun_error_reason()}.
-type rerun_error_reason() :: single_run_error_reason() | 'too_few_instances'.

-type error_result() :: {'error', error_reason()}.
-type error_reason() :: common_error_reason() | 'cant_satisfy'
		      | {'unexpected', single_run_result()}.
-type imm_result() :: error_result()
		    | {'passed', pos_integer(), [sample()], [stats_printer()]}
		    | {'failed',pos_integer(),counterexample(),fail_actions()}.
-type long_result() :: error_result()
		     | {'passed', pos_integer(), [sample()]}
		     | {'failed', pos_integer(), counterexample()}
		     | {'failed', pos_integer(), counterexample(),
			 non_neg_integer(), counterexample()}.
-type short_result() :: boolean() | error_result().

-record(cexm, {fail_reason :: fail_reason(),
	       bound       :: imm_testcase(),
	       size        :: size(),
	       gen_state   :: proper_gen:gen_state()}).
-opaque counterexample() :: #cexm{}.


%%------------------------------------------------------------------------------
%% State handling functions
%%------------------------------------------------------------------------------

%% @private
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
	Size ->
	    case proper_types:find_prop(size_transform, Type) of
		{ok,Transform} -> Transform(Size);
		error          -> Size
	    end
    end.

%% @private
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
    proper_typeserver:start(),
    ok.

-spec global_state_restore(counterexample(), opts()) -> 'ok'.
global_state_restore(#cexm{size = Size, gen_state = GenState}, Opts) ->
    global_state_init(Opts),
    put('$size', Size),
    proper_gen:gen_state_set(GenState),
    ok.

-spec global_state_erase() -> 'ok'.
global_state_erase() ->
    proper_gen:gen_state_erase(),
    proper_typeserver:stop(),
    proper_arith:rand_stop(),
    erase('$constraint_tries'),
    erase('$size'),
    ok.

-spec save_counterexample(counterexample()) -> 'ok'.
save_counterexample(CExm) ->
    put('$counterexample', CExm),
    ok.

-spec get_counterexample() -> {'ok',counterexample()} | 'error'.
get_counterexample() ->
    case get('$counterexample') of
	undefined -> error;
	CExm      -> {ok, CExm}
    end.

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
%% Outwards interface functions
%%------------------------------------------------------------------------------

-spec check(outer_test()) -> long_result() | short_result().
check(OuterTest) ->
    check(OuterTest, []).

-spec check(outer_test(), [user_opt()] | user_opt()) ->
	  long_result() | short_result().
check(OuterTest, UserOpts) ->
    ImmOpts = parse_opts(UserOpts),
    {Test,Opts} = peel_test(OuterTest, ImmOpts),
    test(Test, Opts).

-spec retest(outer_test(), counterexample()) -> rerun_result().
retest(OuterTest, CExm) ->
    retest(OuterTest, CExm, []).

-spec retest(outer_test(), counterexample(), [user_opt()] | user_opt()) ->
	  rerun_result().
retest(OuterTest, CExm, UserOpts) ->
    ImmOpts = parse_opts(UserOpts),
    {Test,Opts} = peel_test(OuterTest, ImmOpts),
    retry(Test, CExm, Opts).

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

-spec peel_test(outer_test(), opts()) -> {test(),opts()}.
peel_test({numtests,N,OuterTest}, Opts) ->
    peel_test(OuterTest, Opts#opts{numtests = N});
peel_test({fails,OuterTest}, Opts) ->
    peel_test(OuterTest, Opts#opts{expect_fail = true});
peel_test({on_output,Print,OuterTest}, Opts) ->
    peel_test(OuterTest, Opts#opts{output_fun = Print});
peel_test(Test, Opts) ->
    {Test, Opts}.


%%------------------------------------------------------------------------------
%% Test declaration functions
%%------------------------------------------------------------------------------

%% TODO: All of these should have a test() or outer_test() return type.
-spec numtests(pos_integer(), outer_test()) -> numtests_clause().
numtests(N, Test) ->
    {numtests, N, Test}.

-spec fails(outer_test()) -> fails_clause().
fails(Test) ->
    {fails, Test}.

-spec on_output(output_fun(), outer_test()) -> on_output_clause().
on_output(Print, Test) ->
    {on_output, Print, Test}.

%% @private
-spec forall(proper_types:raw_type(), dependent_test()) -> forall_clause().
forall(RawType, DTest) ->
    {forall, RawType, DTest}.

%% @private
-spec implies(boolean(), delayed_test()) -> implies_clause().
implies(Pre, DTest) ->
    {implies, Pre, DTest}.

-spec collect(term(), test()) -> sample_clause().
collect(Term, Test) ->
    collect(with_title(""), Term, Test).

-spec collect(stats_printer(), term(), test()) -> sample_clause().
collect(Printer, Term, Test) ->
    aggregate(Printer, [Term], Test).

-spec aggregate(sample(), test()) -> sample_clause().
aggregate(Sample, Test) ->
    aggregate(with_title(""), Sample, Test).

-spec aggregate(stats_printer(), sample(), test()) -> sample_clause().
aggregate(Printer, Sample, Test) ->
    {sample, Sample, Printer, Test}.

-spec measure(title(), number() | [number()], test()) -> sample_clause().
measure(Title, Sample, Test) when is_number(Sample) ->
    measure(Title, [Sample], Test);
measure(Title, Sample, Test) when is_list(Sample) ->
    aggregate(numeric_with_title(Title), Sample, Test).

%% @private
-spec whenfail(side_effects_fun(), delayed_test()) -> whenfail_clause().
whenfail(Action, DTest) ->
    {whenfail, Action, DTest}.

%% @private
-spec trapexit(delayed_test()) -> trapexit_clause().
trapexit(DTest) ->
    {trapexit, DTest}.

%% @private
-spec timeout(time_period(), delayed_test()) -> timeout_clause().
timeout(Limit, DTest) ->
    {timeout, Limit, DTest}.

-spec equals(term(), term()) -> whenfail_clause().
equals(A, B) ->
    ?WHENFAIL(io:format("~w =/= ~w~n",[A,B]), A =:= B).


%%------------------------------------------------------------------------------
%% Main testing functions
%%------------------------------------------------------------------------------

-spec test(test(), opts()) -> long_result() | short_result().
test(Test, #opts{numtests = NumTests, output_fun = Print,
		 long_result = ReturnLong} = Opts) ->
    global_state_init(Opts),
    ImmResult = perform(NumTests, Test, Print),
    Print("~n", []),
    report_imm_result(ImmResult, Opts),
    ShortResult = get_short_result(ImmResult, Opts),
    LongResult = get_long_result(ImmResult, Test, Opts),
    global_state_erase(),
    case ReturnLong of
	true  -> LongResult;
	false -> ShortResult
    end.

-spec retry(test(), counterexample(), opts()) -> rerun_result().
retry(Test, #cexm{bound = ImmTestCase} = CExm, Opts) ->
    global_state_restore(CExm, Opts),
    SingleRunResult = rerun(Test, ImmTestCase),
    RerunResult = get_rerun_result(SingleRunResult, CExm, Opts),
    global_state_erase(),
    RerunResult.

-spec get_short_result(imm_result(), opts()) -> short_result().
get_short_result({passed,_Passed,_Samples,_Printers}, Opts) ->
    not Opts#opts.expect_fail;
get_short_result({failed,_Performed,_CExm,_Actions}, Opts) ->
    Opts#opts.expect_fail;
get_short_result({error,_Reason} = ErrorResult, _Opts) ->
    ErrorResult.

-spec get_long_result(imm_result(), test(), opts()) -> long_result().
get_long_result({passed,Passed,Samples,_Printers}, _Test, _Opts) ->
    {passed, Passed, Samples};
get_long_result({failed,Performed,CExm,_Actions}, Test,
		 #opts{expect_fail = false, noshrink = false,
		       max_shrinks = MaxShrinks, output_fun = Print}) ->
    Print("Shrinking", []),
    #cexm{fail_reason = Reason, bound = ImmTestCase} = CExm,
    StrTest = skip_to_next(Test),
    {Shrinks, MinImmTestCase} =
	proper_shrink:shrink(ImmTestCase, StrTest, Reason, MaxShrinks, Print),
    {failed, MinCExm, MinActions} = rerun(Test, MinImmTestCase),
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
    perform(0, NumTests, ?MAX_TRIES_FACTOR * NumTests, Test, none, none, Print).

-spec perform(non_neg_integer(), non_neg_integer(), non_neg_integer(), test(),
	      [sample()] | 'none', [stats_printer()] | 'none', output_fun()) ->
	  imm_result().
perform(Passed, _ToPass, 0, _Test, Samples, Printers, _Print) ->
    case Passed of
	0 -> {error, cant_satisfy};
	_ -> {passed, Passed, Samples, Printers}
    end;
perform(ToPass, ToPass, _TriesLeft, _Test, Samples, Printers, _Print) ->
    {passed, ToPass, Samples, Printers};
perform(Passed, ToPass, TriesLeft, Test, Samples, Printers, Print) ->
    proper_gen:gen_state_erase(),
    case run(Test) of
	{passed, true_prop, MoreSamples, MorePrinters} ->
	    Print(".", []),
	    NewSamples = add_samples(MoreSamples, Samples),
	    NewPrinters = case Printers of
			      none -> MorePrinters;
			      _    -> Printers
			  end,
	    grow_size(),
	    perform(Passed + 1, ToPass, TriesLeft - 1, Test,
		    NewSamples, NewPrinters, Print);
	{failed, CExm, Actions} ->
	    Print("!", []),
	    {failed, Passed + 1, CExm, Actions};
	{error, cant_generate} = Error ->
	    Error;
	{error, type_mismatch} = Error ->
	    Error;
	{error, {typeserver,_SubReason}} = Error ->
	    Error;
	{error, rejected} ->
	    Print("x", []),
	    grow_size(),
	    perform(Passed, ToPass, TriesLeft - 1, Test,
		    Samples, Printers, Print);
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

-spec rerun(test(), imm_testcase()) -> single_run_result().
rerun(Test, ImmTestCase) ->
    Ctx = #ctx{try_shrunk = true, to_try = ImmTestCase},
    run(Test, Ctx).

-spec run(test(), ctx()) -> single_run_result().
run(true, #ctx{to_try = [], samples = Samples, printers = Printers}) ->
    {passed, true_prop, lists:reverse(Samples), lists:reverse(Printers)};
run(true, _Ctx) ->
    {error, too_many_instances};
run(false, #ctx{to_try = []} = Ctx) ->
    create_failed_result(Ctx, false_prop);
run(false, _Ctx) ->
    {error, too_many_instances};
run({forall,RawType,Prop},
    #ctx{try_shrunk = TryShrunk, to_try = ToTry, bound = Bound} = Ctx) ->
    case {TryShrunk, ToTry} of
	{true, []} ->
	    {passed, didnt_crash, [], []};
	{true, [ImmInstance | Rest]} ->
	    case proper_types:safe_is_instance(ImmInstance, RawType) of
		true ->
		    Instance = proper_gen:clean_instance(ImmInstance),
		    NewCtx = Ctx#ctx{to_try = Rest,
				     bound = [ImmInstance | Bound]},
		    force(Instance, Prop, NewCtx);
		false ->
		    %% TODO: could try to fix the instances here
		    {error, wrong_type};
		{error,_Reason} = Error ->
		    Error
	    end;
	{false, []} ->
	    case proper_gen:safe_generate(RawType) of
		{ok,ImmInstance} ->
		    Instance = proper_gen:clean_instance(ImmInstance),
		    NewCtx = Ctx#ctx{bound = [ImmInstance | Bound]},
		    force(Instance, Prop, NewCtx);
		{error,_Reason} = Error ->
		    Error
	    end
    end;
run({implies,true,Prop}, Ctx) ->
    force(Prop, Ctx);
run({implies,false,_Prop}, _Ctx) ->
    {error, rejected};
run({sample,NewSample,NewPrinter,Prop},
    #ctx{samples = Samples, printers = Printers} = Ctx) ->
    NewCtx = Ctx#ctx{samples = [NewSample | Samples],
		     printers = [NewPrinter | Printers]},
    run(Prop, NewCtx);
run({whenfail,NewAction,Prop}, #ctx{fail_actions = Actions} = Ctx)->
    NewCtx = Ctx#ctx{fail_actions = [NewAction | Actions]},
    force(Prop, NewCtx);
run({trapexit,Prop}, Ctx) ->
    NewCtx = Ctx#ctx{catch_exits = true},
    force(Prop, NewCtx);
run({timeout,Limit,Prop}, Ctx) ->
    Self = self(),
    Child = spawn_link(fun() -> child(Self,Prop,Ctx) end),
    receive
	{result, Result} -> Result
    after Limit ->
	unlink(Child),
	exit(Child, kill),
	clear_mailbox(),
	create_failed_result(Ctx, time_out)
    end.

-spec force(delayed_test(), ctx()) -> single_run_result().
force(Prop, Ctx) ->
    apply_args([], Prop, Ctx).

-spec force(proper_gen:instance(), dependent_test(), ctx()) ->
	  single_run_result().
force(Arg, Prop, Ctx) ->
    apply_args([proper_symb:internal_eval(Arg)], Prop, Ctx).

-spec apply_args([proper_gen:instance()], lazy_test(), ctx()) ->
	  single_run_result().
apply_args(Args, Prop, Ctx) ->
    try apply(Prop, Args) of
	%% TODO: should we care if the code returns true when trapping exits?
	%%       If we are doing that, we are probably testing code that will
	%%       run as a separate process against crashes.
	InnerProp ->
	    run(InnerProp, Ctx)
    catch
	%% TODO: remove our functions from the stacktrace
	error:ErrReason ->
	    Trace = erlang:get_stacktrace(),
	    case ErrReason =:= function_clause
		 andalso threw_exception(Prop, Trace) of
		true  -> {error, type_mismatch};
		false -> create_failed_result(Ctx, {exception,error,ErrReason,
						    Trace})
	    end;
	throw:'$cant_generate' ->
	    {error, cant_generate};
	%% TODO: Do we need to guard against typeserver exceptions too?
	throw:ExcReason ->
	    create_failed_result(Ctx, {exception,throw,ExcReason,
				       erlang:get_stacktrace()});
	exit:ExitReason when Ctx#ctx.catch_exits ->
	    create_failed_result(Ctx, {exception,exit,ExitReason,
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
    Result = force(Prop, Ctx),
    Father ! {result, Result},
    ok.

-spec clear_mailbox() -> 'ok'.
clear_mailbox() ->
    receive
	_ -> clear_mailbox()
    after 0 ->
	ok
    end.

-spec threw_exception(function(), stacktrace()) -> boolean().
threw_exception(Fun, [{TopMod,TopName,TopArgs} | _Rest]) ->
    {module,FunMod} = erlang:fun_info(Fun, module),
    {name,FunName} = erlang:fun_info(Fun, name),
    {arity,FunArity} = erlang:fun_info(Fun, arity),
    TopArity =
	if
	    is_integer(TopArgs) -> TopArgs;
	    is_list(TopArgs)    -> length(TopArgs)
	end,
    FunMod =:= TopMod andalso FunName =:= TopName andalso FunArity =:= TopArity.

-spec clean_testcase(imm_testcase()) -> clean_testcase().
clean_testcase(ImmTestCase) ->
    [proper_gen:clean_instance(I) || I <- ImmTestCase].


%%------------------------------------------------------------------------------
%% Shrinking callback functions
%%------------------------------------------------------------------------------

%% @private
-spec still_fails(proper_gen:imm_instance(), imm_testcase(), dependent_test(),
		  fail_reason()) -> boolean().
still_fails(ImmInstance, TestTail, Prop, OldReason) ->
    Instance = proper_gen:clean_instance(ImmInstance),
    Ctx = #ctx{try_shrunk = true, to_try = TestTail},
    case force(Instance, Prop, Ctx) of
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

-spec skip_to_next(test()) -> stripped_test().
skip_to_next(true) ->
    error;
skip_to_next(false) ->
    false;
skip_to_next({forall,RawType,Prop}) ->
    Type = proper_types:cook_outer(RawType),
    {Type,Prop};
skip_to_next({implies,true,Prop}) ->
    force_skip(Prop);
skip_to_next({implies,false,_Prop}) ->
    error;
skip_to_next({sample,_Sample,_Printer,Prop}) ->
    skip_to_next(Prop);
skip_to_next({whenfail,_Action,Prop}) ->
    force_skip(Prop);
skip_to_next({trapexit,Prop}) ->
    force_skip(Prop);
skip_to_next({timeout,_Limit,_Prop}) ->
    false. % This is OK, since timeout cannot contain any ?FORALLs.

-spec force_skip(delayed_test()) -> stripped_test().
force_skip(Prop) ->
    apply_skip([], Prop).

%% @private
-spec force_skip(proper_gen:instance(), dependent_test()) -> stripped_test().
force_skip(Arg, Prop) ->
    apply_skip([proper_symb:internal_eval(Arg)], Prop).

-spec apply_skip([proper_gen:instance()], lazy_test()) -> stripped_test().
apply_skip(Args, Prop) ->
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
report_imm_result({passed,Passed,Samples,Printers},
		  #opts{expect_fail = ExpectF, output_fun = Print}) ->
    case ExpectF of
	true  -> Print("Error: no test failed~n", []);
	false -> Print("OK, passed ~b tests~n", [Passed])
    end,
    SortedSamples = [lists:sort(Sample) || Sample <- Samples],
    lists:foreach(fun({P,S}) -> apply_stats_printer(P, S, Passed, Print) end,
		  lists:zip(Printers, SortedSamples)),
    ok;
report_imm_result({failed,Performed,_CExm,_Actions},
		  #opts{expect_fail = true, output_fun = Print}) ->
    Print("OK, failed as expected, after ~b tests.~n", [Performed]),
    ok;
report_imm_result({failed,Performed,#cexm{fail_reason = Reason, bound = Bound},
		   Actions},
		  #opts{expect_fail = false, output_fun = Print}) ->
    Print("Failed, after ~b tests.~n", [Performed]),
    report_fail_reason(Reason, Print, false),
    print_bound(Bound, Print),
    execute_actions(Actions),
    ok;
report_imm_result({error,Reason}, #opts{output_fun = Print}) ->
    report_error(Reason, Print),
    ok.

-spec get_rerun_result(single_run_result(), counterexample(), opts()) ->
	  rerun_result().
get_rerun_result({passed,true_prop,_Samples,_Printers}, _CExm,
		 #opts{expect_fail = ExpectF, output_fun = Print}) ->
    Print("The input passed the test.~n", []),
    not ExpectF;
get_rerun_result({passed,didnt_crash,_Samples,_Printers},
		 #cexm{fail_reason = Reason},
		 #opts{expect_fail = ExpectF, output_fun = Print}) ->
    case Reason of
	{exception,_ExcKind,_ExcReason,_StackTrace} ->
	    Print("The input didn't raise an early exception.~n", []),
	    not ExpectF;
	_ ->
	    report_error(too_few_instances, Print),
	    {error, too_few_instances}
    end;
get_rerun_result({failed,#cexm{fail_reason = NewReason},Actions},
		 #cexm{fail_reason = OldReason},
		 #opts{expect_fail = ExpectF, output_fun = Print}) ->
    Print("The input still fails the test", []),
    case same_fail_reason(OldReason, NewReason) of
	true  -> Print(".~n", []);
	false -> Print(", but for a different reason:~n", []),
		 report_fail_reason(NewReason, Print, true)
    end,
    execute_actions(Actions),
    ExpectF;
get_rerun_result({error,Reason} = Error, _CExm, #opts{output_fun = Print}) ->
    report_error(Reason, Print),
    Error.

%% @private
-spec report_error(rerun_error_reason() | error_reason(), output_fun()) -> 'ok'.
report_error(cant_generate, Print) ->
    Print("Error: couldn't produce an instance that satisfies all strict "
	  "constraints after ~b tries.~n", [get('$constraint_tries')]);
report_error(type_mismatch, Print) ->
    Print("Error: the variables' and types' structures inside a ?FORALL don't "
	  "match.~n", []);
report_error({typeserver,SubReason}, Print) ->
    Print("Error: couldn't translate a native type.~nThe typeserver "
	  "responded: ~w~n", [SubReason]);
report_error(wrong_type, Print) ->
    Print(?MISMATCH_MSG ++ "the instances don't match the types.~n", []);
report_error(rejected, Print) ->
    Print(?MISMATCH_MSG ++ "it failed an ?IMPLIES check.~n", []);
report_error(too_many_instances, Print) ->
    Print(?MISMATCH_MSG ++ "it's too long.~n", []); %% that's what she said
report_error(too_few_instances, Print) ->
    Print(?MISMATCH_MSG ++ "it's too short.~n", []);
report_error(cant_satisfy, Print) ->
    Print("Error: no valid test could be generated.~n", []);
report_error({unexpected,Unexpected}, Print) ->
    Print("Internal error: the last run returned an unexpected result:~n~w~n"
	  "Please notify the maintainers about this error~n", [Unexpected]).

-spec report_fail_reason(fail_reason(), output_fun(), boolean()) -> 'ok'.
report_fail_reason(false_prop, _Print, false) ->
    ok;
report_fail_reason(false_prop, Print, true) ->
    Print("The property was falsified.~n", []);
report_fail_reason(time_out, Print, _ReportFalseProp) ->
    Print("Test execution timed out.~n", []);
report_fail_reason({exception,ExcKind,ExcReason,_StackTrace}, Print,
		   _ReportFalseProp) ->
    %% TODO: print stacktrace too?
    Print("An exception was raised: ~w:~w.~n", [ExcKind,ExcReason]).

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


%%------------------------------------------------------------------------------
%% Stats printing functions
%%------------------------------------------------------------------------------

-spec apply_stats_printer(stats_printer(), sample(), pos_integer(),
			  output_fun()) -> 'ok'.
apply_stats_printer(Printer, SortedSample, Passed, Print) ->
    {arity,Arity} = erlang:fun_info(Printer, arity),
    case Arity of
	1 -> Printer(SortedSample);
	2 -> Printer(SortedSample, Passed);
	3 -> Printer(SortedSample, Passed, Print)
    end.

-spec with_title(title()) -> stats_printer().
with_title(Title) ->
    fun(S,P,O) -> plain_stats_printer(S, P, O, Title) end.

-spec plain_stats_printer(sample(),pos_integer(),output_fun(),title()) -> 'ok'.
plain_stats_printer(SortedSample, Passed, Print, Title) ->
    print_title(Title, Print),
    FreqSample = process_sorted_sample(SortedSample),
    lists:foreach(fun({X,F}) -> Print("~b\% ~w~n", [100 * F div Passed,X]) end,
		  FreqSample).

-spec print_title(title(), output_fun()) -> 'ok'.
print_title(RawTitle, Print) ->
    Print("~n", []),
    Title = if
                is_atom(RawTitle) -> atom_to_list(RawTitle);
                is_list(RawTitle) -> RawTitle
	    end,
    case Title of
	"" -> ok;
	_  -> Print(Title ++ "~n", [])
    end.

-spec process_sorted_sample(sample()) -> freq_sample().
process_sorted_sample(SortedSample) ->
    Freqs = get_freqs(SortedSample, []),
    lists:reverse(lists:keysort(2, Freqs)).

-spec get_freqs(sample(), freq_sample()) -> freq_sample().
get_freqs([], Freqs) ->
    Freqs;
get_freqs([Term | Rest], Freqs) ->
    {Freq,Others} = remove_all(Term, 1, Rest),
    get_freqs(Others, [{Term,Freq} | Freqs]).

-spec remove_all(term(), frequency(), sample()) -> {frequency(), sample()}.
remove_all(X, Freq, [X | Rest]) ->
    remove_all(X, Freq + 1, Rest);
remove_all(_X, Freq, Sample) ->
    {Freq, Sample}.

-spec numeric_with_title(title()) -> stats_printer().
numeric_with_title(Title) ->
    fun(S,P,O) -> num_stats_printer(S, P, O, Title) end.

-spec num_stats_printer([number()],pos_integer(),output_fun(),title()) -> 'ok'.
num_stats_printer(SortedSample, _Passed, Print, Title) ->
    print_title(Title, Print),
    {Min,Avg,Max} = get_numeric_stats(SortedSample),
    Print("minimum: ~w~naverage: ~w~nmaximum: ~w~n", [Min,Avg,Max]).

-spec get_numeric_stats([number()]) -> numeric_stats().
get_numeric_stats([]) ->
    {undefined, undefined, undefined};
get_numeric_stats([Min | _Rest] = SortedSample) ->
    {Avg,Max} = avg_and_last(SortedSample, 0, 0),
    {Min, Avg, Max}.

-spec avg_and_last([number(),...], number(), non_neg_integer()) ->
	  {number(),number()}.
avg_and_last([Last], Sum, Len) ->
    {(Sum + Last) / (Len + 1), Last};
avg_and_last([X | Rest], Sum, Len) ->
    avg_and_last(Rest, Sum + X, Len + 1).
