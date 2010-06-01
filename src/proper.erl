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

%% @author Manolis Papadakis <manopapad@gmail.com>
%% @copyright 2010 Manolis Papadakis
%% @version {@version}
%% @doc This is the main PropEr module.

-module(proper).
-export([numtests/2, collect/2, fails/1]).
-export([set_size/1, delete_size/0, grow_size/0, get_size/1, parse_opts/1,
	 global_state_init/1, global_state_erase/1]).
-export([check/1, check/2, still_fails/3, skip_to_next/1]).
-export([print/3]).

-include("proper_internal.hrl").


%% Common functions

numtests(N, Test) -> {'$numtests',N,Test}.

collect(Category, Prop) -> {'$collect',Category,Prop}.

fails(Test) -> {'$fails',Test}.

set_size(Size) ->
    put('$size', Size),
    ok.

delete_size() ->
    erase('$size'),
    ok.

get_size() ->
    get('$size').

grow_size() ->
    Size = get_size(),
    set_size(Size + 1),
    ok.

get_size(Type) ->
    Size1 = get_size(),
    Size2 = case proper_types:find_prop(size_transform, Type) of
		{ok, Transform} -> Transform(Size1);
		error           -> Size1
	    end,
    % TODO: should the size be normalized (streched or pressed)?
    case proper_types:find_prop(size_limit, Type) of
	{ok, Limit} -> erlang:min(Size2, Limit);
	error       -> Size2
    end.

parse_opts(OptsList) ->
    parse_opts_tr(OptsList, #opts{}).

parse_opts_tr([], Opts) ->
    Opts;
parse_opts_tr([Opt | Rest], Opts) ->
    NewOpts =
	case Opt of
	    quiet           -> Opts#opts{quiet = true};
	    {numtests,N}    -> Opts#opts{numtests = N};
	    expect_fail     -> Opts#opts{expect_fail = true};
	    {max_shrinks,N} -> Opts#opts{max_shrinks = N}
	end,
    parse_opts_tr(Rest, NewOpts).


%% Main usage functions

check(Test) ->
    check(Test, #opts{}).

check({'$numtests',N,Test}, Opts = #opts{}) ->
    check(Test, Opts#opts{numtests = N});
% We only allow a 'fails' to be an external wrapper, since the property
% wrapped by a 'fails' is not delayed, and thus a failure-inducing exception
% will cause the test to fail before the 'fails' is processed.
check({'$fails',Test}, Opts = #opts{}) ->
    check(Test, Opts#opts{expect_fail = true});
check(Test, Opts = #opts{}) ->
    global_state_init(Opts),
    NumTests = Opts#opts.numtests,
    Result = perform_tr(0, NumTests, Test, none, Opts),
    report_results(Result, Opts),
    FinalResult =
	case Result of
	    {failed, Performed, Reason, ImmFailedTestCase} ->
		FailedTestCase = proper_gen:clean_instance(ImmFailedTestCase),
		{Shrinks, ImmMinTestCase} =
		    proper_shrink:shrink(ImmFailedTestCase, Test, Reason, Opts),
		MinTestCase = proper_gen:clean_instance(ImmMinTestCase),
		report_shrinking(Shrinks, MinTestCase, Opts),
		{failed,Performed,Reason,FailedTestCase,Shrinks,MinTestCase};
	    _ ->
		Result
	end,
    global_state_erase(Opts),
    case Opts#opts.quiet of
	true  -> FinalResult;
	false -> ok
    end;
check(Test, OptsList) ->
    check(Test, parse_opts(OptsList)).

global_state_init(Opts) ->
    proper_arith:rand_start(Opts),
    set_size(-1),
    ok.

global_state_erase(_Opts) ->
    delete_size(),
    proper_arith:rand_stop(),
    ok.

perform_tr(0, 0, _Test, _CatDicts, _Opts) ->
    {error, cant_satisfy};
perform_tr(Performed, 0, _Test, CatDicts, _Opts) ->
    {passed, Performed, CatDicts};
perform_tr(Performed, Left, Test, CatDicts, Opts) ->
    grow_size(),
    case run(Test, Opts) of
	{passed, Categories} ->
	    print(".", [], Opts),
	    NewCatDicts = update_catdicts(Categories, CatDicts),
	    perform_tr(Performed + 1, Left - 1, Test, NewCatDicts, Opts);
	{failed, Reason, Bound} ->
	    print("!", [], Opts),
	    {failed, Performed + 1, Reason, Bound};
	{error, rejected} ->
	    print("x", [], Opts),
	    perform_tr(Performed, Left - 1, Test, CatDicts, Opts);
	Error = {error, _Reason} ->
	    Error
    end.

update_catdicts(Categories, none) ->
    lists:map(fun(C) -> orddict:from_list([{C,1}]) end, Categories);
update_catdicts(Categories, CatDicts) ->
    lists:zipwith(fun(C,D) -> add_to_category(C,D) end,
		  Categories, CatDicts).

add_to_category(Category, CatDict) ->
    case orddict:find(Category, CatDict) of
	{ok, Count} -> orddict:store(Category, Count + 1, CatDict);
	error       -> orddict:store(Category, 1, CatDict)
    end.

run(Test, Opts) ->
    run(Test, #ctx{}, Opts).

run(true, Context, _Opts) ->
   {passed, lists:reverse(Context#ctx.categories)};
run(false, Context, Opts) ->
    % TODO: should this be suppressed when on quiet mode?
    case Opts#opts.quiet of
	true  -> ok;
	false -> lists:foreach(fun(A) -> ?FORCE(A) end,
			       lists:reverse(Context#ctx.fail_actions))
    end,
    {failed, false_property, lists:reverse(Context#ctx.bound)};
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
run({'$apply',Args,Prop}, Context, Opts) ->
    % TODO: is it correct for this to cover errors and exits too?
    try
	run(erlang:apply(Prop, Args), Context, Opts)
    catch
	throw:Reason ->
	    {failed, false_property, Bound} = run(false, Context, Opts),
	    {failed, {throw,Reason}, Bound}
    end.

still_fails(TestCase, Test, OldReason) ->
    Opts = #opts{quiet = true, try_shrunk = true, shrunk = TestCase},
    case run(Test, Opts) of
	% We check that it's the same fault that caused the crash.
	% TODO: Should we check that the stacktrace is the same?
	{failed, Reason, _Bound} -> OldReason =:= Reason;
	_                        -> false
    end.

% We should never encounter false ?IMPLIES or true final results.
skip_to_next(false) ->
    false;
skip_to_next(Test = {'$forall',_RawType,_Prop}) ->
    Test;
skip_to_next({'$implies',true,Prop}) ->
    skip_to_next({'$apply',[],Prop});
skip_to_next({'$collect',_Category,Prop}) ->
    skip_to_next(Prop);
skip_to_next({'$whenfail',_Action,Prop}) ->
    skip_to_next({'$apply',[],Prop});
skip_to_next({'$apply',Args,Prop}) ->
    try
	skip_to_next(erlang:apply(Prop, Args))
    catch
	throw:_Reason -> false
    end.


% Output functions

print(Str, Args, Opts) ->
    case Opts#opts.quiet of
	true  -> ok;
	false -> io:format(Str, Args)
    end.

report_results(_Result, #opts{quiet = true}) ->
    ok;
report_results({passed,Performed,CatDicts}, Opts) ->
    case Opts#opts.expect_fail of
	true  -> io:format("~nError: no test failed~n", []);
	false -> io:format("~nOK, passed ~b tests~n", [Performed])
    end,
    print_categories(Performed, CatDicts),
    ok;
report_results({failed,Performed,_Reason,ImmFailedTestCase}, Opts) ->
    case Opts#opts.expect_fail of
	true ->
	    io:format("~nOK, failed as expected, after ~b tests.~n",
		      [Performed]);
	false ->
	    io:format("~nFailed, after ~b tests.~n", [Performed]),
	    FailedTestCase = proper_gen:clean_instance(ImmFailedTestCase),
	    print_instances(FailedTestCase),
	    io:format("Shrinking", [])
    end,
    ok;
report_results({error,cant_generate}, _Opts) ->
    io:format("~nError: couldn't produce an instance that satisfies all strict"
	      ++ " constraints after ~b tries~n",
	      [?MAX_TRIES_TO_SATISFY_CONSTRAINTS]),
    ok;
report_results({error,cant_satisfy}, _Opts) ->
    io:format("~nError: no valid test could be generated.~n", []),
    ok.

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

print_instances(Instances) ->
    lists:foreach(fun(I) -> io:format("~w~n", [I]) end, Instances),
    ok.

report_shrinking(_Shrinks, _MinTestCase, #opts{quiet = true}) ->
    ok;
report_shrinking(Shrinks, MinTestCase, _Opts) ->
    io:format("(~b times)~n", [Shrinks]),
    print_instances(MinTestCase),
    ok.
