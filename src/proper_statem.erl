%%% -*- coding: utf-8; erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2010-2022 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>,
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

%%% @copyright 2010-2022 Manolis Papadakis, Eirini Arvaniti, and Kostis Sagonas
%%% @version {@version}
%%% @author Eirini Arvaniti

%%% @doc This module defines the `proper_statem' behaviour, useful for testing
%%% stateful reactive systems whose internal state and side-effects are
%%% specified via an abstract state machine. Given a callback module
%%% implementing the `proper_statem' behaviour (i.e. defining an abstract state
%%% machine of the system under test), PropEr can generate random symbolic
%%% sequences of calls to that system.
%%% As a next step, generated symbolic calls are actually performed, while
%%% monitoring the system's responses to ensure it behaves as expected. Upon
%%% failure, the shrinking mechanism attempts to find a minimal sequence of
%%% calls provoking the same error.
%%%
%%% When including the <code>"proper/include/proper.hrl"</code> header file,
%%% all <a href="#index">API functions </a> of {@module} are automatically
%%% imported, unless `PROPER_NO_IMPORTS' is defined.
%%%
%%% === The role of commands ===
%%% Testcases generated for testing a stateful system are lists of symbolic API
%%% calls to that system. Symbolic representation has several benefits, which
%%% are listed here in increasing order of importance:
%%% <ul>
%%% <li>Generated testcases are easier to read and understand.</li>
%%% <li>Failing testcases are easier to shrink.</li>
%%% <li>The generation phase is side-effect free and this results in
%%%   repeatable testcases, which is essential for correct shrinking.</li>
%%% </ul>
%%% Since the actual results of symbolic calls are not known at generation time,
%%% we use symbolic variables ({@type symbolic_var()}) to refer to them.
%%% A command ({@type command()}) is a symbolic term, used to bind a symbolic
%%% variable to the result of a symbolic call. For example:
%%%
%%% ```[{set, {var,1}, {call,erlang,put,[a,42]}},
%%%     {set, {var,2}, {call,erlang,erase,[a]}},
%%%     {set, {var,3}, {call,erlang,put,[b,{var,2}]}}]'''
%%%
%%% is a command sequence that could be used to test the process dictionary.
%%% In this example, the first call stores the pair `{a,42}' in the process
%%% dictionary, while the second one deletes it. Then, a new pair `{b,{var,2}}'
%%% is stored. `{var,2}' is a symbolic variable bound to the result of
%%% `erlang:erase/1'. This result is not known at generation time, since none of
%%% these operations is performed at that time. After evaluating the command
%%% sequence at runtime, the process dictionary will eventually contain the
%%% pair `{b,42}'.
%%%
%%% === The abstract model-state ===
%%% In order to be able to test impure code, we need a way to track its
%%% internal state (at least the useful part of it). To this end, we use an
%%% abstract state machine representing the possible configurations of the
%%% system under test. When referring to the <i>model state</i>, we mean the
%%% state of the abstract state machine. The <i>model state</i> can be either
%%% symbolic or dynamic:
%%% <ul>
%%% <li>During command generation, we use symbolic variables to bind the
%%% results of symbolic calls. Therefore, the model state might
%%% (and usually does) contain symbolic variables and/or symbolic calls, which
%%% are necessary to operate on symbolic variables. Thus, we refer to it as
%%% symbolic state. For example, assuming that the internal state of the
%%% process dictionary is modeled as a proplist, the model state after
%%% generating the previous command sequence will be `[{b,{var,2}}]'.</li>
%%% <li>During runtime, symbolic calls are evaluated and symbolic variables are
%%% replaced by their corresponding real values. Now we refer to the state as
%%% dynamic state. After running the previous command sequence, the model state
%%% will be `[{b,42}]'.</li>
%%% </ul>
%%%
%%% === The callback functions ===
%%% The following functions must be exported from the callback module
%%% implementing the abstract state machine:
%%% <ul>
%%% <li>`initial_state() ->' {@type symbolic_state()}
%%%   <p>Specifies the symbolic initial state of the state machine. This state
%%%   will be evaluated at command execution time to produce the actual initial
%%%   state. The function is not only called at command generation time, but
%%%   also in order to initialize the state every time the command sequence is
%%%   run (i.e. during normal execution, while shrinking and when checking a
%%%   counterexample). For this reason, it should be deterministic and
%%%   self-contained.</p></li>
%%% <li>`command(S::'{@type symbolic_state()}`) ->' {@type proper_types:type()}
%%%   <p>Generates a symbolic call to be included in the command sequence,
%%%   given the current state `S' of the abstract state machine. However,
%%%   before the call is actually included, a precondition is checked. This
%%%   function will be repeatedly called to produce the next call to be
%%%   included in the test case.</p></li>
%%% <li>`precondition(S::'{@type symbolic_state()}`,
%%%                   Call::'{@type symbolic_call()}`) -> boolean()'
%%%   <p>Specifies the precondition that should hold so that `Call' can be
%%%   included in the command sequence, given the current state `S' of the
%%%   abstract state machine. In case precondition doesn't hold, a new call is
%%%   chosen using the `command/1' generator. If preconditions are very strict,
%%%   it will take a lot of tries for PropEr to randomly choose a valid command.
%%%   Testing will be stopped if the `constraint_tries' limit is reached
%%%   (see the 'Options' section in the {@link proper} module documentation) and
%%%   a `{cant_generate,[{proper_statem,commands,4}]}' error will be produced in
%%%   that case.
%%%   Preconditions are also important for correct shrinking of failing
%%%   testcases. When shrinking command sequences, we try to eliminate commands
%%%   that do not contribute to failure, ensuring that all preconditions still
%%%   hold. Validating preconditions is necessary because during shrinking we
%%%   usually attempt to perform a call with the system being in a state
%%%   different from the state it was when initially running the test.</p></li>
%%% <li>`postcondition(S::'{@type dynamic_state()}`,
%%%                    Call::'{@type symbolic_call()}`,
%%%                    Res::term()) -> boolean()'
%%%   <p>Specifies the postcondition that should hold about the result `Res' of
%%%   performing `Call', given the dynamic state `S' of the abstract state
%%%   machine prior to command execution. This function is called during
%%%   runtime, this is why the state is dynamic.</p></li>
%%% <li>`next_state(S::'{@type symbolic_state()} `|' {@type dynamic_state()}`,
%%%                 Res::term(),
%%%                 Call::'{@type symbolic_call()}`) ->'
%%%        {@type symbolic_state()} `|' {@type dynamic_state()}
%%%   <p>Specifies the next state of the abstract state machine, given the
%%%   current state `S', the symbolic `Call' chosen and its result `Res'. This
%%%   function is called both at command generation and command execution time
%%%   in order to update the model state, therefore the state `S' and the
%%%   result `Res' can be either symbolic or dynamic.</p></li>
%%% </ul>
%%%
%%% === The property used ===
%%% Each test consists of two phases:
%%% <ul>
%%% <li>As a first step, PropEr generates random symbolic command sequences
%%%   deriving information from the callback module implementing the abstract
%%%   state machine. This is the role of {@link commands/1} generator.</li>
%%% <li>As a second step, command sequences are executed so as to check that
%%%   the system behaves as expected. This is the role of
%%%   {@link run_commands/2}, a function that evaluates a symbolic command
%%%   sequence according to an abstract state machine specification.</li>
%%% </ul>
%%%
%%% These two phases are encapsulated in the following property, which can be
%%% used for testing the process dictionary:
%%%
%%% ```prop_pdict() ->
%%%        ?FORALL(Cmds, proper_statem:commands(?MODULE),
%%%                begin
%%%                    {_History, _State, Result} = proper_statem:run_commands(?MODULE, Cmds),
%%%                    cleanup(),
%%%                    Result =:= ok
%%%                end).'''
%%%
%%% When testing impure code, it is very important to keep each test
%%% self-contained. For this reason, almost every property for testing stateful
%%% systems contains some clean-up code. Such code is necessary to put the
%%% system in a known state, so that the next test can be executed
%%% independently from previous ones.
%%%
%%% == Parallel testing ==
%%% After ensuring that a system's behaviour can be described via an abstract
%%% state machine when commands are executed sequentially, it is possible to
%%% move to parallel testing. The same state machine can be used to generate
%%% command sequences that will be executed in parallel to test for race
%%% conditions. A parallel testcase ({@type parallel_testcase()}) consists of
%%% a sequential and a parallel component. The sequential component is a
%%% command sequence that is run first to put the system in a random state.
%%% The parallel component is a list containing 2 command sequences to be
%%% executed in parallel, each of them in a separate newly-spawned process.
%%%
%%% Generating parallel test cases involves the following actions. Initially,
%%% we generate a command sequence deriving information from the abstract
%%% state machine specification, as in the case of sequential statem testing.
%%% Then, we parallelize a random suffix (up to 12 commands) of the initial
%%% sequence by splitting it into 2 subsequences that will be executed
%%% concurrently. Limitations arise from the fact that each subsequence should
%%% be a <i>valid</i> command sequence (i.e. all commands should satisfy
%%% preconditions and use only symbolic variables bound to the results of
%%% preceding calls in the same sequence). Furthermore, we apply an additional
%%% check: we have to ensure that preconditions are satisfied in all possible
%%% interleavings of the concurrent tasks. Otherwise, an exception might be
%%% raised during parallel execution and lead to unexpected (and unwanted) test
%%% failure. In case these constraints cannot be satisfied for a specific test
%%% case, the test case will be executed sequentially. Then an `f' is printed
%%% on screen to inform the user. This usually means that preconditions need
%%% to become less strict for parallel testing to work.
%%%
%%% After running a parallel testcase, PropEr uses the state machine
%%% specification to check if the results observed could have been produced by
%%% a possible serialization of the parallel component. If no such serialization
%%% is possible, then an atomicity violation has been detected. In this case,
%%% the shrinking mechanism attempts to produce a counterexample that is minimal
%%% in terms of concurrent operations. Properties for parallel testing are very
%%% similar to those used for sequential testing.
%%%
%%% ```prop_parallel_testing() ->
%%%        ?FORALL(Testcase, proper_statem:parallel_commands(?MODULE),
%%%                begin
%%%                    {_Sequential, _Parallel, Result} = proper_statem:run_parallel_commands(?MODULE, Testcase),
%%%                    cleanup(),
%%%                    Result =:= ok
%%%                end).'''
%%%
%%% Please note that the actual interleaving of commands of the parallel
%%% component depends on the Erlang scheduler, which is too deterministic.
%%% For PropEr to be able to detect race conditions, the code of the system
%%% under test should be instrumented with `erlang:yield/0' calls to the
%%% scheduler.
%%%
%%% == Stateful Targeted Testing ==
%%% During testing of the system's behavior, there may be some failing command
%%% sequences that the random property based testing does not find with ease,
%%% or at all. In these cases, stateful targeted property based testing can help
%%% find such edge cases, provided a utility value.
%%%
%%% ```prop_targeted_testing() ->
%%%        ?FORALL_TARGETED(Cmds, proper_statem:commands(?MODULE),
%%%                         begin
%%%                             {History, State, Result} = proper_statem:run_commands(?MODULE, Cmds),
%%%                             UV = uv(History, State, Result),
%%%                             ?MAXIMIZE(UV),
%%%                             cleanup(),
%%%                             Result =:= ok
%%%                         end).'''
%%%
%%% Νote that the `UV' value can be computed in any way fit, depending on the
%%% use case. `uv/3' is used here as a dummy function which computes the
%%% utility value.
%%% @end

-module(proper_statem).

-export([commands/1, commands/2, parallel_commands/1, parallel_commands/2,
         more_commands/2]).
-export([run_commands/2, run_commands/3, run_parallel_commands/2,
	 run_parallel_commands/3]).
-export([state_after/2, command_names/1, zip/2]).
%% Exported for PropEr internal usage.
-export([commands_gen/1, commands_gen/2, next_commands_gen/1,
         next_commands_gen/2]).


-export_type([symbolic_var/0, symbolic_call/0, statem_result/0]).


-include("proper_internal.hrl").

-define(WORKERS, 2).
-define(LIMIT, 12).

-define(COMMANDS_SZ_FACTOR, '$commands_size_factor').
-define(RESIZE_FACTOR, proper_types:parameter(?COMMANDS_SZ_FACTOR, 1)).

%% -----------------------------------------------------------------------------
%% Exported only for testing purposes
%% -----------------------------------------------------------------------------

-export([index/2, all_insertions/3, insert_all/2]).
-export([is_valid/4, args_defined/2]).
-export([get_next/6, mk_first_comb/3]).
-export([execute/3, check/6, run/3, get_initial_state/2]).

%% -----------------------------------------------------------------------------
%% Type declarations
%% -----------------------------------------------------------------------------

-type symbolic_state()    :: term().
-type dynamic_state()     :: term().
-type symbolic_var()      :: {'var',pos_integer()}.
-type symbolic_call()     :: {'call',mod_name(),fun_name(),[term()]}.
-type command()           :: {'set',symbolic_var(),symbolic_call()}
			   | {'init',symbolic_state()}.
-type command_list()      :: [command()].
-type parallel_testcase() :: {command_list(),[command_list()]}.
-type parallel_history()  :: [{command(),term()}].
-type history()           :: [{dynamic_state(),term()}].
-type statem_result() :: 'ok'
		       | 'initialization_error'
		       | {'precondition',  'false' | proper:exception()}
		       | {'postcondition', 'false' | proper:exception()}
		       | proper:exception()
		       | 'no_possible_interleaving'.
-type len()         :: non_neg_integer().
-type index()       :: pos_integer().
-type indices()     :: [index()].
-type combination() :: [{pos_integer(),indices()}].
-type lookup()      :: orddict:orddict(index(),command()).
-type threshold()   :: non_neg_integer().
-type next_temp()   :: {proper_gen_next:depth(), proper_gen_next:temperature()}.
-type next_fun()    :: fun((command_list(), next_temp()) ->
                              proper_types:type()).


%% -----------------------------------------------------------------------------
%% Proper_statem behaviour callback functions
%% -----------------------------------------------------------------------------

-callback initial_state() -> symbolic_state().

-callback command(symbolic_state()) -> proper_types:type().

-callback precondition(symbolic_state(), symbolic_call()) -> boolean().

-callback postcondition(dynamic_state(), symbolic_call(), term()) -> boolean().

-callback next_state(symbolic_state() | dynamic_state(), term(),
		     symbolic_call()) -> symbolic_state() | dynamic_state().

%% -----------------------------------------------------------------------------
%% Sequential command generation
%% -----------------------------------------------------------------------------

%% @doc A special PropEr type which generates random command sequences,
%% according to an abstract state machine specification. The function takes as
%% input the name of a callback module, which contains the state machine
%% specification. The initial state is computed by `Mod:initial_state/0'.

-spec commands(mod_name()) -> proper_types:type().
commands(Mod) ->
  ?USERNF(commands_gen(Mod), next_commands_gen(Mod)).

-spec commands_gen(mod_name()) -> proper_types:type().
commands_gen(Mod) ->
    ?LET(InitialState, ?LAZY(Mod:initial_state()),
	 ?SUCHTHAT(
	    Cmds,
	    ?LET(List,
		 ?SIZED(Size,
			proper_types:noshrink(
			  commands_gen(Size * ?RESIZE_FACTOR,
				       Mod, InitialState, 1))),
		 proper_types:shrink_list(List)),
	    is_valid(Mod, InitialState, Cmds, []))).

%% @doc Similar to {@link commands/1}, but generated command sequences always
%% start at a given state. In this case, the first command is always
%% `{init,InitialState}' and is used to correctly initialize the state
%% every time the command sequence is run (i.e. during normal execution,
%% while shrinking and when checking a counterexample). In this case,
%% `Mod:initial_state/0' is never called.

-spec commands(mod_name(), symbolic_state()) -> proper_types:type().
commands(Mod, InitialState) ->
  ?USERNF(commands_gen(Mod, InitialState),
          next_commands_gen(Mod, InitialState)).

-spec commands_gen(mod_name(), symbolic_state()) -> proper_types:type().
commands_gen(Mod, InitialState) ->
    ?SUCHTHAT(
       Cmds,
       ?LET(CmdTail,
	    ?LET(List,
		 ?SIZED(Size,
			proper_types:noshrink(
			  commands_gen(Size * ?RESIZE_FACTOR,
				       Mod, InitialState, 1))),
		 proper_types:shrink_list(List)),
	    [{init,InitialState}|CmdTail]),
       is_valid(Mod, InitialState, Cmds, [])).

%% @private
-spec commands_gen(proper_gen:size(), mod_name(), symbolic_state(), pos_integer()) ->
         proper_types:type().
commands_gen(Size, Mod, State, Count) ->
    ?LAZY(
       proper_types:frequency(
	 [{1, []},
	  {Size, ?LET(Call,
		      ?SUCHTHAT(X, Mod:command(State),
				Mod:precondition(State, X)),
		      begin
			  Var = {var,Count},
			  NextState = Mod:next_state(State, Var, Call),
			  ?LET(
			     Cmds,
			     commands_gen(Size-1, Mod, NextState, Count+1),
			     [{set,Var,Call}|Cmds])
		      end)}])).

%% @doc Increases the expected length of command sequences generated from
%% `CmdType' by a factor `N'.

-spec more_commands(pos_integer(), proper_types:type()) -> proper_types:type().
more_commands(N, CmdType) ->
    proper_types:with_parameter(?COMMANDS_SZ_FACTOR, N, CmdType).


%% -----------------------------------------------------------------------------
%% Parallel command generation
%% -----------------------------------------------------------------------------

%% @doc A special PropEr type which generates parallel testcases,
%% according to an absract state machine specification. The function takes as
%% input the name of a callback module, which contains the state machine
%% specification. The initial state is computed by `Mod:initial_state/0'.

-spec parallel_commands(mod_name()) -> proper_types:type().
parallel_commands(Mod) ->
    ?LET({ShrunkSeq, ShrunkPar},
	 ?LET({Seq, Par},
	      proper_types:noshrink(parallel_gen(Mod)),
	      parallel_shrinker(Mod, Seq, Par)),
	 move_shrinker(ShrunkSeq, ShrunkPar, ?WORKERS)).

%% @doc Similar to {@link parallel_commands/1}, but generated command sequences
%% always start at a given state.

-spec parallel_commands(mod_name(), symbolic_state()) -> proper_types:type().
parallel_commands(Mod, InitialState) ->
    ?LET({ShrunkSeq, ShrunkPar},
	 ?LET({Seq, Par},
	      proper_types:noshrink(parallel_gen(Mod, InitialState)),
	      parallel_shrinker(Mod, Seq, Par)),
	 move_shrinker(ShrunkSeq, ShrunkPar, ?WORKERS)).

-spec parallel_gen(mod_name()) -> proper_types:type().
parallel_gen(Mod) ->
    ?LET(Seq,
	 commands_gen(Mod),
	 mk_parallel_testcase(Mod, Seq)).

-spec parallel_gen(mod_name(), symbolic_state()) -> proper_types:type().
parallel_gen(Mod, InitialState) ->
    ?LET(Seq,
	 commands_gen(Mod, InitialState),
	 mk_parallel_testcase(Mod, Seq)).

-spec mk_parallel_testcase(mod_name(), command_list()) -> proper_types:type().
mk_parallel_testcase(Mod, Seq) ->
    {State, SymbEnv} = state_env_after(Mod, Seq),
    Count = case SymbEnv of
		[]          -> 1;
		[{var,N}|_] -> N + 1
	    end,
    ?LET(Parallel,
	 ?SUCHTHAT(C, commands_gen(?LIMIT, Mod, State, Count),
		   length(C) > ?WORKERS),
	 begin
	     LenPar = length(Parallel),
	     Len = LenPar div ?WORKERS,
	     Comb = mk_first_comb(LenPar, Len, ?WORKERS),
	     LookUp = orddict:from_list(mk_dict(Parallel, 1)),
	     {Seq, fix_parallel(LenPar, Len, Comb, LookUp, Mod,
				State, SymbEnv, ?WORKERS)}
	 end).

-spec parallel_shrinker(mod_name(), command_list(), [command_list()]) ->
	 proper_types:type().
parallel_shrinker(Mod, [{init,I} = Init|Seq], Parallel) ->
    ?SUCHTHAT({Seq1, Parallel1},
	      ?LET(ParInstances,
		   [proper_types:shrink_list(P) || P <- Parallel],
		   ?LET(SeqInstance,
			proper_types:shrink_list(Seq),
			{[Init|SeqInstance], ParInstances})),
	      lists:all(
		fun(P) -> is_valid(Mod, I, Seq1 ++ P, []) end,
		Parallel1));
parallel_shrinker(Mod, Seq, Parallel) ->
    I = Mod:initial_state(),
    ?SUCHTHAT({Seq1, Parallel1},
	      ?LET(ParInstances,
		   [proper_types:shrink_list(P) || P <- Parallel],
		   ?LET(SeqInstance,
			proper_types:shrink_list(Seq),
			{SeqInstance, ParInstances})),
	      lists:all(
		fun(P) -> is_valid(Mod, I, Seq1 ++ P, []) end,
		Parallel1)).

-spec move_shrinker(command_list(), [command_list()], index()) ->
	 proper_types:type().
move_shrinker(Seq, Par, 1) ->
    ?SHRINK({Seq, Par},
	    [{Seq ++ Slice, remove_slice(1, Slice, Par)}
	     ||	Slice <- get_slices(lists:nth(1, Par))]);
move_shrinker(Seq, Par, I) ->
    ?LET({NewSeq, NewPar},
	 ?SHRINK({Seq, Par},
		 [{Seq ++ Slice, remove_slice(I, Slice, Par)}
		  || Slice <- get_slices(lists:nth(I, Par))]),
	 move_shrinker(NewSeq, NewPar, I-1)).


%% -----------------------------------------------------------------------------
%% Targeted command generation
%% -----------------------------------------------------------------------------


%% @private
-spec next_commands_gen(mod_name()) -> next_fun().
next_commands_gen(Mod) ->
  fun (Cmds, {_Depth, Temp}) ->
      MaxRemovals = round(length(Cmds) * Temp),
      ?LET(InitialState, ?LAZY(Mod:initial_state()),
           ?SUCHTHAT(
              NewCmds,
              ?LET(List,
                   ?SIZED(Size,
                          proper_types:noshrink(
                            next_commands_gen(Mod, InitialState, Cmds,
                                              Size * ?RESIZE_FACTOR,
                                              MaxRemovals))),
                   proper_types:shrink_list(List)),
              is_valid(Mod, InitialState, NewCmds, [])))
  end.

%% @private
-spec next_commands_gen(mod_name(), symbolic_state()) -> next_fun().
next_commands_gen(Mod, InitialState) ->
  fun Aux([{init, _S} | Cmds], {Depth, Temp}) ->
      Aux(Cmds, {Depth, Temp});
      Aux(Cmds, {_Depth, Temp}) ->
      MaxRemovals = round(length(Cmds) * Temp),
      ?SUCHTHAT(
         NewCmds,
         ?LET(CmdsTail,
              ?LET(List,
                   ?SIZED(Size,
                          proper_types:noshrink(
                            next_commands_gen(Mod, InitialState, Cmds,
                                              Size * ?RESIZE_FACTOR,
                                              MaxRemovals))),
                   proper_types:shrink_list(List)),
              [{init, InitialState} | CmdsTail]),
         is_valid(Mod, InitialState, NewCmds, []))
  end.

-spec next_commands_gen(mod_name(), symbolic_state(), command_list(),
                    proper_gen:size(), threshold()) ->
        proper_types:type().
next_commands_gen(Mod, InitialState, [_ | _] = Cmds, Size, 0) ->
  next_commands_gen(Mod, InitialState, Cmds, Size, 1);
next_commands_gen(Mod, InitialState, Cmds, Size, MaxRemovals) ->
  %% Try to remove commands after the maximum current size is reached.
  Threshold = case Size > length(Cmds) of
                true -> 0;
                false -> MaxRemovals
              end,
  ?LET(LessCmds,
       ?LET(LessRCmds,
            remove_cmds(lists:reverse(Cmds), Threshold),
            lists:reverse(LessRCmds)),
       ?LET(CmdsTail,
            begin
              Length = length(LessCmds),
              Remaining = Size - Length,
              State = state_after(Mod, [{init, InitialState} | LessCmds]),
              Count = Length + 1,
              commands_gen(Remaining, Mod, State, Count)
            end,
            LessCmds ++ CmdsTail)).

-spec remove_cmds(command_list(), threshold()) -> proper_types:type().
remove_cmds(Cmds, Threshold) ->
  ?LAZY(proper_types:frequency(
          [{1, proper_types:exactly(Cmds)},
           {Threshold, ?LAZY(remove_cmds(tl(Cmds), Threshold - 1))}])).


%% -----------------------------------------------------------------------------
%% Sequential command execution
%% -----------------------------------------------------------------------------

%% @doc Evaluates a given symbolic command sequence `Cmds' according to the
%%  state machine specified in `Mod'. The result is a triple of the form<br/>
%%  `{History, DynamicState, Result}', where:
%% <ul>
%% <li>`History' contains the execution history of all commands that were
%%   executed without raising an exception. It contains tuples of the form
%%   {{@type dynamic_state()}, {@type term()}}, specifying the state prior to
%%   command execution and the actual result of the command.</li>
%% <li>`DynamicState' contains the state of the abstract state machine at
%%   the moment when execution stopped. In case execution has stopped due to a
%%   false postcondition, `DynamicState' corresponds to the state prior to
%%   execution of the last command.</li>
%% <li>`Result' specifies the outcome of command execution. It can be
%%   classified in one of the following categories:
%%   <ul>
%%   <li><b>ok</b>
%%     <p>All commands were successfully run and all postconditions were true.
%%     </p></li>
%%   <li><b>initialization error</b>
%%     <p>There was an error while evaluating the initial state.</p></li>
%%   <li><b>postcondition error</b>
%%     <p>A postcondition was false or raised an exception.</p></li>
%%   <li><b>precondition error</b>
%%     <p>A precondition was false or raised an exception.</p></li>
%%   <li><b>exception</b>
%%     <p>An exception was raised while running a command.</p></li>
%%   </ul></li>
%% </ul>

-spec run_commands(mod_name(), command_list()) ->
         {history(),dynamic_state(),statem_result()}.
run_commands(Mod, Cmds) ->
    run_commands(Mod, Cmds, []).

%% @doc  Similar to {@link run_commands/2}, but also accepts an environment,
%% used for symbolic variable evaluation during command execution. The
%% environment consists of `{Key::atom(), Value::term()}' pairs. Keys may be
%% used in symbolic variables (i.e. `{var,Key}') within the command sequence
%% `Cmds'. These symbolic variables will be replaced by their corresponding
%% `Value' during command execution.

-spec run_commands(mod_name(), command_list(), proper_symb:var_values()) ->
         {history(),dynamic_state(),statem_result()}.
run_commands(Mod, Cmds, Env) ->
    element(1, run(Mod, Cmds, Env)).

%% @private
-spec run(mod_name(), command_list(), proper_symb:var_values()) ->
       {{history(),dynamic_state(),statem_result()}, proper_symb:var_values()}.
run(Mod, Cmds, Env) ->
    InitialState = get_initial_state(Mod, Cmds),
    try proper_symb:eval(Env, InitialState) of
	DynState ->
	    run_commands(Cmds, Env, Mod, [], DynState)
    catch
	_Exc:_Reason ->
	    {{[], undefined, initialization_error}, []}
    end.

-spec run_commands(command_list(), proper_symb:var_values(), mod_name(),
		   history(), dynamic_state()) ->
       {{history(),dynamic_state(),statem_result()}, proper_symb:var_values()}.
run_commands(Cmds, Env, Mod, History, State) ->
    case Cmds of
	[] ->
	    {{lists:reverse(History), State, ok}, Env};
	[{init,_S}|Rest] ->
	    run_commands(Rest, Env, Mod, History, State);
	[{set, {var,V}, {call,M,F,A}}|Rest] ->
	    M2 = proper_symb:eval(Env, M),
	    F2 = proper_symb:eval(Env, F),
	    A2 = proper_symb:eval(Env, A),
	    Call = {call,M2,F2,A2},
	    case check_precondition(Mod, State, Call) of
		true ->
		    case safe_apply(M2, F2, A2) of
			{ok,Res} ->
			    Env2 = [{V,Res}|Env],
			    History2 = [{State,Res}|History],
			    case check_postcondition(Mod, State, Call, Res) of
				true ->
				    State2 = proper_symb:eval(Env2, Mod:next_state(State, Res, Call)),
				    run_commands(Rest, Env2, Mod, History2, State2);
				false ->
				    {{lists:reverse(History2), State, {postcondition,false}}, []};
				{exception,_,_,_} = Exception ->
				    {{lists:reverse(History2), State, {postcondition,Exception}}, []}
			    end;
			{error,Exception} ->
			    {{lists:reverse(History), State, Exception}, []}
		    end;
		false ->
		    {{lists:reverse(History), State, {precondition,false}}, []};
		{exception,_,_,_} = Exc ->
		    {{lists:reverse(History), State, {precondition,Exc}}, []}
	    end
    end.

-spec check_precondition(mod_name(), dynamic_state(), symbolic_call()) ->
         boolean() | proper:exception().
check_precondition(Mod, State, Call) ->
    try Mod:precondition(State, Call)
    catch
	Kind:Reason:StackTrace ->
	    {exception, Kind, Reason, StackTrace}
    end.

-spec check_postcondition(mod_name(), dynamic_state(), symbolic_call(), term()) ->
         boolean() | proper:exception().
check_postcondition(Mod, State, Call, Res) ->
    try Mod:postcondition(State, Call, Res)
    catch
	Kind:Reason:StackTrace ->
	    {exception, Kind, Reason, StackTrace}
    end.

-spec safe_apply(mod_name(), fun_name(), [term()]) ->
         {'ok', term()} | {'error', proper:exception()}.
safe_apply(M, F, A) ->
    try apply(M, F, A) of
	Result -> {ok, Result}
    catch
	Kind:Reason:StackTrace ->
	    {error, {exception, Kind, Reason, StackTrace}}
    end.


%% -----------------------------------------------------------------------------
%% Parallel command execution
%% -----------------------------------------------------------------------------

%% @doc Runs a given parallel testcase according to the state machine
%% specified in `Mod'. The result is a triple of the form<br/>
%% `@{Sequential_history, Parallel_history, Result@}', where:
%% <ul>
%% <li>`Sequential_history' contains the execution history of the
%%   sequential component.</li>
%% <li>`Parallel_history' contains the execution history of each of the
%%   concurrent tasks.</li>
%% <li>`Result' specifies the outcome of the attempt to serialize command
%%   execution, based on the results observed. In addition to results
%%   returned by {@link run_commands/2}, it can also be the atom
%%   `no_possible_interleaving'.</li>
%% </ul>

-spec run_parallel_commands(mod_name(), parallel_testcase()) ->
	 {history(),[parallel_history()],statem_result()}.
run_parallel_commands(Mod, {_Sequential, _Parallel} = Testcase) ->
    run_parallel_commands(Mod, Testcase, []).

%% @doc Similar to {@link run_parallel_commands/2}, but also accepts an
%% environment used for symbolic variable evaluation, exactly as described in
%% {@link run_commands/3}.

-spec run_parallel_commands(mod_name(), parallel_testcase(),
			    proper_symb:var_values()) ->
	 {history(),[parallel_history()],statem_result()}.
run_parallel_commands(Mod, {Sequential, Parallel}, Env) ->
    case run(Mod, Sequential, Env) of
	{{Seq_history, State, ok}, SeqEnv} ->
	    F = fun(T) -> execute(T, SeqEnv, Mod) end,
	    Parallel_history = pmap(F, Parallel),
	    case check(Mod, State, SeqEnv, false, [], Parallel_history) of
		true ->
		    {Seq_history, Parallel_history, ok};
		false ->
		    {Seq_history, Parallel_history, no_possible_interleaving}
	    end;
	{{Seq_history, _, Res}, _} ->
	    {Seq_history, [], Res}
    end.

%% @private
-spec execute(command_list(), proper_symb:var_values(), mod_name()) ->
	 parallel_history().
execute([], _Env, _Mod) -> [];
execute([{set, {var,V}, {call,M,F,A}} = Cmd|Rest], Env, Mod) ->
    M2 = proper_symb:eval(Env, M),
    F2 = proper_symb:eval(Env, F),
    A2 = proper_symb:eval(Env, A),
    Res = apply(M2, F2, A2),
    Env2 = [{V,Res}|Env],
    [{Cmd,Res}|execute(Rest, Env2, Mod)].

-spec pmap(fun((command_list()) -> parallel_history()), [command_list()]) ->
         [parallel_history()].
pmap(F, L) ->
    await(spawn_jobs(F, L)).

-spec spawn_jobs(fun((command_list()) -> parallel_history()),
		 [command_list()]) -> [pid()].
spawn_jobs(F, L) ->
    Parent = self(),
    [spawn_link_cp(fun() -> Parent ! {self(),catch {ok,F(X)}} end) || X <- L].

-spec await([pid()]) -> [parallel_history()].
await([]) -> [];
await([H|T]) ->
    receive
	{H, {ok, Res}} ->
	    [Res|await(T)];
	{H, {'EXIT',_} = Err} ->
	    _ = [exit(Pid, kill) || Pid <- T],
	    _ = [receive {P,_} -> d_ after 0 -> i_ end || P <- T],
	    erlang:error(Err)
    end.

%% @private
-spec check(mod_name(), dynamic_state(), proper_symb:var_values(),
	    boolean(), [parallel_history()], [parallel_history()]) -> boolean().
check(_Mod, _State, _Env, _Changed, [], []) ->
    true;
check(_Mod, _State, _Env, false, _Tried, []) ->
    false;
check(Mod, State, Env, true, Tried, []) ->
    check(Mod, State, Env, false, [], Tried);
check(Mod, State, Env, Changed, Tried, [P|ToTry]) ->
    case P of
	[] ->
	    check(Mod, State, Env, Changed, Tried, ToTry);
	[H|Tail] ->
	    {{set, {var,N}, {call,M,F,A}}, Res} = H,
	    M_ = proper_symb:eval(Env, M),
	    F_ = proper_symb:eval(Env, F),
	    A_ = proper_symb:eval(Env, A),
	    Call = {call,M_,F_,A_},
	    case Mod:postcondition(State, Call, Res) of
		true ->
		    Env2 = [{N, Res}|Env],
		    NextState = proper_symb:eval(
				  Env2, Mod:next_state(State, Res, Call)),
		    check(Mod, NextState, Env2, true, Tried, [Tail|ToTry])
			orelse check(Mod, State, Env, Changed,
				     [P|Tried], ToTry);
		false ->
		    check(Mod, State, Env, Changed, [P|Tried], ToTry)
	    end
    end.


%% -----------------------------------------------------------------------------
%% Other API functions
%% -----------------------------------------------------------------------------

%% @doc Extracts the names of the commands from a given command sequence, in
%% the form of MFAs. It is useful in combination with functions such as
%% {@link proper:aggregate/2} in order to collect statistics about command
%% execution.

-spec command_names(command_list() | parallel_testcase()) -> [mfa()].
command_names({Cmds, L}) ->
    lists:flatten([command_names(Cmds)|[ command_names(Cs) || Cs <- L ]]);
command_names(Cmds) ->
    [{M, F, length(Args)} || {set, _Var, {call,M,F,Args}} <- Cmds].

%% @doc Returns the symbolic state after running a given command sequence,
%% according to the state machine specification found in `Mod'. The commands
%% are not actually executed.

-spec state_after(mod_name(), command_list()) -> symbolic_state().
state_after(Mod, Cmds) ->
    element(1, state_env_after(Mod, Cmds)).

-spec state_env_after(mod_name(), command_list()) ->
         {symbolic_state(), [symbolic_var()]}.
state_env_after(Mod, Cmds) ->
    lists:foldl(fun({init,S}, _) ->
			{S, []};
		   ({set,Var,Call}, {S,Vars}) ->
			{Mod:next_state(S, Var, Call), [Var|Vars]}
		end,
		{get_initial_state(Mod, Cmds), []},
		Cmds).

%% @doc Behaves like `lists:zip/2', but the input lists do no not necessarily
%% have equal length. Zipping stops when the shortest list stops. This is
%% useful for zipping a command sequence with its (failing) execution history.

-spec zip([A], [B]) -> [{A,B}].
zip([A|X], [B|Y]) -> [{A,B}|zip(X, Y)];
zip(_, []) -> [];
zip([], _) -> [].

%% -----------------------------------------------------------------------------
%% Utility functions
%% -----------------------------------------------------------------------------

%% @private
-spec is_valid(mod_name(), symbolic_state(), command_list(), [symbolic_var()]) ->
         boolean().
is_valid(_Mod, _State, [], _SymbEnv) -> true;
is_valid(Mod, _State, [{init,S}|Cmds], _SymbEnv) ->
    is_valid(Mod, S, Cmds, _SymbEnv);
is_valid(Mod, State, [{set, Var, {call,_M,_F,A} = Call}|Cmds], SymbEnv) ->
    args_defined(A, SymbEnv) andalso Mod:precondition(State, Call)
	andalso is_valid(Mod, Mod:next_state(State, Var, Call), Cmds,
			 [Var|SymbEnv]).

%% @private
-spec args_defined([term()], [symbolic_var()]) -> boolean().
args_defined(List, SymbEnv) ->
   lists:all(fun (A) -> arg_defined(A, SymbEnv) end, List).

-spec arg_defined(term(), [symbolic_var()]) -> boolean().
arg_defined({var,I} = V, SymbEnv) when is_integer(I) ->
    lists:member(V, SymbEnv);
arg_defined(Tuple, SymbEnv) when is_tuple(Tuple) ->
    args_defined(tuple_to_list(Tuple), SymbEnv);
arg_defined([Head|Tail], SymbEnv) ->
    arg_defined(Head, SymbEnv) andalso arg_defined(Tail, SymbEnv);
arg_defined(_, _) ->
    true.

%% @private
-spec get_initial_state(mod_name(), command_list()) -> symbolic_state().
get_initial_state(_, [{init,S}|_]) -> S;
get_initial_state(Mod, Cmds) when is_list(Cmds) ->
    Mod:initial_state().

%% @private
-spec fix_parallel(index(), len(), combination() | 'done',
		   lookup(), mod_name(), symbolic_state(), [symbolic_var()],
		   pos_integer()) -> [command_list()].
fix_parallel(_, 0, done, _, _, _, _, _) ->
    exit(error);   %% not supposed to reach here
fix_parallel(MaxIndex, Len, done, LookUp, Mod, State, SymbEnv, W) ->
    Comb = mk_first_comb(MaxIndex, Len-1, W),
    case Len of
	1 -> io:format("f");
	_ -> ok
    end,
    fix_parallel(MaxIndex, Len-1, Comb , LookUp, Mod, State, SymbEnv, W);
fix_parallel(MaxIndex, Len, Comb, LookUp, Mod, State, SymbEnv, W) ->
    CmdLists = lookup_cmd_lists(Comb, LookUp),
    case can_parallelize(CmdLists, Mod, State, SymbEnv) of
	true ->
	    lists:reverse(CmdLists);
	false ->
	    C1 = proplists:get_value(1, Comb),
	    C2 = proplists:get_value(2, Comb),
	    Next = get_next(Comb, Len, MaxIndex, lists:sort(C1 ++ C2), W, 2),
	    fix_parallel(MaxIndex, Len, Next, LookUp, Mod, State, SymbEnv, W)
    end.

-spec can_parallelize([command_list()], mod_name(), symbolic_state(),
		      [symbolic_var()]) -> boolean().
can_parallelize(CmdLists, Mod, State, SymbEnv) ->
    F = fun(C) -> is_valid(Mod, State, C, SymbEnv) end,
    lists:all(F, CmdLists) andalso
	lists:all(F, possible_interleavings(CmdLists)).

%% @private
-spec possible_interleavings([command_list()]) -> [command_list()].
possible_interleavings([P1,P2]) ->
    insert_all(P1, P2);
possible_interleavings([P1|Rest]) ->
    [I || L <- possible_interleavings(Rest),
	  I <- insert_all(P1, L)].

%% @private
%% Returns all possible insertions of the elements of the first list,
%% preserving their order, inside the second list, i.e. all possible
%% command interleavings between two parallel processes
-spec insert_all([term()], [term()]) -> [[term()]].
insert_all([], List) ->
    [List];
insert_all([X], List) ->
    all_insertions(X, length(List) + 1, List);

insert_all([X|[Y|Rest]], List) ->
    [L2 || L1 <- insert_all([Y|Rest], List),
	   L2 <- all_insertions(X, index(Y, L1), L1)].

%% @private
-spec all_insertions(term(), pos_integer(), [term()]) -> [[term()]].
all_insertions(X, Limit, List) ->
    all_insertions_tr(X, Limit, 0, [], List, []).

-spec all_insertions_tr(term(), pos_integer(), len(),
			[term()], [term()], [[term()]]) -> [[term()]].
all_insertions_tr(X, Limit, LengthFront, Front, [], Acc) ->
    case LengthFront < Limit of
	true ->
	    [Front ++ [X] | Acc];
	false ->
	    Acc
    end;
all_insertions_tr(X, Limit, LengthFront, Front, Back = [BackH|BackT], Acc) ->
    case LengthFront < Limit of
	true ->
	    all_insertions_tr(X, Limit, LengthFront+1, Front ++ [BackH],
			      BackT, [Front ++ [X] ++ Back | Acc]);
	false -> Acc
    end.

%% @private
-spec index(term(), [term(),...]) -> index().
index(X, List) ->
    index(X, List, 1).

-spec index(term(), [term(),...], index()) -> index().
index(X, [X|_], N) -> N;
index(X, [_|Rest], N) -> index(X, Rest, N+1).

%% @private
-spec mk_dict(command_list(), pos_integer()) -> [{pos_integer(), command()}].
mk_dict([], _)           -> [];
mk_dict([{init,_}|T], N) -> mk_dict(T, N);
mk_dict([H|T], N)        -> [{N,H}|mk_dict(T, N+1)].

%% @private
-spec mk_first_comb(pos_integer(), len(), pos_integer()) -> combination().
mk_first_comb(N, Len, W) ->
    mk_first_comb_tr(1, N, Len, [], W).

-spec mk_first_comb_tr(pos_integer(), pos_integer(), len(),
		       combination(), pos_integer()) -> combination().
mk_first_comb_tr(Start, N, _Len, Accum, 1) ->
    [{1,lists:seq(Start, N)}|Accum];
mk_first_comb_tr(Start, N, Len, Accum, W) ->
    K = Start + Len,
    mk_first_comb_tr(K, N, Len, [{W,lists:seq(Start, K-1)}|Accum], W-1).

-spec lookup_cmds(indices(), lookup()) -> command_list().
lookup_cmds(Indices, LookUp) ->
    [orddict:fetch(Index, LookUp) || Index <- Indices].

-spec lookup_cmd_lists(combination(), lookup()) -> [command_list()].
lookup_cmd_lists(Combination, LookUp) ->
    [lookup_cmds(Indices, LookUp) || {_, Indices} <- Combination].

%% @private
-spec get_next(combination(), len(), index(), indices(),
	       pos_integer(), pos_integer()) -> combination() | 'done'.
get_next(L, _Len, _MaxIndex, Available, _Workers, 1) ->
    [{1,Available}|proplists:delete(1, L)];
get_next(L, Len, MaxIndex, Available, Workers, N) ->
    C = case proplists:is_defined(N, L) of
	    true ->
		next_comb(MaxIndex, proplists:get_value(N, L), Available);
	    false ->
		lists:sublist(Available, Len)
	end,
    case C of
	done ->
	    if N =:= Workers ->
		    done;
	       N =/= Workers ->
		    C2 = proplists:get_value(N+1, L),
		    NewList = [E || {M,_}=E <- L, M > N],
		    get_next(NewList, Len, MaxIndex,
			     lists:sort(C2 ++ Available), Workers, N+1)
	    end;
	_ ->
	    get_next([{N,C}|proplists:delete(N, L)],
		     Len, MaxIndex, Available -- C, Workers, N-1)
    end.

-spec next_comb(index(), indices(), indices()) -> indices() | 'done'.
next_comb(MaxIndex, Indices, Available) ->
    Res = next_comb_tr(MaxIndex, lists:reverse(Indices), []),
    case is_well_defined(Res, Available) of
	true -> Res;
	false -> next_comb(MaxIndex, Res, Available)
    end.

-spec is_well_defined(indices() | 'done', indices()) -> boolean().
is_well_defined(done, _) -> true;
is_well_defined(Comb, Available) ->
    lists:usort(Comb) =:= Comb andalso
	lists:all(fun(X) -> lists:member(X, Available) end, Comb).

-spec next_comb_tr(index(), indices(), indices()) -> indices() | 'done'.
next_comb_tr(_MaxIndex, [], _Acc) ->
    done;
next_comb_tr(MaxIndex, [MaxIndex | Rest], Acc) ->
    next_comb_tr(MaxIndex, Rest, [1 | Acc]);
next_comb_tr(_MaxIndex, [X | Rest], Acc) ->
    lists:reverse(Rest, [X+1|Acc]).

-spec remove_slice(index(), command_list(), [command_list(),...]) ->
         [command_list(),...].
remove_slice(Index, Slice, List) ->
    remove_slice_tr(Index, Slice, List, [], 1).

-spec remove_slice_tr(index(), command_list(), [command_list(),...],
		      [command_list()], pos_integer()) -> [command_list(),...].
remove_slice_tr(Index, Slice, [H|T], Acc, Index) ->
    lists:reverse(Acc, [H -- Slice] ++ T);
remove_slice_tr(Index, Slice, [H|T], Acc, N) ->
    remove_slice_tr(Index, Slice, T, [H|Acc], N+1).

-spec get_slices(command_list()) -> [command_list()].
get_slices(List) ->
    get_slices_tr(List, List, 1, []).

-spec get_slices_tr(command_list(), command_list(), pos_integer(),
		    [command_list()]) -> [command_list()].
get_slices_tr([], _, _, Acc) -> Acc;
get_slices_tr([_|Tail], List, N, Acc) ->
    get_slices_tr(Tail, List, N+1, [lists:sublist(List, N)|Acc]).

-spec spawn_link_cp(fun(() -> _)) -> pid().
spawn_link_cp(ActualFun) ->
    PDictStuff = [Pair || {K,_V} = Pair <- get(),
			  is_atom(K),
			  re:run(atom_to_list(K), ["^[$]"],
				 [{capture,none}]) =:= match],
    Fun = fun() ->
	      lists:foreach(fun({K,V}) -> put(K,V) end, PDictStuff),
	      proper_arith:rand_reseed(),
	      ActualFun()
	  end,
    spawn_link(Fun).
