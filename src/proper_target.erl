%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2017, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                and  Konstantinos Sagonas <kostis@it.uu.se>
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

%%% @copyright 2017 Andreas Löscher and Kostis Sagonas
%%% @version {@version}
%%% @author Andreas Löscher

%%% @doc This module defines the top-level behaviour for targeted
%%% property-based testing (TPBT). Using TPBT the input generation
%%% is no longer random, but guided by a search strategy to increase
%%% the probability of finding failing input. For this to work the user
%%% has to specify a search strategy and also needs to extract
%%% utility-values from the system under test that the search strategy
%%% then tries to maximize.
%%%
%%% To use TPBT the test specification macros `?FORALL_TARGETED`, `?EXISTS',
%%% and `?NOT_EXISTS' are used. The typical structure for a targeted
%%% property looks as follows:
%%%
%%% ```prop_target() ->                 % Try to check that
%%%      ?EXISTS(Input, Params,         % some input exists
%%%              begin                  % that fullfills the property.
%%%                UV = SUT:run(Input), % Do so by running SUT with Input
%%%                ?MAXIMIZE(UV),       % and maximize its Utility Value
%%%                UV < Threshold       % up to some Threshold.
%%%              end)).'''
%%%
%%% == Macros ==
%%%
%%% <dl>
%%%   <dt>`?MAXIMIZE(UV)'</dt>
%%%   <dd>This tells the search strategy to maximize the value `UV'.</dd>
%%%   <dt>`?MINIMIZE(UV)'</dt>
%%%   <dd>equivalent to `?MAXIMIZE(-UV)'</dd>
%%%   <dt>`?USERNF(Gen, Nf)'</dt>
%%%   <dd>This uses the neighborhood function `Nf' instead of PropEr's
%%%     constructed neighborhood function for this generator. The neighborhood
%%%     function `Fun' should be of type 
%%%    `fun(term(), {Depth :: pos_integer(), Temperature::float()} -> term()'</dd>
%%%   <dt>`?USERMATCHER(Gen, Matcher)'</dt>
%%%   <dd>This overwrites the structural matching of PropEr with the user provided
%%%     `Matcher' function. the matcher should be of type `proper_gen_next:matcher()'</dd>
%%% </dl>

-module(proper_target).
-behaviour(gen_server).

-include_lib("proper_internal.hrl").


%% -----------------------------------------------------------------------------
%% Exports
%% -----------------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([init_strategy/1, cleanup_strategy/0, init_target/1,
         update_uv/2, reset/0, targeted/1, get_shrinker/1]).


%% -----------------------------------------------------------------------------
%% Type declarations
%% -----------------------------------------------------------------------------

-type fitness() :: number().
-type tmap()    :: #{atom() => term()}.

-type threshold() :: fitness() | 'inf'.

-type target_state()  :: term().
-type strategy_data() :: term().
-type next_func()     :: fun ((target_state()) -> {target_state(), any()}).
-type fitness_func()  :: fun ((target_state(), fitness()) -> target_state()) 
                       | none.

-type target()   :: {target_state(), next_func(), fitness_func()}.
-type strategy() :: module().
-type opts()     :: strategy() 
                  | #{search_steps := integer(), search_strategy := strategy()}.

-record(state,
        {strategy           :: strategy(),
         target = undefined :: target_state() | undefined,
         data = undefined   :: strategy_data() | undefined}).
-type state() :: #state{}.

-export_type([fitness/0, tmap/0]).
-export_type([target_state/0, strategy_data/0, next_func/0, fitness_func/0,
              target/0, opts/0]).


%% -----------------------------------------------------------------------------
%% proper_target callback functions for defining strategies
%% ----------------------------------------------------------------------------

%% strategy global initializer
-callback init_strategy(proper:setup_opts()) -> strategy_data().
%% target initializer
-callback init_target(tmap()) -> target_state().
%% next function
-callback next(target_state(), strategy_data()) -> 
  {any(), target_state(), strategy_data()}.
%% update the strategy with the fitness
-callback update_fitness(fitness(), target_state(), strategy_data()) -> 
  {target_state(), strategy_data()}.
%% reset strat
-callback reset(target_state(), strategy_data()) -> 
  {target_state(), strategy_data()}.


%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

%% @doc Initializes targeted gen server based on a search strategy.
%% Depending on the argument, the search steps will be taken from
%% from the process dictionary or the setup options.

-spec init_strategy(Opts :: opts()) -> ok.
init_strategy(Strat) when is_atom(Strat) ->
  Steps = get('$search_steps'),
  init_strategy(#{search_steps => Steps, search_strategy => Strat});
init_strategy(#{search_steps := Steps, search_strategy := Strat}) ->
  Strategy = strategy(Strat),
  proper_gen_next:init(),
  Data = Strategy:init_strategy(#{numtests => Steps}),
  {ok, TargetserverPid} = gen_server:start_link(?MODULE, [{Strategy, Data}], []),
  put('$targetserver_pid', TargetserverPid),
  ok.

%% @doc Cleans up proper_gen_next as well as stopping the gen_server.

-spec cleanup_strategy() -> ok.
cleanup_strategy() ->
  case erase('$targetserver_pid') of
    undefined -> ok;
    TargetserverPid ->
      proper_gen_next:cleanup(),
      gen_server:stop(TargetserverPid)
  end.

%% This is used to create the targeted generator.
%% ?SHRINK so that this can be used with a ?SETUP macro,
%% in conjuction with ?FORALL.

%% @private
-spec targeted(tmap()) -> proper_types:type().
targeted(TMap) ->
  ?SHRINK(proper_types:exactly(?LAZY(targeted_gen(TMap))),
          [get_shrinker(TMap)]).

%% @doc Initialize the target of the strategy.

-spec init_target(tmap()) -> ok.
init_target(TMap) ->
  TargetserverPid = get('$targetserver_pid'),
  safe_call(TargetserverPid, {init_target, TMap}).

%% This produces the next gen instance from the next
%% generator provided by the strategy. It will also
%% update the state and data of the strategy.

%% @private
-spec targeted_gen(tmap()) -> any().
targeted_gen(TMap) ->
  init_target(TMap),
  TargetserverPid = get('$targetserver_pid'),
  gen_server:call(TargetserverPid, gen).

%% @doc Get the shrinker for a tmap.

-spec get_shrinker(tmap()) -> proper_types:type().
get_shrinker(#{gen := Gen}) ->
  Gen.

%% This is used to update the fitness value.
%% Depending on the strategy and the fitness this 
%% may accept the newly generated value.

%% @private
-spec update_uv(fitness(), threshold()) -> boolean().
update_uv(Fitness, Threshold) ->
  TargetserverPid = get('$targetserver_pid'),
  safe_call(TargetserverPid, {update_fitness, Fitness}),
  check_threshold(Threshold, Fitness).

%% @doc Reset the strategy target and data to a random
%% initial value. Useful when the generated instances
%% differ from the target, depending on the problem.

-spec reset() -> ok.
reset() ->
  TargetserverPid = get('$targetserver_pid'),
  safe_call(TargetserverPid, reset).

%% Create a safe call to a gen_server in case it
%% raises noproc. Τhis should only be used for
%% calls that do not return significant values.

%% @private
-spec safe_call(pid(), term()) -> term().
safe_call(Pid, Call) ->
  try
    gen_server:call(Pid, Call)
  catch _:{noproc, _} ->
    ok
  end.

%% @private
check_threshold(Threshold, Fitness) ->
  case Threshold of
    inf -> true;
    _ -> Fitness < Threshold
  end.

%% @private
strategy(Strat) ->
  case Strat of
    simulated_annealing ->
      proper_sa;
    hill_climbing ->
      put(target_sa_acceptfunc, hillclimbing),
      proper_sa;
    _ ->
      Strat
  end.


%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------

%% @private
-spec init(Args :: [{strategy(), strategy_data()}]) -> {ok, state()}.
init([{Strategy, Data}]) ->
  {ok, #state{strategy = Strategy, data = Data}}.

%% @private
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: state()) ->
  {reply, Reply :: term(), NewState :: state()}.
handle_call(gen, _From, State) ->
  Strat = State#state.strategy,
  Target = State#state.target,
  Data = State#state.data,
  {NextValue, NewTarget, NewData} = Strat:next(Target, Data),
  {reply, NextValue, State#state{target = NewTarget, data = NewData}};
handle_call({init_target, TMap}, _From, State) ->
  Strat = State#state.strategy,
  Target = State#state.target,
  NewTarget = case Target of
                undefined ->
                  case TMap of
                    #{gen := Gen} ->
                      NewTMap = proper_gen_next:from_proper_generator(Gen),
                      Strat:init_target(NewTMap);
                    #{first := _First, next := _Next} ->
                      Strat:init_target(TMap)
                  end;
                _ -> Target
              end,
  {reply, ok, State#state{target = NewTarget}};
handle_call({update_fitness, Fitness}, _From, State) ->
  Strat = State#state.strategy,
  Target = State#state.target,
  Data = State#state.data,
  {NewTarget, NewData} = Strat:update_fitness(Fitness, Target, Data),
  {reply, ok, State#state{target = NewTarget, data = NewData}};
handle_call(reset, _From, State) ->
  Strat = State#state.strategy,
  Target = State#state.target,
  Data = State#state.data,
  {NewTarget, NewData} = Strat:reset(Target, Data),
  {reply, ok, State#state{target = NewTarget, data = NewData}}.

%% @private
-spec handle_cast(Request :: term(), State :: state()) ->
  {noreply, NewState :: term()}.
handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
-spec handle_info(Info :: timeout | term(), State :: state()) ->
  {noreply, NewState :: state()}.
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                           term()),
                State :: state()) ->
  ok.
terminate(_Reason, _State) ->
  ok.

%% @private
-spec code_change(OldVsn :: (term() | {down, term()}), State :: state(),
                  Extra :: term()) ->
  {ok, NewState :: state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
