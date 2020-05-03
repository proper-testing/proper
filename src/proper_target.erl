%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2017-2020 Andreas Löscher <andreas.loscher@it.uu.se>
%%%                     and Kostis Sagonas <kostis@it.uu.se>
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

%%% @copyright 2017-2020 Andreas Löscher and Kostis Sagonas
%%% @version {@version}
%%% @author Andreas Löscher

%%% @doc This module defines the top-level behaviour for Targeted
%%% Property-Based Testing (TPBT). Using TPBT the input generation
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

-type strategy()     :: mod_name().
-type fitness()      :: number().
-type search_steps() :: pos_integer().
-type threshold()    :: fitness() | 'inf'.

-type target_state()  :: term().
-type strategy_data() :: term().
-type next_fun_ret()  :: proper_types:type() | proper_gen:instance().
-type next_fun()      :: fun ((...) -> next_fun_ret()).
-type fitness_fun()   :: fun ((target_state(), fitness()) -> target_state())
                       | none.
-type opts()          :: #{search_steps := search_steps(),
                           search_strategy := strategy(),
                           atom() => term()}.

-record(state,
        {strategy           :: strategy(),
         target = undefined :: target_state() | undefined,
         data = undefined   :: strategy_data() | undefined,
         stateful = false   :: boolean()}).
-type state() :: #state{}.

-export_type([strategy/0, fitness/0, search_steps/0]).
-export_type([target_state/0, strategy_data/0, next_fun/0, fitness_fun/0,
              opts/0]).


%% -----------------------------------------------------------------------------
%% proper_target callback functions for defining strategies
%% -----------------------------------------------------------------------------

%% strategy global initializer
-callback init_strategy(search_steps()) -> strategy_data().
%% target initializer
-callback init_target(proper_types:type(), next_fun()) -> target_state().
%% next function
-callback next(target_state(), strategy_data()) ->
  {proper_gen:instance(), target_state(), strategy_data()}.
%% shrinker
-callback get_shrinker(target_state(), strategy_data()) -> proper_types:type().
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

-spec init_strategy(opts()) -> ok.
init_strategy(#{search_steps := Steps, search_strategy := Strat}) ->
  Strategy = strategy(Strat),
  proper_gen_next:init(),
  Data = Strategy:init_strategy(Steps),
  Args = [{Strategy, Data}],
  {ok, TargetserverPid} = gen_server:start_link(?MODULE, Args, []),
  put('$targetserver_pid', TargetserverPid),
  update_pdict(),
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

%% @private
-spec targeted(proper_types:type()) -> proper_types:type().
targeted(RawType) ->
  Type = proper_types:cook_outer(RawType),
  TargetedType = ?SHRINK(proper_types:exactly(?LAZY(targeted_gen())),
                         [get_shrinker(Type)]),
  case proper_types:find_prop(user_nf, Type) of
    {ok, _} -> proper_types:add_prop(is_user_nf, true, TargetedType);
    error -> proper_types:add_prop(is_user_nf, false, TargetedType)
  end.

%% Update the gen_server's process dictionary with some of
%% PropEr's values in its process dictionary.

update_pdict() ->
  TargetserverPid = get('$targetserver_pid'),
  gen_server:call(TargetserverPid, {update_pdict, get()}).

-spec update_pdict([atom()]) -> ok.
update_pdict(Keys) ->
  update_pdict(Keys, []).

update_pdict([], KVs) ->
  TargetserverPid = get('$targetserver_pid'),
  gen_server:call(TargetserverPid, {update_pdict, KVs});
update_pdict([Key | Keys], KVs) ->
  update_pdict(Keys, [{Key, get(Key)} | KVs]).

%% @doc Initialize the target of the strategy.

-spec init_target(proper_types:type()) -> ok.
init_target(RawType) ->
  update_pdict(['$left', '$size']),
  Type = proper_types:cook_outer(RawType),
  case proper_types:find_prop(is_user_nf_stateful, Type) of
    {ok, true} -> init_stateful();
    {ok, false} -> ok;
    error -> ok
  end,
  TargetserverPid = get('$targetserver_pid'),
  safe_call(TargetserverPid, {init_target, Type}).

%% Initialize stateful configuration.

%% @private
-spec init_stateful() -> ok.
init_stateful() ->
  TargetserverPid = get('$targetserver_pid'),
  safe_call(TargetserverPid, init_stateful).

%% This produces the next gen instance from the next
%% generator provided by the strategy. It will also
%% update the state and data of the strategy.

%% @private
-spec targeted_gen() -> any().
targeted_gen() ->
  update_pdict(['$left', '$size']),
  TargetserverPid = get('$targetserver_pid'),
  gen_server:call(TargetserverPid, gen).

%% @doc Get the shrinker for a Type.

-spec get_shrinker(proper_types:type()) -> proper_types:type().
get_shrinker(Type) ->
  TargetserverPid = get('$targetserver_pid'),
  try
    gen_server:call(TargetserverPid, shrinker)
  catch
    _:{noproc, _} ->
      Type
  end.

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
  catch
    _:{noproc, _} ->
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

%% @private
get_stateful_cmds({'$used', Used, {Weights, Cmds}})
  when is_map(Weights), is_list(Cmds) ->
  get_stateful_cmds(Used);
get_stateful_cmds(Cmds) -> Cmds.

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
  #state{strategy = Strat, target = Target, data = Data} = State,
  {NextValue, NewTarget, NewData} = Strat:next(Target, Data),
  Ret = case State#state.stateful of
          true -> get_stateful_cmds(NextValue);
          false -> NextValue
        end,
  {reply, Ret, State#state{target = NewTarget, data = NewData}};

handle_call(shrinker, _From, State) ->
  #state{strategy = Strat, target = Target, data = Data} = State,
  Shrinker = Strat:get_shrinker(Target, Data),
  {reply, Shrinker, State};

handle_call({init_target, Type}, _From, State) ->
  #state{strategy = Strat} = State,
  NextFun = proper_gen_next:from_proper_generator(Type),
  NewTarget = Strat:init_target(Type, NextFun),
  {reply, ok, State#state{target = NewTarget}};

handle_call({update_pdict, KVs}, _From, State) ->
  lists:foreach(fun ({K, V}) -> put(K, V) end, KVs),
  {reply, ok, State};

handle_call(init_stateful, _From, State) ->
  {reply, ok, State#state{stateful = true}};

handle_call({update_fitness, Fitness}, _From, State) ->
  #state{strategy = Strat, target = Target, data = Data} = State,
  {NewTarget, NewData} = Strat:update_fitness(Fitness, Target, Data),
  {reply, ok, State#state{target = NewTarget, data = NewData}};

handle_call(reset, _From, State) ->
  #state{strategy = Strat, target = Target, data = Data} = State,
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
