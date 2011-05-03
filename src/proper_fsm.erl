%%% Copyright 2010-2011 Manolis Papadakis <manopapad@gmail.com>,
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

%%% @copyright 2010-2011 Manolis Papadakis <manopapad@gmail.com>,
%%%                      Eirini Arvaniti <eirinibob@gmail.com>
%%%                  and Kostis Sagonas <kostis@cs.ntua.gr>
%%% @version {@version}
%%% @author Eirini Arvaniti <eirinibob@gmail.com>
%%% @doc This module contains functions for testing finite state machine
%%%      specifications.

-module(proper_fsm).
-export([commands/1, commands/2]).
-export([run_commands/2, run_commands/3]).
-export([state_names/1]).
-export([analyze/1]).

-export([initial_state/1, command/1, precondition/2, next_state/3,
	 postcondition/3]).
-export([target_states/4]).

-include("proper_internal.hrl").
-define(TIMES, 100000).


%% -----------------------------------------------------------------------------
%% Type declarations
%% -----------------------------------------------------------------------------

-type symb_var()   :: proper_statem:symb_var().
-type symb_call()  :: proper_statem:symb_call().
-type fsm_result() :: proper_statem:statem_result().

-type state_name()       :: atom() | tuple().
-type state_data()       :: term().
-type fsm_state()        :: {state_name(),state_data()}.
-type symb_call_gen()    :: {'call',mod_name(),fun_name(),
			     [proper_types:type()]}.
-type transition_gen()   :: {state_name(),symb_call_gen()}.
-type result()           :: term().
-type command()          ::   {'init',fsm_state()}
		            | {'set',symb_var(),symb_call()}.
-type command_list()     :: [command()].
-type history()          :: [{fsm_state(),result()}].
-type tmp_command()      ::   {'init',state()}
		            | {'set',symb_var(),symb_call()}.
-type transition()       :: {state_name(), state_name(), mfa()}.
-type flow()             :: {float(), transition()}.

-record(state, {name :: state_name(),
		data :: state_data(),
		mod  :: mod_name()}).
-type state() :: #state{}.

-record (state_info, {prob        :: float(),
		      transitions :: [{transition_gen(),pos_integer(),float()}],
		      norm        :: float()}).
-type state_info() :: #state_info{}.


%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

-spec commands(mod_name()) -> proper_types:type().
commands(Module) ->
    ?LET([_|Cmds],
	 proper_statem:commands(?MODULE, initial_state(Module)),
	 Cmds).

-spec commands(mod_name(), fsm_state()) -> proper_types:type().
commands(Module, {Name,Data} = InitialState) ->
    State = #state{name = Name, data = Data, mod = Module},
    ?LET([_|Cmds],
	 proper_statem:commands(?MODULE, State),
	 [{init,InitialState}|Cmds]).

-spec run_commands(mod_name(), command_list()) ->
         {history(),fsm_state(),fsm_result()}.
run_commands(Module, Cmds) ->
    run_commands(Module, Cmds, []).

-spec run_commands(mod_name(), command_list(), proper_symb:var_values()) ->
         {history(),fsm_state(),fsm_result()}.
run_commands(Module, Cmds, Env) ->
    Cmds1 = tmp_commands(Module, Cmds),
    {H,S,Res} = proper_statem:run_commands(?MODULE, Cmds1, Env),
    History = [{{Name,Data},R} || {#state{name = Name, data = Data},R} <- H],
    State = {S#state.name, S#state.data},
    {History, State, Res}.

-spec state_names(history()) -> [state_name()].
state_names(History) ->
    [SName || {{SName,_},_Res} <- History].


%% -----------------------------------------------------------------------------
%% State machine specification for fsm commands
%% -----------------------------------------------------------------------------

%% @private
-spec initial_state(mod_name()) -> state().
initial_state(Mod) ->
    S_name = Mod:initial_state(),
    S_data = Mod:initial_state_data(),
    #state{name = S_name, data = S_data, mod = Mod}.

%% @private
-spec command(state()) -> proper_types:type().
command(#state{name = From, data = Data, mod = Mod}) ->
    choose_transition(Mod, From, get_transitions(Mod, From, Data)).

%% @private
-spec precondition(state(), symb_call()) -> boolean().
precondition(#state{name = From, data = Data, mod = Mod}, Call) ->
    Targets = target_states(Mod, From, Data, Call),
    case [To || To <- Targets,
		Mod:precondition(From, cook_history(From, To), Data, Call)] of
	[]   -> false;
	[_T] -> true
    end.

%% @private
-spec next_state(state(), symb_var() | result(), symb_call()) -> state().
next_state(S = #state{name = From, data = Data, mod = Mod} , Var, Call) ->
    To = cook_history(From, transition_target(Mod, From, Data, Call)),
    S#state{name = To,
	    data = Mod:next_state_data(From, To, Data, Var, Call)}.

%% @private
-spec postcondition(state(), symb_call(), result()) -> boolean().
postcondition(#state{name = From, data = Data, mod = Mod}, Call, Res) ->
    To = cook_history(From, transition_target(Mod, From, Data, Call)),
    Mod:postcondition(From, To, Data, Call, Res).


%% -----------------------------------------------------------------------------
%% Utility functions
%% -----------------------------------------------------------------------------

-spec tmp_commands(mod_name(), command_list()) -> [tmp_command()].
tmp_commands(Module, Cmds) ->
    case Cmds of
	[{init, {Name,Data}}|Rest] ->
	    I = #state{name = Name, data = Data, mod = Module},
	    [{init,I}|Rest];
	Rest ->
	    I = initial_state(Module),
	    [{init,I}|Rest]
    end.

-spec get_transitions(mod_name(), state_name(), state_data()) ->
         [transition_gen()].
get_transitions(Mod, StateName, Data) ->
    case StateName of
	From when is_atom(From) ->
	    Mod:From(Data);
	From when is_tuple(From) ->
	    Fun = element(1, From),
	    Args = tl(tuple_to_list(From)),
	    apply(Mod, Fun, Args ++ [Data])
    end.

-spec choose_transition(mod_name(), state_name(), [transition_gen()]) ->
         proper_types:type().
choose_transition(Mod, From, T_list) ->
    case is_exported(Mod, {weight,3}) of
	false ->
	    choose_uniform_transition(T_list);
	true ->
	    choose_weighted_transition(Mod, From, T_list)
    end.

-spec choose_uniform_transition([transition_gen()]) -> proper_types:type().
choose_uniform_transition(T_list) ->
    List = [CallGen || {_,CallGen} <- T_list],
    safe_union(List).

-spec choose_weighted_transition(mod_name(), state_name(),
         [transition_gen()]) -> proper_types:type().
choose_weighted_transition(Mod, From, T_list) ->
    List = [{Mod:weight(From, cook_history(From, To), CallGen), CallGen}
	    || {To,CallGen} <- T_list],
    safe_weighted_union(List).

-spec cook_history(state_name(), state_name()) -> state_name().
cook_history(From, history) -> From;
cook_history(_, To)         -> To.

-spec is_exported(mod_name(), {fun_name(),arity()}) -> boolean().
is_exported(Mod, Fun) ->
    lists:member(Fun, Mod:module_info(exports)).

-spec transition_target(mod_name(), state_name(), state_data(), symb_call()) ->
         state_name().
transition_target(Mod, From, Data, Call) ->
    Targets = target_states(Mod, From, Data, Call),
    hd([T || T <- Targets,
	     Mod:precondition(From, cook_history(From, T), Data, Call)]).

%% @private
-spec target_states(mod_name(), state_name(), state_data(), symb_call()) ->
         [state_name()].
target_states(Module, From, StateData, Call) ->
    find_target(get_transitions(Module, From, StateData), Call, []).

-spec find_target([transition_gen()], symb_call(), [state_name()]) ->
         [state_name()].
find_target([], _, Accum) -> Accum;
find_target(Transitions, Call, Accum) ->
    [{Target,CallGen}|Rest] = Transitions,
    case is_compatible(Call, CallGen) of
	true  -> find_target(Rest, Call, [Target|Accum]);
	false -> find_target(Rest, Call, Accum)
    end.

-spec is_compatible(symb_call(), symb_call_gen()) -> boolean().
is_compatible({call,M,F,A1}, {call,M,F,A2})
  when length(A1) =:= length(A2) ->
    true;
is_compatible(_, _) ->
    false.


%% -----------------------------------------------------------------------------
%% Special types and generators
%% -----------------------------------------------------------------------------

%% @private
-spec safe_union([proper_types:raw_type(),...]) -> proper_types:type().
safe_union(RawChoices) ->
    Choices = [proper_types:cook_outer(C) || C <- RawChoices],
    proper_types:subtype(
      [{generator, fun() -> safe_union_gen(Choices) end}],
      proper_types:union(Choices)).

%% @private
-spec safe_weighted_union([{frequency(),proper_types:raw_type()},...]) ->
         proper_types:type().
safe_weighted_union(RawFreqChoices) ->
    CookFreqType = fun({Freq,RawType}) ->
			   {Freq,proper_types:cook_outer(RawType)} end,
    FreqChoices = lists:map(CookFreqType, RawFreqChoices),
    Choices = [T || {_F,T} <- FreqChoices],
    proper_types:subtype(
      [{generator, fun() -> safe_weighted_union_gen(FreqChoices) end}],
      proper_types:union(Choices)).

%% @private
-spec safe_union_gen([proper_types:type(),...]) -> proper_gen:imm_instance().
safe_union_gen(Choices) ->
    {Choice,Type} = proper_arith:rand_choose(Choices),
    try proper_gen:generate(Type)
    catch
	error:_ ->
	    safe_union_gen(proper_arith:list_remove(Choice, Choices))
    end.

%% @private
-spec safe_weighted_union_gen([{frequency(),proper_types:type()},...]) ->
         proper_gen:imm_instance().
safe_weighted_union_gen(FreqChoices) ->
    {Choice,Type} = proper_arith:freq_choose(FreqChoices),
    try proper_gen:generate(Type)
    catch
	error:_ ->
	    safe_weighted_union_gen(proper_arith:list_remove(Choice,
							     FreqChoices))
    end.


%% -----------------------------------------------------------------------------
%% State space analysis
%% -----------------------------------------------------------------------------

-spec analyze(mod_name()) -> [flow()].
analyze(Mod) ->
    S_name = Mod:initial_state(),
    S_data = Mod:initial_state_data(),
    Transitions = cook(Mod, S_name, get_transitions(Mod, S_name, S_data)),
    Info = #state_info{prob = 1.0,
		       transitions = Transitions,
		       norm = normalize(Transitions)},
    R = orddict:to_list(
	  update_all_states(Mod, ?TIMES,
			    orddict:store(S_name, Info, orddict:new()))),
    %%io:format("Dict: ~w\n", [R]),
    Flow = [{Prob*Pre*Weight/N, {State, Target, get_mfa(Call)}}
	    || {State, #state_info{prob=Prob, transitions=Tr, norm=N}} <- R,
	       {{Target,Call},Weight,Pre} <- Tr],
    Sum = lists:foldl(fun({F,_}, Acc) -> F + Acc end, 0, Flow),
    [{F/Sum,Transition} || {F,Transition} <- Flow].

-spec get_mfa(symb_call()) -> mfa().
get_mfa({call,M,F,Args}) -> {M,F,length(Args)}.

-spec normalize([{transition_gen(),pos_integer(),float()}]) -> float().
normalize(Transitions) ->
    lists:foldl(fun({_,W,Pre}, Accum) -> W * Pre + Accum end, 0,
		Transitions).

-spec cook(mod_name(), state_name(), [transition_gen()]) ->
		  [{transition_gen(),pos_integer(),float()}].
cook(Module, From, Transitions) ->
    case is_exported(Module, {weight,3}) of
	true ->
	    case is_exported(Module, {precondition_probability,3}) of
		true ->
		    [{{cook_history(From, To),CallGen},
		      Module:weight(From, cook_history(From, To), CallGen),
		      Module:precondition_probability(
			From, cook_history(From, To), CallGen)}
		     || {To,CallGen} <- Transitions];
		false ->
		    [{{cook_history(From, To),CallGen},
		      Module:weight(From, cook_history(From, To), CallGen),1}
		     || {To,CallGen} <- Transitions]
	    end;
	false ->
	    case is_exported(Module, {precondition_probability,3}) of
		true ->
		    [{{cook_history(From, To),CallGen},
		      1,Module:precondition_probability(
			  From, cook_history(From, To), CallGen)}
		     || {To,CallGen} <- Transitions];
		false ->
		    [{{cook_history(From, To),CallGen},1,1}
		     || {To,CallGen} <- Transitions]
	    end
    end.

-spec update_all_states(mod_name(), non_neg_integer(), orddict:orddict()) ->
			       orddict:orddict().
update_all_states(_Module, 0, Dict) -> Dict;
update_all_states(Module, N, Dict) ->
    update_all_states(Module, N-1, update_all_states(Module, Dict)).

-spec update_all_states(mod_name(), orddict:orddict()) -> orddict:orddict().
update_all_states(Module, Dict) ->
    AllStates =
	orddict:fold(
	  fun(_S, Info, Accum) ->
		  begin
		      Prior = Info#state_info.prob,
		      Norm  = Info#state_info.norm,
		      case [{To,Prior * W * P / Norm}
			    || {{To,_},W,P} <- Info#state_info.transitions,
			       To =/= history,
			       not orddict:is_key(To, Accum)] of
			  []        -> Accum;
			  NewStates -> add_states(Module, NewStates, Accum)
		      end
		  end
	  end,
	  Dict, Dict),
    orddict:map(fun(S,Info) -> set_state_prob(S, Info, AllStates) end,
		AllStates).

-spec add_states(mod_name(), [{state_name(),float()}], orddict:orddict()) ->
			orddict:orddict().
add_states(Module, States, Dict) ->
    lists:foldl(
      fun(S, Accum) -> add_state(Module, S, Accum) end,
      Dict,
      States).

-spec add_state(mod_name(), {state_name(),float()}, orddict:orddict()) ->
		       orddict:orddict().
add_state(Mod, {S,P}, Dict) ->
    Data = Mod:initial_state_data(),
    Transitions = cook(Mod, S, get_transitions(Mod, S, Data)),
    Info = #state_info{prob = P,
		       transitions = Transitions,
		       norm = normalize(Transitions)},
    orddict:store(S, Info, Dict).

-spec set_state_prob(state_name(), state_info(), orddict:orddict()) ->
			    state_info().
set_state_prob(S, S_info, AllStates) ->
    V = orddict:fold(
	  fun(_State, Info, Acc) ->
		  Prior = Info#state_info.prob,
		  Norm = Info#state_info.norm,
		  C = compute(Info#state_info.transitions, S, Norm),
		  Acc + Prior*C
	  end,
	  0,
	  AllStates),
    S_info#state_info{prob = V}.

-spec compute([{transition_gen(),pos_integer(),float()}], state_name(),
	      float()) -> float().
compute(List, S, Norm) ->
    compute_tr(List, S, Norm, 0.0).

-spec compute_tr([{transition_gen(),pos_integer(),float()}], state_name(),
		 float(), float()) -> float().
compute_tr([], _, _, C) -> C;
compute_tr([{{S,_},W,P}|Rest], S, Norm, C) ->
    compute_tr(Rest, S, Norm, C + P*W/Norm);
compute_tr([_|Rest], S, Norm, C) ->
    compute_tr(Rest, S, Norm, C).
