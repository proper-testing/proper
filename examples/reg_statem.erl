-module(reg_statem).
-include_lib("proper/include/proper.hrl").

-compile(export_all).
-record(state,{pids=[], % pids spawned in this test, not registered right now
	       regs=[]  % list of {Name,Pid} in the registry
	      }).

%% Initialize the state
initial_state() ->
    #state{}.

%% Command generator
command(S) ->
    oneof([{call,erlang,register,[name(),pid(S)]} || S#state.pids=/=[]] ++
	  [{call,?MODULE,unregister,[name()]},
	  {call,erlang,whereis,[name()]},
	  {call,?MODULE,spawn,[]}
	  ]).

-define(names,[a,b,c,d]).
name() ->
    elements(?names).

pid(S) ->
    elements(S#state.pids).

%% Next state transformation, S is the current state
next_state(S,V,{call,_,spawn,_}) ->
    S#state{pids=[V|S#state.pids]};
next_state(S,_V,{call,_,register,[Name,Pid]}) ->
   % S#state{pids=lists:delete(Pid,S#state.pids),
   % regs=[{Name,Pid}|S#state.regs]};
    S#state{regs=[{Name,Pid}|S#state.regs]};    
next_state(S,_V,{call,_,unregister,[Name]}) ->
    %case proplists:get_value(Name,S#state.regs) of
%	undefined -> 
%	    S;
%	Pid ->
%	    S#state{pids=[Pid|S#state.pids],regs=proplists:delete(Name,S#state.regs)}
%    end;
    S#state{regs=proplists:delete(Name,S#state.regs)};
next_state(S,_V,{call,_,_,_}) -> S.

%% Precondition, checked before command is added to the command sequence
%precondition(S,{call,_,register,[Name,_Pid]}) ->
%    not proplists:is_defined(Name,S#state.regs);

precondition(_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state(S,_,<command>)
postcondition(S,{call,_,unregister,[Name]},Res) ->
    case Res of
	{'EXIT',_} ->
	    not unregister_ok(S,Name);
	true ->
	    unregister_ok(S,Name)
    end;
%postcondition(_S,{call,_,whereis,[d]},_Res) ->
%    false;
postcondition(_S,{call,_,_,_},_Res) ->
    true.

%% The conditions under which operations ought to succeed.
unregister_ok(S,Name) ->
    proplists:is_defined(Name,S#state.regs).

%% The main property.
prop_registry() ->
    ?FORALL(Cmds,commands(?MODULE),
	    begin
		{H,S,Res} = run_commands(?MODULE,Cmds),
		cleanup(),
		?WHENFAIL(
		   io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
		   Res == ok)
	    end).

cleanup() ->
    [catch erlang:unregister(Name) || Name <- ?names].

%% Exception-catching versions of the API under test
unregister(Name) ->
    catch erlang:unregister(Name).

%% Spawn a dummy process to use as test data. Processes die after 5
%% seconds, long after the test is over, to avoid filling the Erlang
%% heap with dummy processes.
spawn() ->
    spawn(timer,sleep,[5000]).


