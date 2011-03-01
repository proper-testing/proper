-module(reg_parallel).
-include_lib("proper/include/proper.hrl").

-compile(export_all).
-record(state,{pids=[],regs=[]}).


test() ->
    test(100).

test(NumTests) ->
    timer:tc(proper,quickcheck,[?MODULE:prop_reg_parallel(),NumTests]).

%% Initialize the state
initial_state() ->
    #state{}.

%% Command generator
command(S) ->
    oneof([{call,?MODULE,spawn,[]}] ++
	  [{call,?MODULE,catch_register,[name(),elements(S#state.pids)]}
	   || S#state.pids =/= []] ++
	  [{call,?MODULE,catch_unregister,[name()]}] ++
	  [{call,erlang,whereis,[name()]}]).

-define(names,[a,b,c,d]).
name() ->
    elements(?names).

%% Next state transformation, S is the current state
next_state(S,V,{call,_,spawn,_}) ->
    S#state{pids=[V|S#state.pids]};
next_state(S,_V,{call,_,catch_register,[Name,Pid]}) ->
    case register_ok(S,Name,Pid) of
	true ->
	    S#state{regs=[{Name,Pid}|S#state.regs]};
	false ->
	    S
    end;
next_state(S,_V,{call,_,catch_unregister,[Name]}) ->
    S#state{regs=lists:keydelete(Name,1,S#state.regs)};
next_state(S,_V,{call,_,whereis,[_]}) ->
    S.

register_ok(S,Name,Pid) ->
    not lists:keymember(Name,1,S#state.regs) andalso
	not lists:keymember(Pid,2,S#state.regs).


%% Precondition, checked before command is added to the command sequence
precondition(_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state(S,_,<command>)
postcondition(S,{call,_,catch_register,[Name,Pid]},Res) ->
    case Res of
	true ->
	    register_ok(S,Name,Pid);
	{'EXIT',_} -> 
	    not register_ok(S,Name,Pid)
    end;
 
postcondition(S,{call,_,catch_unregister,[Name]},Res) ->
    case Res of
	true ->
	    unregister_ok(S,Name);
	{'EXIT',_} ->
	    not unregister_ok(S,Name)
    end;
postcondition(S,{call,_,whereis,[Name]},Res) ->
    case Res of
	undefined ->
	    not lists:keymember(Name,1,S#state.regs);
	_R ->
	    lists:member({Name,Res},S#state.regs)
    end;
postcondition(_S,{call,_,_,_},_Res) ->
    true.

unregister_ok(S,Name) ->
    lists:keymember(Name,1,S#state.regs).


%% The main property.
prop_reg_parallel() ->
    ?FORALL(Cmds,parallel_commands(?MODULE),
	    begin
		{Seq,P,Res} = run_parallel_commands(?MODULE,Cmds),
		clean_up(),
		?WHENFAIL(io:format("Seq:~w~nPar:~w~nRes:~w~n", [Seq,P,Res]),
			  Res =:= ok)
	    end).


clean_up() ->
    [catch_unregister(Name) || Name <- ?names].

%% Exception-catching versions of the API under test
catch_unregister(Name) ->
    catch erlang:unregister(Name).

catch_register(Name,Pid) ->
    catch erlang:register(Name,Pid).

%% Spawn a dummy process to use as test data. Processes die after 5
%% seconds, long after the test is over, to avoid filling the Erlang
%% heap with dummy processes.
spawn() ->
    spawn(timer,sleep,[5000]).


