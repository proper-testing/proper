-module (reminder_statem).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

-define(SERVER, evserv).
-define(EVENT, event).
-define(NOW,{{2011,2,14},{12,12,12}}).

-record(state, {events,        %% list of active #event{} records
                clients,       %% list of Pids subscribed as clients
		completed      %% list of completed events 
	       }).

-record(event, {name="",
		pid,
		description="",
		timeout={{1970,1,1},{0,0,0}}}).

test() ->
    proper:quickcheck(?MODULE:prop_reminder()).  

stop() ->
    ?SERVER:terminate().    

clean_up(S) ->
    lists:foreach(fun(Name) -> ?SERVER:cancel(Name) end,  
		  orddict:fetch_keys(S#state.events)),
    flush(),
    stop().   

flush() ->
    receive
	_ -> 
	    flush()
    after 0 -> 
	    ok
    end. 

prop_reminder() ->
    ?FORALL(Cmds,commands(?MODULE),
       ?TRAPEXIT(
	  begin
	      ?SERVER:start_link(),
	      {ok,_Ref} = ?SERVER:subscribe(self()),
	      {H,S,Res} = run_commands(?MODULE,Cmds),
	      clean_up(S),
	      ?WHENFAIL(io:format("History: ~w\nState: ~w\nRes: ~w\n", [H,S,Res]),
			Res == ok)
	   %aggregate(command_names(Cmds),Res == ok)
	  end)).

initial_state() ->
    #state{events=orddict:new(),
	   clients=[{call,erlang,self,[]}],
	   completed=[]
	  }.

command(_S) ->
    oneof([{call, ?SERVER, add_event, 
	    [name(),description(),date_time()]},
	   {call, ?SERVER, cancel, [name()]}
	  ]).

new_pid() -> spawn(timer,sleep,[0]).

name() -> %?SUCHTHAT(S, string(), S=/="").
    elements(["a","b","c","d","e","f","g", "e",
	      "a1","b1","c1","d1","e1","f1","g1", "e1",
	      "a2","b2","c2","d2","e2","f2","g2", "e2",
	      "a3","b3","c3","d3","e3","f3","g3", "e3"]).

description() ->
    "".


date_time() ->
    ?LET({Date,{H,Min,Sec}},?EVENT:now(), 
	 {Date,{range(H-2,H+2),range(Min-2,Min+2),range(Sec-2,Sec+2)}}).

next_state(S,_V,{call,_,add_event,[Name,D,TimeOut]}) ->
    Dict = 
	case ?SERVER:valid_datetime(TimeOut) of
	    true ->
		case orddict:is_key(Name,S#state.events) of
		    false ->
			Event =  #event{name=Name,
					pid=new_pid(),
					description=D,
					timeout=TimeOut},
			orddict:store(Name,Event,S#state.events); 
		    true ->
			S#state.events
		end;
	    false ->
		S#state.events
	end,
    NewDict = orddict:filter(fun (_Name,X) -> not is_past(X#event.timeout) end,
			     Dict),
    Done = orddict:filter(fun (_Name,X) -> is_past(X#event.timeout) end,
			     Dict),
    S#state{events=NewDict, 
	    completed=orddict:fetch_keys(Done) ++ S#state.completed};
next_state(S,_V,{call,_,cancel,[Name]}) ->
    Dict = orddict:filter(fun (_Name, X) -> not is_past(X#event.timeout) end,
			  S#state.events),
    Done = orddict:filter(fun (_Name,X) -> is_past(X#event.timeout) end,
			  S#state.events),
    case orddict:find(Name, Dict) of
	{ok, _E} ->
	    S#state{events=orddict:erase(Name, Dict),
		    completed=[Done|S#state.completed]};
	error ->
	    S#state{events=Dict,  
		    completed=orddict:fetch_keys(Done) ++ S#state.completed}
    end.
%next_state(S,_V,{call,_,listen,[_Delay]}) ->
%    Dict = orddict:filter(fun (_Name, X) -> not is_past(X#event.timeout) end,
%			  S#state.events),
%    Done = orddict:filter(fun (_Name,X) -> is_past(X#event.timeout) end,
%			  S#state.events),
%    S#state{events=Dict, completed=orddict:fetch_keys(Done) ++ S#state.completed}.

precondition(_,_) ->		  
    true.

postcondition(S,{call,_,add_event,[Name,_Descr,Timeout]},Res) ->
    case Res of
	{error,name_clash} ->
	    orddict:is_key(Name,S#state.events);
	{error,bad_timeout} ->
	    not ?SERVER:valid_datetime(Timeout);
	ok ->
	    not orddict:is_key(Name,S#state.events);
	_Other ->  
	    false
    end;
postcondition(_S,{call,_,cancel,_},Res) ->
    Res == ok.
%postcondition(S,{call,_,listen,[_Delay]},Res) -> 
   % io:format("~nDict: ~w~nRes: ~w~n", [S#state.completed, 
%					Res]),
%    lists:all(fun(X) ->  lists:member(X,S#state.completed) end,
%	      get_names(Res)).

get_names(L) ->
    lists:map( fun({done,Name,_}) -> Name end, L).			  

spawn() ->
    spawn(timer,sleep,[5000]).

is_past(Timeout) ->
    Dif = calendar:datetime_to_gregorian_seconds(Timeout) -
	calendar:datetime_to_gregorian_seconds(?EVENT:now()),
    Dif =< 0.

