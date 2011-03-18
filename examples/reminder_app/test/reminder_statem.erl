-module (reminder_statem).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

-define(SERVER, evserv).
-define(EVENT, event).
-define(NOW, ?EVENT:now()).

-record(state, {events,        %% list of active #event{} records
                clients        %% list of Pids subscribed as clients
	       }).

-record(event, {name="",
		pid,
		description="",
		timeout={{1970,1,1},{0,0,0}}}).

test() ->
    test(500).

test(N) ->
    proper:quickcheck(?MODULE:prop_reminder(), N).  

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
    ?FORALL(Cmds, commands(?MODULE),
	    ?TRAPEXIT( 
	       begin
		   ?SERVER:start_link(),
		   {ok,_Ref} = ?SERVER:subscribe(self()),
		   {H,S,Res} = run_commands(?MODULE, Cmds),
		   clean_up(S),
		   ?WHENFAIL(io:format("History: ~w\nState: ~w\nRes: ~w\n", [H,S,Res]),
			     Res == ok)
						%aggregate(command_names(Cmds),Res == ok)
	       end)).

initial_state() ->
    #state{events=orddict:new(),
	   clients=[{call,erlang,self,[]}]
	  }.

command(_S) ->
    oneof([{call, ?SERVER, add_event, [name(),description(),date_time()]},
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
    ?LET({Date,{H,Min,Sec}},?NOW, 
	 {Date,{range(H-1, H+2),range(Min-1, Min+2),range(Sec-1, Sec+2)}}).

next_state(S, _V, {call,_,add_event,[Name,D,TimeOut]}) ->
    Dict = case ?SERVER:valid_datetime(TimeOut) of
	       true ->
		   case orddict:is_key(Name, S#state.events) of
		       false ->
			   Event =  #event{name=Name,
					   pid=new_pid(),
					   description=D,
					   timeout=TimeOut},
			   orddict:store(Name, Event, S#state.events); 
		       true ->
			   S#state.events
		   end;
	       false ->
		   S#state.events
	   end,
    NewDict = orddict:filter(fun(_Name, X) -> not is_past(X#event.timeout) end, Dict),
    S#state{events=NewDict}; 
next_state(S, _V, {call,_,cancel,[Name]}) ->
    Dict = case orddict:find(Name, S#state.events) of
	       {ok,_E} ->
		   orddict:erase(Name, S#state.events);
	       error ->
		   S#state.events
	   end,
    NewDict = orddict:filter(fun(_Name, X) -> not is_past(X#event.timeout) end, Dict),
    S#state{events=NewDict}.

precondition(_,_) ->		  
    true.

postcondition(S, {call,_,add_event,[Name, _Descr, Timeout]}, Res) ->
    case Res of
	{error,name_clash} ->
	    orddict:is_key(Name, S#state.events);
	{error,bad_timeout} ->
	    not ?SERVER:valid_datetime(Timeout);
	ok ->
	    not orddict:is_key(Name, S#state.events);
	_Other ->  
	    false
    end;
postcondition(_S, {call,_,cancel,_}, Res) ->
    Res == ok.

get_names(L) ->
    lists:map(fun({done,Name,_}) -> Name end, L).			  

spawn() ->
    spawn(timer, sleep, [5000]).

is_past(Timeout) ->
    Dif = calendar:datetime_to_gregorian_seconds(Timeout) -
	calendar:datetime_to_gregorian_seconds(?NOW),
    Dif =< 0.

