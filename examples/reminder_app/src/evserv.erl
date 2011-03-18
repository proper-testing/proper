-module(evserv).

-compile(export_all).

-record(state, {events,    %% list of #event{} records
                clients}). %% list of Pids

-record(event, {name="",
                description="",
		pid,
		timeout={{1970,1,1},{0,0,0}}}).

start() ->
    register(?MODULE, Pid=spawn(fun  init/0)),
    Pid.
 
start_link() ->
    register(?MODULE, Pid=spawn_link(fun init/0)),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} ->
	    {ok, Ref};
	{'DOWN', Ref, process, _Pid, Reason} ->
	    {error, Reason}
    after 5000 ->
	    {error, timeout}
    end.

add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
	    {error, timeout}
    end.

cancel(Name) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {cancel, Name}},
    receive
        {Ref, ok} -> ok
    after 5000 ->
	    {error, timeout}
    end.

listen(Delay) ->
    receive
        M = {done, _Name, _Description} ->
	    [M | listen(0)]
    after Delay*1000 ->
	    []
    end.

%% Server side

init() ->
    loop(#state{events=orddict:new(),
                clients=orddict:new()}).

loop(S = #state{}) ->
    receive
	{done, Name} ->
	    %% E = orddict:fetch(Name, S#state.events),
	    case orddict:find(Name, S#state.events) of
		{ok,E} ->
		    send_to_clients({done, E#event.name, E#event.description},
				    S#state.clients),
		    NewEvents = orddict:erase(Name, S#state.events),
		    loop(S#state{events=NewEvents});
		error ->
		    loop(S)
	    end;
	{Pid, MsgRef, {subscribe, Client}} ->
	    Ref = erlang:monitor(process, Client),
	    NewClients = orddict:store(Ref, Client, S#state.clients),
	    Pid ! {MsgRef, ok},
	    loop(S#state{clients=NewClients});

	{Pid, MsgRef, {add, Name, Description, TimeOut}} ->
	    case valid_datetime(TimeOut) of
		true ->
		    case orddict:is_key(Name,S#state.events) of
			false ->
			    EventPid = event:start_link(Name, TimeOut),
			    NewEvents = 
				orddict:store(Name,
					      #event{name=Name,
						     description=Description,
						     pid=EventPid,
						     timeout=TimeOut},
					      S#state.events),
			    Pid ! {MsgRef, ok},
			    loop(S#state{events=NewEvents});
			true ->
			    Pid ! {MsgRef, {error, name_clash}},
			    loop(S)
		    end;
		false ->
		    Pid ! {MsgRef, {error, bad_timeout}},
		    loop(S)
	    end;
	{Pid, MsgRef, {cancel, Name}} ->
	    Events = case orddict:find(Name, S#state.events) of
			 {ok, E} ->
			     event:cancel(E#event.pid),
			     orddict:erase(Name, S#state.events);
			 error ->
			     S#state.events
		     end,
	    Pid ! {MsgRef, ok},
	    loop(S#state{events=Events});
	shutdown ->
	    %% exit(shutdown);
	    ok;
	{'DOWN', Ref, process, _Pid, _Reason} ->
	    loop(S#state{clients=orddict:erase(Ref, S#state.clients)});
	Unknown ->
	    io:format("Unknown message: ~p~n",[Unknown]),
	    loop(S)
    end.

send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

valid_datetime({Date,Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause -> % not in {{D,M,Y},{H,Min,S}} format
	    false
    end;
valid_datetime(_) ->
    false.
 
valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24,
                       M >= 0, M < 60,
                       S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.
