-module(event).

-compile(export_all).
-record(state, {server,
		name="",
		to_go=0}).

%%-define(NOW, {{2011,2,14},{12,12,12}}).
-define(NOW, calendar:local_time()).

start(EventName, Delay) ->
    spawn(?MODULE, init, [self(), EventName, Delay]).
 
start_link(EventName, Delay) ->
    spawn_link(?MODULE, init, [self(), EventName, Delay]).
 
%%% Event's innards
init(Server, EventName, DateTime) ->
    loop(#state{server=Server,
                name=EventName,
		to_go=time_to_go(DateTime)}).

cancel(Pid) ->
    %% Monitor in case the process is already dead
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
	    erlang:demonitor(Ref, [flush]),
	    ok;
	{'DOWN', Ref, process, Pid, _Reason} ->
	    ok
    end.

loop(S = #state{server=Server, to_go=[T|Next]}) ->
    receive
	{Server, Ref, cancel} ->
	    Server ! {Ref,ok}
    after T*1000 ->
	if Next =:= [] ->
	    Server ! {done, S#state.name};
	   Next =/= [] ->
	    loop(S#state{to_go=Next})
	end
    end.

normalize(N) ->
    Limit = 49*24*60*60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].

time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
   Now = ?NOW,
   ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
          calendar:datetime_to_gregorian_seconds(Now),
   Secs = if ToGo > 0  -> ToGo;
             ToGo =< 0 -> 0
          end,
   normalize(Secs).

now() -> ?NOW.
