-module(pdict_statem).
-export([prop_pdict/0]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
	next_state/3]).

-include_lib("proper/include/proper.hrl").

%%
%% A simple statem test for the process dictionary; tests the
%% operations erlang:put/2, erlang:get/1 and and erlang:erase/1.
%%

-define(KEYS, [a,b,c,d]).
prop_pdict() ->
    ?FORALL(Cmds,more_commands(2,commands(?MODULE)),
	    ?FORALL([A,B,C,D],[integer(),integer(),integer(),integer()],
		    begin
			lists:map(fun({K,V})->erlang:put(K,V) end,
				  [{a,A},{b,B},{c,C},{d,D}]),
			{_H,_S,Res} = proper_statem:run_commands(?MODULE,Cmds,[{x,42}]),
			lists:map(fun(Key) -> erlang:erase(Key) end,?KEYS),
			%?WHENFAIL(io:format("History: ~p\nState: ~p\nRes: ~p\n",
			%		    [_H,_S,Res]),
			%	  Result == ok)
			aggregate(command_names(Cmds),Res == ok)
		    end)).

key() ->
    oneof(?KEYS).

initial_state() -> lists:filter(fun({Key,_}) -> lists:member(Key, ?KEYS) end,
                                 erlang:get()).

command([]) ->
    {call, erlang, put, [key(), integer()]};
command(Props) ->
    ?LET({Key,Value}, frequency([{5, elements(Props)},
				 {1, {key(),integer()}},
				 {10, {key(),{var,x}}}]),
	 oneof([{call, erlang, put,   [Key, Value]},
		{call, erlang, get,   [Key]},
		{call, erlang, erase, [Key]}
	       ])).

precondition(_, {call, erlang, put, [_,_]}) ->
    true;
precondition(Props, {call, erlang, get, [Key]}) ->
    proplists:is_defined(Key,Props);
precondition(Props, {call, erlang, erase, [Key]}) ->
    proplists:is_defined(Key,Props);
precondition(_,_) ->
    false.

postcondition(Props, {call, erlang, put, [Key,_]}, undefined) ->
    not proplists:is_defined(Key,Props);
postcondition(Props, {call, erlang, put, [Key,_]}, Old) ->
    [{Key,Old}] =:= proplists:lookup_all(Key,Props);
postcondition(Props, {call, erlang, get, [Key]}, Val) ->
    {Key,Val} =:= proplists:lookup(Key,Props);
postcondition(Props, {call, erlang, erase, [Key]}, Val) ->
    {Key,Val} =:= proplists:lookup(Key,Props);
postcondition(_,_,_) ->
    false.

next_state(Props, _Var, {call, erlang, put, [Key,Value]}) ->
    %% correct model
    [{Key,Value}| proplists:delete(Key,Props)];
    %% wrong model
    %[{Key,Value}| Props];
next_state(Props, _Var, {call, erlang, erase, [Key]}) ->
    proplists:delete(Key,Props);
next_state(Props, _Var, {call, erlang, get, [_]}) ->
    Props.
