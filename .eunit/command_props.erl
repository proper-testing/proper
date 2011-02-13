-module(command_props).

-include_lib("proper/include/proper.hrl").

lofl_check(Lofl, NumLists, ListLen, ListElems) ->
    lofl_check(Lofl, NumLists, ListLen, ListElems, 0).

lofl_check([], NumLists, _ListLen, _ListElems, Acc) ->
    Acc =:= NumLists;
lofl_check([List|Rest], NumLists, ListLen, ListElems, Acc) ->
    list_check(List, ListLen, ListElems)
    andalso lofl_check(Rest, NumLists, ListLen, ListElems, Acc + 1).

list_check([], 0, _Elems) ->
    true;
list_check([], _Left, _Elems) ->
    false;
list_check([X|Rest], Left, Elems) ->
    lists:member(X, Elems)
    andalso list_check(Rest, Left - 1, Elems).

pow(X, Y) ->
    pow_tr(X, Y, 1).

pow_tr(_X, 0, Acc) ->
    Acc;
pow_tr(X, Y, Acc) ->
    pow_tr(X, Y - 1, X * Acc).

no_duplicates(L) -> length(L) =:= length(lists:usort(L)).

short_list(ElemType) ->
    resize(10, list(ElemType)).

short_ne_list(ElemType) ->
    non_empty(short_list(ElemType)).

short_ne_nd_list(ElemType) ->
    ?LET(L,
	 resize(7, non_empty(list(ElemType))),
	 lists:usort(L)).

num_sels(N, Len) ->
    fact(Len) div fact(N) div fact(Len - N).

fact(0) ->
    1;
fact(N) when N >= 1 ->
    N * fact(N-1).

prop_all_selections_are_produced() ->
    ?FORALL(List,
	    short_ne_list(integer()),
	    begin
		Len = length(List),
		?FORALL(N,
			range(0,Len),
			begin
			    AllSels = proper_statem:all_selections(N, List),
			    NumAllSels = num_sels(N, Len),
			    lofl_check(AllSels, NumAllSels, N, List)
			end)
	    end).

prop_index() ->
    ?FORALL(List, short_ne_nd_list(integer()),
	    ?FORALL(X, union(List), 
		    lists:nth(proper_statem:index(X,List),List) =:= X)).

prop_all_insertions() ->
     ?FORALL(List, short_list(integer()),
        begin
	    Len = length(List),
	    ?FORALL(Limit, range(1,Len+1),
		    ?FORALL(X, integer(),
		       begin
			   AllIns = proper_statem:all_insertions(X,Limit,List),
			   length(AllIns) =:= Limit
		       end))
	end
	    ).

prop_insert_all() ->
    ?FORALL(List, short_ne_nd_list(integer()),
       begin
	   Len = length(List),
	   {L1,L2} = lists:split(Len div 2, List),
	   AllIns = proper_statem:insert_all(L1,L2), 
	   ?WHENFAIL(io:format("~nList: ~w, L1: ~w, L2: ~w~nAllIns: ~w~n",
			       [List,L1,L2,AllIns]), 
		     lists:all( fun(L) -> 
					length(L)=:=Len andalso no_duplicates(L)
			                andalso lists:subtract(L,L2) =:= L1
				end, AllIns))
       end).
	   
	   
	       


    
		   
