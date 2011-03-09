-module(command_props).

-include_lib("proper/include/proper.hrl").

ne_nd_list(ElemType) ->
    ?LET(L,
	 non_empty(list(ElemType)),
	 lists:usort(L)).

short_ne_nd_list(ElemType) ->
    ?LET(L,
	 resize(8, non_empty(list(ElemType))),
	 lists:usort(L)).

no_duplicates(L) -> length(L) =:= length(lists:usort(L)).

prop_index() ->
    ?FORALL(List, ne_nd_list(integer()),
	    ?FORALL(X, union(List), 
		    lists:nth(proper_statem:index(X,List),List) =:= X)).

prop_all_insertions() ->
     ?FORALL(List, list(integer()),
        begin
	    Len = length(List),
	    ?FORALL(Limit, range(1,Len+1),
		    ?FORALL(X, integer(),
		       begin
			   AllIns = proper_statem:all_insertions(X,Limit,List),
			   length(AllIns) =:= Limit
		       end))
	end).

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

prop_zip() ->	       
    ?FORALL({X,Y}, {list(),list()},
	    begin
		LenX = length(X),
		LenY = length(Y),
		Res = if LenX < LenY -> 
			      lists:zip(X, lists:sublist(Y, LenX));
			 LenX =:= LenY ->
			      lists:zip(X, Y);
			 LenX > LenY -> 
			      lists:zip(lists:sublist(X, LenY), Y)
		      end,
		equals(zip(X, Y), Res)
	    end).

%% -define(MOD, reg_parallel).

%% prop_p() ->
%%     ?FORALL(Workers, 3,
%% 	    ?FORALL(CmdList, ?SUCHTHAT(X, resize(16, commands(?MOD)), length(X) >= Workers),
%% 		    begin
%% 			N = length(CmdList),
%% 			Len = N div Workers,
%% 			Comb = proper_statem:mk_first_comb(N, Len, Workers),
%% 			LookUp =  orddict:from_list(proper_statem:mk_dict(CmdList,1)),
%% 			State = ?MOD:initial_state(),
%% 			Res =
%% 			    proper_statem:fix_gen(N, Len, Comb, LookUp, ?MOD, State, []), 
%% 			?WHENFAIL(io:format("CmdList: ~w\nResult: ~w\n", [CmdList, Res]),
%% 				  length(Res) =:= Workers) 
%% 		    end)).
	
    
		   
