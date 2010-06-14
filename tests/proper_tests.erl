%%% Copyright 2010 Manolis Papadakis (manopapad@gmail.com)
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

-module(proper_tests).
-compile([export_all]).

-include("proper.hrl").


%% Functions to test

no_duplicates(L) ->
    length(lists:usort(L)) == length(L).

is_sorted([]) -> true;
is_sorted([_]) -> true;
is_sorted([A | [B|T]]) when A =< B -> is_sorted([B | T]);
is_sorted(_) -> false.

same_elements(L1, L2) ->
    length(L1) =:= length(L2) andalso same_elems(L1, L2).

same_elems([], []) ->
    true;
same_elems([H|T], L) ->
    lists:member(H, L) andalso same_elems(T, lists:delete(H, L));
same_elems(_, _) ->
    false.

correctly_sorted(Old, New) ->
    same_elements(Old, New) andalso is_sorted(New).

partition(Pivot, List) ->
    partition_tr(Pivot, List, [], []).

partition_tr(_Pivot, [], Lower, Higher) ->
    {Lower, Higher};
partition_tr(Pivot, [H|T], Lower, Higher) ->
    if
	H =< Pivot -> partition_tr(Pivot, T, [H|Lower], Higher);
	H > Pivot  -> partition_tr(Pivot, T, Lower, [H|Higher])
    end.

quicksort([]) -> [];
quicksort([H|T]) ->
    {Lower, Higher} = partition(H, T),
    quicksort(Lower) ++ [H] ++ quicksort(Higher).


%% Datatypes

uvector(0, _ElemType) ->
   [];
uvector(N, ElemType) ->
    ?LET(Rest,
	 uvector(N-1, ElemType),
	 ?LET(Elem,
	      ?SUCHTHAT(E, ElemType, not lists:member(E,Rest)),
	      [Elem | Rest])).

subset(Generators) ->
    ?LET(Keep,
	 [{boolean(),G} || G <- Generators],
	 [G || {true,G} <- Keep]).

plain_ascii_char() ->
    choose(0, 127).

data() ->
    ?LET(AsciiString,
	 list(plain_ascii_char()),
	 oneof([
	     AsciiString,
	     erlang:list_to_binary(AsciiString)
	 ])).

datalen(Data) when is_list(Data) -> length(Data);
datalen(Data) when is_binary(Data) -> size(Data).

shuffle([]) ->
    [];
shuffle(L) ->
    ?LET(X, elements(L), [X | shuffle(lists:delete(X,L))]).

nelist(ElemType) ->
    [ElemType | list(ElemType)].

ulist(ElemType) ->
    ?LET(L, list(ElemType), L--(L--lists:usort(L))).

unique(ElemTypes) ->
    ?LET(Values,
	 list(ElemTypes),
	 lists:usort(Values)).

ulist2(ElemType) ->
    ?SUCHTHAT(L, list(ElemType), no_duplicates(L)).

kvlist(KeyType, ValueType) ->
    ?LET(Keys,
	 list(KeyType),
	 [{K,ValueType} || K <- Keys]).

my_binary() ->
    ?SIZED(Size,
      	   ?LET(NrBytes,choose(0,Size),my_binary(NrBytes))).

my_binary(NrBytes) ->
    ?LET(Bytes,
	 vector(NrBytes, choose(0,255)),
	 erlang:list_to_binary(Bytes)).

lol(E) -> list(list(E)).

tree(ElemType) ->
    ?SIZED(Size, tree(ElemType,Size)).

tree(_ElemType, 0) ->
    {empty};
tree(ElemType, Size) ->
    Left = tree(ElemType, Size div 2),
    Right = tree(ElemType, Size div 2 - 1 + Size rem 2),
    frequency([
	{1, {empty}},
	{5, ?LETSHRINK([L,R], [Left,Right], {node,ElemType,L,R})}
    ]).

tree_member(_X, {node,_X,_L,_R}) -> true;
tree_member(X, {node,_Y,L,R}) -> tree_member(X, L) orelse tree_member(X, R);
tree_member(_X, {empty}) -> false.

produces_term(X) ->
    try
	_ = binary_to_term(X),
	true
    catch
	error:_ -> false
    end.


%% Various Tests

test(1) ->
    ?FORALL(Xs, list(integer()),
	    collect(length(Xs),
		    collect(Xs =:= [],
			    lists:reverse(lists:reverse(Xs)) == Xs)));
test(2) ->
    ?FORALL(Xs, list(integer()),
	    ?WHENFAIL(io:format("it failed!", []),
		      lists:reverse(Xs) == Xs));
test(3) ->
    ?FORALL(L, list(integer()), correctly_sorted(L, quicksort(L)));
test(4) ->
    ?FORALL(L, list(integer()), correctly_sorted(L, lists:sort(L)));
test(5) ->
    ?FORALL(L, list(integer()), correctly_sorted(L, lists:usort(L)));
test(6) ->
    ?FORALL(L, ulist(integer()), correctly_sorted(L, lists:usort(L)));
test(7) ->
    ?FORALL(I, integer(),
	?FORALL(L, list(integer()),
	    not lists:member(I, lists:delete(I,L))));
test(8) ->
    ?FORALL({I,L},
	    {integer(),list(integer())},
	    ?IMPLIES(no_duplicates(L),
		     not lists:member(I,lists:delete(I,L))));
test(9) ->
    ?FORALL({I,L},
	    {integer(),list(integer())},
	    collect(lists:member(I,L),
		    not lists:member(I,lists:delete(I,L))));
test(10) ->
    ?FORALL(_L,
	    shuffle(lists:seq(1,10)),
	    false);
test(11) ->
    ?FORALL(X, integer(), ?FORALL(Y, integer(), X < Y));
test(12) ->
    ?FORALL(X, integer(), ?IMPLIES(abs(X) > 1, X * X > X));
test(13) ->
    ?FORALL(L, list(integer()),
	?IMPLIES(L /= [],
	    ?FORALL(I, elements(L),
		not lists:member(I, lists:delete(I, L))
	    )
	)
    );
test(14) ->
    ?FORALL(Data, data(),
	    collect(datalen(Data),
		    begin
			Base64 = base64:decode(base64:encode(Data)),
			if
			    is_list(Data) ->
				Base64 == erlang:list_to_binary(Data);
			    is_binary(Data) ->
				Base64 == Data
			end
		    end));
test(15) ->
    ?FORALL(T,
	    ?LET(L,
		 non_empty(list(integer())),
		 ?LET(Y,
		      elements(L),
		      {Y,L})),
	    erlang:element(1,T) =/= 42);
test(16) ->
    ?FORALL(A, atom(), length(erlang:atom_to_list(A)) < 4);
test(17) ->
    ?FORALL(X, binary(), erlang:adler32(X) =/= 42);
test(18) ->
    ?FORALL(L, kvlist(atom(),integer()), not lists:keymember(42,2,L));
test(19) ->
    ?FORALL(T, tree(integer()), not tree_member(42, T));
test(20) ->
    ?FORALL(X,
	    ?LET(L, non_empty(list(integer())), list(oneof(L))),
	    length(X) < 10);
test(21) ->
    ?FORALL(X,
	    ?SUCHTHAT(Y, binary(), produces_term(Y)),
	    term_to_binary(binary_to_term(X)) =:= X);
test(22) ->
    ?FORALL(X,
	    integer(),
	    case X of
		42 -> erlang:throw(you_got_it);
		_  -> true
	    end);
test(_) ->
    ?FORALL(_, integer(), true).
