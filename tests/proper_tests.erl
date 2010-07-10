%%% Copyright 2010 Manolis Papadakis (manopapad@gmail.com)
%%%            and Kostis Sagonas (kostis@cs.ntua.gr)
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

%%% @author Manolis Papadakis <manopapad@gmail.com>
%%% @copyright 2010 Manolis Papadakis and Kostis Sagonas
%%% @version {@version}
%%% @doc This modules contains the Unit tests. You need the EUnit application
%%%	 to compile it.

-module(proper_tests).

-include("proper.hrl").

-include_lib("eunit/include/eunit.hrl").


%%------------------------------------------------------------------------------
%% Helper functions and macros
%%------------------------------------------------------------------------------

-define(SHRINK_TEST_OPTS, [{start_size,10},{max_shrinks,10000}]).

-define(_perfectRun(Test),
	?_perfectRun(Test, [])).

-define(_perfectRun(Test, Opts),
	?_assertRun(true, {passed,100,[]}, Test, Opts)).

-define(_assertRun(ExpShortResult, ExpLongResult, Test, Opts),
	?_test(begin
		   ?assertEqual(ExpShortResult, proper:check(Test,Opts)),
		   proper:clean_garbage(),
		   ?assert(state_is_clean()),
		   ?assertMatch(ExpLongResult,
				proper:check(Test,[long_result|Opts])),
		   proper:clean_garbage(),
		   ?assert(state_is_clean())
		end)).

state_is_clean() ->
    get() =:= [].

-define(_failsWithReason(ExpReason, Test),
	?_failsWith(ExpReason, _, Test)).

-define(_failsWith(ExpShrunk, Test),
	?_failsWith(_, ExpShrunk, Test)).

-define(_failsWith(ExpReason, ExpShrunk, Test),
	?_failRun(ExpReason, _, ExpShrunk, none, Test, [])).

-define(_failsWithOneOf(AllShrunk, Test),
	?_failsWithOneOf(_, AllShrunk, Test)).

-define(_failsWithOneOf(ExpReason, AllShrunk, Test),
	?_failRun(ExpReason, _, _, AllShrunk, Test, [])).

-define(_shrinksTo(ExpShrunkInstance, Type),
	?_failRun(false_prop, _, [ExpShrunkInstance], none,
		  ?FORALL(_X,Type,false), ?SHRINK_TEST_OPTS)).

-define(_shrinksToOneOf(AllShrunkInstances, Type),
	?_failRun(false_prop, _, _, [[X] || X <- AllShrunkInstances],
		  ?FORALL(_X,Type,false), ?SHRINK_TEST_OPTS)).

-define(cExmMatches(ExpReason, ExpBound, AllBound, CExm),
	begin
	    ?assertMatch(ExpReason, proper:get_fail_reason(CExm)),
	    ?assertMatch(ExpBound, proper:get_bound(CExm)),
	    assertEqualsOneOf(proper:get_bound(CExm), AllBound)
	end).

assertEqualsOneOf(_X, none) ->
    ok;
assertEqualsOneOf(X, List) ->
    ?assert(lists:any(fun(Y) -> Y =:= X end, List)).

-define(_failRun(ExpReason, ExpTestCase, ExpShrunk, AllShrunk, Test, Opts),
	?_test(begin
		   ?assertEqual(exp_short_result(Opts),
				proper:check(Test, Opts)),
		   CExm1 = proper:get_counterexample(),
		   ?assert(CExm1 =/= undefined),
		   proper:clean_garbage(),
		   ?assert(state_is_clean()),
		   case proper:check(Test, [long_result | Opts]) of
		       {failed,_,CExm2,_,CExm3} ->
			   ?assertNot(lists:member(fails, Opts)),
			   ?assertNot(lists:member(noshrink, Opts)),
			   ?cExmMatches(ExpReason, ExpShrunk, AllShrunk, CExm1),
			   ?cExmMatches(ExpReason, ExpTestCase, none, CExm2),
			   ?cExmMatches(ExpReason, ExpShrunk, AllShrunk, CExm3);
		       {failed,_,CExm2} ->
			   ?assert(lists:member(fails, Opts) orelse
				   lists:member(noshrink, Opts)),
			   ?cExmMatches(ExpReason, ExpTestCase, none, CExm1),
			   ?cExmMatches(ExpReason, ExpTestCase, none, CExm2)
		   end,
		   proper:clean_garbage(),
		   ?assert(state_is_clean())
	       end)).

exp_short_result(Opts) -> lists:member(fails, Opts).

assert_is_pure_function(F) ->
    {arity,Arity} = erlang:fun_info(F, arity),
    ArgsList = [lists:duplicate(Arity,0), lists:duplicate(Arity,1),
		lists:seq(1,Arity), lists:seq(0,Arity-1)],
    [begin
	 Result = apply(F,Args),
	 ?assertEqual(Result, apply(F,Args)),
	 Result
     end || Args <- ArgsList].

inc_temp() ->
    case get(temp) of
	undefined -> put(temp,1);
	X         -> put(temp,X + 1)
    end,
    ok.

get_temp() ->
    get(temp).

erase_temp() ->
    erase(temp),
    ok.

smaller_lengths_than_my_own(L) ->
    lists:seq(0,length(L)).

correct_smaller_length_aggregation(Tests, SmallerLens) ->
    {Zeros,Larger} = lists:partition(fun(X) -> X =:= 0 end, SmallerLens),
    length(Zeros) =:= Tests
    andalso correct_smaller_length_aggregation(Tests, Larger, 1).

correct_smaller_length_aggregation(0, SmallerLens, _Len) ->
    SmallerLens =:= [];
correct_smaller_length_aggregation(NotMoreThan, SmallerLens, Len) ->
    {Lens,Larger} = lists:partition(fun(X) -> X =:= Len end, SmallerLens),
    Num = length(Lens),
    Num =< NotMoreThan
    andalso correct_smaller_length_aggregation(Num, Larger, Len+1).


%%------------------------------------------------------------------------------
%% Unit test arguments
%%------------------------------------------------------------------------------

types_with_data() ->
    [{integer(), [-1,0,1,42,-200], 0, [0.3,someatom,<<1>>]},
     {integer(7,88), [7,8,87,88,23], 7, [1,90,a]},
     {integer(0,42), [0,11,42], 0, [-1,43]},
     {integer(-99,0), [-88,-99,0], 0, [1,-1112]},
     {integer(-999,-12), [-34,-999,-12], -12, [0,5]},
     {integer(-99,21), [-98,0,21], 0, [-100]},
     {integer(0,0), [0], 0, [1,-1,100,-100]},
     {pos_integer(), [12,1,444], 1, [-12,0]},
     {non_neg_integer(), [42,0], 0, [-9,rr]},
     {neg_integer(), [-222,-1], -1, [0,1111]},
     {float(), [17.65,-1.12], 0.0, [11,atomm,<<>>]},
     {float(7.4,88.0), [7.4,88.0], 7.4, [-1.0,3.2]},
     {float(0.0,42.1), [0.1,42.1], 0.0, [-0.1]},
     {float(-99.9,0.0), [-0.01,-90.0], 0.0, [someatom,-12,-100.0,0.1]},
     {float(-999.08,-12.12), [-12.12,-12.2], -12.12, [-1111.0,1000.0]},
     {float(-71.8,99.0), [-71.8,99.0,0.0,11.1], 0.0, [100.0,-71.9]},
     {float(0.0,0.0), [0.0], 0.0, [0.1,-0.1]},
     {non_neg_float(), [88.8,98.9,0.0], 0.0, [-12,1,-0.01]},
     {atom(), [elvis,'Another Atom',''], '', ["not_an_atom",12,12.2]},
     {binary(), [<<>>,<<12,21>>], <<>>, [<<1,2:3>>,binary_atom,42]},
     {binary(3), [<<41,42,43>>], <<0,0,0>>, [<<1,2,3,4>>]},
     {binary(0), [<<>>], <<>>, [<<1>>]},
     {bitstring(), [<<>>,<<87,76,65,5:4>>], <<>>, [{12,3},11]},
     {bitstring(18), [<<0,1,2:2>>,<<1,32,123:2>>], <<0,0,0:2>>, [<<12,1,1:3>>]},
     {bitstring(32), [<<120,120,120,120>>], <<0,0,0,0>>, [7,8]},
     {bitstring(0), [<<>>], <<>>, [<<1>>]},
     {list(integer()), [[],[2,42],[0,1,1,2,3,5,8,13,21,34,55,89,144]], [],
      [[4,4.2],{12,1},<<12,113>>]},
     {list(atom()), [[on,the,third,day,'of',christmas,my,true,love,sent,to,me]],
      [], [['not',1,list,'of',atoms],not_a_list]},
     {list(union([integer(),atom()])), [[3,french,hens,2],[turtle,doves]], [],
      [{'and',1}]},
     {vector(5,atom()), [[partridge,in,a,pear,tree],[a,b,c,d,e]],
      ['','','','',''], [[a,b,c,d],[a,b,c,d,e,f]]},
     {vector(2,float()), [[0.0,1.1],[4.4,-5.5]], [0.0,0.0], [[1,1]]},
     {vector(0,integer()), [[]], [], [[1],[2]]},
     {union([good,bad,ugly]), [good,bad,ugly], good, [clint,"eastwood"]},
     {union([integer(),atom()]), [twenty_one,21], 0, ["21",<<21>>]},
     {weighted_union([{10,luck},{20,skill},{15,concentrated_power_of_will},
		      {5,pleasure},{50,pain},{100,remember_the_name}]),
      [skill,pain,pleasure], luck, [clear,20,50]},
     {{integer(0,42),list(atom())}, [{42,[a,b]},{21,[c,de,f]},{0,[]}], {0,[]},
      [{-1,[a]},{12},{21,[b,c],12}]},
     {tuple([atom(),integer()]), [{the,1}], {'',0}, [{"a",0.0}]},
     {loose_tuple(integer()), [{1,44,-1},{},{99,-99}], {}, [4,{hello,2},[1,2]]},
     {loose_tuple(union([atom(),float()])), [{a,4.4,b},{},{'',c},{1.2,-3.4}], {},
      [an_atom,0.4,{hello,2},[aa,bb,3.1]]},
     {exactly({[writing],unit,[tests,is],{2},boring}),
      [{[writing],unit,[tests,is],{2},boring}],
      {[writing],unit,[tests,is],{2},boring}, [no,its,'not','!']},
     {[neg_integer(),pos_integer()], [[-12,32],[-1,1]], [-1,1], [[0,0]]},
     {[atom(),integer(),atom(),float()], [[forty_two,42,forty_two,42.0]],
      ['',0,'',0.0], [[proper,is,licensed],[under,the,gpl]]},
     {[42 | list(integer())], [[42],[42,44,22]], [42], [[],[11,12]]},
     {number(), [12,32.3,-9,-77.7], 0, [manolis,papadakis]},
     {boolean(), [true,false], false, [unknown]},
     {string(), ["hello","","world"], "", ['hello']},
     {?LAZY(integer()), [0,2,99], 0, [1.1]},
     {?LAZY(list(float())), [[0.0,1.2,1.99],[]], [], [1.1,[1,2]]},
     {zerostream(10), [[0,0,0],[],[0,0,0,0,0,0,0]], [], [[1,0,0],[0.1]]},
     {?SHRINK(pos_integer(),[0]), [1,12,0], 0, [-1,-9,6.0]},
     {?SHRINK(float(),[integer(),atom()]), [1.0,0.0,someatom,'',42,0], 0,
      [<<>>,"hello"]},
     {noshrink(?SHRINK(42,[0,1])), [42,0,1], 42, [-1]},
     {non_empty(list(integer())), [[1,2,3],[3,42],[11]], [0], [[],[0.1]]},
     {default(42,float()), [4.1,-99.0,0.0,42], 42, [43,44]},
     {?SUCHTHAT(X,non_neg_integer(),X rem 4 =:= 1), [1,5,37,89], 1, [4,-12,11]},
     %% TODO: This behaviour may change in the future, since it's incompatible
     %%       with EQC.
     {?SUCHTHATMAYBE(X,non_neg_integer(),X rem 4 =:= 1), [1,2,3,4,5,37,89], 0,
      [1.1,2.2,-12]},
     {any(), [1,-12,0,99.9,-42.2,0.0,an_atom,'',<<>>,<<1,2>>,<<1,2,3:5>>,[],
	      [42,<<>>],{},{tag,12},{tag,[vals,12,12.2],[],<<>>}], 0, []},
     {list(any()), [[<<>>,a,1,-42.0,{11.8,[]}]], [], [{1,aa},<<>>]}].

function_types() ->
    [function([],atom()),
     function([integer(),integer()],atom()),
     function(5,union([a,b])),
     function(0,function(1,integer()))].

impossible_types() ->
    [?SUCHTHAT(X, pos_integer(), X =< 0),
     ?SUCHTHAT(X, non_neg_integer(), X < 0),
     ?SUCHTHAT(X, neg_integer(), X >= 0),
     ?SUCHTHAT(X, integer(1,10), X > 20),
     ?SUCHTHAT(X, float(0.0,10.0), X < 0.0),
     ?SUCHTHAT(L, vector(12,integer()), length(L) =/= 12),
     ?SUCHTHAT(B, binary(), lists:member(256,binary_to_list(B))),
     ?SUCHTHAT(X, exactly('Lelouch'), X =:= 'vi Brittania')].

symb_calls() ->
    [{[3,2,1], "lists:reverse([1,2,3])", [], {call,lists,reverse,[[1,2,3]]}},
     {[a,b,c,d], "erlang:'++'([a,b],[c,d])",
      [{a,some_value}], {call,erlang,'++',[[a,b],[c,d]]}},
     {42, "erlang:'*'(erlang:'+'(3,3),erlang:'-'(8,1))",
      [{b,dummy_value},{e,another_dummy}],
      {call,erlang,'*',[{call,erlang,'+',[3,3]},{call,erlang,'-',[8,1]}]}},
     {something, "something",
      [{a,somebody},{b,put},{c,something},{d,in_my_drink}], {var,c}},
     {{var,b}, "{var,b}", [{a,not_this},{c,neither_this}], {var,b}},
     {42, "erlang:'+'(40,2)", [{m,40},{n,2}],
      {call,erlang,'+',[{var,m},{var,n}]}},
     {[i,am,{var,iron},man],
      "erlang:'++'(lists:reverse([am,i]),erlang:'++'([{var,iron}],[man]))",
      [{a,man},{b,woman}],
      {call,erlang,'++',[{call,lists,reverse,[[am,i]]},
			 {call,erlang,'++',[[{var,iron}],[{var,a}]]}]}}].

undefined_symb_calls() ->
    [{call,erlang,error,[an_error]},
     {call,erlang,throw,[a_throw]},
     {call,erlang,exit,[an_exit]},
     {call,lists,reverse,[<<12,13>>]},
     {call,erlang,'+',[1,2,3]}].


%%------------------------------------------------------------------------------
%% Unit tests
%%------------------------------------------------------------------------------

%% TODO: write tests for old datatypes, use old tests
%% TODO: check output redirection, quiet, to_file, on_output/2 (maybe by
%%	 writing to a string in the process dictinary), statistics printing,
%%	 standard verbose behaviour
%% TODO: fix compiler warnings
%% TODO: how to test 'crypto' option?
%% TODO: LET and LETSHRINK testing (these need their intermediate form for
%%	 standalone instance testing and shrinking) - update needed after
%%	 fixing the internal shrinking in LETs, use recursive datatypes, like
%%	 trees for testing

is_instance_test_() ->
    [?_assert(proper_types:is_instance(X,Type) andalso state_is_clean())
     || {Type,Xs,_Target,_Ys} <- types_with_data(), X <- Xs].

%% TODO: specific test-starting instances would be useful here
%%	 (start from valid Xs)
shrinks_to_test_() ->
    [?_shrinksTo(Target,Type) || {Type,_Xs,Target,_Ys} <- types_with_data()].

%% TODO: 'unknown' causes problems with 'not', need 'surely'
not_is_instance_test_() ->
    [?_assert(not proper_arith:surely(proper_types:is_instance(Y,Type))
	      andalso state_is_clean())
     || {Type,_Xs,_Target,Ys} <- types_with_data(), Y <- Ys].

generate_test_() ->
    [?_test(begin
		{ok,Instance} = proper_gen:pick(Type),
		?assert(state_is_clean()),
		?assert(proper_types:is_instance(Instance, Type)),
		?assert(state_is_clean())
	    end)
     || {Type,_Xs,_Target,_Ys} <- types_with_data()].

cant_generate_test_() ->
    [?_test(begin
		?assertEqual(error, proper_gen:pick(Type)),
		?assert(state_is_clean())
	    end)
     || Type <- impossible_types()].

%% TODO: Use retesting of counterexample here, instead of using private
%%	 functions to reset the state.
random_functions_test_() ->
    [?_test(begin
		{ok,F} = proper_gen:pick(FunType),
		?assert(proper_types:is_instance(F, FunType)),
		Results1 = assert_is_pure_function(F),
		GenState = proper_gen:gen_state_get(),
		proper:global_state_erase(),
		?assert(state_is_clean()),
		proper:global_state_init_size(10),
		proper_gen:gen_state_set(GenState),
		Results2 = assert_is_pure_function(F),
		?assertEqual(Results1,Results2),
		proper:global_state_erase(),
		?assert(state_is_clean())
	    end)
     || FunType <- function_types()].

true_props_test_() ->
    [?_perfectRun(?FORALL(X,integer(),X < X + 1)),
     ?_perfectRun(?FORALL(X,atom(),list_to_atom(atom_to_list(X)) =:= X)),
     ?_perfectRun(?FORALL(L,list(integer()),is_sorted(L,quicksort(L)))),
     ?_perfectRun(?FORALL(L,list(integer()),is_sorted(L,lists:sort(L)))),
     ?_perfectRun(?FORALL(L,ulist(integer()),is_sorted(L,lists:usort(L)))),
     ?_perfectRun(?FORALL(L,non_empty(list(integer())),L =/= [])),
     ?_assertRun(true, {passed,_,[]},
		 ?FORALL({I,L}, {integer(),list(integer())},
			 ?IMPLIES(no_duplicates(L),
				  not lists:member(I,lists:delete(I,L)))), []),
     ?_perfectRun(?FORALL(L, ?SIZED(Size,resize(Size div 5,list(integer()))),
			  length(L) =< 20)),
     ?_test(begin
		{passed,100,[Lengths,IsEmpty]} =
		    proper:check(?FORALL(L, list(integer()),
					 collect(length(L),collect(L =:= [],
					 lists:reverse(lists:reverse(L))
					 =:= L))),
				long_result),
		proper:clean_garbage(),
		?assert(state_is_clean()),
		?assertEqual(100, length(Lengths)),
		?assertEqual(100, length(IsEmpty)),
		?assertEqual(length([X || X <- Lengths, X =:= 0]),
			     length([X || X <- IsEmpty, X =:= true]))
	    end),
     ?_test(begin
		{passed,100,[SmallerLens]} =
		    proper:check(
			?FORALL(L, list(integer()),
				aggregate(smaller_lengths_than_my_own(L),
					  true)),
			long_result),
		proper:clean_garbage(),
		?assert(state_is_clean()),
		?assert(correct_smaller_length_aggregation(100, SmallerLens))
	    end),
     ?_assertRun(true, {passed,300,[]}, numtests(300,?FORALL(_,1,true)), []),
     ?_perfectRun(?FORALL(X, integer(), ?IMPLIES(abs(X) > 1, X * X > X))),
     ?_perfectRun(?FORALL(X, integer(), ?IMPLIES(X >= 0, true))),
     ?_perfectRun(?FORALL({X,Lim},{int(),?SIZED(Size,Size)},abs(X) =< Lim)),
     ?_perfectRun(?FORALL({X,Lim},{nat(),?SIZED(Size,Size)},X =< Lim)),
     ?_perfectRun(?FORALL(L,orderedlist(integer()),is_sorted(L)))].

false_props_test_() ->
    [?_failsWith(false_prop, [[_Same,_Same]],
		 ?FORALL(L,list(integer()),is_sorted(L,lists:usort(L)))),
     ?_failsWith(false_prop, [[_Same,_Same],_Same],
		 ?FORALL(L, non_empty(list(union([a,b,c,d]))),
			 ?FORALL(X, elements(L),
				 not lists:member(X,lists:delete(X,L))))),
     ?_failsWith(false_prop, ['\000\000\000\000'],
		 ?FORALL(A, atom(), length(atom_to_list(A)) < 4)),
     ?_failsWith({exception,throw,not_zero,_}, [1],
		 ?FORALL(X, non_neg_integer(),
			 case X > 0 of
			     true  -> throw(not_zero);
			     false -> true
			 end)),
     ?_failsWith({exception,exit,you_got_it,_}, [[12,42]],
		 ?FORALL(L, [12,42|list(integer())],
			 ?TRAPEXIT(
			     case lists:member(42, L) of
				 true  -> erlang:exit(you_got_it);
				 false -> true
			     end))),
     ?_failsWith(timeout, _,
		  ?FORALL(_, integer(),
			  ?TIMEOUT(100,timer:sleep(150) =:= ok))),
     ?_assertRun(false, {failed,5,_SameCExm,0,_SameCExm},
		 ?FORALL(X,?SIZED(Size,integer(Size,Size)),X < 5), []),
     ?_test(begin
		proper:check(?FORALL(L, list(atom()),
				     ?WHENFAIL(inc_temp(), length(L) < 5))),
		?assertEqual(2, get_temp()),
		erase_temp(),
		proper:clean_garbage(),
		?assert(state_is_clean())
	    end),
     ?_failsWithOneOf(false_prop, [[{true,false}],[{false,true}]],
		      ?FORALL({B1,B2}, {boolean(),boolean()}, equals(B1,B2))),
     ?_failsWith(false_prop, [2,1],
		 ?FORALL(X,integer(1,10),?FORALL(Y,integer(1,10),X =< Y))),
     ?_failsWith(false_prop, [1,2],
		 ?FORALL(Y,integer(1,10),?FORALL(X,integer(1,10),X =< Y))),
     ?_failsWithOneOf(false_prop, [[[0,1]],[[0,-1]],[[1,0]],[[-1,0]]],
		      ?FORALL(L, list(integer()), lists:reverse(L) =:= L)),
     ?_failsWith(false_prop, [[1,2,3,4,5,6,7,8,9,10]],
		 ?FORALL(_L,shuffle(lists:seq(1,10)),false)),
     ?_assertRun(false, {failed,1,_,0,_}, ?FORALL(_,integer(0,0),false), []),
     ?_assertRun(false, {failed,1,_,0,_}, ?FORALL(_,float(0.0,0.0),false), []),
     ?_assertRun(true, {failed,_,_}, fails(?FORALL(_,integer(),false)), []),
     ?_failsWith(false_prop, [16], ?FORALL(X,?LET(Y,integer(),Y*Y),X < 15)),
     ?_failsWith(false_prop, [0.0],
		 ?FORALL(_, ?LETSHRINK([A,B],[float(),atom()],{A,B}), false))].

error_props_test_() ->
    [?_assertRun({error,cant_generate}, {error,cant_generate},
		 ?FORALL(_,?SUCHTHAT(X,pos_integer(),X =< 0),true), []),
     ?_assertRun({error,cant_satisfy}, {error,cant_satisfy},
		 ?FORALL(X,pos_integer(),?IMPLIES(X =< 0,true)), []),
     ?_assertRun({error,type_mismatch}, {error,type_mismatch},
		 ?FORALL({X,Y}, [integer(),integer()], X < Y), []),
     {setup, fun() -> ok end, fun(_) -> proper:global_state_erase() end,
      ?_assertError(function_clause,
		    proper:check(?FORALL(_,1,lists:min([]) > 0)))}].

eval_test_() ->
    [?_assertEqual(Result, eval(Vars,SymbCall))
     || {Result,_Repr,Vars,SymbCall} <- symb_calls()].

pretty_print_test_() ->
    [?_assert(equal_ignoring_ws(Repr, proper_symb:pretty_print(Vars,SymbCall)))
     || {_Result,Repr,Vars,SymbCall} <- symb_calls()].

not_defined_test_() ->
    [?_assertNot(defined(SymbCall))
     || SymbCall <- undefined_symb_calls()].

options_test_() ->
    [?_assertRun(true, {passed,300,[]}, ?FORALL(_,1,true), [{numtests,300}]),
     ?_assertRun(true, {passed,300,[]}, ?FORALL(_,1,true), [300]),
     ?_assertRun(false, {failed,1,_},
		 ?FORALL(L, list(float()),
			 ?IMPLIES(length(L) > 4, L =:= [])), [noshrink]),
     ?_failRun(false_prop, [42], [42], none,
	       ?FORALL(_,?SHRINK(42,[0,1]),false), [{max_shrinks,0}]),
     ?_failRun(false_prop, _, _, none, ?FORALL(_,integer(),false), [fails]),
     ?_assertRun({error,cant_generate}, {error,cant_generate},
		 ?FORALL(_,?SUCHTHAT(X,pos_integer(),X > 0),true),
		 [{constraint_tries,0}]),
     ?_failRun(false_prop, [12], _, none,
	       ?FORALL(_,?SIZED(Size,integer(Size,Size)),false),
	       [{start_size,12}])].


%%------------------------------------------------------------------------------
%% Helper Predicates
%%------------------------------------------------------------------------------

no_duplicates(L) ->
    length(lists:usort(L)) =:= length(L).

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

is_sorted(Old, New) ->
    same_elements(Old, New) andalso is_sorted(New).

equal_ignoring_ws(Str1, Str2) ->
    WhiteSpace = [32,9,10],
    equal_ignoring_chars(Str1, Str2, WhiteSpace).

equal_ignoring_chars([], [], _Ignore) ->
    true;
equal_ignoring_chars([_SameChar|Rest1], [_SameChar|Rest2], Ignore) ->
    equal_ignoring_chars(Rest1, Rest2, Ignore);
equal_ignoring_chars(Str1 = [Char1|Rest1], Str2 = [Char2|Rest2], Ignore) ->
    case lists:member(Char1, Ignore) of
	true ->
	    equal_ignoring_chars(Rest1, Str2, Ignore);
	false ->
	    case lists:member(Char2, Ignore) of
		true ->
		    equal_ignoring_chars(Str1, Rest2, Ignore);
		false ->
		    false
	    end
    end.


%%------------------------------------------------------------------------------
%% Functions to test
%%------------------------------------------------------------------------------

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


%%------------------------------------------------------------------------------
%% Datatypes to test
%%------------------------------------------------------------------------------

%% TODO: remove this if you make 'shuffle' a default constructor
shuffle([]) ->
    [];
shuffle(L) ->
    ?LET(X, elements(L), [X | shuffle(lists:delete(X,L))]).

ulist(ElemType) ->
    ?LET(L, list(ElemType), L--(L--lists:usort(L))).

zerostream(ExpectedMeanLen) ->
    ?LAZY(frequency([
	{1, []},
	{ExpectedMeanLen, [0 | zerostream(ExpectedMeanLen)]}
    ])).


%%------------------------------------------------------------------------------
%% Old Tests and datatypes
%%------------------------------------------------------------------------------

% nelist(ElemType) ->
%     [ElemType | list(ElemType)].
%
% uvector(0, _ElemType) ->
%    [];
% uvector(N, ElemType) ->
%     ?LET(Rest,
% 	 uvector(N-1, ElemType),
% 	 ?LET(Elem,
% 	      ?SUCHTHAT(E, ElemType, not lists:member(E,Rest)),
% 	      [Elem | Rest])).
%
% subset(Generators) ->
%     ?LET(Keep,
% 	 [{boolean(),G} || G <- Generators],
% 	 [G || {true,G} <- Keep]).
%
%
% unique(ElemTypes) ->
%     ?LET(Values,
% 	 list(ElemTypes),
% 	 lists:usort(Values)).
%
% ulist2(ElemType) ->
%     ?SUCHTHAT(L, list(ElemType), no_duplicates(L)).
%
% kvlist(KeyType, ValueType) ->
%     ?LET(Keys,
% 	 list(KeyType),
% 	 [{K,ValueType} || K <- Keys]).
%
% tree(ElemType) ->
%     ?SIZED(Size, tree(ElemType,Size)).
%
% tree(_ElemType, 0) ->
%     {empty};
% tree(ElemType, Size) ->
%     Left = tree(ElemType, Size div 2),
%     Right = tree(ElemType, Size div 2 - 1 + Size rem 2),
%     frequency([
% 	{1, tree(ElemType,0)},
% 	{5, ?LETSHRINK([L,R], [Left,Right], {node,ElemType,L,R})}
%     ]).
%
% tree_member(_X, {node,_X,_L,_R}) -> true;
% tree_member(X, {node,_Y,L,R}) -> tree_member(X, L) orelse tree_member(X, R);
% tree_member(_X, {empty}) -> false.
%
% symbdict(KeyType, ValueType) ->
%     ?SIZED(Size, symbdict(Size, KeyType, ValueType)).
%
% symbdict(0, _KeyType, _ValueType) ->
%     {call,dict,new,[]};
% symbdict(Size, KeyType, ValueType) ->
%     ?LAZY(
% 	frequency([
% 	    {1,symbdict(0, KeyType, ValueType)},
% 	    {4,?LETSHRINK([Smaller], [symbdict(Size - 1, KeyType, ValueType)],
% 			  {call, dict, append,[KeyType,ValueType,Smaller]})}
% 	])
%     ).
%
% test(15) ->
%     ?FORALL(T,
% 	    ?LET(L,
% 		 non_empty(list(integer())),
% 		 ?LET(Y,
% 		      elements(L),
% 		      {Y,L})),
% 	    erlang:element(1,T) =/= 42);
% test(18) ->
%     ?FORALL(L, kvlist(atom(),integer()), not lists:keymember(42,2,L));
% test(19) ->
%     ?FORALL(T, tree(integer()), not tree_member(42, T));
% test(20) ->
%     ?FORALL(X,
% 	    ?LET(L, non_empty(list(integer())), list(oneof(L))),
% 	    length(X) < 10);
% test(27) ->
%     ?FORALL(SD,
% 	    symbdict(integer(),integer()),
% 	    not dict:is_key(42, eval(SD)));
% test(29) ->
%     ?FORALL({F,L},
% 	    {function(1,integer(1,100)), list(integer())},
% 	    lists:all(fun(X) -> F(X) =/= 42 end, L));
