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
    get() =:= []
    andalso [] =:= [Proc || Proc <- registered(),
			    lists:member(Proc, ?PROPER_REGISTERED)].

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

%% TODO: after fixing the typesystem, use generic reverse function.
assert_is_instance(X, Type) ->
    ?assert(proper_types:is_inst(X, Type) andalso state_is_clean()).

assert_can_generate(Type) ->
    {ok,Instance} = proper_gen:pick(Type),
    ?assert(state_is_clean()),
    assert_is_instance(Instance, Type).

assert_can_translate(Mod, TypeStr) ->
    proper_typeserver:start(),
    Result = proper_typeserver:translate_type({Mod,TypeStr}),
    proper_typeserver:stop(),
    ?assert(state_is_clean()),
    {ok,Type} = Result,
    Type.

%% TODO: Use retesting of counterexample here, instead of using private
%%	 functions to reset the state.
assert_function_type_works(FunType) ->
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
    ?assert(state_is_clean()).

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
    [{integer(), [-1,0,1,42,-200], 0, [0.3,someatom,<<1>>], "integer()"},
     {integer(7,88), [7,8,87,88,23], 7, [1,90,a], "7..88"},
     {integer(0,42), [0,11,42], 0, [-1,43], "0..42"},
     {integer(-99,0), [-88,-99,0], 0, [1,-1112], "-99..0"},
     {integer(-999,-12), [-34,-999,-12], -12, [0,5], "-999..-12"},
     {integer(-99,21), [-98,0,21], 0, [-100], "-99..21"},
     {integer(0,0), [0], 0, [1,-1,100,-100], "0..0"},
     {pos_integer(), [12,1,444], 1, [-12,0], "pos_integer()"},
     {non_neg_integer(), [42,0], 0, [-9,rr], "non_neg_integer()"},
     {neg_integer(), [-222,-1], -1, [0,1111], "neg_integer()"},
     {float(), [17.65,-1.12], 0.0, [11,atomm,<<>>], "float()"},
     {float(7.4,88.0), [7.4,88.0], 7.4, [-1.0,3.2], none},
     {float(0.0,42.1), [0.1,42.1], 0.0, [-0.1], none},
     {float(-99.9,0.0), [-0.01,-90.0], 0.0, [someatom,-12,-100.0,0.1], none},
     {float(-999.08,-12.12), [-12.12,-12.2], -12.12, [-1111.0,1000.0], none},
     {float(-71.8,99.0), [-71.8,99.0,0.0,11.1], 0.0, [100.0,-71.9], none},
     {float(0.0,0.0), [0.0], 0.0, [0.1,-0.1], none},
     {non_neg_float(), [88.8,98.9,0.0], 0.0, [-12,1,-0.01], none},
     {atom(), [elvis,'Another Atom',''], '', ["not_an_atom",12,12.2], "atom()"},
     {binary(), [<<>>,<<12,21>>], <<>>, [<<1,2:3>>,binary_atom,42], "binary()"},
     {binary(3), [<<41,42,43>>], <<0,0,0>>, [<<1,2,3,4>>], "<<_:3>>"},
     {binary(0), [<<>>], <<>>, [<<1>>], none},
     {bitstring(), [<<>>,<<87,76,65,5:4>>], <<>>, [{12,3},11], "bitstring()"},
     {bitstring(18), [<<0,1,2:2>>,<<1,32,123:2>>], <<0,0,0:2>>, [<<12,1,1:3>>],
      "<<_:18, _:_*1>>"},
     {bitstring(32), [<<120,120,120,120>>], <<0,0,0,0>>, [7,8],
      "<<_:32, _:_*1>>"},
     {bitstring(0), [<<>>], <<>>, [<<1>>], none},
     {list(integer()), [[],[2,42],[0,1,1,2,3,5,8,13,21,34,55,89,144]], [],
      [[4,4.2],{12,1},<<12,113>>], "[integer()]"},
     {list(atom()), [[on,the,third,day,'of',christmas,my,true,love,sent,to,me]],
      [], [['not',1,list,'of',atoms],not_a_list], "[atom()]"},
     {list(union([integer(),atom()])), [[3,french,hens,2],[turtle,doves]], [],
      [{'and',1}], "[integer() | atom()]"},
     {vector(5,atom()), [[partridge,in,a,pear,tree],[a,b,c,d,e]],
      ['','','','',''], [[a,b,c,d],[a,b,c,d,e,f]], none},
     {vector(2,float()), [[0.0,1.1],[4.4,-5.5]], [0.0,0.0], [[1,1]], none},
     {vector(0,integer()), [[]], [], [[1],[2]], none},
     {union([good,bad,ugly]), [good,bad,ugly], good, [clint,"eastwood"],
      "good | bad | ugly"},
     {union([integer(),atom()]), [twenty_one,21], 0, ["21",<<21>>],
      "integer() | atom()"},
     {weighted_union([{10,luck},{20,skill},{15,concentrated_power_of_will},
		      {5,pleasure},{50,pain},{100,remember_the_name}]),
      [skill,pain,pleasure], luck, [clear,20,50], none},
     {{integer(0,42),list(atom())}, [{42,[a,b]},{21,[c,de,f]},{0,[]}], {0,[]},
      [{-1,[a]},{12},{21,[b,c],12}], "{0..42,[atom()]}"},
     {tuple([atom(),integer()]), [{the,1}], {'',0}, [{"a",0.0}],
      "{atom(),integer()}"},
     {{}, [{}], {}, [[],{1,2}], "{}"},
     {loose_tuple(integer()), [{1,44,-1},{},{99,-99}], {}, [4,{hello,2},[1,2]],
      none},
     {loose_tuple(union([atom(),float()])), [{a,4.4,b},{},{'',c},{1.2,-3.4}],
      {},[an_atom,0.4,{hello,2},[aa,bb,3.1]], none},
     {exactly({[writing],unit,[tests,is],{2},boring}),
      [{[writing],unit,[tests,is],{2},boring}],
      {[writing],unit,[tests,is],{2},boring}, [no,its,'not','!'], none},
     {[], [[]], [], [[a],[1,2,3]], "[]"},
     {fixed_list([neg_integer(),pos_integer()]), [[-12,32],[-1,1]], [-1,1],
      [[0,0]], none},
     {[atom(),integer(),atom(),float()], [[forty_two,42,forty_two,42.0]],
      ['',0,'',0.0], [[proper,is,licensed],[under,the,gpl]], none},
     {[42 | list(integer())], [[42],[42,44,22]], [42], [[],[11,12]], none},
     {number(), [12,32.3,-9,-77.7], 0, [manolis,papadakis], "number()"},
     {boolean(), [true,false], false, [unknown], "boolean()"},
     {string(), ["hello","","world"], "", ['hello'], "string()"},
     {?LAZY(integer()), [0,2,99], 0, [1.1], "integer()"},
     {?LAZY(list(float())), [[0.0,1.2,1.99],[]], [], [1.1,[1,2]], "[float()]"},
     {zerostream(10), [[0,0,0],[],[0,0,0,0,0,0,0]], [], [[1,0,0],[0.1]], none},
     {?SHRINK(pos_integer(),[0]), [1,12,0], 0, [-1,-9,6.0],
      "pos_integer() | 0"},
     {?SHRINK(float(),[integer(),atom()]), [1.0,0.0,someatom,'',42,0], 0,
      [<<>>,"hello"], "float() | integer() | atom()"},
     {noshrink(?SHRINK(42,[0,1])), [42,0,1], 42, [-1], "42 | 0 | 1"},
     {non_empty(list(integer())), [[1,2,3],[3,42],[11]], [0], [[],[0.1]],
      "[integer(),...]"},
     {default(42,float()), [4.1,-99.0,0.0,42], 42, [43,44], "42 | float()"},
     {?SUCHTHAT(X,non_neg_integer(),X rem 4 =:= 1), [1,5,37,89], 1, [4,-12,11],
      none},
     %% TODO: This behaviour may change in the future, since it's incompatible
     %%       with EQC.
     {?SUCHTHATMAYBE(X,non_neg_integer(),X rem 4 =:= 1), [1,2,3,4,5,37,89], 0,
      [1.1,2.2,-12], "non_neg_integer()"},
     {any(), [1,-12,0,99.9,-42.2,0.0,an_atom,'',<<>>,<<1,2>>,<<1,2,3:5>>,[],
	      [42,<<>>],{},{tag,12},{tag,[vals,12,12.2],[],<<>>}],
	     0, [], "any()"},
     {list(any()), [[<<>>,a,1,-42.0,{11.8,[]}]], [], [{1,aa},<<>>], "[any()]"}].

function_types() ->
    [{function([],atom()), "fun(() -> atom())"},
     {function([integer(),integer()],atom()),
      "fun((integer(),integer()) -> atom())"},
     {function(5,union([a,b])), "fun((_,_,_,_,_) -> a | b)"},
     {function(0,function(1,integer())),
      "fun(() -> fun((_) -> integer()))"}].

remote_builtin_types() ->
    [{types_test1,["#rec1{}","rec1()","exp1()","type1()","type2(atom())",
		   "rem1()","rem2()","types_test1:exp1()",
		   "types_test2:exp1(float())","types_test2:exp2()"]},
     {types_test2,["exp1(#rec1{})","exp2()","#rec1{}","types_test1:exp1()",
		   "types_test2:exp1(binary())","types_test2:exp2()"]}].

impossible_types() ->
    [?SUCHTHAT(X, pos_integer(), X =< 0),
     ?SUCHTHAT(X, non_neg_integer(), X < 0),
     ?SUCHTHAT(X, neg_integer(), X >= 0),
     ?SUCHTHAT(X, integer(1,10), X > 20),
     ?SUCHTHAT(X, float(0.0,10.0), X < 0.0),
     ?SUCHTHAT(L, vector(12,integer()), length(L) =/= 12),
     ?SUCHTHAT(B, binary(), lists:member(256,binary_to_list(B))),
     ?SUCHTHAT(X, exactly('Lelouch'), X =:= 'vi Brittania')].

impossible_builtin_types() ->
    [{types_test1, ["1.1","no_such_module:type1()","no_such_type()"]},
     {types_test2, ["types_test1:type1()","function()","fun((...) -> atom())",
		    "pid()","port()","ref()"]}].

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

%% TODO: use size=100 for is_instance testing?
%% TODO: now typeserver is exact, include more tests
%% TODO: recursive and mutuals, also write the expected types as generators here
%% TODO: remotes + recursives
%% TODO: wrong recursives
%% TODO: one test for is_instance, shrinks_to, can_generate and not_is_instance

%% TODO:
%% Test on:
%% -type a() :: 'aleaf' | b() | [{'rec',a()}] | c() | d().
%% -type b() :: 'bleaf' | {'bnode',b(),b()}.
%% -type c() :: [c()] | {'cnode1',a()} | {'cnode2',d()}.
%% -type d() :: [a()].
%% Test on:
%% -type mylist(T) :: [] | {'cons',T,mylist(T)}
%% -type b() :: mylist(integer()).
%% -type a() :: 'aleaf' | mylist(a()).
%% Test on:
%% -type deeplist() :: [deeplist()].

is_instance_test_() ->
    [?_test(assert_is_instance(X, Type))
     || {Type,Xs,_Target,_Ys,_TypeStr} <- types_with_data(), X <- Xs].

builtin_is_instance_test_() ->
    [?_test(begin
		Type = assert_can_translate(proper, TypeStr),
		lists:foreach(fun(X) -> assert_is_instance(X,Type) end, Xs)
	    end)
     || {_Type,Xs,_Target,_Ys,TypeStr} <- types_with_data(), TypeStr =/= none].

%% TODO: specific test-starting instances would be useful here
%%	 (start from valid Xs)
shrinks_to_test_() ->
    [?_shrinksTo(Target, Type)
     || {Type,_Xs,Target,_Ys,_TypeStr} <- types_with_data()].

not_is_instance_test_() ->
    [?_assert(not proper_types:is_inst(Y, Type) andalso state_is_clean())
     || {Type,_Xs,_Target,Ys,_TypeStr} <- types_with_data(), Y <- Ys].

can_generate_test_() ->
    [?_test(assert_can_generate(Type))
     || {Type,_Xs,_Target,_Ys,_TypeStr} <- types_with_data()].

builtin_can_generate_test_() ->
    [?_test(assert_can_generate(assert_can_translate(proper,TypeStr)))
     || {_Type,_Xs,_Target,_Ys,TypeStr} <- types_with_data(), TypeStr =/= none].

remote_builtin_types_test_() ->
    [?_test(assert_can_translate(Mod,TypeStr))
     || {Mod,Strings} <- remote_builtin_types(), TypeStr <- Strings].

cant_generate_test_() ->
    [?_test(begin
		?assertEqual(error, proper_gen:pick(Type)),
		?assert(state_is_clean())
	    end)
     || Type <- impossible_types()].

builtin_cant_generate_test_() ->
    [?_test(begin
		proper_typeserver:start(),
		Result = proper_typeserver:translate_type({Mod,TypeStr}),
		proper_typeserver:stop(),
		?assert(state_is_clean()),
		?assertMatch({error,_}, Result)
	    end)
     || {Mod,Strings} <- impossible_builtin_types(), TypeStr <- Strings].

random_functions_test_() ->
    [[?_test(assert_function_type_works(FunType)),
      ?_test(assert_function_type_works(assert_can_translate(proper,TypeStr)))]
     || {FunType,TypeStr} <- function_types()].

true_props_test_() ->
    [?_perfectRun(?FORALL(X,integer(),X < X + 1)),
     ?_perfectRun(?FORALL(X,atom(),list_to_atom(atom_to_list(X)) =:= X)),
     ?_perfectRun(?FORALL(L,list(integer()),is_sorted(L,quicksort(L)))),
     ?_perfectRun(?FORALL_B(L,[integer()],is_sorted(L,lists:sort(L)))),
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
     ?_failsWith(time_out, _,
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
