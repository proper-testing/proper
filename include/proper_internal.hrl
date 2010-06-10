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

%% Internal header file
%% This header is included in all PropEr source files.


%% Constants

%% TODO: make all these into parameters, store them in process registry
-define(DEFAULT_RNG_CONST, 10).
-define(MAX_TRIES_TO_SATISFY_CONSTRAINTS, 50).
-define(MAX_RANDOM_TRIES_WHEN_SHRINKING, 5).
-define(MAX_LIST_LEN, 200).


%% Generator macros

-define(FORCE(X), (X)()).
-define(DELAY(X), fun() -> X end).
-define(SIZED(SizeArg,Gen), proper_types:sized(fun(SizeArg) -> Gen end)).
-define(LET(X,RawType,Gen), proper_types:bind(RawType,fun(X) -> Gen end)).
-define(SHRINK(Gen,AltGens),
	proper_types:shrinkwith(?DELAY(Gen),?DELAY(AltGens))).
-define(LETSHRINK(Xs,RawType,Gen),
	proper_types:bind(RawType,
			  fun(Xs) ->
			      proper_types:shrinkwith(?DELAY(Gen),?DELAY(Xs))
			  end)).
-define(SUCHTHAT(X,RawType,Condition),
	proper_types:add_constraint(RawType,fun(X) -> Condition end,true)).
-define(SUCHTHATMAYBE(X,RawType,Condition),
	proper_types:add_constraint(RawType,fun(X) -> Condition end,false)).


%% Type declaration macros

-define(BASIC(PropList), proper_types:new_type(PropList,basic)).
-define(WRAPPER(PropList), proper_types:new_type(PropList,wrapper)).
-define(CONSTRUCTED(PropList), proper_types:new_type(PropList,constructed)).
-define(SEMI_OPAQUE(PropList), proper_types:new_type(PropList,semi_opaque)).
-define(OPAQUE(PropList), proper_types:new_type(PropList,opaque)).
-define(SUBTYPE(Type,PropList), proper_types:subtype(PropList,Type)).


%% Dialyzer Types

-type extint() :: integer() | 'inf'.
-type extnum() :: number() | 'inf'.
-type ternary() :: 'true' | 'false' | 'unknown'.
-type time_period() :: non_neg_integer().
-type size() :: non_neg_integer().
-type length() :: non_neg_integer().
-type position() :: pos_integer().
-type frequency() :: pos_integer().
-type index() :: term().
-type value() :: term().
-type category() :: term().

-type cat_dict() :: [{category(),frequency()}].
-type side_effects_fun() :: fun(() -> 'ok').
-type instance_test() :: fun((instance()) -> boolean()).
-type sized_generator() :: fun((size()) -> imm_instance()).
-type nosize_generator() :: fun(() -> imm_instance()).
-type generator() :: sized_generator() | nosize_generator().
-type combine_fun() :: fun((instance()) -> imm_instance()).
-type alt_gens() :: fun(() -> [imm_instance()]).
-type testcase() :: [imm_instance()].
-type clean_testcase() :: [instance()].
%% TODO: rename to 'shrinker_state()'?
-type state() :: 'init' | 'done' | {'shrunk',position(),_} | term().
-type shrinker() :: fun((imm_instance(), type(), state()) ->
			    {[imm_instance()],state()}).
-type type_kind() :: 'basic' | 'wrapper' | 'constructed'
		   | 'semi_opaque' | 'opaque'.

-type type() :: {'$type', [type_prop()]}.
%% TODO: update raw_type() when adding more standard types
-type raw_type() :: type() | integer() | float() | atom() | tuple()
		  | maybe_improper_list(_,_) | <<_:_ * 1>>.
-type instance() :: term().
%% TODO: update imm_instance() when adding more types: be careful when reading
%%	 anything that returns it
-type imm_instance() :: raw_type() % TODO: is this correct?
		      | instance()
		      | {'$used', _, _}.

-type type_prop_name() :: 'kind' | 'generator' | 'size_limit' | 'size_transform'
			| 'is_instance' | 'shrinkers' | 'internal_type'
			| 'internal_types' | 'get_length' | 'split' | 'join'
			| 'get_indices' | 'remove' | 'retrieve' | 'update'
			| 'constraints' | 'parts_type' | 'combine' | 'alt_gens'.
-type type_prop_value() :: term().
-type type_prop() ::
      {'kind', type_kind()}
    | {'generator', generator()}
    | {'size_limit', size()}
    | {'size_transform', fun((size()) -> size())}
    | {'is_instance', fun((imm_instance()) -> ternary())}
    | {'shrinkers', [shrinker()]}
    | {'internal_type', raw_type()}
    | {'internal_types',
       tuple(type()) | maybe_improper_list(type(),type())}
      % The items returned by 'remove' must be of this type.
    | {'get_length', fun((imm_instance()) -> length())}
      % If this is a container type, this should return the number of elements
      % it contains.
    | {'split', fun((imm_instance()) -> [imm_instance()])
	      | fun((length(),imm_instance()) ->
		    {imm_instance(),imm_instance()})}
      % If present, the appropriate form depends on whether get_length is
      % defined: if get_length is undefined, this must be in the one-argument
      % form (e.g. a tree should be split into its subtrees), else it must be
      % in the two-argument form (e.g. a list should be split in two at the
      % index provided).
    | {'join', fun((imm_instance(),imm_instance()) -> imm_instance())}
    | {'get_indices', fun((imm_instance()) -> [index()])}
      % If this is a container type, this should return a list of indices we
      % can use to remove or insert elements from the given instance.
    | {'remove', fun((index(),imm_instance()) -> imm_instance())}
    | {'retrieve', fun((index(),imm_instance()) -> value())}
    | {'update', fun((index(),value(),imm_instance()) -> imm_instance())}
    | {'constraints', [{instance_test(), boolean()}]}
      % A list of constraints on instances of this type: each constraint is a
      % tuple of a fun that must return 'true' for each valid instance and a
      % boolean field that specifies whether the condition is strict.
    | {'parts_type', type()}
    | {'combine', combine_fun()}
    | {'alt_gens', alt_gens()}.

-type delayed_test() :: fun(() -> test()).
-type test() :: inner_test()
    	      | numtests_clause()
	      | fails_clause().
-type inner_test() :: boolean()
		    | forall_clause()
		    | {'$implies', boolean(), delayed_test()}
		    | collect_clause()
		    | {'$whenfail', side_effects_fun(), delayed_test()}
		    %| {'$trapexit', delayed_test()}
		    %| {'$timeout', time_period(), delayed_test()}
		    | {'$apply', [term()], function()}.
-type numtests_clause() :: {'$numtests', non_neg_integer(), test()}.
-type fails_clause() :: {'$fails', test()}.
-type forall_clause() :: {'$forall', raw_type(),
			  fun((instance()) -> inner_test())}.
-type forall2_clause() :: {'$forall2', raw_type(),
			   fun((instance()) -> inner_test())}.
-type collect_clause() :: {'$collect', category(), inner_test()}.

-record(opts, {quiet       = false :: boolean(),
	       numtests    = 100   :: non_neg_integer(),
	       max_shrinks = 300   :: non_neg_integer(),
	       expect_fail = false :: boolean(),
	       try_shrunk  = false :: boolean(),
	       shrunk              :: testcase()}).

-type opt() :: 'quiet'
	     | {'numtests', non_neg_integer()}
	     | non_neg_integer()
	     | {'max_shrinks', non_neg_integer()}
	     | 'expect_fail'.

-record(ctx, {bound        = [] :: testcase(),
	      fail_actions = [] :: [side_effects_fun()],
	      categories   = [] :: [category()]}).

-type single_run_result() :: {'passed', 'didnt_crash'}
			   | {'passed', {'categories',[category()]}}
			   | {'failed', fail_reason(), testcase(),
			      [side_effects_fun()]}
			   | {'error', 'wrong_type'}
			   | {'error', 'cant_generate'}
			   | {'error', 'rejected'}.

-type fail_reason() :: 'false_property' | {'throw',term()}.

-type result() :: {'passed', pos_integer(), [cat_dict()]}
		| {'failed', pos_integer(), fail_reason(), testcase()}
		| {'error', 'cant_generate'}
		| {'error', 'cant_satisfy'}
		| {'error', {'unexpected',single_run_result()}}.

-type final_result() :: {'passed', pos_integer(), [cat_dict()]}
		      | {'failed', pos_integer(), fail_reason(),
			 clean_testcase(), non_neg_integer(), clean_testcase()}
		      | {'error', 'cant_generate'}
		      | {'error', 'cant_satisfy'}
		      | {'error', {'unexpected',single_run_result()}}.
