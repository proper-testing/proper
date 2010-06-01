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
% This header is included in all PropEr module source files.


%% Constants

-define(DEFAULT_RNG_CONST, 10).
-define(MAX_TRIES_TO_SATISFY_CONSTRAINTS, 50).
-define(MAX_RANDOM_TRIES_WHEN_SHRINKING, 5).
-define(MAX_LIST_LEN, 200). % TODO: make this into a parameter


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
-define(SUBTYPE(Type,PropList), proper_types:subtype(Type,PropList)).


%% Dialyzer Types

-type extnum() :: 'plus_inf' | 'minus_inf' | number().
-type ternary() :: 'true' | 'false' | 'unknown'.
-type size() :: non_neg_integer().
-type length() :: non_neg_integer().
-type position() :: pos_integer().
-type frequency() :: pos_integer().

-type type() :: {'$type', [type_prop()]}.
% TODO: update raw_type() when adding more standard types
-type raw_type() :: type() | integer() | float() | atom() | tuple()
		  | maybe_improper_list(_,_) | <<_:_ * 1>>.
-type instance() :: term().
% TODO: update imm_instance() when adding more types: be careful when reading
%	anything that returns it
-type imm_instance() :: raw_type()
		      | instance()
		      | {'$used', _, _}.
-type index() :: term().
-type value() :: term().
-type parts() :: term().
-type state() :: 'init' | 'done' | {'shrunk',position(),_} | term().

-type type_prop() ::
      {'kind', 'basic' | 'wrapper' | 'constructed' | 'semi_opaque' | 'opaque'}
    | {'generator', fun((size()) -> imm_instance()) | fun(() -> imm_instance())}
    | {'size_limit', size()}
    | {'size_transform', fun((size()) -> size())}
    | {'is_instance', fun((term()) -> ternary())}
    | {'shrinkers',
       [fun((imm_instance(),type(),state()) -> {[imm_instance()],state()})]}
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
    | {'constraints', [{fun((instance()) -> boolean()), boolean()}]}
      % A list of constraints on instances of this type: each constraint is a
      % tuple of a fun that must return 'true' for each valid instance and a
      % boolean field that specifies whether the condition is strict.
    | {'parts_type', type()}
    | {'combine', fun((parts()) -> imm_instance())}
    | {'alt_gens', fun(() -> [imm_instance()])}.

-record(opts, {quiet       = false :: boolean(),
	       numtests    = 100   :: pos_integer(),
	       max_shrinks = 300   :: non_neg_integer(),
	       expect_fail = false :: boolean(),
	       try_shrunk  = false :: boolean(),
	       shrunk              :: [imm_instance()]}).

-record(ctx, {bound        = [] :: [term()],
	      fail_actions = [] :: [fun(() -> _)],
	      categories   = [] :: [term()]}).
