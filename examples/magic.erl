%%% -*- coding: utf-8; erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2017, Andreas Löscher <andreas.loscher@it.uu.se>
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

%%% @copyright 2017 Andreas Löscher
%%% @version {@version}
%%% @author Andreas Löscher

-module(magic).
-export([spells/0, cast_spell/2, cast_spells/2, count_spells/1]).
-export([run_random/1, run_targeted_auto/1, run_targeted_user/1]).


-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(attr, {strength     = 0 :: integer(),
               constitution = 0 :: integer(),
               defense      = 0 :: integer(),
               dexterity    = 0 :: integer(),
               intelligence = 0 :: integer(),
               charisma     = 0 :: integer(),
               wisdom       = 0 :: integer(),
               willpower    = 0 :: integer(),
               perception   = 0 :: integer(),
               luck         = 0 :: integer()}).

-type attr() :: #attr{}.
-type spell() :: attr().

-spec spells() -> list(spell()).
spells() ->
  [#attr{strength = 5, constitution = -2, defense = 0,  dexterity = -3, intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   #attr{strength = 0,  constitution = 0,  defense = 4,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = -4, perception = 0,  luck = 0},
   #attr{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = -3, charisma = 1,  wisdom = 1,  willpower = 0,  perception = 1,  luck = 0},
   #attr{strength = 1,  constitution = 2,  defense = 2,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = -3, perception = 0,  luck = 0},
   #attr{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 1,  charisma = 1,  wisdom = 1,  willpower = -3, perception = 0,  luck = 0},
   #attr{strength = 1,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = -3, charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 2},
   #attr{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 1,  charisma = 1,  wisdom = 1,  willpower = -4, perception = 1,  luck = 0},
   #attr{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 2,  wisdom = 0,  willpower = 0,  perception = 1,  luck = -3},
   #attr{strength = 2,  constitution = -2, defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   #attr{strength = 0,  constitution = 2,  defense = -2, dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   #attr{strength = 0,  constitution = 0,  defense = 2,  dexterity = -2, intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   #attr{strength = 0,  constitution = 0,  defense = 0,  dexterity = 2,  intelligence = -2,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   #attr{strength = -1, constitution = -1, defense = -1, dexterity = -1, intelligence = -1, charisma = -1, wisdom = -1, willpower = 10, perception = -1, luck = -1},
   #attr{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 2,  charisma = -2, wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   #attr{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 2,  wisdom = -2, willpower = 0,  perception = 0,  luck = 0},
   #attr{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 2,  willpower = -2, perception = 0,  luck = 0},
   #attr{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 2,  perception = -2, luck = 0},
   #attr{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 2,  luck = -2},
   #attr{strength = -2,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 2},
   #attr{strength = 5,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = -8}].

%% no penalty for wrongly casted spells
-spec cast_spell(attr(), spell()) -> attr().
cast_spell(Attrs, Spell) ->
  NewAttrs = raw_cast_spell(Attrs, Spell),
  if
    NewAttrs#attr.strength < 0 -> Attrs;
    NewAttrs#attr.constitution < 0 -> Attrs;
    NewAttrs#attr.defense < 0 -> Attrs;
    NewAttrs#attr.dexterity < 0 -> Attrs;
    NewAttrs#attr.intelligence < 0 -> Attrs;
    NewAttrs#attr.charisma < 0 -> Attrs;
    NewAttrs#attr.wisdom < 0 -> Attrs;
    NewAttrs#attr.willpower < 0 -> Attrs;
    NewAttrs#attr.perception < 0 -> Attrs;
    NewAttrs#attr.luck < 0 -> Attrs;
    true -> NewAttrs
  end.

raw_cast_spell(Attrs, Spell) ->
  Attrs#attr{strength     = Attrs#attr.strength     + Spell#attr.strength,
             constitution = Attrs#attr.constitution + Spell#attr.constitution,
             defense      = Attrs#attr.defense      + Spell#attr.defense,
             dexterity    = Attrs#attr.dexterity    + Spell#attr.dexterity,
             intelligence = Attrs#attr.intelligence + Spell#attr.intelligence,
             charisma     = Attrs#attr.charisma     + Spell#attr.charisma,
             wisdom       = Attrs#attr.wisdom       + Spell#attr.wisdom,
             willpower    = Attrs#attr.willpower    + Spell#attr.willpower,
             perception   = Attrs#attr.perception   + Spell#attr.perception,
             luck         = Attrs#attr.luck         + Spell#attr.luck}.

cast_spells(Attrs, []) -> Attrs;
cast_spells(Attrs, [Spell | LeftSpells]) ->
  cast_spells(cast_spell(Attrs, Spell), LeftSpells).

%% Properties
%% ----------

initial_attr() ->
    #attr{strength  = 5, constitution = 5, defense    = 5,
	  dexterity = 5, intelligence = 5, charisma   = 5,
	  wisdom    = 5, willpower    = 5, perception = 5, luck = 5}.

sum_attr(Attrs) ->
  Attrs#attr.strength + Attrs#attr.constitution +
    Attrs#attr.defense + Attrs#attr.dexterity +
    Attrs#attr.intelligence + Attrs#attr.charisma +
    Attrs#attr.wisdom + Attrs#attr.willpower +
    Attrs#attr.perception + Attrs#attr.luck.

list_of_spells() ->
  list(proper_types:noshrink(oneof(spells()))).

prop_spells_random() ->
  ?FORALL(Spells, list_of_spells(),
          begin
            InitialAttr = initial_attr(),
            BuffedAttr = cast_spells(InitialAttr, Spells),
            SumAttr = sum_attr(BuffedAttr),
            ?WHENFAIL(io:format("Number of Spells: ~p~nTotal Attr: ~p~n",
                                [length(Spells), SumAttr]),
                      SumAttr < 2 * sum_attr(InitialAttr))
          end).

prop_spells_targeted_auto() ->
  ?FORALL_TARGETED(Spells, list_of_spells(),
                   begin
                     InitialAttr = initial_attr(),
                     BuffedAttr = cast_spells(InitialAttr, Spells),
                     SumAttr = sum_attr(BuffedAttr),
                     ?MAXIMIZE(SumAttr),
                     ?WHENFAIL(io:format("Number of Spells: ~p~nTotal Attr: ~p~n",
                                         [length(Spells), SumAttr]),
                               SumAttr < 2 * sum_attr(InitialAttr))
                   end).

prop_spells_targeted_user() ->
  ?FORALL_TARGETED(Spells, ?USERNF(list_of_spells(), list_of_spells_next()),
                   begin
                     InitialAttr = initial_attr(),
                     BuffedAttr = cast_spells(InitialAttr, Spells),
                     SumAttr = sum_attr(BuffedAttr),
                     ?MAXIMIZE(SumAttr),
                     ?WHENFAIL(io:format("Number of Spells: ~p~nTotal Attr: ~p~n",
                                         [length(Spells), SumAttr]),
                               SumAttr < 2 * sum_attr(InitialAttr))
                   end).

list_of_spells_next() ->
  Del = 10,
  Add = 40,
  fun (Base, T) ->
      ?LET(OP, oneof([del_run, add_run]),
           case OP of
             del_run -> delete_some_spells(Base, Del, T);
             add_run -> add_some_spells(Base, Add, T)
           end)
  end.

add_some_spells(Spells, Percentage, _) ->
  NumAdd = max(1, trunc(length(Spells) * Percentage / 100)),
  ?LET(AddIndices, indices(NumAdd, 0, length(Spells)),
       begin
         %% have to be able to insert elements in the front of the list
         {Spells2, AddIndices2} = case AddIndices of
                                    [0 | NormalIndices] ->
                                      {[oneof(spells()) | Spells], NormalIndices};
                                    _ ->
                                      {Spells, AddIndices}
                                  end,
         %% handle rest of the list
         {_, [], NewSpells} = lists:foldl(fun (S, {I, [], Acc}) ->
                                              {I + 1, [], [S | Acc]};
                                              %% next function head
                                              (S, {I, AllIds = [DI |DIs], Acc}) ->
                                              case I =:= DI of
                                                true -> {I + 1, DIs, [S, oneof(spells()) |Acc]};
                                                false -> {I + 1, AllIds, [S | Acc]}
                                              end
                                          end, {1, AddIndices2, []}, Spells2),
         lists:reverse(NewSpells)
       end).

delete_some_spells(Spells, Percentage, _) ->
  NumDel = trunc(length(Spells) * Percentage / 100),
  ?LET(DelIndices, indices(NumDel, 1, length(Spells)),
       begin
         %% make sure that every element in the list can be deleted
         {_, [], NewSpells} = lists:foldl(fun (S, {I, [], Acc}) ->
                                              {I + 1, [], [S | Acc]};
                                              %% next function head
                                              (S, {I, AllDIs = [DI | DIs], Acc}) ->
                                              case I =:= DI of
                                                true -> {I + 1, DIs, Acc};
                                                false -> {I + 1, AllDIs, [S | Acc]}
                                              end
                                          end, {1, DelIndices, []}, Spells),
         lists:reverse(NewSpells)
       end).

indices(Num, Low, High) ->
  indices(Num, Low, High, []).

indices(0, _, _, Acc) -> lists:sort(Acc);
indices(N, Low, High, Acc) ->
  ?LET(I, index(Low, High, Acc),
       ?LAZY(indices(N-1, Low, High, [I | Acc]))).

index(Low, High, Blacklist) ->
  ?SUCHTHAT(I, integer(Low, High), not lists:member(I, Blacklist)).

%% utility functions
run_random(N) ->
  proper:quickcheck(prop_spells_random(), [{numtests, N}]).

run_targeted_auto(N) ->
  proper:quickcheck(prop_spells_targeted_auto(), [{numtests, N}, noshrink]).

run_targeted_user(N) ->
  proper:quickcheck(prop_spells_targeted_user(), [{numtests, N}, noshrink]).

count_spells(Spells) ->
  CountedSpells = maps:to_list(count_spells(Spells, #{})),
  lists:sort(fun ({_, A},{_, B}) -> A > B end, CountedSpells).

count_spells([], Acc) -> Acc;
count_spells([H|T], Acc) ->
  NewAcc = case Acc of
             #{H := Count} -> Acc#{H => Count + 1};
             _ -> Acc#{H => 1}
           end,
  count_spells(T, NewAcc).


%% -----------------------------------------------------------------------------
%% EUnit tests
%% -----------------------------------------------------------------------------

-define(_passes(Test),       ?_passes(Test, [])).
-define(_passes(Test, Opts), ?_assert(proper:quickcheck(Test, Opts))).
-define(_fails(Test, Opts),
        ?_test(
           begin
             Result = proper:quickcheck(Test, Opts),
             CExm = proper:counterexample(),
             proper:clean_garbage(),
             ?assertNot(Result),
             ?checkCExm(CExm, Test, Opts)
           end)).
-define(checkCExm(CExm, Test, Opts),
        ?assertNot(proper:check(Test, CExm, Opts))).

magic_props_test_() ->
  %% no point shrinking tests executed only for checking that they fail
  FailOpts = [{numtests,10000}, noshrink],
  [{"Random", ?_passes(prop_spells_random(), [500])},% let's hope we are unlucky
   {timeout, 60,
    {"Targeted auto", ?_fails(prop_spells_targeted_auto(), FailOpts)}},
   {timeout, 60,
    {"Targeted user", ?_fails(prop_spells_targeted_user(), FailOpts)}}].
