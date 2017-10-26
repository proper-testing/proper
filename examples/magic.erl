-module(magic).
-export([spells/0, apply_spell/2, apply_spells/2]).

-include_lib("proper/include/proper.hrl").

-record(stats, {strength     = 0,
                constitution = 0,
                defense      = 0,
                dexterity    = 0,
                intelligence = 0,
                charisma     = 0,
                wisdom       = 0,
                willpower    = 0,
                perception   = 0,
                luck         = 0}).

-type stats() :: #stats{}.
-type spell() :: stats().

-spec spells() -> list(spell()).
spells() ->
  [#stats{strength = 5,  constitution = -2, defense = 0,  dexterity = -3, intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   #stats{strength = 0,  constitution = 0,  defense = 4,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = -4, perception = 0,  luck = 0},
   #stats{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = -3, charisma = 1,  wisdom = 1,  willpower = 0,  perception = 1,  luck = 0},
   #stats{strength = 1,  constitution = 1,  defense = 1,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = -3, perception = 0,  luck = 0},
   #stats{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 2,  charisma = 2,  wisdom = 2,  willpower = -6, perception = 0,  luck = 0},
   #stats{strength = 3,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = -6, charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 3},
   #stats{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 2,  charisma = 0,  wisdom = 0,  willpower = -2, perception = 0,  luck = 0},
   #stats{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 2,  wisdom = 0,  willpower = 0,  perception = 3,  luck = -5},
   #stats{strength = 2,  constitution = -2, defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   #stats{strength = 0,  constitution = 2,  defense = -2, dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   #stats{strength = 0,  constitution = 0,  defense = 2,  dexterity = -2, intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   #stats{strength = 0,  constitution = 0,  defense = 0,  dexterity = 2,  intelligence = -2,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   %% #stats{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = -2, charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   #stats{strength = -1, constitution = -1, defense = -1, dexterity = -1, intelligence = -1, charisma = -1, wisdom = -1, willpower = 10, perception = -1, luck = -1},
   #stats{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 2,  charisma = -2, wisdom = 0,  willpower = 0,  perception = 0,  luck = 0},
   #stats{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 2,  wisdom = -2, willpower = 0,  perception = 0,  luck = 0},
   #stats{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 2,  willpower = -2, perception = 0,  luck = 0},
   #stats{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 2,  perception = -2, luck = 0},
   #stats{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 2,  luck = -2},
   %% #stats{strength = 0,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 2},
   #stats{strength = -2,  constitution = 0,  defense = 0,  dexterity = 0,  intelligence = 0,  charisma = 0,  wisdom = 0,  willpower = 0,  perception = 0,  luck = 2}].

-spec apply_spell(stats(), spell()) -> stats().
apply_spell(State, Spell) ->
  NewState = State#stats{strength     = State#stats.strength     + Spell#stats.strength,
                         constitution = State#stats.constitution + Spell#stats.constitution,
                         defense      = State#stats.defense      + Spell#stats.defense,
                         dexterity    = State#stats.dexterity    + Spell#stats.dexterity,
                         intelligence = State#stats.intelligence + Spell#stats.intelligence,
                         charisma     = State#stats.charisma     + Spell#stats.charisma,
                         wisdom       = State#stats.wisdom       + Spell#stats.wisdom,
                         willpower    = State#stats.willpower    + Spell#stats.willpower,
                         perception   = State#stats.perception   + Spell#stats.perception,
                         luck         = State#stats.luck         + Spell#stats.luck},
  if
    NewState#stats.strength < 0 -> State;
    NewState#stats.constitution < 0 -> State;
    NewState#stats.defense < 0 -> State;
    NewState#stats.dexterity < 0 -> State;
    NewState#stats.intelligence < 0 -> State;
    NewState#stats.charisma < 0 -> State;
    NewState#stats.wisdom < 0 -> State;
    NewState#stats.willpower < 0 -> State;
    NewState#stats.perception < 0 -> State;
    NewState#stats.luck < 0 -> State;
    true -> NewState
  end.

apply_spells(State, []) -> State;
apply_spells(State, [Spell | LeftSpells]) ->
  apply_spells(apply_spell(State, Spell), LeftSpells).

list_of_spells() ->
  list(proper_types:noshrink(oneof(spells()))).

prop_spells() ->
  ?FORALL(Spells, list_of_spells(),
          begin
            InitialStats = #stats{strength     = 10,
                                  constitution = 10,
                                  defense      = 10,
                                  dexterity    = 10,
                                  intelligence = 10,
                                  charisma     = 10,
                                  wisdom       = 10,
                                  willpower    = 10,
                                  perception   = 10,
                                  luck         = 10},
            BuffedStats = apply_spells(InitialStats, Spells),
            SumStats = BuffedStats#stats.strength + BuffedStats#stats.constitution + BuffedStats#stats.defense
            + BuffedStats#stats.dexterity + BuffedStats#stats.intelligence + BuffedStats#stats.charisma
            + BuffedStats#stats.wisdom + BuffedStats#stats.willpower+ BuffedStats#stats.perception
            + BuffedStats#stats.luck,
            ?WHENFAIL(io:format("Number of Spells: ~p~nTotal Stats: ~p~n", [length(Spells), SumStats]), SumStats < 200)
    end).

prop_spells_targeted() ->
  ?FORALL_SA(Spells, ?TARGET(#{gen => list_of_spells()}),
          begin
            InitialStats = #stats{strength     = 10,
                                  constitution = 10,
                                  defense      = 10,
                                  dexterity    = 10,
                                  intelligence = 10,
                                  charisma     = 10,
                                  wisdom       = 10,
                                  willpower    = 10,
                                  perception   = 10,
                                  luck         = 10},
            BuffedStats = apply_spells(InitialStats, Spells),
            SumStats = BuffedStats#stats.strength + BuffedStats#stats.constitution + BuffedStats#stats.defense
            + BuffedStats#stats.dexterity + BuffedStats#stats.intelligence + BuffedStats#stats.charisma
            + BuffedStats#stats.wisdom + BuffedStats#stats.willpower+ BuffedStats#stats.perception
            + BuffedStats#stats.luck,
            ?MAXIMIZE(SumStats),
            ?WHENFAIL(io:format("Number of Spells: ~p~nTotal Stats: ~p~n", [length(Spells), SumStats]), SumStats < 200)
    end).
