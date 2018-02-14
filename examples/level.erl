-module(level).
-export([level0/0, level1/0, level2/0, build_level/1]).
-export([prop_exit/1, prop_exit_targeted/1]).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type pos() :: {integer(), integer()}.
-type brick() :: wall | exit | entrance.
-type level_data() :: [string()].
-type level() :: #{pos() => brick(),
                   exit=>pos(),
                   entrance => pos()}.
-type step() :: left | right | up | down.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Levels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec level0() -> level_data().
level0() ->
  ["#########",
   "#X     E#",
   "#########"].

-spec level1() -> level_data().
level1() ->
  ["######################################################################",
   "#                                                                    #",
   "#   E                                                                #",
   "#                                  #####                             #",
   "#                                  #####                             #",
   "#        #####                     #####        #####                #",
   "#        #####                                  #####                #",
   "#        #####                                  #####                #",
   "#                          #####                                     #",
   "#                          #####                                     #",
   "#                          #####                                     #",
   "#                                         #####          ##########  #",
   "#                                         #####          ##########  #",
   "#             #####                       #####          ##########  #",
   "#             #####                                                  #",
   "#             #####                                                  #",
   "#                                #####                               #",
   "#                                #####                               #",
   "#                                #####         #####                 #",
   "#                                              #####                 #",
   "#                                              #####                 #",
   "#                                                              X     #",
   "#                                                                    #",
   "######################################################################"].


-spec level2() -> level_data().
level2() ->
  ["######################################################################",
   "#                                                                    #",
   "#    X                                                               #",
   "#                                                                    #",
   "#          #             ########   #####     ####   ########        #",
   "#          ###              ##      ##   #    ##  #     ##           #",
   "################            ##      #####     ####      ##           #",
   "#          ###              ##      ##        ##  #     ##           #",
   "#          #                ##      ##        ####      ##           #",
   "#                                                                    #",
   "#                                                                    #",
   "#                   #                                                #",
   "#                   #                                                #",
   "#                   #                #################################",
   "#                   #                                                #",
   "#                   #                                                #",
   "#                   #                                                #",
   "#                   ####################################             #",
   "#                                                                    #",
   "#                                                                    #",
   "################################                                     #",
   "#                                     E                              #",
   "#                                                                    #",
   "######################################################################"].

-spec build_level(list(string())) -> level().
build_level(Data) ->
  build_level(Data, #{}, 0).

build_level([], Acc, _) -> Acc;
build_level([Line | T], Acc, X) ->
  NewAcc = build_level_line(Line, Acc, X, 0),
  build_level(T, NewAcc, X + 1).

build_level_line([], Acc, _, _) -> Acc;
build_level_line([$ | T], Acc, X, Y) ->
  build_level_line(T, Acc, X, Y + 1);
build_level_line([$# | T], Acc, X, Y) ->
  build_level_line(T, Acc#{{X, Y} => wall}, X, Y + 1);
build_level_line([$X | T], Acc, X, Y) ->
  build_level_line(T, Acc#{{X, Y} => exit, exit => {X, Y}}, X, Y + 1);
build_level_line([$E | T], Acc, X, Y) ->
  build_level_line(T, Acc#{{X, Y} => entrance, entrance => {X, Y}}, X, Y + 1);
build_level_line(_, _, _, _) ->
  error(level_data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Movement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec do_step(pos(), step(), level()) -> pos().
do_step(Pos = {X, Y}, Step, Level) ->
  NextPos = case Step of
              left -> {X, Y - 1};
              right -> {X, Y + 1};
              up -> {X - 1, Y};
              down -> {X + 1, Y}
            end,
  case Level of
    #{NextPos := wall} -> Pos;
    _ -> NextPos
  end.

-spec follow_path(pos(), [step()], level()) -> pos() | {exited, pos()}.
follow_path(Start, Path, Level) ->
  #{exit := Exit} = Level,
  lists:foldl(fun (_, Final = {exited, _}) -> Final;
                  (Step, CurrPos) ->
                  case do_step(CurrPos, Step, Level) of
                    Exit -> {exited, Exit};
                    NewPos -> NewPos
                  end
              end, Start, Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
step() ->
  oneof([left, right, up, down]).

path() ->
  list(step()).

path_sa() ->
  #{first => path(),
    next => path_next()}.

path_next() ->
  fun (PrevPath, _) ->
      ?LET(NextSteps, vector(20, step()), PrevPath ++ NextSteps)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Properties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_exit(LevelData) ->
  Level = build_level(LevelData),
  #{entrance := Entrance} = Level,
  ?FORALL(Path, path(),
          case follow_path(Entrance, Path, Level) of
            {exited, _} -> false;
            _ -> true
          end).

prop_exit_targeted(LevelData) ->
  Level = build_level(LevelData),
  #{entrance := Entrance} = Level,
  #{exit := Exit} = Level,
  ?FORALL_SA(Path, ?TARGET(path_sa()),
             case follow_path(Entrance, Path, Level) of
               {exited, _Pos} -> false;
               Pos ->
                 case length(Path) > 500 of
                   true -> proper_sa:reset(), true;
                   _ ->
                     UV = distance(Pos, Exit),
                     ?MINIMIZE(UV),
                     true
                 end
             end).

distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).
