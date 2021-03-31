%%% -*- coding: utf-8; erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2017-2021 Andreas Löscher <andreas.loscher@it.uu.se>
%%%                     and Kostis Sagonas <kostis@it.uu.se>
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

%%% @copyright 2017-2021 Andreas Löscher and Kostis Sagonas
%%% @version {@version}
%%% @author Andreas Löscher and Kostis Sagonas

-module(labyrinth).
-export([maze/1]).
-export([prop_exit_random/1,
	 prop_exit_targeted_user/1,
	 prop_exit_targeted_auto/1]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type pos()     :: {integer(), integer()}.
-type brick()   :: wall | exit | entrance.
-type maze()    :: list(string()).
-type mazemap() :: #{pos() => brick(), exit := pos(), entrance := pos()}.
-type step()    :: left | right | up | down.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mazes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec maze(0..2) -> maze().
maze(0) ->
  ["#########",
   "#X     E#",
   "#########"];
maze(1) ->
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
   "######################################################################"];
maze(2) ->
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

-spec draw_map(maze()) -> mazemap().
draw_map(Maze) ->
  draw_map(Maze, #{}, 0).

draw_map([], Acc, _) -> Acc;
draw_map([Line | T], Acc, X) ->
  NewAcc = draw_line(Line, Acc, X, 0),
  draw_map(T, NewAcc, X + 1).

draw_line([], Acc, _, _) -> Acc;
draw_line([$ | T], Acc, X, Y) ->
  draw_line(T, Acc, X, Y + 1);
draw_line([$# | T], Acc, X, Y) ->
  draw_line(T, Acc#{{X, Y} => wall}, X, Y + 1);
draw_line([$X | T], Acc, X, Y) ->
  draw_line(T, Acc#{{X, Y} => exit, exit => {X, Y}}, X, Y + 1);
draw_line([$E | T], Acc, X, Y) ->
  draw_line(T, Acc#{{X, Y} => entrance, entrance => {X, Y}}, X, Y + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Movement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec try_move_a_step(pos(), step(), mazemap()) -> pos().
try_move_a_step(Pos = {X, Y}, Step, MazeMap) ->
  NextPos = case Step of
              left -> {X, Y - 1};
              right -> {X, Y + 1};
              up -> {X - 1, Y};
              down -> {X + 1, Y}
            end,
  case MazeMap of
    #{NextPos := wall} -> Pos;
    _ -> NextPos
  end.

-spec follow_path(pos(), [step()], mazemap()) -> pos() | {exited, pos()}.
follow_path(Start, Path, MazeMap) ->
  #{exit := Exit} = MazeMap,
  lists:foldl(fun (_, Final = {exited, _}) -> Final;
                  (Step, CurrPos) ->
                  case try_move_a_step(CurrPos, Step, MazeMap) of
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

path_next() ->
  fun (PrevPath, _) ->
      ?LET(NextSteps, vector(20, step()), PrevPath ++ NextSteps)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Properties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop_exit_random(Maze) ->
  MazeMap = draw_map(Maze),
  #{entrance := Entrance} = MazeMap,
  ?FORALL(Path, path(),
          case follow_path(Entrance, Path, MazeMap) of
            {exited, _} -> false;
            _ -> true
          end).

prop_exit_targeted_user(Maze) ->
  MazeMap = draw_map(Maze),
  #{entrance := Entrance, exit := Exit} = MazeMap,
  ?FORALL_TARGETED(Path, ?USERNF(path(), path_next()),
                   case follow_path(Entrance, Path, MazeMap) of
                     {exited, _Pos} -> false;
                     Pos ->
                       case length(Path) > 2000 of
                         true ->
                           proper_target:reset(),
                           true;
                         _ ->
                           UV = distance(Pos, Exit),
                           ?MINIMIZE(UV),
                           true
                       end
                   end).

prop_exit_targeted_auto(Maze) ->
  MazeMap = draw_map(Maze),
  #{entrance := Entrance, exit := Exit} = MazeMap,
  ?FORALL_TARGETED(Path, path(),
                   case follow_path(Entrance, Path, MazeMap) of
                     {exited, _Pos} -> false;
                     Pos ->
                       UV = distance(Pos, Exit),
                       ?MINIMIZE(UV),
                       true
                   end).

distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).


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
-define(_failsWith(ExpectedCExm, Test), ?_failsWith(ExpectedCExm, Test, [])).
-define(_failsWith(ExpectedCExm, Test, Opts),
        ?_test(
           begin
             Result = proper:quickcheck(Test, Opts),
             CExm = proper:counterexample(),
             proper:clean_garbage(),
             ?assertNot(Result),
             ?assertMatch(ExpectedCExm, CExm),
             ?checkCExm(CExm, Test, Opts)
           end)).
-define(checkCExm(CExm, Test, Opts),
        ?assertNot(proper:check(Test, CExm, Opts))).

labyrinth_props_test_() ->
  FailOpts = [{numtests,7500}, noshrink],        % see comment above
  M0 = labyrinth:maze(0), M1 = labyrinth:maze(1), M2 = labyrinth:maze(2),
  SixLeft = [[left,left,left,left,left,left]],
  [{"Random", ?_failsWith(SixLeft, prop_exit_random(M0), [500])},
   {"Targeted user Maze 0", ?_failsWith(SixLeft, prop_exit_targeted_user(M0))},
   {timeout, 42,
    {"Targeted user Maze 1", ?_fails(prop_exit_targeted_user(M1), FailOpts)}},
   {timeout, 42,
    {"Targeted user Maze 2", ?_fails(prop_exit_targeted_user(M2), FailOpts)}},
   {timeout, 42,
    {"Targeted user Maze 3", ?_fails(prop_exit_targeted_auto(M2), FailOpts)}}].
