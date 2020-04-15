%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2017-2020, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                     and  Kostis Sagonas <kostis@it.uu.se>
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

%%% @copyright 2017-2020 Andreas Löscher and Kostis Sagonas
%%% @version {@version}
%%% @author Andreas Löscher and Kostis Sagonas

-module(labyrinth).
-export([maze/1]).
-export([prop_exit/1, prop_exit_user_targeted/1, prop_exit_auto_targeted/1]).

-include_lib("proper/include/proper.hrl").

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

prop_exit(Maze) ->
  MazeMap = draw_map(Maze),
  #{entrance := Entrance} = MazeMap,
  ?FORALL(Path, path(),
          case follow_path(Entrance, Path, MazeMap) of
            {exited, _} -> false;
            _ -> true
          end).

prop_exit_user_targeted(Maze) ->
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

prop_exit_auto_targeted(Maze) ->
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
