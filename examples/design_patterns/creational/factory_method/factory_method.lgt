%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Factory method design pattern:
%
% https://en.wikipedia.org/wiki/Factory_method_pattern


% the example deals with maze games where each maze is composed by a set of
% possibly connected rooms; we start by defing two kinds of rooms: ordinary
% rooms and magic rooms

:- category(room).

	:- public(new/1).
	new(Room) :-
		self(Self),
		create_object(Room, [extends(Self)], [], []).

	:- public(connect/1).
	connect(Room) :-
		::assertz(connected_(Room)).

	:- public(describe/0).

	:- private(connected_/1).
	:- dynamic(connected_/1).

:- end_category.


:- object(magic_room,
	imports(room)).

	describe :-
		write('Magic room'), nl.

:- end_object.


:- object(ordinary_room,
	imports(room)).

	describe :-
		write('Ordinary room'), nl.

:- end_object.


% we now define two types of maze games, a magic maze game using
% magic rooms and an ordinary maze game composed by ordinary rooms

:- category(maze_game).

	% template method to create new maze games
	:- public(new/1).
	new(MazeGame) :-
		self(Self),
		create_object(MazeGame, [extends(Self)], [], []),
		MazeGame::make_room(Room1),
		MazeGame::make_room(Room2),
		Room1::connect(Room2),
		MazeGame::assertz(room_(Room1)),
		MazeGame::assertz(room_(Room2)).

	% factory method for creating rooms; to be defined by concrete
	% maze games, which can thus decide the kind of rooms to create
	:- public(make_room/1).

	:- public(play/0).
	play :-
        write('Playing using:'), nl,
		forall(
			::room_(Room),
			(write(Room), write(' - '), Room::describe)
		).

	:- private(room_/1).
	:- dynamic(room_/1).

:- end_category.


% magic maze games create magic rooms

:- object(magic_maze_game,
	imports(maze_game)).

	make_room(Room) :-
		magic_room::new(Room).

:- end_object.


% ordinary maze games create ordinary rooms

:- object(ordinary_maze_game,
	imports(maze_game)).

	make_room(Room) :-
		ordinary_room::new(Room).

:- end_object.
