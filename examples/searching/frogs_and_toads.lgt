%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(frogs_and_toads,
	instantiates(heuristic_state_space)).

	:- info([
		version is 0:0:0,
		author is 'Paulo Moura',
		date is 2018-12-09,
		comment is 'Frogs and toads puzzle.'
	]).

	:- uses(numberlist, [min/2, max/2]).
	:- uses(set, [insert/3, insert_all/3, select/3]).

	initial_state(start, s([toad,toad,toad], empty, [frog,frog,frog])).

	goal_state(end, s([frog,frog,frog], empty, [toad,toad,toad])).

Only one amphibian (i.e. frog or toad) can move at a time.
Frogs can only move to the left and toads can only move to the right.
Each move is ether a crawl or a hop.
A crawl is a move to an adjacent empty space.
A hop is a move to an empty space that is two spaces away from the starting space, such that the space between the start and end of the hop is occupied by another amphibian.
Frogs can only hop over toads and toads can only hop over frogs.


	% two persons
	next_state(s(Left0, left, Right0), s(Left, right, Right), Cost) :-
		select(Person1, Left0, Left1),
		select(Person2, Left1, Left),
		insert_all([Person1, Person2], Right0, Right),
		Cost is max(Person1, Person2).
	% one person
	next_state(s(Left0, left, Right0), s(Left, right, Right), Person) :-
		select(Person, Left0, Left),
		insert(Right0, Person, Right).
	% reverse
	next_state(s(Left0, right, Right0), s(Left, left, Right), Cost) :-
		next_state(s(Right0, left, Left0), s(Right, right, Left), Cost).

	heuristic(s(Left, Lamp, Right), Heuristic) :-
		(	Lamp = left ->
			min(Left, Heuristic)
		;	min(Right, Heuristic)
		).

	print_state(s(Left, Lamp, Right)) :-
		write_list(Left),
		(	Lamp = left ->
			write(' lamp _|____________|_ ')
		;	write(' _|____________|_ lamp ')
		),
		write_list(Right),
		nl.

	write_list([]).
	write_list([Head| Tail]) :-
		write(Head), write(' '),
		write_list(Tail).

:- end_object.
