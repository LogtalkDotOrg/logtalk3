%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(bridge,
	instantiates(heuristic_state_space)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2004/8/15,
		comment is 'Bridge puzzle.'
	]).

	:- uses(list, [append/3]).
	:- uses(numberlist, [min/2, max/2]).
	:- uses(set, [insert/3, insert_all/3, select/3]).

	initial_state(start, ([], right, [1,3,6,8,12])).

	goal_state(end, ([1,3,6,8,12], left, [])).

	next_state((Left1, left, Right1), (Left2, right, Right2), Slower) :-	% two persons
		append(List, [Person1| Persons], Left1),
		select(Person2, Persons, Others),
		append(List, Others, Left2),
		insert_all([Person1, Person2], Right1, Right2),
		(	Person1 > Person2 ->
			Slower = Person1
		;	Slower = Person2
		).
	next_state((Left1, right, Right1), (Left2, left, Right2), Slower) :-	% two persons
		append(List, [Person1| Persons], Right1),
		select(Person2, Persons, Others),
		append(List, Others, Right2),
		insert_all([Person1, Person2], Left1, Left2),
		(	Person1 > Person2 ->
			Slower = Person1
		;	Slower = Person2
		).
	next_state((Left1, left, Right1), (Left2, right, Right2), Person) :-	% one person
		select(Person, Left1, Left2),
		insert(Right1, Person, Right2).
	next_state((Left1, right, Right1), (Left2, left, Right2), Person) :-	% one person
		select(Person, Right1, Right2),
		insert(Left1, Person, Left2).

	heuristic((Left, Lamp, Right), Heuristic) :-
		(	Lamp = left ->
			list::min(Left, Heuristic)
		;	list::max(Right, Max),
			list::min(Right, Min),
			Heuristic is Max + Min
		).

	print_state((Left, Lamp, Right)) :-
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
