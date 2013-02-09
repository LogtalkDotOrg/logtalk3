%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
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
