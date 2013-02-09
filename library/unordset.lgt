%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- object(unordset,
	implements(setp),
	extends(compound)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/4/22,
		comment is 'Set predicates implemented using non-ordered lists. Uses ==/2 for element comparison and standard term ordering.'
	]).

	delete([], _, []).
	delete([Head| Tail], Element, Remaining) :-
		(	Head == Element ->
			true
		;	delete(Tail, Element, Remaining)
		).

	disjoint(Set1, Set2) :-
		\+ (member(Element, Set1), member(Element, Set2)).

	equal([], []).
	equal([Element1| Set1], [Element2| Set2]) :-
		Element1 == Element2,
		equal(Set1, Set2).

	empty(Set) :-
		Set == [].

	insert(Set, Element, NewSet) :-
		union([Element], Set, NewSet).

	insert_all([], Set, Set).
	insert_all([Element| Tail], Set, NewSet) :-
		union([Element], Set, Set2),
		insert_all(Tail, Set2, NewSet).

	intersect(Set1, Set2) :-
		\+ \+ (member(Element, Set1), member(Element, Set2)).

	intersection([], _, []).
	intersection([Element1| Set1], Set2, Intersection) :-
		(	member(Element1, Set2) ->
			Intersection = [Element1| Tail],
			intersection(Set1, Set2, Tail)
		;	intersection(Set1, Set2, Intersection)
		).

	length(Set, Length) :-
		length(Set, 0, Length).

	length([], Length, Length).
	length([_| Set], Acc, Length) :-
		Acc2 is Acc + 1,
		length(Set, Acc2, Length).

	member(Element, [Element| _]).
	member(Element, [_| Set]) :-
		member(Element, Set).

	new([]).

	powerset(Set, PowerSet):-
		reverse(Set, RSet),
		powerset_1(RSet, [[]], PowerSet).

	powerset_1([], PowerSet, PowerSet).
	powerset_1([X| Xs], Yss0, Yss) :-
		powerset_2(Yss0, X, Yss1),
		powerset_1(Xs, Yss1, Yss).

	powerset_2([], _, []).
	powerset_2([Zs| Zss], X, [Zs, [X| Zs]| Yss]) :-
		powerset_2(Zss, X, Yss).

	reverse(List, Reversed) :-
		reverse(List, [], Reversed).

	reverse([], Reversed, Reversed).
	reverse([Head| Tail], List, Reversed) :-
		reverse(Tail, [Head| List], Reversed).

	select(Head, [Head| Tail], Tail).
	select(Head, [Head2| Tail], [Head2| Tail2]) :-
		select(Head, Tail, Tail2).

	subset([], _).
	subset([Element1| Set1], Set2) :-
		member(Element1, Set2) ->
		subset(Set1, Set2).

	subtract([], _, []).
	subtract([Element1| Set1], Set2, Set) :-
		(	member(Element1, Set2) ->
			subtract(Set1, Set2, Set)
		;	Set = [Element1| Tail],
			subtract(Set1, Set2, Tail)
		).

	symdiff(Set1, Set2, Difference) :-
		subtract(Set1, Set2, Set3),
		subtract(Set2, Set1, Set4),
		union(Set3, Set4, Difference).

	union([], Set, Set).
	union([Element1| Set1], Set2, Union) :-
		(	member(Element1, Set2) ->
			union(Set1, Set2, Union)
		;	Union = [Element1| Tail],
			union(Set1, Set2, Tail)
		).

	valid((-)) :-
		!,
		fail.
	valid([]) :-
		!.
	valid([Element| Set]) :-
		nonvar(Set),
		(	member(Element, Set) ->
			!, 
			fail
		;	valid(Set)
		).

:- end_object.
