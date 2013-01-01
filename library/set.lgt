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



:- object(set,
	implements(setp),
	extends(compound)).

	:- info([
		version is 1.5,
		author is 'Richard O''Keefe; adapted to Logtalk by Paulo Moura.',
		date is 2011/02/16,
		comment is 'Set predicates implemented using ordered lists. Uses ==/2 for element comparison and standard term ordering.']).

	delete([], _, []).
	delete([Head| Tail], Element, Remaining) :-
		compare(Order, Head, Element),
		delete(Order, Head, Tail, Element, Remaining).

	delete(=, _, Tail, _, Tail).
	delete(<, Head, Tail, Element, [Head| Tail2]) :-
		delete(Tail, Element, Tail2).
	delete(>, _, Tail, _, Tail).

	disjoint([], _) :- !.
	disjoint(_, []) :- !.
	disjoint([Head1| Tail1], [Head2| Tail2]) :-
		compare(Order, Head1, Head2),
		disjoint(Order, Head1, Tail1, Head2, Tail2).

	disjoint(<, _, Tail1, Head2, Tail2) :-
		disjoint(Tail1, [Head2| Tail2]).
	disjoint(>, Head1, Tail1, _, Tail2) :-
		disjoint([Head1| Tail1], Tail2).

	equal(Set1, Set2) :-
		Set1 == Set2.

	empty(Set) :-
		Set == [].

	insert([], Element, [Element]).
	insert([Head| Tail], Element, Set) :-
		compare(Order, Head, Element),
		insert(Order, Head, Tail, Element, Set).

	insert(<, Head, Tail, Element, [Head| Set]) :-
		insert(Tail, Element, Set).
	insert(=, Head, Tail, _, [Head| Tail]).
	insert(>, Head, Tail, Element, [Element, Head| Tail]).

	insert_all([], Set, Set).
	insert_all([Head| Tail], Set1, Set3) :-
		insert(Set1, Head, Set2),
		insert_all(Tail, Set2, Set3).

	intersect([Head1| Tail1], [Head2| Tail2]) :-
		compare(Order, Head1, Head2),
		intersect(Order, Head1, Tail1, Head2, Tail2).

	intersect(=, _, _, _, _).
	intersect(<, _, Tail1, Head2, Tail2) :-
		intersect(Tail1, [Head2| Tail2]).
	intersect(>, Head1, Tail1, _, Tail2) :-
		intersect([Head1| Tail1], Tail2).

	intersection(_, [], []) :- !.
	intersection([], _, []) :- !.
	intersection([Head1| Tail1], [Head2| Tail2], Intersection) :-
		compare(Order, Head1, Head2),
		intersection(Order, Head1, Tail1, Head2, Tail2, Intersection).

	intersection(=, Head,  Tail1, _,     Tail2, [Head| Intersection]) :-
		intersection(Tail1, Tail2, Intersection).
	intersection(<, _,     Tail1, Head2, Tail2, Intersection) :-
		intersection(Tail1, [Head2| Tail2], Intersection).
	intersection(>, Head1, Tail1, _,     Tail2, Intersection) :-
		intersection([Head1|Tail1], Tail2, Intersection).

	intersection([], Set, [], Set) :- !.
	intersection([_| _], [], [], []) :- !.
	intersection([Head1|Tail1], [Head2|Tail2], Intersection, Difference) :-
		compare(Order, Head1, Head2),
		intersection(Order, Head1, Tail1, Head2, Tail2, Intersection, Difference).

	intersection(=, Head1, Tail1, _, Tail2, [Head1|Tail], Difference) :-
		intersection(Tail1, Tail2, Tail, Difference).
	intersection(<, _, Tail1, Head2, Tail2, Intersection, Difference) :-
		intersection(Tail1, [Head2| Tail2], Intersection, Difference).
	intersection(>, Head1, Tail1, Head2, Tail2, Intersection, [Head2| TailDifference]) :-
		intersection([Head1| Tail1], Tail2, Intersection, TailDifference).

	length(Set, Length) :-
		length(Set, 0, Length).

	length([], Length, Length).
	length([_| Set], Acc, Length) :-
		Acc2 is Acc + 1,
		length(Set, Acc2, Length).

	member(Element, Set) :-
		(	var(Element) ->
			member_var(Element, Set)
		;	member_nonvar(Element, Set)
		).

	member_var(Element, [Element| _]).
	member_var(Element, [_| Set]) :-
		member_var(Element, Set).

	member_nonvar(Element, [Head| Tail]) :-
	    compare(Order, Element, Head),
	    member_nonvar(Order, Element, Tail).

	member_nonvar(=, _, _).
	member_nonvar(>, Element, [Head| Tail]) :-
    	compare(Order, Element, Head),
    	member_nonvar(Order, Element, Tail).

	memberchk(Element, Set) :-
		member_nonvar(Element, Set),
		!.

	new([]).

	powerset(Set, PowerSet) :-
		reverse(Set, RSet),
		powerset_1(RSet, [[]], PowerSet).

	powerset_1([], PowerSet, PowerSet).
	powerset_1([X| Xs], Yss0, Yss) :-
		powerset_2(Yss0, X, Yss1),
		powerset_1(Xs, Yss1, Yss).

	powerset_2([], _, []).
	powerset_2([Zs| Zss], X, [Zs, [X| Zs]| Yss]) :-
		powerset_2(Zss, X, Yss).

	product([], _, []).
	product([A| As], Bs, Product) :-
		product(Bs, A, Product, Tail),
		product(As, Bs, Tail).

	product([], _, Tail, Tail).
	product([B| Bs], A, [A-B| ABs], Tail) :-
		product(Bs, A, ABs, Tail).

	reverse(List, Reversed) :-
		reverse(List, [], Reversed).

	reverse([], Reversed, Reversed).
	reverse([Head| Tail], List, Reversed) :-
		reverse(Tail, [Head| List], Reversed).

	select(Head, [Head| Tail], Tail).
	select(Head, [Head2| Tail], [Head2| Tail2]) :-
		select(Head, Tail, Tail2).

	selectchk(Elem, List, Remaining) :-
		select(Elem, List, Rest) ->
		Remaining = Rest.

	subset([], _) :- !.
	subset([Head1| Tail1], [Head2| Tail2]) :-
		compare(Order, Head1, Head2),
		subset(Order, Head1, Tail1, Head2, Tail2).

	subset(=, _, Tail1, _, Tail2) :-
		subset(Tail1, Tail2).
	subset(>, Head1, Tail1, _, Tail2) :-
		subset([Head1| Tail1], Tail2).

	subtract(Set, [], Set) :- !.
	subtract([], _, []) :- !.

	subtract([Head1| Tail1], [Head2| Tail2], Difference) :-
		compare(Order, Head1, Head2),
		subtract(Order, Head1, Tail1, Head2, Tail2, Difference).

	subtract(=, _, Tail1, _, Tail2, Difference) :-
		subtract(Tail1, Tail2, Difference).
	subtract(<, Head1, Tail1, Head2, Tail2, [Head1| Difference]) :-
		subtract(Tail1, [Head2| Tail2], Difference).
	subtract(>, Head1, Tail1, _, Tail2, Difference) :-
		subtract([Head1| Tail1], Tail2, Difference).

	symdiff(Set, [], Set) :- !.
	symdiff([], Set, Set) :- !.
	symdiff([Head1| Tail1], [Head2| Tail2], Difference) :-
		compare(Order, Head1, Head2),
		symdiff(Order, Head1, Tail1, Head2, Tail2, Difference).

	symdiff(=, _, Tail1, _, Tail2, Difference) :-
		symdiff(Tail1, Tail2, Difference).
	symdiff(<, Head1, Tail1, Head2, Tail2, [Head1| Difference]) :-
		symdiff(Tail1, [Head2| Tail2], Difference).
	symdiff(>, Head1, Tail1, Head2, Tail2, [Head2| Difference]) :-
		symdiff([Head1| Tail1], Tail2, Difference).

	union(Set, [], Set) :- !.
	union([], Set, Set) :- !.
	union([Head1| Tail1], [Head2| Tail2], Union) :-
		compare(Order, Head1, Head2),
		union(Order, Head1, Tail1, Head2, Tail2, Union).

	union(=, Head,  Tail1, _, Tail2, [Head| Union]) :-
		union(Tail1, Tail2, Union).
	union(<, Head1, Tail1, Head2, Tail2, [Head1| Union]) :-
		union(Tail1, [Head2| Tail2], Union).
	union(>, Head1, Tail1, Head2, Tail2, [Head2| Union]) :-
		union([Head1| Tail1], Tail2, Union).

	union(Set1, [], Set1, []) :- !.
	union([], Set2, Set2, Set2) :- !.
	union([Head1| Tail1], [Head2| Tail2], Union, Difference) :-
		compare(Order, Head1, Head2),
		union(Order, Head1, Tail1, Head2, Tail2, Union, Difference).

	union(=, Head,  Tail1, _,  Tail2, [Head| Union], Difference) :-
		union(Tail1, Tail2, Union, Difference).
	union(<, Head1, Tail1, Head2, Tail2, [Head1| Union], Difference) :-
		union(Tail1, [Head2| Tail2], Union, Difference).
	union(>, Head1, Tail1, Head2, Tail2, [Head2| Union], [Head2| Difference]) :-
		union([Head1| Tail1], Tail2, Union, Difference).

	valid((-)) :-				% catch variables
		!,
		fail.
	valid([]) :-
		!.
	valid([Element| Set]) :-
		check_order(Set, Element).

	check_order((-), _) :-	% catch unbound tails
		!,
		fail.
	check_order([], _) :-
		!.
	check_order([Element2| Set], Element1) :-
		Element2 @> Element1,
		check_order(Set, Element2).

	check(Term) :-
		this(This),
		sender(Sender),
		(	valid(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
