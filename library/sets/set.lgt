%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(set,
	implements(setp),
	extends(compound)).

	:- info([
		version is 1:12:0,
		author is 'Richard O''Keefe (main predicates); adapted to Logtalk by Paulo Moura.',
		date is 2019-05-23,
		comment is 'Set predicates implemented using ordered lists. Uses ``==/2`` for element comparison and standard term ordering.',
		see_also is [set(_)]
	]).

	% for backwards-compatibility with previous versions
	:- alias(setp, [size/2 as length/2]).

	as_set(List, Set) :-
		sort(List, Set).

	as_list(List, List).

	delete([], _, []).
	delete([Head| Tail], Element, Remaining) :-
		compare(Order, Head, Element),
		delete(Order, Head, Tail, Element, Remaining).

	delete(=, _, Tail, _, Tail).
	delete(<, Head, Tail, Element, [Head| Tail2]) :-
		delete(Tail, Element, Tail2).
	delete(>, Head, Tail, _, [Head| Tail]).

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

	size(Set, Size) :-
		size(Set, 0, Size).

	size([], Size, Size).
	size([_| Set], Size0, Size) :-
		Size1 is Size0 + 1,
		size(Set, Size1, Size).

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
		select(Elem, List, Rest),
		!,
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
		(	valid(Term) ->
			true
		;	var(Term) ->
			instantiation_error
		;	type_error(set, Term)
		).

:- end_object.
