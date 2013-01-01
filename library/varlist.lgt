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



:- object(varlist,
	implements(varlistp)).

	:- info([
		version is 1.5,
		author is 'Paulo Moura',
		date is 2009/4/25,
		comment is 'List of variables predicates.']).

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

	delete([], _, []).
	delete([Head| Tail], Element, Remaining) :-
		(	Head == Element ->
			delete(Tail, Element, Remaining)
		;	Remaining = [Head| Tail2],
			delete(Tail, Element, Tail2)
		).

	empty(List) :-
		List == [].

	flatten(List, Flatted) :-
		flatten(List, [], Flatted).

	flatten(Var, Tail, [Var| Tail]) :-
		var(Var),
		!.
	flatten([], Flatted, Flatted) :-
		!.
	flatten([Head| Tail], List, Flatted) :-
		flatten(Tail, List, Aux),
		flatten(Head, Aux, Flatted).

	last([Head| Tail], Last) :-
		last(Tail, Head, Last).

	last([], Head, Last) :-
		Head == Last.
	last([Head| Tail], _, Last) :-
		last(Tail, Head, Last).

	length(List, Length) :-
		length(List, 0, Length).

	length([], Length, Length).
	length([_| Tail], Acc, Length) :-
		Acc2 is Acc + 1,
		length(Tail, Acc2, Length).

	memberchk(Element, [Head| Tail]) :-
		(	Element == Head ->
			true
		;	memberchk(Element, Tail)
		).

	nextto(Element1, Element2, List) :-
		(	var(List) ->
			List = [Element1, Element2| _]
		;	List = [Element11, Element22| _],
			Element1 == Element11,
			Element2 == Element22
		).
	nextto(Element1, Element2, [_| Tail]) :-
		nextto(Element1, Element2, Tail).

	nth0(Nth, List, Element) :-
		nth(Element, List, 0, Nth, _).

	nth0(Nth, List, Element, Tail) :-
		nth(Element, List, 0, Nth, Tail).

	nth1(Nth, List, Element) :-
		nth(Element, List, 1, Nth, _).

	nth1(Nth, List, Element, Tail) :-
		nth(Element, List, 1, Nth, Tail).

	nth(Element, List, Acc, Nth, Tail) :-
		(	integer(Nth),
			Nth >= Acc,
			nth_aux(NthElement, List, Acc, Nth, Tail) ->
			Element == NthElement
		;	var(Nth),
			nth_aux(Element, List, Acc, Nth, Tail)
		).

	nth_aux(Element, [Head| Tail], Position, Position, Tail) :-
		Element == Head.
	nth_aux(Element, [_| List], Count, Position, Tail) :-
		Count2 is Count + 1,
		nth_aux(Element, List, Count2, Position, Tail).

	permutation(List, Permutation) :-
		same_length(List, Permutation),
		is_permutation(List, Permutation).

	is_permutation([], []).
	is_permutation([Head| Tail], Permutation) :-
		select(Head, Permutation, Remaining) ->
		is_permutation(Tail, Remaining).

	prefix([], _).
	prefix(Prefix, [Head2| Tail2]) :-
		(	var(Prefix) ->
			Prefix = [Head2| Tail1]
		;	Prefix = [Head1| Tail1],
			Head1 == Head2
		),
		prefix(Tail1, Tail2).

	reverse(List, Reversed) :-
		reverse(List, [], Reversed, Reversed).

	reverse([], Reversed, Reversed, []).
	reverse([Head| Tail], List, Reversed, [_| Bound]) :-
		reverse(Tail, [Head| List], Reversed, Bound).

	same_length([], []).
	same_length([_| Tail1], [_| Tail2]) :-
		same_length(Tail1, Tail2).

	select(Element, List, Tail) :-
		(	var(List) ->
			List = [Element| Tail]
		;	List = [Head| Tail],
			Element == Head
		).
	select(Element, [Head| Tail], [Head| Tail2]) :-
		select(Element, Tail, Tail2).

	sublist(Sublist, List) :-
		equal(List, Sublist).
	sublist(Sublist, [Head| Tail]) :-
		sublist(Tail, Head, Sublist).

	sublist(List, _, Sublist) :-
		equal(List, Sublist).
	sublist([Head| Tail], _, Sublist) :-
		sublist(Tail, Head, Sublist).
	sublist([Head| Tail], Element1, Sublist) :-
		(	var(Sublist) ->
			Sublist = [Element1| Subtail]		
		;	Sublist = [Element2| Subtail],
			Element1 == Element2
		),
		sublist(Tail, Head, Subtail).

	equal([], []).
	equal([Head| Tail], Sublist) :-
		(	var(Sublist) ->
			Sublist = [Head| Tail]
		;	Sublist = [Subhead| Subtail],
			Subhead == Head,
			equal(Tail, Subtail)
		).

	subtract([], _, []).
	subtract([Head| Tail], List, Rest) :-
		(	memberchk(Head, List) ->
			subtract(Tail, List, Rest)
		;	Rest = [Head| Tail2],
			subtract(Tail, List, Tail2)
		).

	suffix(Sufix, List) :-
		equal(List, Sufix).
	suffix(Sufix, [_| Tail]) :-
		suffix(Sufix, Tail).

	valid((-)) :-		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([Element| List]) :-
		var(Element),
		valid(List).

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
