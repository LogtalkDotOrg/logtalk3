%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(varlist,
	implements(varlistp)).

	:- info([
		version is 1.6,
		author is 'Paulo Moura',
		date is 2017/06/29,
		comment is 'List of variables predicates.',
		see_also is [list, list(_), numberlist, difflist]
	]).

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
		context(Context),
		(	valid(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, Context))
		;	throw(error(type_error(varlist, Term), Context))
		).

:- end_object.
