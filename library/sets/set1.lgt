%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(set(_Type_),
	extends(set)).

	:- info([
		version is 1:24:0,
		author is 'Paulo Moura and Adrian Arroyo',
		date is 2022-02-03,
		comment is 'Set predicates with elements constrained to a single type and custom comparing rules.',
		parnames is ['Type']
	]).

	:- private(sort/2).
	:- mode(sort(+list, -list), one).
	:- info(sort/2, [
		comment is 'Sorts a list in ascending order.',
		argnames is ['List', 'Sorted']
	]).

	:- private(partition/4).
	:- mode(partition(+list, +nonvar, -list, -list), one).
	:- info(partition/4, [
		comment is 'List partition in two sub-lists using a pivot.',
		argnames is ['List', 'Pivot', 'Lowers', 'Biggers']
	]).

	sort([], []).
	sort([P| L], S) :-
		partition(L, P, Small, Large),
		sort(Small, S0),
		sort(Large, S1),
		list::append(S0, [P| S1], S).

	partition([], _, [], []).
	partition([X| L1], P, Small, Large) :-
		(	_Type_::(X < P) ->
			Small = [X| Small1], Large = Large1
		;	_Type_::(X =:= P) ->
			Small = Small1, Large = Large1
		;	Small = Small1, Large = [X| Large1]
		),
		partition(L1, P, Small1, Large1).

	as_set(List, Set) :-
		sort(List, Set).

	insert([], Element, [Element]).
	insert([Head| Tail], Element, Set) :-
		(	_Type_::(Head < Element) ->
			Order = (<)
		;	_Type_::(Head =:= Element) ->
			Order = (=)
		;	Order = (>)
		),
		insert(Order, Head, Tail, Element, Set).

	insert(<, Head, Tail, Element, [Head| Set]) :-
		insert(Tail, Element, Set).
	insert(=, Head, Tail, _, [Head| Tail]).
	insert(>, Head, Tail, Element, [Element, Head| Tail]).

	insert_all([], Set, Set).
	insert_all([Head| Tail], Set1, Set3) :-
		insert(Set1, Head, Set2),
		insert_all(Tail, Set2, Set3).

	valid((-)) :-
		% catch variables
		!,
		fail.
	valid([]) :-
		!.
	valid([Element| Set]) :-
		check_order(Set, Element).

	check_order((-), _) :-
		% catch unbound tails
		!,
		fail.
	check_order([], _) :-
		!.
	check_order([Element2| Set], Element1) :-
		_Type_::valid(Element1),
		_Type_::valid(Element2),
		_Type_::(Element2 > Element1),
		check_order(Set, Element2).

	check(Term) :-
		(	valid(Term) ->
			true
		;	var(Term) ->
			instantiation_error
		;	this(This),
			type_error(This, Term)
		).

:- end_object.
