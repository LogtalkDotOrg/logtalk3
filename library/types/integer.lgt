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


:- object(integer,
	extends(number)).

	:- info([
		version is 1:55:0,
		author is 'Paulo Moura',
		date is 2022-06-21,
		comment is 'Integer data type predicates.',
		remarks is [
			'Portability notes' - 'This object will use the backend Prolog system ``between/3``, ``plus/3``, and ``succ/2`` built-in predicates when available.'
		]
	]).

	:- public(between/3).
	:- mode(between(+integer, +integer, +integer), zero_or_one).
	:- mode(between(+integer, +integer, -integer), zero_or_more).
	:- info(between/3, [
		comment is 'Returns integers in the interval defined by the two first arguments.',
		argnames is ['Lower', 'Upper', 'Integer']
	]).

	:- public(plus/3).
	:- mode(plus(+integer, +integer, ?integer), zero_or_one).
	:- mode(plus(+integer, ?integer, +integer), zero_or_one).
	:- mode(plus(?integer, +integer, +integer), zero_or_one).
	:- info(plus/3, [
		comment is 'Reversible integer sum. At least two of the arguments must be instantiated to integers.',
		argnames is ['I', 'J', 'Sum']
	]).

	:- public(succ/2).
	:- mode(succ(+integer, ?integer), zero_or_one).
	:- mode(succ(?integer, +integer), zero_or_one).
	:- info(succ/2, [
		comment is 'Successor of a natural number. At least one of the arguments must be instantiated to a natural number.',
		argnames is ['I', 'J']
	]).

	:- public(sequence/3).
	:- mode(sequence(+integer, +integer, -list(integer)), zero_or_one).
	:- info(sequence/3, [
		comment is 'Generates a list with the sequence of all integers in the interval ``[Lower,Upper]``. Assumes ``Lower =< Upper`` and fails otherwise.',
		argnames is ['Lower', 'Upper', 'List']
	]).

	:- public(sequence/4).
	:- mode(sequence(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- info(sequence/4, [
		comment is 'Generates a list with the sequence of integers in the interval ``[Lower,Upper]`` by ``Step``. Assumes ``Lower =< Upper, Step >= 1`` and fails otherwise.',
		argnames is ['Lower', 'Upper', 'Step', 'List']
	]).

	:- if(predicate_property(between(_, _, _), built_in)).

		between(Lower, Upper, Integer) :-
			{between(Lower, Upper, Integer)}.

	:- else.

		between(Lower, Upper, Integer) :-
			integer(Lower),
			integer(Upper),
			(	var(Integer) ->
				Lower =< Upper,
				generate(Lower, Upper, Integer)
			;	integer(Integer),
				Lower =< Integer,
				Integer =< Upper
			).

		generate(Lower, _, Lower).
		generate(Lower, Upper, Integer) :-
			Lower < Upper,
			Next is Lower + 1,
			generate(Next, Upper, Integer).

	:- endif.

	:- if(predicate_property(plus(_, _, _), built_in)).

		plus(I, J, Sum) :-
			{plus(I, J, Sum)}.

	:- else.

		plus(I, J, Sum) :-
			integer(I),
			integer(J), !,
			Sum is I + J.
		plus(I, J, Sum) :-
			integer(I),
			integer(Sum), !,
			J is Sum - I.
		plus(I, J, Sum) :-
			integer(J),
			integer(Sum), !,
			I is Sum - J.

	:- endif.

	:- if(predicate_property(succ(_, _), built_in)).

		succ(I, J) :-
			{succ(I, J)}.

	:- else.

		succ(I, J) :-
			integer(I), !,
			I >= 0,
			J is I + 1.
		succ(I, J) :-
			integer(J),
			(	J =:= 0 ->
				fail
			;	J > 0,
				I is J - 1
			).

	:- endif.

	sequence(Lower, Upper, List) :-
		Lower =< Upper,
		gen_sequence(Lower, Upper, List).

	gen_sequence(Upper, Upper, List) :-
		!,
		List = [Upper].
	gen_sequence(Lower, Upper, [Lower| Tail]) :-
		Next is Lower + 1,
		gen_sequence(Next, Upper, Tail).

	sequence(Lower, Upper, Step, List) :-
		Lower =< Upper,
		Step >= 1,
		gen_sequence(Lower, Upper, Step, List).

	gen_sequence(Upper, Upper, _, List) :-
		!,
		List = [Upper].
	gen_sequence(Next, Upper, _, List) :-
		Next > Upper,
		!,
		List = [].
	gen_sequence(Lower, Upper, Step, [Lower| Tail]) :-
		Next is Lower + Step,
		gen_sequence(Next, Upper, Step, Tail).

	valid(Integer) :-
		integer(Integer).

	check(Term) :-
		(	integer(Term) ->
			true
		;	var(Term) ->
			instantiation_error
		;	type_error(integer, Term)
		).

:- end_object.
