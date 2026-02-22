%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(validated).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-22,
		comment is 'Types and predicates for type-checking and handling lists of validation terms. Inspired by Scala Cats and Kotlin Arrow.',
		remarks is [
			'Type-checking support' - 'Defines a ``validated(ValueType, ErrorType)`` type for checking validation terms where the value and error terms must be of the given types.',
			'QuickCheck support' - 'Defines clauses for the ``type::arbitrary/1-2``, ``arbitrary::shrinker/1``, ``arbitrary::shrink/3``, and ``arbitrary::edge_case/2`` predicates to allow generating random values for the ``validated(ValueType, ErrorType)`` type.'
		],
		see_also is [validation, validation(_), type, arbitrary]
	]).

	:- public(valids/2).
	:- mode(valids(+list(validation), -list), one).
	:- info(valids/2, [
		comment is 'Returns the values stored in validation terms that hold valid values.',
		argnames is ['Validations', 'Values']
	]).

	:- public(invalids/2).
	:- mode(invalids(+list(validation), -list), one).
	:- info(invalids/2, [
		comment is 'Returns a flattened list with all errors stored in invalid validation terms.',
		argnames is ['Validations', 'Errors']
	]).

	:- public(partition/3).
	:- mode(partition(+list(validation), -list, -list), one).
	:- info(partition/3, [
		comment is 'Retrieves and partitions valid values and flattened accumulated errors from validation terms.',
		argnames is ['Validations', 'Values', 'Errors']
	]).

	:- public(map/3).
	:- meta_predicate(map(2, *, *)).
	:- mode(map(+callable, +list, --compound), one).
	:- info(map/3, [
		comment is 'Applies a closure to each list element to generate validation terms and returns a pair ``Values-Errors`` accumulating all valid values and all errors in one pass.',
		argnames is ['Closure', 'Terms', 'ValuesErrors']
	]).

	:- public(map/4).
	:- meta_predicate(map(2, *, *, *)).
	:- mode(map(+callable, +list, -list, -list), one).
	:- info(map/4, [
		comment is 'Applies a closure to each list element to generate validation terms and returns valid values and accumulated errors in one pass.',
		argnames is ['Closure', 'Terms', 'Values', 'Errors']
	]).

	:- public(sequence/2).
	:- mode(sequence(+list(validation), --nonvar), one).
	:- info(sequence/2, [
		comment is 'Sequences a list of validation terms into a single validation term, accumulating all errors.',
		argnames is ['Validations', 'Validation']
	]).

	:- public(traverse/3).
	:- meta_predicate(traverse(2, *, *)).
	:- mode(traverse(+callable, +list, --nonvar), one).
	:- info(traverse/3, [
		comment is 'Applies a closure to each list element to generate validation terms and then sequences them, accumulating all errors.',
		argnames is ['Closure', 'Terms', 'Validation']
	]).

	:- uses(list, [
		append/3, reverse/2
	]).

	% type checking support

	:- multifile(type::type/1).
	type::type(validated(_, _)).

	:- multifile(type::check/2).
	type::check(validated(ValueType, ErrorType), Term) :-
		type::check(validation, Term),
		(	Term = valid(Value) ->
			type::check(ValueType, Value)
		;	Term = invalid(Errors),
			check_errors(ErrorType, Errors)
		).

	check_errors(_, []).
	check_errors(ErrorType, [Error| Errors]) :-
		type::check(ErrorType, Error),
		check_errors(ErrorType, Errors).

	% QuickCheck support

	:- multifile(arbitrary::arbitrary/1).
	arbitrary::arbitrary(validated(_, _)).

	:- multifile(arbitrary::arbitrary/2).
	arbitrary::arbitrary(validated(ValueType, ErrorType), Arbitrary) :-
		(	random::maybe ->
			type::arbitrary(ValueType, Value),
			Arbitrary = valid(Value)
		;	type::arbitrary(ErrorType, Error),
			Arbitrary = invalid([Error])
		).

	:- multifile(arbitrary::shrinker/1).
	arbitrary::shrinker(validated(_, _)).

	:- multifile(arbitrary::shrink/3).
	arbitrary::shrink(validated(ValueType, _ErrorType), valid(Large), valid(Small)) :-
		type::shrink(ValueType, Large, Small).
	arbitrary::shrink(validated(_ValueType, ErrorType), invalid([Large]), invalid([Small])) :-
		type::shrink(ErrorType, Large, Small).

	:- multifile(arbitrary::edge_case/2).
	arbitrary::edge_case(validated(ValueType, _ErrorType), valid(Term)) :-
		type::edge_case(ValueType, Term).
	arbitrary::edge_case(validated(_ValueType, ErrorType), invalid([Term])) :-
		type::edge_case(ErrorType, Term).

	% list operations

	valids([], []).
	valids([Validation| Validations], Values) :-
		(	Validation = valid(Value) ->
			Values = [Value| Rest]
		;	Validation = invalid(_),
			Values = Rest
		),
		valids(Validations, Rest).

	invalids([], []).
	invalids([Validation| Validations], Errors) :-
		(	Validation = valid(_) ->
			Errors = RestErrors
		;	Validation = invalid(ValidationErrors),
			append(ValidationErrors, RestErrors, Errors)
		),
		invalids(Validations, RestErrors).

	partition([], [], []).
	partition([Validation| Validations], Values, Errors) :-
		(	Validation = valid(Value) ->
			Values = [Value| RestValues],
			Errors = RestErrors
		;	Validation = invalid(ValidationErrors),
			Values = RestValues,
			append(ValidationErrors, RestErrors, Errors)
		),
		partition(Validations, RestValues, RestErrors).

	map(Closure, Terms, Values-Errors) :-
		map_(Terms, Closure, [], [], ReversedValues, Errors),
		reverse(ReversedValues, Values).

	map(Closure, Terms, Values, Errors) :-
		map(Closure, Terms, Values-Errors).

	:- meta_predicate(map_(*, 2, *, *, *, *)).
	map_([], _, Values, Errors, Values, Errors).
	map_([Term| Terms], Closure, Values0, Errors0, Values, Errors) :-
		call(Closure, Term, Validation),
		(	Validation = valid(Value) ->
			Values1 = [Value| Values0],
			Errors1 = Errors0
		;	Validation = invalid(ValidationErrors),
			Values1 = Values0,
			append(Errors0, ValidationErrors, Errors1)
		),
		map_(Terms, Closure, Values1, Errors1, Values, Errors).

	sequence([], valid([])).
	sequence(Validations, Validation) :-
		sequence_(Validations, [], [], Values, Errors),
		(	Errors == [] ->
			reverse(Values, OrderedValues),
			Validation = valid(OrderedValues)
		;	Validation = invalid(Errors)
		).

	traverse(Closure, Terms, Validation) :-
		traverse_(Terms, Closure, Validations),
		sequence(Validations, Validation).

	:- meta_predicate(traverse_(*, 2, *)).
	traverse_([], _, []).
	traverse_([Term| Terms], Closure, [Validation| Validations]) :-
		call(Closure, Term, Validation),
		traverse_(Terms, Closure, Validations).

	sequence_([], Values, Errors, Values, Errors).
	sequence_([Validation| Validations], Values0, Errors0, Values, Errors) :-
		(	Validation = valid(Value) ->
			Values1 = [Value| Values0],
			Errors1 = Errors0
		;	Validation = invalid(ValidationErrors),
			Values1 = Values0,
			append(Errors0, ValidationErrors, Errors1)
		),
		sequence_(Validations, Values1, Errors1, Values, Errors).

:- end_object.
