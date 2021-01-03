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


:- object(either).

	:- info([
		version is 0:7:0,
		author is 'Paulo Moura',
		date is 2021-01-03,
		comment is 'Types and predicates for extended type-checking and handling of expected terms.',
		remarks is [
			'Type-checking support' - 'Defines a ``either(ValueType, ErrorType)`` type for checking expected terms where the value and error terms must be of the given types.',
			'QuickCheck support' - 'Defines clauses for the ``type::arbitrary/1-2`` predicates to allow generating random values for the ``either(ValueType, ErrorType)`` type.'
		],
		see_also is [expected, expected(_), type, arbitrary]
	]).

	:- public(expecteds/2).
	:- mode(expecteds(+list(expected), -list), one).
	:- info(expecteds/2, [
		comment is 'Returns the values stored in the expected terms that hold a value.',
		argnames is ['Expecteds', 'Values']
	]).

	:- public(unexpecteds/2).
	:- mode(unexpecteds(+list(expected), -list), one).
	:- info(unexpecteds/2, [
		comment is 'Returns the errors stored in the expected terms that hold an error.',
		argnames is ['Expecteds', 'Errors']
	]).

	:- public(partition/3).
	:- mode(partition(+list(expected), -list, -list), one).
	:- info(partition/3, [
		comment is 'Retrieves and partitions the values and errors hold by the expected terms.',
		argnames is ['Expecteds', 'Values', 'Errors']
	]).

	:- multifile(type::type/1).
	type::type(either(_, _)).

	:- multifile(type::check/2).
	type::check(either(ValueType, ErrorType), Term) :-
		type::check(expected, Term),
		expected(Term)::if_expected(type::check(ValueType)),
		expected(Term)::if_unexpected(type::check(ErrorType)).

	:- multifile(arbitrary::arbitrary/1).
	arbitrary::arbitrary(either(_, _)).

	:- multifile(arbitrary::arbitrary/2).
	arbitrary::arbitrary(either(ValueType, ErrorType), Arbitrary) :-
		(	random::maybe ->
			type::arbitrary(ValueType, Value),
			expected::of_expected(Value, Arbitrary)
		;	type::arbitrary(ErrorType, Error),
			expected::of_unexpected(Error, Arbitrary)
		).

	expecteds([], []).
	expecteds([Expected| Expecteds], Values) :-
		(	expected(Expected)::or_else_fail(Value) ->
			Values = [Value| Rest]
		;	Values = Rest
		),
		expecteds(Expecteds, Rest).

	unexpecteds([], []).
	unexpecteds([Expected| Expecteds], Errors) :-
		(	expected(Expected)::is_expected ->
			Errors = Rest
		;	expected(Expected)::unexpected(Error),
			Errors = [Error| Rest]
		),
		unexpecteds(Expecteds, Rest).

	partition([], [], []).
	partition([Expected| Expecteds], Values, Errors) :-
		(	expected(Expected)::or_else_fail(Value) ->
			Values = [Value| RestValues],
			Errors = RestErrors
		;	expected(Expected)::unexpected(Error),
			Values = RestValues,
			Errors = [Error| RestErrors]
		),
		partition(Expecteds, RestValues, RestErrors).

:- end_object.
