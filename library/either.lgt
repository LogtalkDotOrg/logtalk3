%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 0.1,
		author is 'Paulo Moura',
		date is 2017/12/26,
		comment is 'Types and predicates for type-checking and handling expecteds.',
		remarks is [
			'Type either(ExpectedType, UnexpctedType)' - 'Allows type-checking expected references where the expected and unexpected terms must be of the given types.',
			'QuickCheck support' - 'Defines clauses for the arbitrary::arbitrary/1-2 predicates to allow generating random values for the either(ExpectedType, UnexpctedType) type.'
		],
		see_also is [expected, expected(_), type, arbitrary]
	]).

	:- public(expecteds/2).
	:- mode(expecteds(+list(expected), -list), one).
	:- info(expecteds/2, [
		comment is 'Returns the values stored in the expcted references that hold an expected term.',
		argnames is ['References', 'Expecteds']
	]).

	:- public(unexpecteds/2).
	:- mode(unexpecteds(+list(expected), -list), one).
	:- info(unexpecteds/2, [
		comment is 'Returns the values stored in the expcted references that hold an expected term.',
		argnames is ['References', 'Unexpecteds']
	]).

	:- public(partition/3).
	:- mode(partition(+list(expected), -list, -list), one).
	:- info(partition/3, [
		comment is 'Retrieves and partitions the values stored in the expcted references.',
		argnames is ['References', 'Expecteds', 'Unexpecteds']
	]).

	:- multifile(type::type/1).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(type::type/1).
	:- endif.

	type::type(either(_, _)).

	:- multifile(type::check/2).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(type::check/2).
	:- endif.

	type::check(either(ExpectedType, UnexpctedType), Term) :-
		type::check(expected, Term),
		expected(Term)::if_expected(type::check(ExpectedType)),
		expected(Term)::if_unexpected(type::check(UnexpctedType)).

	:- multifile(arbitrary::arbitrary/1).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(arbitrary::arbitrary/1).
	:- endif.

	arbitrary::arbitrary(either(_, _)).

	:- multifile(arbitrary::arbitrary/2).
	% workaround the lack of support for static multifile predicates in Qu-Prolog
	:- if(current_logtalk_flag(prolog_dialect, qp)).
		:- dynamic(arbitrary::arbitrary/2).
	:- endif.

	arbitrary::arbitrary(either(ExpectedType, UnexpctedType), Arbitrary) :-
		random::random(Random),
		(	Random < 0.5 ->
			type::arbitrary(ExpectedType, Expected),
			expected::of_expected(Expected, Arbitrary)
		;	type::arbitrary(UnexpctedType, Unexpcted),
			expected::of_unexpected(Unexpcted, Arbitrary)	
		).

	expecteds([], []).
	expecteds([Reference| References], Expecteds) :-
		(	expected(Reference)::or_else_fail(Expected) ->
			Expecteds = [Expected| Rest]
		;	Expecteds = Rest
		),
		expecteds(References, Rest).

	unexpecteds([], []).
	unexpecteds([Reference| References], Unexpcteds) :-
		(	expected(Reference)::is_expected ->
			Unexpcteds = Rest
		;	expected(Reference)::unexpected(Unexpcted),
			Unexpcteds = [Unexpcted| Rest]
		),
		unexpecteds(References, Rest).

	partition([], [], []).
	partition([Reference| References], Expecteds, Unexpcteds) :-
		(	expected(Reference)::or_else_fail(Expected) ->
			Expecteds = [Expected| Rest],
			Unexpcteds = RestUnexpcteds
		;	expected(Reference)::unexpected(Unexpcted),
			Expecteds = RestExpecteds,
			Unexpcteds = [Unexpcted| Rest]
		),
		partition(References, RestExpecteds, RestUnexpcteds).

:- end_object.
