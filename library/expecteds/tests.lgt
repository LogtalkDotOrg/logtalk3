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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2020-01-04,
		comment is 'Unit tests for the "expecteds" library.'
	]).

	:- discontiguous([
		fails/1, succeeds/1, throws/2
	]).

	cover(expected).
	cover(expected(_)).
	cover(either).

	% type and arbitrary support

	succeeds(expected_type_1_01) :-
		type::type(expected).

	succeeds(either_type_1_01) :-
		type::type(either(_,_)).

	succeeds(either_arbitrary_1_01) :-
		type::arbitrary(either(_,_)).

	% from_goal/4 tests

	succeeds(expected_from_goal_4_01) :-
		expected::from_goal(Y is 1+2, Y, failure, Expected), expected(Expected)::is_expected.

	succeeds(expected_from_goal_4_02) :-
		expected::from_goal(Y is 1+2, Y, failure, Expected), expected(Expected)::expected(Value),
		Value == 3.

	succeeds(expected_from_goal_4_03) :-
		expected::from_goal(Y is _, Y, failure, Expected), expected(Expected)::is_unexpected.

	succeeds(expected_from_goal_4_04) :-
		expected::from_goal(Y is _, Y, failure, Expected), expected(Expected)::unexpected(Error),
		Error == failure.

	% from_goal/3 tests

	succeeds(expected_from_goal_3_01) :-
		expected::from_goal(Y is 1+2, Y, Expected), expected(Expected)::is_expected.

	succeeds(expected_from_goal_3_02) :-
		expected::from_goal(Y is 1+2, Y, Expected), expected(Expected)::expected(Value),
		Value == 3.

	succeeds(expected_from_goal_3_03) :-
		expected::from_goal(Y is _, Y, Expected), expected(Expected)::is_unexpected.

	succeeds(expected_from_goal_3_04) :-
		expected::from_goal(Y is _, Y, Expected), expected(Expected)::unexpected(Error),
		subsumes_term(error(instantiation_error, _), Error).

	% from_goal/2 tests

	succeeds(expected_from_goal_2_01) :-
		expected::from_goal([Y]>>(Y is 1+2), Expected), expected(Expected)::is_expected.

	succeeds(expected_from_goal_2_02) :-
		expected::from_goal([Y]>>(Y is 1+2), Expected), expected(Expected)::expected(Value),
		Value == 3.

	succeeds(expected_from_goal_2_03) :-
		expected::from_goal(is(_), Expected), expected(Expected)::is_unexpected.

	succeeds(expected_from_goal_2_04) :-
		expected::from_goal(is(_), Expected), expected(Expected)::unexpected(Error),
		subsumes_term(error(instantiation_error, _), Error).

	% from_generator/4 tests

	succeeds(expected_from_generator_4_01) :-
		findall(Expected, expected::from_generator(a(X), X, failure, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::is_expected,
		expected(Expected2)::is_expected,
		expected(Expected3)::is_unexpected.

	succeeds(expected_from_generator_4_02) :-
		findall(Expected, expected::from_generator(b(X), X, failure, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::expected(Value1), Value1 == 1,
		expected(Expected2)::expected(Value2), Value2 == 2,
		expected(Expected3)::unexpected(Error), Error == failure.

	% from_generator/3 tests

	succeeds(expected_from_generator_3_01) :-
		findall(Expected, expected::from_generator(a(X), X, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::is_expected,
		expected(Expected2)::is_expected,
		expected(Expected3)::is_unexpected.

	succeeds(expected_from_generator_3_02) :-
		findall(Expected, expected::from_generator(a(X), X, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::expected(Value1), Value1 == 1,
		expected(Expected2)::expected(Value2), Value2 == 2,
		expected(Expected3)::unexpected(Error), Error == e.

	succeeds(expected_from_generator_3_03) :-
		findall(Expected, expected::from_generator(b(X), X, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::expected(Value1), Value1 == 1,
		expected(Expected2)::expected(Value2), Value2 == 2,
		expected(Expected3)::unexpected(Error), Error == fail.

	% from_generator/2 tests

	succeeds(expected_from_generator_2_01) :-
		findall(Expected, expected::from_generator(a, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::is_expected,
		expected(Expected2)::is_expected,
		expected(Expected3)::is_unexpected.

	succeeds(expected_from_generator_2_02) :-
		findall(Expected, expected::from_generator(a, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::expected(Value1), Value1 == 1,
		expected(Expected2)::expected(Value2), Value2 == 2,
		expected(Expected3)::unexpected(Error), Error == e.

	succeeds(expected_from_generator_2_03) :-
		findall(Expected, expected::from_generator(b, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::expected(Value1), Value1 == 1,
		expected(Expected2)::expected(Value2), Value2 == 2,
		expected(Expected3)::unexpected(Error), Error == fail.

	% is_unexpected/0 tests

	succeeds(expected_is_unexpected_1_01) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::is_unexpected.

	succeeds(expected_is_unexpected_1_02) :-
		expected::of_expected(1, Expected), \+ expected(Expected)::is_unexpected.

	% is_expected/0 tests

	succeeds(expected_is_expected_1_01) :-
		expected::of_unexpected(-1, Expected), \+ expected(Expected)::is_expected.

	succeeds(expected_is_expected_1_02) :-
		expected::of_expected(1, Expected), expected(Expected)::is_expected.

	% if_unexpected/1 tests

	succeeds(expected_if_unexpected_1_01) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::if_unexpected({Y}/[X]>>(Y is X + 1)),
		Y == 0.

	succeeds(expected_if_unexpected_1_02) :-
		expected::of_expected(1, Expected), expected(Expected)::if_unexpected({Y}/[X]>>(Y is X + 1)),
		var(Y).

	% if_expected/1 tests

	succeeds(expected_if_expected_1_01) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::if_expected({Y}/[X]>>(Y is X + 1)),
		var(Y).

	succeeds(expected_if_expected_1_02) :-
		expected::of_expected(1, Expected), expected(Expected)::if_expected({Y}/[X]>>(Y is X + 1)),
		Y == 2.

	% if_expected_or_else/2 tests

	succeeds(expected_if_expected_or_else_1_01) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::if_expected_or_else({Y}/[X]>>(Y is X + 1), {Y}/[X]>>(Y is -X)),
		Y == 1.

	succeeds(expected_if_expected_or_else_1_02) :-
		expected::of_expected(1, Expected), expected(Expected)::if_expected_or_else({Y}/[X]>>(Y is X + 1), {Y}/[X]>>(Y is -X)),
		Y == 2.

	% unexpected/1 tests

	throws(expected_unexpected_1_01, error(existence_error(unexpected_error,_), _)) :-
		expected::of_expected(1, Expected), expected(Expected)::unexpected(_).

	succeeds(expected_unexpected_1_02) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::unexpected(Error),
		Error == -1.

	% expected/1 tests

	throws(expected_expected_1_01, error(existence_error(expected_value,_), _)) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::expected(_).

	succeeds(expected_expected_1_02) :-
		expected::of_expected(1, Expected), expected(Expected)::expected(Value),
		Value == 1.

	% map/2 tests

	succeeds(expected_map_2_01) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::map(char_code, NewExpected),
		expected(NewExpected)::is_unexpected.

	succeeds(expected_map_2_02) :-
		expected::of_expected(a, Expected), expected(Expected)::map(char_code, NewExpected),
		expected(NewExpected)::expected(Value), Value == 97.

	% flat_map/2 tests

	succeeds(expected_flat_map_2_01) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::flat_map(flat_map_closure, NewExpected),
		expected(NewExpected)::is_unexpected.

	succeeds(expected_flat_map_2_02) :-
		expected::of_expected(a, Expected), expected(Expected)::flat_map(flat_map_closure, NewExpected),
		expected(NewExpected)::expected(Value), Value == 97.

	% either/3 tests

	succeeds(expected_either_3_01) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::either(either_expected, either_unexpected, NewExpected),
		expected(NewExpected)::expected(Error), Error == 1.

	succeeds(expected_either_3_02) :-
		expected::of_expected(1, Expected), expected(Expected)::either(either_expected, either_unexpected, NewExpected),
		expected(NewExpected)::unexpected(Value), Value == -1.

	% or_else/2 tests

	succeeds(expected_or_else_2_01) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::or_else(Value, 0),
		Value == 0.

	succeeds(expected_or_else_2_02) :-
		expected::of_expected(1, Expected), expected(Expected)::or_else(Value, 0),
		Value == 1.

	% or_else_get/2 tests

	succeeds(expected_or_else_get_2_01) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::or_else_get(Value, current_logtalk_flag(prolog_dialect)),
		atom(Value).

	succeeds(expected_or_else_get_2_02) :-
		expected::of_expected(1, Expected), expected(Expected)::or_else_get(Value, current_logtalk_flag(prolog_dialect)),
		Value == 1.

	throws(expected_or_else_get_2_03, error(existence_error(expected_value,_), _)) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::or_else_get(_, nonvar).

	% or_else_call/2 tests

	succeeds(expected_or_else_call_2_01) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::or_else_call(_, X = 0),
		X == 0.

	succeeds(expected_or_else_call_2_02) :-
		expected::of_expected(1, Expected), expected(Expected)::or_else_call(Value, X = 0),
		Value == 1, var(X).

	% or_else_throw/1 tests

	throws(expected_or_else_throw_1_01, instantiation_error) :-
		expected::of_unexpected(instantiation_error, Expected), expected(Expected)::or_else_throw(_).

	succeeds(expected_or_else_throw_1_02) :-
		expected::of_expected(1, Expected), expected(Expected)::or_else_throw(Value),
		Value == 1.

	% or_else_fail/1 tests

	fails(expected_or_else_fail_1_01) :-
		expected::of_unexpected(-1, Expected), expected(Expected)::or_else_fail(_).

	succeeds(expected_or_else_fail_1_02) :-
		expected::of_expected(1, Expected), expected(Expected)::or_else_fail(Value),
		Value == 1.

	% "expected" type tests

	succeeds(expected_type_checking_support_01) :-
		expected::of_unexpected(-1, Expected),
		type::check(expected, Expected).

	succeeds(expected_type_checking_support_02) :-
		expected::of_expected(1, Expected),
		type::check(expected, Expected).

	succeeds(expected_type_checking_support_03) :-
		expected::from_goal(Y is 1+2, Y, failure, Expected),
		type::check(expected, Expected).

	succeeds(expected_type_checking_support_04) :-
		expected::from_goal(Y is _, Y, failure, Expected),
		type::check(expected, Expected).

	succeeds(expected_type_checking_support_05) :-
		expected::from_goal(Y is 1+2, Y, Expected),
		type::check(expected, Expected).

	succeeds(expected_type_checking_support_06) :-
		expected::from_goal(Y is _, Y, Expected),
		type::check(expected, Expected).

	throws(expected_type_checking_support_07, instantiation_error) :-
		type::check(expected, _).

	throws(expected_type_checking_support_08, type_error(expected,12345)) :-
		type::check(expected, 12345).

	throws(expected_type_checking_support_09, type_error(expected,foobar)) :-
		type::check(expected, foobar).

	throws(expected_type_checking_support_10, type_error(expected,foo(bar,baz))) :-
		type::check(expected, foo(bar,baz)).

	% expecteds/2 tests

	succeeds(either_expecteds_2_01) :-
		either::expecteds([], Values),
		Values == [].

	succeeds(either_expecteds_2_02) :-
		expected::of_unexpected(e1, Expected1),
		expected::of_expected(1, Expected2),
		expected::of_unexpected(e2, Expected3),
		expected::of_expected(2, Expected4),
		either::expecteds([Expected1, Expected2, Expected3, Expected4], Values),
		Values == [1, 2].

	% unexpecteds/2 tests

	succeeds(either_unexpecteds_2_01) :-
		either::unexpecteds([], Errors),
		Errors == [].

	succeeds(either_unexpecteds_2_02) :-
		expected::of_unexpected(e1, Expected1),
		expected::of_expected(1, Expected2),
		expected::of_unexpected(e2, Expected3),
		expected::of_expected(2, Expected4),
		either::unexpecteds([Expected1, Expected2, Expected3, Expected4], Errors),
		Errors == [e1, e2].

	% partition/3 tests

	succeeds(either_partition_3_01) :-
		either::partition([], Values, Errors),
		Values == [],
		Errors == [].

	succeeds(either_partition_3_02) :-
		expected::of_unexpected(e1, Expected1),
		expected::of_expected(1, Expected2),
		expected::of_unexpected(e2, Expected3),
		expected::of_expected(2, Expected4),
		either::partition([Expected1, Expected2, Expected3, Expected4], Values, Errors),
		Values == [1, 2],
		Errors == [e1, e2].

	% "either" type tests

	succeeds(either_type_checking_support_01) :-
		expected::of_unexpected(a, Expected),
		type::check(either(integer, atom), Expected).

	succeeds(either_type_checking_support_02) :-
		expected::of_expected(1, Expected),
		type::check(either(integer, atom), Expected).

	succeeds(either_type_checking_support_03) :-
		expected::from_goal(Y is 1+2, Y, Expected),
		type::check(either(integer, atom), Expected).

	succeeds(either_type_checking_support_04) :-
		expected::from_goal(2 is 3, _, Expected),
		type::check(either(integer, atom), Expected).

	succeeds(either_type_checking_support_05) :-
		expected::from_goal(Y is 1+2, Y, a, Expected),
		type::check(either(integer, atom), Expected).

	succeeds(either_type_checking_support_06) :-
		expected::from_goal(2 is 3, _, a, Expected),
		type::check(either(integer, atom), Expected).

	throws(either_type_checking_support_07, instantiation_error) :-
		type::check(either(integer, atom), _).

	throws(either_type_checking_support_08, type_error(expected,12345)) :-
		type::check(either(integer, atom), 12345).

	throws(either_type_checking_support_09, type_error(expected,foobar)) :-
		type::check(either(integer, atom), foobar).

	throws(either_type_checking_support_10, type_error(expected,foo(bar,baz))) :-
		type::check(either(integer, atom), foo(bar,baz)).

	throws(either_type_checking_support_11, type_error(integer,a)) :-
		expected::of_expected(a, Expected),
		type::check(either(integer, atom), Expected).

	throws(either_type_checking_support_12, type_error(atom,1)) :-
		expected::of_unexpected(1, Expected),
		type::check(either(integer, atom), Expected).

	throws(either_type_checking_support_13, type_error(atom,3)) :-
		expected::from_goal(Y is 1+2, Y, Expected),
		type::check(either(atom, integer), Expected).

	throws(either_type_checking_support_14, type_error(integer,fail)) :-
		expected::from_goal(2 is 3, _, Expected),
		type::check(either(atom, integer), Expected).

	throws(either_type_checking_support_15, type_error(atom,3)) :-
		expected::from_goal(Y is 1+2, Y, fail, Expected),
		type::check(either(atom, integer), Expected).

	throws(either_type_checking_support_16, type_error(integer,fail)) :-
		expected::from_goal(2 is 3, _, fail, Expected),
		type::check(either(atom, integer), Expected).

	% "either" arbitrary tests

	quick_check(
		either_arbitrary_support_01,
		type::arbitrary({either(integer, atom)}, -either(integer, atom))
	).

	% auxiliary predicates

	flat_map_closure(Value, NewExpected) :-
		char_code(Value, NewValue),
		expected::of_expected(NewValue, NewExpected).

	either_unexpected(Error, NewExpected) :-
		NewError is abs(Error),
		expected::of_expected(NewError, NewExpected).

	either_expected(Value, NewExpected) :-
		NewValue is -Value,
		expected::of_unexpected(NewValue, NewExpected).

	a(1).
	a(2).
	a(_) :-
		throw(e).

	b(1).
	b(2).

:- end_object.
