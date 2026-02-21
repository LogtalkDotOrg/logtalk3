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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 2:2:0,
		author is 'Paulo Moura',
		date is 2026-02-21,
		comment is 'Unit tests for the "expecteds" library.'
	]).

	cover(expected).
	cover(expected(_)).
	cover(either).

	% type and arbitrary support

	test(expected_type_1_01, true) :-
		type::type(expected).

	test(either_type_1_01, true) :-
		type::type(either(_,_)).

	test(either_arbitrary_1_01, true) :-
		type::arbitrary(either(_,_)).

	% from_goal/4 tests

	test(expected_from_goal_4_01, true) :-
		expected::from_goal(Y is 1+2, Y, failure, Expected),
		expected(Expected)::is_expected.

	test(expected_from_goal_4_02, true(Value == 3)) :-
		expected::from_goal(Y is 1+2, Y, failure, Expected),
		expected(Expected)::expected(Value).

	test(expected_from_goal_4_03, true) :-
		expected::from_goal(Y is _, Y, failure, Expected),
		expected(Expected)::is_unexpected.

	test(expected_from_goal_4_04, true(Error == failure)) :-
		expected::from_goal(Y is _, Y, failure, Expected),
		expected(Expected)::unexpected(Error).

	% from_goal/3 tests

	test(expected_from_goal_3_01, true) :-
		expected::from_goal(Y is 1+2, Y, Expected),
		expected(Expected)::is_expected.

	test(expected_from_goal_3_02, true(Value == 3)) :-
		expected::from_goal(Y is 1+2, Y, Expected),
		expected(Expected)::expected(Value).

	test(expected_from_goal_3_03, true) :-
		expected::from_goal(Y is _, Y, Expected),
		expected(Expected)::is_unexpected.

	test(expected_from_goal_3_04, subsumes(error(instantiation_error, _), Error)) :-
		expected::from_goal(Y is _, Y, Expected),
		expected(Expected)::unexpected(Error).

	% from_goal/2 tests

	test(expected_from_goal_2_01, true) :-
		expected::from_goal([Y]>>(Y is 1+2), Expected),
		expected(Expected)::is_expected.

	test(expected_from_goal_2_02, true(Value == 3)) :-
		expected::from_goal([Y]>>(Y is 1+2), Expected),
		expected(Expected)::expected(Value).

	test(expected_from_goal_2_03, true) :-
		expected::from_goal(is(_), Expected),
		expected(Expected)::is_unexpected.

	test(expected_from_goal_2_04, subsumes(error(instantiation_error, _), Error)) :-
		expected::from_goal(is(_), Expected),
		expected(Expected)::unexpected(Error).

	% from_generator/4 tests

	test(expected_from_generator_4_01, true) :-
		findall(Expected, expected::from_generator(a(X), X, failure, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::is_expected,
		expected(Expected2)::is_expected,
		expected(Expected3)::is_unexpected.

	test(expected_from_generator_4_02, true(r(Value1,Value2,Error) == r(1,2,failure))) :-
		findall(Expected, expected::from_generator(b(X), X, failure, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::expected(Value1),
		expected(Expected2)::expected(Value2),
		expected(Expected3)::unexpected(Error).

	% from_generator/3 tests

	test(expected_from_generator_3_01, true) :-
		findall(Expected, expected::from_generator(a(X), X, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::is_expected,
		expected(Expected2)::is_expected,
		expected(Expected3)::is_unexpected.

	test(expected_from_generator_3_02, true(r(Value1,Value2,Error) == r(1,2,e))) :-
		findall(Expected, expected::from_generator(a(X), X, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::expected(Value1),
		expected(Expected2)::expected(Value2),
		expected(Expected3)::unexpected(Error).

	test(expected_from_generator_3_03, true(r(Value1,Value2,Error) == r(1,2,fail))) :-
		findall(Expected, expected::from_generator(b(X), X, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::expected(Value1),
		expected(Expected2)::expected(Value2),
		expected(Expected3)::unexpected(Error).

	% from_generator/2 tests

	test(expected_from_generator_2_01, true) :-
		findall(Expected, expected::from_generator(a, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::is_expected,
		expected(Expected2)::is_expected,
		expected(Expected3)::is_unexpected.

	test(expected_from_generator_2_02, true(r(Value1,Value2,Error) == r(1,2,e))) :-
		findall(Expected, expected::from_generator(a, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::expected(Value1),
		expected(Expected2)::expected(Value2),
		expected(Expected3)::unexpected(Error).

	test(expected_from_generator_2_03, true(r(Value1,Value2,Error) == r(1,2,fail))) :-
		findall(Expected, expected::from_generator(b, Expected), [Expected1,Expected2,Expected3]),
		expected(Expected1)::expected(Value1),
		expected(Expected2)::expected(Value2),
		expected(Expected3)::unexpected(Error).

	% is_unexpected/0 tests

	test(expected_is_unexpected_1_01, true) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::is_unexpected.

	test(expected_is_unexpected_1_02, false) :-
		expected::of_expected(1, Expected),
		expected(Expected)::is_unexpected.

	% is_expected/0 tests

	test(expected_is_expected_1_01, false) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::is_expected.

	test(expected_is_expected_1_02, true) :-
		expected::of_expected(1, Expected),
		expected(Expected)::is_expected.

	% if_unexpected/1 tests

	test(expected_if_unexpected_1_01, true(Y == 0)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::if_unexpected({Y}/[X]>>(Y is X + 1)).

	test(expected_if_unexpected_1_02, true(var(Y))) :-
		expected::of_expected(1, Expected),
		expected(Expected)::if_unexpected({Y}/[X]>>(Y is X + 1)).

	% if_expected/1 tests

	test(expected_if_expected_1_01, true(var(Y))) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::if_expected({Y}/[X]>>(Y is X + 1)).

	test(expected_if_expected_1_02, true(Y == 2)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::if_expected({Y}/[X]>>(Y is X + 1)).

	% if_expected_or_else/2 tests

	test(expected_if_expected_or_else_1_01, true(Y == 1)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::if_expected_or_else({Y}/[X]>>(Y is X + 1), {Y}/[X]>>(Y is -X)).

	test(expected_if_expected_or_else_1_02, true(Y == 2)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::if_expected_or_else({Y}/[X]>>(Y is X + 1), {Y}/[X]>>(Y is -X)).

	% unexpected/1 tests

	test(expected_unexpected_1_01, error(existence_error(unexpected_error,_))) :-
		expected::of_expected(1, Expected),
		expected(Expected)::unexpected(_).

	test(expected_unexpected_1_02, true(Error == -1)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::unexpected(Error).

	% expected/1 tests

	test(expected_expected_1_01, error(existence_error(expected_value,_))) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::expected(_).

	test(expected_expected_1_02, true(Value == 1)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::expected(Value).

	% map/2 tests

	test(expected_map_2_01, true) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::map(char_code, NewExpected),
		expected(NewExpected)::is_unexpected.

	test(expected_map_2_02, true(Value == 97)) :-
		expected::of_expected(a, Expected),
		expected(Expected)::map(char_code, NewExpected),
		expected(NewExpected)::expected(Value).

	% flat_map/2 tests

	test(expected_flat_map_2_01, true) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::flat_map(flat_map_closure, NewExpected),
		expected(NewExpected)::is_unexpected.

	test(expected_flat_map_2_02, true(Value == 97)) :-
		expected::of_expected(a, Expected),
		expected(Expected)::flat_map(flat_map_closure, NewExpected),
		expected(NewExpected)::expected(Value).

	% either/3 tests

	test(expected_either_3_01, true(Error == 1)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::either(either_expected, either_unexpected, NewExpected),
		expected(NewExpected)::expected(Error).

	test(expected_either_3_02, true(Value == -1)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::either(either_expected, either_unexpected, NewExpected),
		expected(NewExpected)::unexpected(Value).

	% or_else/2 tests

	test(expected_or_else_2_01, true(Value == 0)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::or_else(Value, 0).

	test(expected_or_else_2_02, true(Value == 1)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::or_else(Value, 0).

	% or_else_get/2 tests

	test(expected_or_else_get_2_01, true(atom(Value))) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::or_else_get(Value, current_logtalk_flag(prolog_dialect)).

	test(expected_or_else_get_2_02, true(Value == 1)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::or_else_get(Value, current_logtalk_flag(prolog_dialect)).

	test(expected_or_else_get_2_03, error(existence_error(expected_value,_))) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::or_else_get(_, nonvar).

	% or_else_call/2 tests

	test(expected_or_else_call_2_01, true(X == 0)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::or_else_call(_, X = 0).

	test(expected_or_else_call_2_02, true((Value == 1, var(X)))) :-
		expected::of_expected(1, Expected),
		expected(Expected)::or_else_call(Value, X = 0).

	% or_else_throw/1 tests

	test(expected_or_else_throw_1_01, ball(instantiation_error)) :-
		expected::of_unexpected(instantiation_error, Expected),
		expected(Expected)::or_else_throw(_).

	test(expected_or_else_throw_1_02, true(Value == 1)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::or_else_throw(Value).

	% or_else_fail/1 tests

	test(expected_or_else_fail_1_01, false) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::or_else_fail(_).

	test(expected_or_else_fail_1_02, true(Value == 1)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::or_else_fail(Value).

	% map_unexpected/2 tests

	test(expected_map_unexpected_2_01, true(Value == 1)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::map_unexpected(map_unexpected_closure, NewExpected),
		expected(NewExpected)::expected(Value).

	test(expected_map_unexpected_2_02, true(Error == 1)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::map_unexpected(map_unexpected_closure, NewExpected),
		expected(NewExpected)::unexpected(Error).

	% map_catching/2 tests

	test(expected_map_catching_2_01, true(NewExpected == Expected)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::map_catching(char_code, NewExpected).

	test(expected_map_catching_2_02, true(Value == 97)) :-
		expected::of_expected(a, Expected),
		expected(Expected)::map_catching(char_code, NewExpected),
		expected(NewExpected)::expected(Value).

	test(expected_map_catching_2_03, true) :-
		expected::of_expected(1, Expected),
		expected(Expected)::map_catching(char_code, NewExpected),
		expected(NewExpected)::is_unexpected.

	test(expected_map_catching_2_04, true(Error == fail)) :-
		expected::of_expected(a, Expected),
		expected(Expected)::map_catching(failing_closure, NewExpected),
		expected(NewExpected)::unexpected(Error).

	% map_both/3 tests

	test(expected_map_both_3_01, true(Value == 2)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::map_both(map_both_value_closure, map_unexpected_closure, NewExpected),
		expected(NewExpected)::expected(Value).

	test(expected_map_both_3_02, true(Error == 1)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::map_both(map_both_value_closure, map_unexpected_closure, NewExpected),
		expected(NewExpected)::unexpected(Error).

	% swap/1 tests

	test(expected_swap_1_01, true(Error == 1)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::swap(NewExpected),
		expected(NewExpected)::unexpected(Error).

	test(expected_swap_1_02, true(Value == -1)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::swap(NewExpected),
		expected(NewExpected)::expected(Value).

	% to_optional/1 tests

	test(expected_to_optional_1_01, true(Value == 1)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::to_optional(Optional),
		optional(Optional)::get(Value).

	test(expected_to_optional_1_02, true) :-
		expected::of_unexpected(error, Expected),
		expected(Expected)::to_optional(Optional),
		optional(Optional)::is_empty.

	% from_optional/3 tests

	test(expected_from_optional_3_01, true(Value == 1)) :-
		optional::of(1, Optional),
		expected::from_optional(Optional, missing, Expected),
		expected(Expected)::expected(Value).

	test(expected_from_optional_3_02, true(Error == missing)) :-
		optional::empty(Optional),
		expected::from_optional(Optional, missing, Expected),
		expected(Expected)::unexpected(Error).

	% flatten/1 tests

	test(expected_flatten_1_01, true(Value == 1)) :-
		expected::of_expected(1, Inner),
		expected::of_expected(Inner, Outer),
		expected(Outer)::flatten(NewExpected),
		expected(NewExpected)::expected(Value).

	test(expected_flatten_1_02, true(Error == oops)) :-
		expected::of_unexpected(oops, Inner),
		expected::of_expected(Inner, Outer),
		expected(Outer)::flatten(NewExpected),
		expected(NewExpected)::unexpected(Error).

	test(expected_flatten_1_03, true(Error == -1)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::flatten(NewExpected),
		expected(NewExpected)::unexpected(Error).

	test(expected_flatten_1_04, true(Value == 1)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::flatten(NewExpected),
		expected(NewExpected)::expected(Value).

	% filter/3 tests

	test(expected_filter_3_01, true(Value == 1)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::filter(integer, not_integer, NewExpected),
		expected(NewExpected)::expected(Value).

	test(expected_filter_3_02, true(Error == not_integer)) :-
		expected::of_expected(a, Expected),
		expected(Expected)::filter(integer, not_integer, NewExpected),
		expected(NewExpected)::unexpected(Error).

	test(expected_filter_3_03, true(Error == -1)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::filter(integer, not_integer, NewExpected),
		expected(NewExpected)::unexpected(Error).

	% map_or_else/3 tests

	test(expected_map_or_else_3_01, true(Value == 97)) :-
		expected::of_expected(a, Expected),
		expected(Expected)::map_or_else(char_code, 0, Value).

	test(expected_map_or_else_3_02, true(Value == 0)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::map_or_else(char_code, 0, Value).

	test(expected_map_or_else_3_03, true(Value == 0)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::map_or_else(char_code, 0, Value).

	% or/2 tests

	test(expected_or_2_01, true(NewExpected == Expected)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::or(NewExpected, expected::of_expected(2)).

	test(expected_or_2_02, true(Value == 2)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::or(NewExpected, expected::of_expected(2)),
		expected(NewExpected)::expected(Value).

	test(expected_or_2_03, true) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::or(NewExpected, expected::of_unexpected(-2)),
		expected(NewExpected)::is_unexpected.

	% or_else_throw/2 tests

	test(expected_or_else_throw_2_01, true(Value == 1)) :-
		expected::of_expected(1, Expected),
		expected(Expected)::or_else_throw(Value, custom_error).

	test(expected_or_else_throw_2_02, ball(custom_error)) :-
		expected::of_unexpected(-1, Expected),
		expected(Expected)::or_else_throw(_, custom_error).

	% zip/3 tests

	test(expected_zip_3_01, true(Value == 4)) :-
		expected::of_expected(1, Expected1),
		expected::of_expected(3, Expected2),
		expected(Expected1)::zip(zip_closure, Expected2, NewExpected),
		expected(NewExpected)::expected(Value).

	test(expected_zip_3_02, true(Error == -1)) :-
		expected::of_unexpected(-1, Expected1),
		expected::of_expected(3, Expected2),
		expected(Expected1)::zip(zip_closure, Expected2, NewExpected),
		expected(NewExpected)::unexpected(Error).

	test(expected_zip_3_03, true(Error == -2)) :-
		expected::of_expected(1, Expected1),
		expected::of_unexpected(-2, Expected2),
		expected(Expected1)::zip(zip_closure, Expected2, NewExpected),
		expected(NewExpected)::unexpected(Error).

	test(expected_zip_3_04, true(Error == -1)) :-
		expected::of_unexpected(-1, Expected1),
		expected::of_unexpected(-2, Expected2),
		expected(Expected1)::zip(zip_closure, Expected2, NewExpected),
		expected(NewExpected)::unexpected(Error).

	% "expected" type tests

	test(expected_type_checking_support_01, true) :-
		expected::of_unexpected(-1, Expected),
		type::check(expected, Expected).

	test(expected_type_checking_support_02, true) :-
		expected::of_expected(1, Expected),
		type::check(expected, Expected).

	test(expected_type_checking_support_03, true) :-
		expected::from_goal(Y is 1+2, Y, failure, Expected),
		type::check(expected, Expected).

	test(expected_type_checking_support_04, true) :-
		expected::from_goal(Y is _, Y, failure, Expected),
		type::check(expected, Expected).

	test(expected_type_checking_support_05, true) :-
		expected::from_goal(Y is 1+2, Y, Expected),
		type::check(expected, Expected).

	test(expected_type_checking_support_06, true) :-
		expected::from_goal(Y is _, Y, Expected),
		type::check(expected, Expected).

	test(expected_type_checking_support_07, ball(instantiation_error)) :-
		type::check(expected, _).

	test(expected_type_checking_support_08, ball(type_error(expected,12345))) :-
		type::check(expected, 12345).

	test(expected_type_checking_support_09, ball(type_error(expected,foobar))) :-
		type::check(expected, foobar).

	test(expected_type_checking_support_10, ball(type_error(expected,foo(bar,baz)))) :-
		type::check(expected, foo(bar,baz)).

	% expecteds/2 tests

	test(either_expecteds_2_01, true(Values == [])) :-
		either::expecteds([], Values).

	test(either_expecteds_2_02, true(Values == [1, 2])) :-
		expected::of_unexpected(e1, Expected1),
		expected::of_expected(1, Expected2),
		expected::of_unexpected(e2, Expected3),
		expected::of_expected(2, Expected4),
		either::expecteds([Expected1, Expected2, Expected3, Expected4], Values).

	% unexpecteds/2 tests

	test(either_unexpecteds_2_01, true(Errors == [])) :-
		either::unexpecteds([], Errors).

	test(either_unexpecteds_2_02, true(Errors == [e1, e2])) :-
		expected::of_unexpected(e1, Expected1),
		expected::of_expected(1, Expected2),
		expected::of_unexpected(e2, Expected3),
		expected::of_expected(2, Expected4),
		either::unexpecteds([Expected1, Expected2, Expected3, Expected4], Errors).

	% partition/3 tests

	test(either_partition_3_01, true(Values-Errors == []-[])) :-
		either::partition([], Values, Errors).

	test(either_partition_3_02, true(Values-Errors == [1,2]-[e1,e2])) :-
		expected::of_unexpected(e1, Expected1),
		expected::of_expected(1, Expected2),
		expected::of_unexpected(e2, Expected3),
		expected::of_expected(2, Expected4),
		either::partition([Expected1, Expected2, Expected3, Expected4], Values, Errors).

	% sequence/2 tests

	test(either_sequence_2_01, true(Values == [])) :-
		either::sequence([], Expected),
		expected(Expected)::expected(Values).

	test(either_sequence_2_02, true(Values == [1,2])) :-
		expected::of_expected(1, Expected1),
		expected::of_expected(2, Expected2),
		either::sequence([Expected1, Expected2], Expected),
		expected(Expected)::expected(Values).

	test(either_sequence_2_03, true(Error == e1)) :-
		expected::of_expected(1, Expected1),
		expected::of_unexpected(e1, Expected2),
		expected::of_unexpected(e2, Expected3),
		either::sequence([Expected1, Expected2, Expected3], Expected),
		expected(Expected)::unexpected(Error).

	% traverse/3 tests

	test(either_traverse_3_01, true(Values == [1,2])) :-
		either::traverse(traverse_expected_closure, [1,2], Expected),
		expected(Expected)::expected(Values).

	test(either_traverse_3_02, true(Error == not_integer(a))) :-
		either::traverse(traverse_expected_closure, [1,a,2], Expected),
		expected(Expected)::unexpected(Error).

	% "either" type tests

	test(either_type_checking_support_01, true) :-
		expected::of_unexpected(a, Expected),
		type::check(either(integer, atom), Expected).

	test(either_type_checking_support_02, true) :-
		expected::of_expected(1, Expected),
		type::check(either(integer, atom), Expected).

	test(either_type_checking_support_03, true) :-
		expected::from_goal(Y is 1+2, Y, Expected),
		type::check(either(integer, atom), Expected).

	test(either_type_checking_support_04, true) :-
		expected::from_goal(2 is 3, _, Expected),
		type::check(either(integer, atom), Expected).

	test(either_type_checking_support_05, true) :-
		expected::from_goal(Y is 1+2, Y, a, Expected),
		type::check(either(integer, atom), Expected).

	test(either_type_checking_support_06, true) :-
		expected::from_goal(2 is 3, _, a, Expected),
		type::check(either(integer, atom), Expected).

	test(either_type_checking_support_07, ball(instantiation_error)) :-
		type::check(either(integer, atom), _).

	test(either_type_checking_support_08, ball(type_error(expected,12345))) :-
		type::check(either(integer, atom), 12345).

	test(either_type_checking_support_09, ball(type_error(expected,foobar))) :-
		type::check(either(integer, atom), foobar).

	test(either_type_checking_support_10, ball(type_error(expected,foo(bar,baz)))) :-
		type::check(either(integer, atom), foo(bar,baz)).

	test(either_type_checking_support_11, ball(type_error(integer,a))) :-
		expected::of_expected(a, Expected),
		type::check(either(integer, atom), Expected).

	test(either_type_checking_support_12, ball(type_error(atom,1))) :-
		expected::of_unexpected(1, Expected),
		type::check(either(integer, atom), Expected).

	test(either_type_checking_support_13, ball(type_error(atom,3))) :-
		expected::from_goal(Y is 1+2, Y, Expected),
		type::check(either(atom, integer), Expected).

	test(either_type_checking_support_14, ball(type_error(integer,fail))) :-
		expected::from_goal(2 is 3, _, Expected),
		type::check(either(atom, integer), Expected).

	test(either_type_checking_support_15, ball(type_error(atom,3))) :-
		expected::from_goal(Y is 1+2, Y, fail, Expected),
		type::check(either(atom, integer), Expected).

	test(either_type_checking_support_16, ball(type_error(integer,fail))) :-
		expected::from_goal(2 is 3, _, fail, Expected),
		type::check(either(atom, integer), Expected).

	% "either" arbitrary tests

	quick_check(
		either_arbitrary_support_01,
		type::arbitrary({either(integer, atom)}, -either(integer, atom))
	).

	% "either" shrinker tests

	test(either_shrinker_1_01, true) :-
		type::shrinker(either(integer, atom)).

	test(either_shrink_3_01, all(type::check(either(integer, atom), Small))) :-
		expected::of_expected(42, Expected),
		type::shrink(either(integer, atom), Expected, Small).

	test(either_shrink_3_02, all(type::check(either(integer, atom), Small))) :-
		expected::of_unexpected(abc, Expected),
		type::shrink(either(integer, atom), Expected, Small).

	test(either_shrink_3_03, false) :-
		expected::of_expected(0, Expected),
		type::shrink(either(integer, atom), Expected, _).

	test(either_shrink_3_04, false) :-
		expected::of_unexpected('', Expected),
		type::shrink(either(integer, atom), Expected, _).

	% "either" edge case tests

	test(either_edge_case_2_01, true) :-
		findall(T, type::edge_case(either(integer, atom), T), Cases),
		Cases \== [].

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

	map_unexpected_closure(Error, NewError) :-
		NewError is abs(Error).

	map_both_value_closure(Value, NewValue) :-
		NewValue is Value + 1.

	failing_closure(_, _) :-
		fail.

	zip_closure(X, Y, Z) :-
		Z is X + Y.

	traverse_expected_closure(Term, Expected) :-
		( 	integer(Term) ->
			expected::of_expected(Term, Expected)
		; 	expected::of_unexpected(not_integer(Term), Expected)
		).

	a(1).
	a(2).
	a(_) :-
		throw(e).

	b(1).
	b(2).

:- end_object.
