%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		version is 0.4,
		author is 'Paulo Moura',
		date is 2019/01/24,
		comment is 'Unit tests for the "expected" library.'
	]).

	:- discontiguous([
		fails/1, succeeds/1, throws/2
	]).

	cover(expected).
	cover(expected(_)).
	cover(either).

	% from_goal/4 tests

	succeeds(expected_from_goal_4_01) :-
		expected::from_goal(Y is 1+2, Y, failure, Ref), expected(Ref)::is_expected.

	succeeds(expected_from_goal_4_02) :-
		expected::from_goal(Y is 1+2, Y, failure, Ref), expected(Ref)::expected(Term),
		Term == 3.

	succeeds(expected_from_goal_4_03) :-
		expected::from_goal(Y is _, Y, failure, Ref), expected(Ref)::is_unexpected.

	succeeds(expected_from_goal_4_04) :-
		expected::from_goal(2 is 3, _, failure, Ref), expected(Ref)::unexpected(Error),
		Error == failure.

	% from_goal/3 tests

	succeeds(expected_from_goal_3_01) :-
		expected::from_goal(Y is 1+2, Y, Ref), expected(Ref)::is_expected.

	succeeds(expected_from_goal_3_02) :-
		expected::from_goal(Y is 1+2, Y, Ref), expected(Ref)::expected(Term),
		Term == 3.

	succeeds(expected_from_goal_3_03) :-
		expected::from_goal(Y is _, Y, Ref), expected(Ref)::is_unexpected.

	% is_unexpected/0 tests

	succeeds(expected_is_unexpected_1_01) :-
		expected::of_unexpected(-1, Ref), expected(Ref)::is_unexpected.

	succeeds(expected_is_unexpected_1_02) :-
		expected::of_expected(1, Ref), \+ expected(Ref)::is_unexpected.

	% is_expected/0 tests

	succeeds(expected_is_expected_1_01) :-
		expected::of_unexpected(-1, Ref), \+ expected(Ref)::is_expected.

	succeeds(expected_is_expected_1_02) :-
		expected::of_expected(1, Ref), expected(Ref)::is_expected.

	% if_unexpected/1 tests

	succeeds(expected_if_unexpected_1_01) :-
		expected::of_unexpected(-1, Ref), expected(Ref)::if_unexpected({Y}/[X]>>(Y is X + 1)),
		Y == 0.

	succeeds(expected_if_unexpected_1_02) :-
		expected::of_expected(1, Ref), expected(Ref)::if_unexpected({Y}/[X]>>(Y is X + 1)),
		var(Y).

	% if_expected/1 tests

	succeeds(expected_if_expected_1_01) :-
		expected::of_unexpected(-1, Ref), expected(Ref)::if_expected({Y}/[X]>>(Y is X + 1)),
		var(Y).

	succeeds(expected_if_expected_1_02) :-
		expected::of_expected(1, Ref), expected(Ref)::if_expected({Y}/[X]>>(Y is X + 1)),
		Y == 2.

	% unexpected/1 tests

	throws(expected_unexpected_1_01, error(existence_error(unexpected_term,_), _)) :-
		expected::of_expected(1, Ref), expected(Ref)::unexpected(_).

	succeeds(expected_unexpected_1_02) :-
		expected::of_unexpected(-1, Ref), expected(Ref)::unexpected(Term),
		Term == -1.

	% expected/1 tests

	throws(expected_expected_1_01, error(existence_error(expected_term,_), _)) :-
		expected::of_unexpected(-1, Ref), expected(Ref)::expected(_).

	succeeds(expected_expected_1_02) :-
		expected::of_expected(1, Ref), expected(Ref)::expected(Term),
		Term == 1.

	% map/2 tests

	succeeds(expected_map_2_01) :-
		expected::of_unexpected(-1, Ref), expected(Ref)::map(char_code, NewRef),
		expected(NewRef)::is_unexpected.

	succeeds(expected_map_2_02) :-
		expected::of_expected(a, Ref), expected(Ref)::map(char_code, NewRef),
		expected(NewRef)::expected(Term), Term == 97.

	% flat_map/2 tests

	succeeds(expected_flat_map_2_01) :-
		expected::of_unexpected(-1, Ref), expected(Ref)::flat_map(flat_map_closure, NewRef),
		expected(NewRef)::is_unexpected.

	succeeds(expected_flat_map_2_02) :-
		expected::of_expected(a, Ref), expected(Ref)::flat_map(flat_map_closure, NewRef),
		expected(NewRef)::expected(Term), Term == 97.

	% or_else/2 tests

	succeeds(expected_or_else_2_01) :-
		expected::of_unexpected(-1, Ref), expected(Ref)::or_else(Term, 0),
		Term == 0.

	succeeds(expected_or_else_2_02) :-
		expected::of_expected(1, Ref), expected(Ref)::or_else(Term, 0),
		Term == 1.

	% or_else_get/2 tests

	succeeds(expected_or_else_get_2_01) :-
		expected::of_unexpected(-1, Ref), expected(Ref)::or_else_get(Term, current_logtalk_flag(prolog_dialect)),
		atom(Term).

	succeeds(expected_or_else_get_2_02) :-
		expected::of_expected(1, Ref), expected(Ref)::or_else_get(Term, current_logtalk_flag(prolog_dialect)),
		Term == 1.

	% or_else_call/2 tests

	succeeds(expected_or_else_call_2_01) :-
		expected::of_unexpected(-1, Ref), expected(Ref)::or_else_call(_, X = 0),
		X == 0.

	succeeds(expected_or_else_call_2_02) :-
		expected::of_expected(1, Ref), expected(Ref)::or_else_call(Term, X = 0),
		Term == 1, var(X).

	% or_else_throw/1 tests

	throws(expected_or_else_throw_1_01, instantiation_error) :-
		expected::of_unexpected(instantiation_error, Ref), expected(Ref)::or_else_throw(_).

	succeeds(expected_or_else_throw_1_02) :-
		expected::of_expected(1, Ref), expected(Ref)::or_else_throw(Term),
		Term == 1.

	% or_else_fail/1 tests

	fails(expected_or_else_fail_1_01) :-
		expected::of_unexpected(-1, Ref), expected(Ref)::or_else_fail(_).

	succeeds(expected_or_else_fail_1_02) :-
		expected::of_expected(1, Ref), expected(Ref)::or_else_fail(Term),
		Term == 1.

	% "expected" type tests

	succeeds(expected_type_checking_support_01) :-
		expected::of_unexpected(-1, Ref),
		type::check(expected, Ref).

	succeeds(expected_type_checking_support_02) :-
		expected::of_expected(1, Ref),
		type::check(expected, Ref).

	succeeds(expected_type_checking_support_03) :-
		expected::from_goal(Y is 1+2, Y, failure, Ref),
		type::check(expected, Ref).

	succeeds(expected_type_checking_support_04) :-
		expected::from_goal(Y is _, Y, failure, Ref),
		type::check(expected, Ref).

	succeeds(expected_type_checking_support_05) :-
		expected::from_goal(Y is 1+2, Y, Ref),
		type::check(expected, Ref).

	succeeds(expected_type_checking_support_06) :-
		expected::from_goal(Y is _, Y, Ref),
		type::check(expected, Ref).

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
		either::expecteds([], Terms),
		Terms == [].

	succeeds(either_expecteds_2_02) :-
		expected::of_unexpected(e1, Ref1),
		expected::of_expected(1, Ref2),
		expected::of_unexpected(e2, Ref3),
		expected::of_expected(2, Ref4),
		either::expecteds([Ref1, Ref2, Ref3, Ref4], Terms),
		Terms == [1, 2].

	% unexpecteds/2 tests

	succeeds(either_unexpecteds_2_01) :-
		either::unexpecteds([], Terms),
		Terms == [].

	succeeds(either_unexpecteds_2_02) :-
		expected::of_unexpected(e1, Ref1),
		expected::of_expected(1, Ref2),
		expected::of_unexpected(e2, Ref3),
		expected::of_expected(2, Ref4),
		either::unexpecteds([Ref1, Ref2, Ref3, Ref4], Terms),
		Terms == [e1, e2].

	% partition/3 tests

	succeeds(either_partition_3_01) :-
		either::partition([], Expecteds, Unexpecteds),
		Expecteds == [],
		Unexpecteds == [].

	succeeds(either_partition_3_02) :-
		expected::of_unexpected(e1, Ref1),
		expected::of_expected(1, Ref2),
		expected::of_unexpected(e2, Ref3),
		expected::of_expected(2, Ref4),
		either::partition([Ref1, Ref2, Ref3, Ref4], Expecteds, Unexpecteds),
		Expecteds == [1, 2],
		Unexpecteds == [e1, e2].

	% "either" type tests

	succeeds(either_type_checking_support_01) :-
		expected::of_unexpected(a, Ref),
		type::check(either(integer, atom), Ref).

	succeeds(either_type_checking_support_02) :-
		expected::of_expected(1, Ref),
		type::check(either(integer, atom), Ref).

	succeeds(either_type_checking_support_03) :-
		expected::from_goal(Y is 1+2, Y, Ref),
		type::check(either(integer, atom), Ref).

	succeeds(either_type_checking_support_04) :-
		expected::from_goal(2 is 3, _, Ref),
		type::check(either(integer, atom), Ref).

	succeeds(either_type_checking_support_05) :-
		expected::from_goal(Y is 1+2, Y, a, Ref),
		type::check(either(integer, atom), Ref).

	succeeds(either_type_checking_support_06) :-
		expected::from_goal(2 is 3, _, a, Ref),
		type::check(either(integer, atom), Ref).

	throws(either_type_checking_support_07, instantiation_error) :-
		type::check(either(integer, atom), _).

	throws(either_type_checking_support_08, type_error(expected,12345)) :-
		type::check(either(integer, atom), 12345).

	throws(either_type_checking_support_09, type_error(expected,foobar)) :-
		type::check(either(integer, atom), foobar).

	throws(either_type_checking_support_10, type_error(expected,foo(bar,baz))) :-
		type::check(either(integer, atom), foo(bar,baz)).

	throws(either_type_checking_support_11, type_error(integer,a)) :-
		expected::of_expected(a, Ref),
		type::check(either(integer, atom), Ref).

	throws(either_type_checking_support_12, type_error(atom,1)) :-
		expected::of_unexpected(1, Ref),
		type::check(either(integer, atom), Ref).

	throws(either_type_checking_support_13, type_error(atom,3)) :-
		expected::from_goal(Y is 1+2, Y, Ref),
		type::check(either(atom, integer), Ref).

	throws(either_type_checking_support_14, type_error(integer,fail)) :-
		expected::from_goal(2 is 3, _, Ref),
		type::check(either(atom, integer), Ref).

	throws(either_type_checking_support_15, type_error(atom,3)) :-
		expected::from_goal(Y is 1+2, Y, fail, Ref),
		type::check(either(atom, integer), Ref).

	throws(either_type_checking_support_16, type_error(integer,fail)) :-
		expected::from_goal(2 is 3, _, fail, Ref),
		type::check(either(atom, integer), Ref).

	% "either" arbitrary tests

	quick_check(
		either_arbitrary_support_01,
		type::arbitrary({either(integer, atom)}, -either(integer, atom))
	).

	% auxiliary predicates

	flat_map_closure(Value, NewRef) :-
		char_code(Value, NewValue),
		expected::of_expected(NewValue, NewRef).

:- end_object.
