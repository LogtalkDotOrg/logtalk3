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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 2:1:0,
		author is 'Paulo Moura',
		date is 2022-05-07,
		comment is 'Unit tests for the "optionals" library.'
	]).

	cover(optional).
	cover(optional(_)).
	cover(maybe).

	% type and arbitrary support

	test(optional_type_1_01, true) :-
		type::type(optional).

	test(maybe_type_1_01, true) :-
		type::type(maybe(_)).

	test(maybe_arbitrary_1_01, true) :-
		type::arbitrary(maybe(_)).

	% from_goal/3 tests

	test(optional_from_goal_3_01, true) :-
		optional::from_goal(Y is 1+2, Y, Optional),
		optional(Optional)::is_present.

	test(optional_from_goal_3_02, true(Term == 3)) :-
		optional::from_goal(Y is 1+2, Y, Optional),
		optional(Optional)::get(Term).

	test(optional_from_goal_3_03, true) :-
		optional::from_goal(Y is _, Y, Optional),
		optional(Optional)::is_empty.

	test(optional_from_goal_3_04, true) :-
		optional::from_goal(2 is 3, _, Optional),
		optional(Optional)::is_empty.

	% from_goal/2 tests

	test(optional_from_goal_2_01, true) :-
		optional::from_goal([Y]>>(Y is 1+2), Optional),
		optional(Optional)::is_present.

	test(optional_from_goal_2_02, true(Term == 3)) :-
		optional::from_goal([Y]>>(Y is 1+2), Optional),
		optional(Optional)::get(Term).

	test(optional_from_goal_2_03, true) :-
		optional::from_goal(is(_), Optional),
		optional(Optional)::is_empty.

	test(optional_from_goal_2_04, true) :-
		optional::from_goal(nonvar, Optional),
		optional(Optional)::is_empty.

	% from_generator/3 tests

	test(optional_from_generator_3_01, true) :-
		findall(Optional, optional::from_generator(a(X), X, Optional), [Optional1,Optional2,Optional3]),
		optional(Optional1)::is_present,
		optional(Optional2)::is_present,
		optional(Optional3)::is_empty.

	test(optional_from_generator_3_02, true) :-
		findall(Optional, optional::from_generator(b(X), X, Optional), [Optional1,Optional2,Optional3]),
		optional(Optional1)::get(Value1), Value1 == 1,
		optional(Optional2)::get(Value2), Value2 == 2,
		optional(Optional3)::is_empty.

	% from_generator/2 tests

	test(optional_from_generator_2_01, true) :-
		findall(Optional, optional::from_generator(a, Optional), [Optional1,Optional2,Optional3]),
		optional(Optional1)::is_present,
		optional(Optional2)::is_present,
		optional(Optional3)::is_empty.

	test(optional_from_generator_2_02, true(Value1-Value2 == 1-2)) :-
		findall(Optional, optional::from_generator(b, Optional), [Optional1,Optional2,Optional3]),
		optional(Optional1)::get(Value1),
		optional(Optional2)::get(Value2),
		optional(Optional3)::is_empty.

	% is_empty/0 tests

	test(optional_is_empty_1_01, true) :-
		optional::empty(Optional),
		optional(Optional)::is_empty.

	test(optional_is_empty_1_02, false) :-
		optional::of(0, Optional),
		optional(Optional)::is_empty.

	% is_present/0 tests

	test(optional_is_present_1_01, false) :-
		optional::empty(Optional),
		optional(Optional)::is_present.

	test(optional_is_present_1_02, true) :-
		optional::of(0, Optional),
		optional(Optional)::is_present.

	% if_empty/1 tests

	test(optional_if_empty_1_01, true(X == 1)) :-
		optional::empty(Optional),
		optional(Optional)::if_empty(X = 1).

	test(optional_if_empty_1_02, true(var(X))) :-
		optional::of(0, Optional),
		optional(Optional)::if_empty(X = 1).

	% if_present/1 tests

	test(optional_if_present_1_01, true(var(Y))) :-
		optional::empty(Optional),
		optional(Optional)::if_present({Y}/[X]>>(Y is X + 1)).

	test(optional_if_present_1_02, true(Y == 1)) :-
		optional::of(0, Optional),
		optional(Optional)::if_present({Y}/[X]>>(Y is X + 1)).

	% if_present_or_else/1 tests

	test(optional_if_present_or_else_2_01, true(X == b)) :-
		optional::empty(Optional),
		optional(Optional)::if_present_or_else('='(X), X = b).

	test(optional_if_present_or_else_2_02, true(X == a)) :-
		optional::of(a, Optional),
		optional(Optional)::if_present_or_else('='(X), X = b).

	% filter/2 tests

	test(optional_filter_2_01, true) :-
		optional::of(1, Optional),
		optional(Optional)::filter(integer, NewOptional),
		optional(NewOptional)::is_present.

	test(optional_filter_2_02, true) :-
		optional::of(a, Optional),
		optional(Optional)::filter(integer, NewOptional),
		optional(NewOptional)::is_empty.

	% map/2 tests

	test(optional_map_2_01, true) :-
		optional::empty(Optional),
		optional(Optional)::map(char_code, NewOptional),
		optional(NewOptional)::is_empty.

	test(optional_map_2_02, true(Term == 97)) :-
		optional::of(a, Optional),
		optional(Optional)::map(char_code, NewOptional),
		optional(NewOptional)::get(Term).

	test(optional_map_2_03, true) :-
		optional::of(a, Optional),
		optional(Optional)::map(is(_), NewOptional),
		optional(NewOptional)::is_empty.

	% flat_map/2 tests

	test(optional_flat_map_2_01, true) :-
		optional::empty(Optional),
		optional(Optional)::flat_map(flat_map_closure, NewOptional),
		optional(NewOptional)::is_empty.

	test(optional_flat_map_2_02, true(Term == 97)) :-
		optional::of(a, Optional),
		optional(Optional)::flat_map(flat_map_closure, NewOptional),
		optional(NewOptional)::get(Term).

	test(optional_flat_map_2_03, true) :-
		optional::of(a, Optional),
		optional(Optional)::flat_map(is(_), NewOptional),
		optional(NewOptional)::is_empty.

	% or/2 tests

	test(optional_or_2_01, true) :-
		optional::empty(Optional),
		optional(Optional)::or(NewOptional, optional::empty),
		optional(NewOptional)::is_empty.

	test(optional_or_2_02, true(Term == a)) :-
		optional::empty(Optional),
		optional(Optional)::or(NewOptional, optional::of(a)),
		optional(NewOptional)::get(Term).

	test(optional_or_2_03, true(NewOptional == Optional)) :-
		optional::of(a, Optional),
		optional(Optional)::or(NewOptional, optional::empty).

	% get/1 tests

	test(optional_get_1_01, error(existence_error(optional_term,_))) :-
		optional::empty(Optional),
		optional(Optional)::get(_).

	test(optional_get_1_02, true(Term == 0)) :-
		optional::of(0, Optional),
		optional(Optional)::get(Term).

	% or_else/2 tests

	test(optional_or_else_2_01, true(Term == 0)) :-
		optional::empty(Optional),
		optional(Optional)::or_else(Term, 0).

	test(optional_or_else_2_02, true(Term == 1)) :-
		optional::of(1, Optional),
		optional(Optional)::or_else(Term, 0).

	% or_else_get/2 tests

	test(optional_or_else_get_2_01, true(atom(Term))) :-
		optional::empty(Optional),
		optional(Optional)::or_else_get(Term, current_logtalk_flag(prolog_dialect)).

	test(optional_or_else_get_2_02, true(Term == 1)) :-
		optional::of(1, Optional),
		optional(Optional)::or_else_get(Term, current_logtalk_flag(prolog_dialect)).

	% or_else_call/2 tests

	test(optional_or_else_call_2_01, true(X == 1)) :-
		optional::empty(Optional),
		optional(Optional)::or_else_call(_, X = 1).

	test(optional_or_else_call_2_02, true(var(X))) :-
		optional::of(1, Optional),
		optional(Optional)::or_else_call(_, X = 1).

	% or_else_fail/1 tests

	test(optional_or_else_fail_1_01, false) :-
		optional::empty(Optional),
		optional(Optional)::or_else_fail(_).

	test(optional_or_else_fail_1_02, true(Term == 1)) :-
		optional::of(1, Optional),
		optional(Optional)::or_else_fail(Term).

	% or_else_throw/2 tests

	test(optional_or_else_throw_2_01, ball(some_error)) :-
		optional::empty(Optional),
		optional(Optional)::or_else_throw(_, some_error).

	test(optional_or_else_throw_2_02, true(Term == 1)) :-
		optional::of(1, Optional),
		optional(Optional)::or_else_throw(Term, some_error).

	% "optional" type tests

	test(optional_type_checking_support_01, true) :-
		optional::empty(Optional),
		type::check(optional, Optional).

	test(optional_type_checking_support_02, true) :-
		optional::of(1, Optional),
		type::check(optional, Optional).

	test(optional_type_checking_support_03, true) :-
		optional::from_goal(Y is 1+2, Y, Optional),
		type::check(optional, Optional).

	test(optional_type_checking_support_04, true) :-
		optional::from_goal(Y is _, Y, Optional),
		type::check(optional, Optional).

	test(optional_type_checking_support_05, ball(instantiation_error)) :-
		type::check(optional, _).

	test(optional_type_checking_support_06, ball(type_error(optional,12345))) :-
		type::check(optional, 12345).

	test(optional_type_checking_support_07, ball(type_error(optional,foobar))) :-
		type::check(optional, foobar).

	test(optional_type_checking_support_08, ball(type_error(optional,foo(bar,baz)))) :-
		type::check(optional, foo(bar,baz)).

	% cat/2 tests

	test(maybe_cat_2_01, true(Terms == [])) :-
		maybe::cat([], Terms).

	test(maybe_cat_2_02, true(Terms == [1, 2])) :-
		optional::empty(Optional1),
		optional::of(1, Optional2),
		optional::empty(Optional3),
		optional::of(2, Optional4),
		maybe::cat([Optional1, Optional2, Optional3, Optional4], Terms).

	% "maybe" type tests

	test(maybe_type_checking_support_01, true) :-
		optional::empty(Optional),
		type::check(maybe(integer), Optional).

	test(maybe_type_checking_support_02, true) :-
		optional::of(1, Optional),
		type::check(maybe(integer), Optional).

	test(maybe_type_checking_support_03, true) :-
		optional::from_goal(Y is 1+2, Y, Optional),
		type::check(maybe(integer), Optional).

	test(maybe_type_checking_support_04, true) :-
		optional::from_goal(Y is _, Y, Optional),
		type::check(maybe(integer), Optional).

	test(maybe_type_checking_support_05, ball(instantiation_error)) :-
		type::check(maybe(integer), _).

	test(maybe_type_checking_support_06, ball(type_error(optional,12345))) :-
		type::check(maybe(integer), 12345).

	test(maybe_type_checking_support_07, ball(type_error(optional,foobar))) :-
		type::check(maybe(integer), foobar).

	test(maybe_type_checking_support_08, ball(type_error(optional,foo(bar,baz)))) :-
		type::check(maybe(integer), foo(bar,baz)).

	test(maybe_type_checking_support_09, ball(type_error(integer,a))) :-
		optional::of(a, Optional),
		type::check(maybe(integer), Optional).

	% "maybe" arbitrary tests

	quick_check(
		maybe_arbitrary_support_01,
		type::arbitrary({maybe(integer)}, -maybe(integer))
	).

	% auxiliary predicates

	flat_map_closure(Value, Optional) :-
		char_code(Value, NewValue),
		optional::of(NewValue, Optional).

	a(1).
	a(2).
	a(_) :-
		throw(e).

	b(1).
	b(2).

:- end_object.
