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
		version is 0.11,
		author is 'Paulo Moura',
		date is 2019/01/30,
		comment is 'Unit tests for the "optionals" library.'
	]).

	:- discontiguous([
		fails/1, succeeds/1, throws/2
	]).

	cover(optional).
	cover(optional(_)).
	cover(maybe).

	% type and arbitrary support

	succeeds(optional_type_1_01) :-
		type::type(optional).

	succeeds(maybe_type_1_01) :-
		type::type(maybe(_)).

	succeeds(maybe_arbitrary_1_01) :-
		type::arbitrary(maybe(_)).

	% from_goal/3 tests

	succeeds(optional_from_goal_3_01) :-
		optional::from_goal(Y is 1+2, Y, Ref), optional(Ref)::is_present.

	succeeds(optional_from_goal_3_02) :-
		optional::from_goal(Y is 1+2, Y, Ref), optional(Ref)::get(Term),
		Term == 3.

	succeeds(optional_from_goal_3_03) :-
		optional::from_goal(Y is _, Y, Ref), optional(Ref)::is_empty.

	succeeds(optional_from_goal_3_04) :-
		optional::from_goal(2 is 3, _, Ref), optional(Ref)::is_empty.

	% is_empty/0 tests

	succeeds(optional_is_empty_1_01) :-
		optional::empty(Ref), optional(Ref)::is_empty.

	succeeds(optional_is_empty_1_02) :-
		optional::of(0, Ref), \+ optional(Ref)::is_empty.

	% is_present/0 tests

	succeeds(optional_is_present_1_01) :-
		optional::empty(Ref), \+ optional(Ref)::is_present.

	succeeds(optional_is_present_1_02) :-
		optional::of(0, Ref), optional(Ref)::is_present.

	% if_empty/1 tests

	succeeds(optional_if_empty_1_01) :-
		optional::empty(Ref), optional(Ref)::if_empty(X = 1),
		X == 1.

	succeeds(optional_if_empty_1_02) :-
		optional::of(0, Ref), optional(Ref)::if_empty(X = 1),
		var(X).

	% if_present/1 tests

	succeeds(optional_if_present_1_01) :-
		optional::empty(Ref), optional(Ref)::if_present({Y}/[X]>>(Y is X + 1)),
		var(Y).

	succeeds(optional_if_present_1_02) :-
		optional::of(0, Ref), optional(Ref)::if_present({Y}/[X]>>(Y is X + 1)),
		Y == 1.

	% filter/2 tests

	succeeds(optional_filter_2_01) :-
		optional::of(1, Ref), optional(Ref)::filter(integer, NewRef),
		optional(NewRef)::is_present.

	succeeds(optional_filter_2_02) :-
		optional::of(a, Ref), optional(Ref)::filter(integer, NewRef),
		optional(NewRef)::is_empty.

	% map/2 tests

	succeeds(optional_map_2_01) :-
		optional::empty(Ref), optional(Ref)::map(char_code, NewRef),
		optional(NewRef)::is_empty.

	succeeds(optional_map_2_02) :-
		optional::of(a, Ref), optional(Ref)::map(char_code, NewRef),
		optional(NewRef)::get(Term), Term == 97.

	% flat_map/2 tests

	succeeds(optional_flat_map_2_01) :-
		optional::empty(Ref), optional(Ref)::flat_map(flat_map_closure, NewRef),
		optional(NewRef)::is_empty.

	succeeds(optional_flat_map_2_02) :-
		optional::of(a, Ref), optional(Ref)::flat_map(flat_map_closure, NewRef),
		optional(NewRef)::get(Term), Term == 97.

	% get/1 tests

	throws(optional_get_1_01, error(existence_error(optional_term,_), _)) :-
		optional::empty(Ref), optional(Ref)::get(_).

	succeeds(optional_get_1_02) :-
		optional::of(0, Ref), optional(Ref)::get(Term),
		Term == 0.

	% or_else/2 tests

	succeeds(optional_or_else_2_01) :-
		optional::empty(Ref), optional(Ref)::or_else(Term, 0),
		Term == 0.

	succeeds(optional_or_else_2_02) :-
		optional::of(1, Ref), optional(Ref)::or_else(Term, 0),
		Term == 1.

	% or_else_get/2 tests

	succeeds(optional_or_else_get_2_01) :-
		optional::empty(Ref), optional(Ref)::or_else_get(Term, current_logtalk_flag(prolog_dialect)),
		atom(Term).

	succeeds(optional_or_else_get_2_02) :-
		optional::of(1, Ref), optional(Ref)::or_else_get(Term, current_logtalk_flag(prolog_dialect)),
		Term == 1.

	% or_else_call/2 tests

	succeeds(optional_or_else_call_2_01) :-
		optional::empty(Ref), optional(Ref)::or_else_call(_, X = 1),
		X == 1.

	succeeds(optional_or_else_call_2_02) :-
		optional::of(1, Ref), optional(Ref)::or_else_call(_, X = 1),
		var(X).

	% or_else_fail/1 tests

	fails(optional_or_else_fail_1_01) :-
		optional::empty(Ref), optional(Ref)::or_else_fail(_).

	succeeds(optional_or_else_fail_1_02) :-
		optional::of(1, Ref), optional(Ref)::or_else_fail(Term),
		Term == 1.

	% "optional" type tests

	succeeds(optional_type_checking_support_01) :-
		optional::empty(Ref),
		type::check(optional, Ref).

	succeeds(optional_type_checking_support_02) :-
		optional::of(1, Ref),
		type::check(optional, Ref).

	succeeds(optional_type_checking_support_03) :-
		optional::from_goal(Y is 1+2, Y, Ref),
		type::check(optional, Ref).

	succeeds(optional_type_checking_support_04) :-
		optional::from_goal(Y is _, Y, Ref),
		type::check(optional, Ref).

	throws(optional_type_checking_support_05, instantiation_error) :-
		type::check(optional, _).

	throws(optional_type_checking_support_06, type_error(optional,12345)) :-
		type::check(optional, 12345).

	throws(optional_type_checking_support_07, type_error(optional,foobar)) :-
		type::check(optional, foobar).

	throws(optional_type_checking_support_08, type_error(optional,foo(bar,baz))) :-
		type::check(optional, foo(bar,baz)).

	% cat/2 tests

	succeeds(maybe_cat_2_01) :-
		maybe::cat([], Terms),
		Terms == [].

	succeeds(maybe_cat_2_02) :-
		optional::empty(Ref1),
		optional::of(1, Ref2),
		optional::empty(Ref3),
		optional::of(2, Ref4),
		maybe::cat([Ref1, Ref2, Ref3, Ref4], Terms),
		Terms == [1, 2].

	% "maybe" type tests

	succeeds(maybe_type_checking_support_01) :-
		optional::empty(Ref),
		type::check(maybe(integer), Ref).

	succeeds(maybe_type_checking_support_02) :-
		optional::of(1, Ref),
		type::check(maybe(integer), Ref).

	succeeds(maybe_type_checking_support_03) :-
		optional::from_goal(Y is 1+2, Y, Ref),
		type::check(maybe(integer), Ref).

	succeeds(maybe_type_checking_support_04) :-
		optional::from_goal(Y is _, Y, Ref),
		type::check(maybe(integer), Ref).

	throws(maybe_type_checking_support_05, instantiation_error) :-
		type::check(maybe(integer), _).

	throws(maybe_type_checking_support_06, type_error(optional,12345)) :-
		type::check(maybe(integer), 12345).

	throws(maybe_type_checking_support_07, type_error(optional,foobar)) :-
		type::check(maybe(integer), foobar).

	throws(maybe_type_checking_support_08, type_error(optional,foo(bar,baz))) :-
		type::check(maybe(integer), foo(bar,baz)).

	throws(maybe_type_checking_support_09, type_error(integer,a)) :-
		optional::of(a, Ref),
		type::check(maybe(integer), Ref).

	% "maybe" arbitrary tests

	quick_check(
		maybe_arbitrary_support_01,
		type::arbitrary({maybe(integer)}, -maybe(integer))
	).

	% auxiliary predicates

	flat_map_closure(Value, NewRef) :-
		char_code(Value, NewValue),
		optional::of(NewValue, NewRef).

:- end_object.
