%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2017/12/27,
		comment is 'Unit tests for the "expected" library.'
	]).

	:- discontiguous([
		fails/1, succeeds/1, throws/2
	]).

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

	throws(expected_type_checking_support_03, instantiation_error) :-
		type::check(expected, _).

	throws(expected_type_checking_support_04, type_error(expected,12345)) :-
		type::check(expected, 12345).

	throws(expected_type_checking_support_05, type_error(expected,foobar)) :-
		type::check(expected, foobar).

	throws(expected_type_checking_support_06, type_error(expected,foo(bar,baz))) :-
		type::check(expected, foo(bar,baz)).

	% auxiliary predicates

	flat_map_closure(Value, NewRef) :-
		char_code(Value, NewValue),
		expected::of_expected(NewValue, NewRef).

:- end_object.
