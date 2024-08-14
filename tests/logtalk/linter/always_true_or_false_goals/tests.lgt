%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-08-14,
		comment is 'Unit tests for the ``always_true_or_false_goals`` linter flag.'
	]).

	:- uses(lgtunit, [
		variant/2
	]).

	:- private(goal_is_always_true/5).
	:- dynamic(goal_is_always_true/5).

	:- private(goal_is_always_false/5).
	:- dynamic(goal_is_always_false/5).

	setup :-
		retractall(goal_is_always_true(_, _, _, _, _)),
		retractall(goal_is_always_false(_, _, _, _, _)),
		logtalk_compile(test_entities, [always_true_or_false_goals(warning), suspicious_calls(silent)]).

	cleanup :-
		retractall(goal_is_always_true(_, _, _, _, _)),
		retractall(goal_is_always_false(_, _, _, _, _)).

	test(always_true_or_false_goals_linter_flag_01, exists(Term == (x \== y))) :-
		goal_is_always_true(_, _, object, tautology, Term).

	test(always_true_or_false_goals_linter_flag_02, exists(Term == (x == y))) :-
		goal_is_always_false(_, _, object, tautology, Term).

	test(always_true_or_false_goals_linter_flag_03, exists(Term == (x == y))) :-
		goal_is_always_false(_, _, object, falsehood, Term).

	test(always_true_or_false_goals_linter_flag_04, exists(Term == (x \== y))) :-
		goal_is_always_true(_, _, object, falsehood, Term).

	test(always_true_or_false_goals_linter_flag_05, exists(variant(Term, a is _ * 2))) :-
		goal_is_always_false(_, _, object, falsehood, Term).

	test(always_true_or_false_goals_linter_flag_06, exists(variant(Term, 1 is sqrt(_)))) :-
		goal_is_always_false(_, _, object, falsehood, Term).

	test(always_true_or_false_goals_linter_flag_07, exists(variant(Term, m(2 * 3 + 4) = m(_ * _)))) :-
		goal_is_always_false(_, _, object, falsehood, Term).

	test(always_true_or_false_goals_linter_flag_08, exists(variant(Term, unify_with_occurs_check(m(2 * 3 + 4), m(_ * _))))) :-
		goal_is_always_false(_, _, object, falsehood, Term).

	test(always_true_or_false_goals_linter_flag_09, exists(variant(Term, a(1, _) \= a(2, _)))) :-
		goal_is_always_true(_, _, object, falsehood, Term).

	test(always_true_or_false_goals_linter_flag_10, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(goal_is_always_true(file, 1-2, object, always_true_or_false_goals, x \== y), core), Tokens).

	test(always_true_or_false_goals_linter_flag_11, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(goal_is_always_false(file, 1-2, object, always_true_or_false_goals, x == y), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(goal_is_always_true(File, Lines, Type, Entity, Term), warning(always_true_or_false_goals), core, _) :-
		assertz(goal_is_always_true(File, Lines, Type, Entity, Term)).
	logtalk::message_hook(goal_is_always_false(File, Lines, Type, Entity, Term), warning(always_true_or_false_goals), core, _) :-
		assertz(goal_is_always_false(File, Lines, Type, Entity, Term)).

:- end_object.
