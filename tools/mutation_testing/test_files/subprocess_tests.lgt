%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


% These tests exercise the test entities and are designed to be run by
% mutation testing subprocesses. They should detect mutations applied to
% mt_sample and mt_other_sample entities (i.e. some of them should fail)

:- object(subprocess_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-03,
		comment is 'Tests exercising test entities, run by mutation testing subprocesses to detect mutants.'
	]).

	cover(mt_sample).
	cover(mt_other_sample).
	cover(mt_dcg_sample).
	cover(mt_code_coverage).

	test(mt_subprocess_check_positive, true) :-
		mt_sample::check(1).

	test(mt_subprocess_check_negative, true) :-
		mt_sample::check(-1).

	test(mt_subprocess_target_positive, true(Y == 2)) :-
		mt_sample::target(1, Y).

	test(mt_subprocess_target_negative, true(Y == -2)) :-
		mt_sample::target(-1, Y).

	test(mt_subprocess_term_check, true) :-
		mt_sample::term_check.

	test(mt_subprocess_always_true, true) :-
		mt_sample::always_true.

	test(mt_subprocess_always_false, fail) :-
		mt_sample::always_false.

	test(mt_subprocess_multi_clause, true) :-
		mt_sample::multi_clause_check.

	test(mt_subprocess_neg_multi, true) :-
		mt_sample::neg_multi_check.

	test(mt_subprocess_disj_cmp, true) :-
		mt_sample::disj_cmp_check.

	test(mt_subprocess_arithmetic_value, true(Value == 5)) :-
		mt_sample::arithmetic_value(Value).

	test(mt_subprocess_truth_choice, true) :-
		mt_sample::truth_choice.

	test(mt_subprocess_pair_match, true) :-
		mt_sample::pair_match_check.

	test(mt_subprocess_ordered_choice, true(Value == first)) :-
		mt_sample::ordered_choice_check(Value).

	test(mt_subprocess_head_bound, true) :-
		mt_sample::head_bound_atom_integer_check.

	test(mt_subprocess_other_check_positive, true) :-
		mt_other_sample::check(1).

	test(mt_subprocess_other_check_zero, fail) :-
		mt_other_sample::check(0).

	test(mt_subprocess_code_coverage, true) :-
		mt_code_coverage::p(3, 4).

	test(mt_subprocess_fail_insertion, true) :-
		mt_fail_insertion::a.

	test(mt_subprocess_clauses_reordering, true(L == [1,2,3])) :-
		findall(X, mt_clauses_reordering::p(X), L).

	test(mt_subprocess_body_goal_negation, true) :-
		mt_body_goal_negation::a.

:- end_object.
