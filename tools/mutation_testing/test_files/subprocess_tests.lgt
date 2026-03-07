%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% These tests exercise the test entities and are designed to be run by
% mutation testing subprocesses. They should detect mutations applied to
% mt_sample and mt_other_sample entities.

:- object(subprocess_tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-03-06,
		comment is 'Tests exercising test entities, run by mutation testing subprocesses to detect mutants.'
	]).

	test(mt_check_positive, true) :-
		mt_sample::check(1).

	test(mt_check_negative, true) :-
		mt_sample::check(-1).

	test(mt_target_positive, true(Y == 2)) :-
		mt_sample::target(1, Y).

	test(mt_target_negative, true(Y == -2)) :-
		mt_sample::target(-1, Y).

	test(mt_term_check, true) :-
		mt_sample::term_check.

	test(mt_always_true, true) :-
		mt_sample::always_true.

	test(mt_always_false, fail) :-
		mt_sample::always_false.

	test(mt_multi_clause, true) :-
		mt_sample::multi_clause_check.

	test(mt_neg_multi, true) :-
		mt_sample::neg_multi_check.

	test(mt_disj_cmp, true) :-
		mt_sample::disj_cmp_check.

	test(mt_arithmetic_value, true(Value == 5)) :-
		mt_sample::arithmetic_value(Value).

	test(mt_truth_choice, true) :-
		mt_sample::truth_choice.

	test(mt_pair_match, true) :-
		mt_sample::pair_match_check.

	test(mt_ordered_choice, true(Value == first)) :-
		mt_sample::ordered_choice_check(Value).

	test(mt_head_bound, true) :-
		mt_sample::head_bound_atom_integer_check.

	test(mt_other_check_positive, true) :-
		mt_other_sample::check(1).

	test(mt_other_check_zero, fail) :-
		mt_other_sample::check(0).

:- end_object.
