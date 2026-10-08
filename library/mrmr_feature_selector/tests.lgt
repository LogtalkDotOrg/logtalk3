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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Unit tests for the MID mRMR feature selector library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		member/2, memberchk/2, length/2
	]).

	cover(mrmr_feature_selector).
	cover(feature_redundancy).

	cleanup :-
		^^clean_file('test_mrmr_output.pl').

	test(mrmr_feature_selector_positive_mid, deterministic(Selected == [signal, distinct, copy])) :-
		mrmr_feature_selector::learn(mrmr_toy, Selector, [selection_strategy(positive_mid)]),
		mrmr_feature_selector::check_selector(Selector),
		mrmr_feature_selector::selected_features(Selector, Selected),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(redundancy_update_rounds(3), Diagnostics),
		memberchk(redundancy_evaluations(6), Diagnostics),
		memberchk(termination(non_positive_mid(step(constant, _, _, MID))), Diagnostics),
		assertion(MID =~= 0.0).

	test(mrmr_feature_selector_positive_mid_zero_first, deterministic(Selected == [])) :-
		Dataset = mrmr_rows([constant-[0]], [example(1, [constant-0], a), example(2, [constant-0], b)]),
		mrmr_feature_selector::learn(Dataset, Selector, [selection_strategy(positive_mid)]),
		mrmr_feature_selector::check_selector(Selector),
		mrmr_feature_selector::selected_features(Selector, Selected),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(redundancy_evaluations(0), Diagnostics),
		memberchk(redundancy_update_rounds(0), Diagnostics),
		memberchk(termination(non_positive_mid(step(constant, _, _, _))), Diagnostics).

	test(mrmr_feature_selector_positive_mid_zero_later, deterministic(Selected == [signal])) :-
		mrmr_feature_selector::learn(mrmr_negative, Selector, [selection_strategy(positive_mid)]),
		mrmr_feature_selector::check_selector(Selector),
		mrmr_feature_selector::selected_features(Selector, Selected),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(redundancy_evaluations(2), Diagnostics),
		memberchk(termination(non_positive_mid(step(noise, _, _, _))), Diagnostics).

	test(mrmr_feature_selector_positive_mid_negative, deterministic) :-
		Dataset = mrmr_rows([strong-[0, 1], weak-[0, 1]], [
			example(1, [strong-0, weak-0], 0),
			example(2, [strong-0, weak-0], 0),
			example(3, [strong-0, weak-1], 0),
			example(4, [strong-1, weak-1], 1),
			example(5, [strong-1, weak-1], 1),
			example(6, [strong-0, weak-0], 1)
		]),
		mrmr_feature_selector::learn(Dataset, Selector, [selection_strategy(positive_mid)]),
		mrmr_feature_selector::check_selector(Selector),
		mrmr_feature_selector::selected_features(Selector, [strong]),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(termination(non_positive_mid(step(weak, _, _, MID))), Diagnostics),
		assertion(MID < 0).

	test(mrmr_feature_selector_positive_mid_exhausted, deterministic) :-
		Dataset = mrmr_rows([signal-[0, 1]], [example(1, [signal-0], a), example(2, [signal-1], b)]),
		mrmr_feature_selector::learn(Dataset, Selector, [selection_strategy(positive_mid)]),
		mrmr_feature_selector::check_selector(Selector),
		mrmr_feature_selector::selected_features(Selector, [signal]),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(termination(candidates_exhausted), Diagnostics),
		memberchk(redundancy_update_rounds(0), Diagnostics).

	test(mrmr_feature_selector_positive_mid_empty, deterministic) :-
		Dataset = mrmr_rows([], [example(1, [], a), example(2, [], b)]),
		mrmr_feature_selector::learn(Dataset, Selector, [selection_strategy(positive_mid)]),
		mrmr_feature_selector::check_selector(Selector),
		mrmr_feature_selector::selected_features(Selector, []).

	test(mrmr_feature_selector_positive_mid_bad_rounds, deterministic) :-
		mrmr_feature_selector::learn(mrmr_toy, mrmr_feature_selector(Scores, Selected, Diagnostics), [selection_strategy(positive_mid)]),
		once(list::select(redundancy_update_rounds(_), Diagnostics, Rest)),
		Bad = mrmr_feature_selector(Scores, Selected, [redundancy_update_rounds(2)| Rest]),
		assertion(\+ mrmr_feature_selector::valid_selector(Bad)).

	test(mrmr_feature_selector_positive_mid_bad_stop, deterministic) :-
		mrmr_feature_selector::learn(mrmr_toy, mrmr_feature_selector(Scores, Selected, Diagnostics), [selection_strategy(positive_mid)]),
		once(list::select(termination(_), Diagnostics, Rest)),
		Bad = mrmr_feature_selector(Scores, Selected, [termination(candidates_exhausted)| Rest]),
		assertion(\+ mrmr_feature_selector::valid_selector(Bad)).

	test(mrmr_feature_selector_positive_mid_repeated, deterministic) :-
		mrmr_feature_selector::learn(mrmr_toy, First, [selection_strategy(positive_mid), selection_strategy(top_k(1))]),
		mrmr_feature_selector::learn(mrmr_toy, Second, [selection_strategy(positive_mid)]),
		mrmr_feature_selector::selected_features(First, [signal, distinct, copy]),
		mrmr_feature_selector::selected_features(Second, [signal, distinct, copy]),
		First = mrmr_feature_selector(Scores, Selected, _),
		Second = mrmr_feature_selector(Scores, Selected, _).

	test(mrmr_feature_selector_positive_mid_export, deterministic) :-
		mrmr_feature_selector::learn(mrmr_toy, Selector, [selection_strategy(positive_mid)]),
		mrmr_feature_selector::export_to_clauses(mrmr_toy, Selector, automatic, [Clause]),
		Clause = automatic(Exported),
		assertion(Exported == Selector),
		mrmr_feature_selector::check_selector(Exported).

	test(mrmr_feature_selector_duplicate_signal, deterministic(Selected == [signal, distinct])) :-
		mrmr_feature_selector::learn(mrmr_toy, Selector, [selection_strategy(top_k(2))]),
		mrmr_feature_selector::check_selector(Selector),
		mrmr_feature_selector::selected_features(Selector, Selected),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(redundancy_evaluations(3), Diagnostics).

	test(mrmr_feature_selector_static_scores, deterministic) :-
		mrmr_feature_selector::learn(mrmr_toy, Selector, [selection_strategy(top_k(2))]),
		mrmr_feature_selector::feature_scores(Selector, [signal-Signal, copy-Copy, distinct-Distinct, constant-Constant]),
		assertion(Signal =~= 1.0),
		assertion(Copy =~= 1.0),
		assertion(Distinct =~= 1.0),
		assertion(Constant =~= 0.0).

	test(mrmr_feature_selector_default_greedy_order, deterministic(Selected == [signal, distinct, copy, constant])) :-
		mrmr_feature_selector::learn(mrmr_toy, Selector),
		mrmr_feature_selector::selected_features(Selector, Selected).

	test(mrmr_feature_selector_trace, deterministic) :-
		mrmr_feature_selector::learn(mrmr_toy, Selector),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(selection_trace([step(signal, Signal, Zero, First), step(distinct, Distinct, Independent, Second),
			step(copy, Copy, Mean, Third), step(constant, Constant, ConstantMean, Fourth)]), Diagnostics),
		assertion(Signal =~= 1.0),
		assertion(Zero =~= 0.0),
		assertion(First =~= 1.0),
		assertion(Distinct =~= 1.0),
		assertion(Independent =~= 0.0),
		assertion(Second =~= 1.0),
		assertion(Copy =~= 1.0),
		assertion(Mean =~= 0.5),
		assertion(Third =~= 0.5),
		assertion(Constant =~= 0.0),
		assertion(ConstantMean =~= 0.0),
		assertion(Fourth =~= 0.0).

	test(mrmr_feature_selector_negative_mid, deterministic) :-
		mrmr_feature_selector::learn(mrmr_negative, Selector),
		mrmr_feature_selector::selected_features(Selector, [signal, noise, noise_copy]),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(selection_trace([_, _, step(noise_copy, Relevance, Mean, MID)]), Diagnostics),
		assertion(Relevance =~= 0.0),
		assertion(Mean =~= 0.5),
		Expected is -0.5,
		assertion(MID =~= Expected).

	test(mrmr_feature_selector_pair_reference, deterministic(Information =~= Expected)) :-
		Left = [a-x, a-x, a-y, b-y, b-y, b-y],
		Right = [u-x, u-x, v-y, u-y, v-y, v-y],
		reference_zip(Left, Right, Pairs),
		reference_mi(Pairs, Expected),
		mrmr_redundancy_probe::pair_mi(Left, Right, Information).

	test(mrmr_feature_selector_redundancy_symmetry, deterministic(Forward =~= Backward)) :-
		Left = [a-x, a-x, a-y, b-y, b-y, b-y],
		Right = [u-x, u-x, v-y, u-y, v-y, v-y],
		mrmr_redundancy_probe::pair_mi(Left, Right, Forward),
		mrmr_redundancy_probe::pair_mi(Right, Left, Backward).

	test(mrmr_feature_selector_redundancy_ignores_target, deterministic(Information =~= 1.0)) :-
		mrmr_redundancy_probe::pair_mi([0-a, 0-b, 1-a, 1-b], [left-z, left-z, right-z, right-z], Information).

	test(mrmr_feature_selector_redundancy_independent, deterministic(Information =~= 0.0)) :-
		mrmr_redundancy_probe::pair_mi([0-a, 0-b, 1-a, 1-b], [0-a, 1-b, 0-a, 1-b], Information).

	test(mrmr_feature_selector_redundancy_constant, deterministic(Information =~= 0.0)) :-
		mrmr_redundancy_probe::pair_mi([0-a, 0-b, 0-a, 0-b], [0-a, 1-b, 0-a, 1-b], Information).

	test(mrmr_feature_selector_redundancy_empty, deterministic(Information =~= 0.0)) :-
		mrmr_redundancy_probe::pair_mi([], [], Information).

	test(mrmr_feature_selector_independent_greedy_reference, deterministic) :-
		reference_columns(Columns),
		reference_dataset(Columns, Dataset),
		reference_selection(Columns, 4, [], Expected, ExpectedTrace),
		mrmr_feature_selector::learn(Dataset, Selector, [selection_strategy(top_k(4))]),
		mrmr_feature_selector::selected_features(Selector, Selected),
		assertion(Selected == Expected),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(selection_trace(Trace), Diagnostics),
		compare_traces(Trace, ExpectedTrace),
		mrmr_feature_selector::feature_scores(Selector, Scores),
		compare_reference_scores(Scores, Columns).

	test(mrmr_feature_selector_evaluation_counts, deterministic) :-
		check_evaluation_count(1, 0),
		check_evaluation_count(2, 3),
		check_evaluation_count(3, 5),
		check_evaluation_count(4, 6),
		check_evaluation_count(20, 6).

	test(mrmr_feature_selector_k_larger_than_candidates, deterministic(Selected == [signal, distinct, copy, constant])) :-
		mrmr_feature_selector::learn(mrmr_toy, Selector, [selection_strategy(top_k(100))]),
		mrmr_feature_selector::selected_features(Selector, Selected).

	test(mrmr_feature_selector_joint_counts, deterministic) :-
		mrmr_feature_selector::learn(mrmr_missing, Selector),
		mrmr_feature_selector::check_selector(Selector),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(example_count(8), Diagnostics),
		memberchk(usable_example_count(4), Diagnostics),
		memberchk(excluded_example_count(4), Diagnostics),
		memberchk(preparation_mode(joint), Diagnostics),
		memberchk(complete_cases([signal-4, copy-4, distinct-4]), Diagnostics),
		memberchk(discretization([signal-equal_frequency(10), copy-categorical, distinct-categorical]), Diagnostics),
		memberchk(occupied_categories([signal-4, copy-2, distinct-2]), Diagnostics).

	test(mrmr_feature_selector_joint_fit_excludes_extrema, deterministic(Selected == [signal, distinct])) :-
		mrmr_feature_selector::learn(mrmr_missing, Selector,
			[selection_strategy(top_k(2)), discretization(equal_width(2))]),
		mrmr_feature_selector::feature_scores(Selector, [signal-Signal, copy-Copy, distinct-Distinct]),
		assertion(Signal =~= 1.0),
		assertion(Copy =~= 1.0),
		assertion(Distinct =~= 1.0),
		mrmr_feature_selector::selected_features(Selector, Selected),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(occupied_categories([signal-2, copy-2, distinct-2]), Diagnostics),
		memberchk(selection_trace([step(signal, _, _, _), step(distinct, _, Mean, _)]), Diagnostics),
		assertion(Mean =~= 0.0).

	test(mrmr_feature_selector_one_fit_all_steps, deterministic) :-
		mrmr_feature_selector::learn(mrmr_missing, Selector, [discretization(equal_width(2))]),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(selection_trace([step(signal, _, _, _), step(distinct, _, _, _), step(copy, _, Mean, _)]), Diagnostics),
		assertion(Mean =~= 0.5),
		memberchk(discretization([signal-equal_width(2), copy-categorical, distinct-categorical]), Diagnostics),
		memberchk(redundancy_evaluations(3), Diagnostics).

	test(mrmr_feature_selector_feature_override, deterministic) :-
		mrmr_feature_selector::learn(mrmr_missing, Selector,
			[feature_discretization(signal, equal_width(1)), discretization(equal_frequency(2))]),
		mrmr_feature_selector::feature_scores(Selector, [copy-Copy, distinct-Distinct, signal-Signal]),
		assertion(Copy =~= 1.0),
		assertion(Distinct =~= 1.0),
		assertion(Signal =~= 0.0),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(discretization([signal-equal_width(1), copy-categorical, distinct-categorical]), Diagnostics).

	test(mrmr_feature_selector_repeated_options, deterministic(Selected == [signal])) :-
		Options = [selection_strategy(top_k(1)), selection_strategy(top_k(3)),
			discretization(equal_width(2)), discretization(equal_frequency(1)),
			feature_discretization(signal, equal_width(2)), feature_discretization(signal, equal_width(1))],
		mrmr_feature_selector::learn(mrmr_missing, Selector, Options),
		mrmr_feature_selector::selector_options(Selector, Stored),
		assertion(Stored == Options),
		mrmr_feature_selector::selected_features(Selector, Selected).

	test(mrmr_feature_selector_public_option_hooks, deterministic) :-
		mrmr_feature_selector::default_options(Defaults),
		assertion(Defaults == [selection_strategy(top_k(10)), discretization(equal_frequency(10))]),
		mrmr_feature_selector::valid_option(selection_strategy(top_k(1))),
		mrmr_feature_selector::valid_option(discretization(categorical)),
		mrmr_feature_selector::valid_option(feature_discretization(signal, equal_width(3))).

	test(mrmr_feature_selector_categorical_labels_preserved, deterministic(Score =~= 1.0)) :-
		Dataset = mrmr_rows([label-[100, 200]],
			[example(1, [label-100], yes), example(2, [label-200], no)]),
		mrmr_feature_selector::learn(Dataset, Selector, [discretization(equal_width(1))]),
		mrmr_feature_selector::feature_scores(Selector, [label-Score]),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(discretization([label-categorical]), Diagnostics),
		memberchk(occupied_categories([label-2]), Diagnostics).

	test(mrmr_feature_selector_constants_tie_order, deterministic(Selected == [zeta, alpha])) :-
		Dataset = mrmr_rows([zeta-[x], alpha-[x]],
			[example(1, [zeta-x, alpha-x], a), example(2, [zeta-x, alpha-x], b)]),
		mrmr_feature_selector::learn(Dataset, Selector),
		mrmr_feature_selector::selected_features(Selector, Selected),
		mrmr_feature_selector::feature_scores(Selector, [zeta-Zeta, alpha-Alpha]),
		assertion(Zeta =~= 0.0),
		assertion(Alpha =~= 0.0).

	test(mrmr_feature_selector_constant_target, deterministic(Selected == [signal, distinct, constant, copy])) :-
		Dataset = mrmr_rows([signal-[0, 1], copy-[0, 1], distinct-[0, 1], constant-[0]],
			[example(1, [signal-0, copy-0, distinct-0, constant-0], x),
			 example(2, [signal-0, copy-0, distinct-1, constant-0], x),
			 example(3, [signal-1, copy-1, distinct-0, constant-0], x),
			 example(4, [signal-1, copy-1, distinct-1, constant-0], x)]),
		mrmr_feature_selector::learn(Dataset, Selector),
		mrmr_feature_selector::selected_features(Selector, Selected).

	test(mrmr_feature_selector_empty_vocabulary, deterministic) :-
		Dataset = mrmr_rows([], [example(1, [], a), example(2, [], b)]),
		mrmr_feature_selector::learn(Dataset, Selector),
		mrmr_feature_selector::feature_scores(Selector, []),
		mrmr_feature_selector::selected_features(Selector, []),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(redundancy_evaluations(0), Diagnostics),
		memberchk(selection_trace([]), Diagnostics).

	test(mrmr_feature_selector_single_candidate, deterministic) :-
		Dataset = mrmr_rows([feature-[a, b]], [example(1, [feature-a], a), example(2, [feature-b], b)]),
		mrmr_feature_selector::learn(Dataset, Selector),
		mrmr_feature_selector::selected_features(Selector, [feature]),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(redundancy_evaluations(0), Diagnostics).

	test(mrmr_feature_selector_no_usable_rows, error(domain_error(mrmr_usable_examples, 0))) :-
		Dataset = mrmr_rows([feature-[a, b]], [example(1, [feature-_], a), example(2, [feature-a], _)]),
		mrmr_feature_selector::learn(Dataset, _).

	test(mrmr_feature_selector_one_usable_row, error(domain_error(mrmr_usable_examples, 1))) :-
		Dataset = mrmr_rows([feature-[a, b]], [example(1, [feature-a], a), example(2, [feature-_], b)]),
		mrmr_feature_selector::learn(Dataset, _).

	test(mrmr_feature_selector_no_examples, error(domain_error(non_empty_examples, Dataset))) :-
		Dataset = mrmr_rows([feature-[a]], []),
		mrmr_feature_selector::learn(Dataset, _).

	test(mrmr_feature_selector_missing_nonmutation, deterministic) :-
		Dataset = mrmr_rows([feature-continuous],
			[example(1, [feature-1], a), example(2, [feature-2], b), example(3, [feature-Missing], a)]),
		copy_term(Dataset, Before),
		mrmr_feature_selector::learn(Dataset, _),
		assertion(var(Missing)),
		assertion(lgtunit::variant(Dataset, Before)).

	test(mrmr_feature_selector_bad_option_zero, error(domain_error(option, selection_strategy(top_k(0))))) :-
		mrmr_feature_selector::learn(mrmr_toy, _, [selection_strategy(top_k(0))]).

	test(mrmr_feature_selector_bad_option_all, error(domain_error(option, selection_strategy(all)))) :-
		mrmr_feature_selector::learn(mrmr_toy, _, [selection_strategy(all)]).

	test(mrmr_feature_selector_bad_option_threshold, error(domain_error(option, selection_strategy(threshold(0.5))))) :-
		mrmr_feature_selector::learn(mrmr_toy, _, [selection_strategy(threshold(0.5))]).

	test(mrmr_feature_selector_bad_option_miq, error(domain_error(option, selection_criterion(miq)))) :-
		mrmr_feature_selector::learn(mrmr_toy, _, [selection_criterion(miq)]).

	test(mrmr_feature_selector_bad_option_bins, error(domain_error(option, discretization(equal_width(0))))) :-
		mrmr_feature_selector::learn(mrmr_toy, _, [discretization(equal_width(0))]).

	test(mrmr_feature_selector_bad_option_variable, error(instantiation_error)) :-
		mrmr_feature_selector::learn(mrmr_toy, _, [_]).

	test(mrmr_feature_selector_bad_option_list, error(type_error(list, bad))) :-
		mrmr_feature_selector::learn(mrmr_toy, _, bad).

	test(mrmr_feature_selector_unknown_override, error(domain_error(unknown_feature, unknown))) :-
		mrmr_feature_selector::learn(mrmr_toy, _, [feature_discretization(unknown, categorical)]).

	test(mrmr_feature_selector_bad_categorical_value, error(domain_error(feature_value, feature-outside))) :-
		Dataset = mrmr_rows([feature-[a, b]], [example(1, [feature-outside], a), example(2, [feature-b], b)]),
		mrmr_feature_selector::learn(Dataset, _).

	test(mrmr_feature_selector_bad_numeric_value, error(type_error(number, bad))) :-
		Dataset = mrmr_rows([feature-continuous], [example(1, [feature-bad], a), example(2, [feature-1], b)]),
		mrmr_feature_selector::learn(Dataset, _).

	test(mrmr_feature_selector_bad_target, error(type_error(atomic, label(a)))) :-
		Dataset = mrmr_rows([feature-[a, b]], [example(1, [feature-a], label(a)), example(2, [feature-b], b)]),
		mrmr_feature_selector::learn(Dataset, _).

	test(mrmr_feature_selector_bad_declaration, error(domain_error(feature_type, feature-unknown))) :-
		Dataset = mrmr_rows([feature-unknown], [example(1, [feature-a], a), example(2, [feature-b], b)]),
		mrmr_feature_selector::learn(Dataset, _).

	test(mrmr_feature_selector_variable_model, error(instantiation_error)) :-
		mrmr_feature_selector::check_selector(_).

	test(mrmr_feature_selector_partial_model_nonmutation, deterministic) :-
		Selector = mrmr_feature_selector(_Scores, [signal], [selection_trace(_Trace)]),
		assertion(\+ mrmr_feature_selector::valid_selector(Selector)).

	test(mrmr_feature_selector_partial_trace_nonmutation, deterministic) :-
		mrmr_feature_selector::learn(mrmr_toy, mrmr_feature_selector(Scores, Selected, Diagnostics)),
		replace_diagnostic(selection_trace([step(signal, _, _, _)| _]), Diagnostics, Changed),
		Selector = mrmr_feature_selector(Scores, Selected, Changed),
		assertion(\+ mrmr_feature_selector::valid_selector(Selector)).

	test(mrmr_feature_selector_wrong_functor, fail) :-
		mrmr_feature_selector::valid_selector(other([], [], [])).

	test(mrmr_feature_selector_invalid_typed_counts, deterministic) :-
		check_bad_diagnostic(candidate_count(bad)),
		check_bad_diagnostic(candidate_count(-1)),
		check_bad_diagnostic(candidate_count(4.0)),
		check_bad_diagnostic(selected_count(bad)),
		check_bad_diagnostic(selected_count(-1)),
		check_bad_diagnostic(selected_count(4.0)),
		check_bad_diagnostic(usable_example_count(bad)),
		check_bad_diagnostic(excluded_example_count(bad)),
		check_bad_diagnostic(redundancy_evaluations(bad)).

	test(mrmr_feature_selector_invalid_metadata, deterministic) :-
		check_bad_diagnostic(model(other)),
		check_bad_diagnostic(example_count(5)),
		check_bad_diagnostic(candidate_count(3)),
		check_bad_diagnostic(selected_count(3)),
		check_bad_diagnostic(usable_example_count(1)),
		check_bad_diagnostic(excluded_example_count(1)),
		check_bad_diagnostic(selection_criterion(miq)),
		check_bad_diagnostic(scoring_metric(symmetrical_uncertainty)),
		check_bad_diagnostic(redundancy_metric(cramers_v)),
		check_bad_diagnostic(preparation_mode(per_feature)),
		check_bad_diagnostic(redundancy_evaluations(7)).

	test(mrmr_feature_selector_invalid_preparation_counts, deterministic) :-
		check_bad_diagnostic(complete_cases([signal-3, copy-4, distinct-4, constant-4])),
		check_bad_diagnostic(complete_cases([signal-4, signal-4, distinct-4, constant-4])),
		check_bad_diagnostic(complete_cases([signal-bad, copy-4, distinct-4, constant-4])),
		check_bad_diagnostic(complete_cases([])),
		check_bad_diagnostic(occupied_categories([signal-5, copy-2, distinct-2, constant-1])),
		check_bad_diagnostic(occupied_categories([signal-0, copy-2, distinct-2, constant-1])),
		check_bad_diagnostic(occupied_categories([])),
		check_bad_diagnostic(discretization([signal-bad, copy-categorical, distinct-categorical, constant-categorical])),
		check_bad_diagnostic(discretization([])).

	test(mrmr_feature_selector_invalid_bin_provenance, deterministic) :-
		mrmr_feature_selector::learn(mrmr_missing, mrmr_feature_selector(Scores, Selected, Diagnostics),
			[feature_discretization(signal, equal_width(2))]),
		replace_diagnostic(occupied_categories([signal-3, copy-2, distinct-2]), Diagnostics, BadCounts),
		check_bad_model(mrmr_feature_selector(Scores, Selected, BadCounts)),
		replace_diagnostic(discretization([signal-equal_width(1), copy-categorical, distinct-categorical]),
			Diagnostics, BadSpecification),
		check_bad_model(mrmr_feature_selector(Scores, Selected, BadSpecification)).

	test(mrmr_feature_selector_invalid_stored_options, deterministic) :-
		check_bad_diagnostic(options([selection_strategy(all), discretization(equal_frequency(10))])),
		check_bad_diagnostic(options([selection_strategy(top_k(2)), discretization(equal_frequency(10))])),
		check_bad_diagnostic(options([discretization(equal_frequency(10))])),
		check_bad_diagnostic(options([selection_strategy(top_k(10))])),
		check_bad_diagnostic(options([selection_strategy(top_k(10)), discretization(equal_frequency(10)),
			feature_discretization(unknown, categorical)])).

	test(mrmr_feature_selector_invalid_traces, deterministic) :-
		check_bad_diagnostic(selection_trace([])),
		check_bad_diagnostic(selection_trace([step(signal, 1, 0, 1)])),
		check_bad_diagnostic(selection_trace([step(signal, 1, 1, 0), step(distinct, 1, 0, 1),
			step(copy, 1, 0.5, 0.5), step(constant, 0, 0, 0)])),
		check_bad_diagnostic(selection_trace([step(signal, 2, 0, 2), step(distinct, 1, 0, 1),
			step(copy, 1, 0.5, 0.5), step(constant, 0, 0, 0)])),
		check_bad_diagnostic(selection_trace([step(signal, 1, 0, 1), step(distinct, 1, 0, 1),
			step(copy, 1, 0.5, 1), step(constant, 0, 0, 0)])),
		check_bad_diagnostic(selection_trace([step(signal, 1, 0, 1), step(copy, 1, 0, 1),
			step(distinct, 1, 0.5, 0.5), step(constant, 0, 0, 0)])),
		check_bad_diagnostic(selection_trace([step(signal, bad, 0, 1), step(distinct, 1, 0, 1),
			step(copy, 1, 0.5, 0.5), step(constant, 0, 0, 0)])),
		Negative is -1,
		check_bad_diagnostic(selection_trace([step(signal, 1, 0, 1), step(distinct, 1, Negative, 2),
			step(copy, 1, 0.5, 0.5), step(constant, 0, 0, 0)])).

	test(mrmr_feature_selector_invalid_scores_and_selection, deterministic) :-
		mrmr_feature_selector::learn(mrmr_toy, mrmr_feature_selector(Scores, Selected, Diagnostics)),
		check_bad_model(mrmr_feature_selector([signal-1, signal-1, distinct-1, constant-0], Selected, Diagnostics)),
		check_bad_model(mrmr_feature_selector([constant-0, signal-1, copy-1, distinct-1], Selected, Diagnostics)),
		check_bad_model(mrmr_feature_selector([copy-1, signal-1, distinct-1, constant-0], Selected, Diagnostics)),
		check_bad_model(mrmr_feature_selector([signal-bad, copy-1, distinct-1, constant-0], Selected, Diagnostics)),
		check_bad_model(mrmr_feature_selector(Scores, [signal, signal, copy, constant], Diagnostics)),
		check_bad_model(mrmr_feature_selector(Scores, [signal, unknown, copy, constant], Diagnostics)),
		check_bad_model(mrmr_feature_selector(Scores, [copy, distinct, signal, constant], Diagnostics)),
		check_bad_model(mrmr_feature_selector(Scores, [], Diagnostics)).

	test(mrmr_feature_selector_export_clauses, deterministic(Loaded == Selector)) :-
		mrmr_feature_selector::learn(mrmr_toy, Selector),
		mrmr_feature_selector::export_to_clauses(mrmr_toy, Selector, mrmr_model, [mrmr_model(Loaded)]),
		mrmr_feature_selector::check_selector(Loaded).

	test(mrmr_feature_selector_print, true) :-
		^^suppress_text_output,
		mrmr_feature_selector::learn(mrmr_toy, Selector),
		mrmr_feature_selector::print_selector(Selector).

	test(mrmr_feature_selector_export_file, true(Loaded == Selector)) :-
		^^file_path('test_mrmr_output.pl', File),
		mrmr_feature_selector::learn(mrmr_toy, Selector),
		mrmr_feature_selector::export_to_file(mrmr_toy, Selector, mrmr_exported_model, File),
		logtalk_load(File),
		{mrmr_exported_model(Loaded)},
		mrmr_feature_selector::check_selector(Loaded).

	test(mrmr_feature_selector_diagnostic_enumeration, deterministic) :-
		mrmr_feature_selector::learn(mrmr_toy, Selector),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		findall(Diagnostic, mrmr_feature_selector::diagnostic(Selector, Diagnostic), Enumerated),
		assertion(Enumerated == Diagnostics).

	% auxiliary predicates

	check_evaluation_count(K, Expected) :-
		mrmr_feature_selector::learn(mrmr_toy, Selector, [selection_strategy(top_k(K))]),
		mrmr_feature_selector::check_selector(Selector),
		mrmr_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(redundancy_evaluations(Expected), Diagnostics).

	replace_diagnostic(Replacement, [Diagnostic| Diagnostics], Changed) :-
		functor(Replacement, Functor, Arity),
		( functor(Diagnostic, Functor, Arity) ->
			Changed = [Replacement| Diagnostics]
		; Changed = [Diagnostic| Rest],
			replace_diagnostic(Replacement, Diagnostics, Rest)
		).

	check_bad_diagnostic(Replacement) :-
		mrmr_feature_selector::learn(mrmr_toy, mrmr_feature_selector(Scores, Selected, Diagnostics)),
		replace_diagnostic(Replacement, Diagnostics, Changed),
		check_bad_model(mrmr_feature_selector(Scores, Selected, Changed)).

	check_bad_model(Selector) :-
		catch(mrmr_feature_selector::check_selector(Selector), Error, true),
		assertion(nonvar(Error)),
		assertion(Error = error(domain_error(selector, Selector), _Context)),
		assertion(\+ mrmr_feature_selector::valid_selector(Selector)).

	reference_columns([
		column(first, [a-x, a-x, a-x, a-y, b-y, b-y, b-z, b-z]),
		column(copy, [a-x, a-x, a-x, a-y, b-y, b-y, b-z, b-z]),
		column(other, [u-x, v-x, u-x, v-y, u-y, v-y, u-z, v-z]),
		column(target_copy, [x-x, x-x, x-x, y-y, y-y, y-y, z-z, z-z]),
		column(constant, [same-x, same-x, same-x, same-y, same-y, same-y, same-z, same-z])
	]).

	reference_dataset(Columns, mrmr_rows(Declarations, Rows)) :-
		findall(
			Feature-Labels,
			(	member(column(Feature, Pairs), Columns),
				findall(Value, member(Value-_Target, Pairs), Values),
				sort(Values, Labels)
			),
			Declarations
		),
		reference_rows(Columns, 1, Rows).

	reference_rows([column(_Feature, [])| _Columns], _Id, []) :-
		!.
	reference_rows(Columns, Id, [example(Id, Values, Target)| Rows]) :-
		reference_row(Columns, Values, Target, Tails),
		NextId is Id + 1,
		reference_rows(Tails, NextId, Rows).

	reference_row([], [], _Target, []).
	reference_row([column(Feature, [Value-Target| Pairs])| Columns], [Feature-Value| Values], Target,
		[column(Feature, Pairs)| Tails]) :-
		reference_row(Columns, Values, Target, Tails).

	reference_mi(Pairs, Information) :-
		length(Pairs, Total),
		sort(Pairs, Cells),
		reference_cells(Cells, Pairs, Total, 0.0, Information).

	reference_cells([], _Pairs, _Total, Information, Information).
	reference_cells([Value-Target| Cells], Pairs, Total, Previous, Information) :-
		findall(1, member(Value-Target, Pairs), Joint),
		findall(1, member(Value-_AnyTarget, Pairs), Rows),
		findall(1, member(_AnyValue-Target, Pairs), Columns),
		length(Joint, CellCount),
		length(Rows, RowCount),
		length(Columns, ColumnCount),
		Next is Previous + CellCount / Total * log(CellCount * Total / (RowCount * ColumnCount)) / log(2),
		reference_cells(Cells, Pairs, Total, Next, Information).

	reference_zip([], [], []).
	reference_zip([Left-_Target| LeftPairs], [Right-_OtherTarget| RightPairs], [Left-Right| Pairs]) :-
		reference_zip(LeftPairs, RightPairs, Pairs).

	reference_selection([], _K, _Chosen, [], []) :-
		!.
	reference_selection(_Columns, 0, _Chosen, [], []) :-
		!.
	reference_selection([Column| Columns], K, Chosen, [Feature| Selected], [Step| Trace]) :-
		reference_step(Column, Chosen, Initial),
		reference_best(Columns, Chosen, Column, Initial, Best, Step),
		Best = column(Feature, _Pairs),
		reference_remove([Column| Columns], Feature, Remaining),
		NextK is K - 1,
		reference_selection(Remaining, NextK, [Best| Chosen], Selected, Trace).

	reference_best([], _Chosen, Best, Step, Best, Step).
	reference_best([Column| Columns], Chosen, Best0, Step0, Best, Step) :-
		reference_step(Column, Chosen, Current),
		Current = step(_Feature, _Relevance, _Mean, MID),
		Step0 = step(_BestFeature, _BestRelevance, _BestMean, BestMID),
		( MID > BestMID ->
			Best1 = Column,
			Step1 = Current
		; Best1 = Best0,
			Step1 = Step0
		),
		reference_best(Columns, Chosen, Best1, Step1, Best, Step).

	reference_step(column(Feature, Pairs), Chosen, step(Feature, Relevance, Mean, MID)) :-
		reference_mi(Pairs, Relevance),
		reference_sum(Chosen, Pairs, Sum),
		length(Chosen, Count),
		( Count =:= 0 ->
			Mean = 0.0
		; Mean is Sum / Count
		),
		MID is Relevance - Mean.

	reference_sum([], _Pairs, 0.0).
	reference_sum([column(_Feature, Selected)| Chosen], Pairs, Sum) :-
		reference_zip(Pairs, Selected, Joint),
		reference_mi(Joint, Information),
		reference_sum(Chosen, Pairs, Rest),
		Sum is Information + Rest.

	reference_remove([column(Feature, _Pairs)| Columns], Feature, Columns) :-
		!.
	reference_remove([Column| Columns], Feature, [Column| Rest]) :-
		reference_remove(Columns, Feature, Rest).

	compare_traces([], []).
	compare_traces([step(Feature, Relevance, Mean, MID)| Trace],
		[step(Feature, ExpectedRelevance, ExpectedMean, ExpectedMID)| Expected]) :-
		assertion(Relevance =~= ExpectedRelevance),
		assertion(Mean =~= ExpectedMean),
		assertion(MID =~= ExpectedMID),
		compare_traces(Trace, Expected).

	compare_reference_scores([], _Columns).
	compare_reference_scores([Feature-Score| Scores], Columns) :-
		memberchk(column(Feature, Pairs), Columns),
		reference_mi(Pairs, Expected),
		assertion(Score =~= Expected),
		compare_reference_scores(Scores, Columns).

:- end_object.
