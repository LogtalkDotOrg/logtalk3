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
	extends(lgtunit),
	imports([feature_discretization, feature_selector_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Smoke tests for the "feature_selection_protocols" library. Reference values for the feature_demo and regression_demo datasets were computed independently in Python.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, msort/2
	]).

	cover(feature_scoring_common).
	cover(feature_discretization).
	cover(variance_score).
	cover(correlation_score).
	cover(anova_f_score).
	cover(fisher_score).
	cover(mutual_information_score(_)).
	cover(mutual_information_score).
	cover(chi_square_score(_)).
	cover(chi_square_score).
	cover(symmetrical_uncertainty_score(_)).
	cover(symmetrical_uncertainty_score).
	cover(cramers_v_score(_)).
	cover(cramers_v_score).
	cover(feature_selector_common).
	cover(sample_selector).

	cleanup :-
		^^clean_file('test_output.pl'),
		^^clean_file('test_categorical_output.pl'),
		^^clean_file('test_uncertainty_output.pl'),
		^^clean_file('test_cramers_v_output.pl').

	test(feature_selection_protocols_fisher_score_reference, deterministic(Score =~= 9.0)) :-
		fisher_score::score([1, 2, 4, 5], [a, a, b, b], Score).

	test(feature_selection_protocols_fisher_score_anova_factor, deterministic(Fisher =~= Expected)) :-
		anova_f_score::score([1, 2, 4, 5], [a, a, b, b], Anova),
		Expected is Anova / 2,
		fisher_score::score([1, 2, 4, 5], [a, a, b, b], Fisher).

	test(feature_selection_protocols_fisher_score_singleton_classes, deterministic(Score =~= 1.0e10)) :-
		fisher_score::score([1, 2], [a, b], Score).

	test(feature_selection_protocols_fisher_score_empty, deterministic(Score =~= 0.0)) :-
		fisher_score::score([], [], Score).

	test(feature_selection_protocols_fisher_score_constant, deterministic(Score =~= 0.0)) :-
		fisher_score::score([0.1, 0.1, 0.1, 0.1], [a, a, b, b], Score).

	test(feature_selection_protocols_fisher_score_one_class, deterministic(Score =~= 0.0)) :-
		fisher_score::score([1, 2], [a, a], Score).

	test(feature_selection_protocols_fisher_score_missing, deterministic(Score =~= 9.0)) :-
		fisher_score::score([1, _, 2, 4, 100, 5], [a, a, a, b, _, b], Score).

	test(feature_selection_protocols_fisher_score_scale, deterministic(Large =~= Small)) :-
		fisher_score::score([1.0e100, 2.0e100, 4.0e100, 5.0e100], [a, a, b, b], Large),
		fisher_score::score([1.0e-100, 2.0e-100, 4.0e-100, 5.0e-100], [a, a, b, b], Small).

	test(feature_selection_protocols_fisher_score_saturation, deterministic(Score =~= 1.0e10)) :-
		fisher_score::score([1, 1.000001, 5, 5.000001], [a, a, b, b], Score).

	test(feature_selection_protocols_fisher_score_non_numeric, error(type_error(number, bad))) :-
		fisher_score::score([bad], [a], _).

	test(feature_selection_protocols_fisher_score_compound_target, error(type_error(atomic, label(a)))) :-
		fisher_score::score([1], [label(a)], _).

	test(feature_selection_protocols_fisher_score_unaligned, error(consistency_error(list_length, 1, 0))) :-
		fisher_score::score([1], [], _).

	test(feature_selection_protocols_fisher_score_multiclass_reference, deterministic(Score =~= 15.0)) :-
		fisher_score::score([1, 2, 3, 6, 8, 10], [a, a, a, b, b, c], Score).

	test(feature_selection_protocols_fisher_score_multiclass_anova_factor, deterministic(Fisher =~= Expected)) :-
		anova_f_score::score([1, 2, 3, 6, 8, 10], [a, a, a, b, b, c], Anova),
		Expected is Anova * 2 / 3,
		fisher_score::score([1, 2, 3, 6, 8, 10], [a, a, a, b, b, c], Fisher).

	test(feature_selection_protocols_fisher_score_translation, deterministic(Score =~= 9.0)) :-
		fisher_score::score([10001, 10002, 10004, 10005], [a, a, b, b], Score).

	test(feature_selection_protocols_fisher_score_extreme_range, deterministic(Score =~= 9.0)) :-
		fisher_score::score([-1.0e308, -5.0e307, 5.0e307, 1.0e308], [a, a, b, b], Score).

	test(feature_selection_protocols_fisher_score_all_missing, deterministic(Score =~= 0.0)) :-
		fisher_score::score([_, _], [a, b], Score).

	test(feature_selection_protocols_fisher_score_missing_not_bound, deterministic) :-
		fisher_score::score([1, MissingValue, 2], [a, b, MissingTarget], Score),
		assertion(Score =~= 0.0),
		assertion(var(MissingValue)),
		assertion(var(MissingTarget)).

	test(feature_selection_protocols_fisher_score_variable_input, error(instantiation_error)) :-
		fisher_score::score(_, [], _).

	test(feature_selection_protocols_fisher_score_nonlist_input, error(type_error(list, bad))) :-
		fisher_score::score(bad, [], _).

	test(feature_selection_protocols_typed_preparation, deterministic) :-
		^^dataset_examples(feature_selection_categorical_dataset, Features, Examples),
		^^prepare_feature_columns(feature_selection_categorical_dataset, Features, Examples, [], per_feature, Columns, Diagnostics),
		assertion(Columns = [column(signal, categorical, _, 2), column(noise, categorical, _, 2), column(constant, categorical, _, 1)]),
		memberchk(complete_cases([signal-4, noise-4, constant-4]), Diagnostics).

	test(feature_selection_protocols_joint_preparation, deterministic) :-
		^^dataset_examples(feature_demo, Features, Examples),
		^^prepare_feature_columns(feature_demo, Features, Examples, [discretization(equal_width(2))], joint, _Columns, Diagnostics),
		memberchk(usable_example_count(20), Diagnostics),
		memberchk(excluded_example_count(0), Diagnostics).

	test(feature_selection_protocols_override_first, deterministic) :-
		^^dataset_examples(feature_demo, Features, Examples),
		^^prepare_feature_columns(feature_demo, Features, Examples, [feature_discretization(f1, equal_width(1)), feature_discretization(f1, equal_frequency(2))], per_feature, [column(f1, equal_width(1), _, 1)| _], _).

	test(feature_selection_protocols_override_unknown, error(domain_error(unknown_feature, absent))) :-
		^^dataset_examples(feature_demo, Features, Examples),
		^^prepare_feature_columns(feature_demo, Features, Examples, [feature_discretization(absent, categorical)], per_feature, _, _).

	test(feature_selection_protocols_preparation_mode, error(domain_error(preparation_mode, unsupported))) :-
		^^dataset_examples(feature_demo, Features, Examples),
		^^prepare_feature_columns(feature_demo, Features, Examples, [], unsupported, _, _).

	test(feature_selection_protocols_symmetrical_uncertainty_counts_unequal_entropies, deterministic(Score =~= Expected)) :-
		Entropy is -(0.75 * log(0.75) + 0.25 * log(0.25)) / log(2),
		Expected is 2 * (Entropy - 0.5) / (Entropy + 1),
		^^contingency_counts([a-x, a-x, a-x, a-x, a-y, a-y, b-y, b-y], Counts),
		^^contingency_score(symmetrical_uncertainty, Counts, Score).

	test(feature_selection_protocols_cramers_v_counts_unequal_entropies, deterministic(Score =~= Expected)) :-
		Expected is sqrt(1 / 3),
		^^contingency_counts([a-x, a-x, a-x, a-x, a-y, a-y, b-y, b-y], Counts),
		^^contingency_score(cramers_v, Counts, Score).

	test(feature_selection_protocols_normalized_counts_rectangular, deterministic) :-
		Entropy is -(2 / 3 * log(2 / 3) + 1 / 3 * log(1 / 3)) / log(2),
		Expected is 2 * Entropy / (log(3) / log(2) + Entropy),
		^^contingency_counts([a-x, a-x, b-x, b-x, c-y, c-y], Counts),
		^^contingency_score(symmetrical_uncertainty, Counts, Uncertainty),
		^^contingency_score(cramers_v, Counts, Association),
		assertion(Uncertainty =~= Expected),
		assertion(Uncertainty < 1.0),
		assertion(Association =~= 1.0).

	test(feature_selection_protocols_normalized_counts_empty, deterministic) :-
		^^contingency_counts([], Counts),
		^^contingency_score(symmetrical_uncertainty, Counts, Uncertainty),
		^^contingency_score(cramers_v, Counts, Association),
		assertion(Uncertainty =~= 0.0),
		assertion(Association =~= 0.0).

	test(feature_selection_protocols_normalized_counts_constant, deterministic) :-
		^^contingency_counts([a-x, a-y], Counts),
		^^contingency_score(symmetrical_uncertainty, Counts, Uncertainty),
		^^contingency_score(cramers_v, Counts, Association),
		assertion(Uncertainty =~= 0.0),
		assertion(Association =~= 0.0).

	test(feature_selection_protocols_symmetrical_uncertainty_independent, deterministic(Score =~= 0.0)) :-
		symmetrical_uncertainty_score::score([a, a, b, b], [x, y, x, y], Score).

	test(feature_selection_protocols_cramers_v_independent, deterministic(Score =~= 0.0)) :-
		cramers_v_score::score([a, a, b, b], [x, y, x, y], Score).

	test(feature_selection_protocols_symmetrical_uncertainty_perfect, deterministic(Score =~= 1.0)) :-
		symmetrical_uncertainty_score::score([a, a, b, b], [x, x, y, y], Score).

	test(feature_selection_protocols_cramers_v_perfect, deterministic(Score =~= 1.0)) :-
		cramers_v_score::score([a, a, b, b], [x, x, y, y], Score).

	test(feature_selection_protocols_symmetrical_uncertainty_multiclass, deterministic(Score =~= 1.0)) :-
		symmetrical_uncertainty_score::score([a, a, b, b, c, c], [x, x, y, y, z, z], Score).

	test(feature_selection_protocols_cramers_v_multiclass, deterministic(Score =~= 1.0)) :-
		cramers_v_score::score([a, a, b, b, c, c], [x, x, y, y, z, z], Score).

	test(feature_selection_protocols_symmetrical_uncertainty_empty, deterministic(Score =~= 0.0)) :-
		symmetrical_uncertainty_score::score([], [], Score).

	test(feature_selection_protocols_cramers_v_empty, deterministic(Score =~= 0.0)) :-
		cramers_v_score::score([], [], Score).

	test(feature_selection_protocols_symmetrical_uncertainty_constant, deterministic(Score =~= 0.0)) :-
		symmetrical_uncertainty_score::score([a, a], [x, y], Score).

	test(feature_selection_protocols_cramers_v_constant, deterministic(Score =~= 0.0)) :-
		cramers_v_score::score([a, a], [x, y], Score).

	test(feature_selection_protocols_symmetrical_uncertainty_invariants, deterministic) :-
		normalized_invariants(symmetrical_uncertainty_score).

	test(feature_selection_protocols_cramers_v_invariants, deterministic) :-
		normalized_invariants(cramers_v_score).

	test(feature_selection_protocols_symmetrical_uncertainty_degenerate, deterministic) :-
		normalized_degenerate_cases(symmetrical_uncertainty_score).

	test(feature_selection_protocols_cramers_v_degenerate, deterministic) :-
		normalized_degenerate_cases(cramers_v_score).

	test(feature_selection_protocols_symmetrical_uncertainty_missing, deterministic) :-
		normalized_missing_cases(symmetrical_uncertainty_score).

	test(feature_selection_protocols_cramers_v_missing, deterministic) :-
		normalized_missing_cases(cramers_v_score).

	test(feature_selection_protocols_symmetrical_uncertainty_configurations, deterministic) :-
		normalized_configurations(symmetrical_uncertainty_score).

	test(feature_selection_protocols_cramers_v_configurations, deterministic) :-
		normalized_configurations(cramers_v_score).

	test(feature_selection_protocols_normalized_metric_reflection, deterministic) :-
		^^check_scoring_metric(symmetrical_uncertainty_score),
		^^check_scoring_metric(symmetrical_uncertainty_score(categorical)),
		^^check_scoring_metric(symmetrical_uncertainty_score(equal_width(2))),
		^^check_scoring_metric(symmetrical_uncertainty_score(equal_frequency(2))),
		^^check_scoring_metric(cramers_v_score),
		^^check_scoring_metric(cramers_v_score(categorical)),
		^^check_scoring_metric(cramers_v_score(equal_width(2))),
		^^check_scoring_metric(cramers_v_score(equal_frequency(2))).

	test(feature_selection_protocols_symmetrical_uncertainty_selector, deterministic) :-
		normalized_selector(symmetrical_uncertainty_score).

	test(feature_selection_protocols_cramers_v_selector, deterministic) :-
		normalized_selector(cramers_v_score).

	test(feature_selection_protocols_symmetrical_uncertainty_width_selector, deterministic(Selected == [f1])) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(symmetrical_uncertainty_score(equal_width(2))), selection_strategy(top_k(1))]),
		sample_selector::check_selector(Selector),
		sample_selector::selected_features(Selector, Selected).

	test(feature_selection_protocols_symmetrical_uncertainty_frequency_selector, deterministic(Selected == [f1])) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(symmetrical_uncertainty_score(equal_frequency(2))), selection_strategy(top_k(1))]),
		sample_selector::check_selector(Selector),
		sample_selector::selected_features(Selector, Selected).

	test(feature_selection_protocols_cramers_v_width_selector, deterministic(Selected == [f1])) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(cramers_v_score(equal_width(2))), selection_strategy(top_k(1))]),
		sample_selector::check_selector(Selector),
		sample_selector::selected_features(Selector, Selected).

	test(feature_selection_protocols_cramers_v_frequency_selector, deterministic(Selected == [f1])) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(cramers_v_score(equal_frequency(2))), selection_strategy(top_k(1))]),
		sample_selector::check_selector(Selector),
		sample_selector::selected_features(Selector, Selected).

	test(feature_selection_protocols_symmetrical_uncertainty_repeated_options, deterministic) :-
		normalized_repeated_options(symmetrical_uncertainty_score).

	test(feature_selection_protocols_cramers_v_repeated_options, deterministic) :-
		normalized_repeated_options(cramers_v_score).

	test(feature_selection_protocols_symmetrical_uncertainty_export, deterministic(Loaded == Selector)) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(symmetrical_uncertainty_score(equal_frequency(2)))]),
		sample_selector::export_to_clauses(feature_demo, Selector, uncertainty_model, [uncertainty_model(Loaded)]),
		sample_selector::check_selector(Loaded).

	test(feature_selection_protocols_cramers_v_export, deterministic(Loaded == Selector)) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(cramers_v_score(equal_frequency(2)))]),
		sample_selector::export_to_clauses(feature_demo, Selector, cramers_v_model, [cramers_v_model(Loaded)]),
		sample_selector::check_selector(Loaded).

	test(feature_selection_protocols_symmetrical_uncertainty_file_export, true(Loaded == Selector)) :-
		^^file_path('test_uncertainty_output.pl', File),
		sample_selector::learn(feature_demo, Selector, [scoring_metric(symmetrical_uncertainty_score(equal_width(2)))]),
		sample_selector::export_to_file(feature_demo, Selector, feature_selection_uncertainty_model, File),
		logtalk_load(File),
		{feature_selection_uncertainty_model(Loaded)},
		sample_selector::check_selector(Loaded).

	test(feature_selection_protocols_cramers_v_file_export, true(Loaded == Selector)) :-
		^^file_path('test_cramers_v_output.pl', File),
		sample_selector::learn(feature_demo, Selector, [scoring_metric(cramers_v_score(equal_width(2)))]),
		sample_selector::export_to_file(feature_demo, Selector, feature_selection_cramers_v_model, File),
		logtalk_load(File),
		{feature_selection_cramers_v_model(Loaded)},
		sample_selector::check_selector(Loaded).

	test(feature_selection_protocols_symmetrical_uncertainty_variable_configuration, deterministic(var(Configuration))) :-
		catch(symmetrical_uncertainty_score(Configuration)::score([], [], _), Error, true),
		assertion(Error = error(instantiation_error, _)).

	test(feature_selection_protocols_cramers_v_variable_configuration, deterministic(var(Configuration))) :-
		catch(cramers_v_score(Configuration)::score([], [], _), Error, true),
		assertion(Error = error(instantiation_error, _)).

	test(feature_selection_protocols_symmetrical_uncertainty_invalid_configuration, error(domain_error(discretization, unknown))) :-
		symmetrical_uncertainty_score(unknown)::score([], [], _).

	test(feature_selection_protocols_cramers_v_invalid_configuration, error(domain_error(discretization, unknown))) :-
		cramers_v_score(unknown)::score([], [], _).

	test(feature_selection_protocols_symmetrical_uncertainty_invalid_bin_count, error(domain_error(positive_integer, 0))) :-
		symmetrical_uncertainty_score(equal_width(0))::score([], [], _).

	test(feature_selection_protocols_cramers_v_invalid_bin_count, error(type_error(integer, 2.5))) :-
		cramers_v_score(equal_frequency(2.5))::score([], [], _).

	test(feature_selection_protocols_symmetrical_uncertainty_compound_target, error(type_error(atomic, label(a)))) :-
		symmetrical_uncertainty_score::score([a], [label(a)], _).

	test(feature_selection_protocols_cramers_v_compound_value, error(type_error(atomic, value(a)))) :-
		cramers_v_score::score([value(a)], [x], _).

	test(feature_selection_protocols_symmetrical_uncertainty_unaligned_lists, error(consistency_error(list_length, 1, 0))) :-
		symmetrical_uncertainty_score::score([a], [], _).

	test(feature_selection_protocols_cramers_v_nonnumeric_feature, error(type_error(number, a))) :-
		cramers_v_score(equal_width(2))::score([a], [x], _).

	test(feature_selection_protocols_symmetrical_uncertainty_nonlist, error(type_error(list, bad))) :-
		symmetrical_uncertainty_score::score(bad, [], _).

	test(feature_selection_protocols_cramers_v_variable_input, error(instantiation_error)) :-
		cramers_v_score::score(_, [], _).

	test(feature_selection_protocols_mutual_information_independent, deterministic(Score =~= 0.0)) :-
		mutual_information_score::score([a, a, b, b], [x, y, x, y], Score).

	test(feature_selection_protocols_chi_square_independent, deterministic(Score =~= 0.0)) :-
		chi_square_score::score([a, a, b, b], [x, y, x, y], Score).

	test(feature_selection_protocols_mutual_information_perfect, deterministic(Score =~= 1.0)) :-
		mutual_information_score::score([a, a, b, b], [x, x, y, y], Score).

	test(feature_selection_protocols_chi_square_empty_cells, deterministic(Score =~= 4.0)) :-
		chi_square_score::score([a, a, b, b], [x, x, y, y], Score).

	test(feature_selection_protocols_mutual_information_asymmetric, deterministic(Score =~= 0.044110417748401)) :-
		mutual_information_score::score([a, a, a, a, b, b], [x, x, x, y, x, y], Score).

	test(feature_selection_protocols_chi_square_asymmetric, deterministic(Score =~= 0.375)) :-
		chi_square_score::score([a, a, a, a, b, b], [x, x, x, y, x, y], Score).

	test(feature_selection_protocols_mutual_information_multiclass, deterministic(Score =~= Expected)) :-
		Expected is log(3) / log(2),
		mutual_information_score::score([a, a, b, b, c, c], [x, x, y, y, z, z], Score).

	test(feature_selection_protocols_chi_square_multiclass, deterministic(Score =~= 12.0)) :-
		chi_square_score::score([a, a, b, b, c, c], [x, x, y, y, z, z], Score).

	test(feature_selection_protocols_mutual_information_missing, deterministic(Score =~= 1.0)) :-
		mutual_information_score::score([a, _, b, c], [x, z, y, _], Score).

	test(feature_selection_protocols_mutual_information_all_missing, deterministic(Score =~= 0.0)) :-
		mutual_information_score::score([_, _], [x, y], Score).

	test(feature_selection_protocols_chi_square_constant, deterministic(Score =~= 0.0)) :-
		chi_square_score::score([a, a], [x, y], Score).

	test(feature_selection_protocols_mutual_information_constant_target, deterministic(Score =~= 0.0)) :-
		mutual_information_score::score([a, b], [x, x], Score).

	test(feature_selection_protocols_width_mutual_information, deterministic(Score =~= 1.0)) :-
		mutual_information_score(equal_width(2))::score([-2, -1, 1, 2], [x, x, y, y], Score).

	test(feature_selection_protocols_frequency_chi_square, deterministic(Score =~= 4.0)) :-
		chi_square_score(equal_frequency(2))::score([4, 1, 2, 3], [y, x, x, y], Score).

	test(feature_selection_protocols_numeric_categories, deterministic(Score =~= 1.0)) :-
		mutual_information_score::score([10, 10, 20, 20], [1, 1, 2, 2], Score).

	test(feature_selection_protocols_width_integer_precision, deterministic(Pairs == [0-a, 1-b, 1-c])) :-
		^^categorical_pairs(equal_width(2), [9007199254740992, 9007199254740993, 9007199254740994], [a, b, c], Pairs).

	test(feature_selection_protocols_categorical_selector, deterministic(Selected == [signal])) :-
		sample_selector::learn(feature_selection_categorical_dataset, Selector, [scoring_metric(mutual_information_score), selection_strategy(top_k(1))]),
		sample_selector::check_selector(Selector),
		sample_selector::selected_features(Selector, Selected).

	test(feature_selection_protocols_chi_square_selector_threshold, deterministic(Selected == [signal])) :-
		sample_selector::learn(feature_selection_categorical_dataset, Selector, [scoring_metric(chi_square_score), selection_strategy(threshold(1.0))]),
		sample_selector::check_selector(Selector),
		sample_selector::selected_features(Selector, Selected).

	test(feature_selection_protocols_width_selector, deterministic(Selected == [f1])) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(mutual_information_score(equal_width(2))), selection_strategy(top_k(1))]),
		sample_selector::selected_features(Selector, Selected).

	test(feature_selection_protocols_frequency_selector, deterministic(Selected == [f1])) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(chi_square_score(equal_frequency(2))), selection_strategy(top_k(1))]),
		sample_selector::selected_features(Selector, Selected).

	test(feature_selection_protocols_configured_metric_reflection, deterministic) :-
		^^check_scoring_metric(mutual_information_score),
		^^check_scoring_metric(chi_square_score),
		^^check_scoring_metric(mutual_information_score(equal_width(2))),
		^^check_scoring_metric(chi_square_score(equal_frequency(2))).

	test(feature_selection_protocols_configured_metric_export, deterministic(Loaded == Selector)) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(mutual_information_score(equal_frequency(2)))]),
		sample_selector::export_to_clauses(feature_demo, Selector, categorical_model, [categorical_model(Loaded)]),
		sample_selector::check_selector(Loaded).

	test(feature_selection_protocols_configured_metric_file_export, true(Loaded == Selector)) :-
		^^file_path('test_categorical_output.pl', File),
		sample_selector::learn(feature_demo, Selector, [scoring_metric(chi_square_score(equal_width(2)))]),
		sample_selector::export_to_file(feature_demo, Selector, feature_selection_exported_model, File),
		logtalk_load(File),
		{feature_selection_exported_model(Loaded)},
		sample_selector::check_selector(Loaded).

	test(feature_selection_protocols_categorical_repeated_options, deterministic(Selected == [signal])) :-
		sample_selector::learn(feature_selection_categorical_dataset, Selector, [scoring_metric(mutual_information_score), scoring_metric(chi_square_score), selection_strategy(top_k(1))]),
		sample_selector::check_selector(Selector),
		sample_selector::selected_features(Selector, Selected).

	test(feature_selection_protocols_categorical_relabeling, deterministic(Score1 =~= Score2)) :-
		mutual_information_score::score([a, a, b, b], [x, x, y, y], Score1),
		mutual_information_score::score([20, 10, 20, 10], [2, 1, 2, 1], Score2).

	test(feature_selection_protocols_chi_square_permutation, deterministic(Score1 =~= Score2)) :-
		chi_square_score::score([a, a, a, a, b, b], [x, x, x, y, x, y], Score1),
		chi_square_score::score([b, a, b, a, a, a], [y, x, x, y, x, x], Score2).

	test(feature_selection_protocols_one_width_bin, deterministic(Score =~= 0.0)) :-
		mutual_information_score(equal_width(1))::score([1, 2, 3, 4], [x, x, y, y], Score).

	test(feature_selection_protocols_one_frequency_bin, deterministic(Score =~= 0.0)) :-
		chi_square_score(equal_frequency(1))::score([1, 2, 3, 4], [x, x, y, y], Score).

	test(feature_selection_protocols_more_bins_than_observations, deterministic(Pairs == [0-x, 1-y])) :-
		^^categorical_pairs(equal_frequency(20), [1, 2], [x, y], Pairs).

	test(feature_selection_protocols_width_many_bins, deterministic(Pairs == [0-x, 19-y])) :-
		^^categorical_pairs(equal_width(20), [1, 2], [x, y], Pairs).

	test(feature_selection_protocols_empty_categorical_score, deterministic(Score =~= 0.0)) :-
		chi_square_score::score([], [], Score).

	test(feature_selection_protocols_single_categorical_observation, deterministic(Score =~= 0.0)) :-
		mutual_information_score::score([a], [x], Score).

	test(feature_selection_protocols_frequency_complete_cases, deterministic(Score =~= 1.0)) :-
		mutual_information_score(equal_frequency(2))::score([0, 1000, _, 2], [x, _, y, y], Score).

	test(feature_selection_protocols_categorical_compound_value, error(type_error(atomic, value(a)))) :-
		mutual_information_score::score([value(a)], [x], _).

	test(feature_selection_protocols_categorical_nonlist_input, error(type_error(list, bad))) :-
		chi_square_score::score(bad, [], _).

	test(feature_selection_protocols_categorical_variable_input, error(instantiation_error)) :-
		mutual_information_score::score(_, [], _).

	test(feature_selection_protocols_variable_configuration_not_bound, deterministic(var(Configuration))) :-
		catch(mutual_information_score(Configuration)::score([], [], _), Error, true),
		assertion(Error = error(instantiation_error, _)).

	test(feature_selection_protocols_invalid_configured_metric, error(domain_error(discretization, unknown))) :-
		chi_square_score(unknown)::score([], [], _).

	test(feature_selection_protocols_categorical_numeric_identity, deterministic(Score =~= 1.0)) :-
		mutual_information_score::score([1, 1.0], [x, y], Score).

	test(feature_selection_protocols_frequency_cutpoint_equality, deterministic(Pairs == [0-a, 0-b, 1-c, 1-d])) :-
		^^categorical_pairs(equal_frequency(2), [1, 2, 3, 4], [a, b, c, d], Pairs).

	test(feature_selection_protocols_frequency_extreme_range, deterministic(Pairs == [0-a, 0-b, 1-c])) :-
		^^categorical_pairs(equal_frequency(2), [-1.0e308, 0.0, 1.0e308], [a, b, c], Pairs).

	test(feature_selection_protocols_width_boundaries, deterministic(Pairs == [0-a, 1-b, 1-c])) :-
		^^categorical_pairs(equal_width(2), [-2, 0, 2], [a, b, c], Pairs).

	test(feature_selection_protocols_width_extreme_range, deterministic(Pairs == [0-a, 1-b, 1-c])) :-
		^^categorical_pairs(equal_width(2), [-1.0e308, 0.0, 1.0e308], [a, b, c], Pairs).

	test(feature_selection_protocols_width_constant, deterministic(Pairs == [0-a, 0-b])) :-
		^^categorical_pairs(equal_width(10), [0.1, 0.1], [a, b], Pairs).

	test(feature_selection_protocols_frequency_order, deterministic(Pairs == [1-a, 0-b, 0-c, 1-d])) :-
		^^categorical_pairs(equal_frequency(2), [4, 1, 2, 3], [a, b, c, d], Pairs).

	test(feature_selection_protocols_frequency_ties, deterministic(Pairs == [0-a, 0-b, 0-c, 1-d])) :-
		^^categorical_pairs(equal_frequency(4), [1, 1.0, 1, 2], [a, b, c, d], Pairs).

	test(feature_selection_protocols_frequency_constant, deterministic(Pairs == [0-a, 0-b])) :-
		^^categorical_pairs(equal_frequency(10), [2, 2], [a, b], Pairs).

	test(feature_selection_protocols_binning_complete_cases, deterministic(Pairs == [0-a, 1-b])) :-
		^^categorical_pairs(equal_width(2), [0, 1000, _, 2], [a, _, a, b], Pairs).

	test(feature_selection_protocols_invalid_empty_configuration, error(domain_error(discretization, unknown))) :-
		^^categorical_pairs(unknown, [], [], _).

	test(feature_selection_protocols_variable_bin_count, error(instantiation_error)) :-
		^^categorical_pairs(equal_width(_), [], [], _).

	test(feature_selection_protocols_zero_bin_count, error(domain_error(positive_integer, 0))) :-
		^^categorical_pairs(equal_frequency(0), [], [], _).

	test(feature_selection_protocols_noninteger_bin_count, error(type_error(integer, 2.5))) :-
		^^categorical_pairs(equal_width(2.5), [], [], _).

	test(feature_selection_protocols_unaligned_scoring_lists, error(consistency_error(list_length, 1, 0))) :-
		^^categorical_pairs(categorical, [a], [], _).

	test(feature_selection_protocols_continuous_atomic_value, error(type_error(number, a))) :-
		^^categorical_pairs(equal_width(2), [a], [b], _).

	test(feature_selection_protocols_compound_target, error(type_error(atomic, label(a)))) :-
		^^categorical_pairs(categorical, [a], [label(a)], _).

	% feature_dataset_protocol tests

	test(feature_demo_attribute_values_2, true(Features == [f1, f2, f3])) :-
		findall(Feature, feature_demo::attribute_values(Feature, continuous), Features).

	test(feature_demo_example_count_1, deterministic(Count == 20)) :-
		feature_demo::example_count(Count).

	test(feature_demo_example_3, true(Target == pos)) :-
		feature_demo::example(1, Features, Target),
		memberchk(f1-9.322, Features).

	% feature_scoring_common / variance_score tests

	test(variance_score_informative_vs_noise_vs_constant, true) :-
		^^feature_names(feature_demo, FeatureNames),
		^^dataset_examples(feature_demo, Examples),
		^^score_features(variance_score, Examples, FeatureNames, FeatureScores),
		memberchk(f2-VarianceF2, FeatureScores),
		memberchk(f1-VarianceF1, FeatureScores),
		memberchk(f3-VarianceF3, FeatureScores),
		assertion(VarianceF2 =~= 41.2711767275),
		assertion(VarianceF1 =~= 15.09796811),
		assertion(VarianceF3 =~= 0.000944427500000002),
		% f3 is near-constant: it must score lowest
		assertion(VarianceF3 < VarianceF1),
		assertion(VarianceF3 < VarianceF2).

	test(variance_score_all_missing, deterministic(Score =~= 0.0)) :-
		variance_score::score([_, _, _], [a, b, c], Score).

	test(variance_score_ignores_targets, true(Score1 == Score2)) :-
		variance_score::score([1.0, 2.0, 3.0], [a, b, c], Score1),
		variance_score::score([1.0, 2.0, 3.0], [x, y, z], Score2).

	% anova_f_score tests

	test(anova_f_score_informative_feature, true(Score =~= 1045.8975482238875)) :-
		^^feature_values(Examples, f1, Values),
		^^dataset_examples(feature_demo, Examples),
		^^targets(Examples, Targets),
		anova_f_score::score(Values, Targets, Score).

	test(anova_f_score_ranking, true) :-
		^^dataset_examples(feature_demo, Examples),
		^^feature_names(feature_demo, FeatureNames),
		^^score_features(anova_f_score, Examples, FeatureNames, FeatureScores),
		FeatureScores = [f1-ScoreF1, f2-ScoreF2, f3-ScoreF3],
		assertion(ScoreF1 =~= 1045.8975482238875),
		assertion(ScoreF2 =~= 2.539553612615278),
		assertion(ScoreF3 =~= 0.05855858248400004).

	test(anova_f_score_fewer_than_two_groups, deterministic(Score =~= 0.0)) :-
		anova_f_score::score([1.0, 2.0, 3.0], [a, a, a], Score).

	test(anova_f_score_not_enough_examples_for_groups, deterministic(Score =~= 0.0)) :-
		anova_f_score::score([1.0, 2.0], [a, b], Score).

	test(anova_f_score_perfect_separation, deterministic(Score =~= 1.0e10)) :-
		anova_f_score::score([1.0, 1.0, 5.0, 5.0], [a, a, b, b], Score).

	test(anova_f_score_missing_values_excluded, true(Score =~= 1045.8975482238875)) :-
		^^dataset_examples(feature_demo, Examples),
		^^feature_values(Examples, f1, Values),
		^^targets(Examples, Targets),
		anova_f_score::score([_| Values], [extra| Targets], Score).

	% correlation_score tests

	test(correlation_score_informative_vs_noise, true) :-
		^^dataset_examples(regression_demo, Examples),
		^^feature_names(regression_demo, FeatureNames),
		^^score_features(correlation_score, Examples, FeatureNames, FeatureScores),
		FeatureScores = [f1-ScoreF1, f2-ScoreF2, f3-ScoreF3],
		assertion(ScoreF1 =~= 0.9970241869039738),
		assertion(ScoreF2 =~= 0.03550408009919597),
		assertion(ScoreF3 =~= 0.0030511757182571674).

	test(correlation_score_fewer_than_two_examples, deterministic(Score =~= 0.0)) :-
		correlation_score::score([1.0], [2.0], Score).

	test(correlation_score_constant_feature, deterministic(Score =~= 0.0)) :-
		correlation_score::score([5.0, 5.0, 5.0], [1.0, 2.0, 3.0], Score).

	test(correlation_score_constant_target, deterministic(Score =~= 0.0)) :-
		correlation_score::score([1.0, 2.0, 3.0], [5.0, 5.0, 5.0], Score).

	test(correlation_score_sign_independent, true(Score1 =~= Score2)) :-
		correlation_score::score([1.0, 2.0, 3.0], [3.0, 2.0, 1.0], Score1),
		correlation_score::score([1.0, 2.0, 3.0], [1.0, 2.0, 3.0], Score2).

	% feature_scoring_common auxiliary helper tests

	test(feature_selection_protocols_constant_decimal_variance, deterministic(Score =~= 0.0)) :-
		variance_score::score([0.1, 0.1, 0.1], [], Score).

	test(feature_selection_protocols_constant_decimal_correlation, deterministic(Score =~= 0.0)) :-
		correlation_score::score([0.1, 0.1, 0.1], [0.1, 0.1, 0.1], Score).

	test(feature_selection_protocols_correlation_large_scale, deterministic(Score =~= 1.0)) :-
		correlation_score::score([1.0e100, 2.0e100, 3.0e100], [1.0e100, 2.0e100, 3.0e100], Score).

	test(feature_selection_protocols_correlation_small_scale, deterministic(Score =~= 1.0)) :-
		correlation_score::score([1.0e-100, 2.0e-100, 3.0e-100], [3.0e-100, 2.0e-100, 1.0e-100], Score).

	test(feature_selection_protocols_correlation_missing_targets, deterministic(Score =~= 1.0)) :-
		correlation_score::score([1.0, 99.0, _, 3.0], [2.0, _, 99.0, 6.0], Score).

	test(feature_selection_protocols_anova_saturation, deterministic(Perfect == NearPerfect)) :-
		anova_f_score::score([1.0, 1.0, 5.0, 5.0], [a, a, b, b], Perfect),
		anova_f_score::score([1.0, 1.000001, 5.0, 5.000001], [a, a, b, b], NearPerfect).

	test(feature_selection_protocols_anova_decimal_constant, deterministic(Score =~= 0.0)) :-
		anova_f_score::score([0.1, 0.1, 0.1, 0.1], [a, a, b, b], Score).

	test(feature_selection_protocols_anova_scale_invariant, deterministic(Score1 =~= Score2)) :-
		anova_f_score::score([1.0, 2.0, 4.0, 5.0], [a, a, b, b], Score1),
		anova_f_score::score([1.0e100, 2.0e100, 4.0e100, 5.0e100], [a, a, b, b], Score2).

	test(complete_pairs_3, deterministic(Pairs == [1.0-a, 3.0-c])) :-
		^^complete_pairs([1.0, _, 3.0], [a, b, c], Pairs).

	test(complete_values_2, deterministic(Values == [1.0, 3.0])) :-
		^^complete_values([1.0, _, 3.0], Values).

	test(group_by_target_2, deterministic(ValuesB == [2.0])) :-
		^^group_by_target([1.0-a, 2.0-b, 3.0-a], Groups),
		memberchk(a-ValuesA, Groups),
		memberchk(b-ValuesB, Groups),
		msort(ValuesA, [1.0, 3.0]).

	% feature_selector_common shared helper tests

	test(dataset_examples_feature_demo, true(Count == 20)) :-
		^^dataset_examples(feature_demo, Examples),
		length(Examples, Count).

	test(dataset_examples_unknown_feature, error(domain_error(unknown_feature, f9))) :-
		^^dataset_examples(bad_feature_dataset, _).

	test(dataset_examples_inconsistent_count, error(consistency_error(example_count, 5, 3))) :-
		^^dataset_examples(inconsistent_example_count, _).

	test(dataset_examples_no_examples, error(domain_error(non_empty_examples, no_examples))) :-
		^^dataset_examples(no_examples, _).

	test(feature_names_2, deterministic(FeatureNames == [f1, f2, f3])) :-
		^^feature_names(feature_demo, FeatureNames).

	test(feature_values_3_missing, deterministic) :-
		^^feature_values([example(1, [f1-1.0], _), example(2, [], _), example(3, [f1-3.0], _)], f1, Values),
		Values = [First, Second, Third],
		assertion(First =~= 1.0),
		assertion(var(Second)),
		assertion(Third =~= 3.0).

	test(targets_2, deterministic(Targets == [pos, neg])) :-
		^^targets([example(1, [], pos), example(2, [], neg)], Targets).

	test(select_top_k_3, deterministic(Selected == [f1, f2])) :-
		^^select_top_k([f1-10.0, f2-5.0, f3-1.0], 2, Selected).

	test(select_top_k_fewer_than_requested, deterministic(Selected == [f1, f2, f3])) :-
		^^select_top_k([f1-10.0, f2-5.0, f3-1.0], 10, Selected).

	test(select_top_k_zero, deterministic(Selected == [])) :-
		^^select_top_k([f1-10.0, f2-5.0], 0, Selected).

	test(select_above_threshold_3, deterministic(Selected == [f1])) :-
		^^select_above_threshold([f1-10.0, f2-5.0, f3-1.0], 7.0, Selected).

	test(select_above_threshold_none, deterministic(Selected == [])) :-
		^^select_above_threshold([f1-10.0, f2-5.0], 100.0, Selected).

	test(check_scoring_metric_valid, deterministic) :-
		^^check_scoring_metric(variance_score).

	test(check_scoring_metric_unbound, error(instantiation_error)) :-
		^^check_scoring_metric(_).

	test(check_scoring_metric_invalid, error(domain_error(scoring_metric, not_an_object))) :-
		^^check_scoring_metric(not_an_object).

	test(valid_selector_metadata_3_true, deterministic) :-
		^^valid_selector_metadata(sample_selector, [scoring_metric(variance_score), selection_strategy(all)], [model(sample_selector), example_count(20), options([scoring_metric(variance_score), selection_strategy(all)]), selected_count(3)]).

	test(valid_selector_metadata_3_false, fail) :-
		^^valid_selector_metadata(sample_selector, [scoring_metric(anova_f_score), selection_strategy(all)], [model(sample_selector), example_count(20), options([scoring_metric(variance_score), selection_strategy(all)]), selected_count(3)]).

	% sample_selector end-to-end tests

	test(feature_selection_protocols_duplicate_declaration, error(domain_error(duplicate_feature, f1))) :-
		sample_selector::learn(feature_selection_invalid_dataset(duplicate_declaration), _Selector).

	test(feature_selection_protocols_duplicate_example, error(domain_error(duplicate_feature, f1))) :-
		sample_selector::learn(feature_selection_invalid_dataset(duplicate_example), _Selector).

	test(feature_selection_protocols_dataset_collection, deterministic(Features == [f1, f2, f3])) :-
		^^dataset_examples(feature_demo, Features, Examples),
		length(Examples, 20).

	test(feature_selection_protocols_indexed_missing_alignment, deterministic(Score =~= 1.0)) :-
		Examples = [example(1, [b-99.0, a-1.0], 2.0), example(2, [b-1.0], 99.0), example(3, [a-3.0, b-2.0], 6.0)],
		^^score_features(correlation_score, Examples, [a], [a-Score]).

	test(feature_selection_protocols_stable_ties, deterministic(Selected == [a, b])) :-
		^^score_features(variance_score, [example(1, [c-1.0, a-1.0, b-1.0], _)], [a, b, c], Scores),
		^^select_top_k(Scores, 2, Selected).

	test(feature_selection_protocols_grouping_preserves_values, deterministic(Groups == [a-[1, 3], b-[2, 4]])) :-
		^^group_by_target([1-a, 2-b, 3-a, 4-b], Groups).

	test(sample_selector_learn_2, deterministic(ground(Selector))) :-
		sample_selector::learn(feature_demo, Selector).

	test(sample_selector_learn_3, deterministic(ground(Selector))) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(anova_f_score)]).

	test(sample_selector_valid_selector, deterministic(sample_selector::valid_selector(Selector))) :-
		sample_selector::learn(feature_demo, Selector).

	test(sample_selector_invalid_selector, fail) :-
		sample_selector::valid_selector(sample_selector(not_an_object, [], [], [model(sample_selector), example_count(20), options([]), selected_count(0)])).

	test(sample_selector_default_options, true) :-
		sample_selector::learn(feature_demo, Selector),
		sample_selector::selector_options(Selector, Options),
		memberchk(scoring_metric(variance_score), Options),
		memberchk(selection_strategy(all), Options).

	test(sample_selector_selected_features_all, deterministic(Selected == [f2, f1, f3])) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(variance_score)]),
		sample_selector::selected_features(Selector, Selected).

	test(sample_selector_selected_features_top_k, deterministic(Selected == [f1, f2])) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(anova_f_score), selection_strategy(top_k(2))]),
		sample_selector::selected_features(Selector, Selected).

	test(sample_selector_selected_features_threshold, deterministic(Selected == [f1])) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(anova_f_score), selection_strategy(threshold(10.0))]),
		sample_selector::selected_features(Selector, Selected).

	test(sample_selector_selected_features_threshold_none, deterministic(Selected == [])) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(anova_f_score), selection_strategy(threshold(2000.0))]),
		sample_selector::selected_features(Selector, Selected).

	test(sample_selector_feature_scores_2, true) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(anova_f_score)]),
		sample_selector::feature_scores(Selector, FeatureScores),
		FeatureScores = [f1-ScoreF1, f2-ScoreF2, f3-ScoreF3],
		assertion(ScoreF1 =~= 1045.8975482238875),
		assertion(ScoreF2 =~= 2.539553612615278),
		assertion(ScoreF3 =~= 0.05855858248400004).

	test(sample_selector_diagnostics_2, true) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(anova_f_score), selection_strategy(top_k(2))]),
		sample_selector::diagnostics(Selector, Diagnostics),
		memberchk(model(sample_selector), Diagnostics),
		memberchk(example_count(20), Diagnostics),
		memberchk(selected_count(2), Diagnostics).

	test(sample_selector_diagnostic_2_enumeration, true(Names == [model, example_count, options])) :-
		sample_selector::learn(feature_demo, Selector),
		findall(Name, (sample_selector::diagnostic(Selector, Diagnostic), functor(Diagnostic, Name, 1)), [Name1, Name2, Name3| _]),
		Names = [Name1, Name2, Name3].

	test(sample_selector_learn_2_same_as_learn_3, true(Selector0 == Selector1)) :-
		sample_selector::learn(feature_demo, Selector0),
		sample_selector::learn(feature_demo, Selector1, []).

	test(sample_selector_invalid_scoring_metric_option, error(domain_error(option, scoring_metric(not_an_object)))) :-
		sample_selector::learn(feature_demo, _, [scoring_metric(not_an_object)]).

	test(sample_selector_invalid_top_k_option, error(domain_error(option, selection_strategy(top_k(0))))) :-
		sample_selector::learn(feature_demo, _, [selection_strategy(top_k(0))]).

	test(sample_selector_invalid_threshold_option, error(domain_error(option, selection_strategy(threshold(foo))))) :-
		sample_selector::learn(feature_demo, _, [selection_strategy(threshold(foo))]).

	test(sample_selector_unknown_option, error(domain_error(option, bogus(1)))) :-
		sample_selector::learn(feature_demo, _, [bogus(1)]).

	test(sample_selector_check_selector_unbound, error(instantiation_error)) :-
		sample_selector::check_selector(_).

	test(sample_selector_check_selector_invalid, error(domain_error(selector, foo))) :-
		sample_selector::check_selector(foo).

	% export and printing

	test(feature_selection_protocols_declared_metric_rejected, error(domain_error(scoring_metric, feature_selection_declared_metric))) :-
		^^check_scoring_metric(feature_selection_declared_metric).

	test(feature_selection_protocols_declared_metric_option, error(domain_error(option, scoring_metric(feature_selection_declared_metric)))) :-
		sample_selector::learn(feature_demo, _, [scoring_metric(feature_selection_declared_metric)]).

	test(feature_selection_protocols_inherited_metric, deterministic) :-
		^^check_scoring_metric(feature_selection_inherited_metric),
		sample_selector::learn(feature_demo, Selector, [scoring_metric(feature_selection_inherited_metric)]),
		sample_selector::check_selector(Selector).

	test(feature_selection_protocols_category_metric, deterministic) :-
		^^check_scoring_metric(feature_selection_category_metric).

	test(feature_selection_protocols_mixed_numeric_ties, deterministic(Features == [a, b, c])) :-
		^^score_features(feature_selection_category_metric, [example(1, [a-1, b-1.0, c-1], _)], [a, b, c], Scores),
		^^select_top_k(Scores, 3, Features).

	test(feature_selection_protocols_integer_score_precision, deterministic(Features == [b, a])) :-
		^^score_features(feature_selection_category_metric, [example(1, [a-9007199254740992, b-9007199254740993], _)], [a, b], Scores),
		^^select_top_k(Scores, 2, Features).

	test(feature_selection_protocols_malformed_scores, fail) :-
		sample_selector::valid_selector(sample_selector(variance_score, [garbage], [not_scored], [model(sample_selector)])).

	test(feature_selection_protocols_partial_selector_not_bound, deterministic(var(Scores))) :-
		Selector = sample_selector(variance_score, Scores, [], [model(sample_selector)]),
		assertion(\+ sample_selector::valid_selector(Selector)).

	test(feature_selection_protocols_missing_options, fail) :-
		sample_selector::valid_selector(sample_selector(variance_score, [], [], [model(sample_selector), example_count(1), selected_count(0)])).

	test(feature_selection_protocols_unsorted_scores, fail) :-
		sample_selector::learn(feature_demo, sample_selector(Metric, [First, Second, Third], Selected, Diagnostics)),
		sample_selector::valid_selector(sample_selector(Metric, [Third, Second, First], Selected, Diagnostics)).

	test(feature_selection_protocols_duplicate_model_scores, fail) :-
		sample_selector::learn(feature_demo, sample_selector(Metric, [First| Scores], Selected, Diagnostics)),
		sample_selector::valid_selector(sample_selector(Metric, [First, First| Scores], Selected, Diagnostics)).

	test(feature_selection_protocols_unknown_selected_feature, fail) :-
		sample_selector::learn(feature_demo, sample_selector(Metric, Scores, _Selected, Diagnostics)),
		sample_selector::valid_selector(sample_selector(Metric, Scores, [unknown], Diagnostics)).

	test(feature_selection_protocols_wrong_selection_strategy, fail) :-
		sample_selector::learn(feature_demo, sample_selector(Metric, Scores, _Selected, Diagnostics)),
		sample_selector::valid_selector(sample_selector(Metric, Scores, [], Diagnostics)).

	test(feature_selection_protocols_wrong_selected_count, fail) :-
		sample_selector::learn(feature_demo, sample_selector(Metric, Scores, Selected, [Model, Count, Options| _])),
		sample_selector::valid_selector(sample_selector(Metric, Scores, Selected, [Model, Count, Options, selected_count(99)])).

	test(feature_selection_protocols_repeated_options, deterministic(Selected == [f1, f2])) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(anova_f_score), scoring_metric(variance_score), selection_strategy(top_k(2)), selection_strategy(all)]),
		sample_selector::check_selector(Selector),
		sample_selector::selected_features(Selector, Selected).

	test(feature_selection_protocols_invalid_stored_option, fail) :-
		sample_selector::learn(feature_demo, sample_selector(Metric, Scores, Selected, [Model, Count, options(Options), SelectedCount])),
		sample_selector::valid_selector(sample_selector(Metric, Scores, Selected, [Model, Count, options([bogus(1)| Options]), SelectedCount])).

	test(feature_selection_protocols_wrong_stored_metric, fail) :-
		sample_selector::learn(feature_demo, sample_selector(_Metric, Scores, Selected, Diagnostics)),
		sample_selector::valid_selector(sample_selector(anova_f_score, Scores, Selected, Diagnostics)).

	test(feature_selection_protocols_invalid_count_selector_error, error(domain_error(selector, Selector))) :-
		Selector = sample_selector(variance_score, [], [], [model(sample_selector), example_count(1), options([scoring_metric(variance_score), selection_strategy(all)]), selected_count(bad)]),
		sample_selector::check_selector(Selector).

	test(feature_selection_protocols_non_numeric_score, fail) :-
		Selector = sample_selector(variance_score, [f1-bad], [f1], [model(sample_selector), example_count(1), options([scoring_metric(variance_score), selection_strategy(all)]), selected_count(1)]),
		sample_selector::valid_selector(Selector).

	test(feature_selection_protocols_duplicate_selected_features, fail) :-
		sample_selector::learn(feature_demo, sample_selector(Metric, Scores, [First| Selected], Diagnostics)),
		sample_selector::valid_selector(sample_selector(Metric, Scores, [First, First| Selected], Diagnostics)).

	test(feature_selection_protocols_malformed_feature_list, error(type_error(pair, not_a_pair))) :-
		sample_selector::learn(feature_selection_invalid_dataset(malformed_features), _Selector).

	test(feature_selection_protocols_variable_feature, error(instantiation_error)) :-
		sample_selector::learn(feature_selection_invalid_dataset(variable_feature), _Selector).

	test(feature_selection_protocols_nonpositive_count, error(domain_error(positive_integer, 0))) :-
		sample_selector::learn(feature_selection_invalid_dataset(zero_count), _Selector).

	test(feature_selection_protocols_noninteger_count, error(type_error(integer, bad))) :-
		sample_selector::learn(feature_selection_invalid_dataset(non_integer_count), _Selector).

	test(feature_selection_protocols_correlation_opposite_extremes, deterministic(Score =~= 1.0)) :-
		correlation_score::score([-1.0e308, 0.0, 1.0e308], [1.0e308, 0.0, -1.0e308], Score).

	test(feature_selection_protocols_constant_decimal_target, deterministic(Score =~= 0.0)) :-
		correlation_score::score([1.0, 2.0, 3.0], [0.1, 0.1, 0.1], Score).

	test(feature_selection_protocols_constant_decimal_feature, deterministic(Score =~= 0.0)) :-
		correlation_score::score([0.1, 0.1, 0.1], [1.0, 2.0, 3.0], Score).

	test(feature_selection_protocols_anova_decimal_separation, deterministic(Score =~= 1.0e10)) :-
		anova_f_score::score([0.1, 0.1, 0.1, 0.2, 0.2, 0.2], [a, a, a, b, b, b], Score).

	test(feature_selection_protocols_large_finite_variance, deterministic(Relative =~= 1.0)) :-
		variance_score::score([-1.0e154, 1.0e154], [], Score),
		Relative is Score / 1.0e308.

	test(feature_selection_protocols_variance_integer_offsets, deterministic(Score =~= 0.25)) :-
		variance_score::score([9007199254740992, 9007199254740993], [], Score).

	test(feature_selection_protocols_mean_opposite_extremes, deterministic(Mean =~= 0.0)) :-
		^^mean_values([-1.0e308, 1.0e308], Mean).

	test(feature_selection_protocols_mean_large_constant, deterministic(Relative =~= 1.0)) :-
		^^mean_values([1.0e308, 1.0e308], Mean),
		Relative is Mean / 1.0e308.

	test(feature_selection_protocols_mean_cancellation_tail, deterministic(Mean =~= Expected)) :-
		Expected is 1.0 / 3.0,
		^^mean_values([-1.0e308, 1.0e308, 1.0], Mean).

	test(sample_selector_export_to_clauses_4, true(Clause == selector_model(Selector))) :-
		sample_selector::learn(feature_demo, Selector, [scoring_metric(anova_f_score), selection_strategy(top_k(2))]),
		sample_selector::export_to_clauses(feature_demo, Selector, selector_model, [Clause]).

	test(sample_selector_export_to_file_4_header, true(HeaderLine == '% exported selector predicate: selector_model/1')) :-
		^^file_path('test_output.pl', File),
		sample_selector::learn(feature_demo, Selector, [scoring_metric(anova_f_score), selection_strategy(top_k(2))]),
		sample_selector::export_to_file(feature_demo, Selector, selector_model, File),
		first_header_line(File, HeaderLine).

	test(sample_selector_export_to_file_4_loadable, true(Selected == [f1, f2])) :-
		^^file_path('test_output.pl', File),
		sample_selector::learn(feature_demo, Selector, [scoring_metric(anova_f_score), selection_strategy(top_k(2))]),
		sample_selector::export_to_file(feature_demo, Selector, selector_model, File),
		logtalk_load(File),
		{selector_model(Loaded)},
		sample_selector::valid_selector(Loaded),
		sample_selector::selected_features(Loaded, Selected).

	test(sample_selector_print_selector_1, true) :-
		^^suppress_text_output,
		sample_selector::learn(feature_demo, Selector),
		sample_selector::print_selector(Selector).

	% auxiliary predicates

	normalized_invariants(Metric) :-
		Metric::score([a, a, a, a, a, a, b, b], [x, x, x, x, y, y, y, y], Score),
		Metric::score([x, x, x, x, y, y, y, y], [a, a, a, a, a, a, b, b], Swapped),
		Metric::score([b, a, b, a, a, a, a, a], [y, x, y, y, x, y, x, x], Permuted),
		Metric::score([10, 10, 10, 10, 10, 10, 20, 20], [1, 1, 1, 1, 2, 2, 2, 2], Relabeled),
		Metric::score([a, a, a, a, a, a, b, b, a, a, a, a, a, a, b, b], [x, x, x, x, y, y, y, y, x, x, x, x, y, y, y, y], Replicated),
		assertion(float(Score)),
		assertion(Score > 0.0),
		assertion(Score < 1.0),
		assertion(Score =~= Swapped),
		assertion(Score =~= Permuted),
		assertion(Score =~= Relabeled),
		assertion(Score =~= Replicated).

	normalized_degenerate_cases(Metric) :-
		Metric::score([a], [x], Singleton),
		Metric::score([_, _], [x, y], Missing),
		Metric::score([a, b], [x, x], ConstantTarget),
		assertion(Singleton =~= 0.0),
		assertion(Missing =~= 0.0),
		assertion(ConstantTarget =~= 0.0).

	normalized_missing_cases(Metric) :-
		Metric::score([a, MissingValue, b, c], [x, z, y, MissingTarget], Score),
		assertion(Score =~= 1.0),
		assertion(var(MissingValue)),
		assertion(var(MissingTarget)).

	normalized_configurations(Family) :-
		Categorical =.. [Family, categorical],
		Width =.. [Family, equal_width(2)],
		Frequency =.. [Family, equal_frequency(2)],
		OneWidth =.. [Family, equal_width(1)],
		OneFrequency =.. [Family, equal_frequency(1)],
		Family::score([a, a, b, b], [x, x, y, y], DefaultScore),
		Categorical::score([a, a, b, b], [x, x, y, y], CategoricalScore),
		Family::score([1, 1.0], [x, y], NumericLabels),
		Width::score([0, 1000, _, 2], [x, _, y, y], WidthScore),
		Frequency::score([0, 1000, _, 2], [x, _, y, y], FrequencyScore),
		OneWidth::score([0, 1, 2, 3], [x, x, y, y], OneWidthScore),
		OneFrequency::score([0, 1, 2, 3], [x, x, y, y], OneFrequencyScore),
		assertion(DefaultScore =~= CategoricalScore),
		assertion(NumericLabels =~= 1.0),
		assertion(WidthScore =~= 1.0),
		assertion(FrequencyScore =~= 1.0),
		assertion(OneWidthScore =~= 0.0),
		assertion(OneFrequencyScore =~= 0.0).

	normalized_selector(Metric) :-
		sample_selector::learn(feature_selection_categorical_dataset, Selector, [scoring_metric(Metric), selection_strategy(top_k(1))]),
		sample_selector::check_selector(Selector),
		sample_selector::selected_features(Selector, Selected),
		sample_selector::diagnostics(Selector, Diagnostics),
		sample_selector::selector_options(Selector, Options),
		assertion(Selected == [signal]),
		assertion(ground(Diagnostics)),
		assertion(member(scoring_metric(Metric), Options)),
		sample_selector::learn(feature_selection_categorical_dataset, ThresholdSelector, [scoring_metric(Metric), selection_strategy(threshold(0.5))]),
		sample_selector::check_selector(ThresholdSelector),
		sample_selector::selected_features(ThresholdSelector, ThresholdSelected),
		assertion(ThresholdSelected == [signal]).

	normalized_repeated_options(Metric) :-
		sample_selector::learn(feature_selection_categorical_dataset, Selector, [scoring_metric(Metric), scoring_metric(variance_score), selection_strategy(top_k(1))]),
		sample_selector::check_selector(Selector),
		assertion(Selector = sample_selector(Metric, _, [signal], _)).

	first_header_line(File, Line) :-
		open(File, read, Stream),
		read_line_atom(Stream, Line),
		close(Stream).

	read_line_atom(Stream, Line) :-
		get_code(Stream, Code),
		(	Code == -1 ->
			Line = end_of_file
		;	read_line_codes(Code, Stream, Codes),
			atom_codes(Line, Codes)
		).

	read_line_codes(-1, _Stream, []) :-
		!.
	read_line_codes(10, _Stream, []) :-
		!.
	read_line_codes(13, Stream, Codes) :-
		!,
		get_code(Stream, NextCode),
		(	NextCode == 10 ->
			Codes = []
		;	read_line_codes(NextCode, Stream, Codes)
		).
	read_line_codes(Code, Stream, [Code| Codes]) :-
		get_code(Stream, NextCode),
		read_line_codes(NextCode, Stream, Codes).

:- end_object.
