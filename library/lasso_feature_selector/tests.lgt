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
		comment is 'Unit tests for the Lasso feature selector library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		memberchk/2, select/3
	]).

	:- private(unpenalized_options/1).
	:- mode(unpenalized_options(-list(compound)), one).
	:- info(unpenalized_options/1, [
		comment is 'Returns explicit solver settings for independent coefficient references.',
		argnames is ['Options']
	]).

	:- private(mixed_dataset/1).
	:- mode(mixed_dataset(-object_identifier), one).
	:- info(mixed_dataset/1, [
		comment is 'Returns an orthogonal continuous and categorical coefficient reference dataset.',
		argnames is ['Dataset']
	]).

	cover(lasso_feature_selector).
	cover(regression_dataset_adapter(_)).
	cover(regression_examples_adapter(_, _)).

	cleanup :-
		^^clean_file('test_lasso_output.pl').

	test(lasso_feature_selector_search_reference, deterministic) :-
		Options = [regularization_search(holdout(0.25, [0, 0.5, 3])), regressor_options([feature_scaling(false)])],
		lasso_feature_selector::learn(lasso_search_dataset, Selector, Options),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(holdout(4, 2, [trial(0, First, _, _, _), trial(0.5, Second, _, _, _), trial(3, Third, _, _, _)], 0)), Diagnostics),
		assertion(First =~= 0.0),
		assertion(Second =~= 0.25),
		assertion(Third =~= 4.0).

	test(lasso_feature_selector_search_refit, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, Selector, [regularization_search(holdout(0.25, [0, 0.5, 3]))]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::selector_options(Selector, Options),
		memberchk(regressor_options(FitOptions), Options),
		lasso_regression::learn(regression_dataset_adapter(lasso_search_dataset), Direct, FitOptions),
		Selector = lasso_feature_selector(Regressor, _, _, _),
		assertion(lgtunit::variant(Regressor, Direct)),
		lasso_regression::diagnostics(Regressor, Nested),
		memberchk(training_example_count(6), Nested).

	test(lasso_feature_selector_search_single_candidate, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, Selector, [regularization_search(holdout(0.25, [0.5])), regressor_options([regularization(100.0), feature_scaling(false)])]),
		lasso_feature_selector::check_selector(Selector),
		Selector = lasso_feature_selector(Regressor, _, _, _),
		lasso_regression::learn(regression_dataset_adapter(lasso_search_dataset), Direct, [regularization(0.5), feature_scaling(false)]),
		assertion(lgtunit::variant(Regressor, Direct)).

	test(lasso_feature_selector_search_ties, deterministic) :-
		Dataset = lasso_fixture([constant-continuous], [example(1, [constant-1], 2), example(2, [constant-1], 2), example(3, [constant-1], 2)]),
		lasso_feature_selector::learn(Dataset, Selector, [regularization_search(holdout(0.2, [1, 3, 3.0, 2]))]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(holdout(2, 1, _, Winner)), Diagnostics),
		assertion(Winner == 3).

	test(lasso_feature_selector_search_scaling_no_leakage, deterministic) :-
		Dataset = lasso_fixture([signal-continuous], [example(1, [signal- -1], -2), example(2, [signal-1], 2), example(3, [signal-100], 200)]),
		lasso_feature_selector::learn(Dataset, Selector, [regularization_search(holdout(0.2, [1.0]))]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(holdout(2, 1, [trial(1.0, MSE, _, _, _)], 1.0)), Diagnostics),
		assertion(MSE =~= 10000.0),
		Selector = lasso_feature_selector(lasso_regressor([continuous(signal, Mean, _)], _, _, _), _, _, _),
		ExpectedMean is 100 / 3,
		assertion(Mean =~= ExpectedMean).

	test(lasso_feature_selector_search_missing_targets, deterministic) :-
		Dataset = lasso_fixture([signal-continuous], [example(1, [signal- -1], -2), example(2, [signal-1000], Unknown), example(3, [signal-1], 2), example(4, [signal-Missing], 0)]),
		lasso_feature_selector::learn(Dataset, Selector, [regularization_search(holdout(0.2, [0, 1]))]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(usable_example_count(3), Diagnostics),
		memberchk(excluded_example_count(1), Diagnostics),
		memberchk(regularization_search_result(holdout(2, 1, _, _)), Diagnostics),
		assertion(var(Unknown)),
		assertion(var(Missing)).

	test(lasso_feature_selector_search_mixed_missing, subsumes(lasso_feature_selector(lasso_regressor([continuous(signal,_,_), categorical(category,[base,up,down]), continuous(constant,_,_)], _, _, _), _, _, _), Selector)) :-
		mixed_dataset(Dataset),
		lasso_feature_selector::learn(Dataset, Selector, [regularization_search(holdout(0.25, [0.0, 0.5]))]),
		lasso_feature_selector::check_selector(Selector).

	test(lasso_feature_selector_search_repeated_options, deterministic) :-
		Options = [regularization_search(holdout(0.25, [0.5])), regularization_search(none), regressor_options([regularization(100.0), regularization(20.0), feature_scaling(false)]), regressor_options([regularization(10.0)])],
		lasso_feature_selector::learn(lasso_search_dataset, Selector, Options),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::selector_options(Selector, Stored),
		memberchk(regressor_options([regularization(First), regularization(Second)| _]), Stored),
		assertion(First =~= 0.5),
		assertion(Second =~= 20.0),
		memberchk(regressor_options([regularization(10.0)]), Stored).

	test(lasso_feature_selector_search_none_first, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector, [regularization_search(none), regularization_search(holdout(0.25, [100.0]))]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(none), Diagnostics),
		lasso_feature_selector::selected_features(Selector, [signal]).

	test(lasso_feature_selector_search_minimum_split, deterministic) :-
		Dataset = lasso_fixture([], [example(1, [], 1), example(2, [], 3)]),
		lasso_feature_selector::learn(Dataset, Selector, [regularization_search(holdout(0.99, [0]))]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(holdout(1, 1, [trial(0, MSE, _, _, _)], 0)), Diagnostics),
		assertion(MSE =~= 4.0).

	test(lasso_feature_selector_search_exhaustion, deterministic) :-
		Dataset = lasso_fixture([signal-continuous], [example(1, [signal-1], 1), example(2, [signal-2], 2), example(3, [signal-3], 3)]),
		lasso_feature_selector::learn(Dataset, Selector, [regularization_search(holdout(0.2, [0.0])), regressor_options([maximum_iterations(1), tolerance(0.0), feature_scaling(false)])]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(holdout(2, 1, [trial(0.0, _, maximum_iterations_exhausted, 1, _)], 0.0)), Diagnostics).

	test(lasso_feature_selector_search_bad_fraction, error(domain_error(option, regularization_search(holdout(1, [0]))))) :-
		lasso_feature_selector::learn(lasso_search_dataset, _, [regularization_search(holdout(1, [0]))]).

	test(lasso_feature_selector_search_empty_grid, error(domain_error(option, regularization_search(holdout(0.2, []))))) :-
		lasso_feature_selector::learn(lasso_search_dataset, _, [regularization_search(holdout(0.2, []))]).

	test(lasso_feature_selector_search_negative_grid, error(domain_error(option, regularization_search(holdout(0.2, [-1]))))) :-
		lasso_feature_selector::learn(lasso_search_dataset, _, [regularization_search(holdout(0.2, [-1]))]).

	test(lasso_feature_selector_search_improper_grid, error(domain_error(option, regularization_search(holdout(0.2, [0| bad]))))) :-
		lasso_feature_selector::learn(lasso_search_dataset, _, [regularization_search(holdout(0.2, [0| bad]))]).

	test(lasso_feature_selector_search_too_few_rows, error(domain_error(lasso_search_examples, 1))) :-
		Dataset = lasso_fixture([], [example(1, [], 1), example(2, [], _)]),
		lasso_feature_selector::learn(Dataset, _, [regularization_search(holdout(0.2, [0]))]).

	test(lasso_feature_selector_search_invalid_later_option, error(domain_error(option, regressor_options([regularization(1.0), regularization(-1.0)])))) :-
		lasso_feature_selector::learn(lasso_search_dataset, _, [regularization_search(holdout(0.2, [0])), regressor_options([regularization(1.0), regularization(-1.0)])]).

	test(lasso_feature_selector_search_bad_counts, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics), [regularization_search(holdout(0.25, [0.0]))]),
		once(select(regularization_search_result(holdout(_, _, Trials, Winner)), Diagnostics, Rest)),
		Bad = lasso_feature_selector(Regressor, Scores, Selected, [regularization_search_result(holdout(3, 3, Trials, Winner))| Rest]),
		assertion(\+ lasso_feature_selector::valid_selector(Bad)).

	test(lasso_feature_selector_search_bad_trial, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics), [regularization_search(holdout(0.25, [0.0]))]),
		once(select(regularization_search_result(holdout(Train, Validation, [trial(Value, _, Status, Iterations, Delta)], Winner)), Diagnostics, Rest)),
		Bad = lasso_feature_selector(Regressor, Scores, Selected, [regularization_search_result(holdout(Train, Validation, [trial(Value, -1, Status, Iterations, Delta)], Winner))| Rest]),
		assertion(\+ lasso_feature_selector::valid_selector(Bad)).

	test(lasso_feature_selector_search_bad_winner, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics), [regularization_search(holdout(0.25, [0.0, 0.5]))]),
		once(select(regularization_search_result(holdout(Train, Validation, Trials, _)), Diagnostics, Rest)),
		Bad = lasso_feature_selector(Regressor, Scores, Selected, [regularization_search_result(holdout(Train, Validation, Trials, 0.5))| Rest]),
		assertion(\+ lasso_feature_selector::valid_selector(Bad)).

	test(lasso_feature_selector_search_export, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, Selector, [regularization_search(holdout(0.25, [0, 0.5]))]),
		lasso_feature_selector::export_to_clauses(lasso_search_dataset, Selector, searched, [searched(Loaded)]),
		assertion(lgtunit::variant(Selector, Loaded)),
		lasso_feature_selector::check_selector(Loaded).

	test(lasso_feature_selector_sparse_reference, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector, [regressor_options([feature_scaling(false), regularization(0.5)])]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::feature_scores(Selector, [signal-Signal, noise-Noise, constant-Constant]),
		assertion(Signal =~= 1.5),
		assertion(Noise =~= 0.0),
		assertion(Constant =~= 0.0),
		lasso_feature_selector::selected_features(Selector, [signal]).

	test(lasso_feature_selector_strict_cutoff, deterministic(Selected == [])) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector, [regressor_options([feature_scaling(false), regularization(0.5)]), coefficient_threshold(1.5)]),
		lasso_feature_selector::selected_features(Selector, Selected).

	test(lasso_feature_selector_top_k_active_only, deterministic(Selected == [signal])) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector, [selection_strategy(top_k(10))]),
		lasso_feature_selector::selected_features(Selector, Selected).

	test(lasso_feature_selector_threshold_active_only, deterministic(Selected == [signal])) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector, [selection_strategy(threshold(-1))]),
		lasso_feature_selector::selected_features(Selector, Selected).

	test(lasso_feature_selector_adapter_agreement, deterministic) :-
		Options = [regularization(0.5), feature_scaling(false)],
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, _, _, _), [regressor_options(Options)]),
		lasso_regression::learn(regression_dataset_adapter(lasso_sparse_dataset), Direct, Options),
		assertion(lgtunit::variant(Regressor, Direct)).

	test(lasso_feature_selector_partial_unchanged, deterministic(var(Scores))) :-
		Selector = lasso_feature_selector(_, Scores, [], []),
		assertion(\+ lasso_feature_selector::valid_selector(Selector)),
		assertion(var(Scores)).

	test(lasso_feature_selector_invalid_weights, error(domain_error(selector, Bad))) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(lasso_regressor(Encoders, Bias, _Weights, Nested), Scores, Selected, Diagnostics)),
		Bad = lasso_feature_selector(lasso_regressor(Encoders, Bias, [], Nested), Scores, Selected, Diagnostics),
		lasso_feature_selector::check_selector(Bad).

	test(lasso_feature_selector_high_regularization, deterministic(Selected == [])) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector, [regressor_options([regularization(100.0)])]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::selected_features(Selector, Selected).

	test(lasso_feature_selector_defaults, deterministic) :-
		lasso_feature_selector::default_options([regressor_options([]), coefficient_threshold(Cutoff), selection_strategy(all), regularization_search(none)]),
		assertion(Cutoff =~= 0.0),
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::selector_options(Selector, Stored),
		memberchk(regressor_options(Nested), Stored),
		lasso_regression::default_options(Defaults),
		assertion(Nested == Defaults).

	test(lasso_feature_selector_constant_all_zero, deterministic) :-
		Dataset = lasso_fixture([z-continuous, a-continuous], [example(1, [z-1, a-2], 5), example(2, [z-1, a-2], 5)]),
		lasso_feature_selector::learn(Dataset, Selector),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::selected_features(Selector, []),
		lasso_feature_selector::feature_scores(Selector, [z-Zero1, a-Zero2]),
		assertion(Zero1 =~= 0.0),
		assertion(Zero2 =~= 0.0).

	test(lasso_feature_selector_constant_top_k, deterministic(Selected == [])) :-
		Dataset = lasso_fixture([constant-continuous], [example(1, [constant-1], 5), example(2, [constant-1], 5)]),
		lasso_feature_selector::learn(Dataset, Selector, [selection_strategy(top_k(1))]),
		lasso_feature_selector::selected_features(Selector, Selected).

	test(lasso_feature_selector_constant_threshold, deterministic(Selected == [])) :-
		Dataset = lasso_fixture([constant-continuous], [example(1, [constant-1], 5), example(2, [constant-1], 5)]),
		lasso_feature_selector::learn(Dataset, Selector, [selection_strategy(threshold(0))]),
		lasso_feature_selector::selected_features(Selector, Selected).

	test(lasso_feature_selector_missing_indicator_selected, deterministic) :-
		Dataset = lasso_fixture([signal-continuous], [example(1, [signal-0], 0), example(2, [signal-0], 0), example(3, [], 4), example(4, [signal-_], 4)]),
		unpenalized_options(Options),
		lasso_feature_selector::learn(Dataset, Selector, Options),
		Selector = lasso_feature_selector(lasso_regressor([continuous(signal, _, _)], _, [ValueWeight, MissingWeight], _), [signal-Score], [signal], _),
		assertion(ValueWeight =~= 0.0),
		assertion(MissingWeight =~= 4.0),
		assertion(Score =~= MissingWeight),
		lasso_feature_selector::check_selector(Selector).

	test(lasso_feature_selector_categorical_baseline_blocks, deterministic) :-
		Dataset = lasso_fixture([category-[base, up, down]], [example(1, [category-base], 0), example(2, [category-up], 2), example(3, [category-down], -4), example(4, [], 6)]),
		unpenalized_options(Options),
		lasso_feature_selector::learn(Dataset, Selector, Options),
		Selector = lasso_feature_selector(lasso_regressor([categorical(category, [base, up, down])], Bias, [Up, Down, Missing], _), [category-Score], [category], _),
		BaselinePrediction is Bias + 1,
		assertion(BaselinePrediction =~= 1.0),
		assertion(Up =~= 2.0),
		assertion(Down =~= -4.0),
		assertion(Missing =~= 6.0),
		assertion(Score =~= 6.0),
		lasso_feature_selector::check_selector(Selector).

	test(lasso_feature_selector_categorical_singleton_missing, deterministic) :-
		Dataset = lasso_fixture([category-[base]], [example(1, [category-base], 0), example(2, [], 4)]),
		unpenalized_options(Options),
		lasso_feature_selector::learn(Dataset, Selector, Options),
		Selector = lasso_feature_selector(lasso_regressor([categorical(category, [base])], _, [Weight], _), [category-Score], [category], _),
		assertion(Weight =~= 4.0),
		assertion(Score =~= Weight).

	test(lasso_feature_selector_mixed_positions, deterministic) :-
		mixed_dataset(Dataset),
		unpenalized_options(Options),
		lasso_feature_selector::learn(Dataset, Selector, Options),
		Selector = lasso_feature_selector(lasso_regressor([continuous(signal, _, _), categorical(category, [base, up, down]), continuous(constant, _, _)], Bias, [Signal, SignalMissing, Up, Down, CategoryMissing, Constant, ConstantMissing], _), [category-CategoryScore, signal-SignalScore, constant-ConstantScore], [category, signal], _),
		BaselinePrediction is Bias + 1,
		assertion(BaselinePrediction =~= 1.0),
		assertion(Signal =~= 3.0),
		assertion(SignalMissing =~= 0.0),
		assertion(Up =~= 2.0),
		assertion(Down =~= -4.0),
		assertion(CategoryMissing =~= 6.0),
		assertion(Constant =~= 0.0),
		assertion(ConstantMissing =~= 0.0),
		assertion(CategoryScore =~= 6.0),
		assertion(SignalScore =~= 3.0),
		assertion(ConstantScore =~= 0.0),
		lasso_feature_selector::check_selector(Selector).

	test(lasso_feature_selector_mixed_adapter_agreement, deterministic) :-
		mixed_dataset(Dataset),
		unpenalized_options([regressor_options(Options)]),
		lasso_feature_selector::learn(Dataset, lasso_feature_selector(Regressor, _, _, _), [regressor_options(Options)]),
		lasso_regression::learn(regression_dataset_adapter(Dataset), Direct, Options),
		assertion(lgtunit::variant(Regressor, Direct)).

	test(lasso_feature_selector_nested_first_occurrence, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector, [regressor_options([regularization(0.5), regularization(100.0), feature_scaling(false), feature_scaling(true)]), regressor_options([regularization(100.0)]), selection_strategy(top_k(1)), selection_strategy(all), coefficient_threshold(0.0), coefficient_threshold(100)]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::feature_scores(Selector, [signal-Signal| _]),
		assertion(Signal =~= 1.5),
		lasso_feature_selector::selected_features(Selector, [signal]),
		lasso_feature_selector::selector_options(Selector, [regressor_options(Nested), regressor_options([regularization(100.0)])| _]),
		Nested = [regularization(First), regularization(Second), feature_scaling(false), feature_scaling(true)| _],
		assertion(First =~= 0.5),
		assertion(Second =~= 100.0).

	test(lasso_feature_selector_maximum_iterations_diagnostics, deterministic(Nested == Direct)) :-
		Dataset = lasso_fixture([signal-continuous], [example(1, [signal-1], 1), example(2, [signal-2], 2), example(3, [signal-3], 3)]),
		lasso_feature_selector::learn(Dataset, Selector, [regressor_options([maximum_iterations(1), tolerance(0.0), regularization(0.0), feature_scaling(false)])]),
		lasso_feature_selector::check_selector(Selector),
		once(lasso_feature_selector::diagnostic(Selector, regressor_diagnostics(Nested))),
		memberchk(convergence(maximum_iterations_exhausted), Nested),
		memberchk(iterations(1), Nested),
		memberchk(encoded_feature_count(2), Nested),
		Selector = lasso_feature_selector(Regressor, _, _, _),
		lasso_regression::diagnostics(Regressor, Direct).

	test(lasso_feature_selector_missing_targets_counts, deterministic(Mean =~= 0.0)) :-
		Dataset = lasso_fixture([signal-continuous], [example(1, [signal- -1], -2), example(2, [signal-1], 2), example(3, [signal-1000], _)]),
		lasso_feature_selector::learn(Dataset, Selector),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(example_count(3), Diagnostics),
		memberchk(usable_example_count(2), Diagnostics),
		memberchk(excluded_example_count(1), Diagnostics),
		memberchk(regressor_diagnostics(Nested), Diagnostics),
		memberchk(training_example_count(2), Nested),
		Selector = lasso_feature_selector(lasso_regressor([continuous(signal, Mean, _)], _, _, _), _, _, _).

	test(lasso_feature_selector_adapter_preserves_missing, deterministic) :-
		Dataset = lasso_fixture([signal-continuous], [example(1, [signal-Missing], 2), example(2, [], 3), example(3, [signal-8], Unknown)]),
		findall(example(Id, Target, Pairs), regression_dataset_adapter(Dataset)::example(Id, Target, Pairs), Examples),
		assertion(lgtunit::variant(Examples, [example(1, 2, [signal-_]), example(2, 3, [])])),
		assertion(var(Missing)),
		assertion(var(Unknown)),
		regression_dataset_adapter(Dataset)::target(target),
		findall(Feature-Values, regression_dataset_adapter(Dataset)::attribute_values(Feature, Values), [signal-continuous]).

	test(lasso_feature_selector_bad_target, error(type_error(number, label))) :-
		lasso_feature_selector::learn(lasso_fixture([signal-continuous], [example(1, [signal-1], label)]), _).

	test(lasso_feature_selector_all_unknown_targets, error(domain_error(non_empty_examples, Dataset))) :-
		Dataset = lasso_fixture([signal-continuous], [example(1, [], _)]),
		lasso_feature_selector::learn(Dataset, _).

	test(lasso_feature_selector_no_examples, error(domain_error(non_empty_examples, Dataset))) :-
		Dataset = lasso_fixture([signal-continuous], []),
		lasso_feature_selector::learn(Dataset, _).

	test(lasso_feature_selector_excluded_row_validated, error(domain_error(unknown_feature, unknown))) :-
		lasso_feature_selector::learn(lasso_fixture([signal-continuous], [example(1, [signal-1], 1), example(2, [unknown-2], _)]), _).

	test(lasso_feature_selector_duplicate_feature, error(domain_error(duplicate_feature, signal))) :-
		lasso_feature_selector::learn(lasso_fixture([signal-continuous, signal-continuous], [example(1, [signal-1], 1)]), _).

	test(lasso_feature_selector_repeated_binding, error(domain_error(duplicate_feature, signal))) :-
		lasso_feature_selector::learn(lasso_fixture([signal-continuous], [example(1, [signal-1, signal-2], 1)]), _).

	test(lasso_feature_selector_empty_domain, error(domain_error(feature_type, category-[]))) :-
		lasso_feature_selector::learn(lasso_fixture([category-[]], [example(1, [], 1)]), _).

	test(lasso_feature_selector_improper_domain, error(domain_error(feature_type, category-[base| bad]))) :-
		lasso_feature_selector::learn(lasso_fixture([category-[base| bad]], [example(1, [], 1)]), _).

	test(lasso_feature_selector_duplicate_domain, error(domain_error(feature_type, category-[base, base]))) :-
		lasso_feature_selector::learn(lasso_fixture([category-[base, base]], [example(1, [], 1)]), _).

	test(lasso_feature_selector_variable_domain, error(domain_error(feature_type, category-_))) :-
		lasso_feature_selector::learn(lasso_fixture([category-_], [example(1, [], 1)]), _).

	test(lasso_feature_selector_nonatom_name, error(domain_error(feature_type, 1-continuous))) :-
		lasso_feature_selector::learn(lasso_fixture([1-continuous], [example(1, [], 1)]), _).

	test(lasso_feature_selector_adapter_duplicate_declarations, error(domain_error(duplicate_feature, signal))) :-
		regression_dataset_adapter(lasso_fixture([signal-continuous, signal-continuous], []))::attribute_values(_, _).

	test(lasso_feature_selector_nested_invalid_option, error(domain_error(option, regressor_options([regularization(-1.0)])))) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, _, [regressor_options([regularization(-1.0)])]).

	test(lasso_feature_selector_negative_cutoff, error(domain_error(option, coefficient_threshold(-1)))) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, _, [coefficient_threshold(-1)]).

	test(lasso_feature_selector_invalid_strategy, error(domain_error(option, selection_strategy(top_k(0))))) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, _, [selection_strategy(top_k(0))]).

	test(lasso_feature_selector_stable_numeric_ties, deterministic) :-
		Dataset = lasso_fixture([z-continuous, a-continuous], [example(1, [z- -1, a- -1], -4), example(2, [z- -1, a-1], 0), example(3, [z-1, a- -1], 0), example(4, [z-1, a-1], 4)]),
		unpenalized_options(Options),
		lasso_feature_selector::learn(Dataset, Selector, [selection_strategy(top_k(1))| Options]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::feature_scores(Selector, [z-First, a-Second]),
		assertion(First =~= 2.0),
		assertion(Second =~= First),
		lasso_feature_selector::selected_features(Selector, [z]).

	test(lasso_feature_selector_threshold_active_boundary, deterministic(Selected == [signal])) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector, [regressor_options([feature_scaling(false), regularization(0.5)]), selection_strategy(threshold(1.5))]),
		lasso_feature_selector::selected_features(Selector, Selected).

	test(lasso_feature_selector_high_selection_threshold, deterministic(Selected == [])) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector, [selection_strategy(threshold(100))]),
		lasso_feature_selector::selected_features(Selector, Selected).

	test(lasso_feature_selector_invalid_encoder, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(lasso_regressor([_| Encoders], Bias, Weights, Nested), Scores, Selected, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(lasso_regressor([broken(signal)| Encoders], Bias, Weights, Nested), Scores, Selected, Diagnostics))).

	test(lasso_feature_selector_duplicate_encoder_names, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(lasso_regressor([First, _Second, Third], Bias, Weights, Nested), Scores, Selected, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(lasso_regressor([First, First, Third], Bias, Weights, Nested), Scores, Selected, Diagnostics))).

	test(lasso_feature_selector_extra_weight, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(lasso_regressor(Encoders, Bias, Weights, Nested), Scores, Selected, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(lasso_regressor(Encoders, Bias, [0.0| Weights], Nested), Scores, Selected, Diagnostics))).

	test(lasso_feature_selector_nonnumeric_weight, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(lasso_regressor(Encoders, Bias, [_| Weights], Nested), Scores, Selected, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(lasso_regressor(Encoders, Bias, [bad| Weights], Nested), Scores, Selected, Diagnostics))).

	test(lasso_feature_selector_bad_score, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, [_| Scores], Selected, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, [signal-bad| Scores], Selected, Diagnostics))).

	test(lasso_feature_selector_unsorted_scores, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, [First, Second, Third], Selected, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, [Third, Second, First], Selected, Diagnostics))).

	test(lasso_feature_selector_wrong_selection, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, Scores, _, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, Scores, [signal, noise], Diagnostics))).

	test(lasso_feature_selector_wrong_model_name, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, Scores, Selected, [model(_)| Diagnostics])),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, Scores, Selected, [model(other)| Diagnostics]))).

	test(lasso_feature_selector_bad_counts, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics)),
		once(select(excluded_example_count(0), Diagnostics, Rest)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, Scores, Selected, [excluded_example_count(1)| Rest]))).

	test(lasso_feature_selector_bad_nested_count, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(lasso_regressor(Encoders, Bias, Weights, Nested), Scores, Selected, Diagnostics)),
		once(select(encoded_feature_count(6), Nested, Rest)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(lasso_regressor(Encoders, Bias, Weights, [encoded_feature_count(5)| Rest]), Scores, Selected, Diagnostics))).

	test(lasso_feature_selector_bad_nested_training_count, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(lasso_regressor(Encoders, Bias, Weights, Nested), Scores, Selected, Diagnostics)),
		once(select(training_example_count(4), Nested, Rest)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(lasso_regressor(Encoders, Bias, Weights, [training_example_count(bad)| Rest]), Scores, Selected, Diagnostics))).

	test(lasso_feature_selector_wrong_regressor_shape, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(_, Scores, Selected, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(other_regressor([], 0.0, [], []), Scores, Selected, Diagnostics))).

	test(lasso_feature_selector_partial_domain_error_unchanged, deterministic(var(Encoders))) :-
		Selector = lasso_feature_selector(lasso_regressor(Encoders, 0.0, [], []), [], [], []),
		assertion(\+ lasso_feature_selector::valid_selector(Selector)).

	test(lasso_feature_selector_variable_model, error(instantiation_error)) :-
		lasso_feature_selector::check_selector(_).

	test(lasso_feature_selector_export_clauses, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector),
		lasso_feature_selector::export_to_clauses(lasso_sparse_dataset, Selector, lasso_model, [lasso_model(Loaded)]),
		assertion(lgtunit::variant(Selector, Loaded)),
		lasso_feature_selector::check_selector(Loaded).

	test(lasso_feature_selector_export_file, variant(Loaded, Selector)) :-
		^^file_path('test_lasso_output.pl', File),
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector, [regularization_search(holdout(0.25, linear(0, 1, 3)))]),
		lasso_feature_selector::export_to_file(lasso_sparse_dataset, Selector, lasso_exported_model, File),
		logtalk_load(File),
		{lasso_exported_model(Loaded)},
		lasso_feature_selector::check_selector(Loaded).

	test(lasso_feature_selector_print, true) :-
		^^suppress_text_output,
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector, [regularization_search(holdout(0.25, linear(0, 1, 3)))]),
		lasso_feature_selector::print_selector(Selector).

	test(lasso_feature_selector_multiple_categorical_blocks, deterministic) :-
		Dataset = lasso_fixture([first-[a, b], second-[base, up, down]], [example(1, [first-a, second-base], 0), example(2, [first-a, second-up], 2), example(3, [first-a, second-down], -4), example(4, [first-b, second-base], 10), example(5, [first-b, second-up], 12), example(6, [first-b, second-down], 6)]),
		unpenalized_options(Options),
		lasso_feature_selector::learn(Dataset, Selector, Options),
		Selector = lasso_feature_selector(lasso_regressor([categorical(first, [a, b]), categorical(second, [base, up, down])], _, [FirstWeight, FirstMissing, UpWeight, DownWeight, SecondMissing], _), [first-FirstScore, second-SecondScore], [first, second], _),
		assertion(FirstWeight =~= 10.0),
		assertion(FirstMissing =~= 0.0),
		assertion(UpWeight =~= 2.0),
		assertion(DownWeight =~= -4.0),
		assertion(SecondMissing =~= 0.0),
		assertion(FirstScore =~= 10.0),
		assertion(SecondScore =~= 4.0),
		lasso_feature_selector::check_selector(Selector).

	test(lasso_feature_selector_missing_input_not_bound, deterministic) :-
		Dataset = lasso_fixture([signal-continuous], [example(1, [signal-Missing], 1), example(2, [signal-1], 2), example(3, [], Unknown)]),
		lasso_feature_selector::learn(Dataset, Selector),
		lasso_feature_selector::check_selector(Selector),
		assertion(var(Missing)),
		assertion(var(Unknown)).

	test(lasso_feature_selector_score_recomputed, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, [_| Scores], Selected, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, [signal-100.0| Scores], Selected, Diagnostics))).

	test(lasso_feature_selector_encoder_name_recomputed, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(lasso_regressor([continuous(_, Mean, Scale)| Encoders], Bias, Weights, Nested), Scores, Selected, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(lasso_regressor([continuous(renamed, Mean, Scale)| Encoders], Bias, Weights, Nested), Scores, Selected, Diagnostics))).

	test(lasso_feature_selector_categorical_length_mismatch, deterministic) :-
		mixed_dataset(Dataset),
		lasso_feature_selector::learn(Dataset, lasso_feature_selector(lasso_regressor([First, categorical(Name, _), Third], Bias, Weights, Nested), Scores, Selected, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(lasso_regressor([First, categorical(Name, [base, up]), Third], Bias, Weights, Nested), Scores, Selected, Diagnostics))).

	test(lasso_feature_selector_improper_encoders, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(lasso_regressor(_, Bias, Weights, Nested), Scores, Selected, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(lasso_regressor([continuous(signal, 0.0, 1.0)| bad], Bias, Weights, Nested), Scores, Selected, Diagnostics))).

	test(lasso_feature_selector_improper_weights, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(lasso_regressor(Encoders, Bias, _, Nested), Scores, Selected, Diagnostics)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(lasso_regressor(Encoders, Bias, [1.0| bad], Nested), Scores, Selected, Diagnostics))).

	test(lasso_feature_selector_bad_encoded_count, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics)),
		once(select(encoded_feature_count(6), Diagnostics, Rest)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, Scores, Selected, [encoded_feature_count(5)| Rest]))).

	test(lasso_feature_selector_bad_candidate_count, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics)),
		once(select(candidate_count(3), Diagnostics, Rest)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, Scores, Selected, [candidate_count(2)| Rest]))).

	test(lasso_feature_selector_bad_selected_count, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics)),
		once(select(selected_count(1), Diagnostics, Rest)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, Scores, Selected, [selected_count(2)| Rest]))).

	test(lasso_feature_selector_bad_usable_count, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics)),
		once(select(usable_example_count(4), Diagnostics, Rest)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, Scores, Selected, [usable_example_count(3)| Rest]))).

	test(lasso_feature_selector_bad_original_count, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics)),
		once(select(example_count(4), Diagnostics, Rest)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, Scores, Selected, [example_count(3)| Rest]))).

	test(lasso_feature_selector_bad_aggregation, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics)),
		once(select(aggregation(max_abs), Diagnostics, Rest)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, Scores, Selected, [aggregation(sum)| Rest]))).

	test(lasso_feature_selector_bad_maximum, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics)),
		once(select(maximum_absolute_coefficient(_), Diagnostics, Rest)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, Scores, Selected, [maximum_absolute_coefficient(100.0)| Rest]))).

	test(lasso_feature_selector_bad_nested_options, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(lasso_regressor(Encoders, Bias, Weights, Nested), Scores, Selected, Diagnostics)),
		once(select(options(_), Nested, Rest)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(lasso_regressor(Encoders, Bias, Weights, [options([regularization(-1.0)])| Rest]), Scores, Selected, Diagnostics))).

	test(lasso_feature_selector_bad_stored_options, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics)),
		once(select(options(_), Diagnostics, Rest)),
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, Scores, Selected, [options([coefficient_threshold(-1)])| Rest]))).

	test(lasso_feature_selector_empty_vocabulary, deterministic) :-
		Dataset = lasso_fixture([], [example(1, [], 1), example(2, [], 2)]),
		lasso_feature_selector::learn(Dataset, Selector),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::feature_scores(Selector, []),
		lasso_feature_selector::selected_features(Selector, []),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(candidate_count(0), Diagnostics),
		memberchk(encoded_feature_count(0), Diagnostics).

	test(lasso_feature_selector_linear_grid_reference, deterministic) :-
		lasso_feature_selector::learn(lasso_sparse_dataset, Selector, [regularization_search(holdout(0.25, linear(0, 1, 3)))]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(holdout(3, 1, [trial(First, _, _, _, _), trial(Middle, _, _, _, _), trial(Last, _, _, _, _)], _)), Diagnostics),
		assertion(First =~= 0.0),
		assertion(Middle =~= 0.5),
		assertion(Last =~= 1.0).

	test(lasso_feature_selector_linear_grid_equivalence, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, Generated, [regularization_search(holdout(0.25, linear(0, 1, 3)))]),
		lasso_feature_selector::learn(lasso_search_dataset, Explicit, [regularization_search(holdout(0.25, [0.0, 0.5, 1.0]))]),
		Generated = lasso_feature_selector(Regressor, Scores, Selected, GeneratedDiagnostics),
		Explicit = lasso_feature_selector(OtherRegressor, OtherScores, OtherSelected, ExplicitDiagnostics),
		assertion(lgtunit::variant(Regressor, OtherRegressor)),
		assertion(Scores == OtherScores),
		assertion(Selected == OtherSelected),
		memberchk(regularization_search_result(Result), GeneratedDiagnostics),
		memberchk(regularization_search_result(Result), ExplicitDiagnostics),
		lasso_feature_selector::selector_options(Generated, Options),
		memberchk(regularization_search(holdout(0.25, linear(0, 1, 3))), Options).

	test(lasso_feature_selector_linear_grid_mse, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, Selector, [regularization_search(holdout(0.25, linear(0, 1, 3))), regressor_options([feature_scaling(false)])]),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(holdout(4, 2, [trial(_, First, _, _, _), trial(_, Second, _, _, _), trial(_, Third, _, _, _)], Winner)), Diagnostics),
		assertion(First =~= 0.0),
		assertion(Second =~= 0.25),
		assertion(Third =~= 1.0),
		assertion(Winner =~= 0.0).

	test(lasso_feature_selector_linear_grid_two_endpoints, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, Selector, [regularization_search(holdout(0.25, linear(0.25, 0.75, 2)))]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(holdout(4, 2, [trial(Lower, _, _, _, _), trial(Upper, _, _, _, _)], _)), Diagnostics),
		assertion(Lower =~= 0.25),
		assertion(Upper =~= 0.75).

	test(lasso_feature_selector_linear_grid_refit, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, Selector, [regularization_search(holdout(0.25, linear(0, 1, 3)))]),
		lasso_feature_selector::selector_options(Selector, Options),
		memberchk(regressor_options(FitOptions), Options),
		lasso_regression::learn(regression_dataset_adapter(lasso_search_dataset), Direct, FitOptions),
		Selector = lasso_feature_selector(Regressor, _, _, _),
		assertion(lgtunit::variant(Regressor, Direct)).

	test(lasso_feature_selector_linear_grid_ties, deterministic(Winner =~= 1.0)) :-
		Dataset = lasso_fixture([], [example(1, [], 2), example(2, [], 2), example(3, [], 2)]),
		lasso_feature_selector::learn(Dataset, Selector, [regularization_search(holdout(0.25, linear(0, 1, 3)))]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(holdout(2, 1, _, Winner)), Diagnostics).

	test(lasso_feature_selector_linear_grid_rounded_duplicates, deterministic) :-
		Dataset = lasso_fixture([], [example(1, [], 2), example(2, [], 2), example(3, [], 2)]),
		lasso_feature_selector::learn(Dataset, Selector, [regularization_search(holdout(0.25, linear(1.0, 1.0000000000000002, 3)))]),
		lasso_feature_selector::check_selector(Selector).

	test(lasso_feature_selector_linear_grid_repeated_options, deterministic) :-
		Options = [regularization_search(holdout(0.25, linear(0.25, 0.75, 2))), regularization_search(none), regressor_options([regularization(100.0), regularization(20.0), feature_scaling(false)]), regressor_options([regularization(10.0)])],
		lasso_feature_selector::learn(lasso_search_dataset, Selector, Options),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::selector_options(Selector, Stored),
		memberchk(regularization_search(holdout(0.25, linear(0.25, 0.75, 2))), Stored),
		memberchk(regularization_search(none), Stored),
		memberchk(regressor_options([regularization(First), regularization(Second)| _]), Stored),
		assertion(First =~= 0.25),
		assertion(Second =~= 20.0),
		memberchk(regressor_options([regularization(10.0)]), Stored).

	test(lasso_feature_selector_linear_grid_none_first, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, Selector, [regularization_search(none), regularization_search(holdout(0.25, linear(0, 1, 3)))]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(none), Diagnostics).

	test(lasso_feature_selector_linear_grid_missing, deterministic) :-
		Dataset = lasso_fixture([signal-continuous], [example(1, [signal- -1], -2), example(2, [signal-1000], Unknown), example(3, [signal-1], 2), example(4, [signal-Missing], 0)]),
		lasso_feature_selector::learn(Dataset, Selector, [regularization_search(holdout(0.2, linear(0, 1, 3)))]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(holdout(2, 1, _, _)), Diagnostics),
		memberchk(usable_example_count(3), Diagnostics),
		assertion(var(Unknown)),
		assertion(var(Missing)).

	test(lasso_feature_selector_linear_grid_mixed, deterministic) :-
		mixed_dataset(Dataset),
		lasso_feature_selector::learn(Dataset, Selector, [regularization_search(holdout(0.25, linear(0, 1, 3)))]),
		lasso_feature_selector::check_selector(Selector).

	test(lasso_feature_selector_linear_grid_exhaustion, deterministic) :-
		Dataset = lasso_fixture([signal-continuous], [example(1, [signal-1], 1), example(2, [signal-2], 2), example(3, [signal-3], 3)]),
		lasso_feature_selector::learn(Dataset, Selector, [regularization_search(holdout(0.2, linear(0, 1, 2))), regressor_options([maximum_iterations(1), tolerance(0.0), feature_scaling(false)])]),
		lasso_feature_selector::check_selector(Selector),
		lasso_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(regularization_search_result(holdout(2, 1, [trial(0.0, _, maximum_iterations_exhausted, 1, _)| _], _)), Diagnostics).

	test(lasso_feature_selector_linear_grid_invalid_count, error(domain_error(option, regularization_search(holdout(0.25, linear(0, 1, 1)))))) :-
		lasso_feature_selector::learn(lasso_search_dataset, _, [regularization_search(holdout(0.25, linear(0, 1, 1)))]).

	test(lasso_feature_selector_linear_grid_noninteger_count, error(domain_error(option, regularization_search(holdout(0.25, linear(0, 1, 2.0)))))) :-
		lasso_feature_selector::learn(lasso_search_dataset, _, [regularization_search(holdout(0.25, linear(0, 1, 2.0)))]).

	test(lasso_feature_selector_linear_grid_negative_bound, error(domain_error(option, regularization_search(holdout(0.25, linear(-1, 1, 3)))))) :-
		lasso_feature_selector::learn(lasso_search_dataset, _, [regularization_search(holdout(0.25, linear(-1, 1, 3)))]).

	test(lasso_feature_selector_linear_grid_equal_bounds, error(domain_error(option, regularization_search(holdout(0.25, linear(1, 1, 3)))))) :-
		lasso_feature_selector::learn(lasso_search_dataset, _, [regularization_search(holdout(0.25, linear(1, 1, 3)))]).

	test(lasso_feature_selector_linear_grid_reversed_bounds, error(domain_error(option, regularization_search(holdout(0.25, linear(2, 1, 3)))))) :-
		lasso_feature_selector::learn(lasso_search_dataset, _, [regularization_search(holdout(0.25, linear(2, 1, 3)))]).

	test(lasso_feature_selector_linear_grid_nonnumeric_bound, error(domain_error(option, regularization_search(holdout(0.25, linear(0, bad, 3)))))) :-
		lasso_feature_selector::learn(lasso_search_dataset, _, [regularization_search(holdout(0.25, linear(0, bad, 3)))]).

	test(lasso_feature_selector_linear_grid_invalid_later, error(domain_error(option, regularization_search(holdout(0.25, linear(0, 1, 1)))))) :-
		lasso_feature_selector::learn(lasso_search_dataset, _, [regularization_search(none), regularization_search(holdout(0.25, linear(0, 1, 1)))]).

	test(lasso_feature_selector_linear_grid_bad_alignment, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, lasso_feature_selector(Regressor, Scores, Selected, Diagnostics), [regularization_search(holdout(0.25, linear(0, 1, 3)))]),
		once(select(options(Options), Diagnostics, Rest)),
		once(select(regularization_search(_), Options, OtherOptions)),
		BadOptions = [regularization_search(holdout(0.25, linear(0, 1, 4)))| OtherOptions],
		assertion(\+ lasso_feature_selector::valid_selector(lasso_feature_selector(Regressor, Scores, Selected, [options(BadOptions)| Rest]))).

	test(lasso_feature_selector_linear_grid_export, deterministic) :-
		lasso_feature_selector::learn(lasso_search_dataset, Selector, [regularization_search(holdout(0.25, linear(0, 1, 3)))]),
		lasso_feature_selector::export_to_clauses(lasso_search_dataset, Selector, linear_model, [linear_model(Loaded)]),
		assertion(lgtunit::variant(Loaded, Selector)),
		lasso_feature_selector::check_selector(Loaded).

	% auxiliary predicates

	unpenalized_options([regressor_options([regularization(0.0), feature_scaling(false), tolerance(1.0e-10)])]).

	mixed_dataset(lasso_fixture([signal-continuous, category-[base, up, down], constant-continuous], [
		example(1, [signal- -1, category-base, constant-0], -3),
		example(2, [signal-1, category-base, constant-0], 3),
		example(3, [signal- -1, category-up, constant-0], -1),
		example(4, [signal-1, category-up, constant-0], 5),
		example(5, [signal- -1, category-down, constant-0], -7),
		example(6, [signal-1, category-down, constant-0], -1),
		example(7, [signal- -1, constant-0], 3),
		example(8, [signal-1, category-_, constant-0], 9)
	])).

:- end_object.
