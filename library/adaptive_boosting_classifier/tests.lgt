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
		version is 3:1:0,
		author is 'Paulo Moura',
		date is 2026-10-22,
		comment is 'Unit tests for the "adaptive_boosting_classifier" library.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- private(classifier_cache/3).
	:- dynamic(classifier_cache/3).

	cover(adaptive_boosting_classifier).

	setup :-
		retractall(classifier_cache(_, _, _)).

	cleanup :-
		retractall(classifier_cache(_, _, _)),
		^^clean_file('test_output.pl').

	% learn/2 tests - verify classifier structure

	test(adaptive_boosting_classifier_learn_2_play_tennis, true(ground(Classifier))) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier).

	test(adaptive_boosting_classifier_valid_classifier_1, deterministic(adaptive_boosting_classifier::valid_classifier(Classifier))) :-
		cached_classifier(play_tennis, Classifier).

	test(adaptive_boosting_classifier_invalid_classifier_1, fail) :-
		adaptive_boosting_classifier::valid_classifier(ab_classifier([weighted_tree(0.5, leaf(yes), [outlook, outlook])], [yes, no], [number_of_estimators(1)])).

	test(adaptive_boosting_classifier_learn_2_classifier_structure, true(functor(Classifier, ab_classifier, 3))) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier).

	test(adaptive_boosting_classifier_learn_2_class_values, true(ClassValues == [yes, no])) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		Classifier = ab_classifier(_, ClassValues, _).

	test(adaptive_boosting_classifier_learn_2_has_weighted_trees, true(NumTrees > 0)) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		Classifier = ab_classifier(WeightedTrees, _, _),
		length(WeightedTrees, NumTrees).

	% learn/3 tests - with options

	test(adaptive_boosting_classifier_learn_3_custom_num_estimators, deterministic(ground(Classifier))) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier, [number_of_estimators(5)]).

	test(adaptive_boosting_classifier_learn_3_estimator_count, deterministic(NumTrees =< 5)) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier, [number_of_estimators(5)]),
		Classifier = ab_classifier(WeightedTrees, _, _),
		length(WeightedTrees, NumTrees).

	% Ensemble behavior tests

	test(adaptive_boosting_classifier_trees_have_positive_alpha, all(Alpha > 0.0)) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier, [number_of_estimators(3)]),
		Classifier = ab_classifier(WeightedTrees, _, _),
		member(weighted_tree(Alpha, _, _), WeightedTrees).

	test(adaptive_boosting_classifier_weight_traversal, deterministic(Weights == [0.5, 1.0, 2.0])) :-
		Trees = [weighted_tree(0.5, leaf(yes), []), weighted_tree(1.0, leaf(no), []), weighted_tree(2.0, leaf(yes), [])],
		findall(Alpha, member(weighted_tree(Alpha, _, _), Trees), Weights).

	% predict/3 tests - ensemble predictions

	test(adaptive_boosting_classifier_predict_3_play_tennis, deterministic(ground(Class))) :-
		cached_classifier(play_tennis, Classifier),
		adaptive_boosting_classifier::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Class).

	test(adaptive_boosting_classifier_predict_3_play_tennis_overcast_yes, deterministic(Class == yes)) :-
		cached_classifier(play_tennis, Classifier, [number_of_estimators(5)]),
		adaptive_boosting_classifier::predict(Classifier, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Class).

	test(adaptive_boosting_classifier_predict_3_contact_lenses, deterministic(ground(Class))) :-
		cached_classifier(contact_lenses, Classifier),
		adaptive_boosting_classifier::predict(Classifier, [age-young, spectacle_prescription-myope, astigmatism-no, tear_production_rate-reduced], Class).

	test(adaptive_boosting_classifier_predict_3_iris_setosa, deterministic(Class == setosa)) :-
		cached_classifier(iris, Classifier, [number_of_estimators(5)]),
		adaptive_boosting_classifier::predict(Classifier, [sepal_length-5.0, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Class).

	test(adaptive_boosting_classifier_predict_3_iris_virginica, deterministic(Class == virginica)) :-
		cached_classifier(iris, Classifier, [number_of_estimators(5)]),
		adaptive_boosting_classifier::predict(Classifier, [sepal_length-6.5, sepal_width-3.0, petal_length-5.5, petal_width-2.0], Class).

	% predict_probabilities/3 tests

	test(adaptive_boosting_classifier_predict_probabilities_3_structure, deterministic(type::check(list(pair(atom, probability)), Probabilities))) :-
		cached_classifier(play_tennis, Classifier),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Probabilities).

	test(adaptive_boosting_classifier_predict_probabilities_3_sum_to_one, deterministic(abs(Sum - 1.0) < 0.001)) :-
		cached_classifier(play_tennis, Classifier),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [outlook-overcast, temperature-mild, humidity-normal, wind-weak], Probabilities),
		sum_probabilities(Probabilities, 0, Sum).

	test(adaptive_boosting_classifier_predict_probabilities_3_all_classes_present, deterministic) :-
		cached_classifier(play_tennis, Classifier),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Probabilities),
		memberchk(yes-YesProbability, Probabilities),
		memberchk(no-NoProbability, Probabilities),
		assertion(number(YesProbability)),
		assertion(number(NoProbability)).

	test(adaptive_boosting_classifier_predict_probabilities_3_iris, deterministic(type::check(list(pair(atom, probability)), Probabilities))) :-
		cached_classifier(iris, Classifier),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [sepal_length-5.0, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Probabilities).

	% export_to_clauses/4 tests

	test(adaptive_boosting_classifier_export_to_clauses_4, deterministic(length(Clauses, 1))) :-
		cached_classifier(play_tennis, Classifier),
		adaptive_boosting_classifier::export_to_clauses(play_tennis, Classifier, classify, Clauses).

	test(adaptive_boosting_classifier_export_to_clauses_4_structure, deterministic(functor(Clause, my_boost, 1))) :-
		cached_classifier(play_tennis, Classifier),
		adaptive_boosting_classifier::export_to_clauses(play_tennis, Classifier, my_boost, [Clause]).

	test(adaptive_boosting_classifier_export_to_clauses_4_usable, deterministic(ground(Prediction))) :-
		cached_classifier(play_tennis, Classifier),
		adaptive_boosting_classifier::export_to_clauses(_Dataset, Classifier, classifier, [classifier(ExportedClassifier)]),
		adaptive_boosting_classifier::predict(ExportedClassifier, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Prediction).

	% export_to_file/4 tests

	test(adaptive_boosting_classifier_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		cached_classifier(play_tennis, Classifier),
		adaptive_boosting_classifier::export_to_file(play_tennis, Classifier, classify, File).

	test(adaptive_boosting_classifier_export_to_file_4_loadable, deterministic(ground(Prediction))) :-
		^^file_path('test_output.pl', File),
		cached_classifier(play_tennis, Classifier),
		adaptive_boosting_classifier::export_to_file(play_tennis, Classifier, classifier, File),
		logtalk_load(File),
		{classifier(LoadedClassifier)},
		adaptive_boosting_classifier::predict(LoadedClassifier, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Prediction).

	test(adaptive_boosting_classifier_diagnostics_2, deterministic((list::memberchk(model(adaptive_boosting_classifier), Diagnostics), list::memberchk(options(Options), Diagnostics)))) :-
		cached_classifier(play_tennis, Classifier),
		adaptive_boosting_classifier::diagnostics(Classifier, Diagnostics),
		adaptive_boosting_classifier::classifier_options(Classifier, Options).

	% print_classifier/1 tests

	test(adaptive_boosting_classifier_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		cached_classifier(play_tennis, Classifier),
		adaptive_boosting_classifier::print_classifier(Classifier).

	test(adaptive_boosting_classifier_print_classifier_1_iris, deterministic) :-
		^^suppress_text_output,
		cached_classifier(iris, Classifier),
		adaptive_boosting_classifier::print_classifier(Classifier).

	% Robustness tests with different datasets

	test(adaptive_boosting_classifier_learn_2_contact_lenses, deterministic(ground(Classifier))) :-
		adaptive_boosting_classifier::learn(contact_lenses, Classifier).

	test(adaptive_boosting_classifier_learn_2_iris, deterministic(ground(Classifier))) :-
		adaptive_boosting_classifier::learn(iris, Classifier).

	test(adaptive_boosting_classifier_learn_2_breast_cancer, deterministic(ground(Classifier))) :-
		adaptive_boosting_classifier::learn(breast_cancer, Classifier).

	% Prediction consistency tests

	test(adaptive_boosting_classifier_predict_consistent_with_probabilities, deterministic(PredictedClass == MaxProbClass)) :-
		cached_classifier(play_tennis, Classifier),
		Instance = [outlook-sunny, temperature-cool, humidity-normal, wind-weak],
		adaptive_boosting_classifier::predict(Classifier, Instance, PredictedClass),
		adaptive_boosting_classifier::predict_probabilities(Classifier, Instance, Probabilities),
		max_probability_class(Probabilities, MaxProbClass).

	% Edge-case datasets and stopping policies

	test(adaptive_boosting_classifier_singleton, deterministic) :-
		adaptive_boosting_classifier::learn(adaptive_boosting_singleton, Classifier),
		assertion(adaptive_boosting_classifier::valid_classifier(Classifier)),
		adaptive_boosting_classifier::predict(Classifier, [feature-a], Class),
		assertion(Class == yes),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [feature-a], Probabilities),
		assertion(Probabilities == [yes-1.0]).

	test(adaptive_boosting_classifier_attribute_free, deterministic) :-
		adaptive_boosting_classifier::learn(adaptive_boosting_constant, Classifier),
		assertion(adaptive_boosting_classifier::valid_classifier(Classifier)),
		adaptive_boosting_classifier::predict(Classifier, [], Class),
		assertion(Class == yes),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [], Probabilities),
		assertion(Probabilities == [yes-1.0, no-0.0]).

	test(adaptive_boosting_classifier_first_learner_rejected, error(domain_error(non_empty_ensemble, adaptive_boosting_conflicting))) :-
		adaptive_boosting_classifier::learn(adaptive_boosting_conflicting, _).

	test(adaptive_boosting_classifier_later_learner_rejected, deterministic(Trees == Previous)) :-
		Previous = [weighted_tree(0.5, leaf(yes), [feature])],
		Examples = [weighted_example(1, yes, [feature-a], 0.5), weighted_example(2, no, [feature-a], 0.5)],
		Metadata = [class(label), class_values([yes, no]), attribute_values(feature, [a])],
		adaptive_boosting_classifier<<build_ensemble(2, [feature], 2, Examples, 2, Metadata, 2, Previous, Trees).

	test(adaptive_boosting_classifier_perfect_supersedes_previous, deterministic) :-
		Previous = [weighted_tree(20.0, leaf(no), [])],
		Examples = [weighted_example(1, yes, [], 1.0)],
		Metadata = [class(label), class_values([yes, no])],
		adaptive_boosting_classifier<<build_ensemble(2, [], 2, Examples, 1, Metadata, 2, Previous, Trees),
		assertion(Trees == [weighted_tree(1.0, c45_model(leaf(yes), leaf_stats([yes-1.0])), [])]),
		Classifier = ab_classifier(Trees, [yes, no], [number_of_estimators(2)]),
		adaptive_boosting_classifier::predict(Classifier, [], Class),
		assertion(Class == yes).

	test(adaptive_boosting_classifier_print_leaf, deterministic) :-
		^^suppress_text_output,
		adaptive_boosting_classifier::learn(adaptive_boosting_constant, Classifier),
		adaptive_boosting_classifier::print_classifier(Classifier).

	% Model validation

	test(adaptive_boosting_classifier_unknown_leaf_class, fail) :-
		adaptive_boosting_classifier::valid_classifier(ab_classifier([weighted_tree(0.5, leaf(other), [feature])], [yes, no], [number_of_estimators(1)])).

	test(adaptive_boosting_classifier_missing_discrete_attribute, fail) :-
		Tree = tree(feature, [a-leaf(yes), b-leaf(no)]),
		adaptive_boosting_classifier::valid_classifier(ab_classifier([weighted_tree(0.5, Tree, [])], [yes, no], [number_of_estimators(1)])).

	test(adaptive_boosting_classifier_missing_continuous_attribute, fail) :-
		Tree = tree(feature, threshold(0.5), leaf(yes), leaf(no)),
		adaptive_boosting_classifier::valid_classifier(ab_classifier([weighted_tree(0.5, Tree, [other])], [yes, no], [number_of_estimators(1)])).

	test(adaptive_boosting_classifier_unknown_nested_class, fail) :-
		Tree = tree(feature, threshold(0.5), leaf(yes), leaf(other)),
		adaptive_boosting_classifier::valid_classifier(ab_classifier([weighted_tree(0.5, Tree, [feature])], [yes, no], [number_of_estimators(1)])).

	test(adaptive_boosting_classifier_open_ensemble, deterministic(var(Tail))) :-
		Classifier = ab_classifier([weighted_tree(0.5, leaf(yes), [])| Tail], [yes, no], [number_of_estimators(1)]),
		assertion(\+ adaptive_boosting_classifier::valid_classifier(Classifier)).

	test(adaptive_boosting_classifier_variable_estimator, deterministic(var(Estimator))) :-
		Classifier = ab_classifier([Estimator], [yes, no], [number_of_estimators(1)]),
		assertion(\+ adaptive_boosting_classifier::valid_classifier(Classifier)).

	test(adaptive_boosting_classifier_improper_ensemble, fail) :-
		adaptive_boosting_classifier::valid_classifier(ab_classifier([weighted_tree(0.5, leaf(yes), [])| invalid], [yes, no], [number_of_estimators(1)])).

	test(adaptive_boosting_classifier_empty_ensemble, fail) :-
		adaptive_boosting_classifier::valid_classifier(ab_classifier([], [yes, no], [number_of_estimators(1)])).

	test(adaptive_boosting_classifier_invalid_weights, all(\+ adaptive_boosting_classifier::valid_classifier(ab_classifier([weighted_tree(Weight, leaf(yes), [])], [yes, no], [number_of_estimators(1)])))) :-
		member(Weight, [0.0, -1.0, 1, invalid]).

	test(adaptive_boosting_classifier_excessive_estimators, fail) :-
		Trees = [weighted_tree(0.5, leaf(yes), []), weighted_tree(0.5, leaf(no), [])],
		adaptive_boosting_classifier::valid_classifier(ab_classifier(Trees, [yes, no], [number_of_estimators(1)])).

	test(adaptive_boosting_classifier_repeated_options_first_wins, deterministic) :-
		Trees = [weighted_tree(0.5, leaf(yes), []), weighted_tree(0.5, leaf(no), [])],
		adaptive_boosting_classifier::valid_classifier(ab_classifier(Trees, [yes, no], [number_of_estimators(2), number_of_estimators(1)])).

	test(adaptive_boosting_classifier_check_variable, error(instantiation_error)) :-
		adaptive_boosting_classifier::check_classifier(_).

	test(adaptive_boosting_classifier_check_invalid, error(domain_error(classifier, invalid))) :-
		adaptive_boosting_classifier::check_classifier(invalid).

	test(adaptive_boosting_classifier_cached_misclassification, deterministic) :-
		Examples = [weighted_example(1, yes, [feature-a], 0.75), weighted_example(2, no, [feature-b], 0.25)],
		adaptive_boosting_classifier<<compute_weighted_error(Examples, leaf(yes), Error, Flags),
		assertion(Error =~= 0.25),
		assertion(Flags == [false, true]),
		adaptive_boosting_classifier<<update_weights(Examples, Flags, Error, 2, Updated),
		adaptive_boosting_classifier<<normalize_weights(Updated, Normalized),
		Normalized = [weighted_example(1, yes, [feature-a], Weight1), weighted_example(2, no, [feature-b], Weight2)],
		assertion(Weight1 =~= 0.5),
		assertion(Weight2 =~= 0.5).

	test(adaptive_boosting_classifier_missing_training_value, deterministic(var(Value))) :-
		Tree = tree(feature, threshold(0.5), leaf(yes), leaf(no)),
		Examples = [weighted_example(1, no, [feature-Value], 1.0)],
		adaptive_boosting_classifier<<compute_weighted_error(Examples, Tree, Error, Flags),
		assertion(Error =~= 0.0),
		assertion(Flags == [false]).

	test(adaptive_boosting_classifier_heterogeneous_features, deterministic) :-
		Trees = [weighted_tree(1.0, tree(first, [a-leaf(yes)]), [first]), weighted_tree(2.0, tree(second, [b-leaf(no)]), [second])],
		Classifier = ab_classifier(Trees, [yes, no], [number_of_estimators(2)]),
		assertion(adaptive_boosting_classifier::valid_classifier(Classifier)),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [extra-ignored, second-b, first-a], [yes-Yes, no-No]),
		ExpectedYes is 1.0 / 3.0,
		ExpectedNo is 2.0 / 3.0,
		assertion(Yes =~= ExpectedYes),
		assertion(No =~= ExpectedNo).

	test(adaptive_boosting_classifier_shared_features, deterministic) :-
		Tree = tree(feature, [a-leaf(yes), b-leaf(no)]),
		Trees = [weighted_tree(1.0, Tree, [feature]), weighted_tree(2.0, Tree, [feature])],
		Classifier = ab_classifier(Trees, [yes, no], [number_of_estimators(2)]),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [extra-ignored, feature-a], Probabilities),
		assertion(Probabilities == [yes-1.0, no-0.0]).

	test(adaptive_boosting_classifier_empty_prediction_collection, deterministic(Predictions == [])) :-
		adaptive_boosting_classifier<<collect_weighted_predictions([], [], Predictions).

	test(adaptive_boosting_classifier_sampling_boundaries, deterministic) :-
		Examples = [weighted_example(1, other, [], 0.0), weighted_example(2, yes, [feature-a], 0.25), weighted_example(3, no, [feature-b], 0.75), weighted_example(4, other, [], 0.0)],
		adaptive_boosting_classifier<<build_cumulative_distribution(Examples, CDF),
		Draws = [0.0-1, 0.25-2, 0.25-3, 0.999-4, 1.0-5],
		adaptive_boosting_classifier<<sample_sorted_draws(CDF, Draws, Selections),
		assertion(Selections == [1-example(1, yes, [feature-a]), 2-example(2, no, [feature-b]), 3-example(3, no, [feature-b]), 4-example(4, no, [feature-b]), 5-example(5, no, [feature-b])]).

	test(adaptive_boosting_classifier_sampling_rounding_fallback, deterministic) :-
		CDF = [cdf_entry(0.5, yes, []), cdf_entry(0.999999999999, no, [])],
		adaptive_boosting_classifier<<sample_sorted_draws(CDF, [0.9999999999999-1], Selections),
		assertion(Selections == [1-example(1, no, [])]).

	test(adaptive_boosting_classifier_sampling_singleton, deterministic) :-
		adaptive_boosting_classifier<<weighted_sample([cdf_entry(1.0, yes, [])], 3, Examples),
		assertion(Examples == [example(1, yes, []), example(2, yes, []), example(3, yes, [])]).

	test(adaptive_boosting_classifier_sampling_empty, deterministic(Examples == [])) :-
		adaptive_boosting_classifier<<weighted_sample([], 0, Examples).

	test(adaptive_boosting_classifier_sampling_reproducible, deterministic) :-
		fast_random(xoshiro128pp)::get_seed(SavedSeed),
		CDF = [cdf_entry(0.1, yes, []), cdf_entry(1.0, no, [])],
		adaptive_boosting_classifier<<weighted_sample(CDF, 20, First),
		fast_random(xoshiro128pp)::set_seed(SavedSeed),
		adaptive_boosting_classifier<<weighted_sample(CDF, 20, Second),
		assertion(First == Second),
		length(First, Count),
		assertion(Count == 20).

	test(adaptive_boosting_classifier_sampling_draw_order, deterministic) :-
		Selections = [3-example(3, yes, [feature-a]), 1-example(1, no, [feature-b]), 2-example(2, yes, [feature-a])],
		keysort(Selections, Ordered),
		pairs::values(Ordered, Examples),
		assertion(Examples == [example(1, no, [feature-b]), example(2, yes, [feature-a]), example(3, yes, [feature-a])]).

	test(adaptive_boosting_classifier_sampling_reference, deterministic) :-
		CDF = [cdf_entry(0.1, yes, [feature-a]), cdf_entry(0.8, no, [feature-b]), cdf_entry(1.0, yes, [feature-c])],
		adaptive_boosting_classifier<<random_draws(50, 1, Draws),
		linear_sample(CDF, Draws, Reference),
		keysort(Draws, SortedDraws),
		adaptive_boosting_classifier<<sample_sorted_draws(CDF, SortedDraws, Selections),
		keysort(Selections, Ordered),
		pairs::values(Ordered, Actual),
		assertion(Actual == Reference).

	test(adaptive_boosting_classifier_estimator_order_on_exhaustion, deterministic(Trees == Previous)) :-
		Previous = [weighted_tree(0.5, leaf(yes), []), weighted_tree(1.0, leaf(no), [])],
		adaptive_boosting_classifier<<build_ensemble(2, [], 2, [], 0, [], 3, Previous, Trees).

	test(adaptive_boosting_classifier_prediction_tie_order, deterministic) :-
		Trees = [weighted_tree(1.0, leaf(yes), []), weighted_tree(1.0, leaf(no), [])],
		Classifier = ab_classifier(Trees, [no, yes], [number_of_estimators(2)]),
		adaptive_boosting_classifier::predict(Classifier, [], Class),
		assertion(Class == no),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [], Probabilities),
		max_probability_class(Probabilities, ProbabilityClass),
		assertion(Class == ProbabilityClass).

	test(adaptive_boosting_classifier_constant_export_roundtrip, deterministic) :-
		adaptive_boosting_classifier::learn(adaptive_boosting_constant, Classifier),
		adaptive_boosting_classifier::export_to_clauses(adaptive_boosting_constant, Classifier, constant_model, [constant_model(Exported)]),
		assertion(Exported == Classifier),
		assertion(adaptive_boosting_classifier::valid_classifier(Exported)),
		adaptive_boosting_classifier::predict(Exported, [], Class),
		assertion(Class == yes),
		adaptive_boosting_classifier::diagnostics(Exported, Diagnostics),
		assertion(list::memberchk(estimators(1), Diagnostics)).

	test(adaptive_boosting_classifier_indexed_votes, deterministic) :-
		benchmark_classes(20, Classes),
		vote_predictions(200, Classes, Classes, Predictions),
		adaptive_boosting_classifier<<accumulate_weighted_votes(Predictions, Classes, Votes),
		adaptive_boosting_classifier<<accumulate_votes_for_classes(Classes, Predictions, Reference),
		assertion(Votes == Reference).

	test(adaptive_boosting_classifier_indexed_zero_votes, deterministic) :-
		benchmark_classes(20, [Class| Classes]),
		adaptive_boosting_classifier<<accumulate_weighted_votes([2.0-Class], [Class| Classes], Votes),
		adaptive_boosting_classifier<<indexed_votes([2.0-Class], [Class| Classes], Indexed),
		assertion(Votes == Indexed),
		Votes = [Class-Weight| Rest],
		assertion(Weight =~= 2.0),
		forall(member(_-Zero, Rest), assertion(Zero =~= 0.0)).

	test(adaptive_boosting_classifier_multiclass_weight_update, deterministic) :-
		Examples = [weighted_example(1, yes, [], 0.8), weighted_example(2, no, [], 0.1), weighted_example(3, other, [], 0.1)],
		adaptive_boosting_classifier<<update_weights(Examples, [false, true, true], 0.2, 3, Updated),
		adaptive_boosting_classifier<<normalize_weights(Updated, Normalized),
		Expected is 1.0 / 3.0,
		forall(member(weighted_example(_, _, _, Weight), Normalized), assertion(Weight =~= Expected)),
		adaptive_boosting_classifier<<learner_weight(0.2, 3, Alpha),
		ExpectedAlpha is log(4.0) + log(2.0),
		assertion(Alpha =~= ExpectedAlpha).

	test(adaptive_boosting_classifier_tiny_error_update, deterministic) :-
		Error is 1.0e-300,
		Examples = [weighted_example(1, yes, [], 1.0), weighted_example(2, no, [], Error)],
		adaptive_boosting_classifier<<learner_weight(Error, 2, Alpha),
		assertion(Alpha > 600.0),
		assertion(Alpha < 800.0),
		adaptive_boosting_classifier<<update_weights(Examples, [false, true], Error, 2, [weighted_example(1, yes, [], Correct), weighted_example(2, no, [], Incorrect)]),
		assertion(Correct =~= 0.5),
		assertion(Incorrect =~= 0.5).

	test(adaptive_boosting_classifier_uniform_fallback, deterministic) :-
		adaptive_boosting_classifier<<normalize_weights([weighted_example(1, yes, [], 0.0), weighted_example(2, no, [], 0.0)], Normalized),
		assertion(Normalized == [weighted_example(1, yes, [], 0.5), weighted_example(2, no, [], 0.5)]).

	test(adaptive_boosting_classifier_zero_vote_normalization, deterministic) :-
		adaptive_boosting_classifier<<normalize_votes([yes-0.0, no-0.0], 0.0, Probabilities),
		assertion(Probabilities == [yes-0.0, no-0.0]).

	% auxiliary predicates

	cached_classifier(Dataset, Classifier) :-
		cached_classifier(Dataset, Classifier, []).

	cached_classifier(Dataset, Classifier, Options) :-
		(	classifier_cache(Dataset, Options, Cached) ->
			Classifier = Cached
		;	adaptive_boosting_classifier::learn(Dataset, Classifier, Options),
			assertz(classifier_cache(Dataset, Options, Classifier))
		).

	benchmark_classes(0, []) :-
		!.
	benchmark_classes(Count, [Class| Classes]) :-
		number_codes(Count, Digits),
		atom_codes(Class, [99, 108, 97, 115, 115, 95| Digits]),
		Remaining is Count - 1,
		benchmark_classes(Remaining, Classes).

	vote_predictions(0, _, _, []) :-
		!.
	vote_predictions(Count, [Class| Classes], AllClasses, [1.0-Class| Predictions]) :-
		(	Classes == [] ->
			NextClasses = AllClasses
		;
			NextClasses = Classes
		),
		Remaining is Count - 1,
		vote_predictions(Remaining, NextClasses, AllClasses, Predictions).

	linear_sample(_, [], []) :-
		!.
	linear_sample(CDF, [Random-Id| Draws], [example(Id, Class, AVs)| Examples]) :-
		linear_select(CDF, Random, Class, AVs),
		linear_sample(CDF, Draws, Examples).

	linear_select([cdf_entry(Cumulative, Class, AVs)| Rest], Random, Class, AVs) :-
		(	Random < Cumulative
		;	Rest == []
		),
		!.
	linear_select([_| Rest], Random, Class, AVs) :-
		linear_select(Rest, Random, Class, AVs).

	sum_probabilities([], Sum, Sum).
	sum_probabilities([_-Probability| Rest], Sum0, Sum) :-
		Sum1 is Sum0 + Probability,
		sum_probabilities(Rest, Sum1, Sum).

	max_probability_class([Class-Probability], Class) :-
		!,
		Probability >= 0.
	max_probability_class([Class1-Probability1, Class2-Probability2| Rest], Class) :-
		(	Probability1 >= Probability2 ->
			max_probability_class([Class1-Probability1| Rest], Class)
		;	max_probability_class([Class2-Probability2| Rest], Class)
		).

:- end_object.
