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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Unit tests for the "adaptive_boosting_classifier" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(adaptive_boosting_classifier).

	cleanup :-
		^^clean_file('test_output.pl').

	% learn/2 tests - verify classifier structure

	test(adaptive_boosting_classifier_learn_2_play_tennis, true(ground(Classifier))) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier).

	test(adaptive_boosting_classifier_valid_classifier_1, deterministic(adaptive_boosting_classifier::valid_classifier(Classifier))) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier).

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

	test(adaptive_boosting_classifier_trees_have_positive_alpha, true) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier, [number_of_estimators(3)]),
		Classifier = ab_classifier(WeightedTrees, _, _),
		forall(
			memberchk(weighted_tree(Alpha, _, _), WeightedTrees),
			^^assertion(Alpha > 0.0)
		).

	% predict/3 tests - ensemble predictions

	test(adaptive_boosting_classifier_predict_3_play_tennis, deterministic(ground(Class))) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		adaptive_boosting_classifier::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Class).

	test(adaptive_boosting_classifier_predict_3_play_tennis_overcast_yes, deterministic(Class == yes)) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier, [number_of_estimators(5)]),
		adaptive_boosting_classifier::predict(Classifier, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Class).

	test(adaptive_boosting_classifier_predict_3_contact_lenses, deterministic(ground(Class))) :-
		adaptive_boosting_classifier::learn(contact_lenses, Classifier),
		adaptive_boosting_classifier::predict(Classifier, [age-young, spectacle_prescription-myope, astigmatism-no, tear_production_rate-reduced], Class).

	test(adaptive_boosting_classifier_predict_3_iris_setosa, deterministic(Class == setosa)) :-
		adaptive_boosting_classifier::learn(iris, Classifier, [number_of_estimators(5)]),
		adaptive_boosting_classifier::predict(Classifier, [sepal_length-5.0, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Class).

	test(adaptive_boosting_classifier_predict_3_iris_virginica, deterministic(Class == virginica)) :-
		adaptive_boosting_classifier::learn(iris, Classifier, [number_of_estimators(5)]),
		adaptive_boosting_classifier::predict(Classifier, [sepal_length-6.5, sepal_width-3.0, petal_length-5.5, petal_width-2.0], Class).

	% predict_probabilities/3 tests

	test(adaptive_boosting_classifier_predict_probabilities_3_structure, deterministic(type::check(list(pair(atom, probability)), Probabilities))) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Probabilities).

	test(adaptive_boosting_classifier_predict_probabilities_3_sum_to_one, deterministic(abs(Sum - 1.0) < 0.001)) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [outlook-overcast, temperature-mild, humidity-normal, wind-weak], Probabilities),
		sum_probabilities(Probabilities, 0, Sum).

	test(adaptive_boosting_classifier_predict_probabilities_3_all_classes_present, deterministic) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Probabilities),
		memberchk(yes-_, Probabilities),
		memberchk(no-_, Probabilities).

	test(adaptive_boosting_classifier_predict_probabilities_3_iris, deterministic(type::check(list(pair(atom, probability)), Probabilities))) :-
		adaptive_boosting_classifier::learn(iris, Classifier),
		adaptive_boosting_classifier::predict_probabilities(Classifier, [sepal_length-5.0, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Probabilities).

	% export_to_clauses/4 tests

	test(adaptive_boosting_classifier_export_to_clauses_4, deterministic(length(Clauses, 1))) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		adaptive_boosting_classifier::export_to_clauses(play_tennis, Classifier, classify, Clauses).

	test(adaptive_boosting_classifier_export_to_clauses_4_structure, deterministic(functor(Clause, my_boost, 1))) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		adaptive_boosting_classifier::export_to_clauses(play_tennis, Classifier, my_boost, [Clause]).

	test(adaptive_boosting_classifier_export_to_clauses_4_usable, deterministic(ground(Prediction))) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		adaptive_boosting_classifier::export_to_clauses(_Dataset, Classifier, classifier, [classifier(ExportedClassifier)]),
		adaptive_boosting_classifier::predict(ExportedClassifier, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Prediction).

	% export_to_file/4 tests

	test(adaptive_boosting_classifier_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		adaptive_boosting_classifier::export_to_file(play_tennis, Classifier, classify, File).

	test(adaptive_boosting_classifier_export_to_file_4_loadable, deterministic(ground(Prediction))) :-
		^^file_path('test_output.pl', File),
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		adaptive_boosting_classifier::export_to_file(play_tennis, Classifier, classifier, File),
		logtalk_load(File),
		{classifier(LoadedClassifier)},
		adaptive_boosting_classifier::predict(LoadedClassifier, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Prediction).

	test(adaptive_boosting_classifier_diagnostics_2, deterministic((list::memberchk(model(adaptive_boosting_classifier), Diagnostics), list::memberchk(options(Options), Diagnostics)))) :-
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		adaptive_boosting_classifier::diagnostics(Classifier, Diagnostics),
		adaptive_boosting_classifier::classifier_options(Classifier, Options).

	% print_classifier/1 tests

	test(adaptive_boosting_classifier_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		adaptive_boosting_classifier::print_classifier(Classifier).

	test(adaptive_boosting_classifier_print_classifier_1_iris, deterministic) :-
		^^suppress_text_output,
		adaptive_boosting_classifier::learn(iris, Classifier),
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
		adaptive_boosting_classifier::learn(play_tennis, Classifier),
		Instance = [outlook-sunny, temperature-cool, humidity-normal, wind-weak],
		adaptive_boosting_classifier::predict(Classifier, Instance, PredictedClass),
		adaptive_boosting_classifier::predict_probabilities(Classifier, Instance, Probabilities),
		max_probability_class(Probabilities, MaxProbClass).

	% Auxiliary predicates

	sum_probabilities([], Sum, Sum).
	sum_probabilities([_-Probability| Rest], Sum0, Sum) :-
		Sum1 is Sum0 + Probability,
		sum_probabilities(Rest, Sum1, Sum).

	max_probability_class([Class-Probability], Class) :-
		Probability >= 0.
	max_probability_class([Class1-Probability1, Class2-Probability2| Rest], Class) :-
		(	Probability1 >= Probability2 ->
			max_probability_class([Class1-Probability1| Rest], Class)
		;	max_probability_class([Class2-Probability2| Rest], Class)
		).

:- end_object.
