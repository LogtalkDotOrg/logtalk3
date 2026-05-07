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
		comment is 'Unit tests for the "random_forest_classifier" library.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(type, [
		check/2
	]).

	cover(random_forest_classifier).

	cleanup :-
		^^clean_file('test_output.pl').

	% learn/2 tests - verify classifier structure

	test(random_forest_learn_2_play_tennis, true(ground(Classifier))) :-
		random_forest_classifier::learn(play_tennis, Classifier).

	test(random_forest_valid_classifier_1, deterministic(random_forest_classifier::valid_classifier(Classifier))) :-
		random_forest_classifier::learn(play_tennis, Classifier).

	test(random_forest_invalid_classifier_1, fail) :-
		random_forest_classifier::valid_classifier(rf_classifier([tree(leaf(yes), [outlook, outlook])], [yes, no], [number_of_trees(1), random_seed(1357911)])).

	test(random_forest_learn_2_classifier_structure, true(functor(Classifier, rf_classifier, 3))) :-
		random_forest_classifier::learn(play_tennis, Classifier).

	test(random_forest_learn_2_default_num_trees, true(NumTrees == 10)) :-
		random_forest_classifier::learn(play_tennis, Classifier),
		Classifier = rf_classifier(Trees, _, _),
		length(Trees, NumTrees).

	test(random_forest_learn_2_class_values, true(ClassValues == [yes, no])) :-
		random_forest_classifier::learn(play_tennis, Classifier),
		Classifier = rf_classifier(_, ClassValues, _).

	% learn/3 tests - with options

	test(random_forest_learn_3_custom_num_trees, true(NumTrees == 5)) :-
		random_forest_classifier::learn(play_tennis, Classifier, [number_of_trees(5)]),
		Classifier = rf_classifier(Trees, _, _),
		length(Trees, NumTrees).

	test(random_forest_learn_3_random_seed_option, deterministic(RandomSeed == 17)) :-
		random_forest_classifier::learn(play_tennis, rf_classifier(_Trees, _ClassValues, Options), [number_of_trees(5), random_seed(17)]),
		memberchk(random_seed(RandomSeed), Options).

	test(random_forest_learn_3_same_seed_same_classifier, deterministic(Classifier1 == Classifier2)) :-
		random_forest_classifier::learn(play_tennis, Classifier1, [number_of_trees(5), random_seed(19)]),
		random_forest_classifier::learn(play_tennis, Classifier2, [number_of_trees(5), random_seed(19)]).

	test(random_forest_learn_3_custom_max_features, true(ground(Classifier))) :-
		random_forest_classifier::learn(play_tennis, Classifier, [number_of_trees(3), maximum_features_per_tree(2)]).

	% Ensemble behavior tests - verify multiple trees with different features

	test(random_forest_ensemble_has_multiple_trees, true(NumTrees > 1)) :-
		random_forest_classifier::learn(play_tennis, Classifier, [number_of_trees(5)]),
		Classifier = rf_classifier(Trees, _, _),
		length(Trees, NumTrees).

	test(random_forest_trees_have_different_features, true) :-
		% With random feature selection, trees should potentially use different features
		random_forest_classifier::learn(iris, Classifier, [number_of_trees(3), maximum_features_per_tree(2)]),
		Classifier = rf_classifier(Trees, _, _),
		% Just verify the structure - each tree has associated feature names
		^^assertion(length(Trees, 3)),
		forall(
			(member(tree(_Tree, FeatureNames), Trees), length(FeatureNames, Max)),
			^^assertion(Max =< 2)
		).

	% predict/3 tests - ensemble predictions

	test(random_forest_predict_3_play_tennis, deterministic(ground(Class))) :-
		random_forest_classifier::learn(play_tennis, Classifier),
		random_forest_classifier::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Class).

	test(random_forest_predict_3_play_tennis_overcast_yes, deterministic(Class == yes)) :-
		% Overcast should strongly predict "yes" even with ensemble
		random_forest_classifier::learn(play_tennis, Classifier, [number_of_trees(5), random_seed(17)]),
		random_forest_classifier::predict(Classifier, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Class).

	test(random_forest_predict_3_contact_lenses, deterministic(ground(Class))) :-
		random_forest_classifier::learn(contact_lenses, Classifier),
		random_forest_classifier::predict(Classifier, [age-young, spectacle_prescription-myope, astigmatism-no, tear_production_rate-reduced], Class).

	test(random_forest_predict_3_iris_setosa, deterministic(Class == setosa)) :-
		% Setosa is well-separated, ensemble should agree
		random_forest_classifier::learn(iris, Classifier, [number_of_trees(5), random_seed(17)]),
		random_forest_classifier::predict(Classifier, [sepal_length-5.0, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Class).

	test(random_forest_predict_3_iris_virginica, deterministic(Class == virginica)) :-
		random_forest_classifier::learn(iris, Classifier, [number_of_trees(5), random_seed(17)]),
		random_forest_classifier::predict(Classifier, [sepal_length-6.5, sepal_width-3.0, petal_length-5.5, petal_width-2.0], Class).

	% predict_probabilities/3 tests - verify voting behavior

	test(random_forest_predict_probabilities_3_structure, deterministic(check(list(pair(atom, probability)), Probabilities))) :-
		random_forest_classifier::learn(play_tennis, Classifier),
		random_forest_classifier::predict_probabilities(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Probabilities).

	test(random_forest_predict_probabilities_3_sum_to_one, deterministic(abs(Sum - 1.0) < 0.001)) :-
		random_forest_classifier::learn(play_tennis, Classifier),
		random_forest_classifier::predict_probabilities(Classifier, [outlook-overcast, temperature-mild, humidity-normal, wind-weak], Probabilities),
		sum_probabilities(Probabilities, 0, Sum).

	test(random_forest_predict_probabilities_3_all_classes_present, deterministic(ground(Yes-No))) :-
		random_forest_classifier::learn(play_tennis, Classifier),
		random_forest_classifier::predict_probabilities(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Probabilities),
		memberchk(yes-Yes, Probabilities),
		memberchk(no-No, Probabilities).

	test(random_forest_predict_probabilities_3_iris, true(check(list(pair(atom, probability)), Probabilities))) :-
		random_forest_classifier::learn(iris, Classifier),
		random_forest_classifier::predict_probabilities(Classifier, [sepal_length-5.0, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Probabilities).

	% export_to_clauses/4 tests

	test(rf_export_to_clauses_4, deterministic(length(Clauses, 1))) :-
		random_forest_classifier::learn(play_tennis, Classifier),
		random_forest_classifier::export_to_clauses(play_tennis, Classifier, classify, Clauses).

	test(rf_export_to_clauses_4_structure, deterministic(functor(Clause, my_forest, 1))) :-
		random_forest_classifier::learn(play_tennis, Classifier),
		random_forest_classifier::export_to_clauses(play_tennis, Classifier, my_forest, [Clause]).

	test(rf_export_to_clauses_4_usable, deterministic(ground(Prediction))) :-
		random_forest_classifier::learn(play_tennis, Classifier),
		random_forest_classifier::export_to_clauses(_Dataset, Classifier, classifier, [classifier(ExportedClassifier)]),
		random_forest_classifier::predict(ExportedClassifier, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Prediction).

	% export_to_file/4 tests

	test(rf_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		random_forest_classifier::learn(play_tennis, Classifier),
		random_forest_classifier::export_to_file(play_tennis, Classifier, classify, File).

	test(rf_export_to_file_4_loadable, true(ground(Prediction))) :-
		^^file_path('test_output.pl', File),
		random_forest_classifier::learn(play_tennis, Classifier),
		random_forest_classifier::export_to_file(play_tennis, Classifier, classifier, File),
		logtalk_load(File),
		{classifier(LoadedClassifier)},
		random_forest_classifier::predict(LoadedClassifier, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Prediction).

	test(random_forest_diagnostics_2, deterministic((memberchk(model(random_forest_classifier), Diagnostics), memberchk(options(Options), Diagnostics)))) :-
		random_forest_classifier::learn(play_tennis, Classifier),
		random_forest_classifier::diagnostics(Classifier, Diagnostics),
		random_forest_classifier::classifier_options(Classifier, Options).

	% print_classifier/1 tests

	test(random_forest_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		random_forest_classifier::learn(play_tennis, Classifier),
		random_forest_classifier::print_classifier(Classifier).

	test(random_forest_print_classifier_1_iris, deterministic) :-
		^^suppress_text_output,
		random_forest_classifier::learn(iris, Classifier),
		random_forest_classifier::print_classifier(Classifier).

	% Robustness tests with different datasets

	test(random_forest_learn_2_contact_lenses, deterministic(ground(Classifier))) :-
		random_forest_classifier::learn(contact_lenses, Classifier).

	test(random_forest_learn_2_iris, deterministic(ground(Classifier))) :-
		random_forest_classifier::learn(iris, Classifier).

	test(random_forest_learn_2_breast_cancer, deterministic(ground(Classifier))) :-
		random_forest_classifier::learn(breast_cancer, Classifier).

	% Prediction consistency tests

	test(random_forest_predict_consistent_with_probabilities, deterministic(PredictedClass == MaxProbClass)) :-
		random_forest_classifier::learn(play_tennis, Classifier),
		Instance = [outlook-sunny, temperature-cool, humidity-normal, wind-weak],
		random_forest_classifier::predict(Classifier, Instance, PredictedClass),
		random_forest_classifier::predict_probabilities(Classifier, Instance, Probabilities),
		max_probability_class(Probabilities, MaxProbClass).

	% Auxiliary predicates

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
