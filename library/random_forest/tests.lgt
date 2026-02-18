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
		date is 2026-02-18,
		comment is 'Unit tests for the "random_forest" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(type, [
		check/2
	]).

	cover(random_forest).

	cleanup :-
		^^clean_file('test_output.pl').

	% learn/2 tests - verify classifier structure

	test(random_forest_learn_2_play_tennis, true(ground(Classifier))) :-
		random_forest::learn(play_tennis, Classifier).

	test(random_forest_learn_2_classifier_structure, true(functor(Classifier, rf_classifier, 3))) :-
		random_forest::learn(play_tennis, Classifier).

	test(random_forest_learn_2_default_num_trees, true(NumTrees == 10)) :-
		random_forest::learn(play_tennis, Classifier),
		Classifier = rf_classifier(Trees, _, _),
		length(Trees, NumTrees).

	test(random_forest_learn_2_class_values, true(ClassValues == [yes, no])) :-
		random_forest::learn(play_tennis, Classifier),
		Classifier = rf_classifier(_, ClassValues, _).

	% learn/3 tests - with options

	test(random_forest_learn_3_custom_num_trees, true(NumTrees == 5)) :-
		random_forest::learn(play_tennis, Classifier, [number_of_trees(5)]),
		Classifier = rf_classifier(Trees, _, _),
		length(Trees, NumTrees).

	test(random_forest_learn_3_custom_max_features, true(ground(Classifier))) :-
		random_forest::learn(play_tennis, Classifier, [number_of_trees(3), maximum_features_per_tree(2)]).

	% Ensemble behavior tests - verify multiple trees with different features

	test(random_forest_ensemble_has_multiple_trees, true(NumTrees > 1)) :-
		random_forest::learn(play_tennis, Classifier, [number_of_trees(5)]),
		Classifier = rf_classifier(Trees, _, _),
		length(Trees, NumTrees).

	test(random_forest_trees_have_different_features, true) :-
		% With random feature selection, trees should potentially use different features
		random_forest::learn(iris, Classifier, [number_of_trees(3), maximum_features_per_tree(2)]),
		Classifier = rf_classifier(Trees, _, _),
		% Just verify the structure - each tree has associated feature names
		^^assertion(length(Trees, 3)),
		forall(
			(memberchk(tree(_Tree, FeatureNames), Trees), length(FeatureNames, Max)),
			^^assertion(Max =< 2)
		).

	% predict/3 tests - ensemble predictions

	test(random_forest_predict_3_play_tennis, true(ground(Class))) :-
		random_forest::learn(play_tennis, Classifier),
		random_forest::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Class).

	test(random_forest_predict_3_play_tennis_overcast_yes, true(Class == yes)) :-
		% Overcast should strongly predict "yes" even with ensemble
		random_forest::learn(play_tennis, Classifier, [number_of_trees(5)]),
		random_forest::predict(Classifier, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Class).

	test(random_forest_predict_3_contact_lenses, true(ground(Class))) :-
		random_forest::learn(contact_lenses, Classifier),
		random_forest::predict(Classifier, [age-young, spectacle_prescription-myope, astigmatism-no, tear_production_rate-reduced], Class).

	test(random_forest_predict_3_iris_setosa, true(Class == setosa)) :-
		% Setosa is well-separated, ensemble should agree
		random_forest::learn(iris, Classifier, [number_of_trees(5)]),
		random_forest::predict(Classifier, [sepal_length-5.0, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Class).

	test(random_forest_predict_3_iris_virginica, true(Class == virginica)) :-
		random_forest::learn(iris, Classifier, [number_of_trees(5)]),
		random_forest::predict(Classifier, [sepal_length-6.5, sepal_width-3.0, petal_length-5.5, petal_width-2.0], Class).

	% predict_probabilities/3 tests - verify voting behavior

	test(random_forest_predict_probabilities_3_structure, true(check(list(pair(atom, probability)), Probabilities))) :-
		random_forest::learn(play_tennis, Classifier),
		random_forest::predict_probabilities(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Probabilities).

	test(random_forest_predict_probabilities_3_sum_to_one, true(abs(Sum - 1.0) < 0.001)) :-
		random_forest::learn(play_tennis, Classifier),
		random_forest::predict_probabilities(Classifier, [outlook-overcast, temperature-mild, humidity-normal, wind-weak], Probabilities),
		sum_probabilities(Probabilities, 0, Sum).

	test(random_forest_predict_probabilities_3_all_classes_present, true) :-
		random_forest::learn(play_tennis, Classifier),
		random_forest::predict_probabilities(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Probabilities),
		memberchk(yes-_, Probabilities),
		memberchk(no-_, Probabilities).

	test(random_forest_predict_probabilities_3_iris, true(check(list(pair(atom, probability)), Probabilities))) :-
		random_forest::learn(iris, Classifier),
		random_forest::predict_probabilities(Classifier, [sepal_length-5.0, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Probabilities).

	% classifier_to_clauses/4 tests

	test(rf_classifier_to_clauses_4, true(length(Clauses, 1))) :-
		random_forest::learn(play_tennis, Classifier),
		random_forest::classifier_to_clauses(play_tennis, Classifier, classify, Clauses).

	test(rf_classifier_to_clauses_4_structure, true(functor(Clause, my_forest, 3))) :-
		random_forest::learn(play_tennis, Classifier),
		random_forest::classifier_to_clauses(play_tennis, Classifier, my_forest, [Clause]).

	test(rf_classifier_to_clauses_4_usable, true(ground(Prediction))) :-
		random_forest::learn(play_tennis, Classifier),
		random_forest::classifier_to_clauses(_Dataset, Classifier, classify, [Clause]),
		random_forest::predict(Clause, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Prediction).

	% classifier_to_file/4 tests

	test(rf_classifier_to_file_4, deterministic) :-
		^^file_path('test_output.pl', File),
		random_forest::learn(play_tennis, Classifier),
		random_forest::classifier_to_file(play_tennis, Classifier, classify, File).

	test(rf_classifier_to_file_4_loadable, true(ground(Prediction))) :-
		^^file_path('test_output.pl', File),
		random_forest::learn(play_tennis, Classifier),
		random_forest::classifier_to_file(play_tennis, Classifier, classify, File),
		logtalk_load(File),
		{classify(Trees, ClassValues, Options)},
		random_forest::predict(classify(Trees, ClassValues, Options), [outlook-overcast, temperature-hot, humidity-high, wind-weak], Prediction).

	% print_classifier/1 tests

	test(random_forest_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		random_forest::learn(play_tennis, Classifier),
		random_forest::print_classifier(Classifier).

	test(random_forest_print_classifier_1_iris, deterministic) :-
		^^suppress_text_output,
		random_forest::learn(iris, Classifier),
		random_forest::print_classifier(Classifier).

	% Robustness tests with different datasets

	test(random_forest_learn_2_contact_lenses, true(ground(Classifier))) :-
		random_forest::learn(contact_lenses, Classifier).

	test(random_forest_learn_2_iris, true(ground(Classifier))) :-
		random_forest::learn(iris, Classifier).

	test(random_forest_learn_2_breast_cancer, true(ground(Classifier))) :-
		random_forest::learn(breast_cancer, Classifier).

	% Prediction consistency tests

	test(random_forest_predict_consistent_with_probabilities, true(PredictedClass == MaxProbClass)) :-
		random_forest::learn(play_tennis, Classifier),
		Instance = [outlook-sunny, temperature-cool, humidity-normal, wind-weak],
		random_forest::predict(Classifier, Instance, PredictedClass),
		random_forest::predict_probabilities(Classifier, Instance, Probabilities),
		max_prob_class(Probabilities, MaxProbClass).

	% Helper predicates

	sum_probabilities([], Sum, Sum).
	sum_probabilities([_-Prob| Rest], Acc, Sum) :-
		NewAcc is Acc + Prob,
		sum_probabilities(Rest, NewAcc, Sum).

	max_prob_class([Class-Prob], Class) :-
		Prob >= 0.
	max_prob_class([C1-P1, C2-P2| Rest], MaxClass) :-
		(	P1 >= P2 ->
			max_prob_class([C1-P1| Rest], MaxClass)
		;	max_prob_class([C2-P2| Rest], MaxClass)
		).

:- end_object.

