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
		date is 2026-02-20,
		comment is 'Unit tests for the "ada_boost" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(ada_boost).

	cleanup :-
		^^clean_file('test_output.pl').

	% learn/2 tests - verify classifier structure

	test(ada_boost_learn_2_play_tennis, true(ground(Classifier))) :-
		ada_boost::learn(play_tennis, Classifier).

	test(ada_boost_learn_2_classifier_structure, true(functor(Classifier, ab_classifier, 3))) :-
		ada_boost::learn(play_tennis, Classifier).

	test(ada_boost_learn_2_class_values, true(ClassValues == [yes, no])) :-
		ada_boost::learn(play_tennis, Classifier),
		Classifier = ab_classifier(_, ClassValues, _).

	test(ada_boost_learn_2_has_weighted_trees, true(NumTrees > 0)) :-
		ada_boost::learn(play_tennis, Classifier),
		Classifier = ab_classifier(WeightedTrees, _, _),
		length(WeightedTrees, NumTrees).

	% learn/3 tests - with options

	test(ada_boost_learn_3_custom_num_estimators, true(ground(Classifier))) :-
		ada_boost::learn(play_tennis, Classifier, [number_of_estimators(5)]).

	test(ada_boost_learn_3_estimator_count, true(NumTrees =< 5)) :-
		ada_boost::learn(play_tennis, Classifier, [number_of_estimators(5)]),
		Classifier = ab_classifier(WeightedTrees, _, _),
		length(WeightedTrees, NumTrees).

	% Ensemble behavior tests

	test(ada_boost_trees_have_positive_alpha, true) :-
		ada_boost::learn(play_tennis, Classifier, [number_of_estimators(3)]),
		Classifier = ab_classifier(WeightedTrees, _, _),
		forall(
			memberchk(weighted_tree(Alpha, _, _), WeightedTrees),
			^^assertion(Alpha > 0.0)
		).

	% predict/3 tests - ensemble predictions

	test(ada_boost_predict_3_play_tennis, true(ground(Class))) :-
		ada_boost::learn(play_tennis, Classifier),
		ada_boost::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Class).

	test(ada_boost_predict_3_play_tennis_overcast_yes, true(Class == yes)) :-
		ada_boost::learn(play_tennis, Classifier, [number_of_estimators(5)]),
		ada_boost::predict(Classifier, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Class).

	test(ada_boost_predict_3_contact_lenses, true(ground(Class))) :-
		ada_boost::learn(contact_lenses, Classifier),
		ada_boost::predict(Classifier, [age-young, spectacle_prescription-myope, astigmatism-no, tear_production_rate-reduced], Class).

	test(ada_boost_predict_3_iris_setosa, true(Class == setosa)) :-
		ada_boost::learn(iris, Classifier, [number_of_estimators(5)]),
		ada_boost::predict(Classifier, [sepal_length-5.0, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Class).

	test(ada_boost_predict_3_iris_virginica, true(Class == virginica)) :-
		ada_boost::learn(iris, Classifier, [number_of_estimators(5)]),
		ada_boost::predict(Classifier, [sepal_length-6.5, sepal_width-3.0, petal_length-5.5, petal_width-2.0], Class).

	% predict_probabilities/3 tests

	test(ada_boost_predict_probabilities_3_structure, true(type::check(list(pair(atom, probability)), Probabilities))) :-
		ada_boost::learn(play_tennis, Classifier),
		ada_boost::predict_probabilities(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Probabilities).

	test(ada_boost_predict_probabilities_3_sum_to_one, true(abs(Sum - 1.0) < 0.001)) :-
		ada_boost::learn(play_tennis, Classifier),
		ada_boost::predict_probabilities(Classifier, [outlook-overcast, temperature-mild, humidity-normal, wind-weak], Probabilities),
		sum_probabilities(Probabilities, 0, Sum).

	test(ada_boost_predict_probabilities_3_all_classes_present, true) :-
		ada_boost::learn(play_tennis, Classifier),
		ada_boost::predict_probabilities(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Probabilities),
		memberchk(yes-_, Probabilities),
		memberchk(no-_, Probabilities).

	test(ada_boost_predict_probabilities_3_iris, true(type::check(list(pair(atom, probability)), Probabilities))) :-
		ada_boost::learn(iris, Classifier),
		ada_boost::predict_probabilities(Classifier, [sepal_length-5.0, sepal_width-3.5, petal_length-1.4, petal_width-0.2], Probabilities).

	% classifier_to_clauses/4 tests

	test(ada_boost_classifier_to_clauses_4, true(length(Clauses, 1))) :-
		ada_boost::learn(play_tennis, Classifier),
		ada_boost::classifier_to_clauses(play_tennis, Classifier, classify, Clauses).

	test(ada_boost_classifier_to_clauses_4_structure, true(functor(Clause, my_boost, 3))) :-
		ada_boost::learn(play_tennis, Classifier),
		ada_boost::classifier_to_clauses(play_tennis, Classifier, my_boost, [Clause]).

	test(ada_boost_classifier_to_clauses_4_usable, true(ground(Prediction))) :-
		ada_boost::learn(play_tennis, Classifier),
		ada_boost::classifier_to_clauses(_Dataset, Classifier, classify, [Clause]),
		ada_boost::predict(Clause, [outlook-overcast, temperature-hot, humidity-high, wind-weak], Prediction).

	% classifier_to_file/4 tests

	test(ada_boost_classifier_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		ada_boost::learn(play_tennis, Classifier),
		ada_boost::classifier_to_file(play_tennis, Classifier, classify, File).

	test(ada_boost_classifier_to_file_4_loadable, true(ground(Prediction))) :-
		^^file_path('test_output.pl', File),
		ada_boost::learn(play_tennis, Classifier),
		ada_boost::classifier_to_file(play_tennis, Classifier, classify, File),
		logtalk_load(File),
		{classify(WeightedTrees, ClassValues, Options)},
		ada_boost::predict(classify(WeightedTrees, ClassValues, Options), [outlook-overcast, temperature-hot, humidity-high, wind-weak], Prediction).

	% print_classifier/1 tests

	test(ada_boost_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		ada_boost::learn(play_tennis, Classifier),
		ada_boost::print_classifier(Classifier).

	test(ada_boost_print_classifier_1_iris, deterministic) :-
		^^suppress_text_output,
		ada_boost::learn(iris, Classifier),
		ada_boost::print_classifier(Classifier).

	% Robustness tests with different datasets

	test(ada_boost_learn_2_contact_lenses, true(ground(Classifier))) :-
		ada_boost::learn(contact_lenses, Classifier).

	test(ada_boost_learn_2_iris, true(ground(Classifier))) :-
		ada_boost::learn(iris, Classifier).

	test(ada_boost_learn_2_breast_cancer, true(ground(Classifier))) :-
		ada_boost::learn(breast_cancer, Classifier).

	% Prediction consistency tests

	test(ada_boost_predict_consistent_with_probabilities, true(PredictedClass == MaxProbClass)) :-
		ada_boost::learn(play_tennis, Classifier),
		Instance = [outlook-sunny, temperature-cool, humidity-normal, wind-weak],
		ada_boost::predict(Classifier, Instance, PredictedClass),
		ada_boost::predict_probabilities(Classifier, Instance, Probabilities),
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
