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
		date is 2026-02-16,
		comment is 'Unit tests for the "knn" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(type, [
		check/2
	]).

	cover(knn).

	cleanup :-
		^^clean_file('test_output.pl').

	% Test learn/2 with continuous iris dataset
	test(knn_learn_2_iris, true(ground(Classifier))) :-
		knn::learn(nb_iris_dataset, Classifier).

	% Test learn/3 with options
	test(knn_learn_3_with_options, true(ground(Classifier))) :-
		knn::learn(nb_iris_dataset, Classifier, [k(5), distance_metric(manhattan)]).

	% Test predict/3 with iris dataset (continuous features)
	test(knn_predict_3_iris_setosa, true(Prediction == setosa)) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction).

	test(knn_predict_3_iris_versicolor, true(Prediction == versicolor)) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict(Classifier, [sepal_length-6.5, sepal_width-3.0, petal_length-4.6, petal_width-1.4], Prediction).

	test(knn_predict_3_iris_virginica, true(Prediction == virginica)) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict(Classifier, [sepal_length-6.7, sepal_width-3.1, petal_length-5.6, petal_width-2.4], Prediction).

	% Test predict/4 with different options
	test(knn_predict_4_different_k, true(ground(Prediction))) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction, [k(5)]).

	test(knn_predict_4_different_metric, true(ground(Prediction))) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction, [distance_metric(manhattan)]).

	% Test predict_probabilities/3 with iris dataset
	test(knn_predict_probabilities_3_iris, true(check(list(pair(atom,probability)),Probabilities))) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict_probabilities(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Probabilities).

	test(knn_predict_probabilities_3_iris_setosa_prob, true(memberchk(setosa-_, Probabilities))) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict_probabilities(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Probabilities).

	% Test predict_probabilities/4 with different weighting schemes
	test(knn_predict_probabilities_4_uniform, true(check(list(pair(atom,probability)),Probabilities))) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict_probabilities(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Probabilities, [weight_scheme(uniform)]).

	test(knn_predict_probabilities_4_distance, true(check(list(pair(atom,probability)),Probabilities))) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict_probabilities(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Probabilities, [weight_scheme(distance)]).

	test(knn_predict_probabilities_4_gaussian, true(check(list(pair(atom,probability)),Probabilities))) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict_probabilities(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Probabilities, [weight_scheme(gaussian)]).

	% Test learn/2 with categorical weather dataset
	test(knn_learn_2_weather, true(ground(Classifier))) :-
		knn::learn(nb_weather_dataset, Classifier).

	% Test predict/3 with weather dataset (categorical features)
	test(knn_predict_3_weather_01, true(Prediction == yes)) :-
		knn::learn(nb_weather_dataset, Classifier),
		knn::predict(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	% Test learn/2 with mixed dataset (categorical + continuous)
	test(knn_learn_2_mixed, true(ground(Classifier))) :-
		knn::learn(nb_mixed_dataset, Classifier).

	% Test predict/3 with mixed dataset
	test(knn_predict_3_mixed_01, true(ground(Prediction))) :-
		knn::learn(nb_mixed_dataset, Classifier),
		knn::predict(Classifier, [age-26, income-36000, student-yes, credit_rating-fair], Prediction).

	% Test classifier_to_clauses/4 - verify exported clause works with predict/3
	test(knn_classifier_to_clauses_4, true(ground(Prediction))) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::classifier_to_clauses(_Dataset, Classifier, classify, [Clause]),
		% Extract classifier from the exported clause and use it
		Clause = classify(ExportedClassifier),
		knn::predict(ExportedClassifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction).

	% Test classifier_to_file/4 - verify exported classifier works with predict/3
	test(knn_classifier_to_file_4, true(ground(Prediction))) :-
		^^file_path('test_output.pl', File),
		knn::learn(nb_iris_dataset, Classifier),
		knn::classifier_to_file(_Dataset, Classifier, classify, File),
		logtalk_load(File),
		{classify(ExportedClassifier)},
		knn::predict(ExportedClassifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction).

	% Test print_classifier/1 (just ensure it doesn't fail)
	test(knn_print_classifier_1, true) :-
		^^suppress_text_output,
		knn::learn(nb_iris_dataset, Classifier),
		knn::print_classifier(Classifier).

	% Test different distance metrics
	test(knn_distance_metric_euclidean, true(ground(Prediction))) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction, [distance_metric(euclidean)]).

	test(knn_distance_metric_manhattan, true(ground(Prediction))) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction, [distance_metric(manhattan)]).

	test(knn_distance_metric_chebyshev, true(ground(Prediction))) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction, [distance_metric(chebyshev)]).

	test(knn_distance_metric_minkowski, true(ground(Prediction))) :-
		knn::learn(nb_iris_dataset, Classifier),
		knn::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction, [distance_metric(minkowski)]).

:- end_object.
