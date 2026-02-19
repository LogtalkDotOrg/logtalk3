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
		date is 2026-02-19,
		comment is 'Unit tests for the "nearest_centroid" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(type, [
		check/2
	]).

	cover(nearest_centroid).

	cleanup :-
		^^clean_file('test_output.pl').

	% Test learn/2 with continuous iris dataset
	test(nearest_centroid_learn_2_iris, true(ground(Classifier))) :-
		nearest_centroid::learn(iris_small, Classifier).

	% Test predict/3 with iris dataset (continuous features)
	test(nearest_centroid_predict_3_iris_setosa, true(Prediction == setosa)) :-
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction).

	test(nearest_centroid_predict_3_iris_versicolor, true(Prediction == versicolor)) :-
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::predict(Classifier, [sepal_length-6.5, sepal_width-3.0, petal_length-4.6, petal_width-1.4], Prediction).

	test(nearest_centroid_predict_3_iris_virginica, true(Prediction == virginica)) :-
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::predict(Classifier, [sepal_length-6.7, sepal_width-3.1, petal_length-5.6, petal_width-2.4], Prediction).

	% Test predict/4 with different options
	test(nearest_centroid_predict_4_different_k, true(ground(Prediction))) :-
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction, [distance_metric(cosine)]).

	test(nearest_centroid_predict_4_different_metric, true(ground(Prediction))) :-
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction, [distance_metric(manhattan)]).

	% Test predict_probabilities/3 with iris dataset
	test(nearest_centroid_predict_probabilities_3_iris, true(check(list(pair(atom,probability)),Probabilities))) :-
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::predict_probabilities(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Probabilities).

	test(nearest_centroid_predict_probabilities_3_iris_setosa_prob, true(memberchk(setosa-_, Probabilities))) :-
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::predict_probabilities(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Probabilities).

	% Test learn/2 with categorical weather dataset
	test(nearest_centroid_learn_2_weather, true(ground(Classifier))) :-
		nearest_centroid::learn(weather, Classifier).

	% Test predict/3 with weather dataset (categorical features)
	test(nearest_centroid_predict_3_weather_01, true(Prediction == yes)) :-
		nearest_centroid::learn(weather, Classifier),
		nearest_centroid::predict(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	% Test learn/2 with mixed dataset (categorical + continuous)
	test(nearest_centroid_learn_2_mixed, true(ground(Classifier))) :-
		nearest_centroid::learn(mixed, Classifier).

	% Test predict/3 with mixed dataset
	test(nearest_centroid_predict_3_mixed_01, true(ground(Prediction))) :-
		nearest_centroid::learn(mixed, Classifier),
		nearest_centroid::predict(Classifier, [age-26, income-36000, student-yes, credit_rating-fair], Prediction).

	% Test classifier_to_clauses/4 - verify exported clause works with predict/3
	test(nearest_centroid_classifier_to_clauses_4, true(ground(Prediction))) :-
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::classifier_to_clauses(_Dataset, Classifier, classify, [ExportedClassifier]),
		% Extract classifier from the exported clause and use it
		nearest_centroid::predict(ExportedClassifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction).

	% Test classifier_to_file/4 - verify file is written
	test(nearest_centroid_classifier_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::classifier_to_file(iris_small, Classifier, classify, File).

	% Test classifier_to_file/4 - verify exported classifier works with predict/3
	test(nearest_centroid_classifier_to_file_4_loaded, true(ground(Prediction))) :-
		^^file_path('test_output.pl', File),
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::classifier_to_file(_Dataset, Classifier, classify, File),
		logtalk_load(File),
		{classify(AttributeNames, FeatureTypes, Centroids)},
		nearest_centroid::predict(classify(AttributeNames, FeatureTypes, Centroids), [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction).

	% Test print_classifier/1 (just ensure it doesn't fail)
	test(nearest_centroid_print_classifier_1, true) :-
		^^suppress_text_output,
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::print_classifier(Classifier).

	% Test different distance metrics
	test(nearest_centroid_distance_metric_euclidean, true(ground(Prediction))) :-
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction, [distance_metric(euclidean)]).

	test(nearest_centroid_distance_metric_manhattan, true(ground(Prediction))) :-
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction, [distance_metric(manhattan)]).

	test(nearest_centroid_distance_metric_chebyshev, true(ground(Prediction))) :-
		nearest_centroid::learn(iris_small, Classifier),
		nearest_centroid::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction, [distance_metric(cosine)]).

:- end_object.
