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
		comment is 'Unit tests for the "naive_bayes" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(naive_bayes).

	cleanup :-
		^^clean_file('test_output.pl').

	% Test learn/2 with categorical weather dataset
	test(naive_bayes_learn_2_weather, true(ground(Classifier))) :-
		naive_bayes::learn(nb_weather_dataset, Classifier).

	% Test predict/3 with weather dataset
	test(naive_bayes_predict_3_weather_01, true(Prediction == yes)) :-
		naive_bayes::learn(nb_weather_dataset, Classifier),
		naive_bayes::predict(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(naive_bayes_predict_3_weather_02, true(Prediction == no)) :-
		naive_bayes::learn(nb_weather_dataset, Classifier),
		naive_bayes::predict(Classifier, [outlook-sunny, temperature-cool, humidity-high, wind-strong], Prediction).

	test(naive_bayes_predict_3_weather_03, true(Prediction == yes)) :-
		naive_bayes::learn(nb_weather_dataset, Classifier),
		naive_bayes::predict(Classifier, [outlook-rainy, temperature-mild, humidity-high, wind-weak], Prediction).

	% Test predict_probability/3 with weather dataset - strict float comparison
	test(naive_bayes_predict_probability_3_weather, true(ProbabilityNo =~= 0.015374453352769681)) :-
		naive_bayes::learn(nb_weather_dataset, Classifier),
		naive_bayes::predict_probabilities(Classifier, [outlook-rainy, temperature-mild, humidity-high, wind-weak], Probabilities),
		memberchk(no-ProbabilityNo, Probabilities).

	test(naive_bayes_predict_probability_3_weather_yes, true(ProbabilityYes =~= 0.02066115702479339)) :-
		naive_bayes::learn(nb_weather_dataset, Classifier),
		naive_bayes::predict_probabilities(Classifier, [outlook-rainy, temperature-mild, humidity-high, wind-weak], Probabilities),
		memberchk(yes-ProbabilityYes, Probabilities).

	% Test learn/2 with continuous iris dataset
	test(naive_bayes_learn_2_iris, true(ground(Classifier))) :-
		naive_bayes::learn(nb_iris_dataset, Classifier).

	% Test predict/3 with iris dataset (continuous features)
	test(naive_bayes_predict_3_iris_setosa, true(Prediction == setosa)) :-
		naive_bayes::learn(nb_iris_dataset, Classifier),
		naive_bayes::predict(Classifier, [sepal_length-5.0, sepal_width-3.3, petal_length-1.4, petal_width-0.2], Prediction).

	test(naive_bayes_predict_3_iris_versicolor, true(Prediction == versicolor)) :-
		naive_bayes::learn(nb_iris_dataset, Classifier),
		naive_bayes::predict(Classifier, [sepal_length-6.5, sepal_width-3.0, petal_length-4.6, petal_width-1.4], Prediction).

	test(naive_bayes_predict_3_iris_virginica, true(Prediction == virginica)) :-
		naive_bayes::learn(nb_iris_dataset, Classifier),
		naive_bayes::predict(Classifier, [sepal_length-6.7, sepal_width-3.1, petal_length-5.6, petal_width-2.4], Prediction).

	% Test predict_probability/3 with iris dataset - strict float comparison
	test(naive_bayes_predict_probability_3_iris_virginica, true(ProbabilityVirginica =~= 0.3209286115664264)) :-
		naive_bayes::learn(nb_iris_dataset, Classifier),
		naive_bayes::predict_probabilities(Classifier, [sepal_length-6.7, sepal_width-3.1, petal_length-5.6, petal_width-2.4], Probabilities),
		memberchk(virginica-ProbabilityVirginica, Probabilities).

	% Test learn/2 with mixed dataset (categorical + continuous)
	test(naive_bayes_learn_2_mixed, true(ground(Classifier))) :-
		naive_bayes::learn(nb_mixed_dataset, Classifier).

	% Test predict/3 with mixed dataset
	test(naive_bayes_predict_3_mixed_01, true(Prediction == no)) :-
		naive_bayes::learn(nb_mixed_dataset, Classifier),
		naive_bayes::predict(Classifier, [age-26, income-36000, student-yes, credit_rating-fair], Prediction).

	test(naive_bayes_predict_3_mixed_02, true(Prediction == yes)) :-
		naive_bayes::learn(nb_mixed_dataset, Classifier),
		naive_bayes::predict(Classifier, [age-48, income-80000, student-no, credit_rating-excellent], Prediction).

	% Test predict_probability/3 with mixed dataset - strict float comparison
	test(naive_bayes_predict_probability_3_mixed_no, true(ProbabilityNo =~= 6.856764584660943e-22)) :-
		naive_bayes::learn(nb_mixed_dataset, Classifier),
		naive_bayes::predict_probabilities(Classifier, [age-48, income-80000, student-no, credit_rating-excellent], Probabilities),
		memberchk(no-ProbabilityNo, Probabilities).

	test(naive_bayes_predict_probability_3_mixed_yes, true(ProbabilityYes =~= 2.942423271044002e-08)) :-
		naive_bayes::learn(nb_mixed_dataset, Classifier),
		naive_bayes::predict_probabilities(Classifier, [age-48, income-80000, student-no, credit_rating-excellent], Probabilities),
		memberchk(yes-ProbabilityYes, Probabilities).

	% Test classifier_to_clauses/4 - verify exported clause works with predict/3
	test(naive_bayes_classifier_to_clauses_3, true(Prediction == yes)) :-
		naive_bayes::learn(nb_weather_dataset, Classifier),
		naive_bayes::classifier_to_clauses(_Dataset, Classifier, classify, [Clause]),
		% Extract classifier from the exported clause and use it
		Clause = classify(ExportedClassifier),
		naive_bayes::predict(ExportedClassifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	% Test classifier_to_file/4 - verify exported clause works with predict/3
	test(naive_bayes_classifier_to_file_3, true(Prediction == yes)) :-
		^^file_path('test_output.pl', File),
		naive_bayes::learn(nb_weather_dataset, Classifier),
		naive_bayes::classifier_to_file(_Dataset, Classifier, classify, File),
		logtalk_load(File),
		{classify(ExportedClassifier)},
		naive_bayes::predict(ExportedClassifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	% Test print_classifier/1 (just ensure it doesn't fail)
	test(naive_bayes_print_classifier_1, true) :-
		^^suppress_text_output,
		naive_bayes::learn(nb_mixed_dataset, Classifier),
		naive_bayes::print_classifier(Classifier).

:- end_object.
