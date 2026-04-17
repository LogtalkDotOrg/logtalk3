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
		date is 2026-04-17,
		comment is 'Unit tests for the "logistic_regression" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(logistic_regression).

	cleanup :-
		^^clean_file('test_output.pl').

	test(logistic_regression_learn_2_weather, deterministic(ground(Classifier))) :-
		logistic_regression::learn(weather, Classifier).

	test(logistic_regression_predict_3_weather_yes, deterministic(Prediction == yes)) :-
		logistic_regression::learn(weather, Classifier),
		logistic_regression::predict(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(logistic_regression_predict_3_weather_no, deterministic(Prediction == no)) :-
		logistic_regression::learn(weather, Classifier),
		logistic_regression::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-strong], Prediction).

	test(logistic_regression_probability_3_weather, deterministic((ProbabilityYes > ProbabilityNo, Total =~= 1.0))) :-
		logistic_regression::learn(weather, Classifier),
		logistic_regression::predict_probabilities(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Probabilities),
		memberchk(yes-ProbabilityYes, Probabilities),
		memberchk(no-ProbabilityNo, Probabilities),
		Total is ProbabilityYes + ProbabilityNo.

	test(logistic_regression_learn_2_mixed, deterministic(ground(Classifier))) :-
		logistic_regression::learn(mixed, Classifier).

	test(logistic_regression_predict_3_mixed_no, deterministic(Prediction == no)) :-
		logistic_regression::learn(mixed, Classifier),
		logistic_regression::predict(Classifier, [age-26, income-36000, student-yes, credit_rating-fair], Prediction).

	test(logistic_regression_predict_3_mixed_yes, deterministic(Prediction == yes)) :-
		logistic_regression::learn(mixed, Classifier),
		logistic_regression::predict(Classifier, [age-45, income-75000, student-no, credit_rating-fair], Prediction).

	test(logistic_regression_learn_2_missing_mixed, deterministic(ground(Classifier))) :-
		logistic_regression::learn(missing_mixed, Classifier).

	test(logistic_regression_predict_3_missing_mixed_yes, deterministic(Prediction == yes)) :-
		logistic_regression::learn(missing_mixed, Classifier),
		logistic_regression::predict(Classifier, [age-_, income-70500, student-no, credit_rating-fair], Prediction).

	test(logistic_regression_predict_3_missing_mixed_no, deterministic(Prediction == no)) :-
		logistic_regression::learn(missing_mixed, Classifier),
		logistic_regression::predict(Classifier, [age-28, income-_, student-yes, credit_rating-excellent], Prediction).

	test(logistic_regression_predict_3_unseen_categorical_value, error(domain_error(attribute_value(credit_rating, [fair, excellent]), poor))) :-
		logistic_regression::learn(mixed, Classifier),
		logistic_regression::predict(Classifier, [age-40, income-50000, student-yes, credit_rating-poor], _Prediction).

	test(logistic_regression_learn_3_custom_options, deterministic((memberchk(learning_rate(0.05), Options), memberchk(maximum_iterations(1500), Options), memberchk(tolerance(1.0e-6), Options), memberchk(l2_regularization(0.01), Options)))) :-
		logistic_regression::learn(weather, lr_classifier(_Classes, _Encoders, _Models, Options), [learning_rate(0.05), maximum_iterations(1500), l2_regularization(0.01)]).

	test(logistic_regression_learn_2_iris_small, deterministic(ground(Classifier))) :-
		logistic_regression::learn(iris_small, Classifier).

	test(logistic_regression_predict_3_iris_setosa, deterministic(Prediction == setosa)) :-
		logistic_regression::learn(iris_small, Classifier),
		logistic_regression::predict(Classifier, [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2], Prediction).

	test(logistic_regression_predict_3_iris_versicolor, deterministic(Prediction == versicolor)) :-
		logistic_regression::learn(iris_small, Classifier),
		logistic_regression::predict(Classifier, [sepal_length-6.7, sepal_width-3.1, petal_length-4.7, petal_width-1.5], Prediction).

	test(logistic_regression_predict_3_iris_virginica, deterministic(Prediction == virginica)) :-
		logistic_regression::learn(iris_small, Classifier),
		logistic_regression::predict(Classifier, [sepal_length-6.4, sepal_width-3.0, petal_length-5.8, petal_width-2.2], Prediction).

	test(logistic_regression_probability_3_iris_small_sum, deterministic(Total =~= 1.0)) :-
		logistic_regression::learn(iris_small, Classifier),
		logistic_regression::predict_probabilities(Classifier, [sepal_length-6.4, sepal_width-3.0, petal_length-5.8, petal_width-2.2], Probabilities),
		sum_probabilities(Probabilities, 0.0, Total).

	test(logistic_regression_classifier_to_clauses_4, deterministic(Prediction == yes)) :-
		logistic_regression::learn(weather, Classifier),
		logistic_regression::classifier_to_clauses(_Dataset, Classifier, classify, [Clause]),
		logistic_regression::predict(Clause, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(logistic_regression_classifier_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		logistic_regression::learn(weather, Classifier),
		logistic_regression::classifier_to_file(weather, Classifier, classify, File).

	test(logistic_regression_classifier_to_file_4_loaded, deterministic(Prediction == yes)) :-
		^^file_path('test_output.pl', File),
		logistic_regression::learn(weather, Classifier),
		logistic_regression::classifier_to_file(_Dataset, Classifier, classify, File),
		logtalk_load(File),
		{classify(Classes, Encoders, Models, Options)},
		logistic_regression::predict(classify(Classes, Encoders, Models, Options), [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(logistic_regression_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		logistic_regression::learn(mixed, Classifier),
		logistic_regression::print_classifier(Classifier).

	sum_probabilities([], Total, Total).
	sum_probabilities([_-Probability| Probabilities], Total0, Total) :-
		Total1 is Total0 + Probability,
		sum_probabilities(Probabilities, Total1, Total).

:- end_object.
