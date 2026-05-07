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
		comment is 'Unit tests for the "linear_svm_classifier" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(linear_svm_classifier).

	cleanup :-
		^^clean_file('test_output.pl').

	test(linear_svm_learn_2_weather, deterministic(ground(Classifier))) :-
		linear_svm_classifier::learn(weather, Classifier).

	test(linear_svm_valid_classifier_1, deterministic(linear_svm_classifier::valid_classifier(Classifier))) :-
		linear_svm_classifier::learn(weather, Classifier).

	test(linear_svm_invalid_classifier_1, fail) :-
		linear_svm_classifier::learn(weather, linear_svm_classifier(Classes, Encoders, _Models, Options)),
		linear_svm_classifier::valid_classifier(linear_svm_classifier(Classes, Encoders, [class_model(yes, 0.0, [0.0]), class_model(no, 0.0, [0.0])], Options)).

	test(linear_svm_predict_3_weather_yes, deterministic(Prediction == yes)) :-
		linear_svm_classifier::learn(weather, Classifier),
		linear_svm_classifier::predict(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(linear_svm_predict_3_weather_no, deterministic(Prediction == no)) :-
		linear_svm_classifier::learn(weather, Classifier),
		linear_svm_classifier::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-strong], Prediction).

	test(linear_svm_learn_2_mixed, deterministic(ground(Classifier))) :-
		linear_svm_classifier::learn(mixed, Classifier).

	test(linear_svm_predict_3_mixed_no, deterministic(Prediction == no)) :-
		linear_svm_classifier::learn(mixed, Classifier),
		linear_svm_classifier::predict(Classifier, [age-26, income-36000, student-yes, credit_rating-fair], Prediction).

	test(linear_svm_predict_3_mixed_yes, deterministic(Prediction == yes)) :-
		linear_svm_classifier::learn(mixed, Classifier),
		linear_svm_classifier::predict(Classifier, [age-45, income-75000, student-no, credit_rating-fair], Prediction).

	test(linear_svm_learn_2_missing_mixed, deterministic(ground(Classifier))) :-
		linear_svm_classifier::learn(missing_mixed, Classifier).

	test(linear_svm_predict_3_missing_mixed_yes, deterministic(Prediction == yes)) :-
		linear_svm_classifier::learn(missing_mixed, Classifier),
		linear_svm_classifier::predict(Classifier, [age-_, income-70500, student-no, credit_rating-fair], Prediction).

	test(linear_svm_predict_3_missing_mixed_no, deterministic(Prediction == no)) :-
		linear_svm_classifier::learn(missing_mixed, Classifier),
		linear_svm_classifier::predict(Classifier, [age-28, income-_, student-yes, credit_rating-excellent], Prediction).

	test(linear_svm_predict_3_unseen_categorical_value, error(domain_error(attribute_value(credit_rating, [fair, excellent]), poor))) :-
		linear_svm_classifier::learn(mixed, Classifier),
		linear_svm_classifier::predict(Classifier, [age-40, income-50000, student-yes, credit_rating-poor], _Prediction).

	test(linear_svm_learn_3_custom_options, deterministic([LearningRate, MaximumIterations, Tolerance, L2Regularization] == [0.05, 1500, 1.0e-6, 0.02])) :-
		linear_svm_classifier::learn(weather, linear_svm_classifier(_Classes, _Encoders, _Models, Options), [learning_rate(0.05), maximum_iterations(1500), l2_regularization(0.02)]),
		memberchk(learning_rate(LearningRate), Options),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(l2_regularization(L2Regularization), Options).

	test(linear_svm_learn_2_iris_small, deterministic(ground(Classifier))) :-
		linear_svm_classifier::learn(iris_small, Classifier).

	test(linear_svm_predict_3_iris_setosa, deterministic(Prediction == setosa)) :-
		linear_svm_classifier::learn(iris_small, Classifier),
		linear_svm_classifier::predict(Classifier, [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2], Prediction).

	test(linear_svm_predict_3_iris_versicolor, deterministic(Prediction == versicolor)) :-
		linear_svm_classifier::learn(iris_small, Classifier),
		linear_svm_classifier::predict(Classifier, [sepal_length-6.7, sepal_width-3.1, petal_length-4.7, petal_width-1.5], Prediction).

	test(linear_svm_predict_3_iris_virginica, deterministic(Prediction == virginica)) :-
		linear_svm_classifier::learn(iris_small, Classifier),
		linear_svm_classifier::predict(Classifier, [sepal_length-6.4, sepal_width-3.0, petal_length-5.8, petal_width-2.2], Prediction).

	test(linear_svm_export_to_clauses_4, deterministic(Prediction == yes)) :-
		linear_svm_classifier::learn(weather, Classifier),
		linear_svm_classifier::export_to_clauses(_Dataset, Classifier, classifier, [classifier(ExportedClassifier)]),
		linear_svm_classifier::predict(ExportedClassifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(linear_svm_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		linear_svm_classifier::learn(weather, Classifier),
		linear_svm_classifier::export_to_file(weather, Classifier, classify, File).

	test(linear_svm_export_to_file_4_loaded, deterministic(Prediction == yes)) :-
		^^file_path('test_output.pl', File),
		linear_svm_classifier::learn(weather, Classifier),
		linear_svm_classifier::export_to_file(weather, Classifier, classifier, File),
		logtalk_load(File),
		{classifier(LoadedClassifier)},
		linear_svm_classifier::predict(LoadedClassifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(linear_svm_diagnostics_2, deterministic((Model == linear_svm_classifier, Options1 == Options2))) :-
		linear_svm_classifier::learn(weather, Classifier),
		linear_svm_classifier::diagnostics(Classifier, Diagnostics),
		linear_svm_classifier::classifier_options(Classifier, Options1),
		memberchk(model(Model), Diagnostics),
		memberchk(options(Options2), Diagnostics).

	test(linear_svm_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		linear_svm_classifier::learn(mixed, Classifier),
		linear_svm_classifier::print_classifier(Classifier).

:- end_object.
