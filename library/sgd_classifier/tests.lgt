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
		date is 2026-05-07,
		comment is 'Unit tests for the "sgd_classifier" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	cover(sgd_classifier).

	cleanup :-
		^^clean_file('test_output.pl').

	test(sgd_classifier_learn_2_weather, deterministic(ground(Classifier))) :-
		sgd_classifier::learn(weather, Classifier).

	test(sgd_classifier_valid_classifier_1, deterministic(sgd_classifier::valid_classifier(Classifier))) :-
		sgd_classifier::learn(weather, Classifier).

	test(sgd_classifier_predict_3_weather_yes, deterministic(Prediction == yes)) :-
		sgd_classifier::learn(weather, Classifier),
		sgd_classifier::predict(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(sgd_classifier_predict_3_weather_no, deterministic(Prediction == no)) :-
		sgd_classifier::learn(weather, Classifier),
		sgd_classifier::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-strong], Prediction).

	test(sgd_classifier_predict_probabilities_3_weather, deterministic((Total =~= 1.0, Yes > No))) :-
		sgd_classifier::learn(weather, Classifier),
		sgd_classifier::predict_probabilities(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Probabilities),
		memberchk(yes-Yes, Probabilities),
		memberchk(no-No, Probabilities),
		Total is Yes + No.

	test(sgd_classifier_learn_2_mixed, deterministic(ground(Classifier))) :-
		sgd_classifier::learn(mixed, Classifier).

	test(sgd_classifier_predict_3_mixed_yes, deterministic(Prediction == yes)) :-
		sgd_classifier::learn(mixed, Classifier),
		sgd_classifier::predict(Classifier, [age-45, income-75000, student-no, credit_rating-fair], Prediction).

	test(sgd_classifier_predict_3_mixed_no, deterministic(Prediction == no)) :-
		sgd_classifier::learn(mixed, Classifier),
		sgd_classifier::predict(Classifier, [age-26, income-36000, student-yes, credit_rating-fair], Prediction).

	test(sgd_classifier_learn_2_iris_small, deterministic(ground(Classifier))) :-
		sgd_classifier::learn(iris_small, Classifier).

	test(sgd_classifier_predict_3_iris_setosa, deterministic(Prediction == setosa)) :-
		sgd_classifier::learn(iris_small, Classifier),
		sgd_classifier::predict(Classifier, [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2], Prediction).

	test(sgd_classifier_predict_3_iris_virginica, deterministic(Prediction == virginica)) :-
		sgd_classifier::learn(iris_small, Classifier),
		sgd_classifier::predict(Classifier, [sepal_length-6.4, sepal_width-3.0, petal_length-5.8, petal_width-2.2], Prediction).

	test(sgd_classifier_learn_3_custom_options, deterministic([Loss, LearningRate, LearningSchedule, L2Regularization] == [hinge, 0.02, inverse_scaling(0.5), 0.001])) :-
		sgd_classifier::learn(weather, sgd_classifier(_Classes, _Encoders, _Loss, _Models, Options), [loss(hinge), learning_rate(0.02), learning_schedule(inverse_scaling(0.5)), l2_regularization(0.001)]),
		memberchk(loss(Loss), Options),
		memberchk(learning_rate(LearningRate), Options),
		memberchk(learning_schedule(LearningSchedule), Options),
		memberchk(l2_regularization(L2Regularization), Options).

	test(sgd_classifier_export_to_clauses_4, deterministic(Prediction == yes)) :-
		sgd_classifier::learn(weather, Classifier),
		sgd_classifier::export_to_clauses(weather, Classifier, classifier, [classifier(ExportedClassifier)]),
		sgd_classifier::predict(ExportedClassifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(sgd_classifier_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		sgd_classifier::learn(weather, Classifier),
		sgd_classifier::export_to_file(weather, Classifier, classify, File).

	test(sgd_classifier_export_to_file_4_loaded, deterministic(Prediction == yes)) :-
		^^file_path('test_output.pl', File),
		sgd_classifier::learn(weather, Classifier),
		sgd_classifier::export_to_file(weather, Classifier, classifier, File),
		logtalk_load(File),
		{classifier(LoadedClassifier)},
		sgd_classifier::predict(LoadedClassifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(sgd_classifier_diagnostics_2, deterministic((Model == sgd_classifier, Loss == log_loss, ground(Options)))) :-
		sgd_classifier::learn(weather, Classifier),
		sgd_classifier::diagnostics(Classifier, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(loss(Loss), Diagnostics),
		memberchk(options(Options), Diagnostics).

	test(sgd_classifier_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		sgd_classifier::learn(mixed, Classifier),
		sgd_classifier::print_classifier(Classifier).

:- end_object.
