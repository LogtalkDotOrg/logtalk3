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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-16,
		comment is 'Unit tests for the "gradient_boosting_classifier" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- private(dataset_classifier_/3).
	:- dynamic(dataset_classifier_/3).

	cover(gradient_boosting_classifier).

	setup :-
		retractall(dataset_classifier_(_, _, _)).

	cleanup :-
		retractall(dataset_classifier_(_, _, _)),
		^^clean_file('test_output.pl').

	test(gradient_boosting_learn_2_weather, deterministic(ground(Classifier))) :-
		gradient_boosting_classifier::learn(weather, Classifier),
		retractall(dataset_classifier_(weather, [], _)),
		assertz(dataset_classifier_(weather, [], Classifier)).

	test(gradient_boosting_valid_classifier_1, deterministic(gradient_boosting_classifier::valid_classifier(Classifier))) :-
		dataset_classifier(weather, [], Classifier).

	test(gradient_boosting_predict_3_weather_yes, deterministic(Prediction == yes)) :-
		dataset_classifier(weather, [number_of_estimators(10), learning_rate(0.5), maximum_depth(2)], Classifier),
		gradient_boosting_classifier::predict(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(gradient_boosting_predict_3_weather_no, deterministic(Prediction == no)) :-
		dataset_classifier(weather, [number_of_estimators(10), learning_rate(0.5), maximum_depth(2)], Classifier),
		gradient_boosting_classifier::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-strong], Prediction).

	test(gradient_boosting_predict_3_iris_setosa, deterministic(Prediction == setosa)) :-
		dataset_classifier(iris_small, [number_of_estimators(8), learning_rate(0.3), maximum_depth(2)], Classifier),
		gradient_boosting_classifier::predict(Classifier, [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2], Prediction).

	test(gradient_boosting_predict_3_mixed_yes, deterministic(Prediction == yes)) :-
		dataset_classifier(mixed, [number_of_estimators(12), learning_rate(0.4), maximum_depth(2)], Classifier),
		gradient_boosting_classifier::predict(Classifier, [age-45, income-75000, student-no, credit_rating-fair], Prediction).

	test(gradient_boosting_predict_probabilities_3_weather, deterministic((Total =~= 1.0, Yes > No))) :-
		dataset_classifier(weather, [number_of_estimators(10), learning_rate(0.5), maximum_depth(2)], Classifier),
		gradient_boosting_classifier::predict_probabilities(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Probabilities),
		memberchk(yes-Yes, Probabilities),
		memberchk(no-No, Probabilities),
		Total is Yes + No.

	test(gradient_boosting_learn_3_custom_options, deterministic([NumberOfEstimators, LearningRate, MaximumDepth] == [5, 0.3, 2])) :-
		gradient_boosting_classifier::learn(weather, gradient_boosting_classifier(_Classes, _InitialScores, _StageTrees, Options), [number_of_estimators(5), learning_rate(0.3), maximum_depth(2)]),
		memberchk(number_of_estimators(NumberOfEstimators), Options),
		memberchk(learning_rate(LearningRate), Options),
		memberchk(maximum_depth(MaximumDepth), Options).

	test(gradient_boosting_export_to_clauses_4, deterministic(Prediction == yes)) :-
		dataset_classifier(weather, [number_of_estimators(10), learning_rate(0.5), maximum_depth(2)], Classifier),
		gradient_boosting_classifier::export_to_clauses(weather, Classifier, classifier, [classifier(ExportedClassifier)]),
		gradient_boosting_classifier::predict(ExportedClassifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(gradient_boosting_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		dataset_classifier(weather, [], Classifier),
		gradient_boosting_classifier::export_to_file(weather, Classifier, classify, File).

	test(gradient_boosting_export_to_file_4_loaded, deterministic(Prediction == yes)) :-
		^^file_path('test_output.pl', File),
		dataset_classifier(weather, [number_of_estimators(10), learning_rate(0.5), maximum_depth(2)], Classifier),
		gradient_boosting_classifier::export_to_file(weather, Classifier, classifier, File),
		logtalk_load(File),
		{classifier(LoadedClassifier)},
		gradient_boosting_classifier::predict(LoadedClassifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(gradient_boosting_diagnostics_2, deterministic(StageCount >= 0)) :-
		dataset_classifier(weather, [], Classifier),
		gradient_boosting_classifier::diagnostics(Classifier, Diagnostics),
		memberchk(model(gradient_boosting_classifier), Diagnostics),
		memberchk(stage_count(StageCount), Diagnostics).

	test(gradient_boosting_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		dataset_classifier(weather, [], Classifier),
		gradient_boosting_classifier::print_classifier(Classifier).

	% auxiliary predicates

	dataset_classifier(Dataset, Options, Classifier) :-
		dataset_classifier_(Dataset, Options, Classifier),
		!.
	dataset_classifier(Dataset, Options, Classifier) :-
		gradient_boosting_classifier::learn(Dataset, Classifier, Options),
		assertz(dataset_classifier_(Dataset, Options, Classifier)).

:- end_object.
