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
		date is 2026-05-06,
		comment is 'Unit tests for the "qda_classifier" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(qda_classifier).

	cleanup :-
		^^clean_file('test_output.pl').

	test(qda_learn_2_iris_small, deterministic(ground(Classifier))) :-
		qda_classifier::learn(iris_small, Classifier).

	test(qda_valid_classifier_1, deterministic(qda_classifier::valid_classifier(Classifier))) :-
		qda_classifier::learn(iris_small, Classifier).

	test(qda_predict_3_setosa, deterministic(Prediction == setosa)) :-
		qda_classifier::learn(iris_small, Classifier),
		qda_classifier::predict(Classifier, [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2], Prediction).

	test(qda_predict_3_virginica, deterministic(Prediction == virginica)) :-
		qda_classifier::learn(iris_small, Classifier),
		qda_classifier::predict(Classifier, [sepal_length-6.4, sepal_width-3.0, petal_length-5.8, petal_width-2.2], Prediction).

	test(qda_predict_scores_3, deterministic(memberchk(setosa-_, Scores))) :-
		qda_classifier::learn(iris_small, Classifier),
		qda_classifier::predict_scores(Classifier, [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2], Scores).

	test(qda_learn_3_custom_options, deterministic((memberchk(feature_scaling(false), Options), memberchk(regularization(1.0e-4), Options)))) :-
		qda_classifier::learn(iris_small, qda_classifier(_Encoders, _Models, Options), [feature_scaling(false), regularization(1.0e-4)]).

	test(qda_export_to_clauses_4, deterministic(Prediction == setosa)) :-
		qda_classifier::learn(iris_small, Classifier),
		qda_classifier::export_to_clauses(iris_small, Classifier, classifier, [classifier(ExportedClassifier)]),
		qda_classifier::predict(ExportedClassifier, [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2], Prediction).

	test(qda_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		qda_classifier::learn(iris_small, Classifier),
		qda_classifier::export_to_file(iris_small, Classifier, classify, File).

	test(qda_export_to_file_4_loaded, deterministic(Prediction == setosa)) :-
		^^file_path('test_output.pl', File),
		qda_classifier::learn(iris_small, Classifier),
		qda_classifier::export_to_file(iris_small, Classifier, classifier, File),
		logtalk_load(File),
		{classifier(LoadedClassifier)},
		qda_classifier::predict(LoadedClassifier, [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2], Prediction).

	test(qda_diagnostics_2, deterministic((memberchk(model(qda_classifier), Diagnostics), memberchk(options(_), Diagnostics)))) :-
		qda_classifier::learn(iris_small, Classifier),
		qda_classifier::diagnostics(Classifier, Diagnostics).

	test(qda_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		qda_classifier::learn(iris_small, Classifier),
		qda_classifier::print_classifier(Classifier).

	test(qda_learn_2_invalid_dataset, error(domain_error(continuous_attribute, outlook))) :-
		qda_classifier::learn(weather, _Classifier).

:- end_object.