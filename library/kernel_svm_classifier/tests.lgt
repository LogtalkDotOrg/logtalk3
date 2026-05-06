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


:- object(xor_dataset,
	implements(dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).

	class(label).

	class_values([negative, positive]).

	example(1, positive, [x1-0.0, x2-0.0]).
	example(2, negative, [x1-0.0, x2-1.0]).
	example(3, negative, [x1-1.0, x2-0.0]).
	example(4, positive, [x1-1.0, x2-1.0]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Unit tests for the "kernel_svm_classifier" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	cover(kernel_svm_classifier).

	cleanup :-
		^^clean_file('test_output.pl').

	test(kernel_svm_learn_2_weather, deterministic(ground(Classifier))) :-
		kernel_svm_classifier::learn(weather, Classifier).

	test(kernel_svm_valid_classifier_1, deterministic(kernel_svm_classifier::valid_classifier(Classifier))) :-
		kernel_svm_classifier::learn(weather, Classifier).

	test(kernel_svm_predict_3_weather_yes, deterministic(Prediction == yes)) :-
		kernel_svm_classifier::learn(weather, Classifier),
		kernel_svm_classifier::predict(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(kernel_svm_predict_3_iris_setosa, deterministic(Prediction == setosa)) :-
		kernel_svm_classifier::learn(iris_small, Classifier, [kernel(rbf(0.5))]),
		kernel_svm_classifier::predict(Classifier, [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2], Prediction).

	test(kernel_svm_predict_probabilities_3_weather, deterministic((Total =~= 1.0, Yes > No))) :-
		kernel_svm_classifier::learn(weather, Classifier),
		kernel_svm_classifier::predict_probabilities(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Probabilities),
		memberchk(yes-Yes, Probabilities),
		memberchk(no-No, Probabilities),
		Total is Yes + No.

	test(kernel_svm_learn_3_custom_options, deterministic((memberchk(kernel(polynomial(2, 1.0, 0.0)), Options), memberchk(learning_rate(0.2), Options), memberchk(maximum_iterations(40), Options)))) :-
		kernel_svm_classifier::learn(weather, kernel_svm_classifier(_Classes, _Encoders, _Kernel, _TrainingRows, _Models, Options), [kernel(polynomial(2, 1.0, 0.0)), learning_rate(0.2), maximum_iterations(40)]).

	test(kernel_svm_rbf_xor_positive, deterministic(Prediction == positive)) :-
		kernel_svm_classifier::learn(xor_dataset, Classifier, [kernel(rbf(2.0)), learning_rate(1.0), maximum_iterations(40), l2_regularization(0.0), feature_scaling(false)]),
		kernel_svm_classifier::predict(Classifier, [x1-0.0, x2-0.0], Prediction).

	test(kernel_svm_rbf_xor_negative, deterministic(Prediction == negative)) :-
		kernel_svm_classifier::learn(xor_dataset, Classifier, [kernel(rbf(2.0)), learning_rate(1.0), maximum_iterations(40), l2_regularization(0.0), feature_scaling(false)]),
		kernel_svm_classifier::predict(Classifier, [x1-0.0, x2-1.0], Prediction).

	test(kernel_svm_export_to_clauses_4, deterministic(Prediction == yes)) :-
		kernel_svm_classifier::learn(weather, Classifier),
		kernel_svm_classifier::export_to_clauses(weather, Classifier, classifier, [classifier(ExportedClassifier)]),
		kernel_svm_classifier::predict(ExportedClassifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(kernel_svm_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		kernel_svm_classifier::learn(weather, Classifier),
		kernel_svm_classifier::export_to_file(weather, Classifier, classify, File).

	test(kernel_svm_export_to_file_4_loaded, deterministic(Prediction == yes)) :-
		^^file_path('test_output.pl', File),
		kernel_svm_classifier::learn(weather, Classifier),
		kernel_svm_classifier::export_to_file(weather, Classifier, classifier, File),
		logtalk_load(File),
		{classifier(LoadedClassifier)},
		kernel_svm_classifier::predict(LoadedClassifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(kernel_svm_diagnostics_2, deterministic((memberchk(model(kernel_svm_classifier), Diagnostics), memberchk(kernel(linear), Diagnostics), memberchk(options(_), Diagnostics)))) :-
		kernel_svm_classifier::learn(weather, Classifier),
		kernel_svm_classifier::diagnostics(Classifier, Diagnostics).

	test(kernel_svm_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		kernel_svm_classifier::learn(weather, Classifier),
		kernel_svm_classifier::print_classifier(Classifier).

:- end_object.
