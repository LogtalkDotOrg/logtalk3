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


:- object(tests_performance,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Performance and reference-fit benchmarks for the "linear_svm_classifier" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2
	]).

	% Reference benchmark: small categorical dataset
	test(weather_reference_fit, true, [note(metrics(train_seconds-TrainTime, training_accuracy-Accuracy))]) :-
		reference_fit(weather, 0.80, TrainTime, Accuracy).

	% Reference benchmark: mixed continuous/categorical dataset
	test(mixed_reference_fit, true, [note(metrics(train_seconds-TrainTime, training_accuracy-Accuracy))]) :-
		reference_fit(mixed, 0.95, TrainTime, Accuracy).

	% Reference benchmark: small multiclass continuous dataset
	test(iris_small_reference_fit, true, [note(metrics(train_seconds-TrainTime, training_accuracy-Accuracy))]) :-
		reference_fit(iris_small, 0.95, TrainTime, Accuracy).

	% Reference benchmark: explicit missing-value handling dataset
	test(missing_mixed_reference_fit, true, [note(metrics(train_seconds-TrainTime, training_accuracy-Accuracy))]) :-
		reference_fit(missing_mixed, 0.95, TrainTime, Accuracy).

	% Reference benchmark: larger real-world dataset with missing categorical values
	test(breast_cancer_reference_fit, true, [note(metrics(train_seconds-TrainTime, training_accuracy-Accuracy))]) :-
		reference_fit(breast_cancer, 0.70, TrainTime, Accuracy).

	reference_fit(Dataset, MinimumAccuracy, TrainTime, Accuracy) :-
		benchmark(linear_svm_classifier::learn(Dataset, _), TrainTime),
		linear_svm_classifier::learn(Dataset, Classifier),
		dataset_accuracy(Dataset, Classifier, Accuracy),
		Accuracy >= MinimumAccuracy.

	dataset_accuracy(Dataset, Classifier, Accuracy) :-
		findall(
			Class-AttributeValues,
			Dataset::example(_, Class, AttributeValues),
			Examples
		),
		length(Examples, Count),
		evaluate_examples(Examples, Classifier, 0, Correct),
		Accuracy is Correct / Count.

	evaluate_examples([], _Classifier, Correct, Correct).
	evaluate_examples([Class-AttributeValues| Examples], Classifier, Correct0, Correct) :-
		linear_svm_classifier::predict(Classifier, AttributeValues, Prediction),
		(	Prediction == Class ->
			Correct1 is Correct0 + 1
		;	Correct1 = Correct0
		),
		evaluate_examples(Examples, Classifier, Correct1, Correct).

:- end_object.
