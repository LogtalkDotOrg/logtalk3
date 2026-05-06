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
		comment is 'Performance and reference-fit benchmarks for the "logistic_regression_classifier" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2,
		memberchk/2
	]).

	% Reference benchmark: small categorical dataset
	test(weather_reference_fit, true, [note(metrics(train_seconds-TrainTime, training_accuracy-Accuracy, mean_log_loss-LogLoss))]) :-
		reference_fit(weather, 0.70, 0.60, TrainTime, Accuracy, LogLoss).

	% Reference benchmark: mixed continuous/categorical dataset
	test(mixed_reference_fit, true, [note(metrics(train_seconds-TrainTime, training_accuracy-Accuracy, mean_log_loss-LogLoss))]) :-
		reference_fit(mixed, 0.80, 0.45, TrainTime, Accuracy, LogLoss).

	% Reference benchmark: small multiclass continuous dataset
	test(iris_small_reference_fit, true, [note(metrics(train_seconds-TrainTime, training_accuracy-Accuracy, mean_log_loss-LogLoss))]) :-
		reference_fit(iris_small, 0.95, 0.30, TrainTime, Accuracy, LogLoss).

	% Reference benchmark: explicit missing-value handling dataset
	test(missing_mixed_reference_fit, true, [note(metrics(train_seconds-TrainTime, training_accuracy-Accuracy, mean_log_loss-LogLoss))]) :-
		reference_fit(missing_mixed, 0.90, 0.35, TrainTime, Accuracy, LogLoss).

	% Reference benchmark: larger real-world dataset with missing categorical values
	test(breast_cancer_reference_fit, true, [note(metrics(train_seconds-TrainTime, training_accuracy-Accuracy, mean_log_loss-LogLoss))]) :-
		reference_fit(breast_cancer, 0.70, 0.60, TrainTime, Accuracy, LogLoss).

	reference_fit(Dataset, MinimumAccuracy, MaximumLogLoss, TrainTime, Accuracy, LogLoss) :-
		benchmark(logistic_regression_classifier::learn(Dataset, _), TrainTime),
		logistic_regression_classifier::learn(Dataset, Classifier),
		dataset_metrics(Dataset, Classifier, Accuracy, LogLoss),
		Accuracy >= MinimumAccuracy,
		LogLoss =< MaximumLogLoss.

	dataset_metrics(Dataset, Classifier, Accuracy, LogLoss) :-
		findall(
			Class-AttributeValues,
			Dataset::example(_, Class, AttributeValues),
			Examples
		),
		length(Examples, Count),
		evaluate_examples(Examples, Classifier, 0, Correct, 0.0, TotalLogLoss),
		Accuracy is Correct / Count,
		LogLoss is TotalLogLoss / Count.

	evaluate_examples([], _, Correct, Correct, TotalLogLoss, TotalLogLoss).
	evaluate_examples([Class-AttributeValues| Examples], Classifier, Correct0, Correct, LogLoss0, LogLoss) :-
		logistic_regression_classifier::predict(Classifier, AttributeValues, Prediction),
		(   Prediction == Class ->
			Correct1 is Correct0 + 1
		;   Correct1 = Correct0
		),
		logistic_regression_classifier::predict_probabilities(Classifier, AttributeValues, Probabilities),
		memberchk(Class-Probability, Probabilities),
		bounded_probability(Probability, SafeProbability),
		ExampleLogLoss is -log(SafeProbability),
		LogLoss1 is LogLoss0 + ExampleLogLoss,
		evaluate_examples(Examples, Classifier, Correct1, Correct, LogLoss1, LogLoss).

	bounded_probability(Probability, SafeProbability) :-
		(   Probability < 1.0e-15 ->
			SafeProbability = 1.0e-15
		;   SafeProbability = Probability
		).

:- end_object.
