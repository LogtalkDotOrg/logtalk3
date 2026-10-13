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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-13,
		comment is 'Performance and reference-fit benchmarks for the "linear_svm_classifier" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1, benchmark/2, benchmark/3
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(population, [
		arithmetic_mean/2, median/2, variance/2
	]).

	:- meta_predicate(benchmark_samples(0, *, *)).

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

	test(linear_svm_classifier_high_cardinality_32, true, [note(metrics(encoded_features-Features, nonzero_density-Density, median_train_seconds-Time, training_accuracy-Accuracy))]) :-
		high_cardinality_fit(32, Features, Density, Time, Accuracy).

	test(linear_svm_classifier_high_cardinality_128, true, [note(metrics(encoded_features-Features, nonzero_density-Density, median_train_seconds-Time, training_accuracy-Accuracy))]) :-
		high_cardinality_fit(128, Features, Density, Time, Accuracy).

	test(linear_svm_classifier_encoding_benchmark, true, [note(metrics(repetitions-10000, median_encode_seconds-Time))]) :-
		svm_high_cardinality(128, 256)::attribute_values(color, Values),
		Encoders = [categorical(color, Values), categorical(shape, Values)],
		linear_svm_classifier << encode_instance(Encoders, [color-64, shape-96], Encoded),
		length(Encoded, 258),
		benchmark_samples(linear_svm_classifier << encode_instance(Encoders, [color-64, shape-96], _), 10000, Time).

	test(linear_svm_classifier_prediction_benchmark, true, [note(metrics(repetitions-10000, median_predict_seconds-Time))]) :-
		linear_svm_classifier::learn(iris_small, Classifier),
		Instance = [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2],
		linear_svm_classifier::predict(Classifier, Instance, setosa),
		benchmark_samples(linear_svm_classifier::predict(Classifier, Instance, _), 10000, Time).

	test(linear_svm_classifier_preprocessing_benchmark, true, [note(metrics(rows-2048, repetitions-200, median_reference_seconds-ReferenceTime, median_preprocessing_seconds-Time))]) :-
		findall(Id-Class-[x-Value], svm_high_cardinality(17, 2048)::example(Id, Class, [color-Value, shape-_]), Examples),
		reference_statistics(Examples, Mean, Scale),
		linear_svm_classifier << continuous_stats(x, Examples, ActualMean, ActualScale),
		assertion([ActualMean, ActualScale] =~= [Mean, Scale]),
		benchmark_samples(reference_statistics(Examples, _, _), 200, ReferenceTime),
		benchmark_samples(linear_svm_classifier << continuous_stats(x, Examples, _, _), 200, Time).

	% auxiliary predicates

	benchmark_samples(Goal, Repetitions, Time) :-
		copy_term(Goal, Warmup),
		once(Warmup),
		findall(
			Sample,
			(	member(_, [1, 2, 3, 4, 5]),
				benchmark(Goal, Repetitions, Sample)
			),
			Samples
		),
		median(Samples, Time).

	high_cardinality_fit(Width, Features, Density, Time, Accuracy) :-
		Dataset = svm_high_cardinality(Width, 256),
		Options = [maximum_iterations(20), tolerance(0)],
		linear_svm_classifier::learn(Dataset, Classifier, Options),
		dataset_accuracy(Dataset, Classifier, Accuracy),
		Accuracy >= 0.95,
		linear_svm_classifier::diagnostics(Classifier, Diagnostics),
		memberchk(encoded_features(Features), Diagnostics),
		Density is 2.0 / Features,
		benchmark_samples(linear_svm_classifier::learn(Dataset, _, Options), 1, Time).

	reference_statistics(Examples, Mean, Scale) :-
		reference_values(Examples, x, Values),
		arithmetic_mean(Values, Mean),
		length(Values, Count),
		(	Count > 1 ->
			variance(Values, Variance)
		;	Variance = 0.0
		),
		(	Variance > 0.0 ->
			Scale is sqrt(Variance)
		;	Scale = 1.0
		).

	reference_values([], _, []).
	reference_values([_-_-Instance| Examples], Attribute, Values) :-
		(	memberchk(Attribute-Value, Instance),
			nonvar(Value) ->
			Values = [Value| Rest]
		;	Values = Rest
		),
		reference_values(Examples, Attribute, Rest).

	reference_fit(Dataset, MinimumAccuracy, TrainTime, Accuracy) :-
		benchmark(linear_svm_classifier::learn(Dataset, Classifier), TrainTime),
		ground(Classifier),
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
