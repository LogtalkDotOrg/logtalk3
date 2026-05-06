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
		comment is 'Performance and reference-fit benchmarks for the "kmedians_clusterer" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2, memberchk/2, nth1/3
	]).

	:- uses(numberlist, [
		manhattan_distance/3
	]).

	test(two_blobs_reference_fit, true, [note(metrics(train_seconds-TrainTime, inertia-Inertia, min_median_distance-MinMedianDistance))]) :-
		reference_fit(two_blobs, [k(2), initialization(spread), feature_scaling(off)], 2.5, 8.0, TrainTime, Inertia, MinMedianDistance).

	test(iris_unlabeled_reference_fit, true, [note(metrics(train_seconds-TrainTime, inertia-Inertia, min_median_distance-MinMedianDistance))]) :-
		reference_fit(iris_unlabeled, [k(3)], 10.5, 4.0, TrainTime, Inertia, MinMedianDistance).

	reference_fit(Dataset, Options, MaximumInertia, MinimumMedianDistance, TrainTime, Inertia, MinMedianDistance) :-
		benchmark(kmedians_clusterer::learn(Dataset, _Clusterer, Options), TrainTime),
		kmedians_clusterer::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Dataset, Clusterer, Inertia, MinMedianDistance),
		Inertia =< MaximumInertia,
		MinMedianDistance >= MinimumMedianDistance.

	clusterer_metrics(Dataset, kmedians_clusterer(Encoders, Medians, Options, _Diagnostics), Inertia, MinMedianDistance) :-
		findall(
			AttributeValues,
			Dataset::example(_, AttributeValues),
			Examples
		),
		training_inertia(Examples, kmedians_clusterer(Encoders, Medians, Options, _Diagnostics), Inertia),
		minimum_median_distance(Medians, MinMedianDistance).

	training_inertia([], _, 0.0).
	training_inertia([AttributeValues| Examples], Clusterer, Inertia) :-
		Clusterer = kmedians_clusterer(Encoders, Medians, _Options, _Diagnostics),
		kmedians_clusterer::cluster(Clusterer, AttributeValues, Cluster),
		encode_instance(Encoders, AttributeValues, Features),
		nth1(Cluster, Medians, Median),
		manhattan_distance(Features, Median, ExampleInertia),
		training_inertia(Examples, Clusterer, RestInertia),
		Inertia is ExampleInertia + RestInertia.

	encode_instance([], _, []).
	encode_instance([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature| Features]) :-
		memberchk(Attribute-Value, AttributeValues),
		Feature is (Value - Mean) / Scale,
		encode_instance(Encoders, AttributeValues, Features).

	minimum_median_distance([], 0.0).
	minimum_median_distance([_], 0.0).
	minimum_median_distance([Median| Medians], MinimumDistance) :-
		minimum_distance_to_others(Median, Medians, FirstMinimumDistance),
		minimum_median_distance(Medians, RestMinimumDistance),
		(   RestMinimumDistance =< 0.0 ->
			MinimumDistance = FirstMinimumDistance
		;   MinimumDistance is min(FirstMinimumDistance, RestMinimumDistance)
		).

	minimum_distance_to_others(Median, [Other| Others], MinimumDistance) :-
		manhattan_distance(Median, Other, Distance),
		minimum_distance_to_others(Median, Others, Distance, MinimumDistance).

	minimum_distance_to_others(_Median, [], MinimumDistance, MinimumDistance).
	minimum_distance_to_others(Median, [Other| Others], MinimumDistance0, MinimumDistance) :-
		manhattan_distance(Median, Other, Distance),
		MinimumDistance1 is min(MinimumDistance0, Distance),
		minimum_distance_to_others(Median, Others, MinimumDistance1, MinimumDistance).

:- end_object.
