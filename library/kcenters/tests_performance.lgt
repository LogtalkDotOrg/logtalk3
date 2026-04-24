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
		date is 2026-04-23,
		comment is 'Performance and reference-fit benchmarks for the "kcenters" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		memberchk/2, nth1/3
	]).

	:- uses(numberlist, [
		euclidean_distance/3, manhattan_distance/3
	]).

	test(two_blobs_reference_fit, true, [note(metrics(train_seconds-TrainTime, max_covering_radius-CoveringRadius, min_center_distance-MinCenterDistance))]) :-
		reference_fit(two_blobs, [k(2), initialization(spread), feature_scaling(off)], 0.7, 5.5, TrainTime, CoveringRadius, MinCenterDistance).

	test(iris_unlabeled_reference_fit, true, [note(metrics(train_seconds-TrainTime, max_covering_radius-CoveringRadius, min_center_distance-MinCenterDistance))]) :-
		reference_fit(iris_unlabeled, [k(3)], 2.5, 3.0, TrainTime, CoveringRadius, MinCenterDistance).

	reference_fit(Dataset, Options, MaximumCoveringRadius, MinimumCenterDistance, TrainTime, CoveringRadius, MinCenterDistance) :-
		benchmark(kcenters::learn(Dataset, _Clusterer, Options), TrainTime),
		kcenters::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Dataset, Clusterer, CoveringRadius, MinCenterDistance),
		CoveringRadius =< MaximumCoveringRadius,
		MinCenterDistance >= MinimumCenterDistance.

	clusterer_metrics(Dataset, kcenters_clusterer(Encoders, Centers, Options), CoveringRadius, MinCenterDistance) :-
		findall(
			AttributeValues,
			Dataset::example(_, AttributeValues),
			Examples
		),
		covering_radius(Examples, kcenters_clusterer(Encoders, Centers, Options), CoveringRadius),
		minimum_center_distance(Centers, MinCenterDistance).

	covering_radius([], _, 0.0).
	covering_radius([AttributeValues| Examples], Clusterer, CoveringRadius) :-
		Clusterer = kcenters_clusterer(Encoders, Centers, Options),
		kcenters::cluster(Clusterer, AttributeValues, Cluster),
		encode_instance(Encoders, AttributeValues, Features),
		nth1(Cluster, Centers, Center),
		distance(Options, Features, Center, ExampleRadius),
		covering_radius(Examples, Clusterer, RestRadius),
		CoveringRadius is max(ExampleRadius, RestRadius).

	encode_instance([], _, []).
	encode_instance([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature| Features]) :-
		memberchk(Attribute-Value, AttributeValues),
		Feature is (Value - Mean) / Scale,
		encode_instance(Encoders, AttributeValues, Features).

	distance(Options, Vector1, Vector2, Distance) :-
		memberchk(distance_metric(Metric), Options),
		named_distance(Metric, Vector1, Vector2, Distance).

	named_distance(euclidean, Vector1, Vector2, Distance) :-
		euclidean_distance(Vector1, Vector2, Distance).
	named_distance(manhattan, Vector1, Vector2, Distance) :-
		manhattan_distance(Vector1, Vector2, Distance).

	squared_distance([], [], 0.0).
	squared_distance([Value1| Values1], [Value2| Values2], DistanceSquared) :-
		squared_distance(Values1, Values2, RestDistanceSquared),
		Delta is Value1 - Value2,
		DistanceSquared is RestDistanceSquared + Delta * Delta.

	minimum_center_distance([], 0.0).
	minimum_center_distance([_], 0.0).
	minimum_center_distance([Center| Centers], MinimumDistance) :-
		minimum_distance_to_others(Center, Centers, FirstMinimumDistance),
		minimum_center_distance(Centers, RestMinimumDistance),
		(   RestMinimumDistance =< 0.0 ->
			MinimumDistance = FirstMinimumDistance
		;   MinimumDistance is min(FirstMinimumDistance, RestMinimumDistance)
		).

	minimum_distance_to_others(Center, [Other| Others], MinimumDistance) :-
		squared_distance(Center, Other, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		minimum_distance_to_others(Center, Others, Distance, MinimumDistance).

	minimum_distance_to_others(_Center, [], MinimumDistance, MinimumDistance).
	minimum_distance_to_others(Center, [Other| Others], MinimumDistance0, MinimumDistance) :-
		squared_distance(Center, Other, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		MinimumDistance1 is min(MinimumDistance0, Distance),
		minimum_distance_to_others(Center, Others, MinimumDistance1, MinimumDistance).

:- end_object.
