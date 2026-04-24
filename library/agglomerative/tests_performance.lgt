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
		comment is 'Performance and reference-fit benchmarks for the "agglomerative" library.'
	]).

	:- uses(lgtunit, [benchmark/2]).
	:- uses(list, [memberchk/2, nth1/3]).
	:- uses(numberlist, [euclidean_distance/3, manhattan_distance/3]).

	test(two_blobs_reference_fit, true, [note(metrics(train_seconds-TrainTime, max_covering_radius-CoveringRadius, min_prototype_distance-MinPrototypeDistance))]) :-
		reference_fit(two_blobs, [k(2), feature_scaling(off)], 0.35, 5.5, TrainTime, CoveringRadius, MinPrototypeDistance).

	test(iris_unlabeled_reference_fit, true, [note(metrics(train_seconds-TrainTime, max_covering_radius-CoveringRadius, min_prototype_distance-MinPrototypeDistance))]) :-
		reference_fit(iris_unlabeled, [k(3)], 1.5, 2.0, TrainTime, CoveringRadius, MinPrototypeDistance).

	reference_fit(Dataset, Options, MaximumCoveringRadius, MinimumPrototypeDistance, TrainTime, CoveringRadius, MinPrototypeDistance) :-
		benchmark(agglomerative::learn(Dataset, _Clusterer, Options), TrainTime),
		agglomerative::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Dataset, Clusterer, CoveringRadius, MinPrototypeDistance),
		CoveringRadius =< MaximumCoveringRadius,
		MinPrototypeDistance >= MinimumPrototypeDistance.

	clusterer_metrics(Dataset, agglomerative_clusterer(Encoders, _Clusters, Prototypes, Options), CoveringRadius, MinPrototypeDistance) :-
		findall(AttributeValues, Dataset::example(_, AttributeValues), Examples),
		covering_radius(Examples, agglomerative_clusterer(Encoders, [], Prototypes, Options), CoveringRadius),
		minimum_prototype_distance(Prototypes, MinPrototypeDistance).

	covering_radius([], _, 0.0).
	covering_radius([AttributeValues| Examples], Clusterer, CoveringRadius) :-
		Clusterer = agglomerative_clusterer(Encoders, _Clusters, Prototypes, Options),
		agglomerative::cluster(Clusterer, AttributeValues, Cluster),
		encode_instance(Encoders, AttributeValues, Features),
		nth1(Cluster, Prototypes, Prototype),
		distance(Options, Features, Prototype, ExampleRadius),
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

	minimum_prototype_distance([], 0.0).
	minimum_prototype_distance([_], 0.0).
	minimum_prototype_distance([Prototype| Prototypes], MinimumDistance) :-
		minimum_distance_to_others(Prototype, Prototypes, FirstMinimumDistance),
		minimum_prototype_distance(Prototypes, RestMinimumDistance),
		(   RestMinimumDistance =< 0.0 ->
			MinimumDistance = FirstMinimumDistance
		;   MinimumDistance is min(FirstMinimumDistance, RestMinimumDistance)
		).

	minimum_distance_to_others(Prototype, [Other| Others], MinimumDistance) :-
		squared_distance(Prototype, Other, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		minimum_distance_to_others(Prototype, Others, Distance, MinimumDistance).

	minimum_distance_to_others(_Prototype, [], MinimumDistance, MinimumDistance).
	minimum_distance_to_others(Prototype, [Other| Others], MinimumDistance0, MinimumDistance) :-
		squared_distance(Prototype, Other, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		MinimumDistance1 is min(MinimumDistance0, Distance),
		minimum_distance_to_others(Prototype, Others, MinimumDistance1, MinimumDistance).

:- end_object.
