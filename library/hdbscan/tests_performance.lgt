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
		comment is 'Performance and reference-fit benchmarks for the "hdbscan" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2
	]).

	test(two_blobs_reference_fit, true, [note(metrics(train_seconds-TrainTime, cluster_count-ClusterCount, noise_count-NoiseCount, min_prototype_distance-MinPrototypeDistance, max_radius-MaxRadius))]) :-
		reference_fit(two_blobs, [minimum_points(2), minimum_cluster_size(3), feature_scaling(off)], 2, 0, 5.5, 0.35, TrainTime, ClusterCount, NoiseCount, MinPrototypeDistance, MaxRadius).

	test(iris_unlabeled_reference_fit, true, [note(metrics(train_seconds-TrainTime, cluster_count-ClusterCount, noise_count-NoiseCount, min_prototype_distance-MinPrototypeDistance, max_radius-MaxRadius))]) :-
		reference_fit(iris_unlabeled, [minimum_points(2), minimum_cluster_size(2), feature_scaling(off)], 2, 0, 4.0, 1.5, TrainTime, ClusterCount, NoiseCount, MinPrototypeDistance, MaxRadius).

	reference_fit(Dataset, Options, ExpectedClusterCount, MaximumNoiseCount, MinimumPrototypeDistance, MaximumRadius, TrainTime, ClusterCount, NoiseCount, MinPrototypeDistance, MaxRadius) :-
		benchmark(hdbscan::learn(Dataset, _Clusterer, Options), TrainTime),
		hdbscan::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Clusterer, ClusterCount, NoiseCount, MinPrototypeDistance, MaxRadius),
		ClusterCount == ExpectedClusterCount,
		NoiseCount =< MaximumNoiseCount,
		MinPrototypeDistance >= MinimumPrototypeDistance,
		MaxRadius =< MaximumRadius.

	clusterer_metrics(hdbscan_clusterer(_Encoders, Clusters, Noise, _Options), ClusterCount, NoiseCount, MinPrototypeDistance, MaxRadius) :-
		length(Clusters, ClusterCount),
		length(Noise, NoiseCount),
		minimum_prototype_distance(Clusters, MinPrototypeDistance),
		maximum_radius(Clusters, MaxRadius).

	minimum_prototype_distance([], 0.0).
	minimum_prototype_distance([_], 0.0).
	minimum_prototype_distance([cluster(_Id, _Points, Prototype, _Radius)| Clusters], MinimumDistance) :-
		minimum_distance_to_clusters(Prototype, Clusters, FirstMinimumDistance),
		minimum_prototype_distance(Clusters, RestMinimumDistance),
		(   RestMinimumDistance =< 0.0 ->
			MinimumDistance = FirstMinimumDistance
		;   MinimumDistance is min(FirstMinimumDistance, RestMinimumDistance)
		).

	minimum_distance_to_clusters(_Prototype, [], 0.0).
	minimum_distance_to_clusters(Prototype, [cluster(_Id, _Points, OtherPrototype, _Radius)| Clusters], MinimumDistance) :-
		squared_distance(Prototype, OtherPrototype, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		minimum_distance_to_clusters(Prototype, Clusters, Distance, MinimumDistance).

	minimum_distance_to_clusters(_Prototype, [], MinimumDistance, MinimumDistance).
	minimum_distance_to_clusters(Prototype, [cluster(_Id, _Points, OtherPrototype, _Radius)| Clusters], MinimumDistance0, MinimumDistance) :-
		squared_distance(Prototype, OtherPrototype, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		MinimumDistance1 is min(MinimumDistance0, Distance),
		minimum_distance_to_clusters(Prototype, Clusters, MinimumDistance1, MinimumDistance).

	maximum_radius([], 0.0).
	maximum_radius([cluster(_Id, _Points, _Prototype, Radius)| Clusters], MaximumRadius) :-
		maximum_radius(Clusters, Radius, MaximumRadius).

	maximum_radius([], MaximumRadius, MaximumRadius).
	maximum_radius([cluster(_Id, _Points, _Prototype, Radius)| Clusters], MaximumRadius0, MaximumRadius) :-
		MaximumRadius1 is max(MaximumRadius0, Radius),
		maximum_radius(Clusters, MaximumRadius1, MaximumRadius).

	squared_distance([], [], 0.0).
	squared_distance([Value1| Values1], [Value2| Values2], DistanceSquared) :-
		squared_distance(Values1, Values2, RestDistanceSquared),
		Delta is Value1 - Value2,
		DistanceSquared is RestDistanceSquared + Delta * Delta.

:- end_object.
