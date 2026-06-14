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
		comment is 'Performance and reference-fit benchmarks for the "hdbscan_clusterer" library.'
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
		reference_fit(iris_unlabeled, [minimum_points(2), minimum_cluster_size(2), feature_scaling(off)], 3, 2, 1.4, 0.5, TrainTime, ClusterCount, NoiseCount, MinPrototypeDistance, MaxRadius).

	reference_fit(Dataset, Options, ExpectedClusterCount, MaximumNoiseCount, MinimumPrototypeDistance, MaximumRadius, TrainTime, ClusterCount, NoiseCount, MinPrototypeDistance, MaxRadius) :-
		benchmark(hdbscan_clusterer::learn(Dataset, _Clusterer, Options), TrainTime),
		hdbscan_clusterer::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Clusterer, ClusterCount, NoiseCount, MinPrototypeDistance, MaxRadius),
		ClusterCount == ExpectedClusterCount,
		NoiseCount =< MaximumNoiseCount,
		MinPrototypeDistance >= MinimumPrototypeDistance,
		MaxRadius =< MaximumRadius.

	clusterer_metrics(hdbscan_clusterer(_Encoders, Clusters, Noise, _Options), ClusterCount, NoiseCount, MinPrototypeDistance, MaxRadius) :-
		length(Clusters, ClusterCount),
		length(Noise, NoiseCount),
		cluster_summaries(Clusters, Summaries),
		minimum_prototype_distance(Summaries, MinPrototypeDistance),
		maximum_radius(Summaries, MaxRadius).

	cluster_summaries([], []).
	cluster_summaries([cluster(_Id, Points, _MaxCoreDistance, _Stability)| Clusters], [cluster_summary(Prototype, Radius)| Summaries]) :-
		points_centroid(Points, Prototype),
		maximum_distance_to_point(Points, Prototype, Radius),
		cluster_summaries(Clusters, Summaries).

	minimum_prototype_distance([], 0.0).
	minimum_prototype_distance([_], 0.0).
	minimum_prototype_distance([cluster_summary(Prototype, _Radius)| Clusters], MinimumDistance) :-
		minimum_distance_to_clusters(Prototype, Clusters, FirstMinimumDistance),
		minimum_prototype_distance(Clusters, RestMinimumDistance),
		(	RestMinimumDistance =< 0.0 ->
			MinimumDistance = FirstMinimumDistance
		;	MinimumDistance is min(FirstMinimumDistance, RestMinimumDistance)
		).

	minimum_distance_to_clusters(_Prototype, [], 0.0).
	minimum_distance_to_clusters(Prototype, [cluster_summary(OtherPrototype, _Radius)| Clusters], MinimumDistance) :-
		squared_distance(Prototype, OtherPrototype, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		minimum_distance_to_clusters(Prototype, Clusters, Distance, MinimumDistance).

	minimum_distance_to_clusters(_Prototype, [], MinimumDistance, MinimumDistance).
	minimum_distance_to_clusters(Prototype, [cluster_summary(OtherPrototype, _Radius)| Clusters], MinimumDistance0, MinimumDistance) :-
		squared_distance(Prototype, OtherPrototype, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		MinimumDistance1 is min(MinimumDistance0, Distance),
		minimum_distance_to_clusters(Prototype, Clusters, MinimumDistance1, MinimumDistance).

	maximum_radius([], 0.0).
	maximum_radius([cluster_summary(_Prototype, Radius)| Clusters], MaximumRadius) :-
		maximum_radius(Clusters, Radius, MaximumRadius).

	maximum_radius([], MaximumRadius, MaximumRadius).
	maximum_radius([cluster_summary(_Prototype, Radius)| Clusters], MaximumRadius0, MaximumRadius) :-
		MaximumRadius1 is max(MaximumRadius0, Radius),
		maximum_radius(Clusters, MaximumRadius1, MaximumRadius).

	points_centroid([Point| Points], Centroid) :-
		zero_vector(Point, ZeroVector),
		sum_points([Point| Points], ZeroVector, 0, Sums, Count),
		average_vector(Sums, Count, Centroid).

	zero_vector([], []).
	zero_vector([_| Values], [0.0| ZeroVector]) :-
		zero_vector(Values, ZeroVector).

	sum_points([], Sums, Count, Sums, Count).
	sum_points([Point| Points], Sums0, Count0, Sums, Count) :-
		add_vectors(Point, Sums0, Sums1),
		Count1 is Count0 + 1,
		sum_points(Points, Sums1, Count1, Sums, Count).

	add_vectors([], [], []).
	add_vectors([Value| Values], [Sum0| Sums0], [Sum| Sums]) :-
		Sum is Sum0 + Value,
		add_vectors(Values, Sums0, Sums).

	average_vector([], _Count, []).
	average_vector([Sum| Sums], Count, [Value| Values]) :-
		Value is Sum / float(Count),
		average_vector(Sums, Count, Values).

	maximum_distance_to_point([Point| Points], Prototype, MaximumDistance) :-
		squared_distance(Point, Prototype, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		maximum_distance_to_point(Points, Prototype, Distance, MaximumDistance).

	maximum_distance_to_point([], _Prototype, MaximumDistance, MaximumDistance).
	maximum_distance_to_point([Point| Points], Prototype, MaximumDistance0, MaximumDistance) :-
		squared_distance(Point, Prototype, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		MaximumDistance1 is max(MaximumDistance0, Distance),
		maximum_distance_to_point(Points, Prototype, MaximumDistance1, MaximumDistance).

	squared_distance([], [], 0.0).
	squared_distance([Value1| Values1], [Value2| Values2], DistanceSquared) :-
		squared_distance(Values1, Values2, RestDistanceSquared),
		Delta is Value1 - Value2,
		DistanceSquared is RestDistanceSquared + Delta * Delta.

:- end_object.
