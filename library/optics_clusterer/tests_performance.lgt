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
		comment is 'Performance and reference-fit benchmarks for the "optics_clusterer" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2
	]).

	test(two_blobs_reference_fit, true, [note(metrics(train_seconds-TrainTime, cluster_count-ClusterCount, noise_count-NoiseCount, min_core_distance-MinCoreDistance))]) :-
		reference_fit(two_blobs, [ordering_and_extraction_epsilons(0.6, 0.6), minimum_points(2), feature_scaling(off)], 2, 0, 5.0, TrainTime, ClusterCount, NoiseCount, MinCoreDistance).

	test(iris_unlabeled_reference_fit, true, [note(metrics(train_seconds-TrainTime, cluster_count-ClusterCount, noise_count-NoiseCount, min_core_distance-MinCoreDistance))]) :-
		reference_fit(iris_unlabeled, [ordering_and_extraction_epsilons(1.25, 1.25), minimum_points(2)], 2, 3, 1.5, TrainTime, ClusterCount, NoiseCount, MinCoreDistance).

	test(large_two_blobs_reference_fit, true, [note(metrics(train_seconds-TrainTime, cluster_count-ClusterCount, noise_count-NoiseCount, min_core_distance-MinCoreDistance))]) :-
		reference_fit(large_two_blobs, [ordering_and_extraction_epsilons(0.18, 0.18), minimum_points(4), feature_scaling(off)], 2, 0, 6.0, TrainTime, ClusterCount, NoiseCount, MinCoreDistance).

	reference_fit(Dataset, Options, ExpectedClusterCount, MaximumNoiseCount, MinimumCoreDistance, TrainTime, ClusterCount, NoiseCount, MinCoreDistance) :-
		benchmark(optics_clusterer::learn(Dataset, _Clusterer, Options), TrainTime),
		optics_clusterer::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Clusterer, ClusterCount, NoiseCount, MinCoreDistance),
		ClusterCount == ExpectedClusterCount,
		NoiseCount =< MaximumNoiseCount,
		MinCoreDistance >= MinimumCoreDistance.

	clusterer_metrics(optics_clusterer(_Encoders, _Ordering, Clusters, Noise, _Options), ClusterCount, NoiseCount, MinCoreDistance) :-
		length(Clusters, ClusterCount),
		length(Noise, NoiseCount),
		minimum_core_distance_between_clusters(Clusters, MinCoreDistance).

	minimum_core_distance_between_clusters([], 0.0).
	minimum_core_distance_between_clusters([_], 0.0).
	minimum_core_distance_between_clusters([cluster(_Id, CorePoints, _BorderPoints, _Bounds)| Clusters], MinimumDistance) :-
		minimum_distance_to_clusters(CorePoints, Clusters, FirstMinimumDistance),
		minimum_core_distance_between_clusters(Clusters, RestMinimumDistance),
		(	RestMinimumDistance =< 0.0 ->
			MinimumDistance = FirstMinimumDistance
		;	MinimumDistance is min(FirstMinimumDistance, RestMinimumDistance)
		).

	minimum_distance_to_clusters(_CorePoints, [], 0.0).
	minimum_distance_to_clusters(CorePoints, [cluster(_Id, OtherCorePoints, _BorderPoints, _Bounds)| Clusters], MinimumDistance) :-
		minimum_distance_between_core_sets(CorePoints, OtherCorePoints, FirstDistance),
		minimum_distance_to_clusters(CorePoints, Clusters, RestDistance),
		(	RestDistance =< 0.0 ->
			MinimumDistance = FirstDistance
		;	MinimumDistance is min(FirstDistance, RestDistance)
		).

	minimum_distance_between_core_sets([CorePoint| CorePoints], OtherCorePoints, MinimumDistance) :-
		minimum_distance_to_other_set(CorePoint, OtherCorePoints, FirstDistance),
		minimum_distance_between_core_sets(CorePoints, OtherCorePoints, FirstDistance, MinimumDistance).

	minimum_distance_between_core_sets([], _OtherCorePoints, MinimumDistance, MinimumDistance).
	minimum_distance_between_core_sets([CorePoint| CorePoints], OtherCorePoints, CurrentDistance, MinimumDistance) :-
		minimum_distance_to_other_set(CorePoint, OtherCorePoints, CandidateDistance),
		NextDistance is min(CurrentDistance, CandidateDistance),
		minimum_distance_between_core_sets(CorePoints, OtherCorePoints, NextDistance, MinimumDistance).

	minimum_distance_to_other_set(CorePoint, [OtherCorePoint| OtherCorePoints], MinimumDistance) :-
		squared_distance(CorePoint, OtherCorePoint, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		minimum_distance_to_other_set(CorePoint, OtherCorePoints, Distance, MinimumDistance).

	minimum_distance_to_other_set(_CorePoint, [], MinimumDistance, MinimumDistance).
	minimum_distance_to_other_set(CorePoint, [OtherCorePoint| OtherCorePoints], CurrentDistance, MinimumDistance) :-
		squared_distance(CorePoint, OtherCorePoint, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		NextDistance is min(CurrentDistance, Distance),
		minimum_distance_to_other_set(CorePoint, OtherCorePoints, NextDistance, MinimumDistance).

	squared_distance([], [], 0.0).
	squared_distance([Value1| Values1], [Value2| Values2], DistanceSquared) :-
		squared_distance(Values1, Values2, RestDistanceSquared),
		Delta is Value1 - Value2,
		DistanceSquared is RestDistanceSquared + Delta * Delta.

:- end_object.
