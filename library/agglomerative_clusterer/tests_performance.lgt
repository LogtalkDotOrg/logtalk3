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
		comment is 'Performance and reference-fit benchmarks for the "agglomerative_clusterer" library.'
	]).

	:- uses(lgtunit, [benchmark/2]).
	:- uses(list, [memberchk/2, nth1/3]).
	:- uses(numberlist, [euclidean_distance/3, manhattan_distance/3]).

	test(two_blobs_reference_fit, true, [note(metrics(train_seconds-TrainTime, max_covering_radius-CoveringRadius, min_prototype_distance-MinPrototypeDistance))]) :-
		reference_fit(two_blobs, [k(2), feature_scaling(off)], 0.35, 5.5, TrainTime, CoveringRadius, MinPrototypeDistance).

	test(large_two_blobs_heap_metrics, true, [note(metrics(train_seconds-TrainTime, maximum_heap_size-MaximumHeapSize, stale_pair_discard_count-StalePairDiscardCount))]) :-
		heap_metrics(large_two_blobs, [k(2), feature_scaling(off)], 100, 2, TrainTime, MaximumHeapSize, StalePairDiscardCount).

	test(large_two_blobs_complete_heap_metrics, true, [note(metrics(train_seconds-TrainTime, maximum_heap_size-MaximumHeapSize, stale_pair_discard_count-StalePairDiscardCount))]) :-
		heap_metrics(large_two_blobs, [k(2), linkage(complete), feature_scaling(off)], 100, 2, TrainTime, MaximumHeapSize, StalePairDiscardCount).

	test(iris_unlabeled_reference_fit, true, [note(metrics(train_seconds-TrainTime, max_covering_radius-CoveringRadius, min_prototype_distance-MinPrototypeDistance))]) :-
		reference_fit(iris_unlabeled, [k(3)], 1.5, 2.0, TrainTime, CoveringRadius, MinPrototypeDistance).

	reference_fit(Dataset, Options, MaximumCoveringRadius, MinimumPrototypeDistance, TrainTime, CoveringRadius, MinPrototypeDistance) :-
		benchmark(agglomerative_clusterer::learn(Dataset, _Clusterer, Options), TrainTime),
		agglomerative_clusterer::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Dataset, Clusterer, CoveringRadius, MinPrototypeDistance),
		CoveringRadius =< MaximumCoveringRadius,
		MinPrototypeDistance >= MinimumPrototypeDistance.

	heap_metrics(Dataset, Options, ExpectedTrainingExampleCount, ExpectedClusterCount, TrainTime, MaximumHeapSize, StalePairDiscardCount) :-
		benchmark(agglomerative_clusterer::learn(Dataset, _Clusterer, Options), TrainTime),
		agglomerative_clusterer::learn(Dataset, Clusterer, Options),
		agglomerative_clusterer::diagnostics(Clusterer, Diagnostics),
		memberchk(cluster_count(ExpectedClusterCount), Diagnostics),
		memberchk(training_example_count(ExpectedTrainingExampleCount), Diagnostics),
		ExpectedMergeCount is ExpectedTrainingExampleCount - ExpectedClusterCount,
		memberchk(merge_count(ExpectedMergeCount), Diagnostics),
		ExpectedInitialPairCount is (ExpectedTrainingExampleCount * (ExpectedTrainingExampleCount - 1)) // 2,
		memberchk(initial_pair_count(ExpectedInitialPairCount), Diagnostics),
		memberchk(pair_selection(priority_queue), Diagnostics),
		memberchk(maximum_heap_size(MaximumHeapSize), Diagnostics),
		memberchk(stale_pair_discard_count(StalePairDiscardCount), Diagnostics),
		total_inserted_pair_count(ExpectedTrainingExampleCount, ExpectedClusterCount, TotalInsertedPairCount),
		MaximumHeapSize >= ExpectedInitialPairCount,
		MaximumHeapSize =< TotalInsertedPairCount,
		StalePairDiscardCount > 0,
		StalePairDiscardCount < TotalInsertedPairCount.

	clusterer_metrics(Dataset, agglomerative_clusterer(Encoders, Clusters, Prototypes, Options, _Diagnostics), CoveringRadius, MinPrototypeDistance) :-
		findall(AttributeValues, Dataset::example(_, AttributeValues), Examples),
		covering_radius(Examples, agglomerative_clusterer(Encoders, Clusters, Prototypes, Options, _Diagnostics), CoveringRadius),
		minimum_prototype_distance(Prototypes, MinPrototypeDistance).

	covering_radius([], _, 0.0).
	covering_radius([AttributeValues| Examples], Clusterer, CoveringRadius) :-
		Clusterer = agglomerative_clusterer(Encoders, Clusters, _Prototypes, Options, _Diagnostics),
		agglomerative_clusterer::cluster(Clusterer, AttributeValues, Cluster),
		encode_instance(Encoders, AttributeValues, Features),
		nth1(Cluster, Clusters, cluster(_ClusterId, Points)),
		cluster_distance(Options, Features, Points, ExampleRadius),
		covering_radius(Examples, Clusterer, RestRadius),
		CoveringRadius is max(ExampleRadius, RestRadius).

	cluster_distance(Options, Features, Points, Distance) :-
		memberchk(linkage(Linkage), Options),
		cluster_distance(Linkage, Features, Points, Options, Distance).

	cluster_distance(single, Features, [Point| Points], Options, Distance) :-
		distance(Options, Features, Point, InitialDistance),
		minimum_distance(Points, Features, Options, InitialDistance, Distance).
	cluster_distance(complete, Features, [Point| Points], Options, Distance) :-
		distance(Options, Features, Point, InitialDistance),
		maximum_distance(Points, Features, Options, InitialDistance, Distance).
	cluster_distance(average, Features, [Point| Points], Options, Distance) :-
		distance(Options, Features, Point, InitialDistance),
		sum_distances(Points, Features, Options, InitialDistance, 1, Sum, Count),
		Distance is Sum / Count.

	minimum_distance([], _Features, _Options, Distance, Distance).
	minimum_distance([Point| Points], Features, Options, CurrentDistance, Distance) :-
		distance(Options, Features, Point, CandidateDistance),
		(   CandidateDistance < CurrentDistance ->
			NextDistance = CandidateDistance
		;   NextDistance = CurrentDistance
		),
		minimum_distance(Points, Features, Options, NextDistance, Distance).

	maximum_distance([], _Features, _Options, Distance, Distance).
	maximum_distance([Point| Points], Features, Options, CurrentDistance, Distance) :-
		distance(Options, Features, Point, CandidateDistance),
		(   CandidateDistance > CurrentDistance ->
			NextDistance = CandidateDistance
		;   NextDistance = CurrentDistance
		),
		maximum_distance(Points, Features, Options, NextDistance, Distance).

	sum_distances([], _Features, _Options, Sum, Count, Sum, Count).
	sum_distances([Point| Points], Features, Options, Sum0, Count0, Sum, Count) :-
		distance(Options, Features, Point, Distance0),
		Sum1 is Sum0 + Distance0,
		Count1 is Count0 + 1,
		sum_distances(Points, Features, Options, Sum1, Count1, Sum, Count).

	total_inserted_pair_count(TrainingExampleCount, TargetClusterCount, TotalInsertedPairCount) :-
		initial_pair_count(TrainingExampleCount, InitialPairCount),
		additional_pair_insertions(TrainingExampleCount, TargetClusterCount, AdditionalPairInsertions),
		TotalInsertedPairCount is InitialPairCount + AdditionalPairInsertions.

	initial_pair_count(TrainingExampleCount, InitialPairCount) :-
		InitialPairCount is (TrainingExampleCount * (TrainingExampleCount - 1)) // 2.

	additional_pair_insertions(ClusterCount, TargetClusterCount, 0) :-
		ClusterCount =< TargetClusterCount + 1,
		!.
	additional_pair_insertions(ClusterCount, TargetClusterCount, AdditionalPairInsertions) :-
		PairsAddedThisMerge is ClusterCount - 2,
		NextClusterCount is ClusterCount - 1,
		additional_pair_insertions(NextClusterCount, TargetClusterCount, RestPairInsertions),
		AdditionalPairInsertions is PairsAddedThisMerge + RestPairInsertions.

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
