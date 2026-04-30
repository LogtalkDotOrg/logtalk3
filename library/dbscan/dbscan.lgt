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


:- object(dbscan,
	imports([clusterer_common, search_indexing])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-30,
		comment is 'DBSCAN clusterer for continuous datasets. Learns from a dataset object implementing the ``clustering_dataset_protocol`` protocol and returns a clusterer term that can be used for assigning new instances to clusters and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses deterministic density-based clustering based on epsilon neighborhoods and minimum point counts.',
			'Feature handling' - 'Supports continuous attributes only. Continuous attributes can be standardized using z-score scaling before clustering.',
			'Distance metrics' - 'Supports Euclidean and Manhattan distances.',
			'Prediction' - 'New instances are assigned to the cluster of the nearest reachable core point within the learned epsilon radius; otherwise the atom ``noise`` is returned.',
			'Clusterer representation' - 'The learned clusterer is represented by default as ``dbscan_clusterer(Encoders, Clusters, Noise, Options)`` where ``Encoders`` stores the feature encoding metadata, ``Clusters`` is a list of ``cluster(Id, CorePoints, BorderPoints)`` terms in cluster-id order, ``Noise`` stores the encoded training points labeled as noise, and ``Options`` stores the effective training options.'
		],
		see_also is [clusterer_protocol, clustering_dataset_protocol, kcenters, kmeans, kmedoids, kmedians]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		last/2, length/2, member/2, nth1/3
	]).

	:- uses(numberlist, [
		euclidean_distance/3, manhattan_distance/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Clusterer, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		findall(
			Id-AttributeValues,
			Dataset::example(Id, AttributeValues),
			Examples
		),
		^^check_examples(Dataset, AttributeNames, Examples),
		^^build_encoders(AttributeNames, Examples, Options, Encoders),
		^^examples_to_rows(Examples, Encoders, Rows),
		build_search_index(Rows, Options, SearchIndex),
		build_neighborhood_index(Rows, SearchIndex, Options, NeighborhoodIndex),
		cluster_rows(Rows, NeighborhoodIndex, Options, ClusterCount, Assignments, CoreIds),
		clusters_from_assignments(ClusterCount, Rows, Assignments, CoreIds, Clusters, Noise),
		Clusterer = dbscan_clusterer(Encoders, Clusters, Noise, Options).

	cluster(Clusterer, Instance, Cluster) :-
		clusterer_data(Clusterer, Encoders, Clusters, _Noise, Options),
		^^encode_instance(Encoders, Instance, Features),
		classify_cluster(Clusters, Features, Options, Cluster).


	clusterer_data(Clusterer, Encoders, Clusters, Noise, Options) :-
		Clusterer =.. [_, Encoders, Clusters, Noise, Options].

	clusterer_diagnostics_data(Clusterer, Diagnostics) :-
		clusterer_data(Clusterer, _Encoders, Clusters, Noise, Options),
		length(Clusters, ClusterCount),
		length(Noise, NoiseCount),
		Diagnostics = [
			model(dbscan),
			cluster_count(ClusterCount),
			noise_count(NoiseCount),
			options(Options)
		].

	check_clusterer(Clusterer) :-
		(   clusterer_data(Clusterer, Encoders, Clusters, Noise, Options),
			length(Encoders, FeatureCount),
			^^valid_continuous_encoders(Encoders),
			valid_clusters(Clusters, FeatureCount, []),
			valid(list(list(number, FeatureCount)), Noise),
			catch(::check_options(Options), _Error, fail) ->
			true
		;   domain_error(clusterer, Clusterer)
		).

	valid_clusters([], _FeatureCount, _SeenIds).
	valid_clusters([cluster(ClusterId, CorePoints, BorderPoints)| Clusters], FeatureCount, SeenIds) :-
		valid(positive_integer, ClusterId),
		\+ member(ClusterId, SeenIds),
		CorePoints \== [],
		valid(list(list(number, FeatureCount)), CorePoints),
		valid(list(list(number, FeatureCount)), BorderPoints),
		valid_clusters(Clusters, FeatureCount, [ClusterId| SeenIds]).

	cluster_rows(Rows, NeighborhoodIndex, Options, ClusterCount, Assignments, CoreIds) :-
		avltree::new(Visited),
		avltree::new(Assignments0),
		avltree::new(CoreIds0),
		cluster_rows(Rows, NeighborhoodIndex, Options, Visited, Assignments0, 0, ClusterCount, Assignments, CoreIds0, CoreIds).

	cluster_rows([], _NeighborhoodIndex, _Options, _Visited, Assignments, ClusterCount, ClusterCount, Assignments, CoreIds, CoreIds).
	cluster_rows([Id-_| RemainingRows], NeighborhoodIndex, Options, Visited0, Assignments0, ClusterCount0, ClusterCount, Assignments, CoreIds0, CoreIds) :-
		(   visited(Id, Visited0) ->
			cluster_rows(RemainingRows, NeighborhoodIndex, Options, Visited0, Assignments0, ClusterCount0, ClusterCount, Assignments, CoreIds0, CoreIds)
		;   mark_visited(Id, Visited0, Visited1),
			region_query(Id, NeighborhoodIndex, NeighborCount, Neighbors),
			^^option(minimum_points(MinimumPoints), Options),
			(   NeighborCount < MinimumPoints ->
				assign_label(Id, noise, Assignments0, Assignments1),
				ClusterCount1 = ClusterCount0,
				Visited2 = Visited1,
				CoreIds1 = CoreIds0
			;   ClusterCount1 is ClusterCount0 + 1,
				assign_label(Id, ClusterCount1, Assignments0, SeedAssignments),
				add_core_id(Id, CoreIds0, SeedCoreIds),
				empty_queue(SeedQueue, SeedQueuedIds),
				enqueue_neighbors(Neighbors, Visited1, SeedQueue, Queue, SeedQueuedIds, QueuedIds),
				expand_cluster(Queue, QueuedIds, NeighborhoodIndex, Options, ClusterCount1, Visited1, Visited2, SeedAssignments, Assignments1, SeedCoreIds, CoreIds1)
			),
			cluster_rows(RemainingRows, NeighborhoodIndex, Options, Visited2, Assignments1, ClusterCount1, ClusterCount, Assignments, CoreIds1, CoreIds)
		).

	expand_cluster(Queue, _QueuedIds, _NeighborhoodIndex, _Options, _ClusterId, Visited, Visited, Assignments, Assignments, CoreIds, CoreIds) :-
		deque::empty(Queue),
		!.
	expand_cluster(Queue0, QueuedIds0, NeighborhoodIndex, Options, ClusterId, Visited0, Visited, Assignments0, Assignments, CoreIds0, CoreIds) :-
		deque::pop_front(Queue0, Id-_, Queue1),
		remove_queue_id(Id, QueuedIds0, QueuedIds1),
		assign_label(Id, ClusterId, Assignments0, Assignments1),
		(   visited(Id, Visited0) ->
			expand_cluster(Queue1, QueuedIds1, NeighborhoodIndex, Options, ClusterId, Visited0, Visited, Assignments1, Assignments, CoreIds0, CoreIds)
		;   region_query(Id, NeighborhoodIndex, NeighborCount, Neighbors),
			mark_visited(Id, Visited0, Visited1),
			^^option(minimum_points(MinimumPoints), Options),
			(   NeighborCount >= MinimumPoints ->
				add_core_id(Id, CoreIds0, CoreIds1),
				enqueue_neighbors(Neighbors, Visited1, Queue1, ExpandedQueue, QueuedIds1, QueuedIds2),
				expand_cluster(ExpandedQueue, QueuedIds2, NeighborhoodIndex, Options, ClusterId, Visited1, Visited, Assignments1, Assignments, CoreIds1, CoreIds)
			;   expand_cluster(Queue1, QueuedIds1, NeighborhoodIndex, Options, ClusterId, Visited1, Visited, Assignments1, Assignments, CoreIds0, CoreIds)
			)
		).

	build_search_index([], _Options, metric_tree(empty)).
	build_search_index(Rows, Options, SearchIndex) :-
		^^build_auto_search_index(Rows, Options, SearchIndex).

	select_metric_pivot(Rows, Options, Pivot, SortedRows) :-
		^^option(pivot_scoring(PivotScoring), Options),
		select_metric_pivot(PivotScoring, Rows, Options, Pivot, SortedRows).

	select_metric_pivot(heuristic, Rows, Options, Pivot, SortedRows) :-
		pivot_candidates(Rows, [FirstCandidate| OtherCandidates]),
		pivot_score(FirstCandidate, Rows, Options, BestScore),
		select_heuristic_metric_pivot(OtherCandidates, Rows, Options, BestScore, Pivot),
		pivot_sorted_rows(Pivot, Rows, Options, SortedRows).
	select_metric_pivot(exact, Rows, Options, Pivot, SortedRows) :-
		pivot_candidates(Rows, [FirstCandidate| OtherCandidates]),
		pivot_profile(FirstCandidate, Rows, Options, BestProfile),
		select_exact_metric_pivot(OtherCandidates, Rows, Options, BestProfile, Pivot, SortedRows).

	select_heuristic_metric_pivot([], _Rows, _Options, candidate(_Dispersion, _Range, Pivot), Pivot).
	select_heuristic_metric_pivot([Candidate| Candidates], Rows, Options, Best0, Pivot) :-
		pivot_score(Candidate, Rows, Options, Score),
		better_heuristic_pivot(Score, Best0, Best1),
		select_heuristic_metric_pivot(Candidates, Rows, Options, Best1, Pivot).

	select_exact_metric_pivot([], _Rows, _Options, candidate(_Gap, _Range, Pivot, SortedRows), Pivot, SortedRows).
	select_exact_metric_pivot([Candidate| Candidates], Rows, Options, Best0, Pivot, SortedRows) :-
		pivot_profile(Candidate, Rows, Options, Profile),
		better_exact_pivot(Profile, Best0, Best1),
		select_exact_metric_pivot(Candidates, Rows, Options, Best1, Pivot, SortedRows).

	pivot_candidates([First| Rows], Candidates) :-
		middle_row([First| Rows], Middle),
		last([First| Rows], Last),
		unique_candidate_rows([Middle, First, Last], Candidates).

	unique_candidate_rows([], []).
	unique_candidate_rows([Row| Rows], UniqueRows) :-
		(   member(Row, Rows) ->
			unique_candidate_rows(Rows, UniqueRows)
		;   UniqueRows = [Row| RestUniqueRows],
			unique_candidate_rows(Rows, RestUniqueRows)
		).

	middle_row(Rows, Middle) :-
		length(Rows, Count),
		MiddleIndex is (Count + 1) // 2,
		nth1(MiddleIndex, Rows, Middle).

	pivot_score(PivotId-PivotVector, Rows, Options, candidate(Dispersion, Range, PivotId-PivotVector)) :-
		pivot_distance_summary(Rows, PivotId, PivotVector, Options, Count, Sum, SumSquares, MinimumDistance, MaximumDistance),
		pivot_dispersion(Count, Sum, SumSquares, Dispersion),
		pivot_range(MinimumDistance, MaximumDistance, Range).

	pivot_sorted_rows(PivotId-PivotVector, Rows, Options, SortedRows) :-
		^^remove_candidate(PivotId-PivotVector, Rows, RemainingRows),
		decorate_by_distance(RemainingRows, PivotVector, Options, DecoratedRows),
		keysort(DecoratedRows, SortedRows).

	pivot_profile(PivotId-PivotVector, Rows, Options, candidate(Gap, Range, PivotId-PivotVector, SortedRows)) :-
		pivot_sorted_rows(PivotId-PivotVector, Rows, Options, SortedRows),
		sorted_rows_quality(SortedRows, Gap, Range).

	pivot_distance_summary(Rows, PivotId, PivotVector, Options, Count, Sum, SumSquares, MinimumDistance, MaximumDistance) :-
		pivot_distance_summary(Rows, PivotId, PivotVector, Options, 0, 0.0, 0.0, none, none, Count, Sum, SumSquares, MinimumDistance, MaximumDistance).

	pivot_distance_summary([], _PivotId, _PivotVector, _Options, Count, Sum, SumSquares, MinimumDistance, MaximumDistance, Count, Sum, SumSquares, MinimumDistance, MaximumDistance).
	pivot_distance_summary([Id-Vector| Rows], PivotId, PivotVector, Options, Count0, Sum0, SumSquares0, MinimumDistance0, MaximumDistance0, Count, Sum, SumSquares, MinimumDistance, MaximumDistance) :-
		(   Id == PivotId ->
			Count1 = Count0,
			Sum1 = Sum0,
			SumSquares1 = SumSquares0,
			MinimumDistance1 = MinimumDistance0,
			MaximumDistance1 = MaximumDistance0
		;   distance(Options, PivotVector, Vector, Distance),
			Count1 is Count0 + 1,
			Sum1 is Sum0 + Distance,
			SumSquares1 is SumSquares0 + Distance * Distance,
			update_distance_bounds(Distance, MinimumDistance0, MaximumDistance0, MinimumDistance1, MaximumDistance1)
		),
		pivot_distance_summary(Rows, PivotId, PivotVector, Options, Count1, Sum1, SumSquares1, MinimumDistance1, MaximumDistance1, Count, Sum, SumSquares, MinimumDistance, MaximumDistance).

	pivot_dispersion(0, _Sum, _SumSquares, 0.0) :-
		!.
	pivot_dispersion(Count, Sum, SumSquares, Dispersion) :-
		Mean is Sum / Count,
		Dispersion is SumSquares - Mean * Sum.

	pivot_range(none, none, 0.0) :-
		!.
	pivot_range(MinimumDistance, MaximumDistance, Range) :-
		Range is MaximumDistance - MinimumDistance.

	sorted_rows_quality(SortedRows, Gap, Range) :-
		^^split_sorted_rows(SortedRows, InnerUpperBound, OuterLowerBound, _InnerRows, _OuterRows),
		bound_gap(InnerUpperBound, OuterLowerBound, Gap),
		sorted_distance_range(SortedRows, Range).

	bound_gap(none, none, 0.0) :-
		!.
	bound_gap(none, LowerBound, LowerBound) :-
		!.
	bound_gap(UpperBound, none, UpperBound) :-
		!.
	bound_gap(UpperBound, LowerBound, Gap) :-
		Gap is LowerBound - UpperBound.

	sorted_distance_range([], 0.0).
	sorted_distance_range([MinDistance-_| SortedRows], Range) :-
		last_distance(SortedRows, MinDistance, MaxDistance),
		Range is MaxDistance - MinDistance.

	last_distance([], Distance, Distance).
	last_distance([Distance-_| SortedRows], _CurrentDistance, LastDistance) :-
		last_distance(SortedRows, Distance, LastDistance).

	update_distance_bounds(Distance, none, none, Distance, Distance) :-
		!.
	update_distance_bounds(Distance, MinimumDistance0, MaximumDistance0, MinimumDistance, MaximumDistance) :-
		MinimumDistance is min(MinimumDistance0, Distance),
		MaximumDistance is max(MaximumDistance0, Distance).

	better_heuristic_pivot(Score, candidate(BestDispersion, BestRange, _BestPivot), Score) :-
		Score = candidate(Dispersion, Range, _Pivot),
		(   Dispersion > BestDispersion ->
			true
		;   Dispersion =:= BestDispersion,
			Range > BestRange
		),
		!.
	better_heuristic_pivot(_Score, Best, Best).

	better_exact_pivot(Profile, candidate(BestGap, BestRange, _BestPivot, _BestSortedRows), Profile) :-
		Profile = candidate(Gap, Range, _Pivot, _SortedRows),
		(   Gap > BestGap ->
			true
		;   Gap =:= BestGap,
			Range > BestRange
		),
		!.
	better_exact_pivot(_Profile, Best, Best).

	decorate_by_distance([], _Vector, _Options, []).
	decorate_by_distance([Id-Vector| Rows], Pivot, Options, [Distance-(Id-Vector)| DecoratedRows]) :-
		distance(Options, Pivot, Vector, Distance),
		decorate_by_distance(Rows, Pivot, Options, DecoratedRows).


	search_index_cell_size(Options, CellSize) :-
		^^option(epsilon(CellSize), Options).

	build_neighborhood_index(Rows, SearchIndex, Options, NeighborhoodIndex) :-
		build_neighborhood_index(Rows, SearchIndex, Options, [], Pairs),
		avltree::as_dictionary(Pairs, NeighborhoodIndex).

	build_neighborhood_index([], _SearchIndex, _Options, Pairs, Pairs).
	build_neighborhood_index([Id-Vector| RemainingRows], SearchIndex, Options, Pairs0, Pairs) :-
		^^option(epsilon(Epsilon), Options),
		^^range_query(SearchIndex, Vector, Options, Epsilon, Neighbors),
		length(Neighbors, NeighborCount),
		build_neighborhood_index(RemainingRows, SearchIndex, Options, [Id-neighbors(NeighborCount, Neighbors)| Pairs0], Pairs).

	region_query(Id, NeighborhoodIndex, NeighborCount, Neighbors) :-
		avltree::lookup(Id, neighbors(NeighborCount, Neighbors), NeighborhoodIndex).

	empty_queue(Queue, QueuedIds) :-
		deque::new(Queue),
		avltree::new(QueuedIds).

	enqueue_neighbors([], _Visited, Queue, Queue, QueuedIds, QueuedIds).
	enqueue_neighbors([Id-Vector| Neighbors], Visited, Queue0, Queue, QueuedIds0, QueuedIds) :-
		(   visited(Id, Visited) ->
			Queue1 = Queue0,
			QueuedIds1 = QueuedIds0
		;   queued(Id, QueuedIds0) ->
			Queue1 = Queue0,
			QueuedIds1 = QueuedIds0
		;   deque::push_back(Id-Vector, Queue0, Queue1),
			avltree::insert(QueuedIds0, Id, true, QueuedIds1)
		),
		enqueue_neighbors(Neighbors, Visited, Queue1, Queue, QueuedIds1, QueuedIds).

	queued(Id, QueuedIds) :-
		avltree::lookup(Id, true, QueuedIds).

	remove_queue_id(Id, QueuedIds0, QueuedIds) :-
		(   avltree::delete(QueuedIds0, Id, true, QueuedIds) ->
			true
		;   QueuedIds = QueuedIds0
		).

	visited(Id, Visited) :-
		avltree::lookup(Id, true, Visited).

	mark_visited(Id, Visited0, Visited) :-
		avltree::insert(Visited0, Id, true, Visited).

	assign_label(Id, Label, Assignments0, Assignments) :-
		(   avltree::lookup(Id, ExistingLabel, Assignments0) ->
			update_label(ExistingLabel, Label, UpdatedLabel),
			avltree::insert(Assignments0, Id, UpdatedLabel, Assignments)
		;   avltree::insert(Assignments0, Id, Label, Assignments)
		).

	update_label(noise, Label, Label) :-
		Label \== noise,
		!.
	update_label(ExistingLabel, _Label, ExistingLabel).

	add_core_id(Id, CoreIds0, CoreIds) :-
		avltree::insert(CoreIds0, Id, true, CoreIds).

	clusters_from_assignments(ClusterCount, Rows, Assignments, CoreIds, Clusters, Noise) :-
		build_clusters(1, ClusterCount, Rows, Assignments, CoreIds, Clusters),
		noise_points(Rows, Assignments, Noise).

	build_clusters(Current, ClusterCount, _Rows, _Assignments, _CoreIds, []) :-
		Current > ClusterCount,
		!.
	build_clusters(Current, ClusterCount, Rows, Assignments, CoreIds, [cluster(Current, CorePoints, BorderPoints)| Clusters]) :-
		cluster_points(Current, Rows, Assignments, CoreIds, CorePoints, BorderPoints),
		Next is Current + 1,
		build_clusters(Next, ClusterCount, Rows, Assignments, CoreIds, Clusters).

	cluster_points(_ClusterId, [], _Assignments, _CoreIds, [], []) :-
		!.
	cluster_points(ClusterId, [Id-Vector| Rows], Assignments, CoreIds, CorePoints, BorderPoints) :-
		(   assignment_label(Id, Assignments, ClusterId) ->
			(   core_point(Id, CoreIds) ->
				CorePoints = [Vector| RestCorePoints],
				BorderPoints = RestBorderPoints
			;   CorePoints = RestCorePoints,
				BorderPoints = [Vector| RestBorderPoints]
			)
		;   CorePoints = RestCorePoints,
			BorderPoints = RestBorderPoints
		),
		cluster_points(ClusterId, Rows, Assignments, CoreIds, RestCorePoints, RestBorderPoints).

	noise_points([], _Assignments, []) :-
		!.
	noise_points([Id-Vector| Rows], Assignments, Noise) :-
		(   assignment_label(Id, Assignments, noise) ->
			Noise = [Vector| RestNoise]
		;   Noise = RestNoise
		),
		noise_points(Rows, Assignments, RestNoise).

	assignment_label(Id, Assignments, Label) :-
		avltree::lookup(Id, Label, Assignments).

	core_point(Id, CoreIds) :-
		avltree::lookup(Id, true, CoreIds).

	classify_cluster(Clusters, Features, Options, Cluster) :-
		findall(
			Distance-CandidateCluster,
			(   member(cluster(CandidateCluster, CorePoints, _BorderPoints), Clusters),
				nearest_reachable_core_distance(CorePoints, Features, Options, Distance)
			),
			Candidates
		),
		(   Candidates == [] ->
			Cluster = noise
		;   best_candidate(Candidates, Cluster, _Distance)
		).

	nearest_reachable_core_distance(CorePoints, Features, Options, Distance) :-
		^^option(epsilon(Epsilon), Options),
		findall(
			CandidateDistance,
			(   member(CorePoint, CorePoints),
				distance(Options, Features, CorePoint, CandidateDistance),
				CandidateDistance =< Epsilon
			),
			Distances
		),
		Distances = [FirstDistance| RestDistances],
		minimum_distance(RestDistances, FirstDistance, Distance).

	minimum_distance([], Distance, Distance).
	minimum_distance([Candidate| Candidates], CurrentDistance, Distance) :-
		(   Candidate < CurrentDistance ->
			NextDistance = Candidate
		;   NextDistance = CurrentDistance
		),
		minimum_distance(Candidates, NextDistance, Distance).

	best_candidate([Distance-Cluster| Candidates], BestCluster, BestDistance) :-
		best_candidate(Candidates, Cluster, Distance, BestCluster, BestDistance).

	best_candidate([], BestCluster, BestDistance, BestCluster, BestDistance).
	best_candidate([Distance-Cluster| Candidates], CurrentCluster, CurrentDistance, BestCluster, BestDistance) :-
		(   Distance < CurrentDistance ->
			NextCluster = Cluster,
			NextDistance = Distance
		;   NextCluster = CurrentCluster,
			NextDistance = CurrentDistance
		),
		best_candidate(Candidates, NextCluster, NextDistance, BestCluster, BestDistance).

	distance(Options, Vector1, Vector2, Distance) :-
		^^option(distance_metric(Metric), Options),
		distance_metric(Metric, Vector1, Vector2, Distance).

	distance_metric(euclidean, Vector1, Vector2, Distance) :-
		euclidean_distance(Vector1, Vector2, Distance).
	distance_metric(manhattan, Vector1, Vector2, Distance) :-
		manhattan_distance(Vector1, Vector2, Distance).

	print_clusterer(dbscan_clusterer(Encoders, Clusters, Noise, Options)) :-
		format('DBSCAN Clusterer~n', []),
		format('================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nClusters:~n', []),
		print_clusters(Clusters),
		length(Noise, NoiseCount),
		format('~nNoise points (~d): ~w~n', [NoiseCount, Noise]).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		format('  ~w (continuous, mean=~4f, scale=~4f)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).

	print_clusters([]).
	print_clusters([cluster(ClusterId, CorePoints, BorderPoints)| Clusters]) :-
		length(CorePoints, CoreCount),
		length(BorderPoints, BorderCount),
		format('  cluster ~d: ~d core points, ~d border points~n', [ClusterId, CoreCount, BorderCount]),
		format('    core: ~w~n', [CorePoints]),
		format('    border: ~w~n', [BorderPoints]),
		print_clusters(Clusters).

	default_option(epsilon(1.0)).
	default_option(minimum_points(2)).
	default_option(distance_metric(euclidean)).
	default_option(feature_scaling(on)).
	default_option(pivot_scoring(heuristic)).

	valid_option(epsilon(Epsilon)) :-
		valid(positive_number, Epsilon).
	valid_option(minimum_points(MinimumPoints)) :-
		valid(positive_integer, MinimumPoints).
	valid_option(distance_metric(Metric)) :-
		once((Metric == euclidean; Metric == manhattan)).
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).
	valid_option(pivot_scoring(PivotScoring)) :-
		once((PivotScoring == exact; PivotScoring == heuristic)).

:- end_object.
