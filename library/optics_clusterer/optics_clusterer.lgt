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


:- object(optics_clusterer,
	imports([clusterer_common, search_indexing])).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-05-22,
		comment is 'OPTICS clusterer for continuous datasets. Learns an ordering from a dataset object implementing the ``clustering_dataset_protocol`` protocol and extracts clusters using an epsilon threshold so the result can be used with the standard clusterer protocol.',
		see_also is [clusterer_protocol, clustering_dataset_protocol, dbscan_clusterer]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		last/2, length/2, member/2, nth1/3, reverse/2
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
		findall(Id-AttributeValues, Dataset::example(Id, AttributeValues), Examples),
		^^check_examples(Dataset, AttributeNames, Examples),
		^^build_encoders(AttributeNames, Examples, Options, Encoders),
		^^examples_to_rows(Examples, Encoders, Rows),
		build_search_index(Rows, Options, SearchIndex),
		optics_order(Rows, SearchIndex, Options, Ordering),
		extract_clusters(Ordering, Options, Clusters, Noise),
		Clusterer = optics_clusterer(Encoders, Ordering, Clusters, Noise, Options).

	cluster(Clusterer, Instance, Cluster) :-
		clusterer_data(Clusterer, Encoders, _Ordering, Clusters, _Noise, Options),
		^^encode_instance(Encoders, Instance, Features),
		classify_cluster(Clusters, Features, Options, Cluster).

	clusterer_data(Clusterer, Encoders, Ordering, Clusters, Noise, Options) :-
		Clusterer =.. [_, Encoders, Ordering, Clusters, Noise, Options].

	clusterer_diagnostics_data(Clusterer, Diagnostics) :-
		clusterer_data(Clusterer, _Encoders, Ordering, Clusters, Noise, Options),
		length(Ordering, OrderingCount),
		length(Clusters, ClusterCount),
		length(Noise, NoiseCount),
		Diagnostics = [
			model(optics_clusterer),
			ordering_count(OrderingCount),
			cluster_count(ClusterCount),
			noise_count(NoiseCount),
			options(Options)
		].

	check_clusterer(Clusterer) :-
		(	clusterer_data(Clusterer, Encoders, Ordering, Clusters, Noise, Options),
			length(Encoders, FeatureCount),
			^^valid_continuous_encoders(Encoders),
			valid_ordering(Ordering, FeatureCount, []),
			valid_clusters(Clusters, FeatureCount, []),
			valid(list(list(number, FeatureCount)), Noise),
			catch(::check_options(Options), _Error, fail) ->
			true
		;	domain_error(clusterer, Clusterer)
		).

	valid_ordering([], _FeatureCount, _SeenIds).
	valid_ordering([ordered(Id, Reachability, CoreDistance, Vector)| Ordering], FeatureCount, SeenIds) :-
		nonvar(Id),
		\+ member(Id, SeenIds),
		valid_optional_distance(Reachability),
		valid_optional_distance(CoreDistance),
		valid(list(number, FeatureCount), Vector),
		valid_ordering(Ordering, FeatureCount, [Id| SeenIds]).

	valid_optional_distance(none).
	valid_optional_distance(Distance) :-
		number(Distance),
		Distance >= 0.0.

	valid_clusters([], _FeatureCount, _SeenIds).
	valid_clusters([cluster(ClusterId, CorePoints, BorderPoints, bounds(Minimums, Maximums))| Clusters], FeatureCount, SeenIds) :-
		valid(positive_integer, ClusterId),
		\+ member(ClusterId, SeenIds),
		CorePoints \== [],
		valid(list(list(number, FeatureCount)), CorePoints),
		valid(list(list(number, FeatureCount)), BorderPoints),
		valid(list(number, FeatureCount), Minimums),
		valid(list(number, FeatureCount), Maximums),
		valid_clusters(Clusters, FeatureCount, [ClusterId| SeenIds]).

	optics_order(Rows, SearchIndex, Options, Ordering) :-
		avltree::new(Processed0),
		optics_order(Rows, SearchIndex, Options, Processed0, [], ReverseOrdering),
		reverse(ReverseOrdering, Ordering).

	optics_order([], _SearchIndex, _Options, _Processed, Ordering, Ordering).
	optics_order([Id-Vector| Rows], SearchIndex, Options, Processed0, Ordering0, Ordering) :-
		(	processed(Id, Processed0) ->
			optics_order(Rows, SearchIndex, Options, Processed0, Ordering0, Ordering)
		;	empty_seed_queue(Seeds0),
			process_point(Id-Vector, none, SearchIndex, Options, Processed0, Processed1, Seeds0, Seeds, Ordering0, Ordering1),
			expand_seeds(Seeds, SearchIndex, Options, Processed1, Processed2, Ordering1, Ordering2),
			optics_order(Rows, SearchIndex, Options, Processed2, Ordering2, Ordering)
		).

	process_point(Id-Vector, Reachability, SearchIndex, Options, Processed0, Processed, Seeds0, Seeds, Ordering0, Ordering) :-
		region_query(Id-Vector, SearchIndex, Options, Neighbors),
		neighbor_distances(Neighbors, Distances),
		core_distance(Distances, Options, CoreDistance),
		Ordering1 = [ordered(Id, Reachability, CoreDistance, Vector)| Ordering0],
		mark_processed(Id, Processed0, Processed),
		(	CoreDistance == none ->
			Seeds = Seeds0
		;	update_seeds(Neighbors, CoreDistance, Processed0, Seeds0, Seeds)
		),
		Ordering = Ordering1.

	expand_seeds(Seeds0, SearchIndex, Options, Processed0, Processed, Ordering0, Ordering) :-
		(	extract_best_seed(Seeds0, seed(Id, Reachability, Vector), Seeds1) ->
			(	processed(Id, Processed0) ->
				expand_seeds(Seeds1, SearchIndex, Options, Processed0, Processed, Ordering0, Ordering)
			;	process_point(Id-Vector, Reachability, SearchIndex, Options, Processed0, Processed1, Seeds1, Seeds2, Ordering0, Ordering1),
				expand_seeds(Seeds2, SearchIndex, Options, Processed1, Processed, Ordering1, Ordering)
			)
		;	Processed = Processed0,
			Ordering = Ordering0
		).

	build_search_index([], _Options, metric_tree(empty)).
	build_search_index([Row| Rows], Options, SearchIndex) :-
		^^option(search_index(SearchIndexType), Options),
		build_search_index(SearchIndexType, [Row| Rows], Options, SearchIndex).

	build_search_index(auto, Rows, Options, SearchIndex) :-
		^^build_auto_search_index(Rows, Options, SearchIndex).
	build_search_index(grid, Rows, Options, SearchIndex) :-
		^^build_grid_index(Rows, Options, SearchIndex).
	build_search_index(metric_tree, Rows, Options, metric_tree(MetricTree)) :-
		^^build_metric_tree(Rows, Options, MetricTree).

	select_metric_pivot(Rows, Options, Pivot, SortedRows) :-
		pivot_candidates(Rows, [FirstCandidate| OtherCandidates]),
		pivot_score(FirstCandidate, Rows, Options, BestScore),
		select_metric_pivot(OtherCandidates, Rows, Options, BestScore, Pivot),
		pivot_sorted_rows(Pivot, Rows, Options, SortedRows).

	select_metric_pivot([], _Rows, _Options, candidate(_Dispersion, _Range, Pivot), Pivot).
	select_metric_pivot([Candidate| Candidates], Rows, Options, Best0, Pivot) :-
		pivot_score(Candidate, Rows, Options, Score),
		better_metric_pivot(Score, Best0, Best1),
		select_metric_pivot(Candidates, Rows, Options, Best1, Pivot).

	pivot_candidates([First| Rows], Candidates) :-
		middle_row([First| Rows], Middle),
		last([First| Rows], Last),
		unique_candidate_rows([Middle, First, Last], Candidates).

	unique_candidate_rows([], []).
	unique_candidate_rows([Row| Rows], UniqueRows) :-
		(	member(Row, Rows) ->
			unique_candidate_rows(Rows, UniqueRows)
		;	UniqueRows = [Row| RestUniqueRows],
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

	pivot_distance_summary(Rows, PivotId, PivotVector, Options, Count, Sum, SumSquares, MinimumDistance, MaximumDistance) :-
		pivot_distance_summary(Rows, PivotId, PivotVector, Options, 0, 0.0, 0.0, none, none, Count, Sum, SumSquares, MinimumDistance, MaximumDistance).

	pivot_distance_summary([], _PivotId, _PivotVector, _Options, Count, Sum, SumSquares, MinimumDistance, MaximumDistance, Count, Sum, SumSquares, MinimumDistance, MaximumDistance).
	pivot_distance_summary([Id-Vector| Rows], PivotId, PivotVector, Options, Count0, Sum0, SumSquares0, MinimumDistance0, MaximumDistance0, Count, Sum, SumSquares, MinimumDistance, MaximumDistance) :-
		(	Id == PivotId ->
			Count1 = Count0,
			Sum1 = Sum0,
			SumSquares1 = SumSquares0,
			MinimumDistance1 = MinimumDistance0,
			MaximumDistance1 = MaximumDistance0
		;	distance(Options, PivotVector, Vector, Distance),
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

	update_distance_bounds(Distance, none, none, Distance, Distance) :-
		!.
	update_distance_bounds(Distance, MinimumDistance0, MaximumDistance0, MinimumDistance, MaximumDistance) :-
		MinimumDistance is min(MinimumDistance0, Distance),
		MaximumDistance is max(MaximumDistance0, Distance).

	better_metric_pivot(Score, candidate(BestDispersion, BestRange, _BestPivot), Score) :-
		Score = candidate(Dispersion, Range, _Pivot),
		(	Dispersion > BestDispersion ->
			true
		;	Dispersion =:= BestDispersion,
			Range > BestRange
		),
		!.
	better_metric_pivot(_Score, Best, Best).

	decorate_by_distance([], _Vector, _Options, []).
	decorate_by_distance([Id-Vector| Rows], Pivot, Options, [Distance-(Id-Vector)| DecoratedRows]) :-
		distance(Options, Pivot, Vector, Distance),
		decorate_by_distance(Rows, Pivot, Options, DecoratedRows).


	search_index_cell_size(Options, CellSize) :-
		^^option(ordering_and_extraction_epsilons(CellSize, _), Options).

	region_query(_Id-Vector, SearchIndex, Options, Neighbors) :-
		^^option(ordering_and_extraction_epsilons(MaximumOrderingEpsilon, _), Options),
		^^range_query(SearchIndex, Vector, Options, MaximumOrderingEpsilon, NeighborRows),
		decorate_neighbors(NeighborRows, Vector, Options, Neighbors).

	decorate_neighbors([], _Vector, _Options, []).
	decorate_neighbors([Id-NeighborVector| NeighborRows], Vector, Options, [neighbor(Id, NeighborVector, Distance)| Neighbors]) :-
		distance(Options, Vector, NeighborVector, Distance),
		decorate_neighbors(NeighborRows, Vector, Options, Neighbors).

	neighbor_distances([], []).
	neighbor_distances([neighbor(_Id, _Vector, Distance)| Neighbors], Distances) :-
		neighbor_distances(Neighbors, Distances0),
		insert_sorted(Distance, Distances0, Distances).

	insert_sorted(Distance, [], [Distance]) :-
		!.
	insert_sorted(Distance, [Current| Distances], [Distance, Current| Distances]) :-
		Distance =< Current,
		!.
	insert_sorted(Distance, [Current| Distances], [Current| Sorted]) :-
		insert_sorted(Distance, Distances, Sorted).

	core_distance(Distances, Options, CoreDistance) :-
		^^option(minimum_points(MinimumPoints), Options),
		(	nth1(MinimumPoints, Distances, CoreDistance0) ->
			CoreDistance = CoreDistance0
		;	CoreDistance = none
		).

	update_seeds([], _CoreDistance, _Processed, Seeds, Seeds).
	update_seeds([neighbor(NeighborId, NeighborVector, Distance)| Neighbors], CoreDistance, Processed, Seeds0, Seeds) :-
		(	processed(NeighborId, Processed) ->
			Seeds1 = Seeds0
		;	Reachability is max(CoreDistance, Distance),
			update_seed(Seeds0, NeighborId, Reachability, NeighborVector, Seeds1)
		),
		update_seeds(Neighbors, CoreDistance, Processed, Seeds1, Seeds).

	processed(Id, Processed) :-
		avltree::lookup(Id, true, Processed).

	mark_processed(Id, Processed0, Processed) :-
		avltree::insert(Processed0, Id, true, Processed).

	empty_seed_queue(seed_queue(Heap, Entries, 0)) :-
		binary_heap_min::new(Heap),
		avltree::new(Entries).

	update_seed(seed_queue(Heap0, Entries0, NextOrdinal0), Id, Reachability, Vector, seed_queue(Heap, Entries, NextOrdinal)) :-
		(	avltree::lookup(Id, seed_entry(Reachability0, Ordinal, _CurrentVector), Entries0) ->
			(	Reachability < Reachability0 ->
				seed_key(Reachability, Ordinal, Key),
				binary_heap_min::insert(Key, seed(Id, Reachability, Ordinal, Vector), Heap0, Heap),
				avltree::insert(Entries0, Id, seed_entry(Reachability, Ordinal, Vector), Entries),
				NextOrdinal = NextOrdinal0
			;	Heap = Heap0,
				Entries = Entries0,
				NextOrdinal = NextOrdinal0
			)
		;	Ordinal = NextOrdinal0,
			seed_key(Reachability, Ordinal, Key),
			binary_heap_min::insert(Key, seed(Id, Reachability, Ordinal, Vector), Heap0, Heap),
			avltree::insert(Entries0, Id, seed_entry(Reachability, Ordinal, Vector), Entries),
			NextOrdinal is NextOrdinal0 + 1
		).

	seed_key(Reachability, Ordinal, Reachability-Ordinal).

	extract_best_seed(seed_queue(Heap0, Entries0, NextOrdinal), Seed, seed_queue(Heap, Entries, NextOrdinal)) :-
		binary_heap_min::delete(Heap0, _Key, seed(Id, Reachability, Ordinal, Vector), Heap1),
		(	avltree::lookup(Id, seed_entry(CurrentReachability, Ordinal, CurrentVector), Entries0),
			Reachability == CurrentReachability,
			Vector == CurrentVector ->
			Seed = seed(Id, Reachability, Vector),
			avltree::delete(Entries0, Id, seed_entry(CurrentReachability, Ordinal, CurrentVector), Entries),
			Heap = Heap1
		;	extract_best_seed(seed_queue(Heap1, Entries0, NextOrdinal), Seed, seed_queue(Heap, Entries, NextOrdinal))
		).

	extract_clusters(Ordering, Options, Clusters, Noise) :-
		^^option(ordering_and_extraction_epsilons(_, ExtractionEpsilon), Options),
		scan_ordering(Ordering, ExtractionEpsilon, none, 0, Clusters, Noise).

	scan_ordering([], _ExtractionEpsilon, none, _ClusterCount, [], []) :-
		!.
	scan_ordering([], _ExtractionEpsilon, current(ClusterId, CorePoints, BorderPoints), _ClusterCount, [Cluster], []) :-
		build_cluster(ClusterId, CorePoints, BorderPoints, Cluster).
	scan_ordering([ordered(_Id, Reachability, CoreDistance, Vector)| Ordering], ExtractionEpsilon, Current0, ClusterCount0, Clusters, Noise) :-
		(	start_new_cluster(Reachability, CoreDistance, ExtractionEpsilon) ->
			ClusterId is ClusterCount0 + 1,
			Current1 = current(ClusterId, [Vector], []),
			scan_ordering(Ordering, ExtractionEpsilon, Current1, ClusterId, RemainingClusters, Noise),
			prepend_finalized_cluster(Current0, RemainingClusters, Clusters)
		;	continues_cluster(Current0, Reachability, ExtractionEpsilon) ->
			add_current_point(Current0, CoreDistance, Vector, ExtractionEpsilon, Current1),
			scan_ordering(Ordering, ExtractionEpsilon, Current1, ClusterCount0, Clusters, Noise)
		;	Noise = [Vector| RemainingNoise],
			scan_ordering(Ordering, ExtractionEpsilon, none, ClusterCount0, RemainingClusters, RemainingNoise),
			prepend_finalized_cluster(Current0, RemainingClusters, Clusters)
		).

	start_new_cluster(Reachability, CoreDistance, ExtractionEpsilon) :-
		is_core(CoreDistance, ExtractionEpsilon),
		(	Reachability == none ->
			true
		;	Reachability > ExtractionEpsilon
		).

	continues_cluster(current(_, _, _), Reachability, ExtractionEpsilon) :-
		Reachability \== none,
		Reachability =< ExtractionEpsilon.

	is_core(CoreDistance, ExtractionEpsilon) :-
		CoreDistance \== none,
		CoreDistance =< ExtractionEpsilon.

	add_current_point(current(ClusterId, CorePoints0, BorderPoints0), CoreDistance, Vector, ExtractionEpsilon, current(ClusterId, CorePoints, BorderPoints)) :-
		(	is_core(CoreDistance, ExtractionEpsilon) ->
			CorePoints = [Vector| CorePoints0],
			BorderPoints = BorderPoints0
		;	CorePoints = CorePoints0,
			BorderPoints = [Vector| BorderPoints0]
		).

	prepend_finalized_cluster(none, Clusters, Clusters).
	prepend_finalized_cluster(current(ClusterId, CorePoints, BorderPoints), Clusters, [Cluster| Clusters]) :-
		build_cluster(ClusterId, CorePoints, BorderPoints, Cluster).

	build_cluster(ClusterId, CorePoints, BorderPoints, cluster(ClusterId, CorePoints, BorderPoints, bounds(Minimums, Maximums))) :-
		core_point_bounds(CorePoints, Minimums, Maximums).

	core_point_bounds([Vector| Vectors], Minimums, Maximums) :-
		core_point_bounds(Vectors, Vector, Vector, Minimums, Maximums).

	core_point_bounds([], Minimums, Maximums, Minimums, Maximums).
	core_point_bounds([Vector| Vectors], Minimums0, Maximums0, Minimums, Maximums) :-
		update_bounds(Vector, Minimums0, Maximums0, Minimums1, Maximums1),
		core_point_bounds(Vectors, Minimums1, Maximums1, Minimums, Maximums).

	update_bounds([], [], [], [], []).
	update_bounds([Value| Values], [Minimum0| Minimums0], [Maximum0| Maximums0], [Minimum| Minimums], [Maximum| Maximums]) :-
		Minimum is min(Minimum0, Value),
		Maximum is max(Maximum0, Value),
		update_bounds(Values, Minimums0, Maximums0, Minimums, Maximums).

	classify_cluster(Clusters, Features, Options, Cluster) :-
		^^option(ordering_and_extraction_epsilons(_, ExtractionEpsilon), Options),
		classify_clusters(Clusters, Features, Options, ExtractionEpsilon, none, Cluster).

	classify_clusters([], _Features, _Options, _ExtractionEpsilon, none, noise) :-
		!.
	classify_clusters([], _Features, _Options, _ExtractionEpsilon, candidate(Cluster, _Distance), Cluster).
	classify_clusters([ClusterData| Clusters], Features, Options, ExtractionEpsilon, Best0, Cluster) :-
		classify_cluster_candidate(ClusterData, Features, Options, ExtractionEpsilon, Best0, Best1),
		classify_clusters(Clusters, Features, Options, ExtractionEpsilon, Best1, Cluster).

	classify_cluster_candidate(cluster(CandidateCluster, CorePoints, _BorderPoints, Bounds), Features, Options, ExtractionEpsilon, Best0, Best) :-
		best_distance_limit(Best0, ExtractionEpsilon, DistanceLimit),
		lower_bound_distance(Bounds, Features, Options, LowerBound),
		(	LowerBound =< DistanceLimit,
			nearest_reachable_core_distance(CorePoints, Features, Options, DistanceLimit, Distance) ->
			update_best_candidate(CandidateCluster, Distance, Best0, Best)
		;	Best = Best0
		).

	best_distance_limit(none, ExtractionEpsilon, ExtractionEpsilon).
	best_distance_limit(candidate(_Cluster, CurrentDistance), ExtractionEpsilon, DistanceLimit) :-
		DistanceLimit is min(CurrentDistance, ExtractionEpsilon).

	update_best_candidate(Cluster, Distance, none, candidate(Cluster, Distance)) :-
		!.
	update_best_candidate(Cluster, Distance, candidate(CurrentCluster, CurrentDistance), Best) :-
		(	Distance < CurrentDistance ->
			Best = candidate(Cluster, Distance)
		;	Best = candidate(CurrentCluster, CurrentDistance)
		).

	nearest_reachable_core_distance(CorePoints, Features, Options, DistanceLimit, Distance) :-
		nearest_reachable_core_distance(CorePoints, Features, Options, DistanceLimit, none, Distance).

	nearest_reachable_core_distance([], _Features, _Options, _DistanceLimit, none, _Distance) :-
		fail.
	nearest_reachable_core_distance([], _Features, _Options, _DistanceLimit, Distance, Distance).
	nearest_reachable_core_distance([CorePoint| CorePoints], Features, Options, DistanceLimit, CurrentDistance0, Distance) :-
		distance(Options, Features, CorePoint, CandidateDistance),
		update_reachable_distance(CandidateDistance, DistanceLimit, CurrentDistance0, CurrentDistance1),
		nearest_reachable_core_distance(CorePoints, Features, Options, DistanceLimit, CurrentDistance1, Distance).

	update_reachable_distance(CandidateDistance, DistanceLimit, none, CandidateDistance) :-
		CandidateDistance =< DistanceLimit,
		!.
	update_reachable_distance(CandidateDistance, DistanceLimit, CurrentDistance0, CurrentDistance) :-
		CandidateDistance =< DistanceLimit,
		!,
		CurrentDistance is min(CurrentDistance0, CandidateDistance).
	update_reachable_distance(_CandidateDistance, _DistanceLimit, CurrentDistance, CurrentDistance).

	lower_bound_distance(bounds(Minimums, Maximums), Features, Options, Distance) :-
		^^option(distance_metric(Metric), Options),
		lower_bound_distance(Metric, Minimums, Maximums, Features, Distance).

	lower_bound_distance(euclidean, Minimums, Maximums, Features, Distance) :-
		lower_bound_distance_squared(Minimums, Maximums, Features, 0.0, DistanceSquared),
		Distance is sqrt(DistanceSquared).
	lower_bound_distance(manhattan, Minimums, Maximums, Features, Distance) :-
		lower_bound_distance_manhattan(Minimums, Maximums, Features, 0.0, Distance).

	lower_bound_distance_squared([], [], [], DistanceSquared, DistanceSquared).
	lower_bound_distance_squared([Minimum| Minimums], [Maximum| Maximums], [Value| Values], DistanceSquared0, DistanceSquared) :-
		bounded_delta(Minimum, Maximum, Value, Delta),
		DistanceSquared1 is DistanceSquared0 + Delta * Delta,
		lower_bound_distance_squared(Minimums, Maximums, Values, DistanceSquared1, DistanceSquared).

	lower_bound_distance_manhattan([], [], [], Distance, Distance).
	lower_bound_distance_manhattan([Minimum| Minimums], [Maximum| Maximums], [Value| Values], Distance0, Distance) :-
		bounded_delta(Minimum, Maximum, Value, Delta),
		Distance1 is Distance0 + Delta,
		lower_bound_distance_manhattan(Minimums, Maximums, Values, Distance1, Distance).

	bounded_delta(Minimum, _Maximum, Value, Delta) :-
		Value < Minimum,
		!,
		Delta is Minimum - Value.
	bounded_delta(_Minimum, Maximum, Value, Delta) :-
		Value > Maximum,
		!,
		Delta is Value - Maximum.
	bounded_delta(_Minimum, _Maximum, _Value, 0.0).

	distance(Options, Vector1, Vector2, Distance) :-
		^^option(distance_metric(Metric), Options),
		distance_metric(Metric, Vector1, Vector2, Distance).

	distance_metric(euclidean, Vector1, Vector2, Distance) :-
		euclidean_distance(Vector1, Vector2, Distance).
	distance_metric(manhattan, Vector1, Vector2, Distance) :-
		manhattan_distance(Vector1, Vector2, Distance).

	print_clusterer(optics_clusterer(Encoders, Ordering, Clusters, Noise, Options)) :-
		format('OPTICS Clusterer~n', []),
		format('================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nOrdering:~n', []),
		print_ordering(Ordering),
		format('~nClusters:~n', []),
		print_clusters(Clusters),
		length(Noise, NoiseCount),
		format('~nNoise (~d): ~w~n', [NoiseCount, Noise]).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		format('  ~w (continuous, mean=~4f, scale=~4f)~n', [Attribute, Mean, Scale]), print_encoders(Encoders).

	print_ordering([]).
	print_ordering([ordered(Id, Reachability, CoreDistance, _)| Ordering]) :-
		format('  ~w reachability=~w core_distance=~w~n', [Id, Reachability, CoreDistance]), print_ordering(Ordering).

	print_clusters([]).
	print_clusters([cluster(ClusterId, CorePoints, BorderPoints, _Bounds)| Clusters]) :-
		length(CorePoints, CoreCount),
		length(BorderPoints, BorderCount),
		format('  cluster ~d: ~d core points, ~d border points~n', [ClusterId, CoreCount, BorderCount]),
		print_clusters(Clusters).

	default_option(ordering_and_extraction_epsilons(1.0, 1.0)).
	default_option(search_index(auto)).
	default_option(minimum_points(2)).
	default_option(distance_metric(euclidean)).
	default_option(feature_scaling(on)).

	valid_option(ordering_and_extraction_epsilons(MaximumOrderingEpsilon, ExtractionEpsilon)) :-
		valid(positive_number, MaximumOrderingEpsilon),
		valid(positive_number, ExtractionEpsilon),
		ExtractionEpsilon =< MaximumOrderingEpsilon.
	valid_option(search_index(SearchIndex)) :-
		once((SearchIndex == auto; SearchIndex == grid; SearchIndex == metric_tree)).
	valid_option(minimum_points(MinimumPoints)) :-
		valid(positive_integer, MinimumPoints).
	valid_option(distance_metric(Metric)) :-
		once((Metric == euclidean; Metric == manhattan)).
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).

:- end_object.
