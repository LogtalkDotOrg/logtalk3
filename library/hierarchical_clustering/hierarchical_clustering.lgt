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


:- object(hierarchical_clustering,
	imports(clusterer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-24,
		comment is 'Hierarchical clusterer for continuous datasets. Learns a full agglomerative merge tree from a dataset object implementing the ``clustering_dataset_protocol`` protocol and then cuts the hierarchy to the requested number of clusters for prediction and export.',
		remarks is [
			'Algorithm' - 'Builds the full bottom-up agglomerative hierarchy and derives the requested partition by cutting the learned dendrogram at the largest remaining merge distances.',
			'Feature handling' - 'Supports continuous attributes only. Continuous attributes can be standardized using z-score scaling before clustering.',
			'Linkage strategies' - 'Supports ``single``, ``complete``, and ``average`` linkage.',
			'Distance metrics' - 'Supports Euclidean and Manhattan distances.',
			'Re-cutting' - 'Supports re-cutting a learned hierarchy to a different number of clusters using ``cut/3`` without retraining.',
			'Tie-breaking' - 'When merge distances or cut heights are equal, the implementation deterministically prefers the smallest node-id pair.',
			'Prediction' - 'New instances are assigned to the nearest learned cluster using the selected linkage strategy and distance metric.',
			'Clusterer representation' - 'The learned clusterer is represented by default as ``hierarchical_clustering_clusterer(Encoders, hierarchy(RootState, MergeRecords, Dendrogram), Clusters, Prototypes, Diagnostics)`` where ``hierarchy(...)`` stores the reusable merge state for re-cutting, ``Clusters`` stores the partition obtained by cutting the hierarchy to ``k`` clusters, ``Prototypes`` stores average vectors for display and export metadata, and ``Diagnostics`` stores the effective options and training metadata.'
		],
		see_also is [clusterer_protocol, clustering_dataset_protocol, agglomerative]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(avltree, [
		as_dictionary/2, delete/4, insert/4, lookup/3
	]).

	:- uses(binary_heap_min, [
		delete/4 as heap_delete/4,
		insert/4 as heap_insert/4,
		new/1 as heap_new/1
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, nth1/3
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

	:- public(cut/3).
	:- mode(cut(+compound, +integer, -compound), one).
	:- info(cut/3, [
		comment is 'Re-cuts a learned hierarchy to a new number of clusters without retraining.',
		argnames is ['Clusterer', 'K', 'RecutClusterer']
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
		length(Rows, Count),
		next_internal_node_id(Examples, NextNodeId),
		^^option(k(K), Options),
		^^check_cluster_count(K, Count),
		initial_states(Rows, InitialStates),
		build_distance_cache(InitialStates, Options, Distances),
		build_pair_queue(InitialStates, Distances, PairQueue),
		build_active_nodes(InitialStates, ActiveNodes),
		build_hierarchy(InitialStates, Options, NextNodeId, Distances, PairQueue, ActiveNodes, runtime_stats(0, 0), RuntimeStats, MergeRecords, RootState, FinalPairQueue),
		pair_queue_max(FinalPairQueue, MaximumHeapSize),
		make_clusterer(hierarchical_clustering_clusterer, Encoders, RootState, MergeRecords, Options, RuntimeStats, MaximumHeapSize, Clusterer).

	cut(Clusterer, K, RecutClusterer) :-
		clusterer_data(Clusterer, Encoders, hierarchy(RootState, MergeRecords, _Dendrogram), _Clusters, _Prototypes, Diagnostics),
		memberchk(training_example_count(Count), Diagnostics),
		^^check_cluster_count(K, Count),
		diagnostics_options(Diagnostics, CurrentOptions),
		replace_k_option(CurrentOptions, K, Options),
		diagnostics_runtime_stats(Diagnostics, RuntimeStats, MaximumHeapSize),
		clusterer_functor(Clusterer, Functor),
		make_clusterer(Functor, Encoders, RootState, MergeRecords, Options, RuntimeStats, MaximumHeapSize, RecutClusterer).

	cluster(Clusterer, Instance, Cluster) :-
		clusterer_data(Clusterer, Encoders, _Hierarchy, Clusters, _Prototypes, Diagnostics),
		diagnostics_options(Diagnostics, Options),
		^^encode_instance(Encoders, Instance, Features),
		nearest_cluster(Clusters, Features, Options, Cluster, _Distance).

	clusterer_diagnostics_data(Clusterer, Diagnostics) :-
		clusterer_data(Clusterer, _Encoders, _Hierarchy, _Clusters, _Prototypes, Diagnostics).

	clusterer_functor(Clusterer, Functor) :-
		functor(Clusterer, Functor, 5).

	clusterer_data(Clusterer, Encoders, Hierarchy, Clusters, Prototypes, Diagnostics) :-
		Clusterer =.. [_Functor, Encoders, Hierarchy, Clusters, Prototypes, Diagnostics].

	make_clusterer(Functor, Encoders, RootState, MergeRecords, Options, RuntimeStats, MaximumHeapSize, Clusterer) :-
		^^option(k(K), Options),
		cut_hierarchy(RootState, MergeRecords, K, FinalStates0),
		order_cluster_states(FinalStates0, FinalStates),
		states_to_clusters(FinalStates, 1, Clusters, Prototypes),
		state_to_dendrogram(RootState, MergeRecords, Dendrogram),
		build_diagnostics(RootState, Clusters, Prototypes, Options, RuntimeStats, MaximumHeapSize, Diagnostics),
		Clusterer =.. [Functor, Encoders, hierarchy(RootState, MergeRecords, Dendrogram), Clusters, Prototypes, Diagnostics].

	build_diagnostics(cluster_state(_NodeId, _Members, Height, TrainingExampleCount), Clusters, Prototypes, Options, runtime_stats(HeapRebuildCount, ScanFallbackCount), MaximumHeapSize, Diagnostics) :-
		length(Clusters, ClusterCount),
		length(Prototypes, PrototypeCount),
		MergeCount is TrainingExampleCount - 1,
		Diagnostics = [
			model(hierarchical_clustering),
			cluster_count(ClusterCount),
			prototype_count(PrototypeCount),
			training_example_count(TrainingExampleCount),
			merge_count(MergeCount),
			dendrogram_height(Height),
			heap_rebuild_count(HeapRebuildCount),
			scan_fallback_count(ScanFallbackCount),
			maximum_heap_size(MaximumHeapSize),
			tie_breaking(node_id_order),
			options(Options)
		].

	diagnostics_options(Diagnostics, Options) :-
		memberchk(options(Options), Diagnostics).

	diagnostics_runtime_stats(Diagnostics, runtime_stats(HeapRebuildCount, ScanFallbackCount), MaximumHeapSize) :-
		memberchk(heap_rebuild_count(HeapRebuildCount), Diagnostics),
		memberchk(scan_fallback_count(ScanFallbackCount), Diagnostics),
		memberchk(maximum_heap_size(MaximumHeapSize), Diagnostics).

	replace_k_option([], K, [k(K)]).
	replace_k_option([k(_)| Options], K, [k(K)| Options]) :-
		!.
	replace_k_option([Option| Options], K, [Option| UpdatedOptions]) :-
		replace_k_option(Options, K, UpdatedOptions).

	next_internal_node_id(Examples, NextNodeId) :-
		maximum_example_id(Examples, 0, MaximumId),
		NextNodeId is MaximumId + 1.

	maximum_example_id([], MaximumId, MaximumId).
	maximum_example_id([Id-_AttributeValues| Examples], MaximumId0, MaximumId) :-
		(   Id > MaximumId0 ->
			MaximumId1 = Id
		;   MaximumId1 = MaximumId0
		),
		maximum_example_id(Examples, MaximumId1, MaximumId).

	initial_states([], []).
	initial_states([Id-Vector| Rows], [cluster_state(Id, [Id-Vector], 0.0, 1)| States]) :-
		initial_states(Rows, States).

	build_active_nodes(States, ActiveNodes) :-
		active_node_pairs(States, Pairs),
		as_dictionary(Pairs, ActiveNodes).

	active_node_pairs([], []).
	active_node_pairs([cluster_state(NodeId, _Members, _Height, _Size)| States], [NodeId-true| Pairs]) :-
		active_node_pairs(States, Pairs).

	build_distance_cache(States, Options, Distances) :-
		build_distance_pairs(States, Options, 1, Pairs),
		as_dictionary(Pairs, Distances).

	build_distance_pairs([], _Options, _Index, []).
	build_distance_pairs([State| States], Options, Index, Pairs) :-
		NextIndex is Index + 1,
		build_state_distance_pairs(States, State, Options, Index, NextIndex, StatePairs),
		append(StatePairs, RemainingPairs, Pairs),
		build_distance_pairs(States, Options, NextIndex, RemainingPairs).

	build_state_distance_pairs([], _State, _Options, _Index, _NextIndex, []).
	build_state_distance_pairs([State| States], cluster_state(NodeId1, [Id1-Vector1], _Height1, _Size1), Options, _Index, _NextIndex, [Key-Distance| Pairs]) :-
		State = cluster_state(NodeId2, [Id2-Vector2], _Height2, _Size2),
		distance_key(NodeId1, NodeId2, Key),
		base_distance(Options, Id1-Vector1, Id2-Vector2, Distance),
		build_state_distance_pairs(States, cluster_state(NodeId1, [Id1-Vector1], _Height1, _Size1), Options, _Index, _NextIndex, Pairs).

	base_distance(Options, _Id1-Vector1, _Id2-Vector2, Distance) :-
		distance(Options, Vector1, Vector2, Distance).

	build_pair_queue(States, Distances, PairQueue) :-
		heap_new(Heap),
		build_pair_queue(States, Distances, pair_queue(Heap, 0, 0), PairQueue).

	build_pair_queue([], _Distances, PairQueue, PairQueue).
	build_pair_queue([State| States], Distances, PairQueue0, PairQueue) :-
		build_state_pair_queue(States, Distances, State, PairQueue0, PairQueue1),
		build_pair_queue(States, Distances, PairQueue1, PairQueue).

	build_state_pair_queue([], _Distances, _State, PairQueue, PairQueue).
	build_state_pair_queue([OtherState| States], Distances, State, PairQueue0, PairQueue) :-
		cached_state_distance(Distances, State, OtherState, Distance),
		state_node_id(State, NodeId1),
		state_node_id(OtherState, NodeId2),
		insert_pair_queue(PairQueue0, NodeId1, NodeId2, Distance, PairQueue1),
		build_state_pair_queue(States, Distances, State, PairQueue1, PairQueue).

	insert_pair_queue(pair_queue(Heap0, Size0, MaximumSize0), NodeId1, NodeId2, Distance, pair_queue(Heap, Size, MaximumSize)) :-
		ordered_node_pair(NodeId1, NodeId2, LeftNodeId, RightNodeId),
		pair_key(Distance, LeftNodeId, RightNodeId, Key),
		heap_insert(Key, pair(LeftNodeId, RightNodeId), Heap0, Heap),
		Size is Size0 + 1,
		(   Size > MaximumSize0 ->
			MaximumSize = Size
		;   MaximumSize = MaximumSize0
		).

	pair_key(Distance, LeftNodeId, RightNodeId, key(Distance, LeftNodeId, RightNodeId)).

	ordered_node_pair(NodeId1, NodeId2, NodeId1, NodeId2) :-
		NodeId1 =< NodeId2,
		!.
	ordered_node_pair(NodeId1, NodeId2, NodeId2, NodeId1).

	pair_queue_max(pair_queue(_Heap, _Size, MaximumSize), MaximumSize).

	build_hierarchy([State], _Options, _NextNodeId, _Distances, PairQueue, _ActiveNodes, RuntimeStats, RuntimeStats, [], State, PairQueue) :-
		!.
	build_hierarchy(States0, Options, NextNodeId0, Distances0, PairQueue0, ActiveNodes0, RuntimeStats0, RuntimeStats, [MergeRecord| MergeRecords], RootState, PairQueue) :-
		select_next_pair(States0, Distances0, PairQueue0, ActiveNodes0, ActiveNodes, RuntimeStats0, RuntimeStats1, LeftNodeId, RightNodeId, Distance, PairQueue1),
		merge_states(States0, LeftNodeId, RightNodeId, NextNodeId0, Distance, States1, MergeRecord, LeftState, RightState, MergedState),
		update_distance_structures(States1, Options, Distances0, PairQueue1, LeftState, RightState, MergedState, Distances1, PairQueue2),
		update_active_nodes(ActiveNodes, LeftState, RightState, MergedState, ActiveNodes1),
		NextNodeId1 is NextNodeId0 + 1,
		build_hierarchy(States1, Options, NextNodeId1, Distances1, PairQueue2, ActiveNodes1, RuntimeStats1, RuntimeStats, MergeRecords, RootState, PairQueue).

	select_next_pair(States, Distances, PairQueue0, ActiveNodes0, ActiveNodes, RuntimeStats0, RuntimeStats, LeftNodeId, RightNodeId, Distance, PairQueue) :-
		(   select_closest_pair(PairQueue0, ActiveNodes0, LeftNodeId, RightNodeId, Distance, PairQueue) ->
			ActiveNodes = ActiveNodes0,
			RuntimeStats = RuntimeStats0
		;   build_active_nodes(States, RebuiltActiveNodes),
			rebuild_pair_queue(States, Distances, PairQueue0, RebuiltPairQueue),
			increment_heap_rebuild_count(RuntimeStats0, RuntimeStats1),
			(   select_closest_pair(RebuiltPairQueue, RebuiltActiveNodes, LeftNodeId, RightNodeId, Distance, PairQueue) ->
				ActiveNodes = RebuiltActiveNodes,
				RuntimeStats = RuntimeStats1
			;   select_closest_pair_by_scan(States, Distances, LeftNodeId, RightNodeId, Distance),
				ActiveNodes = RebuiltActiveNodes,
				PairQueue = RebuiltPairQueue,
				increment_scan_fallback_count(RuntimeStats1, RuntimeStats)
			)
		).

	rebuild_pair_queue(States, Distances, pair_queue(_OldHeap, _OldSize, PreviousMaximumSize), pair_queue(Heap, Size, MaximumSize)) :-
		build_pair_queue(States, Distances, pair_queue(Heap, Size, RebuiltMaximumSize)),
		(   RebuiltMaximumSize > PreviousMaximumSize ->
			MaximumSize = RebuiltMaximumSize
		;   MaximumSize = PreviousMaximumSize
		).

	increment_heap_rebuild_count(runtime_stats(HeapRebuildCount0, ScanFallbackCount), runtime_stats(HeapRebuildCount, ScanFallbackCount)) :-
		HeapRebuildCount is HeapRebuildCount0 + 1.

	increment_scan_fallback_count(runtime_stats(HeapRebuildCount, ScanFallbackCount0), runtime_stats(HeapRebuildCount, ScanFallbackCount)) :-
		ScanFallbackCount is ScanFallbackCount0 + 1.

	select_closest_pair(pair_queue(Heap0, Size0, MaximumSize), ActiveNodes, LeftNodeId, RightNodeId, Distance, pair_queue(Heap, Size, MaximumSize)) :-
		heap_delete(Heap0, key(Distance, LeftNodeId, RightNodeId), pair(LeftNodeId, RightNodeId), Heap1),
		Size1 is Size0 - 1,
		(   active_node(ActiveNodes, LeftNodeId),
			active_node(ActiveNodes, RightNodeId) ->
			Heap = Heap1,
			Size = Size1
		;   select_closest_pair(pair_queue(Heap1, Size1, MaximumSize), ActiveNodes, LeftNodeId, RightNodeId, Distance, pair_queue(Heap, Size, MaximumSize))
		).

	select_closest_pair_by_scan([State| States], Distances, LeftNodeId, RightNodeId, Distance) :-
		state_node_id(State, NodeId1),
		select_state_pair_by_scan(States, Distances, NodeId1, State, LeftNodeId0, RightNodeId0, Distance0),
		select_closest_pair_by_scan(States, Distances, LeftNodeId0, RightNodeId0, Distance0, LeftNodeId, RightNodeId, Distance).

	select_closest_pair_by_scan([], _Distances, LeftNodeId, RightNodeId, Distance, LeftNodeId, RightNodeId, Distance).
	select_closest_pair_by_scan([_State], _Distances, LeftNodeId, RightNodeId, Distance, LeftNodeId, RightNodeId, Distance) :-
		!.
	select_closest_pair_by_scan([State| States], Distances, BestLeftNodeId0, BestRightNodeId0, BestDistance0, LeftNodeId, RightNodeId, Distance) :-
		state_node_id(State, NodeId1),
		select_state_pair_by_scan(States, Distances, NodeId1, State, LeftNodeId1, RightNodeId1, Distance1),
		prefer_pair(LeftNodeId1, RightNodeId1, Distance1, BestLeftNodeId0, BestRightNodeId0, BestDistance0, BestLeftNodeId1, BestRightNodeId1, BestDistance1),
		select_closest_pair_by_scan(States, Distances, BestLeftNodeId1, BestRightNodeId1, BestDistance1, LeftNodeId, RightNodeId, Distance).

	select_state_pair_by_scan([OtherState| States], Distances, LeftNodeId, State, BestLeftNodeId, BestRightNodeId, BestDistance) :-
		state_node_id(OtherState, OtherNodeId),
		ordered_node_pair(LeftNodeId, OtherNodeId, BestLeftNodeId0, BestRightNodeId0),
		cached_state_distance(Distances, State, OtherState, Distance),
		select_state_pair_by_scan(States, Distances, LeftNodeId, State, BestLeftNodeId0, BestRightNodeId0, Distance, BestLeftNodeId, BestRightNodeId, BestDistance).

	select_state_pair_by_scan([], _Distances, _LeftNodeId, _State, BestLeftNodeId, BestRightNodeId, BestDistance, BestLeftNodeId, BestRightNodeId, BestDistance).
	select_state_pair_by_scan([OtherState| States], Distances, LeftNodeId, State, CurrentLeftNodeId, CurrentRightNodeId, CurrentDistance, BestLeftNodeId, BestRightNodeId, BestDistance) :-
		state_node_id(OtherState, OtherNodeId),
		ordered_node_pair(LeftNodeId, OtherNodeId, LeftNodeId1, RightNodeId1),
		cached_state_distance(Distances, State, OtherState, Distance),
		prefer_pair(LeftNodeId1, RightNodeId1, Distance, CurrentLeftNodeId, CurrentRightNodeId, CurrentDistance, NextLeftNodeId, NextRightNodeId, NextDistance),
		select_state_pair_by_scan(States, Distances, LeftNodeId, State, NextLeftNodeId, NextRightNodeId, NextDistance, BestLeftNodeId, BestRightNodeId, BestDistance).

	prefer_pair(LeftNodeId1, RightNodeId1, Distance1, LeftNodeId2, RightNodeId2, Distance2, LeftNodeId1, RightNodeId1, Distance1) :-
		(   Distance1 < Distance2
		;   Distance1 =:= Distance2,
			pair_precedes(LeftNodeId1, RightNodeId1, LeftNodeId2, RightNodeId2)
		),
		!.
	prefer_pair(_LeftNodeId1, _RightNodeId1, _Distance1, LeftNodeId2, RightNodeId2, Distance2, LeftNodeId2, RightNodeId2, Distance2).

	pair_precedes(LeftNodeId1, _RightNodeId1, LeftNodeId2, _RightNodeId2) :-
		LeftNodeId1 < LeftNodeId2.
	pair_precedes(LeftNodeId1, RightNodeId1, LeftNodeId2, RightNodeId2) :-
		LeftNodeId1 == LeftNodeId2,
		RightNodeId1 < RightNodeId2.

	active_node(ActiveNodes, NodeId) :-
		lookup(NodeId, true, ActiveNodes).

	update_active_nodes(ActiveNodes0, LeftState, RightState, MergedState, ActiveNodes) :-
		state_node_id(LeftState, LeftNodeId),
		state_node_id(RightState, RightNodeId),
		state_node_id(MergedState, MergedNodeId),
		delete(ActiveNodes0, LeftNodeId, true, ActiveNodes1),
		delete(ActiveNodes1, RightNodeId, true, ActiveNodes2),
		insert(ActiveNodes2, MergedNodeId, true, ActiveNodes).

	merge_states(States, LeftNodeId, RightNodeId, NextNodeId, Distance, MergedStates, MergeRecord, LeftState, RightState, MergedState) :-
		state_by_node_id(States, LeftNodeId, LeftState),
		state_by_node_id(States, RightNodeId, RightState),
		LeftState = cluster_state(_LeftNodeId, LeftMembers, _LeftHeight, LeftSize),
		RightState = cluster_state(_RightNodeId, RightMembers, _RightHeight, RightSize),
		append(LeftMembers, RightMembers, MergedMembers),
		MergedSize is LeftSize + RightSize,
		MergedState = cluster_state(NextNodeId, MergedMembers, Distance, MergedSize),
		MergeRecord = merge_node(NextNodeId, LeftState, RightState, Distance),
		merge_states_by_node_ids(States, LeftNodeId, RightNodeId, MergedState, false, MergedStates).

	state_by_node_id([State| _States], NodeId, State) :-
		state_node_id(State, NodeId),
		!.
	state_by_node_id([_State| States], NodeId, FoundState) :-
		state_by_node_id(States, NodeId, FoundState).

	state_node_id(cluster_state(NodeId, _Members, _Height, _Size), NodeId).

	merge_states_by_node_ids([], _LeftNodeId, _RightNodeId, _MergedState, _Inserted, []).
	merge_states_by_node_ids([State| States], LeftNodeId, RightNodeId, MergedState, Inserted0, MergedStates) :-
		state_node_id(State, NodeId),
		(   (NodeId == LeftNodeId; NodeId == RightNodeId) ->
			(   Inserted0 == false ->
				MergedStates = [MergedState| RestMergedStates],
				Inserted1 = true
			;   MergedStates = RestMergedStates,
				Inserted1 = Inserted0
			)
		;   MergedStates = [State| RestMergedStates],
			Inserted1 = Inserted0
		),
		merge_states_by_node_ids(States, LeftNodeId, RightNodeId, MergedState, Inserted1, RestMergedStates).

	update_distance_structures([], _Options, Distances, PairQueue, _LeftState, _RightState, _MergedState, Distances, PairQueue).
	update_distance_structures([State| States], Options, Distances0, PairQueue0, LeftState, RightState, MergedState, Distances, PairQueue) :-
		State = cluster_state(NodeId, _Members, _Height, _Size),
		MergedState = cluster_state(MergedNodeId, _MergedMembers, _MergedHeight, _MergedSize),
		(   NodeId == MergedNodeId ->
			Distances1 = Distances0,
			PairQueue1 = PairQueue0
		;   merged_distance(Options, Distances0, LeftState, RightState, State, Distance),
			distance_key(MergedNodeId, NodeId, Key),
			insert(Distances0, Key, Distance, Distances1),
			insert_pair_queue(PairQueue0, MergedNodeId, NodeId, Distance, PairQueue1)
		),
		update_distance_structures(States, Options, Distances1, PairQueue1, LeftState, RightState, MergedState, Distances, PairQueue).

	merged_distance(Options, Distances, LeftState, RightState, State, Distance) :-
		cached_state_distance(Distances, LeftState, State, LeftDistance),
		cached_state_distance(Distances, RightState, State, RightDistance),
		^^option(linkage(Linkage), Options),
		merge_distance(Linkage, LeftState, RightState, LeftDistance, RightDistance, Distance).

	merge_distance(single, _LeftState, _RightState, LeftDistance, RightDistance, Distance) :-
		(   LeftDistance =< RightDistance ->
			Distance = LeftDistance
		;   Distance = RightDistance
		).
	merge_distance(complete, _LeftState, _RightState, LeftDistance, RightDistance, Distance) :-
		(   LeftDistance >= RightDistance ->
			Distance = LeftDistance
		;   Distance = RightDistance
		).
	merge_distance(average, cluster_state(_LeftNodeId, _LeftMembers, _LeftHeight, LeftSize), cluster_state(_RightNodeId, _RightMembers, _RightHeight, RightSize), LeftDistance, RightDistance, Distance) :-
		TotalSize is LeftSize + RightSize,
		Distance is (LeftSize * LeftDistance + RightSize * RightDistance) / TotalSize.

	cached_state_distance(Distances, cluster_state(NodeId1, _Members1, _Height1, _Size1), cluster_state(NodeId2, _Members2, _Height2, _Size2), Distance) :-
		distance_key(NodeId1, NodeId2, Key),
		lookup(Key, Distance, Distances).

	distance_key(NodeId1, NodeId2, NodeId1-NodeId2) :-
		NodeId1 < NodeId2,
		!.
	distance_key(NodeId1, NodeId2, NodeId2-NodeId1).

	cut_hierarchy(RootState, MergeRecords, K, States) :-
		cut_hierarchy_states([RootState], MergeRecords, K, States).

	cut_hierarchy_states(States, _MergeRecords, K, States) :-
		length(States, K),
		!.
	cut_hierarchy_states(States0, MergeRecords, K, States) :-
		select_state_to_split(States0, MergeRecords, Index),
		split_state_at_index(States0, MergeRecords, Index, States1),
		cut_hierarchy_states(States1, MergeRecords, K, States).

	select_state_to_split(States, MergeRecords, BestIndex) :-
		select_state_to_split(States, MergeRecords, 1, 0, -1.0, 0, BestIndex).

	select_state_to_split([], _MergeRecords, _Current, BestIndex, _BestHeight, _BestNodeId, BestIndex) :-
		BestIndex > 0,
		!.
	select_state_to_split([cluster_state(NodeId, _Members, Height, _Size)| States], MergeRecords, Current, BestIndex0, BestHeight0, BestNodeId0, BestIndex) :-
		member(merge_node(NodeId, _LeftState, _RightState, _Distance), MergeRecords),
		!,
		(   Height > BestHeight0 ->
			NextBestIndex = Current,
			NextBestHeight = Height,
			NextBestNodeId = NodeId
		;   Height =:= BestHeight0,
			better_split_node(NodeId, BestNodeId0) ->
			NextBestIndex = Current,
			NextBestHeight = Height,
			NextBestNodeId = NodeId
		;   NextBestIndex = BestIndex0,
			NextBestHeight = BestHeight0,
			NextBestNodeId = BestNodeId0
		),
		NextCurrent is Current + 1,
		select_state_to_split(States, MergeRecords, NextCurrent, NextBestIndex, NextBestHeight, NextBestNodeId, BestIndex).
	select_state_to_split([_State| States], MergeRecords, Current, BestIndex0, BestHeight0, BestNodeId0, BestIndex) :-
		NextCurrent is Current + 1,
		select_state_to_split(States, MergeRecords, NextCurrent, BestIndex0, BestHeight0, BestNodeId0, BestIndex).

	better_split_node(_NodeId, BestNodeId) :-
		BestNodeId =:= 0,
		!.
	better_split_node(NodeId, BestNodeId) :-
		NodeId < BestNodeId.

	split_state_at_index(States, MergeRecords, Index, SplitStates) :-
		nth1(Index, States, State),
		split_state(State, MergeRecords, LeftState, RightState),
		split_state_at_index(States, 1, Index, LeftState, RightState, SplitStates).

	split_state(cluster_state(NodeId, _Members, _Height, _Size), MergeRecords, LeftState, RightState) :-
		memberchk(merge_node(NodeId, LeftState, RightState, _Distance), MergeRecords).

	split_state_at_index([], _Current, _Index, _LeftState, _RightState, []).
	split_state_at_index([_State| States], Index, Index, LeftState, RightState, [LeftState, RightState| SplitStates]) :-
		!,
		Next is Index + 1,
		split_state_at_index(States, Next, Index, LeftState, RightState, SplitStates).
	split_state_at_index([State| States], Current, Index, LeftState, RightState, [State| SplitStates]) :-
		Next is Current + 1,
		split_state_at_index(States, Next, Index, LeftState, RightState, SplitStates).

	order_cluster_states(States, OrderedStates) :-
		order_cluster_states(States, [], OrderedStates).

	order_cluster_states([], OrderedStates, OrderedStates).
	order_cluster_states([State| States], OrderedStates0, OrderedStates) :-
		insert_cluster_state(OrderedStates0, State, OrderedStates1),
		order_cluster_states(States, OrderedStates1, OrderedStates).

	insert_cluster_state([], State, [State]).
	insert_cluster_state([OtherState| States], State, [State, OtherState| States]) :-
		state_order_key(State, Key),
		state_order_key(OtherState, OtherKey),
		Key @=< OtherKey,
		!.
	insert_cluster_state([OtherState| States], State, [OtherState| OrderedStates]) :-
		insert_cluster_state(States, State, OrderedStates).

	state_order_key(cluster_state(NodeId, Members, _Height, _Size), MinimumId-NodeId) :-
		members_minimum_id(Members, MinimumId).

	members_minimum_id([Id-_Vector| Members], MinimumId) :-
		members_minimum_id(Members, Id, MinimumId).

	members_minimum_id([], MinimumId, MinimumId).
	members_minimum_id([Id-_Vector| Members], MinimumId0, MinimumId) :-
		(   Id < MinimumId0 ->
			MinimumId1 = Id
		;   MinimumId1 = MinimumId0
		),
		members_minimum_id(Members, MinimumId1, MinimumId).

	states_to_clusters([], _ClusterId, [], []).
	states_to_clusters([cluster_state(_NodeId, Members, _Height, _Size)| States], ClusterId, [cluster(ClusterId, Points)| Clusters], [Prototype| Prototypes]) :-
		member_vectors(Members, Points),
		average_vectors(Points, Prototype),
		NextClusterId is ClusterId + 1,
		states_to_clusters(States, NextClusterId, Clusters, Prototypes).

	member_vectors([], []).
	member_vectors([_Id-Vector| Members], [Vector| Vectors]) :-
		member_vectors(Members, Vectors).

	average_vectors([Vector| Vectors], Average) :-
		sum_vectors(Vectors, Vector, 1, Sum, Count),
		Factor is 1.0 / Count,
		scale_vector(Sum, Factor, Average).

	sum_vectors([], Sum, Count, Sum, Count).
	sum_vectors([Vector| Vectors], Sum0, Count0, Sum, Count) :-
		add_vectors(Sum0, Vector, Sum1),
		Count1 is Count0 + 1,
		sum_vectors(Vectors, Sum1, Count1, Sum, Count).

	add_vectors([], [], []).
	add_vectors([Left| Lefts], [Right| Rights], [Sum| Sums]) :-
		Sum is Left + Right,
		add_vectors(Lefts, Rights, Sums).

	scale_vector([], _, []).
	scale_vector([Value| Values], Factor, [Scaled| ScaledValues]) :-
		Scaled is Value * Factor,
		scale_vector(Values, Factor, ScaledValues).

	state_to_dendrogram(cluster_state(NodeId, _Members, _Height, Size), MergeRecords, Dendrogram) :-
		(   member(merge_node(NodeId, LeftState, RightState, Distance), MergeRecords) ->
			state_to_dendrogram(LeftState, MergeRecords, LeftDendrogram),
			state_to_dendrogram(RightState, MergeRecords, RightDendrogram),
			Dendrogram = merge(LeftDendrogram, RightDendrogram, Distance, Size)
		;   Dendrogram = leaf(NodeId)
		).

	nearest_cluster([cluster(Cluster, Points)| Clusters], Features, Options, BestCluster, BestDistance) :-
		cluster_distance(Options, Features, Points, InitialDistance),
		nearest_cluster(Clusters, Features, Options, Cluster, InitialDistance, BestCluster, BestDistance).

	nearest_cluster([], _Features, _Options, BestCluster, BestDistance, BestCluster, BestDistance).
	nearest_cluster([cluster(Cluster, Points)| Clusters], Features, Options, BestCluster0, BestDistance0, BestCluster, BestDistance) :-
		cluster_distance(Options, Features, Points, Distance0),
		(   Distance0 < BestDistance0 ->
			BestCluster1 = Cluster,
			BestDistance1 = Distance0
		;   BestCluster1 = BestCluster0,
			BestDistance1 = BestDistance0
		),
		nearest_cluster(Clusters, Features, Options, BestCluster1, BestDistance1, BestCluster, BestDistance).

	cluster_distance(Options, Features, Points, Distance) :-
		^^option(linkage(Linkage), Options),
		cluster_distance(Linkage, Features, Points, Options, Distance).

	cluster_distance(single, Features, [Point| Points], Options, Distance) :-
		distance(Options, Features, Point, InitialDistance),
		minimum_point_distance(Points, Features, Options, InitialDistance, Distance).
	cluster_distance(complete, Features, [Point| Points], Options, Distance) :-
		distance(Options, Features, Point, InitialDistance),
		maximum_point_distance(Points, Features, Options, InitialDistance, Distance).
	cluster_distance(average, Features, [Point| Points], Options, Distance) :-
		distance(Options, Features, Point, InitialDistance),
		sum_point_distances(Points, Features, Options, InitialDistance, 1, Sum, Count),
		Distance is Sum / Count.

	minimum_point_distance([], _Features, _Options, Distance, Distance).
	minimum_point_distance([Point| Points], Features, Options, CurrentDistance, Distance) :-
		distance(Options, Features, Point, CandidateDistance),
		(   CandidateDistance < CurrentDistance ->
			NextDistance = CandidateDistance
		;   NextDistance = CurrentDistance
		),
		minimum_point_distance(Points, Features, Options, NextDistance, Distance).

	maximum_point_distance([], _Features, _Options, Distance, Distance).
	maximum_point_distance([Point| Points], Features, Options, CurrentDistance, Distance) :-
		distance(Options, Features, Point, CandidateDistance),
		(   CandidateDistance > CurrentDistance ->
			NextDistance = CandidateDistance
		;   NextDistance = CurrentDistance
		),
		maximum_point_distance(Points, Features, Options, NextDistance, Distance).

	sum_point_distances([], _Features, _Options, Sum, Count, Sum, Count).
	sum_point_distances([Point| Points], Features, Options, Sum0, Count0, Sum, Count) :-
		distance(Options, Features, Point, Distance0),
		Sum1 is Sum0 + Distance0,
		Count1 is Count0 + 1,
		sum_point_distances(Points, Features, Options, Sum1, Count1, Sum, Count).

	distance(Options, Vector1, Vector2, Distance) :-
		^^option(distance_metric(Metric), Options),
		distance_metric(Metric, Vector1, Vector2, Distance).

	distance_metric(euclidean, Vector1, Vector2, Distance) :-
		euclidean_distance(Vector1, Vector2, Distance).
	distance_metric(manhattan, Vector1, Vector2, Distance) :-
		manhattan_distance(Vector1, Vector2, Distance).

	print_clusterer(Clusterer) :-
		clusterer_data(Clusterer, Encoders, hierarchy(_RootState, _MergeRecords, Dendrogram), Clusters, Prototypes, Diagnostics),
		diagnostics_options(Diagnostics, Options),
		format('Hierarchical Clustering Clusterer~n', []),
		format('=================================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Diagnostics: ~w~n~n', [Diagnostics]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nDendrogram: ~w~n', [Dendrogram]),
		format('~nClusters:~n', []),
		print_clusters(Clusters, Prototypes).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		format('  ~w (continuous, mean=~4f, scale=~4f)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).

	print_clusters([], []).
	print_clusters([cluster(ClusterId, Points)| Clusters], [Prototype| Prototypes]) :-
		length(Points, Count),
		format('  cluster ~d: ~d points, prototype=~w~n', [ClusterId, Count, Prototype]),
		print_clusters(Clusters, Prototypes).

	default_option(k(2)).
	default_option(linkage(average)).
	default_option(distance_metric(euclidean)).
	default_option(feature_scaling(on)).

	valid_option(k(K)) :-
		valid(positive_integer, K).
	valid_option(linkage(Linkage)) :-
		once((Linkage == single; Linkage == complete; Linkage == average)).
	valid_option(distance_metric(Metric)) :-
		once((Metric == euclidean; Metric == manhattan)).
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).

:- end_object.
