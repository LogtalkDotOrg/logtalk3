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


:- object(agglomerative,
	imports(clusterer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'Agglomerative clusterer for continuous datasets. Learns from a dataset object implementing the ``clustering_dataset_protocol`` protocol and returns a clusterer term that can be used for assigning new instances to clusters and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses deterministic bottom-up agglomerative clustering and stops when the requested number of clusters is reached.',
			'Feature handling' - 'Supports continuous attributes only. Continuous attributes can be standardized using z-score scaling before clustering.',
			'Linkage strategies' - 'Supports ``single``, ``complete``, and ``average`` linkage strategies.',
			'Distance metrics' - 'Supports Euclidean and Manhattan distances.',
			'Diagnostics' - 'Exposes training metadata including example count, merge count, initial pair count, maximum heap size, stale-pair discard count, prediction strategy, and deterministic heap-based pair selection.',
			'Prediction' - 'New instances are assigned to the nearest learned cluster using the selected linkage strategy and distance metric applied to the learned cluster members.',
			'Tie-breaking' - 'When two candidate merges have the same distance, the implementation deterministically prefers the smallest node-id pair and orders the final clusters by minimum training example id.',
			'Clusterer representation' - 'The learned clusterer is represented by default as ``agglomerative_clusterer(Encoders, Clusters, Prototypes, Options, Diagnostics)`` where ``Encoders`` stores the feature encoding metadata, ``Clusters`` stores the learned cluster members, ``Prototypes`` stores average vectors for display and export metadata, ``Options`` stores the effective training options, and ``Diagnostics`` stores training metadata.'
		],
		see_also is [clusterer_protocol, clustering_dataset_protocol, kcenters, kmeans, kmedoids]
	]).

	:- uses(avltree, [
		as_dictionary/2, delete/4, insert/4, lookup/3
	]).

	:- uses(binary_heap_min, [
		delete/4 as heap_delete/4, insert/4 as heap_insert/4, new/1 as heap_new/1
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2
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
		length(Rows, Count),
		^^option(k(K), Options),
		^^check_cluster_count(K, Count),
		next_internal_node_id(Examples, NextNodeId),
		initial_states(Rows, InitialStates),
		build_distance_cache(InitialStates, Options, Distances),
		build_pair_queue(InitialStates, Distances, PairQueue),
		build_active_nodes(InitialStates, ActiveNodes),
		agglomerate(InitialStates, K, Options, NextNodeId, Distances, PairQueue, ActiveNodes, runtime_stats(0), RuntimeStats, FinalPairQueue, FinalStates0),
		order_cluster_states(FinalStates0, FinalStates),
		states_to_clusters(FinalStates, 1, Clusters, Prototypes),
		pair_queue_max(FinalPairQueue, MaximumHeapSize),
		build_diagnostics(Clusters, Prototypes, Count, Options, RuntimeStats, MaximumHeapSize, Diagnostics),
		Clusterer = agglomerative_clusterer(Encoders, Clusters, Prototypes, Options, Diagnostics),
		!.

	cluster(Clusterer, Instance, Cluster) :-
		clusterer_data(Clusterer, Encoders, Clusters, _Prototypes, Options, _Diagnostics),
		^^encode_instance(Encoders, Instance, Features),
		nearest_cluster(Clusters, Features, Options, Cluster, _Distance).

	clusterer_data(Clusterer, Encoders, Clusters, Prototypes, Options, Diagnostics) :-
		Clusterer =.. [_Functor, Encoders, Clusters, Prototypes, Options, Diagnostics].

	clusterer_diagnostics_data(Clusterer, Diagnostics) :-
		clusterer_data(Clusterer, _Encoders, _Clusters, _Prototypes, _Options, Diagnostics).

	build_diagnostics(Clusters, Prototypes, TrainingExampleCount, Options, runtime_stats(StalePairDiscardCount), MaximumHeapSize, Diagnostics) :-
		length(Clusters, ClusterCount),
		length(Prototypes, PrototypeCount),
		MergeCount is TrainingExampleCount - ClusterCount,
		InitialPairCount is (TrainingExampleCount * (TrainingExampleCount - 1)) // 2,
		Diagnostics = [
			model(agglomerative),
			cluster_count(ClusterCount),
			prototype_count(PrototypeCount),
			training_example_count(TrainingExampleCount),
			merge_count(MergeCount),
			initial_pair_count(InitialPairCount),
			maximum_heap_size(MaximumHeapSize),
			stale_pair_discard_count(StalePairDiscardCount),
			pair_selection(priority_queue),
			prediction_strategy(cluster_member_linkage_distance),
			tie_breaking(node_id_order),
			options(Options)
		].

	check_clusterer(Clusterer) :-
		(   clusterer_data(Clusterer, Encoders, Clusters, Prototypes, Options, Diagnostics),
			length(Encoders, FeatureCount),
			^^valid_continuous_encoders(Encoders),
			valid_clusters(Clusters, FeatureCount, []),
			valid(list(list(float, FeatureCount)), Prototypes),
			length(Clusters, ClusterCount),
			length(Prototypes, ClusterCount),
			^^valid_clusterer_metadata(agglomerative, Options, Diagnostics),
			^^valid_diagnostic_count(cluster_count, Diagnostics, ClusterCount),
			^^valid_diagnostic_count(prototype_count, Diagnostics, ClusterCount) ->
			true
		;   domain_error(valid_clusterer, Clusterer)
		).

	valid_clusters([], _FeatureCount, _SeenIds).
	valid_clusters([cluster(ClusterId, Points)| Clusters], FeatureCount, SeenIds) :-
		valid(positive_integer, ClusterId),
		\+ memberchk(ClusterId, SeenIds),
		Points \== [],
		valid(list(list(number, FeatureCount)), Points),
		valid_clusters(Clusters, FeatureCount, [ClusterId| SeenIds]).

	pair_queue_max(pair_queue(_Heap, _Size, MaximumSize), MaximumSize).

	increment_stale_pair_discard_count(runtime_stats(StalePairDiscardCount0), runtime_stats(StalePairDiscardCount)) :-
		StalePairDiscardCount is StalePairDiscardCount0 + 1.

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
	initial_states([Id-Vector| Rows], [cluster_state(Id, [Id-Vector], 1)| States]) :-
		initial_states(Rows, States).

	build_active_nodes(States, ActiveNodes) :-
		active_node_pairs(States, Pairs),
		as_dictionary(Pairs, ActiveNodes).

	active_node_pairs([], []).
	active_node_pairs([cluster_state(NodeId, _Members, _Size)| States], [NodeId-true| Pairs]) :-
		active_node_pairs(States, Pairs).

	build_distance_cache(States, Options, Distances) :-
		build_distance_pairs(States, Options, Pairs),
		as_dictionary(Pairs, Distances).

	build_distance_pairs([], _Options, []).
	build_distance_pairs([State| States], Options, Pairs) :-
		build_state_distance_pairs(States, State, Options, StatePairs),
		append(StatePairs, RemainingPairs, Pairs),
		build_distance_pairs(States, Options, RemainingPairs).

	build_state_distance_pairs([], _State, _Options, []).
	build_state_distance_pairs([State| States], cluster_state(NodeId1, [Id1-Vector1], _Size1), Options, [Key-Distance| Pairs]) :-
		State = cluster_state(NodeId2, [Id2-Vector2], _Size2),
		distance_key(NodeId1, NodeId2, Key),
		base_distance(Options, Id1-Vector1, Id2-Vector2, Distance),
		build_state_distance_pairs(States, cluster_state(NodeId1, [Id1-Vector1], _Size1), Options, Pairs).

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

	agglomerate(States, TargetK, _Options, _NextNodeId, _Distances, PairQueue, _ActiveNodes, RuntimeStats, RuntimeStats, PairQueue, States) :-
		length(States, Count),
		Count =< TargetK,
		!.
	agglomerate(States0, TargetK, Options, NextNodeId0, Distances0, PairQueue0, ActiveNodes0, RuntimeStats0, RuntimeStats, FinalPairQueue, States) :-
		select_next_pair(PairQueue0, ActiveNodes0, RuntimeStats0, RuntimeStats1, LeftNodeId, RightNodeId, PairQueue1),
		merge_states(States0, LeftNodeId, RightNodeId, NextNodeId0, States1, LeftState, RightState, MergedState),
		update_distance_cache(States1, Options, Distances0, PairQueue1, LeftState, RightState, MergedState, Distances1, PairQueue2),
		update_active_nodes(ActiveNodes0, LeftState, RightState, MergedState, ActiveNodes1),
		NextNodeId1 is NextNodeId0 + 1,
		agglomerate(States1, TargetK, Options, NextNodeId1, Distances1, PairQueue2, ActiveNodes1, RuntimeStats1, RuntimeStats, FinalPairQueue, States).

	select_next_pair(PairQueue0, ActiveNodes, RuntimeStats0, RuntimeStats, LeftNodeId, RightNodeId, PairQueue) :-
		select_closest_pair(PairQueue0, ActiveNodes, RuntimeStats0, RuntimeStats, LeftNodeId, RightNodeId, _Distance, PairQueue).

	select_closest_pair(pair_queue(_Heap, Size, MaximumSize), _ActiveNodes, _RuntimeStats0, _RuntimeStats, _LeftNodeId, _RightNodeId, _Distance, _PairQueue) :-
		Size =< 0,
		!,
		consistency_error(priority_queue_exhausted, Size, MaximumSize).
	select_closest_pair(pair_queue(Heap0, Size0, MaximumSize), ActiveNodes, RuntimeStats0, RuntimeStats, LeftNodeId, RightNodeId, Distance, pair_queue(Heap, Size, MaximumSize)) :-
		(   heap_delete(Heap0, key(PoppedDistance, PoppedLeftNodeId, PoppedRightNodeId), pair(PoppedLeftNodeId, PoppedRightNodeId), Heap1) ->
			true
		;   consistency_error(heap_delete_failed, Size0, MaximumSize)
		),
		Size1 is Size0 - 1,
		(   active_node(ActiveNodes, PoppedLeftNodeId),
			active_node(ActiveNodes, PoppedRightNodeId) ->
			LeftNodeId = PoppedLeftNodeId,
			RightNodeId = PoppedRightNodeId,
			Distance = PoppedDistance,
			Heap = Heap1,
			Size = Size1,
			RuntimeStats = RuntimeStats0
		;   increment_stale_pair_discard_count(RuntimeStats0, RuntimeStats1),
			select_closest_pair(pair_queue(Heap1, Size1, MaximumSize), ActiveNodes, RuntimeStats1, RuntimeStats, LeftNodeId, RightNodeId, Distance, pair_queue(Heap, Size, MaximumSize))
		).

	active_node(ActiveNodes, NodeId) :-
		lookup(NodeId, true, ActiveNodes).

	update_active_nodes(ActiveNodes0, LeftState, RightState, MergedState, ActiveNodes) :-
		state_node_id(LeftState, LeftNodeId),
		state_node_id(RightState, RightNodeId),
		state_node_id(MergedState, MergedNodeId),
		delete_active_node(ActiveNodes0, LeftNodeId, ActiveNodes1),
		delete_active_node(ActiveNodes1, RightNodeId, ActiveNodes2),
		insert(ActiveNodes2, MergedNodeId, true, ActiveNodes).

	delete_active_node(ActiveNodes0, NodeId, ActiveNodes) :-
		(   lookup(NodeId, true, ActiveNodes0) ->
			delete(ActiveNodes0, NodeId, true, ActiveNodes)
		;   existence_error(active_node, NodeId)
		).

	ordered_node_pair(NodeId1, NodeId2, NodeId1, NodeId2) :-
		NodeId1 =< NodeId2,
		!.
	ordered_node_pair(NodeId1, NodeId2, NodeId2, NodeId1).

	merge_states(States, LeftNodeId, RightNodeId, NextNodeId, MergedStates, LeftState, RightState, MergedState) :-
		state_by_node_id(States, LeftNodeId, LeftState),
		state_by_node_id(States, RightNodeId, RightState),
		LeftState = cluster_state(_LeftNodeId, LeftMembers, LeftSize),
		RightState = cluster_state(_RightNodeId, RightMembers, RightSize),
		append(LeftMembers, RightMembers, MergedMembers),
		MergedSize is LeftSize + RightSize,
		MergedState = cluster_state(NextNodeId, MergedMembers, MergedSize),
		merge_states_by_node_ids(States, LeftNodeId, RightNodeId, MergedState, false, MergedStates).

	state_by_node_id([State| _States], NodeId, State) :-
		state_node_id(State, NodeId),
		!.
	state_by_node_id([], NodeId, _FoundState) :-
		existence_error(cluster_state, NodeId).
	state_by_node_id([_State| States], NodeId, FoundState) :-
		state_by_node_id(States, NodeId, FoundState).

	state_node_id(cluster_state(NodeId, _Members, _Size), NodeId).

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

	update_distance_cache([], _Options, Distances, PairQueue, _LeftState, _RightState, _MergedState, Distances, PairQueue).
	update_distance_cache([State| States], Options, Distances0, PairQueue0, LeftState, RightState, MergedState, Distances, PairQueue) :-
		State = cluster_state(NodeId, _Members, _Size),
		MergedState = cluster_state(MergedNodeId, _MergedMembers, _MergedSize),
		(   NodeId == MergedNodeId ->
			Distances1 = Distances0,
			PairQueue1 = PairQueue0
		;   merged_distance(Options, Distances0, LeftState, RightState, State, Distance),
			distance_key(MergedNodeId, NodeId, Key),
			insert(Distances0, Key, Distance, Distances1),
			insert_pair_queue(PairQueue0, MergedNodeId, NodeId, Distance, PairQueue1)
		),
		update_distance_cache(States, Options, Distances1, PairQueue1, LeftState, RightState, MergedState, Distances, PairQueue).

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
	merge_distance(average, cluster_state(_LeftNodeId, _LeftMembers, LeftSize), cluster_state(_RightNodeId, _RightMembers, RightSize), LeftDistance, RightDistance, Distance) :-
		TotalSize is LeftSize + RightSize,
		Distance is (LeftSize * LeftDistance + RightSize * RightDistance) / TotalSize.

	cached_state_distance(Distances, cluster_state(NodeId1, _Members1, _Size1), cluster_state(NodeId2, _Members2, _Size2), Distance) :-
		distance_key(NodeId1, NodeId2, Key),
		(   lookup(Key, Distance, Distances) ->
			true
		;   existence_error(cached_distance, Key)
		).

	distance_key(NodeId1, NodeId2, NodeId1-NodeId2) :-
		NodeId1 < NodeId2,
		!.
	distance_key(NodeId1, NodeId2, NodeId2-NodeId1).

	order_cluster_states(States, OrderedStates) :-
		order_cluster_states(States, [], OrderedStates).

	order_cluster_states([], OrderedStates, OrderedStates).
	order_cluster_states([State| States], OrderedStates0, OrderedStates) :-
		insert_cluster_state(State, OrderedStates0, OrderedStates1),
		order_cluster_states(States, OrderedStates1, OrderedStates).

	insert_cluster_state(State, [], [State]).
	insert_cluster_state(State, [OtherState| States], [State, OtherState| States]) :-
		state_order_key(State, Key),
		state_order_key(OtherState, OtherKey),
		Key @=< OtherKey,
		!.
	insert_cluster_state(State, [OtherState| States], [OtherState| OrderedStates]) :-
		insert_cluster_state(State, States, OrderedStates).

	state_order_key(cluster_state(NodeId, Members, _Size), MinimumId-NodeId) :-
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
	states_to_clusters([cluster_state(_NodeId, Members, _Size)| States], ClusterId, [cluster(ClusterId, Points)| Clusters], [Prototype| Prototypes]) :-
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
		clusterer_data(Clusterer, Encoders, Clusters, Prototypes, Options, Diagnostics),
		format('Agglomerative Clusterer~n', []),
		format('=======================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Diagnostics: ~w~n~n', [Diagnostics]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
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
