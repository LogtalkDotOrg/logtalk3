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


:- object(hdbscan,
	imports(clusterer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'HDBSCAN clusterer for continuous datasets using a mutual-reachability hierarchy, condensed tree pruning, and stability-based cluster selection.',
		remarks is [
			'Algorithm' - 'Builds the mutual-reachability graph, computes a minimum spanning tree, derives the single-linkage hierarchy, condenses the hierarchy using ``minimum_cluster_size``, and selects clusters using ``eom`` or ``leaf`` selection.',
			'Feature handling' - 'Supports continuous attributes only. Continuous attributes can be standardized using z-score scaling before clustering.',
			'Distance metrics' - 'Supports Euclidean and Manhattan distances.',
			'Prediction' - 'Assigns new instances to the selected cluster with the nearest training point when the distance is within the learned cluster reachability threshold; otherwise the atom ``noise`` is returned.',
			'Clusterer representation' - 'The learned clusterer is represented by default as ``hdbscan_clusterer(Encoders, Clusters, Noise, Options)`` where ``Clusters`` is a list of ``cluster(Id, Points, MaxCoreDistance, Stability)`` terms.'
		],
		see_also is [clusterer_protocol, clustering_dataset_protocol, dbscan, optics]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, msort/2
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

	:- uses(union_find, [
		new/2, find/4, union/4
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
		distance_graph(Rows, Options, PairDistances, NeighborMap),
		^^option(minimum_points(MinimumPoints), Options),
		core_distances(Rows, NeighborMap, MinimumPoints, CoreDistances),
		mutual_reachability_edges(PairDistances, CoreDistances, WeightedEdges),
		keysort(WeightedEdges, SortedEdges),
		build_hierarchy(Rows, SortedEdges, Root),
		^^option(minimum_cluster_size(MinimumClusterSize), Options),
		^^option(cluster_selection_method(Method), Options),
		select_clusters(Root, MinimumClusterSize, Method, Rows, CoreDistances, Clusters, SelectedPointIds),
		noise_points(Rows, SelectedPointIds, Noise),
		Clusterer = hdbscan_clusterer(Encoders, Clusters, Noise, Options).

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
			model(hdbscan),
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
		;   domain_error(valid_clusterer, Clusterer)
		).

	valid_clusters([], _FeatureCount, _SeenIds).
	valid_clusters([cluster(ClusterId, Points, MaxCoreDistance, Stability)| Clusters], FeatureCount, SeenIds) :-
		valid(positive_integer, ClusterId),
		\+ member(ClusterId, SeenIds),
		Points \== [],
		valid(list(list(number, FeatureCount)), Points),
		valid(non_negative_float, MaxCoreDistance),
		valid(non_negative_float, Stability),
		valid_clusters(Clusters, FeatureCount, [ClusterId| SeenIds]).

	distance_graph(Rows, Options, PairDistances, NeighborMap) :-
		initialize_neighbor_map(Rows, NeighborMap0),
		distance_graph(Rows, Options, [], PairDistances, NeighborMap0, NeighborMap).

	distance_graph([], _Options, PairDistances, PairDistances, NeighborMap, NeighborMap).
	distance_graph([Id-Vector| Rows], Options, PairDistances0, PairDistances, NeighborMap0, NeighborMap) :-
		distance_pairs(Id, Vector, Rows, Options, PairDistances0, PairDistances1, NeighborMap0, NeighborMap1),
		distance_graph(Rows, Options, PairDistances1, PairDistances, NeighborMap1, NeighborMap).

	distance_pairs(_Id, _Vector, [], _Options, PairDistances, PairDistances, NeighborMap, NeighborMap) :-
		!.
	distance_pairs(Id1, Vector1, [Id2-Vector2| Rows], Options, PairDistances0, PairDistances, NeighborMap0, NeighborMap) :-
		distance(Options, Vector1, Vector2, Distance),
		PairDistances1 = [pair(Id1, Id2)-Distance| PairDistances0],
		add_neighbor(Id1, Id2-Distance, NeighborMap0, NeighborMap1),
		add_neighbor(Id2, Id1-Distance, NeighborMap1, NeighborMap2),
		distance_pairs(Id1, Vector1, Rows, Options, PairDistances1, PairDistances, NeighborMap2, NeighborMap).

	initialize_neighbor_map([], []).
	initialize_neighbor_map([Id-_| Rows], [Id-[]| NeighborMap]) :-
		initialize_neighbor_map(Rows, NeighborMap).

	add_neighbor(Id, Neighbor, [Id-Neighbors| NeighborMap], [Id-[Neighbor| Neighbors]| NeighborMap]) :-
		!.
	add_neighbor(Id, Neighbor, [Entry| NeighborMap0], [Entry| NeighborMap]) :-
		add_neighbor(Id, Neighbor, NeighborMap0, NeighborMap).

	core_distances([], _NeighborMap, _MinimumPoints, []).
	core_distances([Id-_| Rows], NeighborMap, MinimumPoints, [Id-CoreDistance| CoreDistances]) :-
		memberchk(Id-Neighbors, NeighborMap),
		neighbor_distances(Neighbors, Distances0),
		msort(Distances0, Distances),
		kth_core_distance(MinimumPoints, Distances, CoreDistance),
		core_distances(Rows, NeighborMap, MinimumPoints, CoreDistances).

	neighbor_distances([], []).
	neighbor_distances([_-Distance| Neighbors], [Distance| Distances]) :-
		neighbor_distances(Neighbors, Distances).

	kth_core_distance(_MinimumPoints, [], 0.0) :-
		!.
	kth_core_distance(MinimumPoints, Distances, CoreDistance) :-
		Index is max(1, MinimumPoints - 1),
		nth_or_last(Index, Distances, CoreDistance).

	nth_or_last(1, [Value| _], Value) :-
		!.
	nth_or_last(_Index, [Value], Value) :-
		!.
	nth_or_last(Index, [_| Values], Value) :-
		Index > 1,
		NextIndex is Index - 1,
		nth_or_last(NextIndex, Values, Value).

	mutual_reachability_edges([], _CoreDistances, []).
	mutual_reachability_edges([pair(Id1, Id2)-Distance| PairDistances], CoreDistances, [Weight-(Id1, Id2)| WeightedEdges]) :-
		core_distance_value(Id1, CoreDistances, CoreDistance1),
		core_distance_value(Id2, CoreDistances, CoreDistance2),
		Weight is max(CoreDistance1, max(CoreDistance2, Distance)),
		mutual_reachability_edges(PairDistances, CoreDistances, WeightedEdges).

	core_distance_value(Id, CoreDistances, CoreDistance) :-
		memberchk(Id-CoreDistance, CoreDistances).

	build_hierarchy(Rows, SortedEdges, Root) :-
		point_ids(Rows, PointIds),
		new(PointIds, UnionFind),
		initial_components(PointIds, Components),
		build_hierarchy(SortedEdges, UnionFind, Components, 1, Root).

	build_hierarchy(_SortedEdges, _UnionFind, [_Key-Root], _NextNodeId, Root) :-
		!.
	build_hierarchy([], _UnionFind, [], _NextNodeId, none) :-
		!.
	build_hierarchy([Weight-(Id1, Id2)| SortedEdges], UnionFind0, Components0, NextNodeId0, Root) :-
		find(UnionFind0, Id1, RootId1, UnionFind1),
		find(UnionFind1, Id2, RootId2, UnionFind2),
		(   RootId1 == RootId2 ->
			build_hierarchy(SortedEdges, UnionFind2, Components0, NextNodeId0, Root)
		;   remove_component(RootId1, Components0, LeftNode, Components1),
			remove_component(RootId2, Components1, RightNode, Components2),
			order_nodes(LeftNode, RightNode, OrderedLeft, OrderedRight),
			merge_node(NextNodeId0, Weight, OrderedLeft, OrderedRight, Parent),
			union(UnionFind2, Id1, Id2, UnionFind3),
			find(UnionFind3, Id1, NewRootId, UnionFind4),
			NextNodeId is NextNodeId0 + 1,
			build_hierarchy(SortedEdges, UnionFind4, [NewRootId-Parent| Components2], NextNodeId, Root)
		).

	point_ids([], []).
	point_ids([Id-_| Rows], [Id| PointIds]) :-
		point_ids(Rows, PointIds).

	initial_components([], []).
	initial_components([Id| PointIds], [Id-leaf(Id)| Components]) :-
		initial_components(PointIds, Components).

	remove_component(Id, [Id-Node| Components], Node, Components) :-
		!.
	remove_component(Id, [Entry| Components0], Node, [Entry| Components]) :-
		remove_component(Id, Components0, Node, Components).

	order_nodes(LeftNode, RightNode, OrderedLeft, OrderedRight) :-
		node_min_id(LeftNode, LeftMinId),
		node_min_id(RightNode, RightMinId),
		(   LeftMinId @< RightMinId ->
			OrderedLeft = LeftNode,
			OrderedRight = RightNode
		;   OrderedLeft = RightNode,
			OrderedRight = LeftNode
		).

	merge_node(NodeId, Weight, LeftNode, RightNode, node(NodeId, Weight, Size, MinId, PointIds, LeftNode, RightNode)) :-
		node_size(LeftNode, LeftSize),
		node_size(RightNode, RightSize),
		Size is LeftSize + RightSize,
		node_min_id(LeftNode, LeftMinId),
		node_min_id(RightNode, RightMinId),
		(   LeftMinId @< RightMinId -> MinId = LeftMinId ; MinId = RightMinId ),
		node_points(LeftNode, LeftPointIds),
		node_points(RightNode, RightPointIds),
		append(LeftPointIds, RightPointIds, PointIds0),
		sort(PointIds0, PointIds).

	node_size(leaf(_Id), 1).
	node_size(node(_NodeId, _Weight, Size, _MinId, _PointIds, _Left, _Right), Size).

	node_min_id(leaf(Id), Id).
	node_min_id(node(_NodeId, _Weight, _Size, MinId, _PointIds, _Left, _Right), MinId).

	node_points(leaf(Id), [Id]).
	node_points(node(_NodeId, _Weight, _Size, _MinId, PointIds, _Left, _Right), PointIds).

	select_clusters(none, _MinimumClusterSize, _Method, _Rows, _CoreDistances, [], []).
	select_clusters(Root, MinimumClusterSize, Method, Rows, CoreDistances, Clusters, SelectedPointIds) :-
		evaluate_cluster(Root, 0.0, MinimumClusterSize, Method, Candidates, _SelectedStability),
		sort_candidates(Candidates, SortedCandidates),
		materialize_clusters(SortedCandidates, Rows, CoreDistances, 1, Clusters, [], SelectedPointIds).

	evaluate_cluster(Node, _BirthLambda, MinimumClusterSize, _Method, [], 0.0) :-
		node_size(Node, Size),
		Size < MinimumClusterSize,
		!.
	evaluate_cluster(leaf(Id), _BirthLambda, _MinimumClusterSize, _Method, [candidate(Id, [Id], 0.0)], 0.0) :-
		!.
	evaluate_cluster(node(_NodeId, Weight, Size, MinId, PointIds, LeftNode, RightNode), BirthLambda, MinimumClusterSize, Method, SelectedCandidates, SelectedStability) :-
		lambda_from_weight(Weight, Lambda),
		NodeStability is (Lambda - BirthLambda) * Size,
		evaluate_child(LeftNode, Lambda, MinimumClusterSize, Method, LeftCandidates, LeftStability, LeftValid),
		evaluate_child(RightNode, Lambda, MinimumClusterSize, Method, RightCandidates, RightStability, RightValid),
		combine_selection(Method, candidate(MinId, PointIds, NodeStability), NodeStability, LeftValid, LeftCandidates, LeftStability, RightValid, RightCandidates, RightStability, SelectedCandidates, SelectedStability).

	evaluate_child(Node, BirthLambda, MinimumClusterSize, Method, Candidates, Stability, yes) :-
		node_size(Node, Size),
		Size >= MinimumClusterSize,
		!,
		evaluate_cluster(Node, BirthLambda, MinimumClusterSize, Method, Candidates, Stability).
	evaluate_child(_Node, _BirthLambda, _MinimumClusterSize, _Method, [], 0.0, no).

	combine_selection(leaf, Candidate, NodeStability, LeftValid, LeftCandidates, LeftStability, RightValid, RightCandidates, RightStability, SelectedCandidates, SelectedStability) :-
		(   LeftValid == yes,
			RightValid == yes ->
			append(LeftCandidates, RightCandidates, SelectedCandidates),
			SelectedStability is LeftStability + RightStability
		;   LeftValid == yes ->
			SelectedCandidates = LeftCandidates,
			SelectedStability = LeftStability
		;   RightValid == yes ->
			SelectedCandidates = RightCandidates,
			SelectedStability = RightStability
		;   SelectedCandidates = [Candidate],
			SelectedStability = NodeStability
		).
	combine_selection(eom, Candidate, NodeStability, LeftValid, LeftCandidates, LeftStability, RightValid, RightCandidates, RightStability, SelectedCandidates, SelectedStability) :-
		(   LeftValid == yes,
			RightValid == yes ->
			append(LeftCandidates, RightCandidates, ChildCandidates),
			ChildStability is LeftStability + RightStability,
			choose_eom(Candidate, NodeStability, ChildCandidates, ChildStability, SelectedCandidates, SelectedStability)
		;   LeftValid == yes ->
			choose_eom(Candidate, NodeStability, LeftCandidates, LeftStability, SelectedCandidates, SelectedStability)
		;   RightValid == yes ->
			choose_eom(Candidate, NodeStability, RightCandidates, RightStability, SelectedCandidates, SelectedStability)
		;   SelectedCandidates = [Candidate],
			SelectedStability = NodeStability
		).

	choose_eom(Candidate, NodeStability, ChildCandidates, ChildStability, SelectedCandidates, SelectedStability) :-
		(   ChildStability > NodeStability ->
			SelectedCandidates = ChildCandidates,
			SelectedStability = ChildStability
		;   SelectedCandidates = [Candidate],
			SelectedStability = NodeStability
		).

	lambda_from_weight(Weight, Lambda) :-
		(   Weight =< 0.0 ->
			Lambda = 1.0e12
		;   Lambda is 1.0 / Weight
		).

	sort_candidates(Candidates, SortedCandidates) :-
		decorate_candidates(Candidates, DecoratedCandidates),
		keysort(DecoratedCandidates, SortedDecoratedCandidates),
		undecorate_candidates(SortedDecoratedCandidates, SortedCandidates).

	decorate_candidates([], []).
	decorate_candidates([candidate(MinId, PointIds, Stability)| Candidates], [MinId-candidate(MinId, PointIds, Stability)| DecoratedCandidates]) :-
		decorate_candidates(Candidates, DecoratedCandidates).

	undecorate_candidates([], []).
	undecorate_candidates([_MinId-Candidate| DecoratedCandidates], [Candidate| Candidates]) :-
		undecorate_candidates(DecoratedCandidates, Candidates).

	materialize_clusters([], _Rows, _CoreDistances, _ClusterId, [], SelectedPointIds, SelectedPointIds).
	materialize_clusters([candidate(_MinId, PointIds, Stability)| Candidates], Rows, CoreDistances, ClusterId0, [cluster(ClusterId0, Points, MaxCoreDistance, Stability)| Clusters], SelectedPointIds0, SelectedPointIds) :-
		point_vectors(PointIds, Rows, Points),
		cluster_max_core_distance(PointIds, CoreDistances, MaxCoreDistance),
		append(PointIds, SelectedPointIds0, SelectedPointIds1),
		ClusterId is ClusterId0 + 1,
		materialize_clusters(Candidates, Rows, CoreDistances, ClusterId, Clusters, SelectedPointIds1, SelectedPointIds).

	point_vectors([], _Rows, []).
	point_vectors([Id| PointIds], Rows, [Vector| Points]) :-
		memberchk(Id-Vector, Rows),
		point_vectors(PointIds, Rows, Points).

	cluster_max_core_distance([], _CoreDistances, 0.0).
	cluster_max_core_distance([Id| PointIds], CoreDistances, MaxCoreDistance) :-
		core_distance_value(Id, CoreDistances, CoreDistance0),
		cluster_max_core_distance(PointIds, CoreDistances, CoreDistance0, MaxCoreDistance).

	cluster_max_core_distance([], _CoreDistances, MaxCoreDistance, MaxCoreDistance).
	cluster_max_core_distance([Id| PointIds], CoreDistances, MaxCoreDistance0, MaxCoreDistance) :-
		core_distance_value(Id, CoreDistances, CoreDistance),
		(   CoreDistance > MaxCoreDistance0 ->
			MaxCoreDistance1 = CoreDistance
		;   MaxCoreDistance1 = MaxCoreDistance0
		),
		cluster_max_core_distance(PointIds, CoreDistances, MaxCoreDistance1, MaxCoreDistance).

	noise_points([], _SelectedPointIds, []).
	noise_points([Id-Vector| Rows], SelectedPointIds, Noise) :-
		(   memberchk(Id, SelectedPointIds) ->
			Noise = RestNoise
		;   Noise = [Vector| RestNoise]
		),
		noise_points(Rows, SelectedPointIds, RestNoise).

	classify_cluster([], _Features, _Options, noise).
	classify_cluster(Clusters, Features, Options, Cluster) :-
		findall(
			Distance-ClusterId,
			(	member(cluster(ClusterId, Points, MaxCoreDistance, _Stability), Clusters),
				nearest_point_distance(Points, Features, Options, Distance),
				prediction_threshold(MaxCoreDistance, Threshold),
				Distance =< Threshold
			),
			Candidates
		),
		(   Candidates == [] ->
			Cluster = noise
		;   best_candidate(Candidates, Cluster, _Distance)
		).

	prediction_threshold(MaxCoreDistance, Threshold) :-
		Threshold is max(0.001, MaxCoreDistance * 1.5).

	nearest_point_distance([Point| Points], Features, Options, Distance) :-
		distance(Options, Features, Point, InitialDistance),
		nearest_point_distance(Points, Features, Options, InitialDistance, Distance).

	nearest_point_distance([], _Features, _Options, BestDistance, BestDistance).
	nearest_point_distance([Point| Points], Features, Options, BestDistance0, BestDistance) :-
		distance(Options, Features, Point, Distance0),
		(   Distance0 < BestDistance0 ->
			BestDistance1 = Distance0
		;   BestDistance1 = BestDistance0
		),
		nearest_point_distance(Points, Features, Options, BestDistance1, BestDistance).

	best_candidate([Distance-ClusterId| Candidates], BestClusterId, BestDistance) :-
		best_candidate(Candidates, ClusterId, Distance, BestClusterId, BestDistance).

	best_candidate([], BestClusterId, BestDistance, BestClusterId, BestDistance).
	best_candidate([Distance-ClusterId| Candidates], CurrentClusterId, CurrentDistance, BestClusterId, BestDistance) :-
		(   Distance < CurrentDistance ->
			NextClusterId = ClusterId,
			NextDistance = Distance
		;   NextClusterId = CurrentClusterId,
			NextDistance = CurrentDistance
		),
		best_candidate(Candidates, NextClusterId, NextDistance, BestClusterId, BestDistance).

	distance(Options, Vector1, Vector2, Distance) :-
		^^option(distance_metric(Metric), Options),
		distance_metric(Metric, Vector1, Vector2, Distance).

	distance_metric(euclidean, Vector1, Vector2, Distance) :-
		euclidean_distance(Vector1, Vector2, Distance).
	distance_metric(manhattan, Vector1, Vector2, Distance) :-
		manhattan_distance(Vector1, Vector2, Distance).

	print_clusterer(hdbscan_clusterer(Encoders, Clusters, Noise, Options)) :-
		format('HDBSCAN Clusterer~n', []),
		format('=================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nClusters:~n', []),
		print_clusters(Clusters),
		length(Noise, NoiseCount),
		format('~nNoise (~d): ~w~n', [NoiseCount, Noise]).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		format('  ~w (continuous, mean=~4f, scale=~4f)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).

	print_clusters([]).
	print_clusters([cluster(ClusterId, Points, MaxCoreDistance, Stability)| Clusters]) :-
		length(Points, Count),
		format('  cluster ~d: ~d points, max_core_distance=~4f, stability=~4f~n', [ClusterId, Count, MaxCoreDistance, Stability]),
		print_clusters(Clusters).

	default_option(minimum_points(2)).
	default_option(minimum_cluster_size(2)).
	default_option(cluster_selection_method(eom)).
	default_option(distance_metric(euclidean)).
	default_option(feature_scaling(on)).

	valid_option(minimum_points(MinimumPoints)) :-
		valid(positive_integer, MinimumPoints).
	valid_option(minimum_cluster_size(MinimumClusterSize)) :-
		valid(positive_integer, MinimumClusterSize).
	valid_option(cluster_selection_method(Method)) :-
		once((Method == eom; Method == leaf)).
	valid_option(distance_metric(Metric)) :-
		once((Metric == euclidean; Metric == manhattan)).
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).

:- end_object.
