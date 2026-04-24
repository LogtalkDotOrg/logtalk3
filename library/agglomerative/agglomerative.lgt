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
		date is 2026-04-23,
		comment is 'Agglomerative clusterer for continuous datasets. Learns from a dataset object implementing the ``clustering_dataset_protocol`` protocol and returns a clusterer term that can be used for assigning new instances to clusters and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses deterministic bottom-up agglomerative clustering and stops when the requested number of clusters is reached.',
			'Feature handling' - 'Supports continuous attributes only. Continuous attributes can be standardized using z-score scaling before clustering.',
			'Linkage strategies' - 'Supports ``single``, ``complete``, and ``average`` linkage strategies.',
			'Distance metrics' - 'Supports Euclidean and Manhattan distances.',
			'Clusterer representation' - 'The learned clusterer is represented by default as ``agglomerative_clusterer(Encoders, Clusters, Prototypes, Options)`` where ``Encoders`` stores the feature encoding metadata, ``Clusters`` stores the learned cluster members, ``Prototypes`` stores average vectors for prediction, and ``Options`` stores the effective training options.'
		],
		see_also is [clusterer_protocol, clustering_dataset_protocol, kcenters, kmeans, kmedoids]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, nth1/3
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
		initial_clusters(Rows, InitialClusters),
		agglomerate(InitialClusters, K, Options, FinalStates),
		states_to_clusters(FinalStates, 1, Clusters, Prototypes),
		Clusterer = agglomerative_clusterer(Encoders, Clusters, Prototypes, Options),
		!.

	cluster(Clusterer, Instance, Cluster) :-
		Clusterer =.. [_, Encoders, _Clusters, Prototypes, Options],
		^^encode_instance(Encoders, Instance, Features),
		nearest_prototype(Prototypes, Features, Options, Cluster, _Distance).

	clusterer_diagnostics_data(agglomerative_clusterer(_Encoders, Clusters, Prototypes, Options), Diagnostics) :-
		length(Clusters, ClusterCount),
		length(Prototypes, PrototypeCount),
		Diagnostics = [
			model(agglomerative),
			cluster_count(ClusterCount),
			prototype_count(PrototypeCount),
			options(Options)
		].

	initial_clusters([], []).
	initial_clusters([Id-Vector| Rows], [cluster_state([Id-Vector])| Clusters]) :-
		initial_clusters(Rows, Clusters).

	agglomerate(Clusters, TargetK, _Options, Clusters) :-
		length(Clusters, Count),
		Count =< TargetK,
		!.
	agglomerate(Clusters0, TargetK, Options, Clusters) :-
		select_closest_pair(Clusters0, Options, Index1, Index2),
		merge_clusters_at_indices(Clusters0, Index1, Index2, Clusters1),
		agglomerate(Clusters1, TargetK, Options, Clusters).

	select_closest_pair(Clusters, Options, BestIndex1, BestIndex2) :-
		findall(
			Distance-Index1-Index2,
			(   nth1(Index1, Clusters, Cluster1),
				nth1(Index2, Clusters, Cluster2),
				Index1 < Index2,
				cluster_distance(Options, Cluster1, Cluster2, Distance)
			),
			Candidates
		),
		best_pair(Candidates, BestIndex1, BestIndex2, _BestDistance).

	best_pair([Distance-Index1-Index2| Candidates], BestIndex1, BestIndex2, BestDistance) :-
		best_pair(Candidates, Index1, Index2, Distance, BestIndex1, BestIndex2, BestDistance).

	best_pair([], BestIndex1, BestIndex2, BestDistance, BestIndex1, BestIndex2, BestDistance).
	best_pair([Distance-Index1-Index2| Candidates], CurrentIndex1, CurrentIndex2, CurrentDistance, BestIndex1, BestIndex2, BestDistance) :-
		(   Distance < CurrentDistance ->
			NextIndex1 = Index1,
			NextIndex2 = Index2,
			NextDistance = Distance
		;   NextIndex1 = CurrentIndex1,
			NextIndex2 = CurrentIndex2,
			NextDistance = CurrentDistance
		),
		best_pair(Candidates, NextIndex1, NextIndex2, NextDistance, BestIndex1, BestIndex2, BestDistance).

	merge_clusters_at_indices(Clusters, Index1, Index2, MergedClusters) :-
		nth1(Index1, Clusters, cluster_state(Members1)),
		nth1(Index2, Clusters, cluster_state(Members2)),
		append(Members1, Members2, MergedMembers),
		merge_clusters_at_indices(Clusters, 1, Index1, Index2, cluster_state(MergedMembers), MergedClusters).

	merge_clusters_at_indices([], _Current, _Index1, _Index2, _Merged, []).
	merge_clusters_at_indices([_Cluster| Clusters], Index1, Index1, Index2, Merged, [Merged| MergedClusters]) :-
		!,
		Next is Index1 + 1,
		merge_clusters_at_indices(Clusters, Next, Index1, Index2, Merged, MergedClusters).
	merge_clusters_at_indices([_Cluster| Clusters], Index2, _Index1, Index2, Merged, MergedClusters) :-
		!,
		Next is Index2 + 1,
		merge_clusters_at_indices(Clusters, Next, 0, Index2, Merged, MergedClusters).
	merge_clusters_at_indices([Cluster| Clusters], Current, Index1, Index2, Merged, [Cluster| MergedClusters]) :-
		Next is Current + 1,
		merge_clusters_at_indices(Clusters, Next, Index1, Index2, Merged, MergedClusters).

	cluster_distance(Options, cluster_state(Members1), cluster_state(Members2), Distance) :-
		^^option(linkage(Linkage), Options),
		linkage_distance(Linkage, Members1, Members2, Options, Distance).

	linkage_distance(single, Members1, Members2, Options, Distance) :-
		all_pairwise_distances(Members1, Members2, Options, [First| Rest]),
		minimum_distance(Rest, First, Distance).
	linkage_distance(complete, Members1, Members2, Options, Distance) :-
		all_pairwise_distances(Members1, Members2, Options, [First| Rest]),
		maximum_distance(Rest, First, Distance).
	linkage_distance(average, Members1, Members2, Options, Distance) :-
		all_pairwise_distances(Members1, Members2, Options, Distances),
		sum_distances(Distances, 0.0, 0, Sum, Count),
		Distance is Sum / Count.

	all_pairwise_distances([], _Members2, _Options, []).
	all_pairwise_distances([_Id1-Vector1| Members1], Members2, Options, Distances) :-
		pairwise_distances_to_cluster(Members2, Vector1, Options, Distances1),
		all_pairwise_distances(Members1, Members2, Options, Distances2),
		append(Distances1, Distances2, Distances).

	pairwise_distances_to_cluster([], _Vector, _Options, []).
	pairwise_distances_to_cluster([_Id2-Vector2| Members2], Vector1, Options, [Distance| Distances]) :-
		distance(Options, Vector1, Vector2, Distance),
		pairwise_distances_to_cluster(Members2, Vector1, Options, Distances).

	minimum_distance([], Distance, Distance).
	minimum_distance([Candidate| Candidates], CurrentDistance, Distance) :-
		(   Candidate < CurrentDistance ->
			NextDistance = Candidate
		;   NextDistance = CurrentDistance
		),
		minimum_distance(Candidates, NextDistance, Distance).

	maximum_distance([], Distance, Distance).
	maximum_distance([Candidate| Candidates], CurrentDistance, Distance) :-
		(   Candidate > CurrentDistance ->
			NextDistance = Candidate
		;   NextDistance = CurrentDistance
		),
		maximum_distance(Candidates, NextDistance, Distance).

	sum_distances([], Sum, Count, Sum, Count).
	sum_distances([Distance| Distances], Sum0, Count0, Sum, Count) :-
		Sum1 is Sum0 + Distance,
		Count1 is Count0 + 1,
		sum_distances(Distances, Sum1, Count1, Sum, Count).

	states_to_clusters([], _ClusterId, [], []).
	states_to_clusters([cluster_state(Members)| States], ClusterId, [cluster(ClusterId, Points)| Clusters], [Prototype| Prototypes]) :-
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

	nearest_prototype([Prototype| Prototypes], Features, Options, Cluster, Distance) :-
		distance(Options, Features, Prototype, InitialDistance),
		nearest_prototype(Prototypes, Features, Options, 2, 1, InitialDistance, Cluster, Distance).

	nearest_prototype([], _Features, _Options, _Index, BestCluster, BestDistance, BestCluster, BestDistance).
	nearest_prototype([Prototype| Prototypes], Features, Options, Index, BestCluster0, BestDistance0, BestCluster, BestDistance) :-
		distance(Options, Features, Prototype, Distance0),
		(   Distance0 < BestDistance0 ->
			BestCluster1 = Index,
			BestDistance1 = Distance0
		;   BestCluster1 = BestCluster0,
			BestDistance1 = BestDistance0
		),
		NextIndex is Index + 1,
		nearest_prototype(Prototypes, Features, Options, NextIndex, BestCluster1, BestDistance1, BestCluster, BestDistance).

	distance(Options, Vector1, Vector2, Distance) :-
		^^option(distance_metric(Metric), Options),
		distance_metric(Metric, Vector1, Vector2, Distance).

	distance_metric(euclidean, Vector1, Vector2, Distance) :-
		euclidean_distance(Vector1, Vector2, Distance).
	distance_metric(manhattan, Vector1, Vector2, Distance) :-
		manhattan_distance(Vector1, Vector2, Distance).

	print_clusterer(agglomerative_clusterer(Encoders, Clusters, Prototypes, Options)) :-
		format('Agglomerative Clusterer~n', []),
		format('=======================~n~n', []),
		format('Options: ~w~n~n', [Options]),
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
