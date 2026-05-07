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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Unit tests for the "hierarchical_clustering" library.'
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2, sort/2
	]).

	:- uses(hierarchical_clustering, [
		cluster/3, cut/3, diagnostics/2, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1, valid_clusterer/1
	]).

	cover(hierarchical_clustering).

	cleanup :-
		^^clean_file('test_output.pl').

	test(hierarchical_clustering_learn_2_two_blobs, deterministic(ground(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(hierarchical_clustering_valid_clusterer_1_valid, deterministic(valid_clusterer(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(hierarchical_clustering_valid_clusterer_1_invalid, fail) :-
		learn(two_blobs, hierarchical_clustering_clusterer(Encoders, Hierarchy, Clusters, Prototypes, Diagnostics), [feature_scaling(off)]),
		valid_clusterer(hierarchical_clustering_clusterer(Encoders, Hierarchy, Clusters, [_OnlyPrototype], Diagnostics)),
		Prototypes = [_FirstPrototype| _MorePrototypes].

	test(hierarchical_clustering_learn_3_dendrogram, deterministic(functor(Dendrogram, merge, 4))) :-
		learn(two_blobs, hierarchical_clustering_clusterer(_Encoders, hierarchy(_RootState, _MergeRecords, Dendrogram), _Clusters, _Prototypes, _Diagnostics), [feature_scaling(off)]).

	test(hierarchical_clustering_learn_3_dendrogram_structure, deterministic((LeafIds == [1,2,3,4,5,6,7,8], MergeCount == 7, RootSize == 8, ValidSize == 8, Monotonic == true))) :-
		learn(two_blobs, hierarchical_clustering_clusterer(_Encoders, hierarchy(_RootState, _MergeRecords, Dendrogram), _Clusters, _Prototypes, _Diagnostics), [feature_scaling(off)]),
		dendrogram_leaf_ids(Dendrogram, LeafIds0),
		sort(LeafIds0, LeafIds),
		dendrogram_merge_count(Dendrogram, MergeCount),
		dendrogram_size(Dendrogram, ValidSize),
		( dendrogram_monotonic(Dendrogram) -> Monotonic = true ; Monotonic = false ),
		Dendrogram = merge(_Left, _Right, _Distance, RootSize).

	test(hierarchical_clustering_learn_3_permutation_stable, deterministic(Dendrogram == PermutedDendrogram)) :-
		learn(two_blobs, hierarchical_clustering_clusterer(_Encoders, hierarchy(_RootState, _MergeRecords, Dendrogram), _Clusters, _Prototypes, _Diagnostics), [feature_scaling(off)]),
		learn(two_blobs_permuted, hierarchical_clustering_clusterer(_PermutedEncoders, hierarchy(_PermutedRootState, _PermutedMergeRecords, PermutedDendrogram), _PermutedClusters, _PermutedPrototypes, _PermutedDiagnostics), [feature_scaling(off)]).

	test(hierarchical_clustering_cluster_3_left_blob, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [k(2), linkage(average), distance_metric(euclidean), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0], Cluster).

	test(hierarchical_clustering_cluster_3_right_blob, deterministic(Cluster == 2)) :-
		learn(two_blobs, Clusterer, [k(2), linkage(average), distance_metric(euclidean), feature_scaling(off)]),
		cluster(Clusterer, [x-5.1, y-5.0], Cluster).

	test(hierarchical_clustering_cluster_3_single_linkage_uses_cluster_distance, deterministic(Cluster == 2)) :-
		Clusterer = hierarchical_clustering_clusterer(
			[continuous(x, 0.0, 1.0)],
			hierarchy(cluster_state(0, [], 0.0, 0), [], leaf(0)),
			[cluster(1, [[0.0]]), cluster(2, [[10.0], [11.0]])],
			[[0.0], [10.5]],
			[options([linkage(single), distance_metric(euclidean)])]
		),
		cluster(Clusterer, [x-5.1], Cluster).

	test(hierarchical_clustering_cluster_3_complete_linkage_uses_cluster_distance, deterministic(Cluster == 1)) :-
		Clusterer = hierarchical_clustering_clusterer(
			[continuous(x, 0.0, 1.0)],
			hierarchy(cluster_state(0, [], 0.0, 0), [], leaf(0)),
			[cluster(1, [[0.0]]), cluster(2, [[8.0], [20.0]])],
			[[0.0], [14.0]],
			[options([linkage(complete), distance_metric(euclidean)])]
		),
		cluster(Clusterer, [x-9.0], Cluster).

	test(hierarchical_clustering_learn_3_cut_k_1, deterministic((Clusters = [cluster(1, Points)], length(Points, 8), LeftCluster == 1, RightCluster == 1))) :-
		learn(two_blobs, Clusterer0, [feature_scaling(off)]),
		cut(Clusterer0, 1, Clusterer),
		Clusterer = hierarchical_clustering_clusterer(_Encoders, _Hierarchy, Clusters, _Prototypes, _Diagnostics),
		cluster(Clusterer, [x-1.0, y-1.0], LeftCluster),
		cluster(Clusterer, [x-5.1, y-5.0], RightCluster).

	test(hierarchical_clustering_learn_3_cut_k_count, deterministic((length(Clusters, 8), SingletonClusters == true, UniqueClusterCount == 8, SingletonPrototypes == true))) :-
		learn(two_blobs, Clusterer0, [feature_scaling(off)]),
		cut(Clusterer0, 8, hierarchical_clustering_clusterer(_Encoders, _Hierarchy, Clusters, Prototypes, _Diagnostics)),
		( all_singleton_clusters(Clusters) -> SingletonClusters = true ; SingletonClusters = false ),
		training_assignments(two_blobs, hierarchical_clustering_clusterer(_Encoders, _Hierarchy, Clusters, Prototypes, _Diagnostics), Assignments),
		assignment_cluster_ids(Assignments, ClusterIds0),
		sort(ClusterIds0, ClusterIds),
		length(ClusterIds, UniqueClusterCount),
		( singleton_clusters_match_prototypes(Clusters, Prototypes) -> SingletonPrototypes = true ; SingletonPrototypes = false ).

	test(hierarchical_clustering_diagnostics_2_rich, deterministic((HeapRebuildCount >= 0, MaximumHeapSize > 0, Height > 0.0, [TrainingExampleCount, MergeCount, ScanFallbackCount, TieBreaking] == [8, 7, 0, node_id_order]))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(training_example_count(TrainingExampleCount), Diagnostics),
		memberchk(merge_count(MergeCount), Diagnostics),
		memberchk(heap_rebuild_count(HeapRebuildCount), Diagnostics),
		memberchk(scan_fallback_count(ScanFallbackCount), Diagnostics),
		memberchk(tie_breaking(TieBreaking), Diagnostics),
		memberchk(maximum_heap_size(MaximumHeapSize), Diagnostics),
		memberchk(dendrogram_height(Height), Diagnostics).

	test(hierarchical_clustering_learn_3_custom_options, deterministic([K, Linkage, DistanceMetric, FeatureScaling] == [3, complete, manhattan, off])) :-
		learn(iris_unlabeled, Clusterer, [k(3), linkage(complete), distance_metric(manhattan), feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(k(K), Options),
		memberchk(linkage(Linkage), Options),
		memberchk(distance_metric(DistanceMetric), Options),
		memberchk(feature_scaling(FeatureScaling), Options).

	test(hierarchical_clustering_cluster_3_iris_extremes, deterministic(Cluster1 \== Cluster2)) :-
		learn(iris_unlabeled, Clusterer, [k(3)]),
		cluster(Clusterer, [sepal_length-5.0, sepal_width-3.4, petal_length-1.5, petal_width-0.2], Cluster1),
		cluster(Clusterer, [sepal_length-6.8, sepal_width-3.1, petal_length-5.8, petal_width-2.2], Cluster2).

	test(hierarchical_clustering_export_to_clauses_4, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_clauses(two_blobs, Clusterer, clustered, [ExportedClusterer]),
		cluster(ExportedClusterer, [x-0.9, y-1.1], Cluster).

	test(hierarchical_clustering_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File).

	test(hierarchical_clustering_export_to_file_4_loaded, deterministic(Cluster == 2)) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		cluster(ExportedClusterer, [x-5.0, y-5.0], Cluster).

	test(hierarchical_clustering_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		print_clusterer(Clusterer).

	test(hierarchical_clustering_learn_3_mixed_profiles, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		learn(mixed_profiles, _Clusterer).

	test(hierarchical_clustering_learn_3_invalid_cluster_count, error(domain_error(cluster_count(1, 8), 9))) :-
		learn(two_blobs, _Clusterer, [k(9)]).

	% auxiliary predicates

	training_assignments(Dataset, Clusterer, Assignments) :-
		findall(
			Id-Cluster,
			(
				Dataset::example(Id, AttributeValues),
				cluster(Clusterer, AttributeValues, Cluster)
			),
			Assignments
		).

	assignment_cluster_ids([], []).
	assignment_cluster_ids([_Id-Cluster| Assignments], [Cluster| Clusters]) :-
		assignment_cluster_ids(Assignments, Clusters).

	all_singleton_clusters([]).
	all_singleton_clusters([cluster(_ClusterId, Points)| Clusters]) :-
		Points = [_Point],
		all_singleton_clusters(Clusters).

	singleton_clusters_match_prototypes([], []).
	singleton_clusters_match_prototypes([cluster(_ClusterId, [Point])| Clusters], [Prototype| Prototypes]) :-
		Point == Prototype,
		singleton_clusters_match_prototypes(Clusters, Prototypes).

	dendrogram_leaf_ids(leaf(Id), [Id]).
	dendrogram_leaf_ids(merge(Left, Right, _Distance, _Size), LeafIds) :-
		dendrogram_leaf_ids(Left, LeftIds),
		dendrogram_leaf_ids(Right, RightIds),
		append(LeftIds, RightIds, LeafIds).

	dendrogram_merge_count(leaf(_Id), 0).
	dendrogram_merge_count(merge(Left, Right, _Distance, _Size), MergeCount) :-
		dendrogram_merge_count(Left, LeftCount),
		dendrogram_merge_count(Right, RightCount),
		MergeCount is LeftCount + RightCount + 1.

	dendrogram_size(leaf(_Id), 1).
	dendrogram_size(merge(Left, Right, _Distance, Size), Size) :-
		dendrogram_size(Left, LeftSize),
		dendrogram_size(Right, RightSize),
		Size =:= LeftSize + RightSize.

	dendrogram_monotonic(leaf(_Id)).
	dendrogram_monotonic(merge(Left, Right, Distance, _Size)) :-
		child_distance(Left, LeftDistance),
		child_distance(Right, RightDistance),
		Distance >= LeftDistance,
		Distance >= RightDistance,
		dendrogram_monotonic(Left),
		dendrogram_monotonic(Right).

	child_distance(leaf(_Id), 0.0).
	child_distance(merge(_Left, _Right, Distance, _Size), Distance).

:- end_object.
