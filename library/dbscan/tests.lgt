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
		date is 2026-04-23,
		comment is 'Unit tests for the "dbscan" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(dbscan, [
		cluster/3, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1
	]).

	cover(dbscan).

	cleanup :-
		^^clean_file('test_output.pl').

	test(dbscan_learn_2_two_blobs, deterministic(ground(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(dbscan_learn_3_two_clusters_no_noise, deterministic((length(Clusters, 2), Noise == []))) :-
		learn(two_blobs, dbscan_clusterer(_Encoders, Clusters, Noise, _Options), [epsilon(0.6), minimum_points(2), feature_scaling(off)]).

	test(dbscan_cluster_3_left_blob, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [epsilon(0.6), minimum_points(2), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0], Cluster).

	test(dbscan_cluster_3_right_blob, deterministic(Cluster == 2)) :-
		learn(two_blobs, Clusterer, [epsilon(0.6), minimum_points(2), feature_scaling(off)]),
		cluster(Clusterer, [x-5.1, y-5.0], Cluster).

	test(dbscan_cluster_3_noise, deterministic(Cluster == noise)) :-
		learn(two_blobs, Clusterer, [epsilon(0.6), minimum_points(2), feature_scaling(off)]),
		cluster(Clusterer, [x-9.0, y-9.0], Cluster).

	test(dbscan_learn_3_bridge_noise, deterministic((length(Clusters, 2), length(Noise, 2)))) :-
		learn(bridge_noise, dbscan_clusterer(_Encoders, Clusters, Noise, _Options), [epsilon(1.0), minimum_points(3), feature_scaling(off)]).

	test(dbscan_cluster_3_bridge_noise_gap, deterministic(Cluster == noise)) :-
		learn(bridge_noise, Clusterer, [epsilon(1.0), minimum_points(3), feature_scaling(off)]),
		cluster(Clusterer, [x-2.5, y-0.0], Cluster).

	test(dbscan_learn_3_all_noise, deterministic((Clusters == [], length(Noise, 4)))) :-
		learn(all_noise, dbscan_clusterer(_Encoders, Clusters, Noise, _Options), [epsilon(0.5), minimum_points(2), feature_scaling(off)]).

	test(dbscan_learn_3_single_blob, deterministic((length(Clusters, 1), Noise == []))) :-
		learn(single_blob, dbscan_clusterer(_Encoders, Clusters, Noise, _Options), [epsilon(0.35), minimum_points(2), feature_scaling(off)]).

	test(dbscan_learn_3_duplicate_points, deterministic((length(Clusters, 1), length(Noise, 1)))) :-
		learn(duplicate_points, dbscan_clusterer(_Encoders, Clusters, Noise, _Options), [epsilon(0.2), minimum_points(3), feature_scaling(off)]).

	test(dbscan_cluster_3_duplicate_points_outlier, deterministic(Cluster == noise)) :-
		learn(duplicate_points, Clusterer, [epsilon(0.2), minimum_points(3), feature_scaling(off)]),
		cluster(Clusterer, [x-5.0, y-5.0], Cluster).

	test(dbscan_learn_3_scaling_bands_feature_scaling_off, deterministic((Clusters == [], length(Noise, 6)))) :-
		learn(scaling_bands, dbscan_clusterer(_Encoders, Clusters, Noise, _Options), [epsilon(1.4), minimum_points(2), feature_scaling(off)]).

	test(dbscan_learn_3_scaling_bands_feature_scaling_on, deterministic((length(Clusters, 2), Noise == []))) :-
		learn(scaling_bands, dbscan_clusterer(_Encoders, Clusters, Noise, _Options), [epsilon(1.4), minimum_points(2), feature_scaling(on)]).

	test(dbscan_learn_3_metric_parity_two_blobs, deterministic) :-
		learn(two_blobs, EuclideanClusterer, [epsilon(0.6), minimum_points(2), distance_metric(euclidean), feature_scaling(off)]),
		learn(two_blobs, ManhattanClusterer, [epsilon(0.6), minimum_points(2), distance_metric(manhattan), feature_scaling(off)]),
		training_assignments(two_blobs, EuclideanClusterer, EuclideanAssignments),
		training_assignments(two_blobs, ManhattanClusterer, ManhattanAssignments),
		same_partition(two_blobs, EuclideanAssignments, ManhattanAssignments).

	test(dbscan_learn_3_custom_options, deterministic((memberchk(epsilon(1.0), Options), memberchk(minimum_points(2), Options), memberchk(distance_metric(manhattan), Options), memberchk(feature_scaling(off), Options), memberchk(pivot_scoring(exact), Options)))) :-
		learn(two_blobs, dbscan_clusterer(_Encoders, _Clusters, _Noise, Options), [epsilon(1.0), minimum_points(2), distance_metric(manhattan), feature_scaling(off), pivot_scoring(exact)]).

	test(dbscan_learn_3_pivot_scoring_parity_two_blobs, deterministic) :-
		learn(two_blobs, ExactClusterer, [epsilon(0.6), minimum_points(2), feature_scaling(off), pivot_scoring(exact)]),
		learn(two_blobs, HeuristicClusterer, [epsilon(0.6), minimum_points(2), feature_scaling(off), pivot_scoring(heuristic)]),
		training_assignments(two_blobs, ExactClusterer, ExactAssignments),
		training_assignments(two_blobs, HeuristicClusterer, HeuristicAssignments),
		same_partition(two_blobs, ExactAssignments, HeuristicAssignments).

	test(dbscan_export_to_clauses_4, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [epsilon(0.6), minimum_points(2), feature_scaling(off)]),
		export_to_clauses(two_blobs, Clusterer, clustered, [ExportedClusterer]),
		cluster(ExportedClusterer, [x-0.9, y-1.1], Cluster).

	test(dbscan_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [epsilon(0.6), minimum_points(2), feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File).

	test(dbscan_export_to_file_4_loaded, deterministic(Cluster == 2)) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [epsilon(0.6), minimum_points(2), feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		cluster(ExportedClusterer, [x-5.0, y-5.0], Cluster).

	test(dbscan_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		learn(two_blobs, Clusterer, [epsilon(0.6), minimum_points(2), feature_scaling(off)]),
		print_clusterer(Clusterer).

	test(dbscan_learn_3_mixed_profiles, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		learn(mixed_profiles, _Clusterer).

	training_assignments(Dataset, Clusterer, Assignments) :-
		findall(
			Id-Cluster,
			(
				Dataset::example(Id, Values),
				cluster(Clusterer, Values, Cluster)
			),
			Assignments
		).

	same_partition(Dataset, Assignments1, Assignments2) :-
		findall(Id, Dataset::example(Id, _Values), Ids),
		same_partition_ids(Ids, Assignments1, Assignments2).

	same_partition_ids([], _Assignments1, _Assignments2).
	same_partition_ids([Id| Ids], Assignments1, Assignments2) :-
		same_partition_with_id(Ids, Id, Assignments1, Assignments2),
		same_partition_ids(Ids, Assignments1, Assignments2).

	same_partition_with_id([], _Id, _Assignments1, _Assignments2).
	same_partition_with_id([OtherId| Ids], Id, Assignments1, Assignments2) :-
		same_cluster_relation(Id, OtherId, Assignments1, Assignments2),
		same_partition_with_id(Ids, Id, Assignments1, Assignments2).

	same_cluster_relation(Id1, Id2, Assignments1, Assignments2) :-
		assignment_cluster(Id1, Assignments1, Cluster11),
		assignment_cluster(Id2, Assignments1, Cluster12),
		assignment_cluster(Id1, Assignments2, Cluster21),
		assignment_cluster(Id2, Assignments2, Cluster22),
		(Cluster11 == Cluster12) == (Cluster21 == Cluster22).

	assignment_cluster(Id, [Id-Cluster| _Assignments], Cluster) :-
		!.
	assignment_cluster(Id, [_Assignment| Assignments], Cluster) :-
		assignment_cluster(Id, Assignments, Cluster).

:- end_object.
