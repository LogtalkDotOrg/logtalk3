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


:- object(invalid_agglomerative_two_blobs,
	implements(clustering_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, x-1.1, y-1.0]).
	example(2, [x-5.0, y-5.0]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Unit tests for the "agglomerative_clusterer" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(agglomerative_clusterer, [
		cluster/3, diagnostics/2, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1, valid_clusterer/1
	]).

	cover(agglomerative_clusterer).

	cleanup :-
		^^clean_file('test_output.pl').

	test(agglomerative_learn_2_two_blobs, deterministic(ground(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(agglomerative_valid_clusterer_1_valid, deterministic(valid_clusterer(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(agglomerative_valid_clusterer_1_invalid, fail) :-
		learn(two_blobs, agglomerative_clusterer(Encoders, Clusters, Prototypes, Options, Diagnostics), [feature_scaling(off)]),
		valid_clusterer(agglomerative_clusterer(Encoders, Clusters, [_OnlyPrototype], Options, Diagnostics)),
		Prototypes = [_FirstPrototype| _MorePrototypes].

	test(agglomerative_cluster_3_left_blob, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [k(2), linkage(average), distance_metric(euclidean), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0], Cluster).

	test(agglomerative_cluster_3_right_blob, deterministic(Cluster == 2)) :-
		learn(two_blobs, Clusterer, [k(2), linkage(average), distance_metric(euclidean), feature_scaling(off)]),
		cluster(Clusterer, [x-5.1, y-5.0], Cluster).

	test(agglomerative_cluster_3_linkage_consistent_single, deterministic(Cluster == 2)) :-
		Clusterer = agglomerative_clusterer(
			[continuous(x, 0.0, 1.0)],
			[cluster(1, [[0.0]]), cluster(2, [[10.0], [11.0]])],
			[[0.0], [10.5]],
			[k(2), linkage(single), distance_metric(euclidean), feature_scaling(off)],
			[options([k(2), linkage(single), distance_metric(euclidean), feature_scaling(off)])]
		),
		cluster(Clusterer, [x-5.1], Cluster).

	test(agglomerative_cluster_3_linkage_consistent_complete, deterministic(Cluster == 1)) :-
		Clusterer = agglomerative_clusterer(
			[continuous(x, 0.0, 1.0)],
			[cluster(1, [[0.0]]), cluster(2, [[8.0], [20.0]])],
			[[0.0], [14.0]],
			[k(2), linkage(complete), distance_metric(euclidean), feature_scaling(off)],
			[options([k(2), linkage(complete), distance_metric(euclidean), feature_scaling(off)])]
		),
		cluster(Clusterer, [x-9.0], Cluster).

	test(agglomerative_learn_3_permutation_stable, deterministic(Assignments == PermutedAssignments)) :-
		learn(two_blobs, Clusterer, [k(2), linkage(average), distance_metric(euclidean), feature_scaling(off)]),
		learn(two_blobs_permuted, PermutedClusterer, [k(2), linkage(average), distance_metric(euclidean), feature_scaling(off)]),
		training_assignments(two_blobs, Clusterer, Assignments),
		training_assignments(two_blobs_permuted, PermutedClusterer, PermutedAssignments).

	test(agglomerative_learn_3_duplicate_points_stable_order, deterministic((Cluster1 == 1, Cluster2 == 2))) :-
		learn(duplicate_points, Clusterer, [k(2), linkage(average), distance_metric(euclidean), feature_scaling(off)]),
		cluster(Clusterer, [x-0.0, y-0.0], Cluster1),
		cluster(Clusterer, [x-5.0, y-5.0], Cluster2).

	test(agglomerative_diagnostics_2_rich, deterministic((memberchk(training_example_count(8), Diagnostics), memberchk(merge_count(6), Diagnostics), memberchk(initial_pair_count(28), Diagnostics), memberchk(maximum_heap_size(MaximumHeapSize), Diagnostics), memberchk(stale_pair_discard_count(StalePairDiscardCount), Diagnostics), memberchk(pair_selection(priority_queue), Diagnostics), memberchk(prediction_strategy(cluster_member_linkage_distance), Diagnostics), memberchk(tie_breaking(node_id_order), Diagnostics), MaximumHeapSize > 0, StalePairDiscardCount >= 0))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics).

	test(agglomerative_learn_3_custom_options, deterministic((memberchk(k(3), Options), memberchk(linkage(complete), Options), memberchk(distance_metric(manhattan), Options), memberchk(feature_scaling(off), Options)))) :-
		learn(iris_unlabeled, agglomerative_clusterer(_Encoders, _Clusters, _Prototypes, Options, _Diagnostics), [k(3), linkage(complete), distance_metric(manhattan), feature_scaling(off)]).

	test(agglomerative_cluster_3_iris_extremes, deterministic(Cluster1 \== Cluster2)) :-
		learn(iris_unlabeled, Clusterer, [k(3)]),
		cluster(Clusterer, [sepal_length-5.0, sepal_width-3.4, petal_length-1.5, petal_width-0.2], Cluster1),
		cluster(Clusterer, [sepal_length-6.8, sepal_width-3.1, petal_length-5.8, petal_width-2.2], Cluster2).

	test(agglomerative_export_to_clauses_4, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_clauses(two_blobs, Clusterer, clustered, [ExportedClusterer]),
		cluster(ExportedClusterer, [x-0.9, y-1.1], Cluster).

	test(agglomerative_export_to_clauses_4_preserves_payload, deterministic(Payload == ExportedPayload)) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_clauses(two_blobs, Clusterer, clustered, [ExportedClusterer]),
		Clusterer =.. [_Functor| Payload],
		ExportedClusterer =.. [_ExportedFunctor| ExportedPayload].

	test(agglomerative_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File).

	test(agglomerative_export_to_file_4_loaded, deterministic(Cluster == 2)) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		cluster(ExportedClusterer, [x-5.0, y-5.0], Cluster).

	test(agglomerative_export_to_file_4_loaded_preserves_payload, deterministic(Payload == ExportedPayload)) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		Clusterer =.. [_Functor| Payload],
		ExportedClusterer =.. [_ExportedFunctor| ExportedPayload].

	test(agglomerative_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		print_clusterer(Clusterer).

	test(agglomerative_cluster_3_missing_attribute, error(existence_error(attribute, y))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		cluster(Clusterer, [x-1.0], _Cluster).

	test(agglomerative_cluster_3_duplicate_attribute, error(domain_error(attribute_occurrences(x, 1), 2))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, x-1.1, y-1.0], _Cluster).

	test(agglomerative_cluster_3_undeclared_attribute, error(domain_error(declared_attribute(_), z))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0, z-2.0], _Cluster).

	test(agglomerative_learn_3_mixed_profiles, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		learn(mixed_profiles, _Clusterer).

	test(agglomerative_learn_3_invalid_cluster_count, error(domain_error(cluster_count(1, 8), 9))) :-
		learn(two_blobs, _Clusterer, [k(9)]).

	test(agglomerative_learn_3_duplicate_training_attribute, error(domain_error(attribute_occurrences(x, 1), 2))) :-
		learn(invalid_agglomerative_two_blobs, _Clusterer, [feature_scaling(off)]).

	training_assignments(Dataset, Clusterer, Assignments) :-
		findall(
			Id-Cluster,
			(
				Dataset::example(Id, AttributeValues),
				cluster(Clusterer, AttributeValues, Cluster)
			),
			Assignments0
		),
		sort(Assignments0, Assignments).

:- end_object.
