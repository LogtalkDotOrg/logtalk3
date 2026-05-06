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
		date is 2026-05-06,
		comment is 'Unit tests for the "hdbscan_clusterer" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(hdbscan_clusterer, [
		cluster/3, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1, valid_clusterer/1
	]).

	cover(hdbscan_clusterer).

	cleanup :-
		^^clean_file('test_output.pl').

	test(hdbscan_learn_2_two_blobs, deterministic(ground(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(hdbscan_valid_clusterer_1_valid, deterministic(valid_clusterer(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(hdbscan_valid_clusterer_1_invalid, fail) :-
		learn(two_blobs, hdbscan_clusterer(Encoders, [cluster(ClusterId, Points, _MaxCoreDistance, Stability)| Clusters], Noise, Options)),
		valid_clusterer(hdbscan_clusterer(Encoders, [cluster(ClusterId, Points, -1.0, Stability)| Clusters], Noise, Options)).

	test(hdbscan_learn_3_two_clusters_no_noise, deterministic((length(Clusters, 2), Noise == []))) :-
		learn(two_blobs, hdbscan_clusterer(_Encoders, Clusters, Noise, _Options), [minimum_points(2), minimum_cluster_size(3), feature_scaling(off)]).

	test(hdbscan_cluster_3_left_blob, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [minimum_points(2), minimum_cluster_size(3), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0], Cluster).

	test(hdbscan_cluster_3_right_blob, deterministic(Cluster == 2)) :-
		learn(two_blobs, Clusterer, [minimum_points(2), minimum_cluster_size(3), feature_scaling(off)]),
		cluster(Clusterer, [x-5.1, y-5.0], Cluster).

	test(hdbscan_cluster_3_noise, deterministic(Cluster == noise)) :-
		learn(two_blobs, Clusterer, [minimum_points(2), minimum_cluster_size(3), feature_scaling(off)]),
		cluster(Clusterer, [x-9.0, y-9.0], Cluster).

	test(hdbscan_learn_3_bridge_noise_eom, deterministic((length(Clusters, 2), length(Noise, 2)))) :-
		learn(bridge_noise, hdbscan_clusterer(_Encoders, Clusters, Noise, _Options), [minimum_points(3), minimum_cluster_size(4), feature_scaling(off)]).

	test(hdbscan_cluster_3_bridge_noise_left, deterministic(Cluster == 1)) :-
		learn(bridge_noise, Clusterer, [minimum_points(3), minimum_cluster_size(4), feature_scaling(off)]),
		cluster(Clusterer, [x-0.0, y-0.0], Cluster).

	test(hdbscan_cluster_3_bridge_noise_right, deterministic(Cluster == 2)) :-
		learn(bridge_noise, Clusterer, [minimum_points(3), minimum_cluster_size(4), feature_scaling(off)]),
		cluster(Clusterer, [x-5.0, y-0.0], Cluster).

	test(hdbscan_cluster_3_bridge_noise_gap, deterministic(Cluster == noise)) :-
		learn(bridge_noise, Clusterer, [minimum_points(3), minimum_cluster_size(4), feature_scaling(off)]),
		cluster(Clusterer, [x-2.5, y-0.0], Cluster).

	test(hdbscan_learn_3_custom_options, deterministic((memberchk(minimum_points(2), Options), memberchk(minimum_cluster_size(3), Options), memberchk(cluster_selection_method(leaf), Options), memberchk(distance_metric(manhattan), Options), memberchk(feature_scaling(off), Options)))) :-
		learn(two_blobs, hdbscan_clusterer(_Encoders, _Clusters, _Noise, Options), [minimum_points(2), minimum_cluster_size(3), cluster_selection_method(leaf), distance_metric(manhattan), feature_scaling(off)]).

	test(hdbscan_export_to_clauses_4, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [minimum_points(2), minimum_cluster_size(3), feature_scaling(off)]),
		export_to_clauses(two_blobs, Clusterer, clustered, [ExportedClusterer]),
		cluster(ExportedClusterer, [x-0.9, y-1.1], Cluster).

	test(hdbscan_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [minimum_points(2), minimum_cluster_size(3), feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File).

	test(hdbscan_export_to_file_4_loaded, deterministic(Cluster == 2)) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [minimum_points(2), minimum_cluster_size(3), feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		cluster(ExportedClusterer, [x-5.0, y-5.0], Cluster).

	test(hdbscan_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		learn(two_blobs, Clusterer, [minimum_points(2), minimum_cluster_size(3), feature_scaling(off)]),
		print_clusterer(Clusterer).

	test(hdbscan_learn_3_mixed_profiles, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		learn(mixed_profiles, _Clusterer).

:- end_object.
