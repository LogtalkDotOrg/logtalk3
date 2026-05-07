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


:- object(invalid_two_blobs_declarations,
	implements(clustering_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-1.0]).
	example(2, [x-5.0, y-5.0]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Unit tests for the "kcenters_clusterer" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(kcenters_clusterer, [
		cluster/3, diagnostics/2, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1, valid_clusterer/1
	]).

	cover(kcenters_clusterer).

	cleanup :-
		^^clean_file('test_output.pl').

	test(kcenters_learn_2_two_blobs, deterministic(ground(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(kcenters_valid_clusterer_1_valid, deterministic(valid_clusterer(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(kcenters_valid_clusterer_1_invalid, fail) :-
		learn(two_blobs, kcenters_clusterer(Encoders, Centers, Options, _Diagnostics)),
		valid_clusterer(kcenters_clusterer(Encoders, Centers, Options, [model(kcenters_clusterer), center_count(99), selection_strategy(first_k), options(Options)])).

	test(kcenters_cluster_3_left_blob, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [k(2), initialization(spread), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0], Cluster).

	test(kcenters_cluster_3_right_blob, deterministic(Cluster == 2)) :-
		learn(two_blobs, Clusterer, [k(2), initialization(spread), feature_scaling(off)]),
		cluster(Clusterer, [x-5.1, y-5.0], Cluster).

	test(kcenters_learn_3_custom_options, deterministic([K, Initialization, DistanceMetric, FeatureScaling] == [3, first_k, manhattan, off])) :-
		learn(iris_unlabeled, kcenters_clusterer(_Encoders, _Centers, Options, _Diagnostics), [k(3), initialization(first_k), distance_metric(manhattan), feature_scaling(off)]),
		memberchk(k(K), Options),
		memberchk(initialization(Initialization), Options),
		memberchk(distance_metric(DistanceMetric), Options),
		memberchk(feature_scaling(FeatureScaling), Options).

	test(kcenters_diagnostics_2_rich_metadata, deterministic([Model, CenterCount, TrainingExampleCount, SelectionStrategy, DistanceMetric, FeatureScaling] == [kcenters_clusterer, 2, 8, deterministic_farthest_first, manhattan, off])) :-
		learn(two_blobs, Clusterer, [k(2), initialization(spread), distance_metric(manhattan), feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(center_count(CenterCount), Diagnostics),
		memberchk(training_example_count(TrainingExampleCount), Diagnostics),
		memberchk(selection_strategy(SelectionStrategy), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(distance_metric(DistanceMetric), Options),
		memberchk(feature_scaling(FeatureScaling), Options).

	test(kcenters_cluster_3_iris_extremes, deterministic(Cluster1 \== Cluster2)) :-
		learn(iris_unlabeled, Clusterer, [k(3)]),
		cluster(Clusterer, [sepal_length-5.0, sepal_width-3.4, petal_length-1.5, petal_width-0.2], Cluster1),
		cluster(Clusterer, [sepal_length-6.8, sepal_width-3.1, petal_length-5.8, petal_width-2.2], Cluster2).

	test(kcenters_export_to_clauses_4, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_clauses(two_blobs, Clusterer, clustered, [ExportedClusterer]),
		cluster(ExportedClusterer, [x-0.9, y-1.1], Cluster).

	test(kcenters_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer),
		export_to_file(two_blobs, Clusterer, clustered, File).

	test(kcenters_export_to_file_4_loaded, deterministic(Cluster == 2)) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		cluster(ExportedClusterer, [x-5.0, y-5.0], Cluster).

	test(kcenters_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		learn(two_blobs, Clusterer),
		print_clusterer(Clusterer).

	test(kcenters_learn_3_mixed_profiles, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		learn(mixed_profiles, _Clusterer).

	test(kcenters_learn_3_duplicate_attribute_declaration, error(permission_error(repeat, attribute_declaration, x))) :-
		learn(invalid_two_blobs_declarations, _Clusterer, [feature_scaling(off)]).

	test(kcenters_learn_3_invalid_cluster_count, error(domain_error(cluster_count(1, 8), 9))) :-
		learn(two_blobs, _Clusterer, [k(9)]).

:- end_object.
