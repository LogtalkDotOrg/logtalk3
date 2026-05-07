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
		comment is 'Unit tests for the "kmedians_clusterer" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(kmedians_clusterer, [
		cluster/3, diagnostics/2, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1, valid_clusterer/1
	]).

	cover(kmedians_clusterer).

	cleanup :-
		^^clean_file('test_output.pl').

	test(kmedians_learn_2_two_blobs, deterministic(ground(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(kmedians_valid_clusterer_1_valid, deterministic(valid_clusterer(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(kmedians_valid_clusterer_1_invalid, fail) :-
		learn(two_blobs, kmedians_clusterer(Encoders, Medians, Options, _Diagnostics)),
		valid_clusterer(kmedians_clusterer(Encoders, Medians, Options, [model(kmedians_clusterer), median_count(99), options(Options)])).

	test(kmedians_cluster_3_left_blob, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [k(2), initialization(spread), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0], Cluster).

	test(kmedians_cluster_3_right_blob, deterministic(Cluster == 2)) :-
		learn(two_blobs, Clusterer, [k(2), initialization(spread), feature_scaling(off)]),
		cluster(Clusterer, [x-5.1, y-5.0], Cluster).

	test(kmedians_learn_3_custom_options, deterministic([K, MaximumIterations, Tolerance, Initialization, FeatureScaling] == [3, 40, 1.0e-5, first_k, off])) :-
		learn(iris_unlabeled, kmedians_clusterer(_Encoders, _Medians, Options, _Diagnostics), [k(3), maximum_iterations(40), tolerance(1.0e-5), initialization(first_k), feature_scaling(off)]),
		memberchk(k(K), Options),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(initialization(Initialization), Options),
		memberchk(feature_scaling(FeatureScaling), Options).

	test(kmedians_diagnostics_2_rich_metadata, deterministic((Iterations >= 1, FinalShift >= 0.0, [Model, MedianCount, TrainingExampleCount, FeatureScaling] == [kmedians_clusterer, 2, 8, off]))) :-
		learn(two_blobs, Clusterer, [k(2), initialization(spread), feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(median_count(MedianCount), Diagnostics),
		memberchk(training_example_count(TrainingExampleCount), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		assertion(nonvar(Convergence)),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_shift(FinalShift), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(feature_scaling(FeatureScaling), Options).

	test(kmedians_learn_3_maximum_iterations_termination, deterministic((FinalShift > 0.0, Convergence == maximum_iterations, Iterations == 1))) :-
		learn(two_blobs, Clusterer, [k(2), maximum_iterations(1), tolerance(0.0), initialization(first_k), feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_shift(FinalShift), Diagnostics).

	test(kmedians_learn_3_tolerance_termination, deterministic((FinalShift >= 0.0, Convergence == tolerance, Iterations == 1))) :-
		learn(two_blobs, Clusterer, [k(2), maximum_iterations(40), tolerance(1000.0), initialization(first_k), feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_shift(FinalShift), Diagnostics).

	test(kmedians_cluster_3_iris_extremes, deterministic(Cluster1 \== Cluster2)) :-
		learn(iris_unlabeled, Clusterer, [k(3)]),
		cluster(Clusterer, [sepal_length-5.0, sepal_width-3.4, petal_length-1.5, petal_width-0.2], Cluster1),
		cluster(Clusterer, [sepal_length-6.8, sepal_width-3.1, petal_length-5.8, petal_width-2.2], Cluster2).

	test(kmedians_export_to_clauses_4, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_clauses(two_blobs, Clusterer, clustered, [ExportedClusterer]),
		cluster(ExportedClusterer, [x-0.9, y-1.1], Cluster).

	test(kmedians_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer),
		export_to_file(two_blobs, Clusterer, clustered, File).

	test(kmedians_export_to_file_4_loaded, deterministic(Cluster == 2)) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		cluster(ExportedClusterer, [x-5.0, y-5.0], Cluster).

	test(kmedians_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		learn(two_blobs, Clusterer),
		print_clusterer(Clusterer).

	test(kmedians_learn_3_mixed_profiles, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		learn(mixed_profiles, _Clusterer).

	test(kmedians_learn_3_duplicate_attribute_declaration, error(permission_error(repeat, attribute_declaration, x))) :-
		learn(invalid_two_blobs_declarations, _Clusterer, [feature_scaling(off)]).

	test(kmedians_learn_3_invalid_cluster_count, error(domain_error(cluster_count(1, 8), 9))) :-
		learn(two_blobs, _Clusterer, [k(9)]).

:- end_object.
