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


:- object(concentric_rings,
	implements(clustering_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(i1, [x-1.0, y-0.0]).
	example(i2, [x-0.707, y-0.707]).
	example(i3, [x-0.0, y-1.0]).
	example(i4, [x- -0.707, y-0.707]).
	example(i5, [x- -1.0, y-0.0]).
	example(i6, [x- -0.707, y- -0.707]).
	example(i7, [x-0.0, y- -1.0]).
	example(i8, [x-0.707, y- -0.707]).
	example(o1, [x-3.0, y-0.0]).
	example(o2, [x-2.772, y-1.148]).
	example(o3, [x-2.121, y-2.121]).
	example(o4, [x-1.148, y-2.772]).
	example(o5, [x-0.0, y-3.0]).
	example(o6, [x- -1.148, y-2.772]).
	example(o7, [x- -2.121, y-2.121]).
	example(o8, [x- -2.772, y-1.148]).
	example(o9, [x- -3.0, y-0.0]).
	example(o10, [x- -2.772, y- -1.148]).
	example(o11, [x- -2.121, y- -2.121]).
	example(o12, [x- -1.148, y- -2.772]).
	example(o13, [x-0.0, y- -3.0]).
	example(o14, [x-1.148, y- -2.772]).
	example(o15, [x-2.121, y- -2.121]).
	example(o16, [x-2.772, y- -1.148]).

:- end_object.


:- object(identical_points,
	implements(clustering_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-1.0]).
	example(2, [x-1.0, y-1.0]).
	example(3, [x-1.0, y-1.0]).

:- end_object.


:- object(invalid_spectral_dataset,
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
		date is 2026-08-01,
		comment is 'Unit tests for the "spectral_clusterer" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(spectral_clusterer, [
		cluster/3, diagnostics/2, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1, valid_clusterer/1
	]).

	cover(spectral_clusterer).

	cleanup :-
		^^clean_file('test_output.pl').

	test(spectral_learn_2_two_blobs, deterministic(ground(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(spectral_valid_clusterer_1_valid, deterministic(valid_clusterer(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(spectral_valid_clusterer_1_invalid, fail) :-
		learn(two_blobs, spectral_clusterer(Encoders, Rows, Degrees, Components, Eigenvalues, Centroids, Sigma, Options, _Diagnostics)),
		valid_clusterer(spectral_clusterer(Encoders, Rows, Degrees, Components, Eigenvalues, Centroids, Sigma, Options, [model(spectral_clusterer), cluster_count(99), options(Options)])).

	test(spectral_cluster_3_two_blobs, deterministic(LeftCluster \== RightCluster)) :-
		learn(two_blobs, Clusterer, [k(2), sigma(1.0), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0], LeftCluster),
		cluster(Clusterer, [x-5.0, y-5.0], RightCluster).

	test(spectral_cluster_3_nystrom_nearby_points, deterministic((Left1 == Left2, Right1 == Right2, Left1 \== Right1))) :-
		learn(two_blobs, Clusterer, [k(2), sigma(1.0), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0], Left1),
		cluster(Clusterer, [x-1.2, y-0.9], Left2),
		cluster(Clusterer, [x-5.0, y-5.0], Right1),
		cluster(Clusterer, [x-5.2, y-4.9], Right2).

	test(spectral_cluster_3_concentric_rings, deterministic((Inner1 == Inner2, Outer1 == Outer2, Inner1 \== Outer1))) :-
		learn(concentric_rings, Clusterer, [k(2), sigma(0.4), maximum_iterations(20), tolerance(1.0e-4), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-0.0], Inner1),
		cluster(Clusterer, [x-0.0, y-1.0], Inner2),
		cluster(Clusterer, [x-3.0, y-0.0], Outer1),
		cluster(Clusterer, [x-0.0, y-3.0], Outer2).

	test(spectral_learn_3_custom_options, deterministic([K, Sigma, MaximumIterations, Tolerance, Initialization, FeatureScaling] == [3, 2.0, 40, 1.0e-5, first_k, off])) :-
		learn(iris_unlabeled, spectral_clusterer(_Encoders, _Rows, _Degrees, _Components, _Eigenvalues, _Centroids, _ResolvedSigma, Options, _Diagnostics), [k(3), sigma(2.0), maximum_iterations(40), tolerance(1.0e-5), initialization(first_k), feature_scaling(off)]),
		memberchk(k(K), Options),
		memberchk(sigma(Sigma), Options),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(initialization(Initialization), Options),
		memberchk(feature_scaling(FeatureScaling), Options).

	test(spectral_diagnostics_2_rich_metadata, deterministic((ClusterCount == 2, TrainingExampleCount == 8, ResolvedSigma > 0.0, Iterations >= 1, FinalShift >= 0.0))) :-
		learn(two_blobs, Clusterer),
		diagnostics(Clusterer, Diagnostics),
		memberchk(model(spectral_clusterer), Diagnostics),
		memberchk(cluster_count(ClusterCount), Diagnostics),
		memberchk(training_example_count(TrainingExampleCount), Diagnostics),
		memberchk(resolved_sigma(ResolvedSigma), Diagnostics),
		memberchk(eigenvalues(Eigenvalues), Diagnostics),
		assertion(Eigenvalues = [_, _]),
		memberchk(convergence(Convergence), Diagnostics),
		assertion(nonvar(Convergence)),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_shift(FinalShift), Diagnostics),
		memberchk(options(_Options), Diagnostics).

	test(spectral_learn_3_maximum_iterations_termination, deterministic((Convergence == maximum_iterations, Iterations == 1, FinalShift >= 0.0))) :-
		learn(two_blobs, Clusterer, [k(2), sigma(1.0), maximum_iterations(1), tolerance(0.0), initialization(first_k), feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_shift(FinalShift), Diagnostics).

	test(spectral_export_to_clauses_4, deterministic(LeftCluster \== RightCluster)) :-
		learn(two_blobs, Clusterer, [sigma(1.0), feature_scaling(off)]),
		export_to_clauses(two_blobs, Clusterer, clustered, [ExportedClusterer]),
		cluster(ExportedClusterer, [x-1.0, y-1.0], LeftCluster),
		cluster(ExportedClusterer, [x-5.0, y-5.0], RightCluster).

	test(spectral_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer),
		export_to_file(two_blobs, Clusterer, clustered, File).

	test(spectral_export_to_file_4_loaded, deterministic(Cluster == Expected)) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [sigma(1.0), feature_scaling(off)]),
		cluster(Clusterer, [x-5.0, y-5.0], Expected),
		export_to_file(two_blobs, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		cluster(ExportedClusterer, [x-5.0, y-5.0], Cluster).

	test(spectral_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		learn(two_blobs, Clusterer),
		print_clusterer(Clusterer).

	test(spectral_cluster_3_duplicate_attribute, error(domain_error(attribute_occurrences(x, 1), 2))) :-
		learn(two_blobs, Clusterer),
		cluster(Clusterer, [x-1.0, x-1.1, y-1.0], _Cluster).

	test(spectral_cluster_3_undeclared_attribute, error(domain_error(declared_attribute(_), z))) :-
		learn(two_blobs, Clusterer),
		cluster(Clusterer, [x-1.0, y-1.0, z-2.0], _Cluster).

	test(spectral_learn_3_duplicate_training_attribute, error(domain_error(attribute_occurrences(x, 1), 2))) :-
		learn(invalid_spectral_dataset, _Clusterer).

	test(spectral_learn_3_mixed_profiles, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		learn(mixed_profiles, _Clusterer).

	test(spectral_learn_3_invalid_cluster_count, error(domain_error(cluster_count(1, 8), 9))) :-
		learn(two_blobs, _Clusterer, [k(9)]).

	test(spectral_learn_3_invalid_sigma, error(domain_error(option, sigma(0.0)))) :-
		learn(two_blobs, _Clusterer, [sigma(0.0)]).

	test(spectral_learn_3_auto_sigma_identical_points, error(domain_error(positive_pairwise_distance, 0.0))) :-
		learn(identical_points, _Clusterer, [k(2), sigma(auto), feature_scaling(off)]).

	test(spectral_learn_3_insufficient_components, error(domain_error(component_count, 2-1))) :-
		learn(identical_points, _Clusterer, [k(2), sigma(1.0), feature_scaling(off)]).

:- end_object.
