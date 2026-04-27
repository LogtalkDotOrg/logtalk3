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


:- object(invalid_gaussian_mixture_two_blobs,
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
		date is 2026-04-27,
		comment is 'Unit tests for the "gaussian_mixture" library.'
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(gaussian_mixture, [
		cluster/3, cluster_probabilities/3, clusterer_options/2, diagnostic/2, diagnostics/2, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1, valid_clusterer/1
	]).

	cover(gaussian_mixture).

	cleanup :-
		^^clean_file('test_output.pl').

	test(gaussian_mixture_learn_2_two_blobs, deterministic(ground(Clusterer))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]).

	test(gaussian_mixture_valid_clusterer_1_valid, deterministic(valid_clusterer(Clusterer))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]).

	test(gaussian_mixture_valid_clusterer_1_invalid, fail) :-
		learn(two_blobs, gaussian_mixture_clusterer(Encoders, Components, _Weights, Options, Diagnostics), [feature_scaling(off)]),
		valid_clusterer(gaussian_mixture_clusterer(Encoders, Components, [1.0], Options, Diagnostics)).

	test(gaussian_mixture_cluster_3_left_blob, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [k(2), initialization(spread), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0], Cluster).

	test(gaussian_mixture_cluster_3_right_blob, deterministic(Cluster == 2)) :-
		learn(two_blobs, Clusterer, [k(2), initialization(spread), feature_scaling(off)]),
		cluster(Clusterer, [x-5.1, y-5.0], Cluster).

	test(gaussian_mixture_cluster_probabilities_3_left_blob, deterministic((Cluster == 1, Probability1 > Probability2, Sum > 0.999999, Sum < 1.000001))) :-
		learn(two_blobs, Clusterer, [k(2), initialization(spread), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0], Cluster),
		cluster_probabilities(Clusterer, [x-1.0, y-1.0], Probabilities),
		Probabilities = [1-Probability1, 2-Probability2],
		Sum is Probability1 + Probability2.

	test(gaussian_mixture_cluster_probabilities_3_ignores_zero_weight_components, deterministic(Probabilities == [1-0.0, 2-1.0])) :-
		Clusterer = gaussian_mixture_clusterer(
			[continuous(x, 0.0, 1.0)],
			[component([0.0], [1.0]), component([10.0], [1.0])],
			[0.0, 1.0],
			[],
			[]
		),
		cluster_probabilities(Clusterer, [x-10.0], Probabilities).

	test(gaussian_mixture_cluster_3_missing_attribute, error(existence_error(attribute, y))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		cluster(Clusterer, [x-1.0], _Cluster).

	test(gaussian_mixture_cluster_3_duplicate_attribute, error(domain_error(attribute_occurrences(x, 1), 2))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, x-1.1, y-1.0], _Cluster).

	test(gaussian_mixture_cluster_3_undeclared_attribute, error(domain_error(declared_attribute(_), z))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0, z-2.0], _Cluster).

	test(gaussian_mixture_cluster_3_non_numeric_attribute, error(type_error(number, foo))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		cluster(Clusterer, [x-foo, y-1.0], _Cluster).

	test(gaussian_mixture_cluster_3_uninstantiated_attribute, error(instantiation_error)) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		cluster(Clusterer, [x-_, y-1.0], _Cluster).

	test(gaussian_mixture_learn_3_single_blob_k_1, deterministic((Cluster == 1, memberchk(components(1), Diagnostics), memberchk(convergence(tolerance), Diagnostics)))) :-
		learn(single_blob, Clusterer, [k(1), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-1.0], Cluster),
		diagnostics(Clusterer, Diagnostics).

	test(gaussian_mixture_learn_3_custom_options, deterministic((memberchk(k(3), Options), memberchk(initialization(first_k), Options), memberchk(feature_scaling(off), Options), memberchk(maximum_iterations(50), Options), memberchk(tolerance(0.0001), Options), memberchk(covariance_regularization(0.001), Options), memberchk(dead_component_policy(reseed), Options)))) :-
		learn(iris_unlabeled, gaussian_mixture_clusterer(_Encoders, _Components, _Weights, Options, _Diagnostics), [k(3), initialization(first_k), feature_scaling(off), maximum_iterations(50), tolerance(0.0001), covariance_regularization(0.001), dead_component_policy(reseed)]).

	test(gaussian_mixture_diagnostics_2, deterministic((memberchk(model(gaussian_mixture), Diagnostics), memberchk(training_examples(8), Diagnostics), memberchk(convergence(_), Diagnostics), memberchk(iterations(Iterations), Diagnostics), Iterations > 0, memberchk(average_log_likelihood(_), Diagnostics), memberchk(final_delta(_), Diagnostics), memberchk(options(Options), Diagnostics), memberchk(dead_component_policy(zero_weight), Options)))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics),
		clusterer_options(Clusterer, Options).

	test(gaussian_mixture_learn_3_dead_component_policy_reseed, deterministic(memberchk(dead_component_policy(reseed), Options))) :-
		learn(imbalanced_three_modes, Clusterer, [k(4), initialization(first_k), feature_scaling(off), dead_component_policy(reseed)]),
		clusterer_options(Clusterer, Options).

	test(gaussian_mixture_learn_3_dead_component_policy_runtime_difference, deterministic((memberchk(0.0, ZeroWeights), \+ member(0.0, ReseedWeights), ZeroWeights \== ReseedWeights, ZeroAssignments == ReseedAssignments, memberchk(dead_component_policy(zero_weight), ZeroOptions), memberchk(dead_component_policy(reseed), ReseedOptions)))) :-
		Options = [k(3), initialization(first_k), feature_scaling(off), maximum_iterations(50), covariance_regularization(1.0e-12)],
		learn(dead_component_blobs, ZeroClusterer, [dead_component_policy(zero_weight)| Options]),
		learn(dead_component_blobs, ReseedClusterer, [dead_component_policy(reseed)| Options]),
		ZeroClusterer = gaussian_mixture_clusterer(_ZeroEncoders, _ZeroComponents, ZeroWeights, _ZeroEffectiveOptions, _ZeroDiagnostics),
		ReseedClusterer = gaussian_mixture_clusterer(_ReseedEncoders, _ReseedComponents, ReseedWeights, _ReseedEffectiveOptions, _ReseedDiagnostics),
		training_assignments(dead_component_blobs, ZeroClusterer, ZeroAssignments),
		training_assignments(dead_component_blobs, ReseedClusterer, ReseedAssignments),
		clusterer_options(ZeroClusterer, ZeroOptions),
		clusterer_options(ReseedClusterer, ReseedOptions).

	test(gaussian_mixture_diagnostic_2_enumerates, deterministic(Enumerated == Diagnostics)) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		diagnostics(Clusterer, Diagnostics),
		findall(Diagnostic, diagnostic(Clusterer, Diagnostic), Enumerated).

	test(gaussian_mixture_cluster_3_iris_extremes, deterministic(Cluster1 \== Cluster2)) :-
		learn(iris_unlabeled, Clusterer, [k(3), maximum_iterations(50)]),
		cluster(Clusterer, [sepal_length-5.0, sepal_width-3.4, petal_length-1.5, petal_width-0.2], Cluster1),
		cluster(Clusterer, [sepal_length-6.8, sepal_width-3.1, petal_length-5.8, petal_width-2.2], Cluster2).

	test(gaussian_mixture_cluster_3_ignores_zero_weight_components, deterministic(Cluster == 2)) :-
		Clusterer = gaussian_mixture_clusterer(
			[continuous(x, 0.0, 1.0)],
			[component([0.0], [1.0]), component([10.0], [1.0])],
			[0.0, 1.0],
			[],
			[]
		),
		cluster(Clusterer, [x-0.0], Cluster).

	test(gaussian_mixture_learn_3_row_order_invariance_spread, deterministic(same_partition(OriginalAssignments, PermutedAssignments))) :-
		learn(two_blobs, OriginalClusterer, [k(2), initialization(spread), feature_scaling(off)]),
		learn(two_blobs_permuted, PermutedClusterer, [k(2), initialization(spread), feature_scaling(off)]),
		training_assignments(two_blobs, OriginalClusterer, OriginalAssignments),
		training_assignments(two_blobs, PermutedClusterer, PermutedAssignments).

	test(gaussian_mixture_export_to_clauses_4, deterministic(Cluster == 1)) :-
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_clauses(two_blobs, Clusterer, clustered, [ExportedClusterer]),
		cluster(ExportedClusterer, [x-0.9, y-1.1], Cluster).

	test(gaussian_mixture_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File).

	test(gaussian_mixture_export_to_file_4_loaded, deterministic(Cluster == 2)) :-
		^^file_path('test_output.pl', File),
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		export_to_file(two_blobs, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		cluster(ExportedClusterer, [x-5.0, y-5.0], Cluster).

	test(gaussian_mixture_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		learn(two_blobs, Clusterer, [feature_scaling(off)]),
		print_clusterer(Clusterer).

	test(gaussian_mixture_learn_3_duplicate_points_oversized_k, error(domain_error(cluster_count(1, 6), 7))) :-
		learn(duplicate_points, _Clusterer, [k(7), feature_scaling(off)]).

	test(gaussian_mixture_learn_3_maximum_iterations_termination, deterministic((memberchk(convergence(maximum_iterations), Diagnostics), memberchk(iterations(1), Diagnostics), memberchk(final_delta(Delta), Diagnostics), Delta > 0.0))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off), maximum_iterations(1), tolerance(1.0e-9)]),
		diagnostics(Clusterer, Diagnostics).

	test(gaussian_mixture_learn_3_tolerance_termination, deterministic((memberchk(convergence(tolerance), Diagnostics), memberchk(iterations(1), Diagnostics), memberchk(final_delta(Delta), Diagnostics), Delta > 0.0))) :-
		learn(two_blobs, Clusterer, [feature_scaling(off), maximum_iterations(50), tolerance(1000.0)]),
		diagnostics(Clusterer, Diagnostics).

	test(gaussian_mixture_learn_3_mixed_profiles, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		learn(mixed_profiles, _Clusterer).

	test(gaussian_mixture_learn_3_invalid_cluster_count, error(domain_error(cluster_count(1, 8), 9))) :-
		learn(two_blobs, _Clusterer, [k(9)]).

	test(gaussian_mixture_learn_3_duplicate_training_attribute, error(domain_error(attribute_occurrences(x, 1), 2))) :-
		learn(invalid_gaussian_mixture_two_blobs, _Clusterer, [feature_scaling(off)]).

	training_assignments(Dataset, Clusterer, Assignments) :-
		findall(
			Id-Cluster,
			(
				Dataset::example(Id, AttributeValues),
				cluster(Clusterer, AttributeValues, Cluster)
			),
			Assignments
		).

	same_partition(Assignments1, Assignments2) :-
		forall(
			(
				member(Id1-Cluster1, Assignments1),
				member(Id2-Cluster2, Assignments1),
				Id1 < Id2
			),
			(
				same_cluster_relation(Assignments2, Id1, Id2, Same2),
				(Cluster1 == Cluster2 -> Same1 = true; Same1 = false),
				Same1 == Same2
			)
		).

	same_cluster_relation(Assignments, Id1, Id2, Same) :-
		memberchk(Id1-Cluster1, Assignments),
		memberchk(Id2-Cluster2, Assignments),
		(Cluster1 == Cluster2 -> Same = true; Same = false).

:- end_object.
