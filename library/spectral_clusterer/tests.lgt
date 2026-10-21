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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Unit tests for the "spectral_clusterer" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(spectral_clusterer, [
		cluster/3, diagnostics/2, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1, valid_clusterer/1
	]).

	cover(spectral_clusterer).

	:- private(cached_model_/2).
	:- dynamic(cached_model_/2).

	setup :-
		retractall(cached_model_(_, _)).

	cleanup :-
		retractall(cached_model_(_, _)),
		^^clean_file('test_output.pl').

	test(spectral_learn_2_two_blobs, deterministic(ground(Clusterer))) :-
		learn(two_blobs, Clusterer).

	test(spectral_valid_clusterer_1_valid, deterministic(valid_clusterer(Clusterer))) :-
		model_fixture(default, Clusterer).

	test(spectral_valid_clusterer_1_invalid, fail) :-
		model_fixture(default, spectral_clusterer(Encoders, Rows, Degrees, Components, Eigenvalues, Projection, Centroids, Sigma, Options, _Diagnostics)),
		valid_clusterer(spectral_clusterer(Encoders, Rows, Degrees, Components, Eigenvalues, Projection, Centroids, Sigma, Options, [model(spectral_clusterer), cluster_count(99), options(Options)])).

	test(spectral_cluster_3_two_blobs, deterministic(LeftCluster \== RightCluster)) :-
		model_fixture(explicit_sigma, Clusterer),
		cluster(Clusterer, [x-1.0, y-1.0], LeftCluster),
		cluster(Clusterer, [x-5.0, y-5.0], RightCluster).

	test(spectral_cluster_3_nystrom_nearby_points, deterministic((Left1 == Left2, Right1 == Right2, Left1 \== Right1))) :-
		model_fixture(explicit_sigma, Clusterer),
		cluster(Clusterer, [x-1.0, y-1.0], Left1),
		cluster(Clusterer, [x-1.2, y-0.9], Left2),
		cluster(Clusterer, [x-5.0, y-5.0], Right1),
		cluster(Clusterer, [x-5.2, y-4.9], Right2).

	test(spectral_cluster_3_concentric_rings, deterministic((Inner1 == Inner2, Outer1 == Outer2, Inner1 \== Outer1))) :-
		learn(concentric_rings, Clusterer, [k(2), sigma(0.4), maximum_iterations(20), tolerance(1.0e-4), eigensolver_tolerance(1.0e-6), feature_scaling(off)]),
		cluster(Clusterer, [x-1.0, y-0.0], Inner1),
		cluster(Clusterer, [x-0.0, y-1.0], Inner2),
		cluster(Clusterer, [x-3.0, y-0.0], Outer1),
		cluster(Clusterer, [x-0.0, y-3.0], Outer2),
		forall(
			concentric_rings::example(Id, Instance),
			(	cluster(Clusterer, Instance, Assignment),
				atom_chars(Id, [Ring| _]),
				(	Ring == i ->
					assertion(Assignment == Inner1)
				;	assertion(Assignment == Outer1)
				)
			)
		),
		diagnostics(Clusterer, RingDiagnostics),
		memberchk(maximum_eigenpair_residual(Residual), RingDiagnostics),
		Residual =< 1.0e-6.

	test(spectral_learn_3_custom_options, deterministic([K, Sigma, MaximumIterations, Tolerance, Initialization, FeatureScaling] == [3, 2.0, 40, 1.0e-5, first_k, off])) :-
		learn(iris_unlabeled, spectral_clusterer(_Encoders, _Rows, _Degrees, _Components, _Eigenvalues, _Projection, _Centroids, _ResolvedSigma, Options, _Diagnostics), [k(3), sigma(2.0), maximum_iterations(40), tolerance(1.0e-5), initialization(first_k), feature_scaling(off)]),
		memberchk(k(K), Options),
		memberchk(sigma(Sigma), Options),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(initialization(Initialization), Options),
		memberchk(feature_scaling(FeatureScaling), Options).

	test(spectral_diagnostics_2_rich_metadata, deterministic((ClusterCount == 2, TrainingExampleCount == 8, ResolvedSigma > 0.0, Iterations >= 1, FinalShift >= 0.0))) :-
		model_fixture(default, Clusterer),
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
		model_fixture(explicit_sigma, Clusterer),
		export_to_clauses(two_blobs, Clusterer, clustered, [ExportedClusterer]),
		cluster(ExportedClusterer, [x-1.0, y-1.0], LeftCluster),
		cluster(ExportedClusterer, [x-5.0, y-5.0], RightCluster).

	test(spectral_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		model_fixture(default, Clusterer),
		export_to_file(two_blobs, Clusterer, clustered, File).

	test(spectral_export_to_file_4_loaded, deterministic(Cluster == Expected)) :-
		^^file_path('test_output.pl', File),
		model_fixture(explicit_sigma, Clusterer),
		cluster(Clusterer, [x-5.0, y-5.0], Expected),
		export_to_file(two_blobs, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		cluster(ExportedClusterer, [x-5.0, y-5.0], Cluster).

	test(spectral_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		model_fixture(default, Clusterer),
		print_clusterer(Clusterer).

	test(spectral_cluster_3_duplicate_attribute, error(domain_error(attribute_occurrences(x, 1), 2))) :-
		model_fixture(default, Clusterer),
		cluster(Clusterer, [x-1.0, x-1.1, y-1.0], _Cluster).

	test(spectral_cluster_3_undeclared_attribute, error(domain_error(declared_attribute(_), z))) :-
		model_fixture(default, Clusterer),
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

	test(spectral_clusterer_validation_dimensions, all(\+ spectral_clusterer::valid_clusterer(Changed))) :-
		model_fixture(default, Model),
		member(Index-Value, [1-[], 2-[], 3-[], 4-[], 5-[], 6-[], 7-[], 8-0.0, 10-[]]),
		replace_model_argument(Model, Index, Value, Changed).

	test(spectral_clusterer_validation_options, all(\+ spectral_clusterer::valid_clusterer(Changed))) :-
		model_fixture(default, Model),
		member(Option, [k(1), k(9), sigma(99.0)]),
		prepend_model_options(Model, [Option], Changed).

	test(spectral_clusterer_validation_repeated_options, deterministic(valid_clusterer(Changed))) :-
		model_fixture(default, Model),
		prepend_model_options(Model, [k(2), k(1)], Changed).

	test(spectral_clusterer_validation_exported_functor, deterministic(valid_clusterer(Exported))) :-
		model_fixture(default, Model),
		export_to_clauses(two_blobs, Model, renamed, [Exported]).

	test(spectral_clusterer_validation_empty, fail) :-
		valid_clusterer(spectral_clusterer([], [], [], [], [], [], [], 1.0, [k(1), sigma(auto)], [model(spectral_clusterer), options([k(1), sigma(auto)]), cluster_count(0), training_example_count(0), resolved_sigma(1.0), eigenvalues([])])).

	test(spectral_clusterer_validation_wrong_arity, error(domain_error(clusterer, bad(model)))) :-
		spectral_clusterer::check_clusterer(bad(model)).

	test(spectral_clusterer_validation_atom, error(domain_error(clusterer, bad))) :-
		spectral_clusterer::check_clusterer(bad).

	test(spectral_clusterer_validation_variable, error(instantiation_error)) :-
		spectral_clusterer::check_clusterer(_).

	test(spectral_clusterer_validation_nonground, error(instantiation_error)) :-
		spectral_clusterer::check_clusterer(bad(_)).

	test(spectral_clusterer_prediction_zero_degree, error(domain_error(positive_spectral_value(instance_degree), 0.0))) :-
		model_fixture(default, Model),
		cluster(Model, [x-10000.0, y-10000.0], _).

	test(spectral_clusterer_spread_three_clusters, deterministic) :-
		learn(iris_unlabeled, Model, [k(3), initialization(spread), maximum_iterations(40)]),
		valid_clusterer(Model).

	test(spectral_clusterer_affinity_reference, deterministic(Affinity =~= Expected)) :-
		spectral_clusterer<<rbf_affinity([1.0, 2.0], [3.0, 5.0], 2.0, Affinity),
		Expected is exp(-13.0 / 8.0).

	test(spectral_clusterer_affinity_tiny_sigma, deterministic(Affinity =:= 0)) :-
		spectral_clusterer<<rbf_affinity([1.0, 2.0, 3.0], [1.0, 4.0, 6.0], 1.0e-200, Affinity).

	test(spectral_clusterer_affinity_large_sigma, deterministic(Affinity =~= 1.0)) :-
		spectral_clusterer<<rbf_affinity([1.0], [2.0], 1.0e200, Affinity).

	test(spectral_clusterer_affinity_identical, deterministic(Affinity =~= 1.0)) :-
		spectral_clusterer<<rbf_affinity([1.0], [1.0], 1.0e-200, Affinity).

	test(spectral_clusterer_large_sigma_learning, deterministic) :-
		learn(two_blobs, Model, [k(1), sigma(1.0e200)]),
		valid_clusterer(Model).

	test(spectral_clusterer_large_sigma_rank, error(domain_error(component_count, 2-1))) :-
		learn(two_blobs, _, [k(2), sigma(1.0e200)]).

	test(spectral_clusterer_tiny_sigma_learning, deterministic) :-
		learn(identical_points, Model, [k(1), sigma(1.0e-200)]),
		valid_clusterer(Model).

	test(spectral_clusterer_embedding_extreme_norm, deterministic(Normalized =~= [0.6, 0.8])) :-
		spectral_clusterer<<normalize_embedding_row([3.0e200, 4.0e200], 0.0, Normalized).

	test(spectral_clusterer_embedding_zero, error(domain_error(positive_spectral_value(embedding_norm), 0.0))) :-
		spectral_clusterer<<normalize_embedding_row([0.0], 0.0, _).

	test(spectral_clusterer_logarithmic_duplicate, deterministic(Affinity =~= 1.0)) :-
		spectral_clusterer<<logarithmic_affinity([1.0], [1.0], 1.0, Affinity).

	test(spectral_clusterer_logarithmic_reference, deterministic(Affinity =~= Expected)) :-
		spectral_clusterer<<logarithmic_affinity([0.0], [1.0], 1.0, Affinity),
		Expected is exp(-0.5).

	test(spectral_clusterer_spectral_exhaustion, error(domain_error(spectral_convergence, _))) :-
		learn(two_blobs, _, [eigensolver_maximum_iterations(1), eigensolver_tolerance(1.0e-12)]).

	test(spectral_clusterer_spectral_cutoff, error(domain_error(component_count, 2-0))) :-
		learn(two_blobs, _, [eigenvalue_cutoff(2.0)]).

	test(spectral_clusterer_spectral_independent_controls, deterministic) :-
		learn(two_blobs, Model, [tolerance(1.0), eigensolver_tolerance(1.0e-10)]),
		diagnostics(Model, Diagnostics),
		memberchk(eigensolver_convergence(tolerance), Diagnostics),
		memberchk(maximum_eigenpair_residual(Residual), Diagnostics),
		Residual =< 1.0e-10,
		cluster(Model, [x-1.0, y-1.0], _).

	test(spectral_clusterer_spectral_identity, deterministic) :-
		learn(two_blobs, Model, [k(2), sigma(1.0e-200)]),
		valid_clusterer(Model).

	test(spectral_clusterer_spectral_zero_cutoff, deterministic) :-
		learn(two_blobs, Model, [k(1), eigenvalue_cutoff(0.0)]),
		valid_clusterer(Model).

	test(spectral_clusterer_spectral_options, all(\+ spectral_clusterer::valid_option(Option))) :-
		member(Option, [eigensolver_tolerance(-1.0), eigenvalue_cutoff(-1.0), eigensolver_maximum_iterations(0)]).

	test(spectral_clusterer_symmetric_affinity, all(Actual =~= Expected)) :-
		spectral_clusterer<<build_affinity_matrix([[0.0], [1.0], [1.0]], 2.0, Matrix),
		Matrix = [[Diagonal1, Pair12, Pair13], [Pair21, Diagonal2, Pair23], [Pair31, Pair32, Diagonal3]],
		assertion([Diagonal1, Diagonal2, Diagonal3, Pair23] =~= [1.0, 1.0, 1.0, 1.0]),
		assertion(Pair12 == Pair21),
		assertion(Pair13 == Pair31),
		assertion(Pair23 == Pair32),
		spectral_clusterer<<row_sums(Matrix, Degrees),
		spectral_clusterer<<build_normalized_adjacency(Matrix, Degrees, Normalized),
		list::nth1(RowIndex, Matrix, Row),
		list::nth1(ColumnIndex, Row, Affinity),
		list::nth1(RowIndex, Degrees, Degree),
		list::nth1(ColumnIndex, Degrees, OtherDegree),
		list::nth1(RowIndex, Normalized, NormalizedRow),
		list::nth1(ColumnIndex, NormalizedRow, Actual),
		Expected is Affinity / sqrt(Degree * OtherDegree).

	test(spectral_clusterer_singleton_affinity, deterministic(Matrix =~= [[1.0]])) :-
		spectral_clusterer<<build_affinity_matrix([[0.0]], 1.0, Matrix).

	test(spectral_clusterer_order_statistics_reference, all(Actual == Expected)) :-
		member(Keys, [[1], [4, 1, 9, 2], [8, 3, 1, 8, 2, 6, 3, 9, 4, 1, 7, 5, 8], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], [10, 9, 8, 7, 6, 5, 4, 3, 2, 1], [2, 2, 2, 2, 2, 2]]),
		list::msort(Keys, Sorted),
		list::nth0(Index, Sorted, Expected),
		spectral_clusterer<<order_statistic(Keys, Index, Actual).

	test(spectral_clusterer_sigma_even_distances, deterministic(Sigma =~= 2.5)) :-
		spectral_clusterer<<median_distance_keys([4.0, 9.0], squared, Sigma).

	test(spectral_clusterer_sigma_odd_distances, deterministic(Sigma =~= 3.0)) :-
		spectral_clusterer<<median_distance_keys([16.0, 9.0, 4.0], squared, Sigma).

	test(spectral_clusterer_sigma_duplicate_exclusion, deterministic(Keys =~= [1.0, 1.0])) :-
		spectral_clusterer<<pairwise_distance_keys([[0.0], [1.0], [1.0]], squared, Keys).

	test(spectral_clusterer_sigma_leading_duplicate, deterministic(Keys =~= [1.0, 1.0])) :-
		spectral_clusterer<<pairwise_distance_keys([[0.0], [0.0], [1.0]], squared, Keys).

	test(spectral_clusterer_sigma_large_distances, deterministic(Sigma =~= 1.0e200)) :-
		spectral_clusterer<<resolve_sigma([[0.0], [1.0e200], [2.0e200], [2.0e200]], [sigma(auto)], Sigma).

	test(spectral_clusterer_sigma_tiny_distances, deterministic(Ratio =~= 1.0)) :-
		spectral_clusterer<<resolve_sigma([[0.0], [1.0e-200]], [sigma(auto)], Sigma),
		Sigma > 0.0,
		Ratio is Sigma / 1.0e-200.

	test(spectral_clusterer_centroid_aggregation, deterministic(Updated =~= [[1.0], [10.0], [100.0]])) :-
		spectral_clusterer<<update_centroids([[0.0], [2.0], [10.0]], [[0.0], [10.0], [100.0]], Updated).

	test(spectral_clusterer_centroid_empty_rows, deterministic(Updated =~= [[0.0], [10.0]])) :-
		spectral_clusterer<<update_centroids([], [[0.0], [10.0]], Updated).

	test(spectral_clusterer_centroid_tie, deterministic(Cluster == 1)) :-
		spectral_clusterer<<nearest_centroid([[0.0], [2.0]], [1.0], Cluster, _).

	test(spectral_clusterer_centroid_duplicate_rows, deterministic(Updated =~= [[Expected]])) :-
		Expected is 2.0 / 3.0,
		spectral_clusterer<<update_centroids([[0.0], [0.0], [2.0]], [[0.0]], Updated).

	test(spectral_clusterer_prepared_projection_reference, deterministic) :-
		model_fixture(explicit_sigma, Model),
		Model = spectral_clusterer(_, Rows, Degrees, Components, Eigenvalues, Projection, Centroids, Sigma, _, _),
		forall(
			member(Features, [[1.0, 1.0], [1.2, 0.9], [5.0, 5.0], [5.2, 4.9]]),
			(	spectral_clusterer<<affinity_vector(Rows, Features, Sigma, Affinities, 0.0, _),
				numberlist::sum(Affinities, Degree),
				reference_affinities(Affinities, Degrees, Degree, NormalizedAffinities),
				reference_projection(Components, Eigenvalues, NormalizedAffinities, Reference0),
				spectral_clusterer<<normalize_embedding_row(Reference0, 0.0, Reference),
				spectral_clusterer<<project_prepared(Projection, Affinities, Prepared0),
				spectral_clusterer<<normalize_embedding_row(Prepared0, 0.0, Prepared),
				assertion(Prepared =~= Reference),
				spectral_clusterer<<nearest_centroid(Centroids, Reference, Expected, _),
				Features = [X, Y],
				cluster(Model, [x-X, y-Y], Actual),
				assertion(Actual == Expected)
			)
		).

	test(spectral_clusterer_old_model_rejected, fail) :-
		model_fixture(default, spectral_clusterer(Encoders, Rows, Degrees, Components, Values, _, Centroids, Sigma, Options, Diagnostics)),
		valid_clusterer(spectral_clusterer(Encoders, Rows, Degrees, Components, Values, Centroids, Sigma, Options, Diagnostics)).

	test(spectral_clusterer_projection_nonground, error(instantiation_error)) :-
		model_fixture(default, Model),
		replace_model_argument(Model, 6, [[_]], Changed),
		spectral_clusterer::check_clusterer(Changed).

	test(spectral_clusterer_projection_nonnumeric, fail) :-
		model_fixture(default, Model),
		Model = spectral_clusterer(_, _, _, _, _, [[_| Tail]| Rows], _, _, _, _),
		replace_model_argument(Model, 6, [[bad| Tail]| Rows], Changed),
		valid_clusterer(Changed).

	test(spectral_clusterer_validation_spectral_diagnostics, all(\+ spectral_clusterer::valid_clusterer(Changed))) :-
		model_fixture(default, Model),
		diagnostics(Model, Diagnostics),
		member(Replacement, [eigensolver_convergence(maximum_iterations), eigensolver_iterations([0]), eigenpair_residuals([-1.0, 0.0]), maximum_eigenpair_residual(-1.0), orthogonality_error(-1.0)]),
		functor(Replacement, Functor, 1),
		functor(Original, Functor, 1),
		list::selectchk(Original, Diagnostics, OtherDiagnostics),
		replace_model_argument(Model, 10, [Replacement| OtherDiagnostics], Changed).

	test(spectral_clusterer_empty_attributes, error(domain_error(non_empty_attributes, empty_attributes))) :-
		learn(empty_attributes, _, [k(1), sigma(1.0)]).

	test(spectral_clusterer_single_point_learning, deterministic(Cluster == 1)) :-
		learn(single_point, Model, [k(1), sigma(1.0)]),
		valid_clusterer(Model),
		cluster(Model, [x-1.0], Cluster).

	test(spectral_clusterer_all_components_learning, deterministic) :-
		learn(two_points, Model, [k(2), sigma(1.0e-200)]),
		valid_clusterer(Model).

	test(spectral_clusterer_projection_underflow_scaling, deterministic(Embedding =~= [1.0])) :-
		spectral_clusterer<<projection_embedding([[1.0e-200]], [1.0e-200], Embedding).

	test(spectral_clusterer_spread_reference, all(Actual == Expected)) :-
		member(Rows, [[[0.0], [1.0], [-1.0], [1.0]], [[2.0], [2.0], [2.0], [2.0]], [[0], [0.0], [1], [1.0]], [[0.0, 0.0], [1.0, 2.0], [3.0, -1.0], [-2.0, 1.0], [4.0, 3.0]]]),
		list::nth1(Count, Rows, _),
		reference_spread(Count, Rows, Expected),
		spectral_clusterer<<initialize_centroids(spread, Count, Rows, Actual).

	test(spectral_clusterer_spread_tie_order, deterministic(Centroids == [[0.0], [1.0], [-1.0], [1.0]])) :-
		spectral_clusterer<<initialize_centroids(spread, 4, [[0.0], [1.0], [-1.0], [1.0]], Centroids).

	test(spectral_clusterer_first_k_order, deterministic(Centroids == [[0.0], [1.0], [-1.0]])) :-
		spectral_clusterer<<initialize_centroids(first_k, 3, [[0.0], [1.0], [-1.0], [1.0]], Centroids).

	test(spectral_clusterer_affinity_maximum_reference, all((Actual == Expected, ActualMaximum == ExpectedMaximum))) :-
		member(case(Rows, Features, Sigma), [case([[0.0], [1.0], [2.0]], [0.0], 1.0), case([[0.0], [1.0], [2.0]], [1.25], 1.0), case([[0.0], [1.0], [2.0]], [2.0], 1.0e-200), case([[0.0], [1.0]], [10000.0], 1.0)]),
		findall(
			Affinity,
			(	member(Row, Rows),
				spectral_clusterer<<rbf_affinity(Row, Features, Sigma, Affinity)
			),
			Expected
		),
		spectral_clusterer<<maximum_absolute(Expected, 0.0, ExpectedMaximum),
		spectral_clusterer<<affinity_vector(Rows, Features, Sigma, Actual, 0.0, ActualMaximum).

	test(spectral_clusterer_prediction_single_positive_affinity, deterministic(Cluster == 1)) :-
		learn(two_points, Model, [k(1), sigma(1.0e-200), feature_scaling(off)]),
		cluster(Model, [x-1.0], Cluster).

	test(spectral_clusterer_prediction_underflow_projection, deterministic(Cluster == 1)) :-
		learn(single_point, Model, [k(1), sigma(1.0), feature_scaling(off)]),
		replace_model_argument(Model, 6, [[1.0e-200]], Prepared),
		Query is 1.0 + sqrt(2.0 * log(1.0e200)),
		cluster(Prepared, [x-Query], Cluster).

	% auxiliary predicates

	reference_spread(Count, [First| Rows], [First| Centroids]) :-
		Remaining is Count - 1,
		reference_spread_select(Remaining, Rows, [First], Centroids).

	reference_spread_select(0, _Rows, _Selected, []) :-
		!.
	reference_spread_select(Count, [First| Rows], Selected, [Best| Centroids]) :-
		reference_nearest_distance(Selected, First, Distance),
		reference_farthest(Rows, Selected, First, Distance, Best),
		list::selectchk(Best, [First| Rows], RemainingRows),
		Remaining is Count - 1,
		reference_spread_select(Remaining, RemainingRows, [Best| Selected], Centroids).

	reference_farthest([], _Selected, Best, _Distance, Best).
	reference_farthest([Row| Rows], Selected, Best0, Distance0, Best) :-
		reference_nearest_distance(Selected, Row, Distance),
		(	Distance > Distance0 ->
			Best1 = Row,
			Distance1 = Distance
		;	Best1 = Best0,
			Distance1 = Distance0
		),
		reference_farthest(Rows, Selected, Best1, Distance1, Best).

	reference_nearest_distance([Centroid| Centroids], Row, Minimum) :-
		spectral_clusterer<<squared_euclidean_distance(Row, Centroid, 0.0, Distance),
		reference_minimum_distance(Centroids, Row, Distance, Minimum).

	reference_minimum_distance([], _Row, Minimum, Minimum).
	reference_minimum_distance([Centroid| Centroids], Row, Minimum0, Minimum) :-
		spectral_clusterer<<squared_euclidean_distance(Row, Centroid, 0.0, Distance),
		Minimum1 is min(Minimum0, Distance),
		reference_minimum_distance(Centroids, Row, Minimum1, Minimum).

	model_fixture(Key, Model) :-
		(	cached_model_(Key, Model) ->
			true
		;	(Key == default -> Options = []; Options = [k(2), sigma(1.0), feature_scaling(off)]),
			learn(two_blobs, Model, Options),
			assertz(cached_model_(Key, Model))
		).

	reference_affinities([], [], _Degree, []).
	reference_affinities([Affinity| Affinities], [TrainingDegree| Degrees], Degree, [Value| Values]) :-
		Value is Affinity / sqrt(Degree * TrainingDegree),
		reference_affinities(Affinities, Degrees, Degree, Values).

	reference_projection([], [], _Affinities, []).
	reference_projection([Component| Components], [Eigenvalue| Eigenvalues], Affinities, [Coordinate| Coordinates]) :-
		linear_algebra::dot_product(Component, Affinities, Product),
		Coordinate is Product / Eigenvalue,
		reference_projection(Components, Eigenvalues, Affinities, Coordinates).

	replace_model_argument(Model, Index, Value, Changed) :-
		Model =.. [Functor| Arguments],
		replace_argument(Arguments, Index, Value, ChangedArguments),
		Changed =.. [Functor| ChangedArguments].

	replace_argument([_| Arguments], 1, Value, [Value| Arguments]) :-
		!.
	replace_argument([Argument| Arguments], Index, Value, [Argument| ChangedArguments]) :-
		Next is Index - 1,
		replace_argument(Arguments, Next, Value, ChangedArguments).

	prepend_model_options(spectral_clusterer(Encoders, Rows, Degrees, Components, Eigenvalues, Projection, Centroids, Sigma, Options, Diagnostics), Prefix, spectral_clusterer(Encoders, Rows, Degrees, Components, Eigenvalues, Projection, Centroids, Sigma, ChangedOptions, ChangedDiagnostics)) :-
		list::append(Prefix, Options, ChangedOptions),
		list::selectchk(options(Options), Diagnostics, OtherDiagnostics),
		ChangedDiagnostics = [options(ChangedOptions)| OtherDiagnostics].

:- end_object.
