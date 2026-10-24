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
		version is 2:1:0,
		author is 'Paulo Moura',
		date is 2026-10-24,
		comment is 'Unit tests for the "umap_projection" library.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, nth1/3, select/3
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(fast_random(xoshiro128pp), [
		get_seed/1, set_seed/1, randomize/1
	]).

	cover(umap_projection).

	:- private(cached_reducer/2).
	:- dynamic(cached_reducer/2).

	setup :-
		retractall(cached_reducer(_, _)),
		umap_projection::learn(correlated_plane, Plane, [n_neighbors(3), initialization(random), maximum_iterations(5), transform_seed(19), negative_sample_rate(1)]),
		assertz(cached_reducer(plane, Plane)),
		umap_projection::learn(mixed_umap_dataset, Mixed, [feature_scaling(false), n_neighbors(2), initialization(random), maximum_iterations(5)]),
		assertz(cached_reducer(mixed, Mixed)),
		umap_projection::learn(mixed_umap_dataset, Validation, [n_neighbors(3), initialization(random), maximum_iterations(2)]),
		assertz(cached_reducer(validation, Validation)).

	cleanup :-
		retractall(cached_reducer(_, _)).

	test(umap_learn_3_structure, deterministic(functor(DimensionReducer, umap_reducer, 6))) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), maximum_iterations(5)]).

	test(umap_learn_3_effective_neighbors, deterministic(NeighborCount == 8)) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [maximum_iterations(5)]),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(effective_n_neighbors(NeighborCount), Diagnostics).

	test(umap_learn_3_metric_manhattan, deterministic(Metric == manhattan)) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), distance_metric(manhattan), initialization(random), maximum_iterations(5)]),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(distance_metric(Metric), Diagnostics).

	test(umap_learn_3_cosine_zero_rows, deterministic) :-
		umap_projection::learn(cosine_umap_dataset, DimensionReducer, [n_neighbors(2), distance_metric(cosine), feature_scaling(false), initialization(random), maximum_iterations(5)]),
		umap_projection::check_dimension_reducer(DimensionReducer).

	test(umap_learn_3_missing_values, deterministic([XMean, MissingX, MissingY] == [3.0, 0.0, 0.0])) :-
		umap_projection::learn(missing_values_umap_dataset, umap_reducer(Encoders, _ExampleIds, TrainingRows, _EmbeddingRows, _FuzzyGraph, _Diagnostics), [n_neighbors(2), initialization(random), maximum_iterations(5)]),
		memberchk(continuous(x, XMean, _XScale), Encoders),
		TrainingRows = [_FirstRow, [MissingX, _SecondY], [_ThirdX, MissingY]| _].

	test(umap_learn_3_categorical_encoding, deterministic((
		Encoders == [continuous(age, 35.0, 1.0), categorical(channel, [online, retail])],
		TrainingRows == [
			[-15.0, 1.0, 0.0, 0.0],
			[-5.0, 0.0, 1.0, 0.0],
			[5.0, 0.0, 0.0, 1.0],
			[15.0, 1.0, 0.0, 0.0]
		]
	))) :-
		umap_projection::learn(mixed_umap_dataset, DimensionReducer, [feature_scaling(false), n_neighbors(2), initialization(random), maximum_iterations(5)]),
		DimensionReducer = umap_reducer(Encoders, _ExampleIds, TrainingRows, _EmbeddingRows, _FuzzyGraph, _Diagnostics),
		umap_projection::check_dimension_reducer(DimensionReducer).

	test(umap_learn_3_categorical_reducer, deterministic((EncodedFeatureCount == 4, ComponentCount == 2))) :-
		umap_projection::learn(categorical_umap_dataset, DimensionReducer, [n_components(2), n_neighbors(2), initialization(random), maximum_iterations(5)]),
		umap_projection::check_dimension_reducer(DimensionReducer),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(encoded_feature_count(EncodedFeatureCount), Diagnostics),
		memberchk(component_count(ComponentCount), Diagnostics).

	test(umap_learn_3_mixed_feature_counts, deterministic((FeatureCount == 2, EncodedFeatureCount == 4))) :-
		cached_model(mixed, DimensionReducer),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(feature_count(FeatureCount), Diagnostics),
		memberchk(encoded_feature_count(EncodedFeatureCount), Diagnostics).

	test(umap_learn_3_mixed_metrics, deterministic) :-
		check_mixed_metric(euclidean),
		check_mixed_metric(manhattan),
		check_mixed_metric(cosine).

	test(umap_transform_3_categorical, deterministic) :-
		cached_model(mixed, DimensionReducer),
		umap_projection::transform(DimensionReducer, [age-35.0, channel-retail], ReducedInstance),
		length(ReducedInstance, 2).

	test(umap_transform_3_missing_categorical, deterministic) :-
		cached_model(mixed, DimensionReducer),
		umap_projection::transform(DimensionReducer, [age-35.0, channel-_], ReducedInstance),
		length(ReducedInstance, 2).

	test(umap_transform_3_invalid_categorical_value, error(domain_error(attribute_value(channel, [online, retail]), phone))) :-
		cached_model(mixed, DimensionReducer),
		umap_projection::transform(DimensionReducer, [age-35.0, channel-phone], _ReducedInstance).

	test(umap_learn_3_invalid_categorical_value, error(domain_error(attribute_value(channel, [online, retail]), phone))) :-
		umap_projection::learn(invalid_categorical_value_umap_dataset, _DimensionReducer, [n_neighbors(2), maximum_iterations(5)]).

	test(umap_learn_3_empty_categorical_domain, error(domain_error(attribute_declarations, channel))) :-
		umap_projection::learn(empty_categorical_domain_umap_dataset, _DimensionReducer, [n_neighbors(2), maximum_iterations(5)]).

	test(umap_learn_3_duplicate_categorical_domain, error(domain_error(attribute_declarations, channel))) :-
		umap_projection::learn(duplicate_categorical_domain_umap_dataset, _DimensionReducer, [n_neighbors(2), maximum_iterations(5)]).

	test(umap_learn_3_variable_categorical_domain, error(domain_error(attribute_declarations, channel))) :-
		umap_projection::learn(variable_categorical_domain_umap_dataset, _DimensionReducer, [n_neighbors(2), maximum_iterations(5)]).

	test(umap_learn_3_improper_categorical_domain, error(domain_error(attribute_declarations, channel))) :-
		umap_projection::learn(improper_categorical_domain_umap_dataset, _DimensionReducer, [n_neighbors(2), maximum_iterations(5)]).

	test(umap_learn_3_all_missing_categorical, error(domain_error(observed_attribute_values, channel))) :-
		umap_projection::learn(all_missing_categorical_umap_dataset, _DimensionReducer, [n_neighbors(2), maximum_iterations(5)]).

	test(umap_transform_3_shape, deterministic) :-
		cached_model(plane, DimensionReducer),
		umap_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance),
		length(ReducedInstance, 2),
		memberchk(component_1-Component1, ReducedInstance),
		memberchk(component_2-Component2, ReducedInstance),
		number(Component1),
		number(Component2).

	test(umap_transform_3_seed_reproducibility, deterministic(ReducedInstance1 == ReducedInstance2)) :-
		cached_model(plane, DimensionReducer),
		Instance = [x-2.75, y-5.5, z-8.25],
		umap_projection::transform(DimensionReducer, Instance, ReducedInstance1),
		umap_projection::transform(DimensionReducer, Instance, ReducedInstance2).

	test(umap_transform_3_restores_random_state, deterministic(SeedBefore == SeedAfter)) :-
		cached_model(plane, DimensionReducer),
		get_seed(SeedBefore),
		umap_projection::transform(DimensionReducer, [x-2.75, y-5.5, z-8.25], _ReducedInstance),
		get_seed(SeedAfter).

	test(umap_learn_3_fuzzy_graph, deterministic) :-
		cached_model(plane, umap_reducer(_Encoders, _ExampleIds, _TrainingRows, _EmbeddingRows, FuzzyGraph, Diagnostics)),
		FuzzyGraph = [edge(FirstSource, FirstTarget, FirstWeight)| _],
		FirstSource < FirstTarget,
		FirstWeight > 0.0,
		FirstWeight =< 1.0,
		memberchk(graph_edge_count(EdgeCount), Diagnostics),
		length(FuzzyGraph, EdgeCount).

	test(umap_learn_3_curve_parameters, deterministic((abs(CurveA - 1.57694) < 0.05, abs(CurveB - 0.89506) < 0.05))) :-
		cached_model(plane, DimensionReducer),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(curve_parameters(CurveA, CurveB), Diagnostics).

	test(umap_learn_3_spectral_initialization, deterministic(InitializationUsed == spectral)) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), maximum_iterations(5)]),
		umap_projection::check_dimension_reducer(DimensionReducer),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(initialization_used(InitializationUsed), Diagnostics).

	test(umap_learn_3_random_initialization, deterministic(InitializationUsed == random)) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), initialization(random), maximum_iterations(5)]),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(initialization_used(InitializationUsed), Diagnostics).

	test(umap_learn_3_restores_random_state, deterministic(SeedBefore == SeedAfter)) :-
		get_seed(SeedBefore),
		umap_projection::learn(correlated_plane, _DimensionReducer, [n_neighbors(3), maximum_iterations(5)]),
		get_seed(SeedAfter).

	test(umap_learn_3_seed_reproducibility, deterministic(Embedding1 == Embedding2)) :-
		Options = [n_neighbors(3), maximum_iterations(2), random_seed(27)],
		umap_projection::learn(correlated_plane, umap_reducer(_Encoders1, _ExampleIds1, _TrainingRows1, Embedding1, _FuzzyGraph1, _Diagnostics1), Options),
		umap_projection::learn(correlated_plane, umap_reducer(_Encoders2, _ExampleIds2, _TrainingRows2, Embedding2, _FuzzyGraph2, _Diagnostics2), Options).

	test(umap_learn_3_optimization_diagnostics, deterministic((Iterations == 5, number(InitialCrossEntropy), number(FinalCrossEntropy)))) :-
		cached_model(plane, DimensionReducer),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(initial_cross_entropy(InitialCrossEntropy), Diagnostics),
		memberchk(final_cross_entropy(FinalCrossEntropy), Diagnostics).

	test(umap_learn_2_singleton_dataset, error(domain_error(minimum_number_of_examples, 1))) :-
		umap_projection::learn(singleton_measurement, _DimensionReducer).

	test(umap_learn_3_min_dist_above_spread, error(domain_error(min_dist_spread, 2.0-1.0))) :-
		umap_projection::learn(correlated_plane, _DimensionReducer, [min_dist(2.0)]).

	test(umap_learn_3_local_connectivity_error, error(domain_error(local_connectivity, 4.0))) :-
		umap_projection::learn(correlated_plane, _DimensionReducer, [n_neighbors(3), local_connectivity(4.0)]).

	test(umap_learn_3_metric_error, error(domain_error(option, distance_metric(chebyshev)))) :-
		umap_projection::learn(correlated_plane, _DimensionReducer, [distance_metric(chebyshev)]).

	test(umap_learn_3_initialization_error, error(domain_error(option, initialization(pca)))) :-
		umap_projection::learn(correlated_plane, _DimensionReducer, [initialization(pca)]).

	test(umap_check_dimension_reducer_1_variable, error(instantiation_error)) :-
		umap_projection::check_dimension_reducer(_).

	test(umap_check_dimension_reducer_1_malformed, error(domain_error(dimension_reducer, umap_reducer([], [], [], [], [], [])))) :-
		umap_projection::check_dimension_reducer(umap_reducer([], [], [], [], [], [])).

	test(umap_check_dimension_reducer_1_invalid_encoded_width, error(domain_error(dimension_reducer, _))) :-
		cached_model(mixed, umap_reducer(Encoders, ExampleIds, [FirstRow| TrainingRows], EmbeddingRows, FuzzyGraph, Diagnostics)),
		FirstRow = [_| ShortRow],
		umap_projection::check_dimension_reducer(umap_reducer(Encoders, ExampleIds, [ShortRow| TrainingRows], EmbeddingRows, FuzzyGraph, Diagnostics)).

	test(umap_learn_3_continuous_encoded_feature_count, deterministic(EncodedFeatureCount == 3)) :-
		cached_model(plane, DimensionReducer),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(encoded_feature_count(EncodedFeatureCount), Diagnostics).

	test(umap_projection_transform_zero_memberships, deterministic(SeedBefore == SeedAfter)) :-
		umap_projection::learn(duplicate_outlier_umap_dataset, Reducer, [n_components(1), n_neighbors(4), feature_scaling(false), initialization(random), maximum_iterations(2), negative_sample_rate(1)]),
		get_seed(SeedBefore),
		umap_projection::transform(Reducer, [x-0.0], [component_1-Coordinate]),
		number(Coordinate),
		abs(Coordinate) < 1.0e100,
		get_seed(SeedAfter).

	test(umap_projection_transform_schedule_boundaries, deterministic) :-
		Cases = [
			[]-[],
			[membership(0, 0.0)]-[],
			[membership(0, 0.0), membership(1, 1.0)]-[transform_schedule(1, 1.0, 1.0, 0.2, 0.2)],
			[membership(0, 1.0), membership(1, 0.01), membership(2, 0.001)]-[transform_schedule(0, 1.0, 1.0, 0.2, 0.2), transform_schedule(1, 100.0, 100.0, 20.0, 20.0)]
		],
		forall(member(Memberships-Expected, Cases), check_transform_schedules(Memberships, Expected)).

	test(umap_projection_duplicate_self_neighbors, deterministic) :-
		umap_projection<<exact_nearest_neighbors([[0.0], [0.0], [1.0]], euclidean, 2, Neighbors),
		assertion(Neighbors == [[neighbor(0, 0.0), neighbor(1, 0.0)], [neighbor(1, 0.0), neighbor(0, 0.0)], [neighbor(2, 0.0), neighbor(0, 1.0)]]).

	test(umap_projection_identical_full_neighbors, deterministic) :-
		umap_projection<<exact_nearest_neighbors([[0.0], [0.0], [0.0]], euclidean, 3, Neighbors),
		assertion(Neighbors == [[neighbor(0, 0.0), neighbor(1, 0.0), neighbor(2, 0.0)], [neighbor(1, 0.0), neighbor(0, 0.0), neighbor(2, 0.0)], [neighbor(2, 0.0), neighbor(0, 0.0), neighbor(1, 0.0)]]).

	test(umap_projection_neighbor_count_boundaries, deterministic) :-
		forall(member(Requested-Expected, [2-2, 5-5, 10-5]), check_neighbor_count(Requested, Expected)).

	test(umap_projection_local_connectivity_matrix, deterministic) :-
		Cases = [
			connectivity([1.0, 2.0, 3.0], 0.5, 0.5),
			connectivity([1.0, 2.0, 3.0], 1.0, 1.0),
			connectivity([1.0, 2.0, 3.0], 1.5, 1.5),
			connectivity([0.0, 0.0, 2.0], 0.5, 1.0),
			connectivity([2.0], 1.5, 2.0),
			connectivity([1.0, 2.0, 3.0], 4.0, 3.0),
			connectivity([0.0, 0.0], 0.5, 0.0),
			connectivity([], 1.0, 0.0),
			connectivity([1.0, 2.0], 1.000001, 1.0)
		],
		forall(member(Case, Cases), check_connectivity(Case)).

	test(umap_projection_fractional_connectivity_learning, deterministic) :-
		umap_projection::learn(mixed_umap_dataset, Reducer, [n_neighbors(3), local_connectivity(0.5), initialization(random), maximum_iterations(2)]),
		umap_projection::check_dimension_reducer(Reducer).

	test(umap_projection_symmetric_negative_sampling, deterministic) :-
		get_seed(OriginalSeed),
		randomize(23),
		get_seed(StartSeed),
		umap_projection<<(
			assignable_rows([[0.0], [2.0], [5.0]], ActualHandles),
			index_rows(ActualHandles, ActualIndex),
			bind_edge_schedules([schedule(0, 2, 1.0, 1.0, 0.04, 0.04)], ActualIndex, Bound),
			process_schedules(Bound, 1, 3, 1.0, 1.0, 0.1, 1.0, ActualIndex, ActualSchedules),
			assigned_rows(ActualHandles, Actual)
		),
		set_seed(StartSeed),
		umap_projection<<(
			assignable_rows([[0.0], [2.0], [5.0]], ExpectedHandles),
			index_rows(ExpectedHandles, ExpectedIndex),
			avltree::lookup(0, FirstHandle, ExpectedIndex),
			avltree::lookup(2, LastHandle, ExpectedIndex),
			attractive_update(FirstHandle, LastHandle, 1.0, 1.0, 0.1),
			assigned_rows(ExpectedHandles, AttractionOnly),
			negative_sample_count(1, 0.04, 0.04, Count),
			negative_updates(Count, endpoint(0, FirstHandle), 3, ExpectedIndex, 1.0, 1.0, 0.1, 1.0),
			negative_updates(Count, endpoint(2, LastHandle), 3, ExpectedIndex, 1.0, 1.0, 0.1, 1.0),
			assigned_rows(ExpectedHandles, Expected)
		),
		set_seed(OriginalSeed),
		assertion(Actual == Expected),
		Actual = [_, _, Highest],
		AttractionOnly = [_, _, HighestBeforeRepulsion],
		assertion(Highest \== HighestBeforeRepulsion),
		ActualSchedules = [schedule(endpoint(0, _), endpoint(2, _), _, NextPositive, _, NextNegative)],
		NextPositive =~= 2.0,
		NextNegative > 1.0.

	test(umap_projection_repulsion_boundaries, deterministic) :-
		umap_projection<<(
			assignable_rows([[0.0], [0.0], [2.0]], Handles),
			Handles = [Source, Coincident, Target],
			negative_updates(0, endpoint(0, Source), 3, _, 1.0, 1.0, 1.0, 1.0),
			repulsive_update(Source, Coincident, 1.0, 1.0, 1.0, 1.0),
			repulsive_update(Source, Target, 1.0, 1.0, 1.0, 0.0),
			assigned_rows(Handles, Rows)
		),
		assertion(Rows == [[0.0], [0.0], [2.0]]).

	test(umap_projection_metric_option_unbound, false) :-
		umap_projection::valid_option(distance_metric(_)).

	test(umap_projection_metric_option_values, deterministic) :-
		forall(member(Metric, [euclidean, manhattan, cosine]), umap_projection::valid_option(distance_metric(Metric))),
		assertion(\+ umap_projection::valid_option(distance_metric(chebyshev))).

	test(umap_projection_nonground_reducer, error(instantiation_error)) :-
		umap_projection::check_dimension_reducer(umap_reducer([], [], [], [], [], [_])).

	test(umap_projection_invalid_diagnostics_matrix, deterministic) :-
		cached_model(validation, Reducer),
		Cases = [effective_n_neighbors(0), effective_n_neighbors(-1), effective_n_neighbors(2.5), effective_n_neighbors(5), effective_n_neighbors(2), component_count(4), feature_count(8), encoded_feature_count(8), sample_count(8), graph_edge_count(0), distance_metric(chebyshev), curve_parameters(0.0, 1.0), initialization_used(other), iterations(9), attribute_names([wrong])],
		forall(member(Diagnostic, Cases), check_invalid_diagnostic(Reducer, Diagnostic)).

	test(umap_projection_missing_stored_options, deterministic) :-
		cached_model(validation, umap_reducer(Encoders, Ids, Rows, Embedding, Graph, Diagnostics)),
		once(select(options(Options), Diagnostics, Rest)),
		once(select(transform_seed(_), Options, MissingOptions)),
		assertion(\+ umap_projection::valid_dimension_reducer(umap_reducer(Encoders, Ids, Rows, Embedding, Graph, [options(MissingOptions)| Rest]))),
		once(select(min_dist(_), Options, OtherOptions)),
		assertion(\+ umap_projection::valid_dimension_reducer(umap_reducer(Encoders, Ids, Rows, Embedding, Graph, [options([min_dist(2.0)| OtherOptions])| Rest]))).

	test(umap_projection_duplicate_options_first_occurrence, deterministic) :-
		umap_projection::learn(mixed_umap_dataset, Reducer, [n_neighbors(3), n_neighbors(2), initialization(random), maximum_iterations(2)]),
		umap_projection::check_dimension_reducer(Reducer),
		umap_projection::diagnostics(Reducer, Diagnostics),
		memberchk(effective_n_neighbors(3), Diagnostics).

	test(umap_projection_sparse_fuzzy_merge, deterministic) :-
		Directed = [directed(2, 0, 0.4), directed(0, 1, 0.5), directed(1, 0, 0.25)],
		forall(member(Mix, [0.0, 0.5, 1.0]), check_fuzzy_merge(Directed, Mix)),
		umap_projection<<merge_directed_memberships(0, 3, [], 1.0, Empty),
		assertion(Empty == []).

	test(umap_projection_avl_graph_index, deterministic) :-
		umap_projection<<build_graph_index([edge(0, 1, 0.5), edge(1, 2, 0.25)], 4, Index),
		avltree::as_list(Index, Pairs),
		assertion(Pairs == [0-vertex(0.5, [1-0.5]), 1-vertex(0.75, [0-0.5, 2-0.25]), 2-vertex(0.25, [1-0.25]), 3-vertex(0.0, [])]),
		umap_projection<<graph_connected([edge(0, 1, 0.5), edge(1, 2, 0.25)], 3),
		assertion(\+ (umap_projection<<graph_connected([edge(0, 1, 0.5), edge(1, 2, 0.25)], 4))),
		umap_projection<<graph_connected([], 1).

	test(umap_projection_normalized_adjacency_reference, deterministic) :-
		umap_projection<<normalized_adjacency(0, 3, [edge(0, 1, 0.5), edge(1, 2, 0.25)], [0.5, 0.75, 0.25], Matrix),
		First is sqrt(2.0 / 3.0),
		Second is sqrt(1.0 / 3.0),
		check_numeric_matrix(Matrix, [[0.0, First, 0.0], [First, 0.0, Second], [0.0, Second, 0.0]]).

	test(umap_projection_exact_cross_entropy_reference, deterministic) :-
		forall(member(Graph, [[], [edge(0, 2, 0.5)], [edge(0, 1, 1.0), edge(0, 2, 0.5), edge(1, 2, 0.25)]]), check_entropy_reference(Graph)),
		umap_projection<<embedding_cross_entropy([], [[0.0], [0.0]], 1.0, 1.0, Coincident),
		Expected is -log(1.0 - (1.0 - 1.0e-12)),
		Coincident =~= Expected.

	test(umap_projection_avl_row_handle_sharing, deterministic) :-
		umap_projection<<(
			assignable_rows([[0.0], [2.0]], Handles),
			index_rows(Handles, Index),
			bind_edge_schedules([schedule(0, 1, 1.0, 1.0, 0.2, 0.2)], Index, [schedule(endpoint(0, Source), endpoint(1, Target), _, _, _, _)]),
			attractive_update(Source, Target, 1.0, 1.0, 0.1),
			avltree::lookup(0, IndexedSource, Index),
			IndexedSource => Changed,
			assigned_rows(Handles, Rows)
		),
		Rows = [Changed, OtherRow],
		OtherRow =~= [1.92],
		Changed =~= [0.08].

	test(umap_projection_transform_preserves_embedding, deterministic) :-
		cached_model(validation, Reducer),
		copy_term(Reducer, Original),
		umap_projection::transform(Reducer, [age-35.0, channel-retail], _),
		assertion(Reducer == Original).

	test(umap_projection_partial_spectral_bipartite, deterministic) :-
		Weight is 1.0 / sqrt(2.0),
		Matrix = [[0.0, Weight, 0.0], [Weight, 0.0, Weight], [0.0, Weight, 0.0]],
		umap_projection<<spectral_components(Matrix, [1.0, 2.0, 1.0], 2, [First, Second]),
		linear_algebra::matrix_vector_product(Matrix, First, FirstProduct),
		numberlist::scalar_product(First, FirstProduct, FirstValue),
		linear_algebra::matrix_vector_product(Matrix, Second, SecondProduct),
		numberlist::scalar_product(Second, SecondProduct, SecondValue),
		FirstValue =~= 0.0,
		SecondValue =~= -1.0.

	test(umap_projection_partial_spectral_repeated_eigenvalues, deterministic) :-
		Matrix = [[0.0, 0.5, 0.5], [0.5, 0.0, 0.5], [0.5, 0.5, 0.0]],
		umap_projection<<spectral_components(Matrix, [1.0, 1.0, 1.0], 2, Components),
		forall(member(Vector, Components), check_spectral_value(Matrix, Vector, -0.5)).

	test(umap_projection_invalid_spectral_residual, false) :-
		umap_projection<<valid_spectral_components([[0.0, 1.0], [1.0, 0.0]], [0.0, 1.0], [[1.0, 0.0]], 1.0e-8).

	test(umap_projection_disconnected_spectral_fallback, deterministic) :-
		umap_projection::learn(duplicate_outlier_umap_dataset, Reducer, [n_components(1), n_neighbors(2), set_op_mix_ratio(0.0), maximum_iterations(2)]),
		umap_projection::check_dimension_reducer(Reducer),
		umap_projection::diagnostics(Reducer, Diagnostics),
		memberchk(initialization_used(random_fallback), Diagnostics).

	test(umap_projection_exact_neighbors_full_sort_reference, deterministic) :-
		Rows = [[0.0, 0.0], [1.0, 0.0], [0.0, 1.0], [1.0, 0.0], [-1.0, -1.0]],
		forall(
			( 	member(Metric, [euclidean, manhattan, cosine]),
				member(Count, [2, 3, 5])
			),
			check_neighbor_reference(Rows, Metric, Count)
		),
		umap_projection<<nearest_neighbors(Rows, [0.0, 0.0], euclidean, 0, Empty),
		assertion(Empty == []).

	test(umap_projection_large_neighbors_full_sort_reference, all(check_neighbor_reference(Rows, Metric, Count))) :-
		neighbor_fixture_rows(0, 33, Rows),
		member(Metric, [euclidean, manhattan, cosine]),
		member(Count, [2, 3, 15, 33]).

	test(umap_projection_bounded_heap_changes, deterministic) :-
		binary_heap(>)::new(Initial),
		umap_projection<<bounded_heap_insert(0, key(2.0, 2), Initial, Empty, false),
		assertion(Empty == Initial),
		umap_projection<<bounded_heap_insert(1, key(2.0, 2), Empty, First, true),
		umap_projection<<bounded_heap_insert(1, key(2.0, 1), First, Replaced, true),
		umap_projection<<bounded_heap_insert(1, key(2.0, 1), Replaced, Equal, false),
		assertion(Equal == Replaced),
		umap_projection<<bounded_heap_insert(1, key(3.0, 0), Equal, Worse, false),
		assertion(Worse == Equal),
		umap_projection<<bounded_heap_insert(1, key(1.0, 3), Worse, Better, true),
		umap_projection<<heap_neighbors(Better, Neighbors),
		assertion(Neighbors == [neighbor(3, 1.0)]).

	test(umap_projection_cached_curve_boundaries, deterministic) :-
		forall(member(MinDist-Spread, [0.0-1.0, 0.1-1.0, 1.0-1.0, 0.2-2.0]), check_curve_fit(MinDist, Spread)),
		umap_projection<<fit_curve_iterations(25, [], 0.0, 0.0, LogA, LogB),
		assertion(LogA =~= 0.0),
		assertion(LogB =~= 0.0),
		umap_projection<<curve_value_and_jacobian(0.0, 0.0, 1.0, 1.0, Value, JacobianA, JacobianB),
		assertion(Value =~= 1.0),
		assertion(JacobianA =~= 0.0),
		assertion(JacobianB =~= 0.0).

	test(umap_projection_observations_binding_order, deterministic) :-
		umap_projection<<build_attribute_observations([x, y], [1-[y-2.0, x-1.0], 2-[x-_, y-4.0], 3-[y-_, x-3.0]], Observations),
		avltree::as_list(Observations, Pairs),
		assertion(Pairs == [x-[1.0, 3.0], y-[2.0, 4.0]]).

	test(umap_projection_mean_empty_and_example_hook, deterministic) :-
		umap_projection<<arithmetic_mean_or_zero([], Mean),
		assertion(Mean =~= 0.0),
		umap_projection<<example_attribute_values(id-[x-1.0], Bindings),
		assertion(Bindings == [x-1.0]).

	test(umap_projection_print_reducer, deterministic) :-
		^^suppress_text_output,
		cached_model(plane, Reducer),
		umap_projection::print_dimension_reducer(Reducer).

	test(umap_projection_unversioned_representation, deterministic) :-
		cached_model(validation, Reducer),
		Reducer = umap_reducer(_Encoders, _Ids, _Rows, _Embedding, _Graph, Diagnostics),
		assertion(\+ member(representation_version(_), Diagnostics)),
		umap_projection::valid_dimension_reducer(Reducer).

	test(umap_projection_exported_representation, deterministic) :-
		cached_model(validation, Reducer),
		umap_projection::export_to_clauses(mixed_umap_dataset, Reducer, saved_umap, [saved_umap(Exported)]),
		assertion(Exported == Reducer),
		umap_projection::check_dimension_reducer(Exported).

	test(umap_projection_invalid_rows_and_graph, deterministic) :-
		cached_model(validation, umap_reducer(Encoders, Ids, Rows, Embedding, _Graph, Diagnostics)),
		forall(member(Graph, [[edge(0, 0, 1.0)], [edge(0, 4, 1.0)], [edge(0, 1, 0.0)], [edge(0, 1, 1.1)], [edge(1, 2, 0.5), edge(0, 1, 0.5)]]), assertion(\+ umap_projection::valid_dimension_reducer(umap_reducer(Encoders, Ids, Rows, Embedding, Graph, Diagnostics)))).

	test(umap_projection_reducer_identifier_set, deterministic) :-
		cached_model(validation, umap_reducer(Encoders, _Ids, Rows, Embedding, Graph, Diagnostics)),
		umap_projection::check_dimension_reducer(umap_reducer(Encoders, [1, 1.0, sample(a), sample(b)], Rows, Embedding, Graph, Diagnostics)),
		assertion(\+ umap_projection::valid_dimension_reducer(umap_reducer(Encoders, [sample(a), 1, sample(b), sample(a)], Rows, Embedding, Graph, Diagnostics))).

	test(umap_projection_binding_error_precedence, all(check_binding_error(Reducer, Bindings, Expected))) :-
		cached_model(mixed, Reducer),
		Cases = [
			[age-20.0, age-30.0, unknown-0]-domain_error(attribute_occurrences, age),
			[channel-retail, unknown-0]-existence_error(attribute, age),
			[age-invalid, channel-retail, unknown-0]-domain_error(declared_attribute, unknown),
			[age-invalid, channel-online, channel-retail]-domain_error(attribute_occurrences, channel),
			[channel-retail, age-invalid]-type_error(number, invalid),
			[age-35.0, channel-phone]-domain_error(attribute_value(channel, [online, retail]), phone)
		],
		member(Bindings-Expected, Cases).

	test(umap_projection_binding_order_and_missing_values, deterministic) :-
		cached_model(mixed, umap_reducer(Encoders, _Ids, _Rows, _Embedding, _Graph, _Diagnostics)),
		umap_projection<<encode_instance(Encoders, [channel-MissingChannel, age-MissingAge], Features),
		assertion(Features == [0.0, 0.0, 0.0, 1.0]),
		assertion(var(MissingChannel)),
		assertion(var(MissingAge)),
		umap_projection<<encode_instance(Encoders, [channel-retail, age-35.0], Observed),
		assertion(Observed == [0.0, 0.0, 1.0, 0.0]).

	test(umap_projection_binding_variable_key, deterministic(Attribute == age)) :-
		cached_model(mixed, umap_reducer(Encoders, _Ids, _Rows, _Embedding, _Graph, _Diagnostics)),
		umap_projection<<encode_instance(Encoders, [Attribute-35.0, channel-retail], _Features).

	test(umap_projection_binding_open_tail, true(Tail == [])) :-
		cached_model(mixed, umap_reducer(Encoders, _Ids, _Rows, _Embedding, _Graph, _Diagnostics)),
		umap_projection<<encode_instance(Encoders, [age-35.0, channel-retail| Tail], _Features).

	test(umap_projection_binding_improper_list, false) :-
		cached_model(mixed, Reducer),
		umap_projection::transform(Reducer, [age-35.0, channel-retail| invalid], _Reduced).

	test(umap_projection_wide_reordered_training, deterministic(length(Reduced, 2))) :-
		umap_projection::learn(wide_umap_dataset, Reducer, [feature_scaling(false), n_neighbors(3), initialization(random), maximum_iterations(2), negative_sample_rate(1)]),
		umap_projection::check_dimension_reducer(Reducer),
		Reducer = umap_reducer(_Encoders, _Ids, [First, Second, Third, Fourth], _Embedding, _Graph, _Diagnostics),
		forall(member(Scale-Row, [-1.5-First, -0.5-Second, 0.5-Third, 1.5-Fourth]), check_scaled_row(Row, 16, Scale)),
		wide_umap_dataset::example(2, Instance),
		umap_projection::transform(Reducer, Instance, Reduced).

	% auxiliary predicates

	check_scaled_row(Row, Width, Scale) :-
		length(Row, Width),
		findall(
			Expected,
			( 	nth1(Index, Row, _Value),
				Expected is Index * Scale
			),
			ExpectedRow
		),
		assertion(Row =~= ExpectedRow).

	check_binding_error(Reducer, Bindings, Expected) :-
		catch(
			( 	umap_projection::transform(Reducer, Bindings, _Reduced),
				Outcome = success
			),
			error(Formal, _Context),
			Outcome = error(Formal)
		),
		assertion(Outcome == error(Expected)).

	neighbor_fixture_rows(Index, Count, []) :-
		Index >= Count,
		!.
	neighbor_fixture_rows(Index, Count, [[First, Second]| Rows]) :-
		First is float(Index mod 5),
		Second is float(Index mod 3),
		Next is Index + 1,
		neighbor_fixture_rows(Next, Count, Rows).

	cached_model(Name, Reducer) :-
		once(cached_reducer(Name, Reducer)).

	check_curve_fit(MinDist, Spread) :-
		umap_projection<<fit_curve_parameters([min_dist(MinDist), spread(Spread)], CurveA, CurveB),
		CurveA > 0.0,
		CurveB > 0.0,
		Distance is 3.0 * Spread / 2.0,
		LogDistance is log(Distance),
		umap_projection<<curve_value_and_jacobian(Distance, LogDistance, CurveA, CurveB, Value, JacobianA, JacobianB),
		Expected is 1.0 / (1.0 + CurveA * Distance ** (2.0 * CurveB)),
		Value =~= Expected,
		ExpectedA is -Expected * (1.0 - Expected),
		ExpectedB is ExpectedA * 2.0 * CurveB * log(Distance),
		JacobianA =~= ExpectedA,
		JacobianB =~= ExpectedB.

	check_neighbor_reference(Rows, Metric, Count) :-
		umap_projection<<exact_nearest_neighbors(Rows, Metric, Count, Actual),
		reference_training_neighbors(Rows, Rows, Metric, Count, 0, Expected),
		assertion(Actual == Expected),
		umap_projection<<nearest_neighbors(Rows, [0.0, 0.0], Metric, Count, Transformed),
		reference_distances(Rows, [0.0, 0.0], Metric, 0, -1, Distances),
		keysort(Distances, Sorted),
		reference_prefix(Count, Sorted, ExpectedTransform),
		assertion(Transformed == ExpectedTransform).

	reference_training_neighbors([], _All, _Metric, _Count, _Index, []).
	reference_training_neighbors([Row| Rows], All, Metric, Count, Index, [[neighbor(Index, 0.0)| Neighbors]| Rest]) :-
		reference_distances(All, Row, Metric, 0, Index, Distances),
		keysort(Distances, Sorted),
		OtherCount is Count - 1,
		reference_prefix(OtherCount, Sorted, Neighbors),
		Next is Index + 1,
		reference_training_neighbors(Rows, All, Metric, Count, Next, Rest).

	reference_distances([], _Row, _Metric, _Index, _Excluded, []).
	reference_distances([Other| Rows], Row, Metric, Index, Excluded, Distances) :-
		( 	Index =:= Excluded ->
			Distances = Rest
		;	umap_projection<<metric_distance(Metric, Row, Other, Distance),
			Distances = [Distance-Index| Rest]
		),
		Next is Index + 1,
		reference_distances(Rows, Row, Metric, Next, Excluded, Rest).

	reference_prefix(0, _Pairs, []) :-
		!.
	reference_prefix(Count, [Distance-Index| Pairs], [neighbor(Index, Distance)| Neighbors]) :-
		Next is Count - 1,
		reference_prefix(Next, Pairs, Neighbors).

	check_spectral_value(Matrix, Vector, Expected) :-
		linear_algebra::matrix_vector_product(Matrix, Vector, Product),
		numberlist::scalar_product(Vector, Product, Value),
		Value =~= Expected.

	check_numeric_matrix([], []).
	check_numeric_matrix([Row| Rows], [Expected| Expecteds]) :-
		Row =~= Expected,
		check_numeric_matrix(Rows, Expecteds).

	check_entropy_reference(Graph) :-
		umap_projection<<embedding_cross_entropy(Graph, [[0.0], [1.0], [3.0]], 1.0, 1.0, Actual),
		reference_entropy_pair(Graph, 0, 1, 1.0, First),
		reference_entropy_pair(Graph, 0, 2, 9.0, Second),
		reference_entropy_pair(Graph, 1, 2, 4.0, Third),
		Expected is First + Second + Third,
		Actual =~= Expected.

	reference_entropy_pair(Graph, Source, Target, SquaredDistance, Contribution) :-
		( 	member(edge(Source, Target, Weight), Graph) ->
			true
		;	Weight = 0.0
		),
		Probability is 1.0 / (1.0 + SquaredDistance),
		Contribution is -(Weight * log(Probability) + (1.0 - Weight) * log(1.0 - Probability)).

	check_fuzzy_merge(Directed, Mix) :-
		umap_projection<<merge_directed_memberships(0, 3, Directed, Mix, Graph),
		FirstWeight is Mix * 0.625 + (1.0 - Mix) * 0.125,
		SecondWeight is Mix * 0.4,
		( 	SecondWeight > 0.0 ->
			Graph = [edge(0, 1, ActualFirst), edge(0, 2, ActualSecond)],
			ActualSecond =~= SecondWeight
		;	Graph = [edge(0, 1, ActualFirst)]
		),
		ActualFirst =~= FirstWeight.

	check_invalid_diagnostic(umap_reducer(Encoders, Ids, Rows, Embedding, Graph, Diagnostics), Diagnostic) :-
		functor(Diagnostic, Name, Arity),
		functor(Old, Name, Arity),
		select(Old, Diagnostics, Rest),
		assertion(\+ umap_projection::valid_dimension_reducer(umap_reducer(Encoders, Ids, Rows, Embedding, Graph, [Diagnostic| Rest]))).

	check_connectivity(connectivity(Distances, Connectivity, Expected)) :-
		umap_projection<<local_connectivity_distance(Distances, Connectivity, Rho),
		Rho =~= Expected.

	check_neighbor_count(Requested, Expected) :-
		umap_projection::learn(duplicate_outlier_umap_dataset, Reducer, [n_components(1), n_neighbors(Requested), initialization(random), maximum_iterations(2)]),
		umap_projection::diagnostics(Reducer, Diagnostics),
		memberchk(effective_n_neighbors(Count), Diagnostics),
		assertion(Count == Expected).

	check_transform_schedules(Memberships, Expected) :-
		umap_projection<<transform_schedules(Memberships, 100, 5, Schedules),
		assertion(Schedules == Expected).

	check_mixed_metric(Metric) :-
		umap_projection::learn(mixed_umap_dataset, DimensionReducer, [feature_scaling(false), n_neighbors(2), distance_metric(Metric), initialization(random), maximum_iterations(5)]),
		umap_projection::check_dimension_reducer(DimensionReducer).

:- end_object.
