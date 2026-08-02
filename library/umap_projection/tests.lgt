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


:- object(missing_values_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-2.0]).
	example(2, [x-_, y-4.0]).
	example(3, [x-3.0, y-_]).
	example(4, [x-5.0, y-8.0]).

:- end_object.


:- object(cosine_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(zero, [x-0.0, y-0.0]).
	example(x, [x-1.0, y-0.0]).
	example(y, [x-0.0, y-1.0]).
	example(xy, [x-1.0, y-1.0]).

:- end_object.


:- object(mixed_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(age, continuous).
	attribute_values(channel, [online, retail]).

	example(1, [age-20.0, channel-online]).
	example(2, [age-30.0, channel-retail]).
	example(3, [age-40.0, channel-_]).
	example(4, [age-50.0, channel-online]).

:- end_object.


:- object(categorical_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(color, [red, green, blue]).

	example(1, [color-red]).
	example(2, [color-green]).
	example(3, [color-blue]).
	example(4, [color-red]).

:- end_object.


:- object(invalid_categorical_value_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).

	example(1, [channel-online]).
	example(2, [channel-phone]).
	example(3, [channel-retail]).

:- end_object.


:- object(empty_categorical_domain_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, []).

	example(1, [channel-online]).
	example(2, [channel-retail]).
	example(3, [channel-online]).

:- end_object.


:- object(duplicate_categorical_domain_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail, online]).

	example(1, [channel-online]).
	example(2, [channel-retail]).
	example(3, [channel-online]).

:- end_object.


:- object(variable_categorical_domain_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, _]).

	example(1, [channel-online]).
	example(2, [channel-retail]).
	example(3, [channel-online]).

:- end_object.


:- object(improper_categorical_domain_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online| retail]).

	example(1, [channel-online]).
	example(2, [channel-retail]).
	example(3, [channel-online]).

:- end_object.


:- object(all_missing_categorical_umap_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).

	example(1, [channel-_]).
	example(2, [channel-_]).
	example(3, [channel-_]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-02,
		comment is 'Unit tests for the "umap_projection" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		get_seed/1
	]).

	cover(umap_projection).

	cleanup :-
		^^clean_file('test_output.pl').

	test(umap_learn_3_structure, deterministic(functor(DimensionReducer, umap_reducer, 6))) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), maximum_iterations(5)]).

	test(umap_learn_3_effective_neighbors, deterministic(NeighborCount == 7)) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [maximum_iterations(5)]),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(effective_n_neighbors(NeighborCount), Diagnostics).

	test(umap_learn_3_metric_manhattan, deterministic(Metric == manhattan)) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), distance_metric(manhattan), maximum_iterations(5)]),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(distance_metric(Metric), Diagnostics).

	test(umap_learn_3_cosine_zero_rows, deterministic) :-
		umap_projection::learn(cosine_umap_dataset, DimensionReducer, [n_neighbors(2), distance_metric(cosine), feature_scaling(false), maximum_iterations(5)]),
		umap_projection::check_dimension_reducer(DimensionReducer).

	test(umap_learn_3_missing_values, deterministic([XMean, MissingX, MissingY] == [3.0, 0.0, 0.0])) :-
		umap_projection::learn(missing_values_umap_dataset, umap_reducer(Encoders, _ExampleIds, TrainingRows, _EmbeddingRows, _FuzzyGraph, _Diagnostics), [n_neighbors(2), maximum_iterations(5)]),
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
		umap_projection::learn(mixed_umap_dataset, DimensionReducer, [feature_scaling(false), n_neighbors(2), maximum_iterations(5)]),
		DimensionReducer = umap_reducer(Encoders, _ExampleIds, TrainingRows, _EmbeddingRows, _FuzzyGraph, _Diagnostics),
		umap_projection::check_dimension_reducer(DimensionReducer).

	test(umap_learn_3_categorical_reducer, deterministic((EncodedFeatureCount == 4, ComponentCount == 2))) :-
		umap_projection::learn(categorical_umap_dataset, DimensionReducer, [n_components(2), n_neighbors(2), maximum_iterations(5)]),
		umap_projection::check_dimension_reducer(DimensionReducer),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(encoded_feature_count(EncodedFeatureCount), Diagnostics),
		memberchk(component_count(ComponentCount), Diagnostics).

	test(umap_learn_3_mixed_feature_counts, deterministic((FeatureCount == 2, EncodedFeatureCount == 4))) :-
		umap_projection::learn(mixed_umap_dataset, DimensionReducer, [feature_scaling(false), n_neighbors(2), maximum_iterations(5)]),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(feature_count(FeatureCount), Diagnostics),
		memberchk(encoded_feature_count(EncodedFeatureCount), Diagnostics).

	test(umap_learn_3_mixed_metrics, deterministic) :-
		check_mixed_metric(euclidean),
		check_mixed_metric(manhattan),
		check_mixed_metric(cosine).

	test(umap_transform_3_categorical, deterministic) :-
		umap_projection::learn(mixed_umap_dataset, DimensionReducer, [feature_scaling(false), n_neighbors(2), maximum_iterations(5)]),
		umap_projection::transform(DimensionReducer, [age-35.0, channel-retail], ReducedInstance),
		length(ReducedInstance, 2).

	test(umap_transform_3_missing_categorical, deterministic) :-
		umap_projection::learn(mixed_umap_dataset, DimensionReducer, [feature_scaling(false), n_neighbors(2), maximum_iterations(5)]),
		umap_projection::transform(DimensionReducer, [age-35.0, channel-_], ReducedInstance),
		length(ReducedInstance, 2).

	test(umap_transform_3_invalid_categorical_value, error(domain_error(attribute_value(channel, [online, retail]), phone))) :-
		umap_projection::learn(mixed_umap_dataset, DimensionReducer, [feature_scaling(false), n_neighbors(2), maximum_iterations(5)]),
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
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), maximum_iterations(5)]),
		umap_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance),
		length(ReducedInstance, 2),
		memberchk(component_1-Component1, ReducedInstance),
		memberchk(component_2-Component2, ReducedInstance),
		number(Component1),
		number(Component2).

	test(umap_transform_3_seed_reproducibility, deterministic(ReducedInstance1 == ReducedInstance2)) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), maximum_iterations(5), transform_seed(19)]),
		Instance = [x-2.75, y-5.5, z-8.25],
		umap_projection::transform(DimensionReducer, Instance, ReducedInstance1),
		umap_projection::transform(DimensionReducer, Instance, ReducedInstance2).

	test(umap_transform_3_restores_random_state, deterministic(SeedBefore == SeedAfter)) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), maximum_iterations(5)]),
		get_seed(SeedBefore),
		umap_projection::transform(DimensionReducer, [x-2.75, y-5.5, z-8.25], _ReducedInstance),
		get_seed(SeedAfter).

	test(umap_learn_3_fuzzy_graph, deterministic) :-
		umap_projection::learn(correlated_plane, umap_reducer(_Encoders, _ExampleIds, _TrainingRows, _EmbeddingRows, FuzzyGraph, Diagnostics), [n_neighbors(3), maximum_iterations(5)]),
		FuzzyGraph = [edge(FirstSource, FirstTarget, FirstWeight)| _],
		FirstSource < FirstTarget,
		FirstWeight > 0.0,
		FirstWeight =< 1.0,
		memberchk(graph_edge_count(EdgeCount), Diagnostics),
		length(FuzzyGraph, EdgeCount).

	test(umap_learn_3_curve_parameters, deterministic((abs(CurveA - 1.57694) < 0.05, abs(CurveB - 0.89506) < 0.05))) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), maximum_iterations(5)]),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(curve_parameters(CurveA, CurveB), Diagnostics).

	test(umap_learn_3_spectral_initialization, deterministic(InitializationUsed == spectral)) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), maximum_iterations(5)]),
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
		Options = [n_neighbors(3), maximum_iterations(5), random_seed(27)],
		umap_projection::learn(correlated_plane, umap_reducer(_Encoders1, _ExampleIds1, _TrainingRows1, Embedding1, _FuzzyGraph1, _Diagnostics1), Options),
		umap_projection::learn(correlated_plane, umap_reducer(_Encoders2, _ExampleIds2, _TrainingRows2, Embedding2, _FuzzyGraph2, _Diagnostics2), Options).

	test(umap_learn_3_optimization_diagnostics, deterministic((Iterations == 5, number(InitialCrossEntropy), number(FinalCrossEntropy)))) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), maximum_iterations(5)]),
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
		umap_projection::learn(mixed_umap_dataset, umap_reducer(Encoders, ExampleIds, [FirstRow| TrainingRows], EmbeddingRows, FuzzyGraph, Diagnostics), [feature_scaling(false), n_neighbors(2), maximum_iterations(5)]),
		FirstRow = [_| ShortRow],
		umap_projection::check_dimension_reducer(umap_reducer(Encoders, ExampleIds, [ShortRow| TrainingRows], EmbeddingRows, FuzzyGraph, Diagnostics)).

	test(umap_learn_3_continuous_encoded_feature_count, deterministic(EncodedFeatureCount == 3)) :-
		umap_projection::learn(correlated_plane, DimensionReducer, [n_neighbors(3), maximum_iterations(5)]),
		umap_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(encoded_feature_count(EncodedFeatureCount), Diagnostics).

	check_mixed_metric(Metric) :-
		umap_projection::learn(mixed_umap_dataset, DimensionReducer, [feature_scaling(false), n_neighbors(2), distance_metric(Metric), maximum_iterations(5)]),
		umap_projection::check_dimension_reducer(DimensionReducer).

:- end_object.
