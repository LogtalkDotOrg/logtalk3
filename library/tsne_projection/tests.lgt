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


:- object(duplicate_id_tsne_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(a, [x-0.0, y-0.0]).
	example(a, [x-1.0, y-1.0]).

:- end_object.


:- object(non_continuous_tsne_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(score, continuous).

	example(1, [channel-online, score-1.0]).
	example(2, [channel-retail, score-2.0]).

:- end_object.


:- object(missing_values_tsne_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-2.0]).
	example(2, [x-_, y-4.0]).
	example(3, [x-3.0, y-_]).
	example(4, [x-5.0, y-8.0]).
	example(5, [x-7.0, y-10.0]).
	example(6, [x-9.0, y-12.0]).

:- end_object.


:- object(all_missing_attribute_tsne_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-_, y-1.0]).
	example(2, [x-_, y-2.0]).
	example(3, [x-_, y-3.0]).
	example(4, [x-_, y-4.0]).
	example(5, [x-_, y-5.0]).
	example(6, [x-_, y-6.0]).

:- end_object.


:- object(nonnumeric_observed_tsne_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-invalid, y-1.0]).
	example(2, [x-2.0, y-2.0]).

:- end_object.


:- object(omitted_attribute_tsne_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0]).
	example(2, [x-2.0, y-2.0]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-01,
		comment is 'Unit tests for the "tsne_projection" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		get_seed/1
	]).

	cover(tsne_projection).

	cleanup :-
		^^clean_file('test_output.pl').

	quick_options([
		perplexity(2.0),
		learning_rate(20.0),
		maximum_iterations(100),
		early_exaggeration_iterations(20),
		tolerance(1.0e-6)
	]).

	test(tsne_learn_2_correlated_plane, deterministic(ground(DimensionReducer))) :-
		tsne_projection::learn(correlated_plane, DimensionReducer).

	test(tsne_learn_3_structure, deterministic(functor(DimensionReducer, tsne_reducer, 5))) :-
		quick_options(Options),
		tsne_projection::learn(correlated_plane, DimensionReducer, Options).

	test(tsne_learn_3_dimensions, deterministic([ExampleCount, TrainingCount, EmbeddingCount, ComponentCount] == [8, 8, 8, 2])) :-
		quick_options(Options),
		tsne_projection::learn(correlated_plane, tsne_reducer(_Encoders, ExampleIds, TrainingRows, EmbeddingRows, _Diagnostics), Options),
		length(ExampleIds, ExampleCount),
		length(TrainingRows, TrainingCount),
		length(EmbeddingRows, EmbeddingCount),
		EmbeddingRows = [FirstEmbeddingRow| _],
		length(FirstEmbeddingRow, ComponentCount).

	test(tsne_learn_3_same_seed_reproducible, deterministic(EmbeddingRows1 == EmbeddingRows2)) :-
		quick_options(Options),
		tsne_projection::learn(correlated_plane, tsne_reducer(_Encoders1, _ExampleIds1, _TrainingRows1, EmbeddingRows1, _Diagnostics1), Options),
		tsne_projection::learn(correlated_plane, tsne_reducer(_Encoders2, _ExampleIds2, _TrainingRows2, EmbeddingRows2, _Diagnostics2), Options).

	test(tsne_learn_3_restores_random_state, deterministic(SeedBefore == SeedAfter)) :-
		get_seed(SeedBefore),
		quick_options(Options),
		tsne_projection::learn(correlated_plane, _DimensionReducer, Options),
		get_seed(SeedAfter).

	test(tsne_check_dimension_reducer_1, deterministic) :-
		quick_options(Options),
		tsne_projection::learn(correlated_plane, DimensionReducer, Options),
		tsne_projection::check_dimension_reducer(DimensionReducer).

	test(tsne_valid_dimension_reducer_1_malformed_embedding, fail) :-
		quick_options(Options),
		tsne_projection::learn(correlated_plane, tsne_reducer(Encoders, ExampleIds, TrainingRows, [_FirstRow| EmbeddingRows], Diagnostics), Options),
		tsne_projection::valid_dimension_reducer(tsne_reducer(Encoders, ExampleIds, TrainingRows, EmbeddingRows, Diagnostics)).

	test(tsne_diagnostics_2_kl_improves, deterministic(FinalKL < InitialKL)) :-
		tsne_projection::learn(correlated_plane, DimensionReducer, [perplexity(2.0), learning_rate(20.0), maximum_iterations(300), early_exaggeration_iterations(50), tolerance(1.0e-6)]),
		tsne_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(initial_kl_divergence(InitialKL), Diagnostics),
		memberchk(final_kl_divergence(FinalKL), Diagnostics).

	test(tsne_diagnostics_2_metadata, deterministic([Model, SampleCount, ComponentCount, Preprocessing] == [tsne_projection, 8, 2, [center(true), feature_scaling(true), missing_values(mean_imputation)]])) :-
		quick_options(Options),
		tsne_projection::learn(correlated_plane, DimensionReducer, Options),
		tsne_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(sample_count(SampleCount), Diagnostics),
		memberchk(component_count(ComponentCount), Diagnostics),
		memberchk(preprocessing(Preprocessing), Diagnostics).

	test(tsne_transform_3_component_names, deterministic) :-
		quick_options(Options),
		tsne_projection::learn(correlated_plane, DimensionReducer, Options),
		tsne_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance),
		assertion(length(ReducedInstance, 2)),
		memberchk(component_1-Component1, ReducedInstance),
		memberchk(component_2-Component2, ReducedInstance),
		assertion(number(Component1)),
		assertion(number(Component2)).

	test(tsne_learn_3_missing_values_mean_imputation, deterministic([XMean, MissingX, MissingY] == [5.0, 0.0, 0.0])) :-
		quick_options(Options),
		tsne_projection::learn(missing_values_tsne_dataset, tsne_reducer(Encoders, _ExampleIds, TrainingRows, _EmbeddingRows, _Diagnostics), Options),
		memberchk(continuous(x, XMean, _XScale), Encoders),
		TrainingRows = [_FirstRow, [MissingX, _SecondY], [_ThirdX, MissingY]| _].

	test(tsne_learn_3_missing_values_ground_reducer, deterministic(ground(DimensionReducer))) :-
		quick_options(Options),
		tsne_projection::learn(missing_values_tsne_dataset, DimensionReducer, Options),
		tsne_projection::check_dimension_reducer(DimensionReducer).

	test(tsne_transform_3_missing_value, deterministic) :-
		quick_options(Options),
		tsne_projection::learn(missing_values_tsne_dataset, DimensionReducer, Options),
		tsne_projection::transform(DimensionReducer, [x-_, y-6.0], ReducedInstance),
		memberchk(component_1-Component1, ReducedInstance),
		memberchk(component_2-Component2, ReducedInstance),
		assertion(number(Component1)),
		assertion(number(Component2)).

	test(tsne_transform_3_omitted_attribute, error(existence_error(attribute, y))) :-
		quick_options(Options),
		tsne_projection::learn(missing_values_tsne_dataset, DimensionReducer, Options),
		tsne_projection::transform(DimensionReducer, [x-1.0], _ReducedInstance).

	test(tsne_transform_3_duplicate_attribute, error(domain_error(attribute_occurrences, x))) :-
		quick_options(Options),
		tsne_projection::learn(missing_values_tsne_dataset, DimensionReducer, Options),
		tsne_projection::transform(DimensionReducer, [x-1.0, x-2.0, y-3.0], _ReducedInstance).

	test(tsne_transform_3_undeclared_attribute, error(domain_error(declared_attribute, z))) :-
		quick_options(Options),
		tsne_projection::learn(missing_values_tsne_dataset, DimensionReducer, Options),
		tsne_projection::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0], _ReducedInstance).

	test(tsne_transform_3_nonnumeric_observed_value, error(type_error(number, invalid))) :-
		quick_options(Options),
		tsne_projection::learn(missing_values_tsne_dataset, DimensionReducer, Options),
		tsne_projection::transform(DimensionReducer, [x-invalid, y-2.0], _ReducedInstance).

	test(tsne_transform_3_deterministic, deterministic(ReducedInstance1 == ReducedInstance2)) :-
		quick_options(Options),
		tsne_projection::learn(correlated_plane, DimensionReducer, Options),
		tsne_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance1),
		tsne_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance2).

	test(tsne_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		quick_options(Options),
		tsne_projection::learn(correlated_plane, DimensionReducer, Options),
		tsne_projection::export_to_clauses(correlated_plane, DimensionReducer, reduced, [Clause]).

	test(tsne_export_to_file_4, deterministic) :-
		^^file_path('test_output.pl', File),
		quick_options(Options),
		tsne_projection::learn(correlated_plane, DimensionReducer, Options),
		tsne_projection::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File, [reload(always)]),
		{reducer(Reducer)},
		tsne_projection::check_dimension_reducer(Reducer).

	test(tsne_export_to_file_4_missing_values, deterministic) :-
		^^file_path('test_output.pl', File),
		quick_options(Options),
		tsne_projection::learn(missing_values_tsne_dataset, DimensionReducer, Options),
		tsne_projection::export_to_file(missing_values_tsne_dataset, DimensionReducer, reducer, File),
		logtalk_load(File, [reload(always)]),
		{reducer(Reducer)},
		tsne_projection::check_dimension_reducer(Reducer),
		tsne_projection::transform(Reducer, [x-_, y-6.0], ReducedInstance),
		assertion(ground(ReducedInstance)).

	test(tsne_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		quick_options(Options),
		tsne_projection::learn(correlated_plane, DimensionReducer, Options),
		tsne_projection::print_dimension_reducer(DimensionReducer).

	test(tsne_learn_3_perplexity_too_large, error(domain_error(perplexity, 8.0-8))) :-
		tsne_projection::learn(correlated_plane, _DimensionReducer, [perplexity(8.0)]).

	test(tsne_learn_3_exaggeration_iterations_too_large, error(domain_error(early_exaggeration_iterations, 11-10))) :-
		tsne_projection::learn(correlated_plane, _DimensionReducer, [maximum_iterations(10), early_exaggeration_iterations(11)]).

	test(tsne_learn_3_component_count_too_large, error(domain_error(component_count, 4-3))) :-
		tsne_projection::learn(correlated_plane, _DimensionReducer, [n_components(4)]).

	test(tsne_learn_2_singleton_dataset, error(domain_error(minimum_number_of_examples, 1))) :-
		tsne_projection::learn(singleton_measurement, _DimensionReducer).

	test(tsne_learn_2_duplicate_example_id, error(domain_error(example_identifier, a))) :-
		tsne_projection::learn(duplicate_id_tsne_dataset, _DimensionReducer).

	test(tsne_learn_2_non_continuous_dataset, error(domain_error(continuous_attribute, channel))) :-
		tsne_projection::learn(non_continuous_tsne_dataset, _DimensionReducer).

	test(tsne_learn_2_all_missing_attribute, error(domain_error(observed_attribute_values, x))) :-
		tsne_projection::learn(all_missing_attribute_tsne_dataset, _DimensionReducer).

	test(tsne_learn_2_nonnumeric_observed_value, error(type_error(number, invalid))) :-
		tsne_projection::learn(nonnumeric_observed_tsne_dataset, _DimensionReducer).

	test(tsne_learn_2_omitted_attribute, error(existence_error(attribute, y))) :-
		tsne_projection::learn(omitted_attribute_tsne_dataset, _DimensionReducer).

:- end_object.
