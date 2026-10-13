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
		date is 2026-10-13,
		comment is 'Unit tests for the "tsne_projection" library.'
	]).

	:- uses(fast_random(xoshiro128pp), [
		get_seed/1
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, selectchk/3
	]).

	:- uses(numberlist, [
		sum/2
	]).

	cover(tsne_projection).

	:- private(cached_reducer/2).
	:- dynamic(cached_reducer/2).

	:- private(recorded_warning/1).
	:- dynamic(recorded_warning/1).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, warning, tsne_projection, _Tokens) :-
		assertz(recorded_warning(Message)).

	setup :-
		retractall(cached_reducer(_, _)),
		quick_options(Options),
		tsne_projection::learn(correlated_plane, Plane, Options),
		assertz(cached_reducer(plane, Plane)),
		tsne_projection::learn(missing_values_tsne_dataset, Missing, Options),
		assertz(cached_reducer(missing, Missing)).

	cleanup :-
		retractall(cached_reducer(_, _)),
		retractall(recorded_warning(_)),
		^^clean_file('test_output.pl').

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
		cached_model(plane, DimensionReducer),
		tsne_projection::check_dimension_reducer(DimensionReducer).

	test(tsne_valid_dimension_reducer_1_malformed_embedding, fail) :-
		cached_model(plane, tsne_reducer(Encoders, ExampleIds, TrainingRows, [_FirstRow| EmbeddingRows], Diagnostics)),
		tsne_projection::valid_dimension_reducer(tsne_reducer(Encoders, ExampleIds, TrainingRows, EmbeddingRows, Diagnostics)).

	test(tsne_diagnostics_2_kl_improves, deterministic(FinalKL < InitialKL)) :-
		tsne_projection::learn(correlated_plane, DimensionReducer, [perplexity(2.0), learning_rate(20.0), maximum_iterations(300), early_exaggeration_iterations(50), tolerance(1.0e-6)]),
		tsne_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(initial_kl_divergence(InitialKL), Diagnostics),
		memberchk(final_kl_divergence(FinalKL), Diagnostics).

	test(tsne_diagnostics_2_metadata, deterministic([Model, SampleCount, ComponentCount, Preprocessing] == [tsne_projection, 8, 2, [center(true), feature_scaling(true), missing_values(mean_imputation)]])) :-
		cached_model(plane, DimensionReducer),
		tsne_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(sample_count(SampleCount), Diagnostics),
		memberchk(component_count(ComponentCount), Diagnostics),
		memberchk(preprocessing(Preprocessing), Diagnostics).

	test(tsne_transform_3_component_names, deterministic) :-
		cached_model(plane, DimensionReducer),
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
		cached_model(missing, DimensionReducer),
		tsne_projection::check_dimension_reducer(DimensionReducer).

	test(tsne_transform_3_missing_value, deterministic) :-
		cached_model(missing, DimensionReducer),
		tsne_projection::transform(DimensionReducer, [x-_, y-6.0], ReducedInstance),
		memberchk(component_1-Component1, ReducedInstance),
		memberchk(component_2-Component2, ReducedInstance),
		assertion(number(Component1)),
		assertion(number(Component2)).

	test(tsne_transform_3_omitted_attribute, error(existence_error(attribute, y))) :-
		cached_model(missing, DimensionReducer),
		tsne_projection::transform(DimensionReducer, [x-1.0], _ReducedInstance).

	test(tsne_transform_3_duplicate_attribute, error(domain_error(attribute_occurrences, x))) :-
		cached_model(missing, DimensionReducer),
		tsne_projection::transform(DimensionReducer, [x-1.0, x-2.0, y-3.0], _ReducedInstance).

	test(tsne_transform_3_undeclared_attribute, error(domain_error(declared_attribute, z))) :-
		cached_model(missing, DimensionReducer),
		tsne_projection::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0], _ReducedInstance).

	test(tsne_transform_3_nonnumeric_observed_value, error(type_error(number, invalid))) :-
		cached_model(missing, DimensionReducer),
		tsne_projection::transform(DimensionReducer, [x-invalid, y-2.0], _ReducedInstance).

	test(tsne_transform_3_deterministic, deterministic(ReducedInstance1 == ReducedInstance2)) :-
		cached_model(plane, DimensionReducer),
		tsne_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance1),
		tsne_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance2).

	test(tsne_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		cached_model(plane, DimensionReducer),
		tsne_projection::export_to_clauses(correlated_plane, DimensionReducer, reduced, [Clause]).

	test(tsne_export_to_file_4, deterministic) :-
		^^file_path('test_output.pl', File),
		cached_model(plane, DimensionReducer),
		tsne_projection::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File, [reload(always)]),
		{reducer(Reducer)},
		tsne_projection::check_dimension_reducer(Reducer).

	test(tsne_export_to_file_4_missing_values, deterministic) :-
		^^file_path('test_output.pl', File),
		cached_model(missing, DimensionReducer),
		tsne_projection::export_to_file(missing_values_tsne_dataset, DimensionReducer, reducer, File),
		logtalk_load(File, [reload(always)]),
		{reducer(Reducer)},
		tsne_projection::check_dimension_reducer(Reducer),
		tsne_projection::transform(Reducer, [x-_, y-6.0], ReducedInstance),
		assertion(ground(ReducedInstance)).

	test(tsne_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		cached_model(plane, DimensionReducer),
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

	test(tsne_projection_empty_distances, deterministic(Matrix == [])) :-
		tsne_projection << squared_distance_matrix([], Matrix).

	test(tsne_projection_attribute_values_hook, deterministic(Values == [x-1.0])) :-
		tsne_projection << example_attribute_values(a-[x-1.0], Values).

	test(tsne_projection_variable_id, error(instantiation_error)) :-
		tsne_projection::learn(variable_id_tsne_dataset, _Reducer).

	test(tsne_projection_partial_id, error(instantiation_error)) :-
		tsne_projection::learn(partial_id_tsne_dataset, _Reducer).

	test(tsne_projection_ground_compound_ids, deterministic) :-
		tsne_projection::learn(two_points_tsne_dataset, Reducer, [n_components(1), perplexity(1.0), maximum_iterations(2), early_exaggeration_iterations(0)]),
		tsne_projection::check_dimension_reducer(Reducer).

	test(tsne_projection_variable_model, error(instantiation_error)) :-
		tsne_projection::check_dimension_reducer(_Reducer).

	test(tsne_projection_partial_model, error(instantiation_error)) :-
		tsne_projection::check_dimension_reducer(tsne_reducer(_, [], [], [], [])).

	test(tsne_projection_missing_stored_options, deterministic) :-
		cached_model(plane, tsne_reducer(Encoders, Ids, Rows, Embedding, Diagnostics)),
		selectchk(options(Options), Diagnostics, Rest),
		forall(member(Option, Options), (
			selectchk(Option, Options, Incomplete),
			assertion(\+ tsne_projection::valid_dimension_reducer(tsne_reducer(Encoders, Ids, Rows, Embedding, [options(Incomplete)| Rest])))
		)),
		assertion(\+ tsne_projection::valid_dimension_reducer(tsne_reducer(Encoders, Ids, Rows, Embedding, [options([])| Rest]))).

	test(tsne_projection_empty_options_transform, error(domain_error(dimension_reducer, _))) :-
		cached_model(plane, tsne_reducer(Encoders, Ids, Rows, Embedding, Diagnostics)),
		selectchk(options(_), Diagnostics, Rest),
		tsne_projection::transform(tsne_reducer(Encoders, Ids, Rows, Embedding, [options([])| Rest]), [x-2.0, y-4.0, z-6.0], _Reduced).

	test(tsne_projection_stored_option_consistency, deterministic) :-
		cached_model(plane, tsne_reducer(Encoders, Ids, Rows, Embedding, Diagnostics)),
		selectchk(options(Options), Diagnostics, Rest),
		forall(member(Invalid, [n_components(1), feature_scaling(false), maximum_iterations(1), perplexity(8.0)]), (
			assertion(\+ tsne_projection::valid_dimension_reducer(tsne_reducer(Encoders, Ids, Rows, Embedding, [options([Invalid| Options])| Rest])))
		)),
		tsne_projection::check_dimension_reducer(tsne_reducer(Encoders, Ids, Rows, Embedding, [options([perplexity(2.0)| Options])| Rest])).

	test(tsne_projection_distance_reference, deterministic) :-
		tsne_projection << squared_distance_matrix([[0.0, 0.0], [1.0, 2.0], [1.0, 2.0]], Matrix),
		assertion(Matrix == [[0.0, 5.0, 5.0], [5.0, 0.0, 0.0], [5.0, 0.0, 0.0]]).

	test(tsne_projection_conditional_reference, deterministic) :-
		tsne_projection << conditional_probabilities([0.0, 1.0, 1.0], 1, 2.0, [Excluded, Left, Right], _Achieved, _Status),
		assertion(Excluded =~= 0.0),
		assertion(Left =~= 0.5),
		assertion(Right =~= 0.5).

	test(tsne_projection_perplexity_below_one, error(domain_error(option, perplexity(0.5)))) :-
		tsne_projection::learn(correlated_plane, _Reducer, [perplexity(0.5)]).

	test(tsne_projection_perplexity_above_support, error(domain_error(perplexity, 7.5-8))) :-
		tsne_projection::learn(correlated_plane, _Reducer, [perplexity(7.5)]).

	test(tsne_projection_perplexity_upper_endpoint, deterministic) :-
		tsne_projection::learn(correlated_plane, Reducer, [perplexity(7.0), maximum_iterations(2), early_exaggeration_iterations(0)]),
		tsne_projection::check_dimension_reducer(Reducer),
		once(tsne_projection::diagnostic(Reducer, affinity_search(summary(8, 0, 0), achieved_perplexity_range(Minimum, Maximum)))),
		assertion(Minimum =~= 7.0),
		assertion(Maximum =~= 7.0).

	test(tsne_projection_affinity_statuses, deterministic) :-
		tsne_projection << conditional_probabilities([0.0, 1.0, 1.0], 1, 1.0, Probabilities, Achieved, tie_limited),
		assertion(Probabilities =~= [0.0, 0.5, 0.5]),
		assertion(Achieved =~= 2.0),
		tsne_projection << conditional_probabilities([1.0, 2.0, 3.0], 0, 3.0, Uniform, Upper, matched),
		assertion(Uniform =~= [0.3333333333333333, 0.3333333333333333, 0.3333333333333333]),
		assertion(Upper =~= 3.0),
		tsne_projection << search_precision([0.0, 1.0, 2.0], 1, 1.0, 0.0, 0.0, unbounded, 1.0, 60, _Limited, _Actual, iteration_limit),
		tsne_projection << affinity_summary([2.0-iteration_limit], summary(0, 0, 0), 2.0, 2.0, affinity_search(summary(0, 0, 1), achieved_perplexity_range(_, _))).

	test(tsne_projection_tied_dataset_and_transform_warning, deterministic) :-
		tsne_projection::learn(tied_points_tsne_dataset, Reducer, [n_components(1), perplexity(1.0), maximum_iterations(2), early_exaggeration_iterations(0)]),
		tsne_projection::check_dimension_reducer(Reducer),
		once(tsne_projection::diagnostic(Reducer, affinity_search(summary(0, 3, 0), achieved_perplexity_range(Minimum, Maximum)))),
		assertion(Minimum =~= 2.0),
		assertion(Maximum =~= 2.0),
		retractall(recorded_warning(_)),
		tsne_projection::transform(Reducer, [x-1.0], Reduced),
		assertion(ground(Reduced)),
		findall(Message, recorded_warning(Message), [perplexity_mismatch(Requested, Achieved, tie_limited)]),
		assertion(Requested =~= 1.0),
		assertion(Achieved =~= 3.0).

	test(tsne_projection_affinity_metadata_required, deterministic) :-
		cached_model(plane, tsne_reducer(Encoders, Ids, Rows, Embedding, Diagnostics)),
		selectchk(affinity_search(_, _), Diagnostics, Rest),
		assertion(\+ tsne_projection::valid_dimension_reducer(tsne_reducer(Encoders, Ids, Rows, Embedding, Rest))),
		assertion(\+ tsne_projection::valid_dimension_reducer(tsne_reducer(Encoders, Ids, Rows, Embedding, [affinity_search(summary(0, 0, 0), achieved_perplexity_range(1.0, 2.0))| Rest]))).

	test(tsne_projection_joint_reference, deterministic) :-
		tsne_projection << joint_probability_matrix([[0.0, 0.8, 0.2], [0.3, 0.0, 0.7], [0.6, 0.4, 0.0]], Matrix),
		matrix_close(Matrix, [[0.0, 0.18333333333333335, 0.13333333333333333], [0.18333333333333335, 0.0, 0.18333333333333335], [0.13333333333333333, 0.18333333333333335, 0.0]]).

	test(tsne_projection_center_reference, deterministic) :-
		forall(
			member(Input-Expected, [[]-[], [[1.0], [3.0]]-[[-1.0], [1.0]], [[2.0, 3.0], [4.0, 7.0]]-[[-1.0, -2.0], [1.0, 2.0]], [[3.0, 3.0]]-[[0.0, 0.0]]]),
			check_center(Input, Expected)
		).

	test(tsne_projection_training_gradient_finite_difference, deterministic) :-
		Probabilities = [[0.0, 0.2, 0.1], [0.2, 0.0, 0.2], [0.1, 0.2, 0.0]],
		Embedding = [[0.5], [2.0], [-1.0]],
		tsne_projection << low_dimensional_weights(Embedding, Weights, WeightSum),
		tsne_projection << gradient_matrix(Probabilities, Weights, WeightSum, Embedding, 1.0, [[Gradient]| _]),
		tsne_projection << kl_divergence(Probabilities, [[0.500001], [2.0], [-1.0]], Upper),
		tsne_projection << kl_divergence(Probabilities, [[0.499999], [2.0], [-1.0]], Lower),
		Derivative is (Upper - Lower) / 0.000002,
		assertion(Gradient =~= Derivative).

	test(tsne_projection_conditional_gradient_finite_difference, deterministic) :-
		forall(member(case(Probabilities, Rows, Coordinates), [case([0.8, 0.2], [[0.0], [2.0]], [0.5]), case([0.6, 0.3, 0.1], [[0.0, 1.0], [2.0, -1.0], [-1.0, 3.0]], [0.5, 0.25]), case([0.8, 0.2], [[10.0], [12.0]], [10.5])]), (
			linear_algebra::transpose_matrix(Rows, Columns),
			tsne_projection << new_point_gradient_columns(Probabilities, Rows, Columns, Coordinates, Gradients),
			check_derivatives(Gradients, Probabilities, Rows, Coordinates, 1)
		)).

	test(tsne_projection_new_point_update_and_stops, deterministic) :-
		tsne_projection << update_new_coordinates([0.5], [0.1], [0.1], 0.2, 0.5, Updated, Updates, 0.0, Delta),
		assertion(Updated =~= [0.53]),
		assertion(Updates =~= [0.03]),
		assertion(Delta =~= 0.03),
		tsne_projection << optimize_new_coordinates([0.5, 0.5], [[0.0], [2.0]], [learning_rate(0.1), maximum_iterations(10), tolerance(1.0e-7)], [1.0], Center),
		assertion(Center =~= [1.0]),
		tsne_projection << optimize_new_coordinates([0.8, 0.2], [[0.0], [2.0]], [learning_rate(0.1), maximum_iterations(1), tolerance(1.0e-7)], [0.5], Limited),
		assertion(Limited =~= [0.48659829059829057]).

	test(tsne_projection_gaussian_search_numerical_cases, deterministic) :-
		forall(member(case(Distances, Excluded, Perplexity), [case([0.0, 1.0, 2.0], 1, 1.5), case([0.0, 0.0, 10.0, 10000.0], 1, 2.5), case([1.0, 2.0, 3.0], 0, 2.0)]), (
			tsne_projection << conditional_probabilities(Distances, Excluded, Perplexity, Probabilities, Achieved, matched),
			sum(Probabilities, Sum),
			assertion(Sum =~= 1.0),
			Difference is abs(log(Achieved) - log(Perplexity)),
			assertion(Difference =< 1.0e-5)
		)).

	test(tsne_projection_symmetric_weight_reference, deterministic(Sum =~= 2.8)) :-
		tsne_projection << low_dimensional_weights([[0.0], [2.0], [2.0]], Weights, Sum),
		matrix_close(Weights, [[0.0, 0.2, 0.2], [0.2, 0.0, 1.0], [0.2, 1.0, 0.0]]).

	test(tsne_projection_aligned_preprocessing, deterministic) :-
		tsne_projection << check_examples(fixture, [x, y], [a-[y-2.0, x-5.0], b-[x-5.0, y-_], c-[y-4.0, x-5.0]], Aligned),
		pairs::values(Aligned, Values),
		linear_algebra::transpose_matrix(Values, Columns),
		tsne_projection << build_encoders([x, y], Columns, [feature_scaling(false)], Encoders),
		assertion(Encoders == [continuous(x, 5.0, 1.0), continuous(y, 3.0, 1.0)]),
		tsne_projection << examples_to_rows(Aligned, Encoders, Rows),
		matrix_close(Rows, [[0.0, -1.0], [0.0, 0.0], [0.0, 1.0]]).

	test(tsne_projection_indexed_identifier_terms, deterministic) :-
		tsne_projection << valid_example_ids([a, 1, 1.0, id(a), id(b)]),
		assertion(\+ (tsne_projection << valid_example_ids([a, id(a), a]))),
		assertion(\+ (tsne_projection << valid_example_ids([id(_), b]))).

	test(tsne_projection_optimization_phase_boundaries, deterministic) :-
		tsne_projection << optimization_phase(1, 0, 12.0, Scale0, Momentum0),
		assertion(Scale0 =~= 1.0),
		assertion(Momentum0 =~= 0.8),
		tsne_projection << optimization_phase(2, 2, 12.0, Scale1, Momentum1),
		assertion(Scale1 =~= 12.0),
		assertion(Momentum1 =~= 0.5),
		tsne_projection << optimization_phase(3, 2, 12.0, Scale2, Momentum2),
		assertion(Scale2 =~= 1.0),
		assertion(Momentum2 =~= 0.8).

	test(tsne_projection_adaptive_gain_reference, deterministic(Delta =~= 0.162)) :-
		tsne_projection << update_row([0.5, 0.2], [0.1, -0.2], [-0.3, -0.3], [1.0, 1.0], 0.1, 0.5, Updated, Updates, Gains, 0.0, Delta),
		assertion(Updated =~= [0.338, 0.066]),
		assertion(Updates =~= [-0.162, -0.134]),
		assertion(Gains =~= [1.2, 0.8]).

	test(tsne_projection_first_option_and_early_limit, deterministic) :-
		tsne_projection::learn(correlated_plane, Reducer, [perplexity(2.0), perplexity(5.0), maximum_iterations(2), early_exaggeration_iterations(2)]),
		tsne_projection::check_dimension_reducer(Reducer),
		tsne_projection::diagnostics(Reducer, Diagnostics),
		memberchk(iterations(2), Diagnostics),
		memberchk(convergence(maximum_iterations_exhausted), Diagnostics),
		tsne_projection::dimension_reducer_options(Reducer, Options),
		assertion(memberchk(perplexity(2.0), Options)).

	test(tsne_projection_learning_duplicate_precedence, error(domain_error(attribute_occurrences, x))) :-
		tsne_projection::learn(duplicate_attribute_tsne_dataset, _Reducer, [perplexity(1.0)]).

	test(tsne_projection_learning_undeclared_precedence, error(domain_error(declared_attribute, z))) :-
		tsne_projection::learn(undeclared_attribute_tsne_dataset, _Reducer, [perplexity(1.0)]).

	test(tsne_projection_transform_missing_precedence, error(existence_error(attribute, y))) :-
		cached_model(missing, Reducer),
		tsne_projection::transform(Reducer, [x-invalid, z-3.0], _Reduced).

	test(tsne_projection_single_observation_unit_scale, deterministic) :-
		tsne_projection::learn(single_observed_tsne_dataset, Reducer, [n_components(1), perplexity(1.0), maximum_iterations(2), early_exaggeration_iterations(0)]),
		tsne_projection::check_dimension_reducer(Reducer),
		Reducer = tsne_reducer(Encoders, _Ids, Rows, _Embedding, _Diagnostics),
		assertion(Encoders == [continuous(x, 8.0, 1.0)]),
		matrix_close(Rows, [[0.0], [0.0], [0.0]]).

	test(tsne_projection_attribute_metadata_consistency, deterministic) :-
		cached_model(plane, tsne_reducer(Encoders, Ids, Rows, Embedding, Diagnostics)),
		selectchk(attribute_names(_), Diagnostics, Rest),
		assertion(\+ tsne_projection::valid_dimension_reducer(tsne_reducer(Encoders, Ids, Rows, Embedding, [attribute_names([z, x, y])| Rest]))).

	test(tsne_projection_affinity_metadata_bounds, deterministic) :-
		cached_model(plane, tsne_reducer(Encoders, Ids, Rows, Embedding, Diagnostics)),
		selectchk(affinity_search(_, _), Diagnostics, Rest),
		forall(member(Invalid, [affinity_search(summary(-1, 9, 0), achieved_perplexity_range(1.0, 2.0)), affinity_search(summary(8.0, 0, 0), achieved_perplexity_range(1.0, 2.0)), affinity_search(summary(8, 0, 0), achieved_perplexity_range(0.5, 2.0)), affinity_search(summary(8, 0, 0), achieved_perplexity_range(3.0, 2.0)), affinity_search(summary(8, 0, 0), achieved_perplexity_range(1.0, 8.0))]), (
			assertion(\+ tsne_projection::valid_dimension_reducer(tsne_reducer(Encoders, Ids, Rows, Embedding, [Invalid| Rest])))
		)).

	test(tsne_projection_conditional_small_step_descent, deterministic(After < Before)) :-
		conditional_loss([0.8, 0.2], [[0.0], [2.0]], [0.5], Before),
		tsne_projection << optimize_new_coordinates([0.8, 0.2], [[0.0], [2.0]], [learning_rate(0.1), maximum_iterations(1), tolerance(1.0e-7)], [0.5], Coordinates),
		conditional_loss([0.8, 0.2], [[0.0], [2.0]], Coordinates, After).

	% auxiliary predicates

	quick_options([
		perplexity(2.0),
		learning_rate(20.0),
		maximum_iterations(100),
		early_exaggeration_iterations(20),
		tolerance(1.0e-6)
	]).

	cached_model(Key, DimensionReducer) :-
		cached_reducer(Key, DimensionReducer),
		!.

	check_center(Input, Expected) :-
		tsne_projection << center_embedding(Input, Actual),
		matrix_close(Actual, Expected).

	matrix_close([], []).
	matrix_close([Actual| ActualRows], [Expected| ExpectedRows]) :-
		assertion(Actual =~= Expected),
		matrix_close(ActualRows, ExpectedRows).

	check_derivatives([], _Probabilities, _Rows, _Coordinates, _Index).
	check_derivatives([Gradient| Gradients], Probabilities, Rows, Coordinates, Index) :-
		perturb_coordinate(Coordinates, Index, 0.000001, UpperCoordinates),
		perturb_coordinate(Coordinates, Index, -0.000001, LowerCoordinates),
		conditional_loss(Probabilities, Rows, UpperCoordinates, Upper),
		conditional_loss(Probabilities, Rows, LowerCoordinates, Lower),
		Derivative is (Upper - Lower) / 0.000002,
		assertion(Gradient =~= Derivative),
		Next is Index + 1,
		check_derivatives(Gradients, Probabilities, Rows, Coordinates, Next).

	perturb_coordinate([Coordinate| Coordinates], 1, Offset, [Perturbed| Coordinates]) :-
		!,
		Perturbed is Coordinate + Offset.
	perturb_coordinate([Coordinate| Coordinates], Index, Offset, [Coordinate| Perturbed]) :-
		Next is Index - 1,
		perturb_coordinate(Coordinates, Next, Offset, Perturbed).

	conditional_loss(Probabilities, Rows, Coordinates, Loss) :-
		reference_weights(Rows, Coordinates, Weights, 0.0, Sum),
		reference_loss(Probabilities, Weights, Sum, 0.0, Loss).

	reference_weights([], _Coordinates, [], Sum, Sum).
	reference_weights([Row| Rows], Coordinates, [Weight| Weights], Sum0, Sum) :-
		reference_distance(Row, Coordinates, 0.0, Distance),
		Weight is 1.0 / (1.0 + Distance),
		Sum1 is Sum0 + Weight,
		reference_weights(Rows, Coordinates, Weights, Sum1, Sum).

	reference_distance([], [], Sum, Sum).
	reference_distance([Left| Lefts], [Right| Rights], Sum0, Sum) :-
		Sum1 is Sum0 + (Left - Right) ** 2,
		reference_distance(Lefts, Rights, Sum1, Sum).

	reference_loss([], [], _Sum, Loss, Loss).
	reference_loss([Probability| Probabilities], [Weight| Weights], Sum, Loss0, Loss) :-
		Loss1 is Loss0 + Probability * log(Probability / (Weight / Sum)),
		reference_loss(Probabilities, Weights, Sum, Loss1, Loss).

:- end_object.
