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


:- object(invalid_probabilistic_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(score, continuous).

	example(1, [channel-online, score-1.0]).
	example(2, [channel-retail, score-2.0]).

:- end_object.


:- object(duplicate_attribute_probabilistic_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, x-1.1, y-2.0]).
	example(2, [x-2.0, y-4.0]).

:- end_object.


:- object(undeclared_attribute_probabilistic_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-2.0, junk-9.0]).
	example(2, [x-2.0, y-4.0]).

:- end_object.


:- object(duplicate_attribute_declaration_probabilistic_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-2.0]).
	example(2, [x-2.0, y-4.0]).

:- end_object.


:- object(two_example_probabilistic_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).
	attribute_values(z, continuous).

	example(1, [x-1.0, y-2.0, z-3.0]).
	example(2, [x-2.0, y-4.0, z-6.0]).

:- end_object.


:- object(one_attribute_probabilistic_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).

	example(1, [x-1.0]).
	example(2, [x-2.0]).
	example(3, [x-4.0]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-29,
		comment is 'Unit tests for the "probabilistic_pca" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(probabilistic_pca).

	cleanup :-
		^^clean_file('test_output.pl').

	test(probabilistic_pca_learn_2_correlated_plane, deterministic(ground(DimensionReducer))) :-
		probabilistic_pca::learn(correlated_plane, DimensionReducer).

	test(probabilistic_pca_learn_2_structure, deterministic(functor(DimensionReducer, probabilistic_pca_reducer, 6))) :-
		probabilistic_pca::learn(correlated_plane, DimensionReducer).

	test(probabilistic_pca_learn_3_custom_options, deterministic) :-
		probabilistic_pca::learn(correlated_plane, probabilistic_pca_reducer(_Encoders, Components, Loadings, NoiseVariance, ExplainedVariances, Diagnostics), [n_components(1), feature_scaling(false), shortfall_policy(error), maximum_iterations(200), tolerance(1.0e-7)]),
		assertion(length(Components, 1)),
		assertion(length(Loadings, 1)),
		ExplainedVariances = [Variance],
		assertion(Variance > 0.0),
		assertion(NoiseVariance >= 0.0),
		memberchk(options(Options), Diagnostics),
		assertion(memberchk(n_components(1), Options)),
		assertion(memberchk(feature_scaling(false), Options)),
		assertion(memberchk(shortfall_policy(error), Options)),
		assertion(memberchk(maximum_iterations(200), Options)),
		assertion(memberchk(tolerance(1.0e-7), Options)),
		assertion(memberchk(model(probabilistic_pca), Diagnostics)).

	test(probabilistic_pca_check_dimension_reducer_1, deterministic) :-
		probabilistic_pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		probabilistic_pca::check_dimension_reducer(DimensionReducer).

	test(probabilistic_pca_diagnostics_2, deterministic) :-
		probabilistic_pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		probabilistic_pca::diagnostics(DimensionReducer, Diagnostics),
		assertion(memberchk(model(probabilistic_pca), Diagnostics)),
		assertion(memberchk(component_count(1), Diagnostics)),
		assertion(memberchk(explained_variances([_]), Diagnostics)),
		assertion(memberchk(noise_variance(_), Diagnostics)).

	test(probabilistic_pca_learn_2_component_count_exceeds_feature_count, error(domain_error(component_count, 4-3))) :-
		probabilistic_pca::learn(correlated_plane, _DimensionReducer, [n_components(4)]).

	test(probabilistic_pca_learn_2_component_count_exceeds_centered_sample_rank, error(domain_error(component_count, 2-1))) :-
		probabilistic_pca::learn(two_example_probabilistic_pca_dataset, _DimensionReducer, [n_components(2)]).

	test(probabilistic_pca_learn_2_one_attribute_noise_variance, deterministic) :-
		probabilistic_pca::learn(one_attribute_probabilistic_pca_dataset, probabilistic_pca_reducer(_Encoders, Components, Loadings, NoiseVariance, ExplainedVariances, Diagnostics), [n_components(1), feature_scaling(false)]),
		assertion(length(Components, 1)),
		assertion(length(Loadings, 1)),
		ExplainedVariances = [Variance],
		assertion(Variance > 0.0),
		assertion(NoiseVariance =:= 0.0),
		assertion(memberchk(noise_variance(0.0), Diagnostics)).

	test(probabilistic_pca_learn_3_shortfall_policy_truncate, deterministic) :-
		probabilistic_pca::learn(low_rank_rectangular, probabilistic_pca_reducer(_Encoders, Components, Loadings, NoiseVariance, ExplainedVariances, Diagnostics), [n_components(3), feature_scaling(false), shortfall_policy(truncate)]),
		assertion(length(Components, 2)),
		assertion(length(Loadings, 2)),
		assertion(length(ExplainedVariances, 2)),
		assertion(NoiseVariance >= 0.0),
		memberchk(options(Options), Diagnostics),
		assertion(memberchk(shortfall_policy(truncate), Options)),
		memberchk(shortfall(truncated(RequestedComponentCount, LearnedComponentCount, ResidualEigenvalue, Tolerance)), Diagnostics),
		assertion(RequestedComponentCount == 3),
		assertion(LearnedComponentCount == 2),
		assertion(ResidualEigenvalue >= 0.0),
		assertion(Tolerance > 0.0).

	test(probabilistic_pca_learn_3_shortfall_policy_error, error(domain_error(component_count, 3-2))) :-
		probabilistic_pca::learn(low_rank_rectangular, _DimensionReducer, [n_components(3), feature_scaling(false), shortfall_policy(error)]).

	test(probabilistic_pca_learn_3_shortfall_policy_unbound_value, error(domain_error(option, shortfall_policy(_)))) :-
		probabilistic_pca::learn(correlated_plane, _DimensionReducer, [shortfall_policy(_Policy)]).

	test(probabilistic_pca_transform_3_component_names, deterministic) :-
		probabilistic_pca::learn(high_dimensional_measurements, DimensionReducer),
		probabilistic_pca::transform(DimensionReducer, [f1-0.9, f2-1.1, f3-1.0, f4-2.0, f5-2.2, f6-2.1], ReducedInstance),
		assertion(length(ReducedInstance, 2)),
		assertion(memberchk(component_1-_, ReducedInstance)),
		assertion(memberchk(component_2-_, ReducedInstance)).

	test(probabilistic_pca_transform_3_monotonic_component, deterministic(Score1 < Score2)) :-
		probabilistic_pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		probabilistic_pca::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0], [component_1-Score1]),
		probabilistic_pca::transform(DimensionReducer, [x-4.5, y-9.0, z-13.7], [component_1-Score2]).

	test(probabilistic_pca_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		probabilistic_pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		probabilistic_pca::export_to_clauses(correlated_plane, DimensionReducer, reduced, [Clause]).

	test(probabilistic_pca_export_to_file_4, deterministic) :-
		^^file_path('test_output.pl', File),
		probabilistic_pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		probabilistic_pca::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		Reducer = probabilistic_pca_reducer(_Encoders, Components, Loadings, NoiseVariance, ExplainedVariances, _Diagnostics),
		assertion(length(Components, 1)),
		assertion(length(Loadings, 1)),
		assertion(NoiseVariance >= 0.0),
		ExplainedVariances = [Variance],
		assertion(Variance > 0.0).

	test(probabilistic_pca_transform_3_exported_functor, deterministic(memberchk(component_1-_, ReducedInstance))) :-
		^^file_path('test_output.pl', File),
		probabilistic_pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		probabilistic_pca::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		probabilistic_pca::transform(Reducer, [x-1.0, y-2.0, z-3.0], ReducedInstance).

	test(probabilistic_pca_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		probabilistic_pca::learn(correlated_plane, DimensionReducer),
		probabilistic_pca::print_dimension_reducer(DimensionReducer).

	test(probabilistic_pca_learn_2_singleton_dataset, error(domain_error(minimum_number_of_examples, 1))) :-
		probabilistic_pca::learn(singleton_measurement, _DimensionReducer).

	test(probabilistic_pca_learn_2_duplicate_training_attribute, error(domain_error(attribute_occurrences, x))) :-
		probabilistic_pca::learn(duplicate_attribute_probabilistic_pca_dataset, _DimensionReducer).

	test(probabilistic_pca_learn_2_undeclared_training_attribute, error(domain_error(declared_attribute, junk))) :-
		probabilistic_pca::learn(undeclared_attribute_probabilistic_pca_dataset, _DimensionReducer).

	test(probabilistic_pca_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, x))) :-
		probabilistic_pca::learn(duplicate_attribute_declaration_probabilistic_pca_dataset, _DimensionReducer).

	test(probabilistic_pca_transform_3_duplicate_attribute, error(domain_error(attribute_occurrences, x))) :-
		probabilistic_pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		probabilistic_pca::transform(DimensionReducer, [x-1.0, x-1.1, y-2.0, z-3.0], _ReducedInstance).

	test(probabilistic_pca_transform_3_undeclared_attribute, error(domain_error(declared_attribute, junk))) :-
		probabilistic_pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		probabilistic_pca::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0, junk-9.0], _ReducedInstance).

	test(probabilistic_pca_learn_2_invalid_dataset, error(domain_error(continuous_attribute, channel))) :-
		probabilistic_pca::learn(invalid_probabilistic_pca_dataset, _DimensionReducer).

:- end_object.
