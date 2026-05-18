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


:- object(invalid_kernel_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(score, continuous).

	example(1, [channel-online, score-1.0]).
	example(2, [channel-retail, score-2.0]).

:- end_object.


:- object(duplicate_attribute_kernel_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, x-1.1, y-2.0]).
	example(2, [x-2.0, y-4.0]).

:- end_object.


:- object(undeclared_attribute_kernel_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-2.0, junk-9.0]).
	example(2, [x-2.0, y-4.0]).

:- end_object.


:- object(duplicate_attribute_declaration_kernel_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-2.0]).
	example(2, [x-2.0, y-4.0]).

:- end_object.


:- object(shortfall_kernel_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-2.0]).
	example(2, [x-1.0, y-2.0]).
	example(3, [x-1.0, y-2.0]).

:- end_object.


:- object(concentric_circles_kernel_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-0.0]).
	example(2, [x-0.0, y-1.0]).
	example(3, [x-(-1.0), y-0.0]).
	example(4, [x-0.0, y-(-1.0)]).
	example(5, [x-2.0, y-0.0]).
	example(6, [x-0.0, y-2.0]).
	example(7, [x-(-2.0), y-0.0]).
	example(8, [x-0.0, y-(-2.0)]).

:- end_object.


:- object(nonlinear_separation_kernel_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-1.0]).
	example(2, [x-1.2, y-0.8]).
	example(3, [x-(-1.0), y-(-1.0)]).
	example(4, [x-(-1.2), y-(-0.8)]).
	example(5, [x-1.0, y-(-1.0)]).
	example(6, [x-1.2, y-(-0.8)]).
	example(7, [x-(-1.0), y-1.0]).
	example(8, [x-(-1.2), y-0.8]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-18,
		comment is 'Unit tests for the "kernel_pca_projection" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	cover(kernel_pca_projection).

	cleanup :-
		^^clean_file('test_output.pl').

	component_score(ReducedInstance, ComponentName, Score) :-
		memberchk(ComponentName-Score, ReducedInstance).

	test(kernel_pca_learn_2_correlated_plane, deterministic(ground(DimensionReducer))) :-
		kernel_pca_projection::learn(correlated_plane, DimensionReducer).

	test(kernel_pca_learn_2_structure, deterministic(functor(DimensionReducer, kernel_pca_reducer, 7))) :-
		kernel_pca_projection::learn(correlated_plane, DimensionReducer).

	test(kernel_pca_learn_3_custom_options, deterministic([NComponents, FeatureScaling, ShortfallPolicy, Kernel, MaximumIterations, Tolerance, Model] == [1, false, error, polynomial(2, 0.5, 1.0), 200, 1.0e-7, kernel_pca_projection])) :-
		kernel_pca_projection::learn(correlated_plane, kernel_pca_reducer(_Encoders, _TrainingRows, _RowMeans, _TotalMean, Components, ExplainedVariances, Diagnostics), [n_components(1), feature_scaling(false), shortfall_policy(error), kernel(polynomial(2, 0.5, 1.0)), maximum_iterations(200), tolerance(1.0e-7)]),
		assertion(ground(Components)),
		assertion(length(Components, 1)),
		ExplainedVariances = [Variance],
		assertion(Variance > 0.0),
		memberchk(options(Options), Diagnostics),
		assertion(ground(Options)),
		memberchk(n_components(NComponents), Options),
		memberchk(feature_scaling(FeatureScaling), Options),
		memberchk(shortfall_policy(ShortfallPolicy), Options),
		memberchk(kernel(Kernel), Options),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(model(Model), Diagnostics).

	test(kernel_pca_learn_3_shortfall_policy_truncate, deterministic([ComponentCount, ShortfallPolicy, RequestedComponentCount, LearnedComponentCount] == [0, truncate, 1, 0])) :-
		kernel_pca_projection::learn(shortfall_kernel_pca_dataset, kernel_pca_reducer(_Encoders, _TrainingRows, _RowMeans, _TotalMean, Components, ExplainedVariances, Diagnostics), [n_components(1), feature_scaling(false), shortfall_policy(truncate), kernel(linear)]),
		assertion(Components == []),
		assertion(ExplainedVariances == []),
		memberchk(component_count(ComponentCount), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(shortfall_policy(ShortfallPolicy), Options),
		memberchk(shortfall(truncated(RequestedComponentCount, LearnedComponentCount, ResidualEigenvalue, Tolerance)), Diagnostics),
		assertion(ResidualEigenvalue >= 0.0),
		assertion(Tolerance =:= 1.0e-8).

	test(kernel_pca_learn_3_shortfall_policy_error, error(domain_error(component_count, 1-0))) :-
		kernel_pca_projection::learn(shortfall_kernel_pca_dataset, _DimensionReducer, [n_components(1), feature_scaling(false), shortfall_policy(error), kernel(linear)]).

	test(kernel_pca_learn_3_shortfall_policy_unbound_value, error(domain_error(option, shortfall_policy(_)))) :-
		kernel_pca_projection::learn(correlated_plane, _DimensionReducer, [shortfall_policy(_Policy)]).

	test(kernel_pca_check_dimension_reducer_1, deterministic) :-
		kernel_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		kernel_pca_projection::check_dimension_reducer(DimensionReducer).

	test(kernel_pca_diagnostics_2, deterministic([Model, SampleCount, ComponentCount, Preprocessing, ShortfallPolicy] == [kernel_pca_projection, 8, 1, [center(true), feature_scaling(true)], truncate])) :-
		kernel_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		kernel_pca_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(sample_count(SampleCount), Diagnostics),
		memberchk(component_count(ComponentCount), Diagnostics),
		memberchk(explained_variances([ExplainedVariance]), Diagnostics),
		assertion(nonvar(ExplainedVariance)),
		memberchk(preprocessing(Preprocessing), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(shortfall_policy(ShortfallPolicy), Options),
		assertion(\+ member(shortfall(_), Diagnostics)).

	test(kernel_pca_learn_2_component_count_exceeds_sample_rank, error(domain_error(component_count, 8-7))) :-
		kernel_pca_projection::learn(correlated_plane, _DimensionReducer, [n_components(8)]).

	test(kernel_pca_transform_3_component_names, deterministic) :-
		kernel_pca_projection::learn(high_dimensional_measurements, DimensionReducer),
		kernel_pca_projection::transform(DimensionReducer, [f1-0.9, f2-1.1, f3-1.0, f4-2.0, f5-2.2, f6-2.1], ReducedInstance),
		assertion(length(ReducedInstance, 2)),
		memberchk(component_1-Component1Score, ReducedInstance),
		memberchk(component_2-Component2Score, ReducedInstance),
		assertion(nonvar(Component1Score)),
		assertion(nonvar(Component2Score)).

	test(kernel_pca_transform_3_monotonic_component, deterministic) :-
		kernel_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1), kernel(linear)]),
		kernel_pca_projection::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0], [component_1-Score1]),
		kernel_pca_projection::transform(DimensionReducer, [x-4.5, y-9.0, z-13.7], [component_1-Score2]),
		AbsScore1 is abs(Score1),
		AbsScore2 is abs(Score2),
		MagnitudeDelta is abs(AbsScore1 - AbsScore2),
		assertion(Score1 * Score2 < 0.0),
		assertion(MagnitudeDelta < 0.05).

	test(kernel_pca_transform_3_rbf_component_names, deterministic) :-
		kernel_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(2), kernel(rbf(0.25))]),
		kernel_pca_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance),
		assertion(length(ReducedInstance, 2)),
		memberchk(component_1-Component1Score, ReducedInstance),
		memberchk(component_2-Component2Score, ReducedInstance),
		assertion(nonvar(Component1Score)),
		assertion(nonvar(Component2Score)).

	test(kernel_pca_transform_3_rbf_concentric_circles_radius_response, deterministic) :-
		kernel_pca_projection::learn(concentric_circles_kernel_pca_dataset, DimensionReducer, [n_components(2), feature_scaling(false), kernel(rbf(0.5))]),
		kernel_pca_projection::transform(DimensionReducer, [x-0.70710678, y-0.70710678], InnerReducedInstance),
		kernel_pca_projection::transform(DimensionReducer, [x-1.41421356, y-1.41421356], OuterReducedInstance),
		component_score(InnerReducedInstance, component_1, InnerScore),
		component_score(OuterReducedInstance, component_1, OuterScore),
		RadiusDelta is abs(InnerScore - OuterScore),
		assertion(RadiusDelta > 0.05).

	test(kernel_pca_transform_3_rbf_nonlinear_separation_mirror_response, deterministic) :-
		kernel_pca_projection::learn(nonlinear_separation_kernel_pca_dataset, DimensionReducer, [n_components(2), feature_scaling(false), kernel(rbf(0.5))]),
		kernel_pca_projection::transform(DimensionReducer, [x-1.2, y-1.1], UpperReducedInstance),
		kernel_pca_projection::transform(DimensionReducer, [x-1.2, y-(-1.1)], LowerReducedInstance),
		component_score(UpperReducedInstance, component_1, UpperScore1),
		component_score(LowerReducedInstance, component_1, LowerScore1),
		component_score(UpperReducedInstance, component_2, UpperScore2),
		component_score(LowerReducedInstance, component_2, LowerScore2),
		MirrorDelta is abs(UpperScore1 - LowerScore1),
		MagnitudeDelta is abs(abs(UpperScore2) - abs(LowerScore2)),
		assertion(MirrorDelta < 1.0e-4),
		assertion(UpperScore2 * LowerScore2 < 0.0),
		assertion(MagnitudeDelta < 1.0e-4).

	test(kernel_pca_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		kernel_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		kernel_pca_projection::export_to_clauses(correlated_plane, DimensionReducer, reduced, [Clause]).

	test(kernel_pca_export_to_file_4, deterministic) :-
		^^file_path('test_output.pl', File),
		kernel_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		kernel_pca_projection::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File, [reload(always)]),
		{reducer(Reducer)},
		Reducer = kernel_pca_reducer(_Encoders, TrainingRows, RowMeans, TotalMean, Components, ExplainedVariances, _Diagnostics),
		assertion(ground(TrainingRows)),
		assertion(ground(RowMeans)),
		assertion(number(TotalMean)),
		assertion(ground(Components)),
		assertion(length(Components, 1)),
		ExplainedVariances = [Variance],
		assertion(Variance > 0.0).

	test(kernel_pca_transform_3_exported_functor, deterministic) :-
		^^file_path('test_output.pl', File),
		kernel_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1), kernel(rbf(0.25))]),
		kernel_pca_projection::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File, [reload(always)]),
		{reducer(Reducer)},
		kernel_pca_projection::transform(Reducer, [x-1.0, y-2.0, z-3.0], ReducedInstance),
		memberchk(component_1-Component1Score, ReducedInstance),
		assertion(nonvar(Component1Score)).

	test(kernel_pca_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		kernel_pca_projection::learn(correlated_plane, DimensionReducer),
		kernel_pca_projection::print_dimension_reducer(DimensionReducer).

	test(kernel_pca_learn_2_singleton_dataset, error(domain_error(minimum_number_of_examples, 1))) :-
		kernel_pca_projection::learn(singleton_measurement, _DimensionReducer).

	test(kernel_pca_learn_2_duplicate_training_attribute, error(domain_error(attribute_occurrences, x))) :-
		kernel_pca_projection::learn(duplicate_attribute_kernel_pca_dataset, _DimensionReducer).

	test(kernel_pca_learn_2_undeclared_training_attribute, error(domain_error(declared_attribute, junk))) :-
		kernel_pca_projection::learn(undeclared_attribute_kernel_pca_dataset, _DimensionReducer).

	test(kernel_pca_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, x))) :-
		kernel_pca_projection::learn(duplicate_attribute_declaration_kernel_pca_dataset, _DimensionReducer).

	test(kernel_pca_transform_3_duplicate_attribute, error(domain_error(attribute_occurrences, x))) :-
		kernel_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		kernel_pca_projection::transform(DimensionReducer, [x-1.0, x-1.1, y-2.0, z-3.0], _ReducedInstance).

	test(kernel_pca_transform_3_undeclared_attribute, error(domain_error(declared_attribute, junk))) :-
		kernel_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		kernel_pca_projection::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0, junk-9.0], _ReducedInstance).

	test(kernel_pca_learn_2_invalid_dataset, error(domain_error(continuous_attribute, channel))) :-
		kernel_pca_projection::learn(invalid_kernel_pca_dataset, _DimensionReducer).

	test(kernel_pca_learn_3_unbound_kernel_value, error(domain_error(option, kernel(_)))) :-
		kernel_pca_projection::learn(correlated_plane, _DimensionReducer, [kernel(_Kernel)]).

	test(kernel_pca_learn_3_negative_polynomial_offset, error(domain_error(option, kernel(polynomial(2, 0.5, -1.0))))) :-
		kernel_pca_projection::learn(correlated_plane, _DimensionReducer, [kernel(polynomial(2, 0.5, -1.0))]).

:- end_object.
