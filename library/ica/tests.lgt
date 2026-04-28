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


:- object(invalid_ica_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(score, continuous).

	example(1, [channel-online, score-1.0]).
	example(2, [channel-retail, score-2.0]).

:- end_object.


:- object(duplicate_attribute_ica_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).

	example(1, [x1-1.0, x1-1.1, x2-2.0]).
	example(2, [x1-2.0, x2-4.0]).

:- end_object.


:- object(undeclared_attribute_ica_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).

	example(1, [x1-1.0, x2-2.0, junk-9.0]).
	example(2, [x1-2.0, x2-4.0]).

:- end_object.


:- object(duplicate_attribute_declaration_ica_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x1, continuous).
	attribute_values(x2, continuous).

	example(1, [x1-1.0, x2-2.0]).
	example(2, [x1-2.0, x2-4.0]).

:- end_object.


:- object(sample_limited_ica_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).
	attribute_values(f3, continuous).
	attribute_values(f4, continuous).

	example(1, [f1-1.0, f2-2.0, f3-3.0, f4-1.0]).
	example(2, [f1-2.0, f2-1.0, f3-0.0, f4-4.0]).
	example(3, [f1-4.0, f2-3.0, f3-1.0, f4-2.0]).

:- end_object.


:- object(near_singular_ica_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).
	attribute_values(x3, continuous).

	example(1, [x1-(-4.0), x2-(-8.01), x3-3.99]).
	example(2, [x1-(-3.0), x2-(-5.99), x3-3.01]).
	example(3, [x1-(-2.0), x2-(-4.01), x3-2.01]).
	example(4, [x1-(-1.0), x2-(-1.99), x3-1.01]).
	example(5, [x1-1.0, x2-1.99, x3-(-1.01)]).
	example(6, [x1-2.0, x2-4.01, x3-(-1.99)]).
	example(7, [x1-3.0, x2-5.99, x3-(-2.99)]).
	example(8, [x1-4.0, x2-8.01, x3-(-3.99)]).

:- end_object.


:- object(harder_mixed_independent_sources,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).
	attribute_values(x3, continuous).
	attribute_values(x4, continuous).

	example(1,  [x1-(-5.0), x2-0.0, x3-(-11.0), x4-(-16.0)]).
	example(2,  [x1-(-7.0), x2-8.0, x3-(-1.0), x4-(-8.0)]).
	example(3,  [x1-3.0, x2-5.0, x3-(-6.0), x4-(-3.0)]).
	example(4,  [x1-1.0, x2-13.0, x3-4.0, x4-5.0]).
	example(5,  [x1-(-9.0), x2-(-3.0), x3-6.0, x4-(-3.0)]).
	example(6,  [x1-1.0, x2-(-7.0), x3-(-8.0), x4-(-7.0)]).
	example(7,  [x1-(-3.0), x2-1.0, x3-12.0, x4-9.0]).
	example(8,  [x1-7.0, x2-(-3.0), x3-(-2.0), x4-5.0]).
	example(9,  [x1-(-5.0), x2-(-11.0), x3-10.0, x4-5.0]).
	example(10, [x1-5.0, x2-(-15.0), x3-(-4.0), x4-1.0]).
	example(11, [x1-3.0, x2-(-1.0), x3-12.0, x4-15.0]).
	example(12, [x1-13.0, x2-(-5.0), x3-(-2.0), x4-11.0]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-28,
		comment is 'Unit tests for the "ica" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(ica).

	cleanup :-
		^^clean_file('test_output.pl').

	test(ica_learn_2_mixed_independent_sources, deterministic(ground(DimensionReducer))) :-
		ica::learn(mixed_independent_sources, DimensionReducer).

	test(ica_learn_2_structure, deterministic(functor(DimensionReducer, ica_reducer, 3))) :-
		ica::learn(mixed_independent_sources, DimensionReducer).

	test(ica_learn_3_custom_options, deterministic) :-
		ica::learn(mixed_independent_sources, ica_reducer(_Encoders, Components, Diagnostics), [n_components(1), feature_scaling(true), maximum_iterations(200), tolerance(1.0e-7)]),
		assertion(ground(Components)),
		assertion(length(Components, 1)),
		memberchk(options(Options), Diagnostics),
		assertion(ground(Options)),
		assertion(memberchk(n_components(1), Options)),
		assertion(memberchk(feature_scaling(true), Options)),
		assertion(memberchk(maximum_iterations(200), Options)),
		assertion(memberchk(tolerance(1.0e-7), Options)),
		assertion(memberchk(model(ica), Diagnostics)).

	test(ica_check_dimension_reducer_1, deterministic) :-
		ica::learn(mixed_independent_sources, DimensionReducer, [n_components(2)]),
		ica::check_dimension_reducer(DimensionReducer).

	test(ica_diagnostics_2, deterministic) :-
		ica::learn(mixed_independent_sources, DimensionReducer, [n_components(2)]),
		ica::diagnostics(DimensionReducer, Diagnostics),
		assertion(memberchk(model(ica), Diagnostics)),
		assertion(memberchk(component_count(2), Diagnostics)),
		memberchk(whitening_eigenvalues([FirstEigenvalue, SecondEigenvalue]), Diagnostics),
		assertion(FirstEigenvalue >= SecondEigenvalue),
		assertion(SecondEigenvalue > 0.0),
		memberchk(convergence(Convergences), Diagnostics),
		assertion(ground(Convergences)),
		assertion(length(Convergences, 2)),
		memberchk(iterations(IterationCounts), Diagnostics),
		assertion(ground(IterationCounts)),
		assertion(length(IterationCounts, 2)),
		memberchk(final_delta(FinalDeltas), Diagnostics),
		assertion(ground(FinalDeltas)),
		assertion(length(FinalDeltas, 2)).

	test(ica_diagnostics_2_maximum_iterations, deterministic) :-
		ica::learn(mixed_independent_sources, DimensionReducer, [n_components(2), maximum_iterations(1), tolerance(1.0e-30)]),
		ica::diagnostics(DimensionReducer, Diagnostics),
		memberchk(convergence(Convergences), Diagnostics),
		assertion(ground(Convergences)),
		assertion(memberchk(maximum_iterations_exhausted, Convergences)),
		memberchk(options(Options), Diagnostics),
		assertion(ground(Options)),
		assertion(memberchk(maximum_iterations(1), Options)),
		assertion(memberchk(tolerance(1.0e-30), Options)).

	test(ica_learn_2_component_count_exceeds_rank, error(domain_error(component_count, 3-2))) :-
		ica::learn(mixed_independent_sources, _DimensionReducer, [n_components(3)]).

	test(ica_learn_2_component_count_exceeds_centered_sample_bound, error(domain_error(component_count, 3-2))) :-
		ica::learn(sample_limited_ica_dataset, _DimensionReducer, [n_components(3)]).

	test(ica_transform_3_component_names, deterministic) :-
		ica::learn(mixed_independent_sources, DimensionReducer, [n_components(2)]),
		ica::transform(DimensionReducer, [x1-(-5.0), x2-(-4.0), x3-(-4.0)], ReducedInstance),
		assertion(length(ReducedInstance, 2)),
		assertion(memberchk(component_1-_, ReducedInstance)),
		assertion(memberchk(component_2-_, ReducedInstance)).

	test(ica_transform_3_separates_sources, deterministic) :-
		ica::learn(mixed_independent_sources, DimensionReducer, [n_components(2)]),
		ica::transform(DimensionReducer, [x1-(-5.0), x2-(-4.0), x3-(-4.0)], ExampleA),
		ica::transform(DimensionReducer, [x1-1.0, x2-8.0, x3-2.0], ExampleB),
		ica::transform(DimensionReducer, [x1-(-1.0), x2-(-8.0), x3-(-2.0)], ExampleC),
		absolute_component_differences(ExampleA, ExampleB, Source1Delta1, Source1Delta2),
		absolute_component_differences(ExampleA, ExampleC, Source2Delta1, Source2Delta2),
		smaller_delta_index(Source1Delta1, Source1Delta2, Source1InvariantIndex, Source1SmallDelta, Source1LargeDelta),
		smaller_delta_index(Source2Delta1, Source2Delta2, Source2InvariantIndex, Source2SmallDelta, Source2LargeDelta),
		assertion(Source1InvariantIndex \== Source2InvariantIndex),
		assertion(Source1SmallDelta * 4.0 < Source1LargeDelta),
		assertion(Source2SmallDelta * 4.0 < Source2LargeDelta).

	test(ica_diagnostics_2_near_singular_covariance, deterministic) :-
		ica::learn(near_singular_ica_dataset, DimensionReducer, [n_components(3), maximum_iterations(1500), tolerance(1.0e-10)]),
		ica::diagnostics(DimensionReducer, Diagnostics),
		assertion(memberchk(component_count(3), Diagnostics)),
		memberchk(whitening_eigenvalues([FirstEigenvalue, SecondEigenvalue, ThirdEigenvalue]), Diagnostics),
		assertion(FirstEigenvalue >= SecondEigenvalue),
		assertion(SecondEigenvalue >= ThirdEigenvalue),
		assertion(ThirdEigenvalue > 0.0).

	test(ica_transform_3_separates_three_sources, deterministic) :-
		ica::learn(harder_mixed_independent_sources, DimensionReducer, [n_components(3), maximum_iterations(1500)]),
		ica::transform(DimensionReducer, [x1-0.0, x2-0.0, x3-0.0, x4-0.0], Baseline),
		assertion(length(Baseline, 3)),
		ica::transform(DimensionReducer, [x1-6.0, x2-(-12.0), x3-6.0, x4-12.0], Source1Changed),
		ica::transform(DimensionReducer, [x1-12.0, x2-6.0, x3-(-6.0), x4-6.0], Source2Changed),
		ica::transform(DimensionReducer, [x1-(-6.0), x2-6.0, x3-12.0, x4-6.0], Source3Changed),
		component_deltas(Baseline, Source1Changed, Source1Deltas),
		dominant_delta_index(Source1Deltas, Source1Index, Source1Largest, Source1Residual),
		component_deltas(Baseline, Source2Changed, Source2Deltas),
		dominant_delta_index(Source2Deltas, Source2Index, Source2Largest, Source2Residual),
		component_deltas(Baseline, Source3Changed, Source3Deltas),
		dominant_delta_index(Source3Deltas, Source3Index, Source3Largest, Source3Residual),
		assertion(Source1Index \== Source2Index),
		assertion(Source1Index \== Source3Index),
		assertion(Source2Index \== Source3Index),
		assertion(Source1Largest > Source1Residual),
		assertion(Source2Largest > Source2Residual),
		assertion(Source3Largest > Source3Residual).

	test(ica_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		ica::learn(mixed_independent_sources, DimensionReducer, [n_components(2)]),
		ica::export_to_clauses(mixed_independent_sources, DimensionReducer, reduced, [Clause]).

	test(ica_export_to_file_4, deterministic) :-
		^^file_path('test_output.pl', File),
		ica::learn(mixed_independent_sources, DimensionReducer, [n_components(2)]),
		ica::export_to_file(mixed_independent_sources, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		Reducer = ica_reducer(_Encoders, Components, Diagnostics),
		assertion(ground(Components)),
		assertion(ground(Diagnostics)),
		assertion(length(Components, 2)),
		assertion(memberchk(model(ica), Diagnostics)).

	test(ica_transform_3_exported_functor, deterministic(memberchk(component_1-_, ReducedInstance))) :-
		^^file_path('test_output.pl', File),
		ica::learn(mixed_independent_sources, DimensionReducer, [n_components(2)]),
		ica::export_to_file(mixed_independent_sources, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		ica::transform(Reducer, [x1-(-5.0), x2-(-4.0), x3-(-4.0)], ReducedInstance).

	test(ica_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		ica::learn(mixed_independent_sources, DimensionReducer),
		ica::print_dimension_reducer(DimensionReducer).

	test(ica_learn_2_singleton_dataset, error(domain_error(minimum_number_of_examples, 1))) :-
		ica::learn(singleton_measurement, _DimensionReducer).

	test(ica_learn_2_duplicate_training_attribute, error(domain_error(attribute_occurrences, x1))) :-
		ica::learn(duplicate_attribute_ica_dataset, _DimensionReducer).

	test(ica_learn_2_undeclared_training_attribute, error(domain_error(declared_attribute, junk))) :-
		ica::learn(undeclared_attribute_ica_dataset, _DimensionReducer).

	test(ica_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, x1))) :-
		ica::learn(duplicate_attribute_declaration_ica_dataset, _DimensionReducer).

	test(ica_transform_3_duplicate_attribute, error(domain_error(attribute_occurrences, x1))) :-
		ica::learn(mixed_independent_sources, DimensionReducer, [n_components(2)]),
		ica::transform(DimensionReducer, [x1-(-5.0), x1-(-4.5), x2-(-4.0), x3-(-4.0)], _ReducedInstance).

	test(ica_transform_3_undeclared_attribute, error(domain_error(declared_attribute, junk))) :-
		ica::learn(mixed_independent_sources, DimensionReducer, [n_components(2)]),
		ica::transform(DimensionReducer, [x1-(-5.0), x2-(-4.0), x3-(-4.0), junk-9.0], _ReducedInstance).

	test(ica_learn_2_invalid_dataset, error(domain_error(continuous_attribute, channel))) :-
		ica::learn(invalid_ica_dataset, _DimensionReducer).

	absolute_component_differences([component_1-FirstLeft, component_2-SecondLeft], [component_1-FirstRight, component_2-SecondRight], FirstDelta, SecondDelta) :-
		FirstDelta is abs(FirstLeft - FirstRight),
		SecondDelta is abs(SecondLeft - SecondRight).

	component_deltas([], [], []).
	component_deltas([_Name-Left| Lefts], [_Name-Right| Rights], [Delta| Deltas]) :-
		Delta is abs(Left - Right),
		component_deltas(Lefts, Rights, Deltas).

	dominant_delta_index([FirstDelta, SecondDelta, ThirdDelta], 1, FirstDelta, Residual) :-
		FirstDelta >= SecondDelta,
		FirstDelta >= ThirdDelta,
		!,
		Residual is SecondDelta + ThirdDelta.
	dominant_delta_index([FirstDelta, SecondDelta, ThirdDelta], 2, SecondDelta, Residual) :-
		SecondDelta >= ThirdDelta,
		!,
		Residual is FirstDelta + ThirdDelta.
	dominant_delta_index([FirstDelta, SecondDelta, ThirdDelta], 3, ThirdDelta, Residual) :-
		Residual is FirstDelta + SecondDelta.

	smaller_delta_index(FirstDelta, SecondDelta, 1, FirstDelta, SecondDelta) :-
		FirstDelta =< SecondDelta,
		!.
	smaller_delta_index(FirstDelta, SecondDelta, 2, SecondDelta, FirstDelta).

:- end_object.
