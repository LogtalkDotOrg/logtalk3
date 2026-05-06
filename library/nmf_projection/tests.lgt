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


:- object(invalid_nmf_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(score, continuous).

	example(1, [channel-online, score-1.0]).
	example(2, [channel-retail, score-2.0]).

:- end_object.


:- object(negative_nmf_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	example(1, [f1-1.0, f2-2.0]).
	example(2, [f1-(-1.0), f2-0.5]).

:- end_object.


:- object(duplicate_attribute_nmf_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	example(1, [f1-1.0, f1-1.1, f2-2.0]).
	example(2, [f1-2.0, f2-4.0]).

:- end_object.


:- object(undeclared_attribute_nmf_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	example(1, [f1-1.0, f2-2.0, junk-9.0]).
	example(2, [f1-2.0, f2-4.0]).

:- end_object.


:- object(duplicate_attribute_declaration_nmf_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	example(1, [f1-1.0, f2-2.0]).
	example(2, [f1-2.0, f2-4.0]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Unit tests for the "nmf_projection" library.'
	]).

	:- uses(lgtunit, [
		assertion/1, tolerance_equal/4
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(nmf_projection).

	cleanup :-
		^^clean_file('test_output.pl').

	test(nmf_learn_2_parts_based_measurements, deterministic(ground(DimensionReducer))) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer).

	test(nmf_learn_2_structure, deterministic(functor(DimensionReducer, nmf_reducer, 3))) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer).

	test(nmf_learn_3_custom_options, deterministic) :-
		nmf_projection::learn(parts_based_measurements, nmf_reducer(_Encoders, Components, Diagnostics), [n_components(1), center(false), feature_scaling(true), maximum_iterations(250), tolerance(1.0e-7)]),
		assertion(ground(Components)),
		assertion(length(Components, 1)),
		memberchk(options(Options), Diagnostics),
		assertion(ground(Options)),
		assertion(memberchk(n_components(1), Options)),
		assertion(memberchk(center(false), Options)),
		assertion(memberchk(feature_scaling(true), Options)),
		assertion(memberchk(model(nmf_projection), Diagnostics)).

	test(nmf_check_dimension_reducer_1, deterministic) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2)]),
		nmf_projection::check_dimension_reducer(DimensionReducer).

	test(nmf_valid_dimension_reducer_1_invalid_negative_component, fail) :-
		nmf_projection::learn(parts_based_measurements, nmf_reducer(Encoders, [_Component| Components], Diagnostics), [n_components(2)]),
		nmf_projection::valid_dimension_reducer(nmf_reducer(Encoders, [[-1.0, 0.0, 0.0, 0.0]| Components], Diagnostics)).

	test(nmf_diagnostics_2, deterministic) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2)]),
		nmf_projection::diagnostics(DimensionReducer, Diagnostics),
		assertion(memberchk(model(nmf_projection), Diagnostics)),
		assertion(memberchk(component_count(2), Diagnostics)),
		assertion(memberchk(convergence(_), Diagnostics)),
		memberchk(iterations(Iterations), Diagnostics),
		assertion(ground(Iterations)),
		assertion(Iterations >= 1),
		memberchk(final_delta(FinalDelta), Diagnostics),
		assertion(FinalDelta >= 0.0),
		memberchk(reconstruction_error(ReconstructionError), Diagnostics),
		assertion(ReconstructionError >= 0.0).

	test(nmf_diagnostics_2_maximum_iterations, deterministic) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2), maximum_iterations(1), tolerance(1.0e-12)]),
		nmf_projection::diagnostics(DimensionReducer, Diagnostics),
		assertion(memberchk(convergence(maximum_iterations_exhausted), Diagnostics)),
		assertion(memberchk(iterations(1), Diagnostics)),
		memberchk(final_delta(FinalDelta), Diagnostics),
		assertion(FinalDelta > 0.0),
		memberchk(options(Options), Diagnostics),
		assertion(ground(Options)),
		assertion(memberchk(maximum_iterations(1), Options)),
		assertion(memberchk(tolerance(1.0e-12), Options)).

	test(nmf_diagnostics_2_tolerance_convergence, deterministic) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2), maximum_iterations(25), tolerance(10.0)]),
		nmf_projection::diagnostics(DimensionReducer, Diagnostics),
		assertion(memberchk(convergence(tolerance), Diagnostics)),
		assertion(memberchk(iterations(1), Diagnostics)),
		memberchk(final_delta(FinalDelta), Diagnostics),
		assertion(FinalDelta =< 10.0),
		memberchk(options(Options), Diagnostics),
		assertion(ground(Options)),
		assertion(memberchk(maximum_iterations(25), Options)),
		assertion(memberchk(tolerance(10.0), Options)).

	test(nmf_diagnostics_2_respects_training_iterations, deterministic) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2), maximum_iterations(2), tolerance(1.0e-30)]),
		nmf_projection::diagnostics(DimensionReducer, Diagnostics),
		assertion(memberchk(convergence(maximum_iterations_exhausted), Diagnostics)),
		assertion(memberchk(iterations(2), Diagnostics)),
		memberchk(final_delta(FinalDelta), Diagnostics),
		assertion(FinalDelta > 0.0),
		memberchk(options(Options), Diagnostics),
		assertion(ground(Options)),
		assertion(memberchk(maximum_iterations(2), Options)),
		assertion(memberchk(tolerance(1.0e-30), Options)).

	test(nmf_transform_3_component_names, deterministic) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2)]),
		nmf_projection::transform(DimensionReducer, [f1-3.0, f2-0.0, f3-1.5, f4-0.0], ReducedInstance),
		assertion(length(ReducedInstance, 2)),
		assertion(memberchk(component_1-_, ReducedInstance)),
		assertion(memberchk(component_2-_, ReducedInstance)).

	test(nmf_transform_3_distinguishes_parts, deterministic) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2)]),
		nmf_projection::transform(DimensionReducer, [f1-3.0, f2-0.0, f3-1.5, f4-0.0], [component_1-FirstA, component_2-SecondA]),
		nmf_projection::transform(DimensionReducer, [f1-0.0, f2-3.0, f3-0.0, f4-1.5], [component_1-FirstB, component_2-SecondB]),
		assertion(FirstA > SecondA),
		assertion(FirstB < SecondB).

	test(nmf_learn_2_singleton_dataset, deterministic) :-
		nmf_projection::learn(singleton_measurement, nmf_reducer(_Encoders, _Components, Diagnostics), [n_components(1)]),
		assertion(memberchk(model(nmf_projection), Diagnostics)),
		assertion(memberchk(component_count(1), Diagnostics)).

	test(nmf_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2)]),
		nmf_projection::export_to_clauses(parts_based_measurements, DimensionReducer, reduced, [Clause]).

	test(nmf_export_to_file_4, deterministic) :-
		^^file_path('test_output.pl', File),
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2)]),
		nmf_projection::export_to_file(parts_based_measurements, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		Reducer = nmf_reducer(_Encoders, Components, _Diagnostics),
		assertion(ground(Components)),
		assertion(length(Components, 2)).

	test(nmf_transform_3_exported_functor, deterministic(memberchk(component_1-_, ReducedInstance))) :-
		^^file_path('test_output.pl', File),
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2)]),
		nmf_projection::export_to_file(parts_based_measurements, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		nmf_projection::transform(Reducer, [f1-3.0, f2-0.0, f3-1.5, f4-0.0], ReducedInstance).

	test(nmf_transform_3_feature_scaling_true_exported_functor, deterministic) :-
		^^file_path('test_output.pl', File),
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2), feature_scaling(true)]),
		nmf_projection::transform(DimensionReducer, [f1-3.0, f2-0.0, f3-1.5, f4-0.0], ReducedInstance),
		nmf_projection::export_to_file(parts_based_measurements, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		nmf_projection::transform(Reducer, [f1-3.0, f2-0.0, f3-1.5, f4-0.0], ExportedReducedInstance),
		ReducedInstance = [component_1-FirstWeight, component_2-SecondWeight],
		ExportedReducedInstance = [component_1-ExportedFirstWeight, component_2-ExportedSecondWeight],
		assertion(tolerance_equal(FirstWeight, ExportedFirstWeight, 1.0e-12, 1.0e-12)),
		assertion(tolerance_equal(SecondWeight, ExportedSecondWeight, 1.0e-12, 1.0e-12)).

	test(nmf_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		nmf_projection::learn(parts_based_measurements, DimensionReducer),
		nmf_projection::print_dimension_reducer(DimensionReducer).

	test(nmf_learn_2_duplicate_training_attribute, error(domain_error(attribute_occurrences, f1))) :-
		nmf_projection::learn(duplicate_attribute_nmf_dataset, _DimensionReducer).

	test(nmf_learn_2_undeclared_training_attribute, error(domain_error(declared_attribute, junk))) :-
		nmf_projection::learn(undeclared_attribute_nmf_dataset, _DimensionReducer).

	test(nmf_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, f1))) :-
		nmf_projection::learn(duplicate_attribute_declaration_nmf_dataset, _DimensionReducer).

	test(nmf_transform_3_duplicate_attribute, error(domain_error(attribute_occurrences, f1))) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2)]),
		nmf_projection::transform(DimensionReducer, [f1-3.0, f1-3.1, f2-0.0, f3-1.5, f4-0.0], _ReducedInstance).

	test(nmf_transform_3_undeclared_attribute, error(domain_error(declared_attribute, junk))) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2)]),
		nmf_projection::transform(DimensionReducer, [f1-3.0, f2-0.0, f3-1.5, f4-0.0, junk-9.0], _ReducedInstance).

	test(nmf_transform_3_negative_attribute, error(domain_error(non_negative_attribute, f1-(-1.0)))) :-
		nmf_projection::learn(parts_based_measurements, DimensionReducer, [n_components(2)]),
		nmf_projection::transform(DimensionReducer, [f1-(-1.0), f2-0.0, f3-1.5, f4-0.0], _ReducedInstance).

	test(nmf_learn_2_invalid_dataset, error(domain_error(continuous_attribute, channel))) :-
		nmf_projection::learn(invalid_nmf_dataset, _DimensionReducer).

	test(nmf_learn_2_negative_dataset, error(domain_error(non_negative_attribute, f1-(-1.0)))) :-
		nmf_projection::learn(negative_nmf_dataset, _DimensionReducer).

	test(nmf_learn_2_component_count_exceeds_shape, error(domain_error(component_count, 5-4))) :-
		nmf_projection::learn(parts_based_measurements, _DimensionReducer, [n_components(5)]).

:- end_object.
