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


:- object(invalid_truncated_svd_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(score, continuous).

	example(1, [channel-online, score-1.0]).
	example(2, [channel-retail, score-2.0]).

:- end_object.


:- object(duplicate_attribute_truncated_svd_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	example(1, [f1-1.0, f1-1.1, f2-2.0]).
	example(2, [f1-2.0, f2-4.0]).

:- end_object.


:- object(undeclared_attribute_truncated_svd_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	example(1, [f1-1.0, f2-2.0, junk-9.0]).
	example(2, [f1-2.0, f2-4.0]).

:- end_object.


:- object(duplicate_attribute_declaration_truncated_svd_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f1, continuous).
	attribute_values(f2, continuous).

	example(1, [f1-1.0, f2-2.0]).
	example(2, [f1-2.0, f2-4.0]).

:- end_object.


:- object(leading_zero_feature_measurements,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(f1, continuous).
	attribute_values(f2, continuous).
	attribute_values(f3, continuous).

	example(1, [f1-0.0, f2-1.0, f3-2.0]).
	example(2, [f1-0.0, f2-2.0, f3-4.0]).
	example(3, [f1-0.0, f2-3.0, f3-6.0]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Unit tests for the "truncated_svd_projection" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(truncated_svd_projection).

	cleanup :-
		^^clean_file('test_output.pl').

	test(truncated_svd_learn_2_low_rank_rectangular, deterministic(ground(DimensionReducer))) :-
		truncated_svd_projection::learn(low_rank_rectangular, DimensionReducer).

	test(truncated_svd_learn_2_structure, deterministic(functor(DimensionReducer, truncated_svd_reducer, 4))) :-
		truncated_svd_projection::learn(low_rank_rectangular, DimensionReducer).

	test(truncated_svd_learn_3_custom_options, deterministic((length(Components, 1), SingularValues = [SingularValue], SingularValue > 0.0, [Center, FeatureScaling, Model] == [false, true, truncated_svd_projection]))) :-
		truncated_svd_projection::learn(low_rank_rectangular, truncated_svd_reducer(_Encoders, Components, SingularValues, Diagnostics), [n_components(1), center(false), feature_scaling(true), maximum_iterations(200), tolerance(1.0e-7)]),
		memberchk(options(Options), Diagnostics),
		memberchk(center(Center), Options),
		memberchk(feature_scaling(FeatureScaling), Options),
		memberchk(model(Model), Diagnostics).

	test(truncated_svd_check_dimension_reducer_1, deterministic) :-
		truncated_svd_projection::learn(low_rank_rectangular, DimensionReducer, [n_components(1)]),
		truncated_svd_projection::check_dimension_reducer(DimensionReducer).

	test(truncated_svd_valid_dimension_reducer_1_invalid_singular_values_length, fail) :-
		truncated_svd_projection::learn(low_rank_rectangular, truncated_svd_reducer(Encoders, Components, _SingularValues, Diagnostics), [n_components(1)]),
		truncated_svd_projection::valid_dimension_reducer(truncated_svd_reducer(Encoders, Components, [], Diagnostics)).

	test(truncated_svd_valid_dimension_reducer_1_invalid_singular_values_order, fail) :-
		truncated_svd_projection::learn(low_rank_rectangular, truncated_svd_reducer(Encoders, Components, [First, Second], Diagnostics), [n_components(2)]),
		Ascending is First + Second,
		truncated_svd_projection::valid_dimension_reducer(truncated_svd_reducer(Encoders, Components, [Second, Ascending], Diagnostics)).

	test(truncated_svd_diagnostics_2, deterministic((Iterations >= 0, FinalDelta >= 0.0, [Model, ComponentCount] == [truncated_svd_projection, 1]))) :-
		truncated_svd_projection::learn(low_rank_rectangular, DimensionReducer, [n_components(1)]),
		truncated_svd_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(component_count(ComponentCount), Diagnostics),
		memberchk(singular_values([SingularValue]), Diagnostics),
		assertion(nonvar(SingularValue)),
		memberchk(convergence([Convergence]), Diagnostics),
		assertion(nonvar(Convergence)),
		memberchk(iterations([Iterations]), Diagnostics),
		memberchk(final_delta([FinalDelta]), Diagnostics).

	test(truncated_svd_diagnostics_2_maximum_iterations, deterministic((FinalDelta > 0.0, Convergence == maximum_iterations_exhausted, Iterations == 1, MaximumIterations == 1, Tolerance == 1.0e-12))) :-
		truncated_svd_projection::learn(high_dimensional_measurements, DimensionReducer, [n_components(1), maximum_iterations(1), tolerance(1.0e-12)]),
		truncated_svd_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(convergence([Convergence]), Diagnostics),
		memberchk(iterations([Iterations]), Diagnostics),
		memberchk(final_delta([FinalDelta]), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(tolerance(Tolerance), Options).

	test(truncated_svd_learn_2_exact_rank_request, deterministic((length(Components, 2), SingularValues = [First, Second], First >= Second, Second > 0.0))) :-
		truncated_svd_projection::learn(low_rank_rectangular, truncated_svd_reducer(_Encoders, Components, SingularValues, _Diagnostics), [n_components(2)]).

	test(truncated_svd_learn_2_rank_deficient_request, error(domain_error(component_count, 3-2))) :-
		truncated_svd_projection::learn(low_rank_rectangular, _DimensionReducer, [n_components(3)]).

	test(truncated_svd_transform_3_component_names, deterministic(length(ReducedInstance, 2))) :-
		truncated_svd_projection::learn(high_dimensional_measurements, DimensionReducer, [n_components(2)]),
		truncated_svd_projection::transform(DimensionReducer, [f1-0.9, f2-1.1, f3-1.0, f4-2.0, f5-2.2, f6-2.1], ReducedInstance),
		memberchk(component_1-Component1Score, ReducedInstance),
		memberchk(component_2-Component2Score, ReducedInstance),
		assertion(nonvar(Component1Score)),
		assertion(nonvar(Component2Score)).

	test(truncated_svd_transform_3_distinguishes_rank_rows, deterministic(Score1 =\= Score2)) :-
		truncated_svd_projection::learn(low_rank_rectangular, DimensionReducer, [n_components(1), center(false)]),
		truncated_svd_projection::transform(DimensionReducer, [f1-1.0, f2-0.0, f3-1.0], [component_1-Score1]),
		truncated_svd_projection::transform(DimensionReducer, [f1-0.0, f2-1.0, f3-1.0], [component_1-Score2]).

	test(truncated_svd_learn_2_leading_zero_feature, deterministic((SingularValues = [SingularValue], SingularValue > 0.0))) :-
		truncated_svd_projection::learn(leading_zero_feature_measurements, truncated_svd_reducer(_Encoders, _Components, SingularValues, _Diagnostics), [n_components(1), center(false)]).

	test(truncated_svd_learn_2_singleton_dataset, deterministic((SingularValues = [SingularValue], SingularValue > 0.0))) :-
		truncated_svd_projection::learn(singleton_measurement, truncated_svd_reducer(_Encoders, _Components, SingularValues, _Diagnostics), [n_components(1), center(false)]).

	test(truncated_svd_diagnostics_2_singleton_tolerance, deterministic([Convergence, Iterations, FinalDelta] == [[tolerance], [1], [0.0]])) :-
		truncated_svd_projection::learn(singleton_measurement, DimensionReducer, [n_components(1), center(false)]),
		truncated_svd_projection::diagnostics(DimensionReducer, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_delta(FinalDelta), Diagnostics).

	test(truncated_svd_learn_2_centered_singleton_dataset, error(domain_error(component_count, 1-0))) :-
		truncated_svd_projection::learn(singleton_measurement, _DimensionReducer, [n_components(1), center(true)]).

	test(truncated_svd_valid_dimension_reducer_1_empty_components, deterministic) :-
		truncated_svd_projection::learn(low_rank_rectangular, truncated_svd_reducer(Encoders, _Components, _SingularValues, _Diagnostics), [n_components(1)]),
		Diagnostics = [model(truncated_svd_projection), options([n_components(1), center(false), feature_scaling(false), maximum_iterations(1000), tolerance(1.0e-8)])],
		truncated_svd_projection::valid_dimension_reducer(truncated_svd_reducer(Encoders, [], [], Diagnostics)).

	test(truncated_svd_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		truncated_svd_projection::learn(low_rank_rectangular, DimensionReducer, [n_components(1)]),
		truncated_svd_projection::export_to_clauses(low_rank_rectangular, DimensionReducer, reduced, [Clause]).

	test(truncated_svd_export_to_file_4, deterministic((Reducer = truncated_svd_reducer(_Encoders, Components, SingularValues, _Diagnostics), length(Components, 1), SingularValues = [SingularValue], SingularValue > 0.0))) :-
		^^file_path('test_output.pl', File),
		truncated_svd_projection::learn(low_rank_rectangular, DimensionReducer, [n_components(1)]),
		truncated_svd_projection::export_to_file(low_rank_rectangular, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)}.

	test(truncated_svd_transform_3_exported_functor, deterministic) :-
		^^file_path('test_output.pl', File),
		truncated_svd_projection::learn(low_rank_rectangular, DimensionReducer, [n_components(1)]),
		truncated_svd_projection::export_to_file(low_rank_rectangular, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		truncated_svd_projection::transform(Reducer, [f1-1.0, f2-1.0, f3-2.0], ReducedInstance),
		memberchk(component_1-Component1Score, ReducedInstance),
		assertion(nonvar(Component1Score)).

	test(truncated_svd_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		truncated_svd_projection::learn(low_rank_rectangular, DimensionReducer),
		truncated_svd_projection::print_dimension_reducer(DimensionReducer).

	test(truncated_svd_learn_2_duplicate_training_attribute, error(domain_error(attribute_occurrences, f1))) :-
		truncated_svd_projection::learn(duplicate_attribute_truncated_svd_dataset, _DimensionReducer).

	test(truncated_svd_learn_2_undeclared_training_attribute, error(domain_error(declared_attribute, junk))) :-
		truncated_svd_projection::learn(undeclared_attribute_truncated_svd_dataset, _DimensionReducer).

	test(truncated_svd_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, f1))) :-
		truncated_svd_projection::learn(duplicate_attribute_declaration_truncated_svd_dataset, _DimensionReducer).

	test(truncated_svd_transform_3_duplicate_attribute, error(domain_error(attribute_occurrences, f1))) :-
		truncated_svd_projection::learn(low_rank_rectangular, DimensionReducer, [n_components(1)]),
		truncated_svd_projection::transform(DimensionReducer, [f1-1.0, f1-1.1, f2-0.0, f3-1.0], _ReducedInstance).

	test(truncated_svd_transform_3_undeclared_attribute, error(domain_error(declared_attribute, junk))) :-
		truncated_svd_projection::learn(low_rank_rectangular, DimensionReducer, [n_components(1)]),
		truncated_svd_projection::transform(DimensionReducer, [f1-1.0, f2-0.0, f3-1.0, junk-9.0], _ReducedInstance).

	test(truncated_svd_learn_2_invalid_dataset, error(domain_error(continuous_attribute, channel))) :-
		truncated_svd_projection::learn(invalid_truncated_svd_dataset, _DimensionReducer).

:- end_object.
