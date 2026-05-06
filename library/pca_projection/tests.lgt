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


:- object(invalid_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(score, continuous).

	example(1, [channel-online, score-1.0]).
	example(2, [channel-retail, score-2.0]).

:- end_object.


:- object(duplicate_attribute_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, x-1.1, y-2.0]).
	example(2, [x-2.0, y-4.0]).

:- end_object.


:- object(undeclared_attribute_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-2.0, junk-9.0]).
	example(2, [x-2.0, y-4.0]).

:- end_object.


:- object(duplicate_attribute_declaration_pca_dataset,
	implements(dimension_reduction_dataset_protocol)).

	attribute_values(x, continuous).
	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-1.0, y-2.0]).
	example(2, [x-2.0, y-4.0]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Unit tests for the "pca_projection" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(pca_projection).

	cleanup :-
		^^clean_file('test_output.pl').

	test(pca_learn_2_correlated_plane, deterministic(ground(DimensionReducer))) :-
		pca_projection::learn(correlated_plane, DimensionReducer).

	test(pca_learn_2_structure, deterministic(functor(DimensionReducer, pca_reducer, 4))) :-
		pca_projection::learn(correlated_plane, DimensionReducer).

	test(pca_learn_3_custom_options, deterministic) :-
		pca_projection::learn(correlated_plane, pca_reducer(_Encoders, Components, ExplainedVariances, Diagnostics), [n_components(1), feature_scaling(false), maximum_iterations(200), tolerance(1.0e-7)]),
		assertion(ground(Components)),
		assertion(length(Components, 1)),
		ExplainedVariances = [Variance],
		assertion(Variance > 0.0),
		memberchk(options(Options), Diagnostics),
		assertion(ground(Options)),
		assertion(memberchk(n_components(1), Options)),
		assertion(memberchk(feature_scaling(false), Options)),
		assertion(memberchk(maximum_iterations(200), Options)),
		assertion(memberchk(tolerance(1.0e-7), Options)),
		assertion(memberchk(model(pca_projection), Diagnostics)).

	test(pca_check_dimension_reducer_1, deterministic) :-
		pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		pca_projection::check_dimension_reducer(DimensionReducer).

	test(pca_diagnostics_2, deterministic) :-
		pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		pca_projection::diagnostics(DimensionReducer, Diagnostics),
		assertion(memberchk(model(pca_projection), Diagnostics)),
		assertion(memberchk(component_count(1), Diagnostics)),
		assertion(memberchk(explained_variances([_]), Diagnostics)).

	test(pca_learn_2_component_count_exceeds_feature_count, error(domain_error(component_count, 4-3))) :-
		pca_projection::learn(correlated_plane, _DimensionReducer, [n_components(4)]).

	test(pca_transform_3_component_names, deterministic) :-
		pca_projection::learn(high_dimensional_measurements, DimensionReducer),
		pca_projection::transform(DimensionReducer, [f1-0.9, f2-1.1, f3-1.0, f4-2.0, f5-2.2, f6-2.1], ReducedInstance),
		assertion(length(ReducedInstance, 2)),
		assertion(memberchk(component_1-_, ReducedInstance)),
		assertion(memberchk(component_2-_, ReducedInstance)).

	test(pca_transform_3_monotonic_component, deterministic(Score1 < Score2)) :-
		pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		pca_projection::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0], [component_1-Score1]),
		pca_projection::transform(DimensionReducer, [x-4.5, y-9.0, z-13.7], [component_1-Score2]).

	test(pca_learn_2_anti_correlated_plane_regression, deterministic) :-
		pca_projection::learn(anti_correlated_plane, DimensionReducer, [n_components(1), feature_scaling(false)]),
		pca_projection::transform(DimensionReducer, [x-1.0, y-(-1.0)], [component_1-Score1]),
		pca_projection::transform(DimensionReducer, [x-(-1.0), y-1.0], [component_1-Score2]),
		assertion(Score1 =\= 0.0),
		assertion(Score2 =\= 0.0),
		assertion(Score1 * Score2 < 0.0).

	test(pca_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		pca_projection::export_to_clauses(correlated_plane, DimensionReducer, reduced, [Clause]).

	test(pca_export_to_file_4, deterministic) :-
		^^file_path('test_output.pl', File),
		pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		pca_projection::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		Reducer = pca_reducer(_Encoders, Components, ExplainedVariances, _Diagnostics),
		assertion(ground(Components)),
		assertion(length(Components, 1)),
		ExplainedVariances = [Variance],
		assertion(Variance > 0.0).

	test(pca_transform_3_exported_functor, deterministic(memberchk(component_1-_, ReducedInstance))) :-
		^^file_path('test_output.pl', File),
		pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		pca_projection::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		pca_projection::transform(Reducer, [x-1.0, y-2.0, z-3.0], ReducedInstance).

	test(pca_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		pca_projection::learn(correlated_plane, DimensionReducer),
		pca_projection::print_dimension_reducer(DimensionReducer).

	test(pca_learn_2_singleton_dataset, error(domain_error(minimum_number_of_examples, 1))) :-
		pca_projection::learn(singleton_measurement, _DimensionReducer).

	test(pca_learn_2_duplicate_training_attribute, error(domain_error(attribute_occurrences, x))) :-
		pca_projection::learn(duplicate_attribute_pca_dataset, _DimensionReducer).

	test(pca_learn_2_undeclared_training_attribute, error(domain_error(declared_attribute, junk))) :-
		pca_projection::learn(undeclared_attribute_pca_dataset, _DimensionReducer).

	test(pca_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, x))) :-
		pca_projection::learn(duplicate_attribute_declaration_pca_dataset, _DimensionReducer).

	test(pca_transform_3_duplicate_attribute, error(domain_error(attribute_occurrences, x))) :-
		pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		pca_projection::transform(DimensionReducer, [x-1.0, x-1.1, y-2.0, z-3.0], _ReducedInstance).

	test(pca_transform_3_undeclared_attribute, error(domain_error(declared_attribute, junk))) :-
		pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		pca_projection::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0, junk-9.0], _ReducedInstance).

	test(pca_learn_2_invalid_dataset, error(domain_error(continuous_attribute, channel))) :-
		pca_projection::learn(invalid_pca_dataset, _DimensionReducer).

:- end_object.
