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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-20,
		comment is 'Unit tests for the "pca" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(pca).

	cleanup :-
		^^clean_file('test_output.pl').

	test(pca_learn_2_correlated_plane, deterministic(ground(DimensionReducer))) :-
		pca::learn(correlated_plane, DimensionReducer).

	test(pca_learn_2_structure, deterministic(functor(DimensionReducer, pca_reducer, 4))) :-
		pca::learn(correlated_plane, DimensionReducer).

	test(pca_learn_3_custom_options, deterministic((length(Components, 1), ExplainedVariances = [Variance], Variance > 0.0, memberchk(n_components(1), Options), memberchk(feature_scaling(off), Options)))) :-
		pca::learn(correlated_plane, pca_reducer(_Encoders, Components, ExplainedVariances, Options), [n_components(1), feature_scaling(off), maximum_iterations(200), tolerance(1.0e-7)]).

	test(pca_transform_3_component_names, deterministic((length(ReducedInstance, 2), memberchk(component_1-_, ReducedInstance), memberchk(component_2-_, ReducedInstance)))) :-
		pca::learn(high_dimensional_measurements, DimensionReducer),
		pca::transform(DimensionReducer, [f1-0.9, f2-1.1, f3-1.0, f4-2.0, f5-2.2, f6-2.1], ReducedInstance).

	test(pca_transform_3_monotonic_component, deterministic(Score1 < Score2)) :-
		pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		pca::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0], [component_1-Score1]),
		pca::transform(DimensionReducer, [x-4.5, y-9.0, z-13.7], [component_1-Score2]).

	test(pca_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		pca::export_to_clauses(correlated_plane, DimensionReducer, reduced, [Clause]).

	test(pca_export_to_file_4, deterministic((Reducer = pca_reducer(_Encoders, Components, ExplainedVariances, _Options), length(Components, 1), ExplainedVariances = [Variance], Variance > 0.0))) :-
		^^file_path('test_output.pl', File),
		pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		pca::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)}.

	test(pca_transform_3_exported_functor, deterministic(memberchk(component_1-_, ReducedInstance))) :-
		^^file_path('test_output.pl', File),
		pca::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		pca::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		pca::transform(Reducer, [x-1.0, y-2.0, z-3.0], ReducedInstance).

	test(pca_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		pca::learn(correlated_plane, DimensionReducer),
		pca::print_dimension_reducer(DimensionReducer).

	test(pca_learn_2_invalid_dataset, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		pca::learn(invalid_pca_dataset, _DimensionReducer).

:- end_object.
