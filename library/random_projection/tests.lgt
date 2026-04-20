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


:- object(invalid_random_projection_dataset,
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
		comment is 'Unit tests for the "random_projection" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(random_projection).

	cleanup :-
		^^clean_file('test_output.pl').

	test(random_projection_learn_2_correlated_plane, deterministic(ground(DimensionReducer))) :-
		random_projection::learn(correlated_plane, DimensionReducer).

	test(random_projection_learn_2_structure, deterministic(functor(DimensionReducer, random_projection_reducer, 3))) :-
		random_projection::learn(correlated_plane, DimensionReducer).

	test(random_projection_learn_3_custom_options, deterministic((length(Components, 1), memberchk(n_components(1), Options), memberchk(feature_scaling(off), Options), memberchk(random_seed(17), Options)))) :-
		random_projection::learn(correlated_plane, random_projection_reducer(_Encoders, Components, Options), [n_components(1), feature_scaling(off), random_seed(17)]).

	test(random_projection_transform_3_component_names, deterministic((length(ReducedInstance, 2), memberchk(component_1-_, ReducedInstance), memberchk(component_2-_, ReducedInstance)))) :-
		random_projection::learn(high_dimensional_measurements, DimensionReducer, [random_seed(11)]),
		random_projection::transform(DimensionReducer, [f1-0.9, f2-1.1, f3-1.0, f4-2.0, f5-2.2, f6-2.1], ReducedInstance).

	test(random_projection_same_seed_reproducible, deterministic(DimensionReducer1 == DimensionReducer2)) :-
		random_projection::learn(correlated_plane, DimensionReducer1, [random_seed(19)]),
		random_projection::learn(correlated_plane, DimensionReducer2, [random_seed(19)]).

	test(random_projection_different_seed_changes_components, deterministic(Components1 \== Components2)) :-
		random_projection::learn(correlated_plane, random_projection_reducer(_Encoders1, Components1, _Options1), [random_seed(17)]),
		random_projection::learn(correlated_plane, random_projection_reducer(_Encoders2, Components2, _Options2), [random_seed(23)]).

	test(random_projection_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		random_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
		random_projection::export_to_clauses(correlated_plane, DimensionReducer, reduced, [Clause]).

	test(random_projection_export_to_file_4, deterministic((Reducer = random_projection_reducer(_Encoders, Components, _Options), length(Components, 1)))) :-
		^^file_path('test_output.pl', File),
		random_projection::learn(correlated_plane, DimensionReducer, [n_components(1), random_seed(29)]),
		random_projection::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)}.

	test(random_projection_transform_3_exported_functor, deterministic(memberchk(component_1-_, ReducedInstance))) :-
		^^file_path('test_output.pl', File),
		random_projection::learn(correlated_plane, DimensionReducer, [n_components(1), random_seed(29)]),
		random_projection::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(Reducer)},
		random_projection::transform(Reducer, [x-1.0, y-2.0, z-3.0], ReducedInstance).

	test(random_projection_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		random_projection::learn(correlated_plane, DimensionReducer),
		random_projection::print_dimension_reducer(DimensionReducer).

	test(random_projection_learn_2_invalid_dataset, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		random_projection::learn(invalid_random_projection_dataset, _DimensionReducer).

:- end_object.
