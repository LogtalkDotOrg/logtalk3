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


:- object(sample_dimension_reducer,
	imports(dimension_reducer_common)).

	:- uses(list, [
		member/2, memberchk/2
	]).

	learn(_Dataset, sample_dimension_reducer([x, y], [component_1, component_2]), _Options).

	transform(sample_dimension_reducer(Attributes, Components), Instance, ReducedInstance) :-
		findall(Component-Value,
			(
				member(Component, Components),
				component_value(Component, Attributes, Instance, Value)
			),
			ReducedInstance).

	component_value(component_1, [Attribute1| _], Instance, Value) :-
		memberchk(Attribute1-Value, Instance).
	component_value(component_2, [_| Attributes], Instance, Value) :-
		Attributes = [Attribute2| _],
		memberchk(Attribute2-Value, Instance).

	print_dimension_reducer_properties(DimensionReducer) :-
		writeq(DimensionReducer), nl.

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-20,
		comment is 'Smoke tests for the "dimension_reduction_protocols" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cleanup :-
		^^clean_file('test_output.pl').

	test(correlated_plane_attribute_values, deterministic(Attributes == [x-continuous, y-continuous, z-continuous])) :-
		findall(Attribute-Values, correlated_plane::attribute_values(Attribute, Values), Attributes).

	test(correlated_plane_examples_count, deterministic(Count == 8)) :-
		findall(Id, correlated_plane::example(Id, _Values), Ids),
		length(Ids, Count).

	test(high_dimensional_measurements_examples_shape, deterministic((memberchk(f1-0.9, Values), memberchk(f6-2.1, Values)))) :-
		high_dimensional_measurements::example(1, Values).

	test(labeled_measurements_class_values, deterministic((Class == label, Values == [alpha, beta, gamma]))) :-
		labeled_measurements::class(Class),
		labeled_measurements::class_values(Values).

	test(labeled_measurements_unsupervised_example_2, deterministic(memberchk(length-5.1, Values))) :-
		labeled_measurements::example(1, Values).

	test(labeled_measurements_supervised_example_3, deterministic((Label == gamma, memberchk(weight-2.3, Values)))) :-
		labeled_measurements::example(8, Label, Values).

	test(sample_dimension_reducer_learn_2, deterministic(DimensionReducer == sample_dimension_reducer([x, y], [component_1, component_2]))) :-
		sample_dimension_reducer::learn(correlated_plane, DimensionReducer).

	test(sample_dimension_reducer_transform_3, deterministic(ReducedInstance == [component_1-1.0, component_2-2.0])) :-
		sample_dimension_reducer::learn(correlated_plane, DimensionReducer),
		sample_dimension_reducer::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0], ReducedInstance).

	test(sample_export_to_clauses_4, deterministic(Clause == reduced(sample_dimension_reducer([x, y], [component_1, component_2])))) :-
		sample_dimension_reducer::learn(correlated_plane, DimensionReducer),
		sample_dimension_reducer::export_to_clauses(correlated_plane, DimensionReducer, reduced, [Clause]).

	test(sample_export_to_file_4, deterministic(DimensionReducer == sample_dimension_reducer([x, y], [component_1, component_2]))) :-
		^^file_path('test_output.pl', File),
		sample_dimension_reducer::learn(correlated_plane, DimensionReducer),
		sample_dimension_reducer::export_to_file(correlated_plane, DimensionReducer, reducer, File),
		logtalk_load(File),
		{reducer(DimensionReducer)}.

	test(sample_dimension_reducer_print_dimension_reducer_1, deterministic) :-
		^^suppress_text_output,
		sample_dimension_reducer::learn(correlated_plane, DimensionReducer),
		sample_dimension_reducer::print_dimension_reducer(DimensionReducer).

:- end_object.
