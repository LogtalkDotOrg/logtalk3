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

	:- uses(pairs, [
		keys/2
	]).

	:- uses(list, [
		length/2
	]).

	learn(Dataset, sample_dimension_reducer(Encoders, Components, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		findall(Id-AttributeValues, Dataset::example(Id, AttributeValues), Examples),
		^^check_examples_non_empty(Dataset, Examples),
		^^check_example_values(Examples, AttributeNames),
		^^build_encoders(AttributeNames, Examples, Options, Encoders),
		length(AttributeNames, FeatureCount),
		identity_prefix_components(FeatureCount, Components),
		^^base_dimension_reducer_diagnostics(sample_dimension_reducer, AttributeNames, Components, Options, [], Diagnostics).

	identity_prefix_components(FeatureCount, Components) :-
		component_count(FeatureCount, ComponentCount),
		identity_prefix_components(1, ComponentCount, FeatureCount, Components).

	component_count(FeatureCount, 1) :-
		FeatureCount =< 1,
		!.
	component_count(_FeatureCount, 2).

	identity_prefix_components(Index, ComponentCount, _FeatureCount, []) :-
		Index > ComponentCount,
		!.
	identity_prefix_components(Index, ComponentCount, FeatureCount, [Component| Components]) :-
		basis_vector(1, FeatureCount, Index, Component),
		NextIndex is Index + 1,
		identity_prefix_components(NextIndex, ComponentCount, FeatureCount, Components).

	basis_vector(Current, FeatureCount, _Index, []) :-
		Current > FeatureCount,
		!.
	basis_vector(Index, FeatureCount, Index, [1.0| Component]) :-
		!,
		Next is Index + 1,
		basis_vector(Next, FeatureCount, Index, Component).
	basis_vector(Current, FeatureCount, Index, [0.0| Component]) :-
		Next is Current + 1,
		basis_vector(Next, FeatureCount, Index, Component).

	print_dimension_reducer_properties(DimensionReducer) :-
		writeq(DimensionReducer), nl.

	example_attribute_values(_-AttributeValues, AttributeValues).

	default_option(feature_scaling(false)).

	valid_option(feature_scaling(FeatureScaling)) :-
		type::valid(boolean, FeatureScaling).

:- end_object.


:- object(target_measurements,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(length, continuous).
	attribute_values(width, continuous).
	attribute_values(weight, continuous).

	target(score).

	example(1, 0.5, [length-1.0, width-2.0, weight-3.0]).
	example(2, 1.5, [length-2.0, width-3.0, weight-5.0]).

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

	test(target_measurements_target_attribute, deterministic(Target == score)) :-
		target_measurements::target(Target).

	test(target_measurements_example_3, deterministic((Score == 1.5, memberchk(weight-5.0, Values)))) :-
		target_measurements::example(2, Score, Values).

	test(sample_dimension_reducer_learn_2, deterministic(sample_dimension_reducer::valid_dimension_reducer(DimensionReducer))) :-
		sample_dimension_reducer::learn(correlated_plane, DimensionReducer).

	test(sample_dimension_reducer_transform_3, deterministic(ReducedInstance == [component_1-(-1.75), component_2-(-3.5)])) :-
		sample_dimension_reducer::learn(correlated_plane, DimensionReducer),
		sample_dimension_reducer::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0], ReducedInstance).

	test(sample_dimension_reducer_diagnostics_2, deterministic((memberchk(model(sample_dimension_reducer), Diagnostics), memberchk(options([feature_scaling(false)]), Diagnostics)))) :-
		sample_dimension_reducer::learn(correlated_plane, DimensionReducer),
		sample_dimension_reducer::diagnostics(DimensionReducer, Diagnostics).

	test(sample_dimension_reducer_options_2, deterministic(Options == [feature_scaling(false)])) :-
		sample_dimension_reducer::learn(correlated_plane, DimensionReducer),
		sample_dimension_reducer::dimension_reducer_options(DimensionReducer, Options).

	test(sample_export_to_clauses_4, deterministic(Clause == reduced(DimensionReducer))) :-
		sample_dimension_reducer::learn(correlated_plane, DimensionReducer),
		sample_dimension_reducer::export_to_clauses(correlated_plane, DimensionReducer, reduced, [Clause]).

	test(sample_export_to_file_4, deterministic(sample_dimension_reducer::valid_dimension_reducer(DimensionReducer))) :-
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
