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


:- category(dimension_reducer_common,
	implements(dimension_reducer_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-20,
		comment is 'Shared predicates for dimension reducer learning defaults, dataset helpers, transformation, export, and printing.'
	]).

	:- protected(dimension_reducer_data/3).
	:- mode(dimension_reducer_data(+compound, -list, -list), one).
	:- info(dimension_reducer_data/3, [
		comment is 'Hook predicate that importing dimension reducer implementations must define in order to expose the learned encoders and projection components.',
		argnames is ['DimensionReducer', 'Encoders', 'Components']
	]).

	:- protected(print_dimension_reducer_properties/1).
	:- mode(print_dimension_reducer_properties(+compound), one).
	:- info(print_dimension_reducer_properties/1, [
		comment is 'Hook predicate that importing dimension reducer implementations must define in order to print the learned reducer in a human-readable form.',
		argnames is ['DimensionReducer']
	]).

	:- protected(example_attribute_values/2).
	:- mode(example_attribute_values(+compound, -list(pair)), one).
	:- info(example_attribute_values/2, [
		comment is 'Hook predicate that importing dimension reducer implementations must define in order to extract example attribute values from their local training example representation.',
		argnames is ['Example', 'AttributeValues']
	]).

	:- protected(dataset_attributes/2).
	:- mode(dataset_attributes(+object_identifier, -list(pair)), one).
	:- info(dataset_attributes/2, [
		comment is 'Collects the dataset attribute declarations as `Attribute-Values` pairs.',
		argnames is ['Dataset', 'Attributes']
	]).

	:- protected(check_continuous_attributes/1).
	:- mode(check_continuous_attributes(+list(pair)), one).
	:- info(check_continuous_attributes/1, [
		comment is 'Checks that all declared dataset attributes are continuous.',
		argnames is ['Attributes']
	]).

	:- protected(check_examples_non_empty/2).
	:- mode(check_examples_non_empty(+object_identifier, +list), one).
	:- info(check_examples_non_empty/2, [
		comment is 'Checks that a training example collection is not empty.',
		argnames is ['Dataset', 'Examples']
	]).

	:- protected(check_example_values/2).
	:- mode(check_example_values(+list, +list(atom)), one).
	:- info(check_example_values/2, [
		comment is 'Checks that all example attribute values are present and numeric for the declared attributes.',
		argnames is ['Examples', 'AttributeNames']
	]).

	:- protected(check_example_attributes/2).
	:- mode(check_example_attributes(+list(atom), +list(pair)), one).
	:- info(check_example_attributes/2, [
		comment is 'Checks that a single example contains numeric values for all declared attributes.',
		argnames is ['AttributeNames', 'AttributeValues']
	]).

	:- protected(attribute_value/3).
	:- mode(attribute_value(+atom, +list(pair), -term), one).
	:- info(attribute_value/3, [
		comment is 'Looks up an attribute value in a list of `Attribute-Value` pairs.',
		argnames is ['Attribute', 'AttributeValues', 'Value']
	]).

	:- protected(build_encoders/4).
	:- mode(build_encoders(+list(atom), +list, +list(compound), -list(compound)), one).
	:- info(build_encoders/4, [
		comment is 'Builds continuous feature encoders by computing per-attribute centering and optional scaling statistics.',
		argnames is ['AttributeNames', 'Examples', 'Options', 'Encoders']
	]).

	:- protected(known_attribute_values/3).
	:- mode(known_attribute_values(+atom, +list, -list(number)), one).
	:- info(known_attribute_values/3, [
		comment is 'Collects the known numeric values for a given attribute across the training examples.',
		argnames is ['Attribute', 'Examples', 'Values']
	]).

	:- protected(encode_instance/3).
	:- mode(encode_instance(+list(compound), +list(pair), -list(number)), one).
	:- info(encode_instance/3, [
		comment is 'Encodes an instance using the learned continuous attribute encoders.',
		argnames is ['Encoders', 'AttributeValues', 'Features']
	]).

	:- protected(project_components/4).
	:- mode(project_components(+list(list(number)), +list(number), +integer, -list(pair)), one).
	:- info(project_components/4, [
		comment is 'Projects encoded features onto the learned components and returns `component_N-Score` pairs.',
		argnames is ['Components', 'Features', 'Index', 'ReducedInstance']
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		length/2, member/2
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	learn(Dataset, DimensionReducer) :-
		::learn(Dataset, DimensionReducer, []).

	transform(DimensionReducer, Instance, ReducedInstance) :-
		::dimension_reducer_data(DimensionReducer, Encoders, Components),
		::encode_instance(Encoders, Instance, Features),
		::project_components(Components, Features, 1, ReducedInstance).

	export_to_clauses(_Dataset, DimensionReducer, Functor, [Clause]) :-
		Clause =.. [Functor, DimensionReducer].

	export_to_file(Dataset, DimensionReducer, Functor, File) :-
		::export_to_clauses(Dataset, DimensionReducer, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Dataset, DimensionReducer, Functor, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	print_dimension_reducer(DimensionReducer) :-
		::print_dimension_reducer_properties(DimensionReducer).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		).

	check_continuous_attributes([]).
	check_continuous_attributes([Attribute-Values| Attributes]) :-
		(	Values == continuous ->
			true
		;	domain_error(continuous_attribute(Attribute), Values)
		),
		check_continuous_attributes(Attributes).

	check_examples_non_empty(Dataset, Examples) :-
		(   Examples == [] ->
			domain_error(non_empty_dataset, Dataset)
		;   true
		).

	check_example_values([], _AttributeNames).
	check_example_values([Example| Examples], AttributeNames) :-
		::example_attribute_values(Example, AttributeValues),
		::check_example_attributes(AttributeNames, AttributeValues),
		check_example_values(Examples, AttributeNames).

	check_example_attributes([], _AttributeValues).
	check_example_attributes([Attribute| Attributes], AttributeValues) :-
		attribute_value(Attribute, AttributeValues, Value),
		(   nonvar(Value) ->
			true
		;   instantiation_error
		),
		(   number(Value) ->
			true
		;   type_error(number, Value)
		),
		check_example_attributes(Attributes, AttributeValues).

	attribute_value(Attribute, AttributeValues, Value) :-
		(   member(Attribute-Value, AttributeValues) ->
			true
		;   existence_error(attribute, Attribute)
		).

	build_encoders([], _Examples, _Options, []).
	build_encoders([Attribute| Attributes], Examples, Options, [continuous(Attribute, Mean, Scale)| Encoders]) :-
		continuous_stats(Attribute, Examples, Options, Mean, Scale),
		build_encoders(Attributes, Examples, Options, Encoders).

	continuous_stats(Attribute, Examples, Options, Mean, Scale) :-
		known_attribute_values(Attribute, Examples, Values),
		arithmetic_mean(Values, Mean),
		^^option(feature_scaling(FeatureScaling), Options),
		(   FeatureScaling == on ->
			length(Values, Count),
			(   Count > 1 ->
				variance(Values, Variance)
			;   Variance = 0.0
			),
			(   Variance > 0.0 ->
				Scale is sqrt(Variance)
			;   Scale = 1.0
			)
		;   Scale = 1.0
		).

	known_attribute_values(_Attribute, [], []).
	known_attribute_values(Attribute, [Example| Examples], [Value| Values]) :-
		::example_attribute_values(Example, AttributeValues),
		attribute_value(Attribute, AttributeValues, Value),
		known_attribute_values(Attribute, Examples, Values).

	encode_instance([], _AttributeValues, []).
	encode_instance([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature| Features]) :-
		attribute_value(Attribute, AttributeValues, Value),
		(   nonvar(Value) ->
			true
		;   instantiation_error
		),
		(   number(Value) ->
			true
		;   type_error(number, Value)
		),
		Feature is (Value - Mean) / Scale,
		encode_instance(Encoders, AttributeValues, Features).

	project_components([], _Features, _Index, []).
	project_components([Component| Components], Features, Index, [ComponentName-Score| ReducedInstance]) :-
		dot_product(Component, Features, Score),
		component_name(Index, ComponentName),
		NextIndex is Index + 1,
		project_components(Components, Features, NextIndex, ReducedInstance).

	component_name(Index, Name) :-
		atomic_concat(component_, Index, Name).

	write_comment_header(_Dataset, _DimensionReducer, Functor, Stream) :-
		Template =.. [Functor, 'Reducer'],
		format(Stream, '% ~w~n', [Template]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

:- end_category.
