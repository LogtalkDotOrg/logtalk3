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
		date is 2026-05-04,
		comment is 'Shared predicates for dimension reducer learning defaults, dataset helpers, transformation, export, and printing.'
	]).

	:- protected(check_component_count/3).
	:- mode(check_component_count(+integer, +integer, -integer), one).
	:- info(check_component_count/3, [
		comment is 'Checks that a requested component count does not exceed the supported maximum and returns the accepted count.',
		argnames is ['RequestedComponentCount', 'MaxComponentCount', 'ComponentCount']
	]).

	:- protected(dimension_reducer_data/3).
	:- mode(dimension_reducer_data(+compound, -list, -list), one).
	:- info(dimension_reducer_data/3, [
		comment is 'Default hook predicate for exposing the learned encoders and projection components from a reducer term. Importing implementations may override it when using a non-standard reducer representation.',
		argnames is ['DimensionReducer', 'Encoders', 'Components']
	]).

	:- protected(dimension_reducer_diagnostics_data/2).
	:- mode(dimension_reducer_diagnostics_data(+compound, -list(compound)), one).
	:- info(dimension_reducer_diagnostics_data/2, [
		comment is 'Default hook predicate for exposing diagnostics metadata from a reducer term. Importing implementations may override it when using a non-standard reducer representation.',
		argnames is ['DimensionReducer', 'Diagnostics']
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

	:- protected(base_dimension_reducer_diagnostics/6).
	:- mode(base_dimension_reducer_diagnostics(+atom, +list(atom), +list, +list(compound), +list(compound), -list(compound)), one).
	:- info(base_dimension_reducer_diagnostics/6, [
		comment is 'Builds common diagnostics metadata terms for a learned reducer and appends reducer-specific diagnostics terms.',
		argnames is ['Model', 'AttributeNames', 'Components', 'Options', 'ExtraDiagnostics', 'Diagnostics']
	]).

	:- protected(preprocessing_diagnostics/3).
	:- mode(preprocessing_diagnostics(+boolean, +list(compound), -list(compound)), one).
	:- info(preprocessing_diagnostics/3, [
		comment is 'Builds shared preprocessing diagnostics metadata from an explicit centering flag and the effective training options.',
		argnames is ['Center', 'Options', 'Preprocessing']
	]).

	:- protected(iterative_dimension_reducer_diagnostics/11).
	:- mode(iterative_dimension_reducer_diagnostics(+atom, +list(atom), +list, +integer, +list(compound), +list(compound), +term, +term, +term, +list(compound), -list(compound)), one).
	:- info(iterative_dimension_reducer_diagnostics/11, [
		comment is 'Builds diagnostics metadata for reducers that report sample counts, iterative convergence terms, and optional leading or trailing reducer-specific diagnostics.',
		argnames is [
			'Model', 'AttributeNames', 'Components', 'SampleCount', 'Options', 'LeadingDiagnostics',
			'Convergence', 'Iterations', 'FinalDelta', 'TrailingDiagnostics', 'Diagnostics'
		]
	]).

	:- protected(component_iteration_diagnostics/4).
	:- mode(component_iteration_diagnostics(+list(compound), -list(atom), -list(integer), -list(number)), one).
	:- info(component_iteration_diagnostics/4, [
		comment is 'Extracts per-component convergence, iteration, and final-delta lists from component diagnostics records.',
		argnames is ['ComponentDiagnostics', 'Convergences', 'IterationCounts', 'FinalDeltas']
	]).

	:- protected(zero_vector_like/2).
	:- mode(zero_vector_like(+list(list(number)), -list(number)), one).
	:- info(zero_vector_like/2, [
		comment is 'Constructs a zero vector matching the length of the first vector in a list of vectors, or returns the empty list when the input is empty.',
		argnames is ['Vectors', 'ZeroVector']
	]).

	:- protected(basis_vector/3).
	:- mode(basis_vector(+integer, +integer, -list(number)), one).
	:- info(basis_vector/3, [
		comment is 'Constructs a canonical basis vector for the requested size and one-based index.',
		argnames is ['Size', 'Index', 'Vector']
	]).

	:- protected(initial_vectors/2).
	:- mode(initial_vectors(+integer, -list(list(number))), one).
	:- info(initial_vectors/2, [
		comment is 'Constructs the default all-ones initial vector followed by canonical basis vectors for the requested size.',
		argnames is ['Size', 'Vectors']
	]).

	:- protected(basis_initial_vectors/3).
	:- mode(basis_initial_vectors(+integer, +integer, -list(list(number))), one).
	:- info(basis_initial_vectors/3, [
		comment is 'Constructs canonical basis vectors from the given one-based index up to the requested size.',
		argnames is ['Index', 'Size', 'Vectors']
	]).

	:- protected(extract_components/5).
	:- mode(extract_components(+list(list(number)), +integer, +list(compound), -list(list(number)), -list(number)), one).
	:- info(extract_components/5, [
		comment is 'Extracts leading positive eigen-components from a numeric matrix using the shared symmetric eigensolver until the requested count or the configured tolerance is reached.',
		argnames is ['Matrix', 'Requested', 'Options', 'Components', 'Eigenvalues']
	]).

	:- protected(known_attribute_values/3).
	:- mode(known_attribute_values(+list, +atom, -list(number)), one).
	:- info(known_attribute_values/3, [
		comment is 'Collects the known numeric values for a given attribute across the training examples.',
		argnames is ['Examples', 'Attribute', 'Values']
	]).

	:- protected(examples_to_rows/3).
	:- mode(examples_to_rows(+list, +list(compound), -list(list(number))), one).
	:- info(examples_to_rows/3, [
		comment is 'Encodes a list of training examples into numeric feature rows using the importing reducer example hook and learned encoders.',
		argnames is ['Examples', 'Encoders', 'Rows']
	]).

	:- protected(encode_instance/3).
	:- mode(encode_instance(+list(compound), +list(pair), -list(number)), one).
	:- info(encode_instance/3, [
		comment is 'Encodes an instance using the learned continuous attribute encoders.',
		argnames is ['Encoders', 'AttributeValues', 'Features']
	]).

	:- protected(encoder_attribute_names/2).
	:- mode(encoder_attribute_names(+list(compound), -list(atom)), one).
	:- info(encoder_attribute_names/2, [
		comment is 'Collects encoder attribute names preserving encoder order.',
		argnames is ['Encoders', 'AttributeNames']
	]).

	:- protected(project_components/4).
	:- mode(project_components(+list(list(number)), +list(number), +integer, -list(pair)), one).
	:- info(project_components/4, [
		comment is 'Projects encoded features onto the learned components and returns `component_N-Score` pairs.',
		argnames is ['Components', 'Features', 'Index', 'ReducedInstance']
	]).

	:- protected(valid_linear_encoders/1).
	:- mode(valid_linear_encoders(+list(compound)), zero_or_one).
	:- info(valid_linear_encoders/1, [
		comment is 'True when a list of encoders only contains valid ``continuous/3`` encoder terms with distinct attributes.',
		argnames is ['Encoders']
	]).

	:- protected(valid_projection_components/2).
	:- mode(valid_projection_components(+list(compound), +list(list(number))), zero_or_one).
	:- info(valid_projection_components/2, [
		comment is 'True when projection components are numeric vectors compatible with the encoder feature dimension.',
		argnames is ['Encoders', 'Components']
	]).

	:- protected(valid_dimension_reducer_metadata/1).
	:- mode(valid_dimension_reducer_metadata(+list(compound)), zero_or_one).
	:- info(valid_dimension_reducer_metadata/1, [
		comment is 'True when diagnostics metadata records the reducer model and effective training options.',
		argnames is ['Diagnostics']
	]).

	:- protected(print_dimension_reducer_details/3).
	:- mode(print_dimension_reducer_details(+list(compound), +list(compound), +list), one).
	:- info(print_dimension_reducer_details/3, [
		comment is 'Prints the common diagnostics, encoders, and component-count lines used by reducer-specific pretty printers.',
		argnames is ['Diagnostics', 'Encoders', 'Components']
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		append/3, last/2, length/2, member/2, memberchk/2, reverse/2
	]).

	:- uses(linear_algebra, [
		basis_vector/3, new_vector/3, symmetric_eigen/4, symmetric_eigen/5
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	learn(Dataset, DimensionReducer) :-
		::learn(Dataset, DimensionReducer, []).

	transform(DimensionReducer, Instance, ReducedInstance) :-
		::check_dimension_reducer(DimensionReducer),
		::dimension_reducer_data(DimensionReducer, Encoders, Components),
		::encode_instance(Encoders, Instance, Features),
		::project_components(Components, Features, 1, ReducedInstance).

	check_dimension_reducer(DimensionReducer) :-
		(	var(DimensionReducer) ->
			instantiation_error
		;   ::dimension_reducer_data(DimensionReducer, Encoders, Components),
			::dimension_reducer_diagnostics_data(DimensionReducer, Diagnostics),
			::valid_linear_encoders(Encoders),
			::valid_projection_components(Encoders, Components),
			::valid_dimension_reducer_metadata(Diagnostics) ->
			true
		;   domain_error(dimension_reducer, DimensionReducer)
		).

	valid_dimension_reducer(DimensionReducer) :-
		catch(::check_dimension_reducer(DimensionReducer), _Error, fail).

	diagnostics(DimensionReducer, Diagnostics) :-
		::dimension_reducer_diagnostics_data(DimensionReducer, Diagnostics).

	diagnostic(DimensionReducer, Diagnostic) :-
		::dimension_reducer_diagnostics_data(DimensionReducer, Diagnostics),
		member(Diagnostic, Diagnostics).

	dimension_reducer_options(DimensionReducer, Options) :-
		::dimension_reducer_diagnostics_data(DimensionReducer, Diagnostics),
		memberchk(options(Options), Diagnostics).

	export_to_clauses(_Dataset, DimensionReducer, Functor, [Clause]) :-
		Clause =.. [Functor, DimensionReducer].

	export_to_file(Dataset, DimensionReducer, Functor, File) :-
		::check_dimension_reducer(DimensionReducer),
		::export_to_clauses(Dataset, DimensionReducer, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Dataset, DimensionReducer, Functor, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	print_dimension_reducer(DimensionReducer) :-
		::check_dimension_reducer(DimensionReducer),
		::print_dimension_reducer_properties(DimensionReducer).

	check_component_count(RequestedComponentCount, MaxComponentCount, RequestedComponentCount) :-
		RequestedComponentCount =< MaxComponentCount,
		!.
	check_component_count(RequestedComponentCount, MaxComponentCount, _ComponentCount) :-
		domain_error(component_count, RequestedComponentCount-MaxComponentCount).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes0
		),
		check_attribute_declarations(Attributes0),
		Attributes = Attributes0.

	check_attribute_declarations([]).
	check_attribute_declarations([Attribute-_Values| Attributes]) :-
		attribute_occurrences(Attributes, Attribute, 0, Count),
		(	Count == 0 ->
			true
		;	domain_error(attribute_declarations, Attribute)
		),
		check_attribute_declarations(Attributes).

	check_continuous_attributes([]).
	check_continuous_attributes([Attribute-Values| Attributes]) :-
		(	Values == continuous ->
			true
		;	domain_error(continuous_attribute, Attribute)
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

	check_example_attributes(AttributeNames, AttributeValues) :-
		check_attribute_bindings(AttributeNames, AttributeValues),
		check_example_attributes_checked(AttributeNames, AttributeValues).

	check_example_attributes_checked([], _AttributeValues).
	check_example_attributes_checked([Attribute| Attributes], AttributeValues) :-
		attribute_value(Attribute, AttributeValues, Value),
		(   nonvar(Value) ->
			true
		;   instantiation_error
		),
		(   number(Value) ->
			true
		;   type_error(number, Value)
		),
		check_example_attributes_checked(Attributes, AttributeValues).

	check_attribute_bindings(AttributeNames, AttributeValues) :-
		check_declared_attribute_bindings(AttributeNames, AttributeValues),
		check_undeclared_attribute_bindings(AttributeValues, AttributeNames).

	check_declared_attribute_bindings([], _AttributeValues).
	check_declared_attribute_bindings([Attribute| Attributes], AttributeValues) :-
		attribute_occurrences(AttributeValues, Attribute, 0, Count),
		(   Count == 1 ->
			true
		;   Count == 0 ->
			existence_error(attribute, Attribute)
		;   domain_error(attribute_occurrences, Attribute)
		),
		check_declared_attribute_bindings(Attributes, AttributeValues).

	check_undeclared_attribute_bindings([], _AttributeNames).
	check_undeclared_attribute_bindings([Attribute-_Value| AttributeValues], AttributeNames) :-
		(   member(Attribute, AttributeNames) ->
			true
		;   domain_error(declared_attribute, Attribute)
		),
		check_undeclared_attribute_bindings(AttributeValues, AttributeNames).

	attribute_occurrences([], _Attribute, Count, Count).
	attribute_occurrences([Attribute-_Value| AttributeValues], Attribute, Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		attribute_occurrences(AttributeValues, Attribute, Count1, Count).
	attribute_occurrences([_OtherAttribute-_Value| AttributeValues], Attribute, Count0, Count) :-
		attribute_occurrences(AttributeValues, Attribute, Count0, Count).

	attribute_value(Attribute, AttributeValues, Value) :-
		(   member(Attribute-Value, AttributeValues) ->
			true
		;   existence_error(attribute, Attribute)
		).

	build_encoders([], _Examples, _Options, []).
	build_encoders([Attribute| Attributes], Examples, Options, [continuous(Attribute, Mean, Scale)| Encoders]) :-
		continuous_stats(Attribute, Examples, Options, Mean, Scale),
		build_encoders(Attributes, Examples, Options, Encoders).

	base_dimension_reducer_diagnostics(Model, AttributeNames, Components, Options, ExtraDiagnostics, Diagnostics) :-
		length(AttributeNames, FeatureCount),
		length(Components, ComponentCount),
		Diagnostics = [
			model(Model),
			options(Options),
			attribute_names(AttributeNames),
			feature_count(FeatureCount),
			component_count(ComponentCount)
			| ExtraDiagnostics
		].

	preprocessing_diagnostics(Center, Options, [center(Center), feature_scaling(FeatureScaling)]) :-
		memberchk(feature_scaling(FeatureScaling), Options).

	iterative_dimension_reducer_diagnostics(Model, AttributeNames, Components, SampleCount, Options, LeadingDiagnostics, Convergence, Iterations, FinalDelta, TrailingDiagnostics, Diagnostics) :-
		length(AttributeNames, FeatureCount),
		length(Components, ComponentCount),
		append(
			LeadingDiagnostics,
			[
				convergence(Convergence),
				iterations(Iterations),
				final_delta(FinalDelta)
				| TrailingDiagnostics
			],
			ExtraDiagnostics
		),
		Diagnostics = [
			model(Model),
			options(Options),
			attribute_names(AttributeNames),
			feature_count(FeatureCount),
			sample_count(SampleCount),
			component_count(ComponentCount)
			| ExtraDiagnostics
		].

	component_iteration_diagnostics([], [], [], []).
	component_iteration_diagnostics([component_diagnostics(Convergence, Iterations, FinalDelta)| ComponentDiagnostics], [Convergence| Convergences], [Iterations| IterationCounts], [FinalDelta| FinalDeltas]) :-
		component_iteration_diagnostics(ComponentDiagnostics, Convergences, IterationCounts, FinalDeltas).

	zero_vector_like([], []).
	zero_vector_like([Vector| _Vectors], ZeroVector) :-
		length(Vector, Size),
		new_vector(Size, 0.0, ZeroVector).

	initial_vectors(Size, [InitialVector| BasisVectors]) :-
		new_vector(Size, 1.0, InitialVector),
		basis_initial_vectors(1, Size, BasisVectors).

	basis_initial_vectors(Index, Size, []) :-
		Index > Size,
		!.
	basis_initial_vectors(Index, Size, [BasisVector| BasisVectors]) :-
		basis_vector(Size, Index, BasisVector),
		NextIndex is Index + 1,
		basis_initial_vectors(NextIndex, Size, BasisVectors).

	extract_components(_Matrix, 0, _Options, [], []) :-
		!.
	extract_components(Matrix, Requested, Options, Components, Eigenvalues) :-
		^^option(tolerance(Tolerance), Options),
		^^option(maximum_iterations(MaximumIterations), Options),
		symmetric_eigen(Matrix, Tolerance, MaximumIterations, OrderedComponents, OrderedEigenvalues),
		take_positive_components(OrderedComponents, OrderedEigenvalues, Requested, Tolerance, Components, Eigenvalues).

	take_positive_components(_Components0, _Eigenvalues0, 0, _Tolerance, [], []) :-
		!.
	take_positive_components([], [], _Requested, _Tolerance, [], []).
	take_positive_components([Component| Components0], [Eigenvalue| Eigenvalues0], Requested, Tolerance, Components, Eigenvalues) :-
		(   Eigenvalue > Tolerance ->
			Components = [Component| Components1],
			Eigenvalues = [Eigenvalue| Eigenvalues1],
			NextRequested is Requested - 1,
			take_positive_components(Components0, Eigenvalues0, NextRequested, Tolerance, Components1, Eigenvalues1)
		;   Components = [],
			Eigenvalues = []
		).

	continuous_stats(Attribute, Examples, Options, Mean, Scale) :-
		known_attribute_values(Examples, Attribute, Values),
		arithmetic_mean(Values, Mean),
		^^option(feature_scaling(FeatureScaling), Options),
		(   FeatureScaling == true ->
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

	known_attribute_values([], _Attribute, []).
	known_attribute_values([Example| Examples], Attribute, [Value| Values]) :-
		::example_attribute_values(Example, AttributeValues),
		attribute_value(Attribute, AttributeValues, Value),
		known_attribute_values(Examples, Attribute, Values).

	examples_to_rows([], _Encoders, []).
	examples_to_rows([Example| Examples], Encoders, [Features| Rows]) :-
		::example_attribute_values(Example, AttributeValues),
		encode_instance(Encoders, AttributeValues, Features),
		examples_to_rows(Examples, Encoders, Rows).

	encode_instance(Encoders, AttributeValues, Features) :-
		encoder_attribute_names(Encoders, AttributeNames),
		check_attribute_bindings(AttributeNames, AttributeValues),
		encode_instance_checked(Encoders, AttributeValues, Features).

	encoder_attribute_names([], []).
	encoder_attribute_names([continuous(Attribute, _Mean, _Scale)| Encoders], [Attribute| AttributeNames]) :-
		encoder_attribute_names(Encoders, AttributeNames).

	encode_instance_checked([], _AttributeValues, []).
	encode_instance_checked([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature| Features]) :-
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
		encode_instance_checked(Encoders, AttributeValues, Features).

	project_components([], _Features, _Index, []).
	project_components([Component| Components], Features, Index, [ComponentName-Score| ReducedInstance]) :-
		dot_product(Component, Features, Score),
		component_name(Index, ComponentName),
		NextIndex is Index + 1,
		project_components(Components, Features, NextIndex, ReducedInstance).

	print_dimension_reducer_details(Diagnostics, Encoders, Components) :-
		format('Diagnostics: ~w~n', [Diagnostics]),
		format('Encoders: ~w~n', [Encoders]),
		length(Components, ComponentCount),
		format('Components: ~w~n', [ComponentCount]).

	component_name(Index, Name) :-
		atomic_concat(component_, Index, Name).

	valid_linear_encoders(Encoders) :-
		valid(list(compound), Encoders),
		valid_linear_encoders_(Encoders, []).

	valid_linear_encoders_([], _SeenAttributes).
	valid_linear_encoders_([continuous(Attribute, Mean, Scale)| Encoders], SeenAttributes) :-
		atom(Attribute),
		valid(number, Mean),
		valid(positive_number, Scale),
		\+ member(Attribute, SeenAttributes),
		valid_linear_encoders_(Encoders, [Attribute| SeenAttributes]).

	valid_projection_components(Encoders, Components) :-
		valid(list, Components),
		length(Encoders, FeatureCount),
		valid_projection_components_(Components, FeatureCount).

	valid_projection_components_([], _FeatureCount).
	valid_projection_components_([Component| Components], FeatureCount) :-
		valid(list(number), Component),
		length(Component, FeatureCount),
		valid_projection_components_(Components, FeatureCount).

	dimension_reducer_data(DimensionReducer, Encoders, Components) :-
		DimensionReducer =.. [_Functor, Encoders, Components| _].

	dimension_reducer_diagnostics_data(DimensionReducer, Diagnostics) :-
		DimensionReducer =.. [_Functor| Arguments],
		last(Arguments, Diagnostics).

	valid_dimension_reducer_metadata(Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(Model), Diagnostics),
		atom(Model),
		memberchk(options(Options), Diagnostics),
		catch(::check_options(Options), _Error, fail).

	write_comment_header(_Dataset, _DimensionReducer, Functor, Stream) :-
		Template =.. [Functor, 'Reducer'],
		format(Stream, '% exported dimension reducer predicate: ~q/1~n', [Functor]),
		(   ::diagnostics(_DimensionReducer, Diagnostics) ->
			format(Stream, '% diagnostics: ~q~n', [Diagnostics])
		;   true
		),
		format(Stream, '% ~w~n', [Template]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

:- end_category.
