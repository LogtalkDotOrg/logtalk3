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
		date is 2026-04-30,
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

	:- protected(zero_vector/2).
	:- mode(zero_vector(+integer, -list(number)), one).
	:- info(zero_vector/2, [
		comment is 'Constructs a numeric zero vector with the requested length.',
		argnames is ['Count', 'Zeroes']
	]).

	:- protected(zero_vector_like/2).
	:- mode(zero_vector_like(+list(list(number)), -list(number)), one).
	:- info(zero_vector_like/2, [
		comment is 'Constructs a zero vector matching the length of the first vector in a list of vectors, or returns the empty list when the input is empty.',
		argnames is ['Vectors', 'ZeroVector']
	]).

	:- protected(scale_vector/3).
	:- mode(scale_vector(+list(number), +number, -list(number)), one).
	:- info(scale_vector/3, [
		comment is 'Scales each element of a numeric vector by the given factor.',
		argnames is ['Vector', 'Scale', 'ScaledVector']
	]).

	:- protected(add_vectors/3).
	:- mode(add_vectors(+list(number), +list(number), -list(number)), one).
	:- info(add_vectors/3, [
		comment is 'Adds two numeric vectors element-wise.',
		argnames is ['Vector1', 'Vector2', 'Vector']
	]).

	:- protected(subtract_vectors/3).
	:- mode(subtract_vectors(+list(number), +list(number), -list(number)), one).
	:- info(subtract_vectors/3, [
		comment is 'Subtracts the second numeric vector from the first element-wise.',
		argnames is ['Vector1', 'Vector2', 'Vector']
	]).

	:- protected(initial_vector/2).
	:- mode(initial_vector(+integer, -list(number)), one).
	:- info(initial_vector/2, [
		comment is 'Constructs the canonical all-ones initial vector with the requested length.',
		argnames is ['Size', 'Vector']
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

	:- protected(make_vector/3).
	:- mode(make_vector(+integer, +number, -list(number)), one).
	:- info(make_vector/3, [
		comment is 'Constructs a numeric vector with the requested length, filled with the given value.',
		argnames is ['Count', 'Value', 'Vector']
	]).

	:- protected(basis_initial_vectors/3).
	:- mode(basis_initial_vectors(+integer, +integer, -list(list(number))), one).
	:- info(basis_initial_vectors/3, [
		comment is 'Constructs canonical basis vectors from the given one-based index up to the requested size.',
		argnames is ['Index', 'Size', 'Vectors']
	]).

	:- protected(make_matrix/4).
	:- mode(make_matrix(+integer, +integer, +number, -list(list(number))), one).
	:- info(make_matrix/4, [
		comment is 'Constructs a numeric matrix with the requested row and column counts, filled with the given value.',
		argnames is ['Rows', 'Columns', 'Value', 'Matrix']
	]).

	:- protected(matrix_vector_product/3).
	:- mode(matrix_vector_product(+list(list(number)), +list(number), -list(number)), one).
	:- info(matrix_vector_product/3, [
		comment is 'Computes the matrix-vector product for a numeric matrix and vector.',
		argnames is ['Matrix', 'Vector', 'Product']
	]).

	:- protected(vector_norm/2).
	:- mode(vector_norm(+list(number), -float), one).
	:- info(vector_norm/2, [
		comment is 'Computes the Euclidean norm of a numeric vector.',
		argnames is ['Vector', 'Norm']
	]).

	:- protected(normalize_vector/2).
	:- mode(normalize_vector(+list(number), -list(number)), one).
	:- info(normalize_vector/2, [
		comment is 'Normalizes a numeric vector to unit length when its norm is above the shared numerical tolerance.',
		argnames is ['Vector', 'NormalizedVector']
	]).

	:- protected(difference_norm/3).
	:- mode(difference_norm(+list(number), +list(number), -float), one).
	:- info(difference_norm/3, [
		comment is 'Computes the Euclidean norm of the difference between two numeric vectors.',
		argnames is ['Vector1', 'Vector2', 'Norm']
	]).

	:- protected(stabilize_vector_sign/2).
	:- mode(stabilize_vector_sign(+list(number), -list(number)), one).
	:- info(stabilize_vector_sign/2, [
		comment is 'Normalizes a vector sign convention by flipping vectors whose first significant component is negative.',
		argnames is ['Vector', 'StableVector']
	]).

	:- protected(first_significant_component/2).
	:- mode(first_significant_component(+list(number), -number), one).
	:- info(first_significant_component/2, [
		comment is 'Returns the first component whose absolute value exceeds the shared numerical tolerance, defaulting to zero when no such component exists.',
		argnames is ['Vector', 'First']
	]).

	:- protected(zero_matrix/2).
	:- mode(zero_matrix(+integer, -list(list(number))), one).
	:- info(zero_matrix/2, [
		comment is 'Constructs a square numeric zero matrix with the requested size.',
		argnames is ['Size', 'Matrix']
	]).

	:- protected(zero_matrix/3).
	:- mode(zero_matrix(+integer, +integer, -list(list(number))), one).
	:- info(zero_matrix/3, [
		comment is 'Constructs a numeric zero matrix with the requested row and column counts.',
		argnames is ['Rows', 'Columns', 'Matrix']
	]).

	:- protected(outer_product/3).
	:- mode(outer_product(+list(number), +list(number), -list(list(number))), one).
	:- info(outer_product/3, [
		comment is 'Computes the outer product of two numeric vectors.',
		argnames is ['Vector1', 'Vector2', 'Matrix']
	]).

	:- protected(add_matrices/3).
	:- mode(add_matrices(+list(list(number)), +list(list(number)), -list(list(number))), one).
	:- info(add_matrices/3, [
		comment is 'Adds two numeric matrices element-wise.',
		argnames is ['Matrix1', 'Matrix2', 'Matrix']
	]).

	:- protected(subtract_matrices/3).
	:- mode(subtract_matrices(+list(list(number)), +list(list(number)), -list(list(number))), one).
	:- info(subtract_matrices/3, [
		comment is 'Subtracts the second numeric matrix from the first element-wise.',
		argnames is ['Matrix1', 'Matrix2', 'Matrix']
	]).

	:- protected(scale_matrix/3).
	:- mode(scale_matrix(+list(list(number)), +number, -list(list(number))), one).
	:- info(scale_matrix/3, [
		comment is 'Scales each element of a numeric matrix by the given factor.',
		argnames is ['Matrix', 'Scale', 'ScaledMatrix']
	]).

	:- protected(transpose_matrix/2).
	:- mode(transpose_matrix(+list(list(number)), -list(list(number))), one).
	:- info(transpose_matrix/2, [
		comment is 'Transposes a numeric matrix represented as a list of row lists.',
		argnames is ['Matrix', 'Transpose']
	]).

	:- protected(matrix_value/4).
	:- mode(matrix_value(+list(list(number)), +integer, +integer, -number), one).
	:- info(matrix_value/4, [
		comment is 'Looks up a numeric matrix element using one-based row and column indices.',
		argnames is ['Matrix', 'RowIndex', 'ColumnIndex', 'Value']
	]).

	:- protected(mean_vector/2).
	:- mode(mean_vector(+list(list(number)), -list(number)), one).
	:- info(mean_vector/2, [
		comment is 'Computes the column-wise arithmetic mean vector for a numeric row matrix.',
		argnames is ['Rows', 'Mean']
	]).

	:- protected(extract_components/5).
	:- mode(extract_components(+list(list(number)), +integer, +list(compound), -list(list(number)), -list(number)), one).
	:- info(extract_components/5, [
		comment is 'Extracts leading eigen-components from a numeric matrix using repeated power iteration and deflation until the requested count or the configured tolerance is reached.',
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

	:- protected(covariance_matrix/2).
	:- mode(covariance_matrix(+list(list(number)), -list(list(number))), one).
	:- info(covariance_matrix/2, [
		comment is 'Computes the sample covariance matrix for a list of centered numeric feature rows.',
		argnames is ['Rows', 'CovarianceMatrix']
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

	zero_vector(0, []) :-
		!.
	zero_vector(Count, [0.0| Zeroes]) :-
		Count > 0,
		NextCount is Count - 1,
		zero_vector(NextCount, Zeroes).

	zero_vector_like([], []).
	zero_vector_like([Vector| _Vectors], ZeroVector) :-
		length(Vector, Size),
		zero_vector(Size, ZeroVector).

	make_vector(0, _Value, []) :-
		!.
	make_vector(Count, Value, [Value| Values]) :-
		Count > 0,
		RemainingCount is Count - 1,
		make_vector(RemainingCount, Value, Values).

	scale_vector([], _Scale, []).
	scale_vector([Value| Values], Scale, [Scaled| ScaledValues]) :-
		Scaled is Value * Scale,
		scale_vector(Values, Scale, ScaledValues).

	add_vectors([], [], []).
	add_vectors([Value1| Values1], [Value2| Values2], [Value| Values]) :-
		Value is Value1 + Value2,
		add_vectors(Values1, Values2, Values).

	subtract_vectors([], [], []).
	subtract_vectors([Value1| Values1], [Value2| Values2], [Value| Values]) :-
		Value is Value1 - Value2,
		subtract_vectors(Values1, Values2, Values).

	initial_vector(0, []) :-
		!.
	initial_vector(Size, [1.0| Vector]) :-
		Size > 0,
		NextSize is Size - 1,
		initial_vector(NextSize, Vector).

	basis_vector(Size, Index, Vector) :-
		basis_vector(1, Size, Index, Vector).

	basis_vector(Current, Size, _Index, []) :-
		Current > Size,
		!.
	basis_vector(Index, Size, Index, [1.0| Vector]) :-
		!,
		Next is Index + 1,
		basis_vector(Next, Size, Index, Vector).
	basis_vector(Current, Size, Index, [0.0| Vector]) :-
		Next is Current + 1,
		basis_vector(Next, Size, Index, Vector).

	initial_vectors(Size, [InitialVector| BasisVectors]) :-
		initial_vector(Size, InitialVector),
		basis_initial_vectors(1, Size, BasisVectors).

	basis_initial_vectors(Index, Size, []) :-
		Index > Size,
		!.
	basis_initial_vectors(Index, Size, [BasisVector| BasisVectors]) :-
		basis_vector(Size, Index, BasisVector),
		NextIndex is Index + 1,
		basis_initial_vectors(NextIndex, Size, BasisVectors).

	matrix_vector_product([], _Vector, []).
	matrix_vector_product([Row| Rows], Vector, [Value| Values]) :-
		dot_product(Row, Vector, Value),
		matrix_vector_product(Rows, Vector, Values).

	vector_norm(Vector, Norm) :-
		dot_product(Vector, Vector, SumSquares),
		Norm is sqrt(SumSquares).

	normalize_vector(Vector, NormalizedVector) :-
		vector_norm(Vector, Norm),
		(   Norm =< 1.0e-12 ->
			NormalizedVector = Vector
		;   scale_vector(Vector, 1.0 / Norm, NormalizedVector)
		).

	difference_norm(Vector1, Vector2, Norm) :-
		subtract_vectors(Vector1, Vector2, Difference),
		vector_norm(Difference, Norm).

	stabilize_vector_sign(Vector, StableVector) :-
		(   first_significant_component(Vector, First),
			First < 0.0 ->
			scale_vector(Vector, -1.0, StableVector)
		;   StableVector = Vector
		).

	first_significant_component([Value| _Values], Value) :-
		abs(Value) > 1.0e-12,
		!.
	first_significant_component([_Value| Values], First) :-
		first_significant_component(Values, First).
	first_significant_component([], 0.0).

	zero_matrix(Size, Matrix) :-
		zero_matrix(Size, Size, Matrix).

	zero_matrix(0, _Columns, []) :-
		!.
	zero_matrix(Rows, Columns, [Row| Matrix]) :-
		Rows > 0,
		zero_vector(Columns, Row),
		NextRows is Rows - 1,
		zero_matrix(NextRows, Columns, Matrix).

	make_matrix(0, _Columns, _Value, []) :-
		!.
	make_matrix(Rows, Columns, Value, [Row| Matrix]) :-
		Rows > 0,
		make_vector(Columns, Value, Row),
		NextRows is Rows - 1,
		make_matrix(NextRows, Columns, Value, Matrix).

	outer_product([], _Vector, []).
	outer_product([Value| Values], Vector, [Row| Rows]) :-
		scale_vector(Vector, Value, Row),
		outer_product(Values, Vector, Rows).

	add_matrices([], [], []).
	add_matrices([Row1| Rows1], [Row2| Rows2], [Row| Rows]) :-
		add_vectors(Row1, Row2, Row),
		add_matrices(Rows1, Rows2, Rows).

	subtract_matrices([], [], []).
	subtract_matrices([Row1| Rows1], [Row2| Rows2], [Row| Rows]) :-
		subtract_vectors(Row1, Row2, Row),
		subtract_matrices(Rows1, Rows2, Rows).

	scale_matrix([], _Scale, []).
	scale_matrix([Row| Rows], Scale, [ScaledRow| ScaledRows]) :-
		scale_vector(Row, Scale, ScaledRow),
		scale_matrix(Rows, Scale, ScaledRows).

	transpose_matrix([], []) :-
		!.
	transpose_matrix([[]| _], []) :-
		!.
	transpose_matrix(Matrix, [Column| Columns]) :-
		extract_first_column(Matrix, Column, RemainingMatrix),
		transpose_matrix(RemainingMatrix, Columns).

	extract_first_column([], [], []).
	extract_first_column([[Head| Tail]| Rows], [Head| Column], [Tail| RemainingRows]) :-
		extract_first_column(Rows, Column, RemainingRows).

	matrix_value([Row| _Rows], 1, ColumnIndex, Value) :-
		!,
		list_value(Row, ColumnIndex, Value).
	matrix_value([_Row| Rows], RowIndex, ColumnIndex, Value) :-
		NextRowIndex is RowIndex - 1,
		matrix_value(Rows, NextRowIndex, ColumnIndex, Value).

	list_value([Value| _Values], 1, Value) :-
		!.
	list_value([_Value| Values], Index, Value) :-
		NextIndex is Index - 1,
		list_value(Values, NextIndex, Value).

	mean_vector(Rows, Mean) :-
		transpose_matrix(Rows, Columns),
		column_means(Columns, Mean).

	column_means([], []).
	column_means([Column| Columns], [Mean| Means]) :-
		arithmetic_mean(Column, Mean),
		column_means(Columns, Means).

	extract_components(_Matrix, 0, _Options, [], []) :-
		!.
	extract_components(Matrix, Requested, Options, Components, Eigenvalues) :-
		extract_components(Matrix, Requested, Options, [], [], Components, Eigenvalues).

	extract_components(_Matrix, 0, _Options, ComponentsAcc, EigenvaluesAcc, Components, Eigenvalues) :-
		!,
		reverse(ComponentsAcc, Components),
		reverse(EigenvaluesAcc, Eigenvalues).
	extract_components(Matrix, Requested, Options, ComponentsAcc, EigenvaluesAcc, Components, Eigenvalues) :-
		principal_component(Matrix, Options, Eigenvalue, Eigenvector),
		^^option(tolerance(Tolerance), Options),
		(   Eigenvalue =< Tolerance ->
			reverse(ComponentsAcc, Components),
			reverse(EigenvaluesAcc, Eigenvalues)
		;   deflate_matrix(Matrix, Eigenvalue, Eigenvector, DeflatedMatrix),
			NextRequested is Requested - 1,
			extract_components(DeflatedMatrix, NextRequested, Options, [Eigenvector| ComponentsAcc], [Eigenvalue| EigenvaluesAcc], Components, Eigenvalues)
		).

	principal_component(Matrix, Options, Eigenvalue, Eigenvector) :-
		length(Matrix, Size),
		initial_vectors(Size, InitialVectors),
		zero_vector(Size, ZeroVector),
		principal_component_candidates(Matrix, Options, InitialVectors, 0.0, ZeroVector, Eigenvalue, Eigenvector).

	principal_component_candidates(_Matrix, _Options, [], BestEigenvalue, BestEigenvector, BestEigenvalue, BestEigenvector) :-
		!.
	principal_component_candidates(Matrix, Options, [InitialVector| InitialVectors], BestEigenvalue0, BestEigenvector0, BestEigenvalue, BestEigenvector) :-
		normalize_vector(InitialVector, NormalizedInitial),
		iterate_component(Matrix, Options, 0, NormalizedInitial, CandidateEigenvalue, CandidateEigenvector),
		(   CandidateEigenvalue > BestEigenvalue0 ->
			BestEigenvalue1 = CandidateEigenvalue,
			BestEigenvector1 = CandidateEigenvector
		;   BestEigenvalue1 = BestEigenvalue0,
			BestEigenvector1 = BestEigenvector0
		),
		principal_component_candidates(Matrix, Options, InitialVectors, BestEigenvalue1, BestEigenvector1, BestEigenvalue, BestEigenvector).

	iterate_component(Matrix, Options, Iteration, Vector0, Eigenvalue, Eigenvector) :-
		matrix_vector_product(Matrix, Vector0, Product),
		vector_norm(Product, Norm),
		^^option(tolerance(Tolerance), Options),
		(   Norm =< Tolerance ->
			Eigenvalue = 0.0,
			Eigenvector = Vector0
		;   scale_vector(Product, 1.0 / Norm, Vector1),
			stabilize_vector_sign(Vector1, StableVector),
			difference_norm(StableVector, Vector0, Delta),
			^^option(maximum_iterations(MaximumIterations), Options),
			(   (Delta =< Tolerance ; Iteration >= MaximumIterations) ->
				rayleigh_quotient(Matrix, StableVector, Eigenvalue),
				Eigenvector = StableVector
			;   NextIteration is Iteration + 1,
				iterate_component(Matrix, Options, NextIteration, StableVector, Eigenvalue, Eigenvector)
			)
		).

	rayleigh_quotient(Matrix, Vector, Eigenvalue) :-
		matrix_vector_product(Matrix, Vector, Product),
		dot_product(Vector, Product, Eigenvalue).

	deflate_matrix(Matrix, Eigenvalue, Eigenvector, DeflatedMatrix) :-
		outer_product(Eigenvector, Eigenvector, OuterProduct),
		scale_matrix(OuterProduct, Eigenvalue, ScaledOuterProduct),
		subtract_matrices(Matrix, ScaledOuterProduct, DeflatedMatrix).

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

	covariance_matrix(Rows, CovarianceMatrix) :-
		Rows = [FirstRow| _],
		length(FirstRow, FeatureCount),
		zero_matrix(FeatureCount, ZeroMatrix),
		accumulate_outer_products(Rows, ZeroMatrix, SumMatrix),
		length(Rows, Count),
		Scale is 1.0 / (Count - 1),
		scale_matrix(SumMatrix, Scale, CovarianceMatrix).

	accumulate_outer_products([], Matrix, Matrix).
	accumulate_outer_products([Row| Rows], Matrix0, Matrix) :-
		outer_product(Row, Row, Outer),
		add_matrices(Matrix0, Outer, Matrix1),
		accumulate_outer_products(Rows, Matrix1, Matrix).

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
