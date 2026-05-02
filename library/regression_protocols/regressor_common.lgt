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


:- category(regressor_common,
	implements(regressor_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-02,
		comment is 'Shared predicates for regressor learning defaults, diagnostics, validation, dataset validation, export, and pretty-print helpers.'
	]).

	:- protected(regressor_diagnostics_data/2).
	:- mode(regressor_diagnostics_data(+compound, -list(compound)), one).
	:- info(regressor_diagnostics_data/2, [
		comment is 'Default hook predicate for exposing diagnostics metadata from a regressor term. Importing implementations may override it when using a non-standard regressor representation.',
		argnames is ['Regressor', 'Diagnostics']
	]).

	:- protected(regressor_export_template/4).
	:- mode(regressor_export_template(+object_identifier, +compound, +atom, -callable), one).
	:- info(regressor_export_template/4, [
		comment is 'Hook predicate that importing regressor implementations must define in order to expose the exported regressor template for a given functor.',
		argnames is ['Dataset', 'Regressor', 'Functor', 'Template']
	]).

	:- protected(regressor_term_template/2).
	:- mode(regressor_term_template(+compound, -callable), one).
	:- info(regressor_term_template/2, [
		comment is 'Hook predicate that importing regressor implementations must define in order to expose the learned regressor term template used by pretty-printing helpers.',
		argnames is ['Regressor', 'Template']
	]).

	:- protected(dataset_attributes/2).
	:- mode(dataset_attributes(+object_identifier, -list(pair)), one).
	:- info(dataset_attributes/2, [
		comment is 'Collects the dataset attribute declarations as `Attribute-Values` pairs.',
		argnames is ['Dataset', 'Attributes']
	]).

	:- protected(dataset_examples/2).
	:- mode(dataset_examples(+object_identifier, -list(compound)), one).
	:- info(dataset_examples/2, [
		comment is 'Collects the dataset examples as `example(Id, TargetValue, AttributeValues)` terms.',
		argnames is ['Dataset', 'Examples']
	]).

	:- protected(check_examples/2).
	:- mode(check_examples(+object_identifier, +list), one_or_error).
	:- info(check_examples/2, [
		comment is 'Validates that the collected examples list is non-empty, only contains numeric targets, and only uses declared attributes without duplicate bindings.',
		argnames is ['Dataset', 'Examples'],
		exceptions is [
			'Examples is the empty list' - domain_error(non_empty_dataset, 'Dataset'),
			'An example target is not numeric' - type_error(number, 'Target'),
			'An example repeats a declared attribute binding' - domain_error(attribute_occurrences, 'Attribute'),
			'An example contains an undeclared attribute binding' - domain_error(declared_attribute, 'Attribute')
		]
	]).

	:- protected(print_regressor_template/1).
	:- mode(print_regressor_template(+compound), one).
	:- info(print_regressor_template/1, [
		comment is 'Pretty-printing helper predicate used by importing regressor implementations to show the learned regressor term template.',
		argnames is ['Regressor']
	]).

	:- protected(base_regressor_diagnostics/6).
	:- mode(base_regressor_diagnostics(+atom, +atom, +integer, +list(compound), +list(compound), -list(compound)), one).
	:- info(base_regressor_diagnostics/6, [
		comment is 'Builds common diagnostics metadata terms for a learned regressor and appends regressor-specific diagnostics terms.',
		argnames is ['Model', 'Target', 'TrainingExampleCount', 'Options', 'ExtraDiagnostics', 'Diagnostics']
	]).

	:- protected(valid_attribute_names/1).
	:- mode(valid_attribute_names(+list(atom)), zero_or_one).
	:- info(valid_attribute_names/1, [
		comment is 'True when a list of attribute names is a proper list of distinct atoms.',
		argnames is ['AttributeNames']
	]).

	:- protected(valid_attribute_declarations/1).
	:- mode(valid_attribute_declarations(+list(pair)), zero_or_one).
	:- info(valid_attribute_declarations/1, [
		comment is 'True when a list of attribute declarations is a proper list of distinct ``Attribute-Values`` pairs where values are either ``continuous`` or a valid discrete value list.',
		argnames is ['Attributes']
	]).

	:- protected(valid_discrete_values/1).
	:- mode(valid_discrete_values(+list), zero_or_one).
	:- info(valid_discrete_values/1, [
		comment is 'True when a list of categorical values is non-empty, contains only nonvar terms, and has no duplicates.',
		argnames is ['Values']
	]).

	:- protected(valid_regression_encoders/1).
	:- mode(valid_regression_encoders(+list(compound)), zero_or_one).
	:- info(valid_regression_encoders/1, [
		comment is 'True when a list of encoders only contains valid ``continuous/3`` or ``categorical/2`` encoder terms with distinct attributes.',
		argnames is ['Encoders']
	]).

	:- protected(valid_regressor_options/1).
	:- mode(valid_regressor_options(+list(compound)), zero_or_one).
	:- info(valid_regressor_options/1, [
		comment is 'True when a list of options is structurally valid for the receiving regressor implementation.',
		argnames is ['Options']
	]).

	:- protected(valid_regressor_metadata/2).
	:- mode(valid_regressor_metadata(+atom, +list(compound)), zero_or_one).
	:- info(valid_regressor_metadata/2, [
		comment is 'True when diagnostics metadata contains the expected model term and records a structurally valid effective options list.',
		argnames is ['Model', 'Diagnostics']
	]).

	:- protected(valid_diagnostic_count/3).
	:- mode(valid_diagnostic_count(+atom, +list(compound), +integer), zero_or_one).
	:- info(valid_diagnostic_count/3, [
		comment is 'True when diagnostics contains a count term with the given functor and integer value.',
		argnames is ['Functor', 'Diagnostics', 'Count']
	]).

	:- protected(valid_linear_model_diagnostics/1).
	:- mode(valid_linear_model_diagnostics(+list(compound)), zero_or_one).
	:- info(valid_linear_model_diagnostics/1, [
		comment is 'True when diagnostics contains structurally valid linear-model optimization metadata terms for convergence, completed iterations, and final parameter delta.',
		argnames is ['Diagnostics']
	]).

	:- protected(valid_encoded_rows/2).
	:- mode(valid_encoded_rows(+list(compound), +list), zero_or_one).
	:- info(valid_encoded_rows/2, [
		comment is 'True when encoded training rows match the feature count induced by the encoders and carry numeric targets.',
		argnames is ['Encoders', 'Rows']
	]).

	:- protected(encoded_feature_count/2).
	:- mode(encoded_feature_count(+list(compound), -integer), one).
	:- info(encoded_feature_count/2, [
		comment is 'Counts the number of numeric features induced by a list of continuous and categorical encoders, including missing-value indicator features.',
		argnames is ['Encoders', 'FeatureCount']
	]).

	:- protected(continuous_stats/5).
	:- mode(continuous_stats(+atom, +list(compound), +list(compound), -float, -positive_float), one).
	:- info(continuous_stats/5, [
		comment is 'Computes the mean and scaling factor used to encode a continuous attribute from the training examples according to the effective feature scaling option.',
		argnames is ['Attribute', 'Examples', 'Options', 'Mean', 'Scale']
	]).

	:- private(known_attribute_values/3).
	:- mode(known_attribute_values(+list(compound), +atom, -list(number)), one).
	:- info(known_attribute_values/3, [
		comment is 'Collects the known numeric values for a continuous attribute from the training examples, skipping omitted and variable values.',
		argnames is ['Examples', 'Attribute', 'Values']
	]).

	:- protected(examples_to_rows/3).
	:- mode(examples_to_rows(+list(compound), +list(compound), -list(pair)), one).
	:- info(examples_to_rows/3, [
		comment is 'Encodes dataset examples as numeric feature-vector and target pairs using a list of encoders.',
		argnames is ['Examples', 'Encoders', 'Rows']
	]).

	:- protected(encode_instance/3).
	:- mode(encode_instance(+list(compound), +list(pair), -list(float)), one).
	:- info(encode_instance/3, [
		comment is 'Validates and encodes an attribute-value list as a numeric feature vector using a list of continuous and categorical encoders.',
		argnames is ['Encoders', 'AttributeValues', 'Features']
	]).

	:- private(check_attribute_bindings/2).
	:- mode(check_attribute_bindings(+list(atom), +list(pair)), one).
	:- info(check_attribute_bindings/2, [
		comment is 'Checks that an attribute-value list contains only declared attributes and does not repeat any declared attribute; omitted declared attributes are allowed and treated as missing values.',
		argnames is ['AttributeNames', 'AttributeValues']
	]).

	:- private(check_declared_attribute_bindings/2).
	:- mode(check_declared_attribute_bindings(+list(atom), +list(pair)), one).
	:- info(check_declared_attribute_bindings/2, [
		comment is 'Checks that no declared attribute appears more than once in an attribute-value list.',
		argnames is ['AttributeNames', 'AttributeValues']
	]).

	:- private(check_undeclared_attribute_bindings/2).
	:- mode(check_undeclared_attribute_bindings(+list(pair), +list(atom)), one).
	:- info(check_undeclared_attribute_bindings/2, [
		comment is 'Checks that every attribute occurring in an attribute-value list is declared.',
		argnames is ['AttributeValues', 'AttributeNames']
	]).

	:- private(attribute_occurrences/4).
	:- mode(attribute_occurrences(+list(pair), +atom, +integer, -integer), one).
	:- info(attribute_occurrences/4, [
		comment is 'Counts the number of times an attribute occurs in an attribute-value list.',
		argnames is ['AttributeValues', 'Attribute', 'Count0', 'Count']
	]).

	:- private(declared_attribute_names/2).
	:- mode(declared_attribute_names(+list(pair), -list(atom)), one).
	:- info(declared_attribute_names/2, [
		comment is 'Collects the declared attribute names from dataset attribute declarations.',
		argnames is ['Attributes', 'AttributeNames']
	]).

	:- private(encoder_attribute_names/2).
	:- mode(encoder_attribute_names(+list(compound), -list(atom)), one).
	:- info(encoder_attribute_names/2, [
		comment is 'Collects the declared attribute names from a list of regression encoders.',
		argnames is ['Encoders', 'AttributeNames']
	]).

	:- private(encode_instance_checked/3).
	:- mode(encode_instance_checked(+list(compound), +list(pair), -list(float)), one).
	:- info(encode_instance_checked/3, [
		comment is 'Encodes an already validated attribute-value list as a numeric feature vector using a list of continuous and categorical encoders.',
		argnames is ['Encoders', 'AttributeValues', 'Features']
	]).

	:- protected(fit_linear_model/7).
	:- mode(fit_linear_model(+object_identifier, +list(compound), -list(compound), -integer, -float, -list(float), -list(compound)), one).
	:- info(fit_linear_model/7, [
		comment is 'Builds linear-model encoders from the training dataset, encodes the examples, fits a bias plus weight vector using batch gradient descent and an L2 regularization option, and returns optimization diagnostics terms.',
		argnames is ['Dataset', 'Options', 'Encoders', 'TrainingExampleCount', 'Bias', 'Weights', 'TrainingDiagnostics']
	]).

	:- private(valid_linear_model_convergence/1).
	:- mode(valid_linear_model_convergence(+atom), zero_or_one).
	:- info(valid_linear_model_convergence/1, [
		comment is 'True when a linear-model optimization stop reason is recognized.',
		argnames is ['Convergence']
	]).

	:- protected(valid_feature_labels/1).
	:- mode(valid_feature_labels(+list(compound)), zero_or_one).
	:- info(valid_feature_labels/1, [
		comment is 'True when a list of regression-tree feature labels only contains valid ``feature/2`` terms.',
		argnames is ['FeatureLabels']
	]).

	:- protected(valid_regression_tree/2).
	:- mode(valid_regression_tree(+compound, +positive_integer), zero_or_one).
	:- info(valid_regression_tree/2, [
		comment is 'True when a regression tree only contains valid ``leaf/1`` and ``node/5`` terms using feature indexes within bounds.',
		argnames is ['Tree', 'FeatureCount']
	]).

	:- private(normalize_continuous/4).
	:- mode(normalize_continuous(+number, +float, +positive_float, -float), one).
	:- info(normalize_continuous/4, [
		comment is 'Normalizes a continuous value using a stored mean and scale.',
		argnames is ['Value', 'Mean', 'Scale', 'Feature']
	]).

	:- private(check_categorical_value/3).
	:- mode(check_categorical_value(+atom, +list, +nonvar), one).
	:- info(check_categorical_value/3, [
		comment is 'Validates that a categorical value is declared for an attribute.',
		argnames is ['Attribute', 'Values', 'Value']
	]).

	:- private(one_hot_encode/3).
	:- mode(one_hot_encode(+list, +nonvar, -list(float)), one).
	:- info(one_hot_encode/3, [
		comment is 'Encodes a declared categorical value using one-hot encoding plus a trailing missing-value indicator feature set to zero.',
		argnames is ['Values', 'Value', 'Encoded']
	]).

	:- private(missing_one_hot_encode/2).
	:- mode(missing_one_hot_encode(+list, -list(float)), one).
	:- info(missing_one_hot_encode/2, [
		comment is 'Encodes a missing categorical value as zeroes plus a trailing missing-value indicator feature set to one.',
		argnames is ['Values', 'Encoded']
	]).

	:- private(zero_vector_from_values/2).
	:- mode(zero_vector_from_values(+list, -list(float)), one).
	:- info(zero_vector_from_values/2, [
		comment is 'Creates a zero vector with one element per declared categorical value.',
		argnames is ['Values', 'Zeroes']
	]).

	:- private(build_linear_encoders/4).
	:- mode(build_linear_encoders(+list(pair), +list(compound), +list(compound), -list(compound)), one).
	:- info(build_linear_encoders/4, [
		comment is 'Builds the encoder list used by linear models from dataset attribute declarations, training examples, and the effective feature scaling option.',
		argnames is ['Attributes', 'Examples', 'Options', 'Encoders']
	]).

	:- private(train_linear_model/6).
	:- mode(train_linear_model(+list(pair), +integer, +list(compound), -float, -list(float), -list(compound)), one).
	:- info(train_linear_model/6, [
		comment is 'Fits a linear model bias and weight vector from encoded rows using batch gradient descent and returns optimization diagnostics terms.',
		argnames is ['Rows', 'FeatureCount', 'Options', 'Bias', 'Weights', 'TrainingDiagnostics']
	]).

	:- private(optimize_linear_model/11).
	:- mode(optimize_linear_model(+list(pair), +list(compound), +integer, +float, +list(float), +float, -float, -list(float), -atom, -integer, -float), one).
	:- info(optimize_linear_model/11, [
		comment is 'Runs iterative batch gradient-descent updates until the tolerance or maximum iteration limit is reached and reports the stop reason, completed iterations, and final maximum parameter delta.',
		argnames is ['Rows', 'Options', 'Iteration', 'Bias0', 'Weights0', 'PreviousDelta', 'Bias', 'Weights', 'Convergence', 'Iterations', 'FinalDelta']
	]).

	:- private(update_linear_parameters/7).
	:- mode(update_linear_parameters(+list(pair), +float, +list(float), +list(compound), -float, -list(float), -float), one).
	:- info(update_linear_parameters/7, [
		comment is 'Performs one batch gradient-descent parameter update step and returns the maximum absolute parameter change.',
		argnames is ['Rows', 'Bias0', 'Weights0', 'Options', 'Bias1', 'Weights1', 'MaxDelta']
	]).

	:- private(accumulate_linear_gradients/7).
	:- mode(accumulate_linear_gradients(+list(pair), +float, +list(float), +float, +list(float), -float, -list(float)), one).
	:- info(accumulate_linear_gradients/7, [
		comment is 'Accumulates the batch bias and weight gradients over a list of encoded training rows.',
		argnames is ['Rows', 'Bias', 'Weights', 'GradientBias0', 'GradientWeights0', 'GradientBias', 'GradientWeights']
	]).

	:- private(add_scaled_vector/4).
	:- mode(add_scaled_vector(+list(float), +number, +list(float), -list(float)), one).
	:- info(add_scaled_vector/4, [
		comment is 'Adds a scaled feature vector to an accumulated gradient vector.',
		argnames is ['Features', 'Scale', 'Gradients0', 'Gradients']
	]).

	:- private(update_linear_weights/8).
	:- mode(update_linear_weights(+list(float), +list(float), +float, +float, +float, -list(float), +float, -float), one).
	:- info(update_linear_weights/8, [
		comment is 'Updates the weight vector using the mean gradients and L2 regularization term while tracking the maximum absolute weight change.',
		argnames is ['Weights0', 'Gradients', 'Scale', 'Regularization', 'LearningRate', 'Weights1', 'MaxDelta0', 'MaxDelta']
	]).

	:- private(zero_vector/2).
	:- mode(zero_vector(+integer, -list(float)), one).
	:- info(zero_vector/2, [
		comment is 'Creates a zero-filled float vector with the requested length.',
		argnames is ['Count', 'Zeroes']
	]).

	:- private(zero_vector_like/2).
	:- mode(zero_vector_like(+list, -list(float)), one).
	:- info(zero_vector_like/2, [
		comment is 'Creates a zero-filled float vector with the same length as the reference list.',
		argnames is ['Reference', 'Zeroes']
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		append/3, length/2, last/2, member/2, memberchk/2
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

	learn(Dataset, Regressor) :-
		::learn(Dataset, Regressor, []).

	check_regressor(Regressor) :-
		(   var(Regressor) ->
			instantiation_error
		; 	::regressor_term_template(Regressor, _Template),
			::regressor_diagnostics_data(Regressor, _Diagnostics) ->
			true
		;   domain_error(regressor, Regressor)
		).

	valid_regressor(Regressor) :-
		catch(::check_regressor(Regressor), _Error, fail).

	diagnostics(Regressor, Diagnostics) :-
		::regressor_diagnostics_data(Regressor, Diagnostics).

	diagnostic(Regressor, Diagnostic) :-
		::regressor_diagnostics_data(Regressor, Diagnostics),
		member(Diagnostic, Diagnostics).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		).

	dataset_examples(Dataset, Examples) :-
		findall(
			example(Id, TargetValue, AttributeValues),
			Dataset::example(Id, TargetValue, AttributeValues),
			Examples
		).

	check_examples(Dataset, Examples) :-
		(	Examples == [] ->
			domain_error(non_empty_dataset, Dataset)
		;	true
		),
		::dataset_attributes(Dataset, Attributes),
		declared_attribute_names(Attributes, AttributeNames),
		check_targets(Examples, AttributeNames).

	check_targets([], _AttributeNames).
	check_targets([example(_Id, Target, AttributeValues)| Examples], AttributeNames) :-
		(	number(Target) ->
			true
		;	type_error(number, Target)
		),
		check_attribute_bindings(AttributeNames, AttributeValues),
		check_targets(Examples, AttributeNames).

	print_regressor_template(Regressor) :-
		::regressor_term_template(Regressor, Template),
		format('Template: ~w~n', [Template]).

	regressor_diagnostics_data(Regressor, Diagnostics) :-
		Regressor =.. [_| Arguments],
		last(Arguments, Diagnostics).

	base_regressor_diagnostics(Model, Target, TrainingExampleCount, Options, ExtraDiagnostics, Diagnostics) :-
		Diagnostics = [
			model(Model),
			target(Target),
			training_example_count(TrainingExampleCount),
			options(Options)
		| ExtraDiagnostics
		].

	valid_attribute_names(AttributeNames) :-
		valid(list(atom), AttributeNames),
		valid_distinct_terms(AttributeNames).

	valid_attribute_declarations(Attributes) :-
		valid(list(pair), Attributes),
		valid_attribute_declarations_(Attributes, []).

	valid_discrete_values(Values) :-
		valid(list(nonvar), Values),
		Values \== [],
		valid_distinct_terms(Values).

	valid_regression_encoders(Encoders) :-
		valid(list(compound), Encoders),
		valid_regression_encoders_(Encoders, []).

	valid_regressor_options(Options) :-
		valid(list(compound), Options),
		catch(::check_options(Options), _Error, fail).

	valid_regressor_metadata(Model, Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(options(Options), Diagnostics),
		catch(::check_options(Options), _Error, fail).

	valid_diagnostic_count(Functor, Diagnostics, Count) :-
		integer(Count),
		Diagnostic =.. [Functor, Value],
		memberchk(Diagnostic, Diagnostics),
		integer(Value),
		Value =:= Count.

	valid_linear_model_diagnostics(Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		valid_linear_model_convergence(Convergence),
		memberchk(iterations(Iterations), Diagnostics),
		integer(Iterations),
		Iterations >= 1,
		memberchk(final_delta(FinalDelta), Diagnostics),
		valid(non_negative_float, FinalDelta).

	valid_linear_model_convergence(tolerance).
	valid_linear_model_convergence(maximum_iterations_exhausted).

	valid_encoded_rows(Encoders, Rows) :-
		valid(list(compound), Rows),
		Rows \== [],
		encoded_feature_count(Encoders, FeatureCount),
		valid_encoded_rows_(Rows, FeatureCount).

	valid_feature_labels(FeatureLabels) :-
		valid(list(compound), FeatureLabels),
		valid_feature_labels_(FeatureLabels).

	valid_regression_tree(Tree, FeatureCount) :-
		integer(FeatureCount),
		FeatureCount > 0,
		valid_regression_tree_(Tree, FeatureCount).

	regressor_options(Regressor, Options) :-
		diagnostics(Regressor, Diagnostics),
		memberchk(options(Options), Diagnostics).

	export_to_file(Dataset, Regressor, Functor, File) :-
		::export_to_clauses(Dataset, Regressor, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Dataset, Functor, Regressor, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_comment_header(Dataset, Functor, Regressor, Stream) :-
		::regressor_export_template(Dataset, Regressor, Functor, Template),
		functor(Template, _, Arity),
		format(Stream, '% exported regressor predicate: ~q/~d~n', [Functor, Arity]),
		format(Stream, '% training dataset: ~q~n', [Dataset]),
		Dataset::target(Target),
		format(Stream, '% target: ~q~n', [Target]),
		::dataset_attributes(Dataset, Attributes),
		format(Stream, '% attributes: ~q~n', [Attributes]),
		(   ::diagnostics(Regressor, Diagnostics) ->
			format(Stream, '% diagnostics: ~q~n', [Diagnostics])
		;   true
		),
		format(Stream, '% ~w~n', [Template]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	valid_distinct_terms([]).
	valid_distinct_terms([Term| Terms]) :-
		\+ member(Term, Terms),
		valid_distinct_terms(Terms).

	valid_attribute_declarations_([], _SeenAttributes).
	valid_attribute_declarations_([Attribute-Values| Attributes], SeenAttributes) :-
		atom(Attribute),
		\+ member(Attribute, SeenAttributes),
		(   Values == continuous
		;   valid_discrete_values(Values)
		),
		valid_attribute_declarations_(Attributes, [Attribute| SeenAttributes]).

	valid_regression_encoders_([], _SeenAttributes).
	valid_regression_encoders_([continuous(Attribute, Mean, Scale)| Encoders], SeenAttributes) :-
		atom(Attribute),
		valid(float, Mean),
		valid(positive_float, Scale),
		\+ member(Attribute, SeenAttributes),
		valid_regression_encoders_(Encoders, [Attribute| SeenAttributes]).
	valid_regression_encoders_([categorical(Attribute, Values)| Encoders], SeenAttributes) :-
		atom(Attribute),
		valid_discrete_values(Values),
		\+ member(Attribute, SeenAttributes),
		valid_regression_encoders_(Encoders, [Attribute| SeenAttributes]).

	continuous_stats(Attribute, Examples, Options, Mean, Scale) :-
		::option(feature_scaling(FeatureScaling), Options),
		(   FeatureScaling == true ->
			::known_attribute_values(Examples, Attribute, Values),
			(   Values == [] ->
				Mean = 0.0,
				Scale = 1.0
			;   arithmetic_mean(Values, Mean),
				length(Values, Count),
				(   Count > 1 ->
					variance(Values, Variance)
				;   Variance = 0.0
				),
				(   Variance > 0.0 ->
					Scale is sqrt(Variance)
				;   Scale = 1.0
				)
			)
		;   Mean = 0.0,
			Scale = 1.0
		).

	known_attribute_values([], _, []).
	known_attribute_values([example(_Id, _Target, AttributeValues)| Examples], Attribute, Values) :-
		(   member(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			Values = [Value| Rest]
		;   Values = Rest
		),
		known_attribute_values(Examples, Attribute, Rest).

	examples_to_rows([], _, []).
	examples_to_rows([example(_Id, Target, AttributeValues)| Examples], Encoders, [Features-Target| Rows]) :-
		encode_instance_checked(Encoders, AttributeValues, Features),
		examples_to_rows(Examples, Encoders, Rows).

	encode_instance(Encoders, AttributeValues, Features) :-
		encoder_attribute_names(Encoders, AttributeNames),
		check_attribute_bindings(AttributeNames, AttributeValues),
		encode_instance_checked(Encoders, AttributeValues, Features).

	encode_instance_checked([], _, []).
	encode_instance_checked([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature, Missing| Features]) :-
		!,
		(   member(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			normalize_continuous(Value, Mean, Scale, Feature),
			Missing = 0.0
		;   Feature = 0.0,
			Missing = 1.0
		),
		encode_instance_checked(Encoders, AttributeValues, Features).
	encode_instance_checked([categorical(Attribute, Values)| Encoders], AttributeValues, Features) :-
		(   member(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			check_categorical_value(Attribute, Values, Value),
			one_hot_encode(Values, Value, Encoded)
		;   missing_one_hot_encode(Values, Encoded)
		),
		encode_instance_checked(Encoders, AttributeValues, RestFeatures),
		append(Encoded, RestFeatures, Features).

	check_attribute_bindings(AttributeNames, AttributeValues) :-
		check_declared_attribute_bindings(AttributeNames, AttributeValues),
		check_undeclared_attribute_bindings(AttributeValues, AttributeNames).

	check_declared_attribute_bindings([], _AttributeValues).
	check_declared_attribute_bindings([Attribute| Attributes], AttributeValues) :-
		attribute_occurrences(AttributeValues, Attribute, 0, Count),
		(   Count =< 1 ->
			true
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

	declared_attribute_names([], []).
	declared_attribute_names([Attribute-_Values| Attributes], [Attribute| AttributeNames]) :-
		declared_attribute_names(Attributes, AttributeNames).

	encoder_attribute_names([], []).
	encoder_attribute_names([continuous(Attribute, _Mean, _Scale)| Encoders], [Attribute| AttributeNames]) :-
		!,
		encoder_attribute_names(Encoders, AttributeNames).
	encoder_attribute_names([categorical(Attribute, _Values)| Encoders], [Attribute| AttributeNames]) :-
		encoder_attribute_names(Encoders, AttributeNames).

	fit_linear_model(Dataset, Options, Encoders, TrainingExampleCount, Bias, Weights, TrainingDiagnostics) :-
		::dataset_attributes(Dataset, Attributes),
		::dataset_examples(Dataset, Examples),
		::check_examples(Dataset, Examples),
		build_linear_encoders(Attributes, Examples, Options, Encoders),
		::examples_to_rows(Examples, Encoders, Rows),
		::encoded_feature_count(Encoders, FeatureCount),
		train_linear_model(Rows, FeatureCount, Options, Bias, Weights, TrainingDiagnostics),
		length(Examples, TrainingExampleCount).

	normalize_continuous(Value, Mean, Scale, Feature) :-
		(   number(Value) ->
			true
		;   type_error(number, Value)
		),
		Feature is (Value - Mean) / Scale.

	check_categorical_value(Attribute, Values, Value) :-
		(   member(Value, Values) ->
			true
		;   domain_error(attribute_value(Attribute, Values), Value)
		).

	one_hot_encode(Values, Value, Encoded) :-
		one_hot_encode_(Values, Value, Encoded0),
		append(Encoded0, [0.0], Encoded).

	one_hot_encode_([], _, []).
	one_hot_encode_([Category| Categories], Value, [Feature| Features]) :-
		(   Value == Category ->
			Feature = 1.0
		;   Feature = 0.0
		),
		one_hot_encode_(Categories, Value, Features).

	missing_one_hot_encode(Values, Encoded) :-
		zero_vector_from_values(Values, Zeroes),
		append(Zeroes, [1.0], Encoded).

	zero_vector_from_values([], []).
	zero_vector_from_values([_| Values], [0.0| Zeroes]) :-
		zero_vector_from_values(Values, Zeroes).

	encoded_feature_count([], 0).
	encoded_feature_count([continuous(_, _, _)| Encoders], Count) :-
		!,
		encoded_feature_count(Encoders, RestCount),
		Count is RestCount + 2.
	encoded_feature_count([categorical(_, Values)| Encoders], Count) :-
		length(Values, ValueCount),
		encoded_feature_count(Encoders, RestCount),
		Count is RestCount + ValueCount + 1.

	build_linear_encoders([], _Examples, _Options, []).
	build_linear_encoders([Attribute-Values| Rest], Examples, Options, [Encoder| Encoders]) :-
		(   Values == continuous ->
			::continuous_stats(Attribute, Examples, Options, Mean, Scale),
			Encoder = continuous(Attribute, Mean, Scale)
		;   Encoder = categorical(Attribute, Values)
		),
		build_linear_encoders(Rest, Examples, Options, Encoders).

	train_linear_model(Rows, FeatureCount, Options, Bias, Weights, TrainingDiagnostics) :-
		zero_vector(FeatureCount, InitialWeights),
		optimize_linear_model(Rows, Options, 0, 0.0, InitialWeights, 0.0, Bias, Weights, Convergence, Iterations, FinalDelta),
		TrainingDiagnostics = [convergence(Convergence), iterations(Iterations), final_delta(FinalDelta)].

	optimize_linear_model(Rows, Options, Iteration, Bias0, Weights0, PreviousDelta, Bias, Weights, Convergence, Iterations, FinalDelta) :-
		::option(maximum_iterations(MaxIterations), Options),
		(   Iteration >= MaxIterations ->
			Bias = Bias0,
			Weights = Weights0,
			Convergence = maximum_iterations_exhausted,
			Iterations = Iteration,
			FinalDelta = PreviousDelta
		;   update_linear_parameters(Rows, Bias0, Weights0, Options, Bias1, Weights1, MaxDelta),
			::option(tolerance(Tolerance), Options),
			NextIteration is Iteration + 1,
			(   MaxDelta =< Tolerance ->
				Bias = Bias1,
				Weights = Weights1,
				Convergence = tolerance,
				Iterations = NextIteration,
				FinalDelta = MaxDelta
			;   optimize_linear_model(Rows, Options, NextIteration, Bias1, Weights1, MaxDelta, Bias, Weights, Convergence, Iterations, FinalDelta)
			)
		).

	update_linear_parameters(Rows, Bias0, Weights0, Options, Bias1, Weights1, MaxDelta) :-
		length(Rows, Count),
		zero_vector_like(Weights0, InitialGradientWeights),
		accumulate_linear_gradients(Rows, Bias0, Weights0, 0.0, InitialGradientWeights, GradientBias, GradientWeights),
		Scale is 1.0 / Count,
		::option(learning_rate(LearningRate), Options),
		::option(l2_regularization(Regularization), Options),
		MeanBiasGradient is GradientBias * Scale,
		Bias1 is Bias0 - LearningRate * MeanBiasGradient,
		BiasDelta is abs(Bias1 - Bias0),
		update_linear_weights(Weights0, GradientWeights, Scale, Regularization, LearningRate, Weights1, 0.0, MaxWeightDelta),
		MaxDelta is max(BiasDelta, MaxWeightDelta).

	accumulate_linear_gradients([], _, _, GradientBias, GradientWeights, GradientBias, GradientWeights).
	accumulate_linear_gradients([Features-Target| Rows], Bias, Weights, GradientBias0, GradientWeights0, GradientBias, GradientWeights) :-
		dot_product(Weights, Features, Linear),
		Prediction is Bias + Linear,
		Error is Prediction - Target,
		GradientBias1 is GradientBias0 + Error,
		add_scaled_vector(Features, Error, GradientWeights0, GradientWeights1),
		accumulate_linear_gradients(Rows, Bias, Weights, GradientBias1, GradientWeights1, GradientBias, GradientWeights).

	add_scaled_vector([], _, [], []).
	add_scaled_vector([Feature| Features], Scale, [Gradient| Gradients], [Updated| UpdatedGradients]) :-
		Updated is Gradient + Feature * Scale,
		add_scaled_vector(Features, Scale, Gradients, UpdatedGradients).

	update_linear_weights([], [], _, _, _, [], MaxDelta, MaxDelta).
	update_linear_weights([Weight0| Weights0], [Gradient| Gradients], Scale, Regularization, LearningRate, [Weight1| Weights1], MaxDelta0, MaxDelta) :-
		MeanGradient is Gradient * Scale + Regularization * Weight0,
		Weight1 is Weight0 - LearningRate * MeanGradient,
		Delta is abs(Weight1 - Weight0),
		MaxDelta1 is max(Delta, MaxDelta0),
		update_linear_weights(Weights0, Gradients, Scale, Regularization, LearningRate, Weights1, MaxDelta1, MaxDelta).

	zero_vector(0, []) :-
		!.
	zero_vector(Count, [0.0| Zeroes]) :-
		Count > 0,
		NextCount is Count - 1,
		zero_vector(NextCount, Zeroes).

	zero_vector_like([], []).
	zero_vector_like([_| Values], [0.0| Zeroes]) :-
		zero_vector_like(Values, Zeroes).

	valid_encoded_rows_([], _FeatureCount).
	valid_encoded_rows_([Features-Target| Rows], FeatureCount) :-
		valid(list(float, FeatureCount), Features),
		number(Target),
		valid_encoded_rows_(Rows, FeatureCount).

	valid_feature_labels_([]).
	valid_feature_labels_([feature(Attribute, value)| FeatureLabels]) :-
		atom(Attribute),
		valid_feature_labels_(FeatureLabels).
	valid_feature_labels_([feature(Attribute, missing)| FeatureLabels]) :-
		atom(Attribute),
		valid_feature_labels_(FeatureLabels).
	valid_feature_labels_([feature(Attribute, category(Value))| FeatureLabels]) :-
		atom(Attribute),
		nonvar(Value),
		valid_feature_labels_(FeatureLabels).

	valid_regression_tree_(leaf(Target), _FeatureCount) :-
		number(Target).
	valid_regression_tree_(node(Index, Threshold, Fallback, LeftTree, RightTree), FeatureCount) :-
		integer(Index),
		Index >= 1,
		Index =< FeatureCount,
		number(Threshold),
		number(Fallback),
		valid_regression_tree_(LeftTree, FeatureCount),
		valid_regression_tree_(RightTree, FeatureCount).

:- end_category.
