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


:- category(classifier_common,
	implements(classifier_protocol),
	extends(options)).

	:- info([
		version is 2:0:1,
		author is 'Paulo Moura',
		date is 2026-06-14,
		comment is 'Shared predicates for classifier diagnostics, dataset validation, mixed-feature distance calculations, and export.'
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- protected(classifier_diagnostics_data/2).
	:- mode(classifier_diagnostics_data(+compound, -list(compound)), one).
	:- info(classifier_diagnostics_data/2, [
		comment is 'Hook predicate that importing classifier implementations must define in order to expose diagnostics metadata.',
		argnames is ['Classifier', 'Diagnostics']
	]).

	:- protected(classifier_export_template/4).
	:- mode(classifier_export_template(+object_identifier, +compound, +atom, -callable), one).
	:- info(classifier_export_template/4, [
		comment is 'Hook predicate that importing classifier implementations must define in order to expose the exported classifier template for a given functor.',
		argnames is ['Dataset', 'Classifier', 'Functor', 'Template']
	]).

	:- protected(classifier_term_template/2).
	:- mode(classifier_term_template(+compound, -callable), one).
	:- info(classifier_term_template/2, [
		comment is 'Hook predicate that importing classifier implementations must define in order to expose the learned classifier term template used by pretty-printing helpers.',
		argnames is ['Classifier', 'Template']
	]).

	:- protected(print_classifier_template/1).
	:- mode(print_classifier_template(+compound), one).
	:- info(print_classifier_template/1, [
		comment is 'Pretty-printing helper predicate used by importing classifier implementations to show the learned classifier term template.',
		argnames is ['Classifier']
	]).

	:- protected(valid_attribute_names/1).
	:- mode(valid_attribute_names(+list(atom)), zero_or_one).
	:- info(valid_attribute_names/1, [
		comment is 'True when a list of attribute names is a proper list of distinct atoms.',
		argnames is ['AttributeNames']
	]).

	:- protected(valid_class_values/1).
	:- mode(valid_class_values(+list(atom)), zero_or_one).
	:- info(valid_class_values/1, [
		comment is 'True when a list of class values is a non-empty proper list of distinct atoms.',
		argnames is ['ClassValues']
	]).

	:- protected(valid_feature_types/2).
	:- mode(valid_feature_types(+list, +list), zero_or_one).
	:- info(valid_feature_types/2, [
		comment is 'True when a list of feature type tags is non-empty and each tag belongs to the given allowed set.',
		argnames is ['FeatureTypes', 'AllowedTypes']
	]).

	:- protected(valid_discrete_values/1).
	:- mode(valid_discrete_values(+list), zero_or_one).
	:- info(valid_discrete_values/1, [
		comment is 'True when a list of categorical values is non-empty, contains only nonvar terms, and has no duplicates.',
		argnames is ['Values']
	]).

	:- protected(valid_linear_encoders/1).
	:- mode(valid_linear_encoders(+list(compound)), zero_or_one).
	:- info(valid_linear_encoders/1, [
		comment is 'True when a list of encoders only contains valid ``continuous/3`` or ``categorical/2`` encoder terms with distinct attributes.',
		argnames is ['Encoders']
	]).

	:- protected(dataset_attributes/2).
	:- mode(dataset_attributes(+object_identifier, -list(pair)), one).
	:- info(dataset_attributes/2, [
		comment is 'Collects the declared dataset attributes and their value domains.',
		argnames is ['Dataset', 'Attributes']
	]).

	:- protected(dataset_examples/2).
	:- mode(dataset_examples(+object_identifier, -list(compound)), one).
	:- info(dataset_examples/2, [
		comment is 'Collects the dataset training examples as ``Id-Class-AttributeValues`` terms.',
		argnames is ['Dataset', 'Examples']
	]).

	:- protected(check_examples_non_empty/2).
	:- mode(check_examples_non_empty(+object_identifier, +list), one).
	:- info(check_examples_non_empty/2, [
		comment is 'Checks that a training example collection is not empty.',
		argnames is ['Dataset', 'Examples']
	]).

	:- protected(check_examples/2).
	:- mode(check_examples(+object_identifier, +list), one_or_error).
	:- info(check_examples/2, [
		comment is 'Checks that a training dataset is non-empty, that all example classes belong to the declared class values, and that provided attribute bindings use declared attributes with values matching the declared domains. Missing attribute bindings are allowed.',
		argnames is ['Dataset', 'Examples'],
		exceptions is [
			'``Examples`` is empty' - domain_error(non_empty_dataset, 'Dataset'),
			'``Dataset`` class values declaration is invalid' - domain_error(class_values, 'ClassValues'),
			'An example class is not one of the declared class values' - domain_error(dataset_example_class, 'Class'),
			'An example attribute binding uses an undeclared attribute or a value outside the declared attribute domain' - domain_error(dataset_example_attribute_values, 'AttributeValues')
		]
	]).

	:- protected(check_complete_examples/2).
	:- mode(check_complete_examples(+object_identifier, +list), one_or_error).
	:- info(check_complete_examples/2, [
		comment is 'Checks that a training dataset is non-empty, that all example classes belong to the declared class values, and that each example contains every declared attribute exactly once with values matching the declared domains. Missing values represented using variables are allowed.',
		argnames is ['Dataset', 'Examples'],
		exceptions is [
			'``Examples`` is empty' - domain_error(non_empty_dataset, 'Dataset'),
			'``Dataset`` class values declaration is invalid' - domain_error(class_values, 'ClassValues'),
			'An example class is not one of the declared class values' - domain_error(dataset_example_class, 'Class'),
			'An example attribute set is missing a declared attribute, contains a duplicate attribute, uses an undeclared attribute, or contains a non-variable value outside the declared attribute domain' - domain_error(dataset_example_attribute_values, 'AttributeValues')
		]
	]).

	:- protected(check_complete_examples_nonvar/2).
	:- mode(check_complete_examples_nonvar(+object_identifier, +list), one_or_error).
	:- info(check_complete_examples_nonvar/2, [
		comment is 'Checks that a training dataset is non-empty, that all example classes belong to the declared class values, and that each example contains every declared attribute exactly once with non-variable values matching the declared domains.',
		argnames is ['Dataset', 'Examples'],
		exceptions is [
			'``Examples`` is empty' - domain_error(non_empty_dataset, 'Dataset'),
			'``Dataset`` class values declaration is invalid' - domain_error(class_values, 'ClassValues'),
			'An example class is not one of the declared class values' - domain_error(dataset_example_class, 'Class'),
			'An example attribute set is missing a declared attribute, contains a duplicate attribute, uses an undeclared attribute, contains a variable value, or contains a value outside the declared attribute domain' - domain_error(dataset_example_attribute_values, 'AttributeValues')
		]
	]).

	:- protected(build_linear_encoders/4).
	:- mode(build_linear_encoders(+list(pair), +list(compound), +boolean, -list(compound)), one).
	:- info(build_linear_encoders/4, [
		comment is 'Builds linear-model encoders for continuous and categorical attributes. Continuous encoders optionally standardize features when ``FeatureScaling`` is true.',
		argnames is ['Attributes', 'Examples', 'FeatureScaling', 'Encoders']
	]).

	:- protected(examples_to_linear_rows/3).
	:- mode(examples_to_linear_rows(+list(compound), +list(compound), -list(pair)), one).
	:- info(examples_to_linear_rows/3, [
		comment is 'Encodes examples into ``Features-Class`` rows using the given linear encoders.',
		argnames is ['Examples', 'Encoders', 'Rows']
	]).

	:- protected(encode_linear_instance/3).
	:- mode(encode_linear_instance(+list(compound), +list(pair), -list(float)), one).
	:- info(encode_linear_instance/3, [
		comment is 'Encodes an instance into a numeric feature vector using the given linear encoders.',
		argnames is ['Encoders', 'Instance', 'Features']
	]).

	:- protected(linear_encoders_feature_count/2).
	:- mode(linear_encoders_feature_count(+list(compound), -integer), one).
	:- info(linear_encoders_feature_count/2, [
		comment is 'Computes the number of numeric features produced by a set of linear encoders.',
		argnames is ['Encoders', 'Count']
	]).

	:- protected(valid_classifier_metadata/2).
	:- mode(valid_classifier_metadata(+atom, +list(compound)), zero_or_one).
	:- info(valid_classifier_metadata/2, [
		comment is 'True when diagnostics metadata contains the expected model term.',
		argnames is ['Model', 'Diagnostics']
	]).

	:- protected(valid_classifier_metadata/3).
	:- mode(valid_classifier_metadata(+atom, +list(compound), +list(compound)), zero_or_one).
	:- info(valid_classifier_metadata/3, [
		comment is 'True when diagnostics metadata contains the expected model term and records the given effective options.',
		argnames is ['Model', 'Options', 'Diagnostics']
	]).

	:- protected(mixed_feature_distance/5).
	:- mode(mixed_feature_distance(+term, +list, +list, +list, -float), one_or_error).
	:- info(mixed_feature_distance/5, [
		comment is 'Computes a distance between two mixed-feature vectors using the given feature types and one of the supported metrics ``euclidean``, ``manhattan``, ``chebyshev``, ``cosine``, or ``minkowski(Order)``.',
		argnames is ['Metric', 'FeatureTypes', 'Values1', 'Values2', 'Distance'],
		exceptions is [
			'``Metric`` is ``minkowski(Order)`` and ``Order`` is not a positive number' - domain_error(positive_number, 'Order')
		]
	]).

	learn(Dataset, Classifier) :-
		::learn(Dataset, Classifier, []).

	check_classifier(Classifier) :-
		(	var(Classifier) ->
			instantiation_error
		;	::classifier_diagnostics_data(Classifier, _Diagnostics) ->
			true
		;	domain_error(classifier, Classifier)
		).

	valid_classifier(Classifier) :-
		catch(::check_classifier(Classifier), _Error, fail).

	diagnostics(Classifier, Diagnostics) :-
		::classifier_diagnostics_data(Classifier, Diagnostics).

	diagnostic(Classifier, Diagnostic) :-
		diagnostics(Classifier, Diagnostics),
		member(Diagnostic, Diagnostics).

	classifier_options(Classifier, Options) :-
		diagnostics(Classifier, Diagnostics),
		memberchk(options(Options), Diagnostics).

	valid_attribute_names(AttributeNames) :-
		valid(list(atom), AttributeNames),
		valid_distinct_terms(AttributeNames).

	valid_class_values(ClassValues) :-
		valid(list(atom), ClassValues),
		ClassValues \== [],
		valid_distinct_terms(ClassValues).

	valid_feature_types(FeatureTypes, AllowedTypes) :-
		valid(list, FeatureTypes),
		FeatureTypes \== [],
		valid(list, AllowedTypes),
		AllowedTypes \== [],
		valid_feature_types_(FeatureTypes, AllowedTypes).

	valid_discrete_values(Values) :-
		valid(list(nonvar), Values),
		Values \== [],
		valid_distinct_terms(Values).

	valid_linear_encoders(Encoders) :-
		valid(list(compound), Encoders),
		valid_linear_encoders_(Encoders, []).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		),
		check_attribute_declarations(Attributes).

	dataset_examples(Dataset, Examples) :-
		findall(
			Id-Class-AttributeValues,
			Dataset::example(Id, Class, AttributeValues),
			Examples
		).

	check_examples_non_empty(Dataset, Examples) :-
		(	Examples == [] ->
			domain_error(non_empty_dataset, Dataset)
		;	true
		).

	check_examples(Dataset, Examples) :-
		::check_examples_non_empty(Dataset, Examples),
		Dataset::class_values(ClassValues),
		(	valid_class_values(ClassValues) ->
			true
		;	domain_error(class_values, ClassValues)
		),
		::dataset_attributes(Dataset, Attributes),
		check_examples(Examples, ClassValues, Attributes).

	check_complete_examples(Dataset, Examples) :-
		::check_examples_non_empty(Dataset, Examples),
		Dataset::class_values(ClassValues),
		(	valid_class_values(ClassValues) ->
			true
		;	domain_error(class_values, ClassValues)
		),
		::dataset_attributes(Dataset, Attributes),
		check_complete_examples(Examples, ClassValues, Attributes).

	check_complete_examples_nonvar(Dataset, Examples) :-
		::check_examples_non_empty(Dataset, Examples),
		Dataset::class_values(ClassValues),
		(	valid_class_values(ClassValues) ->
			true
		;	domain_error(class_values, ClassValues)
		),
		::dataset_attributes(Dataset, Attributes),
		check_complete_examples_nonvar(Examples, ClassValues, Attributes).

	build_linear_encoders([], _, _FeatureScaling, []).
	build_linear_encoders([Attribute-Values| Rest], Examples, FeatureScaling, [Encoder| Encoders]) :-
		(	Values == continuous ->
			continuous_stats(Attribute, Examples, FeatureScaling, Mean, Scale),
			Encoder = continuous(Attribute, Mean, Scale)
		;	Encoder = categorical(Attribute, Values)
		),
		build_linear_encoders(Rest, Examples, FeatureScaling, Encoders).

	examples_to_linear_rows([], _Encoders, []).
	examples_to_linear_rows([_-Class-AttributeValues| Examples], Encoders, [Features-Class| Rows]) :-
		encode_linear_instance(Encoders, AttributeValues, Features),
		examples_to_linear_rows(Examples, Encoders, Rows).

	encode_linear_instance([], _, []).
	encode_linear_instance([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature, Missing| Features]) :-
		!,
		(	memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			normalize_continuous(Value, Mean, Scale, Feature),
			Missing = 0.0
		;	Feature = 0.0,
			Missing = 1.0
		),
		encode_linear_instance(Encoders, AttributeValues, Features).
	encode_linear_instance([categorical(Attribute, Values)| Encoders], AttributeValues, Features) :-
		(	memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			check_categorical_value(Attribute, Values, Value),
			one_hot_encode(Values, Value, Encoded)
		;	missing_one_hot_encode(Values, Encoded)
		),
		append(Encoded, RestFeatures, Features),
		encode_linear_instance(Encoders, AttributeValues, RestFeatures).

	linear_encoders_feature_count(Encoders, Count) :-
		linear_encoders_feature_count(Encoders, 0, Count).

	valid_classifier_metadata(Model, Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(Model), Diagnostics).

	valid_classifier_metadata(Model, Options, Diagnostics) :-
		valid_classifier_metadata(Model, Diagnostics),
		memberchk(options(Options), Diagnostics),
		catch(::check_options(Options), _Error, fail).

	mixed_feature_distance(euclidean, FeatureTypes, Values1, Values2, Distance) :-
		sum_squared_feature_differences(FeatureTypes, Values1, Values2, 0.0, SumSquares),
		Distance is sqrt(SumSquares).
	mixed_feature_distance(manhattan, FeatureTypes, Values1, Values2, Distance) :-
		sum_absolute_feature_differences(FeatureTypes, Values1, Values2, 0.0, Distance).
	mixed_feature_distance(chebyshev, FeatureTypes, Values1, Values2, Distance) :-
		max_absolute_feature_difference(FeatureTypes, Values1, Values2, 0.0, Distance).
	mixed_feature_distance(minkowski(Order), FeatureTypes, Values1, Values2, Distance) :-
		(	Order > 0.0 ->
			sum_powered_feature_differences(FeatureTypes, Values1, Values2, Order, 0.0, Sum),
			Distance is Sum ** (1.0 / Order)
		;	domain_error(positive_number, Order)
		).
	mixed_feature_distance(cosine, FeatureTypes, Values1, Values2, Distance) :-
		numeric_feature_dot_product(FeatureTypes, Values1, Values2, 0.0, DotProduct),
		numeric_feature_magnitude(FeatureTypes, Values1, Magnitude1),
		numeric_feature_magnitude(FeatureTypes, Values2, Magnitude2),
		(	(Magnitude1 =:= 0 ; Magnitude2 =:= 0) ->
			Distance = 1.0
		;	Similarity is float(DotProduct / (Magnitude1 * Magnitude2)),
			Distance is float(1.0 - Similarity)
		).

	print_classifier_template(Classifier) :-
		::classifier_term_template(Classifier, Template),
		format('Template: ~w~n', [Template]).

	export_to_file(Dataset, Classifier, Functor, File) :-
		::export_to_clauses(Dataset, Classifier, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Dataset, Functor, Classifier, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_comment_header(Dataset, Functor, Classifier, Stream) :-
		::classifier_export_template(Dataset, Classifier, Functor, Template),
		functor(Template, _, Arity),
		format(Stream, '% exported classifier predicate: ~q/~d~n', [Functor, Arity]),
		format(Stream, '% training dataset: ~q~n', [Dataset]),
		dataset_prediction_schema(Dataset, Functor, Schema),
		format(Stream, '% dataset prediction schema: ~w~n', [Schema]),
		(	::diagnostics(Classifier, Diagnostics) ->
			format(Stream, '% diagnostics: ~q~n', [Diagnostics])
		;	true
		),
		format(Stream, '% ~w~n', [Template]).

	dataset_prediction_schema(Dataset, Functor, Schema) :-
		Dataset::class(Class),
		findall(
			Attribute,
			Dataset::attribute_values(Attribute, _),
			Arguments,
			[Class]
		),
		title_case(Arguments, TitleCaseArguments),
		Schema =.. [Functor| TitleCaseArguments].

	% assumes ASCII attribute and class names
	title_case([], []).
	title_case([Name| Names], [TitleCaseName| TitleCaseNames]) :-
		atom_codes(Name, [Letter| Letters]),
		(	0'a @=< Letter, Letter @=< 0'z ->
			UpperCaseLetter is Letter - 32,
			atom_codes(TitleCaseName, [UpperCaseLetter| Letters])
		;	TitleCaseName = Name
		),
		title_case(Names, TitleCaseNames).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	valid_distinct_terms([]).
	valid_distinct_terms([Term| Terms]) :-
		\+ member(Term, Terms),
		valid_distinct_terms(Terms).

	check_attribute_declarations([]).
	check_attribute_declarations([Attribute-Values| Attributes]) :-
		(	atom(Attribute),
			\+ member(Attribute-_, Attributes),
			(	Values == continuous
			;	valid_discrete_values(Values)
			) ->
			true
		;	domain_error(attribute_declarations, Attribute)
		),
		check_attribute_declarations(Attributes).

	check_examples([], _ClassValues, _Attributes).
	check_examples([_-Class-AttributeValues| Examples], ClassValues, Attributes) :-
		check_example_class(Class, ClassValues),
		check_example_attributes(Attributes, AttributeValues),
		check_examples(Examples, ClassValues, Attributes).

	check_complete_examples([], _ClassValues, _Attributes).
	check_complete_examples([_-Class-AttributeValues| Examples], ClassValues, Attributes) :-
		check_example_class(Class, ClassValues),
		check_complete_example_attributes(Attributes, AttributeValues),
		check_complete_examples(Examples, ClassValues, Attributes).

	check_complete_examples_nonvar([], _ClassValues, _Attributes).
	check_complete_examples_nonvar([_-Class-AttributeValues| Examples], ClassValues, Attributes) :-
		check_example_class(Class, ClassValues),
		check_complete_example_attributes_nonvar(Attributes, AttributeValues),
		check_complete_examples_nonvar(Examples, ClassValues, Attributes).

	check_example_class(Class, ClassValues) :-
		(	atom(Class) ->
			(	member(Class, ClassValues) ->
				true
			;	existence_error(class_value, Class)
			)
		;	type_error(atom, Class)
		).

	check_example_attributes(Attributes, AttributeValues) :-
		declared_attribute_names(Attributes, AttributeNames),
		check_attribute_bindings(AttributeNames, AttributeValues),
		check_attribute_values(AttributeValues, Attributes).

	check_complete_example_attributes(Attributes, AttributeValues) :-
		declared_attribute_names(Attributes, AttributeNames),
		check_complete_attribute_bindings(AttributeNames, AttributeValues),
		check_attribute_values(AttributeValues, Attributes).

	check_complete_example_attributes_nonvar(Attributes, AttributeValues) :-
		declared_attribute_names(Attributes, AttributeNames),
		check_complete_attribute_bindings(AttributeNames, AttributeValues),
		check_nonvar_attribute_values(AttributeValues, Attributes).

	check_attribute_bindings(AttributeNames, AttributeValues) :-
		check_declared_attribute_bindings(AttributeNames, AttributeValues),
		check_undeclared_attribute_bindings(AttributeValues, AttributeNames).

	check_complete_attribute_bindings(AttributeNames, AttributeValues) :-
		check_required_attribute_bindings(AttributeNames, AttributeValues),
		check_undeclared_attribute_bindings(AttributeValues, AttributeNames).

	check_declared_attribute_bindings([], _AttributeValues).
	check_declared_attribute_bindings([Attribute| Attributes], AttributeValues) :-
		attribute_occurrences(AttributeValues, Attribute, 0, Count),
		(	Count =< 1 ->
			true
		;	domain_error(attribute_occurrences(Attribute, 1), Count)
		),
		check_declared_attribute_bindings(Attributes, AttributeValues).

	check_required_attribute_bindings([], _AttributeValues).
	check_required_attribute_bindings([Attribute| Attributes], AttributeValues) :-
		attribute_occurrences(AttributeValues, Attribute, 0, Count),
		(	Count == 1 ->
			true
		;	Count == 0 ->
			existence_error(attribute, Attribute)
		;	domain_error(attribute_occurrences(Attribute, 1), Count)
		),
		check_required_attribute_bindings(Attributes, AttributeValues).

	check_undeclared_attribute_bindings([], _AttributeNames).
	check_undeclared_attribute_bindings([Attribute-_Value| AttributeValues], AttributeNames) :-
		(	member(Attribute, AttributeNames) ->
			true
		;	domain_error(declared_attribute(AttributeNames), Attribute)
		),
		check_undeclared_attribute_bindings(AttributeValues, AttributeNames).

	check_attribute_values([], _Attributes).
	check_attribute_values([Attribute-Value| AttributeValues], Attributes) :-
		memberchk(Attribute-Values, Attributes),
		(	nonvar(Value) ->
			check_attribute_value(Values, Attribute, Value)
		;	true
		),
		check_attribute_values(AttributeValues, Attributes).

	check_nonvar_attribute_values([], _Attributes).
	check_nonvar_attribute_values([Attribute-Value| AttributeValues], Attributes) :-
		memberchk(Attribute-Values, Attributes),
		(	nonvar(Value) ->
			check_attribute_value(Values, Attribute, Value)
		;	instantiation_error
		),
		check_nonvar_attribute_values(AttributeValues, Attributes).

	check_attribute_value(continuous, _Attribute, Value) :-
		!,
		(	number(Value) ->
			true
		;	type_error(number, Value)
		).
	check_attribute_value(Values, Attribute, Value) :-
		check_categorical_value(Attribute, Values, Value).

	declared_attribute_names([], []).
	declared_attribute_names([Attribute-_| Attributes], [Attribute| AttributeNames]) :-
		declared_attribute_names(Attributes, AttributeNames).

	attribute_occurrences([], _Attribute, Count, Count).
	attribute_occurrences([Attribute-_Value| AttributeValues], Attribute, Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		attribute_occurrences(AttributeValues, Attribute, Count1, Count).
	attribute_occurrences([_OtherAttribute-_Value| AttributeValues], Attribute, Count0, Count) :-
		attribute_occurrences(AttributeValues, Attribute, Count0, Count).

	valid_feature_types_([], _AllowedTypes).
	valid_feature_types_([FeatureType| FeatureTypes], AllowedTypes) :-
		memberchk(FeatureType, AllowedTypes),
		valid_feature_types_(FeatureTypes, AllowedTypes).

	valid_linear_encoders_([], _SeenAttributes).
	valid_linear_encoders_([continuous(Attribute, Mean, Scale)| Encoders], SeenAttributes) :-
		!,
		atom(Attribute),
		valid(float, Mean),
		valid(positive_float, Scale),
		\+ member(Attribute, SeenAttributes),
		valid_linear_encoders_(Encoders, [Attribute| SeenAttributes]).
	valid_linear_encoders_([categorical(Attribute, Values)| Encoders], SeenAttributes) :-
		atom(Attribute),
		valid_discrete_values(Values),
		\+ member(Attribute, SeenAttributes),
		valid_linear_encoders_(Encoders, [Attribute| SeenAttributes]).

	continuous_stats(Attribute, Examples, FeatureScaling, Mean, Scale) :-
		known_attribute_values(Examples, Attribute, Values),
		(	Values == [] ->
			Mean = 0.0,
			Scale = 1.0
		;	FeatureScaling == true ->
			arithmetic_mean(Values, Mean),
			length(Values, Count),
			(	Count > 1 ->
				variance(Values, Variance)
			;	Variance = 0.0
			),
			(	Variance > 0.0 ->
				Scale is sqrt(Variance)
			;	Scale = 1.0
			)
		;	Mean = 0.0,
			Scale = 1.0
		).

	known_attribute_values([], _, []).
	known_attribute_values([_-_-AttributeValues| Examples], Attribute, Values) :-
		(	memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			Values = [Value| Rest]
		;	Values = Rest
		),
		known_attribute_values(Examples, Attribute, Rest).

	normalize_continuous(Value, Mean, Scale, Feature) :-
		(	number(Value) ->
			true
		;	type_error(number, Value)
		),
		Feature is (Value - Mean) / Scale.

	one_hot_encode([], _, [0.0]).
	one_hot_encode([Category| Categories], Value, [Feature| Features]) :-
		(	Value == Category ->
			Feature = 1.0
		;	Feature = 0.0
		),
		one_hot_encode(Categories, Value, Features).

	missing_one_hot_encode([], [1.0]).
	missing_one_hot_encode([_| Values], [0.0| Zeroes]) :-
		missing_one_hot_encode(Values, Zeroes).

	check_categorical_value(Attribute, Values, Value) :-
		(	member(Value, Values) ->
			true
		;	domain_error(attribute_value(Attribute, Values), Value)
		).

	linear_encoders_feature_count([], Count, Count).
	linear_encoders_feature_count([continuous(_, _, _)| Encoders], Count0, Count) :-
		!,
		Count1 is Count0 + 2,
		linear_encoders_feature_count(Encoders, Count1, Count).
	linear_encoders_feature_count([categorical(_, Values)| Encoders], Count0, Count) :-
		length(Values, ValueCount),
		Count1 is Count0 + ValueCount + 1,
		linear_encoders_feature_count(Encoders, Count1, Count).

	sum_squared_feature_differences([], [], [], SumSquares, SumSquares).
	sum_squared_feature_differences([FeatureType| FeatureTypes], [Value1| Values1], [Value2| Values2], SumSquares0, SumSquares) :-
		feature_distance_squared(FeatureType, Value1, Value2, DistanceSquared),
		SumSquares1 is SumSquares0 + DistanceSquared,
		sum_squared_feature_differences(FeatureTypes, Values1, Values2, SumSquares1, SumSquares).

	sum_absolute_feature_differences([], [], [], SumDistances, SumDistances).
	sum_absolute_feature_differences([FeatureType| FeatureTypes], [Value1| Values1], [Value2| Values2], SumDistances0, SumDistances) :-
		feature_distance_absolute(FeatureType, Value1, Value2, Distance),
		SumDistances1 is SumDistances0 + Distance,
		sum_absolute_feature_differences(FeatureTypes, Values1, Values2, SumDistances1, SumDistances).

	max_absolute_feature_difference([], [], [], MaxDistance, MaxDistance).
	max_absolute_feature_difference([FeatureType| FeatureTypes], [Value1| Values1], [Value2| Values2], MaxDistance0, MaxDistance) :-
		feature_distance_absolute(FeatureType, Value1, Value2, Distance),
		MaxDistance1 is max(MaxDistance0, Distance),
		max_absolute_feature_difference(FeatureTypes, Values1, Values2, MaxDistance1, MaxDistance).

	sum_powered_feature_differences([], [], [], _Order, SumDistances, SumDistances).
	sum_powered_feature_differences([FeatureType| FeatureTypes], [Value1| Values1], [Value2| Values2], Order, SumDistances0, SumDistances) :-
		feature_distance_absolute(FeatureType, Value1, Value2, Distance),
		PoweredDistance is Distance ** Order,
		SumDistances1 is SumDistances0 + PoweredDistance,
		sum_powered_feature_differences(FeatureTypes, Values1, Values2, Order, SumDistances1, SumDistances).

	feature_distance_squared(numeric, Value1, Value2, DistanceSquared) :-
		Difference is Value1 - Value2,
		DistanceSquared is Difference * Difference.
	feature_distance_squared(categorical, Value, Value, 0) :-
		!.
	feature_distance_squared(categorical, _, _, 1).

	feature_distance_absolute(numeric, Value1, Value2, Distance) :-
		Distance is abs(Value1 - Value2).
	feature_distance_absolute(categorical, Value, Value, 0) :-
		!.
	feature_distance_absolute(categorical, _, _, 1).

	numeric_feature_dot_product([], [], [], DotProduct, DotProduct).
	numeric_feature_dot_product([numeric| FeatureTypes], [Value1| Values1], [Value2| Values2], DotProduct0, DotProduct) :-
		!,
		DotProduct1 is DotProduct0 + Value1 * Value2,
		numeric_feature_dot_product(FeatureTypes, Values1, Values2, DotProduct1, DotProduct).
	numeric_feature_dot_product([categorical| FeatureTypes], [_| Values1], [_| Values2], DotProduct0, DotProduct) :-
		numeric_feature_dot_product(FeatureTypes, Values1, Values2, DotProduct0, DotProduct).

	numeric_feature_magnitude(FeatureTypes, Values, Magnitude) :-
		numeric_feature_sum_squares(FeatureTypes, Values, 0.0, SumSquares),
		Magnitude is sqrt(SumSquares).

	numeric_feature_sum_squares([], [], SumSquares, SumSquares).
	numeric_feature_sum_squares([numeric| FeatureTypes], [Value| Values], SumSquares0, SumSquares) :-
		!,
		SumSquares1 is SumSquares0 + Value * Value,
		numeric_feature_sum_squares(FeatureTypes, Values, SumSquares1, SumSquares).
	numeric_feature_sum_squares([categorical| FeatureTypes], [_| Values], SumSquares0, SumSquares) :-
		numeric_feature_sum_squares(FeatureTypes, Values, SumSquares0, SumSquares).

:- end_category.
