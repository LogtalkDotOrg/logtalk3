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


:- category(clusterer_common,
	implements(clusterer_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-30,
		comment is 'Shared predicates for clusterer learning defaults, export, and common dataset and encoding helpers.'
	]).

	:- protected(dataset_attributes/2).
	:- mode(dataset_attributes(+object_identifier, -list(pair)), one).
	:- info(dataset_attributes/2, [
		comment is 'Collects the dataset attribute declarations as `Attribute-Values` pairs.',
		argnames is ['Dataset', 'Attributes']
	]).

	:- protected(valid_attribute_names/1).
	:- mode(valid_attribute_names(+list(atom)), zero_or_one).
	:- info(valid_attribute_names/1, [
		comment is 'True when a list of attribute names is a proper list of distinct atoms.',
		argnames is ['AttributeNames']
	]).

	:- protected(valid_continuous_encoders/1).
	:- mode(valid_continuous_encoders(+list(compound)), zero_or_one).
	:- info(valid_continuous_encoders/1, [
		comment is 'True when a list of encoders only contains valid continuous encoder terms with distinct attributes.',
		argnames is ['Encoders']
	]).

	:- protected(valid_discrete_encoders/1).
	:- mode(valid_discrete_encoders(+list(compound)), zero_or_one).
	:- info(valid_discrete_encoders/1, [
		comment is 'True when a list of encoders only contains valid discrete encoder terms with distinct attributes.',
		argnames is ['Encoders']
	]).

	:- protected(valid_mixed_encoders/1).
	:- mode(valid_mixed_encoders(+list(compound)), zero_or_one).
	:- info(valid_mixed_encoders/1, [
		comment is 'True when a list of encoders only contains valid continuous or discrete encoder terms with distinct attributes.',
		argnames is ['Encoders']
	]).

	:- protected(valid_mixed_vectors/2).
	:- mode(valid_mixed_vectors(+list(compound), +list), zero_or_one).
	:- info(valid_mixed_vectors/2, [
		comment is 'True when all vectors conform to the given continuous or discrete encoder specifications.',
		argnames is ['Encoders', 'Vectors']
	]).

	:- protected(valid_clusterer_metadata/3).
	:- mode(valid_clusterer_metadata(+atom, +list(compound), +list(compound)), zero_or_one).
	:- info(valid_clusterer_metadata/3, [
		comment is 'True when diagnostics metadata contains the expected model term and records the given effective options.',
		argnames is ['Model', 'Options', 'Diagnostics']
	]).

	:- protected(valid_diagnostic_count/3).
	:- mode(valid_diagnostic_count(+atom, +list(compound), +integer), zero_or_one).
	:- info(valid_diagnostic_count/3, [
		comment is 'True when diagnostics contains a count term with the given functor and integer value.',
		argnames is ['Functor', 'Diagnostics', 'Count']
	]).

	:- protected(valid_diagnostic_choice/3).
	:- mode(valid_diagnostic_choice(+atom, +list(compound), +list), zero_or_one).
	:- info(valid_diagnostic_choice/3, [
		comment is 'True when diagnostics contains a term with the given functor and a value selected from the allowed choices.',
		argnames is ['Functor', 'Diagnostics', 'Choices']
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

	:- protected(check_examples/3).
	:- mode(check_examples(+object_identifier, +list(atom), +list), one).
	:- info(check_examples/3, [
		comment is 'Checks that a continuous training dataset is non-empty and that all example values are numeric.',
		argnames is ['Dataset', 'AttributeNames', 'Examples']
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
		comment is 'Checks that a single example contains exactly the declared attributes and that all values are numeric.',
		argnames is ['AttributeNames', 'AttributeValues']
	]).

	:- protected(check_attribute_bindings/2).
	:- mode(check_attribute_bindings(+list(atom), +list(pair)), one).
	:- info(check_attribute_bindings/2, [
		comment is 'Checks that an attribute-value list contains each declared attribute exactly once and no undeclared attributes.',
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
	:- mode(known_attribute_values(+list, +atom, -list(number)), one).
	:- info(known_attribute_values/3, [
		comment is 'Collects the known numeric values for a given attribute across the training examples.',
		argnames is ['Examples', 'Attribute', 'Values']
	]).

	:- protected(examples_to_rows/3).
	:- mode(examples_to_rows(+list, +list(compound), -list(pair)), one).
	:- info(examples_to_rows/3, [
		comment is 'Encodes training examples into `Id-Features` rows using the object-local or imported encoder implementation.',
		argnames is ['Examples', 'Encoders', 'Rows']
	]).

	:- protected(encode_instance/3).
	:- mode(encode_instance(+list(compound), +list(pair), -list(number)), one).
	:- info(encode_instance/3, [
		comment is 'Encodes an instance using the learned continuous attribute encoders after checking that it contains exactly the declared attributes.',
		argnames is ['Encoders', 'AttributeValues', 'Features']
	]).

	:- protected(check_encoded_attribute_bindings/2).
	:- mode(check_encoded_attribute_bindings(+list(compound), +list(pair)), one).
	:- info(check_encoded_attribute_bindings/2, [
		comment is 'Checks that an attribute-value list contains each attribute described by the encoders exactly once and no undeclared attributes.',
		argnames is ['Encoders', 'AttributeValues']
	]).

	:- protected(normalize_continuous/4).
	:- mode(normalize_continuous(+number, +number, +number, -number), one).
	:- info(normalize_continuous/4, [
		comment is 'Normalizes a continuous value using the learned centering and scaling parameters.',
		argnames is ['Value', 'Mean', 'Scale', 'Feature']
	]).

	:- protected(check_cluster_count/2).
	:- mode(check_cluster_count(+integer, +integer), one).
	:- info(check_cluster_count/2, [
		comment is 'Checks that the requested cluster count does not exceed the number of examples.',
		argnames is ['K', 'Count']
	]).

	:- protected(take_first_k/3).
	:- mode(take_first_k(+integer, +list, -list), one).
	:- info(take_first_k/3, [
		comment is 'Collects the first `K` vectors from `Id-Vector` rows.',
		argnames is ['K', 'Rows', 'Vectors']
	]).

	:- protected(remove_candidate/3).
	:- mode(remove_candidate(+pair, +list(pair), -list(pair)), one).
	:- info(remove_candidate/3, [
		comment is 'Removes the first matching `Id-Vector` candidate from a candidate list.',
		argnames is ['Candidate', 'Candidates', 'RemainingCandidates']
	]).

	:- protected(clusterer_diagnostics_data/2).
	:- mode(clusterer_diagnostics_data(+compound, -list(compound)), one).
	:- info(clusterer_diagnostics_data/2, [
		comment is 'Hook predicate that importing clusterer implementations must define in order to expose diagnostics metadata.',
		argnames is ['Clusterer', 'Diagnostics']
	]).

	:- uses(format, [
		format/3
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Clusterer) :-
		::learn(Dataset, Clusterer, []).

	check_clusterer(Clusterer) :-
		(	var(Clusterer) ->
			instantiation_error
		;   ::clusterer_diagnostics_data(Clusterer, _Diagnostics) ->
			true
		;   domain_error(clusterer, Clusterer)
		).

	valid_clusterer(Clusterer) :-
		catch(::check_clusterer(Clusterer), _Error, fail).

	diagnostics(Clusterer, Diagnostics) :-
		::clusterer_diagnostics_data(Clusterer, Diagnostics).

	diagnostic(Clusterer, Diagnostic) :-
		diagnostics(Clusterer, Diagnostics),
		member(Diagnostic, Diagnostics).

	clusterer_options(Clusterer, Options) :-
		diagnostics(Clusterer, Diagnostics),
		memberchk(options(Options), Diagnostics).

	valid_attribute_names(AttributeNames) :-
		valid(list(atom), AttributeNames),
		valid_distinct_terms(AttributeNames).

	valid_continuous_encoders(Encoders) :-
		valid(list(compound), Encoders),
		valid_continuous_encoders(Encoders, []).

	valid_discrete_encoders(Encoders) :-
		valid(list(compound), Encoders),
		valid_discrete_encoders(Encoders, []).

	valid_mixed_encoders(Encoders) :-
		valid(list(compound), Encoders),
		valid_mixed_encoders(Encoders, []).

	valid_mixed_vectors(Encoders, Vectors) :-
		valid(list, Vectors),
		valid_mixed_vectors_(Vectors, Encoders).

	valid_clusterer_metadata(Model, Options, Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(options(Options), Diagnostics),
		catch(::check_options(Options), _Error, fail).

	valid_diagnostic_count(Functor, Diagnostics, Count) :-
		integer(Count),
		Count >= 0,
		Diagnostic =.. [Functor, Count],
		memberchk(Diagnostic, Diagnostics).

	valid_diagnostic_choice(Functor, Diagnostics, Choices) :-
		valid(list, Choices),
		Diagnostic =.. [Functor, Choice],
		memberchk(Diagnostic, Diagnostics),
		memberchk(Choice, Choices).

	export_to_clauses(_Dataset, Clusterer, Functor, [Clause]) :-
		Clusterer =.. [_| Arguments],
		Clause =.. [Functor| Arguments].

	export_to_file(Dataset, Clusterer, Functor, File) :-
		Clause =.. [Functor, Clusterer],
		open(File, write, Stream),
		write_comment_header(Dataset, Clusterer, Functor, Stream),
		write_clauses([Clause], Stream),
		close(Stream).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		),
		check_attribute_declarations(Attributes).

	check_attribute_declarations([]).
	check_attribute_declarations([Attribute-_| Attributes]) :-
		attribute_declaration_occurrences(Attributes, Attribute, 1, Count),
		(   Count == 1 ->
			true
		;   permission_error(repeat, attribute_declaration, Attribute)
		),
		remove_attribute_declarations(Attributes, Attribute, RemainingAttributes),
		check_attribute_declarations(RemainingAttributes).

	attribute_declaration_occurrences([], _Attribute, Count, Count).
	attribute_declaration_occurrences([Attribute-_Values| Attributes], Attribute, Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		attribute_declaration_occurrences(Attributes, Attribute, Count1, Count).
	attribute_declaration_occurrences([_OtherAttribute-_Values| Attributes], Attribute, Count0, Count) :-
		attribute_declaration_occurrences(Attributes, Attribute, Count0, Count).

	remove_attribute_declarations([], _Attribute, []).
	remove_attribute_declarations([Attribute-_Values| Attributes], Attribute, RemainingAttributes) :-
		!,
		remove_attribute_declarations(Attributes, Attribute, RemainingAttributes).
	remove_attribute_declarations([Declaration| Attributes], Attribute, [Declaration| RemainingAttributes]) :-
		remove_attribute_declarations(Attributes, Attribute, RemainingAttributes).

	check_continuous_attributes([]).
	check_continuous_attributes([Attribute-Values| Attributes]) :-
		(   Values == continuous ->
			true
		;   domain_error(continuous_attribute(Attribute), Values)
		),
		check_continuous_attributes(Attributes).

	check_examples_non_empty(Dataset, Examples) :-
		(   Examples == [] ->
			domain_error(non_empty_dataset, Dataset)
		;   true
		).

	check_examples(Dataset, AttributeNames, Examples) :-
		::check_examples_non_empty(Dataset, Examples),
		::check_example_values(Examples, AttributeNames).

	check_example_values([], _AttributeNames).
	check_example_values([_-AttributeValues| Examples], AttributeNames) :-
		::check_example_attributes(AttributeNames, AttributeValues),
		check_example_values(Examples, AttributeNames).

	check_example_attributes(AttributeNames, AttributeValues) :-
		::check_attribute_bindings(AttributeNames, AttributeValues),
		check_example_attributes_checked(AttributeNames, AttributeValues).

	check_example_attributes_checked([], _AttributeValues).
	check_example_attributes_checked([Attribute| Attributes], AttributeValues) :-
		::attribute_value(Attribute, AttributeValues, Value),
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
		;   domain_error(attribute_occurrences(Attribute, 1), Count)
		),
		check_declared_attribute_bindings(Attributes, AttributeValues).

	check_undeclared_attribute_bindings([], _AttributeNames).
	check_undeclared_attribute_bindings([Attribute-_Value| AttributeValues], AttributeNames) :-
		(   member(Attribute, AttributeNames) ->
			true
		;   domain_error(declared_attribute(AttributeNames), Attribute)
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

	continuous_stats(Attribute, Examples, Options, Mean, Scale) :-
		^^option(feature_scaling(FeatureScaling), Options),
		(   FeatureScaling == on ->
			::known_attribute_values(Examples, Attribute, Values),
			arithmetic_mean(Values, Mean),
			length(Values, Count),
			(   Count > 1 ->
				variance(Values, Variance)
			;   Variance = 0.0
			),
			(   Variance > 0.0 ->
				Scale is sqrt(Variance)
			;   Scale = 1.0
			)
		;   Mean = 0.0,
			Scale = 1.0
		).

	known_attribute_values([], _Attribute, []).
	known_attribute_values([_-AttributeValues| Examples], Attribute, [Value| Values]) :-
		::attribute_value(Attribute, AttributeValues, Value),
		known_attribute_values(Examples, Attribute, Values).

	examples_to_rows([], _Encoders, []).
	examples_to_rows([Id-AttributeValues| Examples], Encoders, [Id-Features| Rows]) :-
		::encode_instance(Encoders, AttributeValues, Features),
		examples_to_rows(Examples, Encoders, Rows).

	encode_instance(Encoders, AttributeValues, Features) :-
		::check_encoded_attribute_bindings(Encoders, AttributeValues),
		encode_instance_checked(Encoders, AttributeValues, Features).

	check_encoded_attribute_bindings(Encoders, AttributeValues) :-
		encoder_attribute_names(Encoders, AttributeNames),
		check_attribute_bindings(AttributeNames, AttributeValues).

	encoder_attribute_names([], []).
	encoder_attribute_names([continuous(Attribute, _Mean, _Scale)| Encoders], [Attribute| AttributeNames]) :-
		!,
		encoder_attribute_names(Encoders, AttributeNames).
	encoder_attribute_names([discrete(Attribute, _AllowedValues)| Encoders], [Attribute| AttributeNames]) :-
		encoder_attribute_names(Encoders, AttributeNames).

	encode_instance_checked([], _AttributeValues, []).
	encode_instance_checked([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature| Features]) :-
		::attribute_value(Attribute, AttributeValues, Value),
		::normalize_continuous(Value, Mean, Scale, Feature),
		encode_instance_checked(Encoders, AttributeValues, Features).

	normalize_continuous(Value, Mean, Scale, Feature) :-
		(   nonvar(Value) ->
			true
		;   instantiation_error
		),
		(   number(Value) ->
			true
		;   type_error(number, Value)
		),
		Feature is float((Value - Mean) / Scale).

	check_cluster_count(K, Count) :-
		(   K =< Count ->
			true
		;   domain_error(cluster_count(1, Count), K)
		).

	take_first_k(0, _Rows, []) :-
		!.
	take_first_k(K, [_-Vector| Rows], [Vector| Vectors]) :-
		K > 0,
		NextK is K - 1,
		take_first_k(NextK, Rows, Vectors).

	remove_candidate(Id-Vector, [Id-Vector| Candidates], Candidates) :-
		!.
	remove_candidate(Candidate, [Head| Candidates], [Head| RemainingCandidates]) :-
		remove_candidate(Candidate, Candidates, RemainingCandidates).

	write_comment_header(Dataset, Clusterer, Functor, Stream) :-
		format(Stream, '% exported clusterer predicate: ~q/~d~n', [Functor, 1]),
		format(Stream, '% training dataset: ~q~n', [Dataset]),
		(   ::diagnostics(Clusterer, Diagnostics) ->
			format(Stream, '% diagnostics: ~q~n', [Diagnostics])
		;   true
		),
		format(Stream, '% ~q(Clusterer)~n', [Functor]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	valid_continuous_encoders([], _Seen).
	valid_continuous_encoders([continuous(Attribute, Mean, Scale)| Encoders], Seen) :-
		atom(Attribute),
		valid(float, Mean),
		valid(positive_float, Scale),
		\+ member(Attribute, Seen),
		valid_continuous_encoders(Encoders, [Attribute| Seen]).

	valid_discrete_encoders([], _Seen).
	valid_discrete_encoders([discrete(Attribute, AllowedValues)| Encoders], Seen) :-
		atom(Attribute),
		valid_discrete_allowed_values(AllowedValues),
		\+ member(Attribute, Seen),
		valid_discrete_encoders(Encoders, [Attribute| Seen]).

	valid_mixed_encoders([], _Seen).
	valid_mixed_encoders([continuous(Attribute, Mean, Scale)| Encoders], Seen) :-
		atom(Attribute),
		valid(float, Mean),
		valid(positive_float, Scale),
		\+ member(Attribute, Seen),
		valid_mixed_encoders(Encoders, [Attribute| Seen]).
	valid_mixed_encoders([discrete(Attribute, AllowedValues)| Encoders], Seen) :-
		atom(Attribute),
		valid_discrete_allowed_values(AllowedValues),
		\+ member(Attribute, Seen),
		valid_mixed_encoders(Encoders, [Attribute| Seen]).

	valid_discrete_allowed_values(AllowedValues) :-
		valid(list, AllowedValues),
		AllowedValues \== [],
		type::valid(list(nonvar), AllowedValues),
		valid_distinct_terms(AllowedValues).

	valid_distinct_terms([]).
	valid_distinct_terms([Term| Terms]) :-
		\+ member(Term, Terms),
		valid_distinct_terms(Terms).

	valid_mixed_vectors_([], _Encoders).
	valid_mixed_vectors_([Vector| Vectors], Encoders) :-
		valid_mixed_vector(Encoders, Vector),
		valid_mixed_vectors_(Vectors, Encoders).

	valid_mixed_vector(Encoders, Vector) :-
		length(Encoders, Length),
		length(Vector, Length),
		valid_mixed_vector_values(Encoders, Vector).

	valid_mixed_vector_values([], []).
	valid_mixed_vector_values([continuous(_Attribute, _Mean, _Scale)| Encoders], [Value| Values]) :-
		number(Value),
		valid_mixed_vector_values(Encoders, Values).
	valid_mixed_vector_values([discrete(_Attribute, AllowedValues)| Encoders], [Value| Values]) :-
		nonvar(Value),
		memberchk(Value, AllowedValues),
		valid_mixed_vector_values(Encoders, Values).

:- end_category.
