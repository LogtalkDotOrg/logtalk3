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
		date is 2026-05-01,
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
	:- mode(check_examples(+object_identifier, +list), one).
	:- info(check_examples/2, [
		comment is 'Validates that the collected examples list is non-empty and only contains numeric targets.',
		argnames is ['Dataset', 'Examples']
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

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		length/2, last/2, member/2, memberchk/2
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
		check_targets(Examples).

	check_targets([]).
	check_targets([example(_Id, Target, _AttributeValues)| Examples]) :-
		(	number(Target) ->
			true
		;	type_error(number, Target)
		),
		check_targets(Examples).

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

	encoded_feature_count([], 0).
	encoded_feature_count([continuous(_, _, _)| Encoders], Count) :-
		!,
		encoded_feature_count(Encoders, RestCount),
		Count is RestCount + 2.
	encoded_feature_count([categorical(_, Values)| Encoders], Count) :-
		length(Values, ValueCount),
		encoded_feature_count(Encoders, RestCount),
		Count is RestCount + ValueCount + 1.

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
