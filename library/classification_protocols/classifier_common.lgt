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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'Shared predicates for classifier diagnostics and export.'
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		member/2, memberchk/2
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

	check_classifier(Classifier) :-
		(   ::classifier_diagnostics_data(Classifier, _Diagnostics) ->
			true
		;   domain_error(valid_classifier, Classifier)
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

	valid_classifier_metadata(Model, Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(Model), Diagnostics).

	valid_classifier_metadata(Model, Options, Diagnostics) :-
		valid_classifier_metadata(Model, Diagnostics),
		memberchk(options(Options), Diagnostics),
		catch(::check_options(Options), _Error, fail).

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

	valid_feature_types_([], _AllowedTypes).
	valid_feature_types_([FeatureType| FeatureTypes], AllowedTypes) :-
		memberchk(FeatureType, AllowedTypes),
		valid_feature_types_(FeatureTypes, AllowedTypes).

	valid_linear_encoders_([], _SeenAttributes).
	valid_linear_encoders_([continuous(Attribute, Mean, Scale)| Encoders], SeenAttributes) :-
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

:- end_category.
