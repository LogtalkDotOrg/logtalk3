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
	implements(classifier_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-04-20,
		comment is 'Shared predicates for classifier diagnostics and export.'
	]).

	:- uses(format, [
		format/2,
		format/3
	]).

	:- uses(list, [
		member/2,
		memberchk/2
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

	diagnostics(Classifier, Diagnostics) :-
		::classifier_diagnostics_data(Classifier, Diagnostics).

	diagnostic(Classifier, Diagnostic) :-
		diagnostics(Classifier, Diagnostics),
		member(Diagnostic, Diagnostics).

	classifier_options(Classifier, Options) :-
		diagnostics(Classifier, Diagnostics),
		memberchk(options(Options), Diagnostics).

	print_classifier_template(Classifier) :-
		::classifier_term_template(Classifier, Template),
		format('Template: ~w~n', [Template]).

	classifier_to_file(Dataset, Classifier, Functor, File) :-
		::classifier_to_clauses(Dataset, Classifier, Functor, Clauses),
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

:- end_category.
