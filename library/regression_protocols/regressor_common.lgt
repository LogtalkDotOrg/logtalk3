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
	implements(regressor_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-21,
		comment is 'Shared predicates for regressor learning defaults, dataset validation, export, and pretty-print helpers.'
	]).

	:- uses(format, [
		format/2,
		format/3
	]).

	:- uses(list, [
		last/2
	]).

	:- protected(regressor_options/2).
	:- mode(regressor_options(+compound, -list(compound)), one).
	:- info(regressor_options/2, [
		comment is 'Hook predicate that importing regressor implementations may override in order to expose the effective regressor options.',
		argnames is ['Regressor', 'Options']
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

	learn(Dataset, Regressor) :-
		::learn(Dataset, Regressor, []).

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

	regressor_options(Regressor, Options) :-
		Regressor =.. [_| Arguments],
		last(Arguments, Options).

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
		(   ::regressor_options(Regressor, Options) ->
			format(Stream, '% options: ~q~n', [Options])
		;   true
		),
		format(Stream, '% ~w~n', [Template]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

:- end_category.
