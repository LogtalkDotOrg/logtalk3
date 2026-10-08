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


:- protocol(feature_selector_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Protocol for feature selection models.',
		see_also is [feature_dataset_protocol, feature_scoring_protocol]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one_or_error).
	:- info(learn/3, [
		comment is 'Learns a feature selector from the given dataset object using the specified options.',
		argnames is ['Dataset', 'Selector', 'Options'],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'A chi-square expectation is below the requested minimum' - domain_error(chi_square_expected_count, 'Feature-expected(ObservedMinimum,RequiredMinimum)'),
			'A generated regularization bound exceeds backend floating-point limits' - evaluation_error(float_overflow),
			'A feature declaration is unsupported by the receiving selector' - domain_error(feature_type, 'Feature-Declaration'),
			'A complete feature value required to be numeric is not numeric' - type_error(number, 'Value'),
			'A complete categorical target is not atomic' - type_error(atomic, 'Target'),
			'A dataset feature name, feature list, or declared count is a variable' - instantiation_error,
			'A dataset feature name is not atomic' - type_error(atomic, 'Feature'),
			'An example feature list is not a list' - type_error(list, 'Features'),
			'An example feature entry is not a pair' - type_error(pair, 'Entry'),
			'The declared count is not an integer' - type_error(integer, 'DeclaredCount'),
			'The declared count is not positive' - domain_error(positive_integer, 'DeclaredCount'),
			'A feature is declared or supplied more than once' - domain_error(duplicate_feature, 'Feature'),
			'An example names an undeclared feature' - domain_error(unknown_feature, 'Feature'),
			'The dataset contains no examples' - domain_error(non_empty_examples, 'Dataset'),
			'The declared and observed example counts differ' - consistency_error(example_count, 'DeclaredCount', 'ObservedCount')
		]
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -compound), one_or_error).
	:- info(learn/2, [
		comment is 'Learns a feature selector from the given dataset object using default options.',
		argnames is ['Dataset', 'Selector'],
		exceptions is [
			'A feature declaration is unsupported by the receiving selector' - domain_error(feature_type, 'Feature-Declaration'),
			'A complete feature value required to be numeric is not numeric' - type_error(number, 'Value'),
			'A complete categorical target is not atomic' - type_error(atomic, 'Target'),
			'A dataset feature name, feature list, or declared count is a variable' - instantiation_error,
			'A dataset feature name is not atomic' - type_error(atomic, 'Feature'),
			'An example feature list is not a list' - type_error(list, 'Features'),
			'An example feature entry is not a pair' - type_error(pair, 'Entry'),
			'The declared count is not an integer' - type_error(integer, 'DeclaredCount'),
			'The declared count is not positive' - domain_error(positive_integer, 'DeclaredCount'),
			'A feature is declared or supplied more than once' - domain_error(duplicate_feature, 'Feature'),
			'An example names an undeclared feature' - domain_error(unknown_feature, 'Feature'),
			'The dataset contains no examples' - domain_error(non_empty_examples, 'Dataset'),
			'The declared and observed example counts differ' - consistency_error(example_count, 'DeclaredCount', 'ObservedCount')
		]
	]).

	:- public(selected_features/2).
	:- mode(selected_features(+compound, -list(atomic)), one).
	:- info(selected_features/2, [
		comment is 'Returns the features selected by the learned selector.',
		argnames is ['Selector', 'Features']
	]).

	:- public(feature_scores/2).
	:- mode(feature_scores(+compound, -list(pair)), one).
	:- info(feature_scores/2, [
		comment is 'Returns every candidate feature with its relevance score, as a list of ``Feature-Score`` pairs sorted by decreasing score (not just the selected ones; see selected_features/2). Shared filter implementations preserve declaration order for numeric ties.',
		argnames is ['Selector', 'FeatureScores']
	]).

	:- public(check_selector/1).
	:- mode(check_selector(@compound), one_or_error).
	:- info(check_selector/1, [
		comment is 'Checks that a learned selector term is structurally valid for the receiving implementation. Throws an exception when the term is not a valid selector representation.',
		argnames is ['Selector'],
		exceptions is [
			'``Selector`` is a variable' - instantiation_error,
			'``Selector`` is neither a variable nor a valid selector' - domain_error(selector, 'Selector')
		]
	]).

	:- public(valid_selector/1).
	:- mode(valid_selector(@compound), zero_or_one).
	:- info(valid_selector/1, [
		comment is 'True when a learned selector term is structurally valid for the receiving implementation. Succeeds iff check_selector/1 succeeds without throwing an exception.',
		argnames is ['Selector']
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+compound, -list(compound)), one).
	:- info(diagnostics/2, [
		comment is 'Returns diagnostics metadata for a learned selector.',
		argnames is ['Selector', 'Diagnostics']
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+compound, ?compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Enumerates individual diagnostics metadata terms for a learned selector.',
		argnames is ['Selector', 'Diagnostic']
	]).

	:- public(selector_options/2).
	:- mode(selector_options(+compound, -list(compound)), one).
	:- info(selector_options/2, [
		comment is 'Returns the effective training options recorded in a learned selector diagnostics metadata.',
		argnames is ['Selector', 'Options']
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +atom, -list(clause)), one).
	:- info(export_to_clauses/4, [
		comment is 'Converts a selector into a list of predicate clauses. ``Functor`` is the functor for the generated predicate clauses.',
		argnames is ['Dataset', 'Selector', 'Functor', 'Clauses']
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +atom, +atom), one).
	:- info(export_to_file/4, [
		comment is 'Exports a selector to a file. ``Functor`` is the functor for the generated predicate clauses.',
		argnames is ['Dataset', 'Selector', 'Functor', 'File']
	]).

	:- public(print_selector/1).
	:- mode(print_selector(+compound), one).
	:- info(print_selector/1, [
		comment is 'Prints a selector to the current output stream in a human-readable format.',
		argnames is ['Selector']
	]).

:- end_protocol.
