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


:- protocol(recommender_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-04,
		comment is 'Protocol for recommendation models and user-item relevance scoring.',
		see_also is [rating_dataset_protocol, similarity_metric_protocol]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one_or_error).
	:- info(learn/3, [
		comment is 'Learns a recommender model from the given rating dataset object using the specified options.',
		argnames is ['Dataset', 'Recommender', 'Options'],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The dataset contains no ratings' - domain_error(non_empty_ratings, 'Dataset'),
			'A user or item identifier is a variable' - instantiation_error,
			'A user or item identifier is not atomic' - type_error(atomic, 'Identifier'),
			'The same ``User``-``Item`` pair is rated more than once' - domain_error(duplicate_rating, 'User'-'Item'),
			'The declared rating count is a variable' - instantiation_error,
			'The declared rating count is neither a variable nor an integer' - type_error(integer, 'DeclaredCount'),
			'The declared rating count is an integer but is not positive' - domain_error(positive_integer, 'DeclaredCount'),
			'The declared and observed rating counts differ' - consistency_error(rating_count, 'DeclaredCount', 'ObservedCount'),
			'A rating value is not a number' - type_error(number, 'Rating'),
			'A scale bound is a variable' - instantiation_error,
			'A scale bound is not numeric' - type_error(number, 'Bound'),
			'Scale bounds are reversed' - domain_error(rating_scale, 'Min'-'Max'),
			'A rating value falls outside the declared rating scale' - domain_error(rating_scale('Min', 'Max'), 'Rating'),
			'A content-based dataset has no catalog items' - domain_error(non_empty_catalog, 'Dataset'),
			'A catalog or content item is declared more than once' - domain_error(duplicate_item, 'Item'),
			'Content declarations do not cover the catalog exactly once' - domain_error(item_content_coverage, 'Dataset'),
			'A rated item is absent from the content catalog' - domain_error(catalog_item, 'Item'),
			'A content descriptor is invalid' - domain_error(item_content, 'Content'),
			'Content representations are mixed' - domain_error(content_representation, 'Content'),
			'A content list is not a proper list' - type_error(list, 'List'),
			'A content descriptor, list, feature, or weight is a variable' - instantiation_error,
			'A vector entry is not a pair' - type_error(pair, 'Entry'),
			'A sparse vector repeats a feature key' - domain_error(duplicate_feature, 'Feature'),
			'A sparse weight is not a number' - type_error(number, 'Weight'),
			'A sparse weight is negative or nonfinite' - domain_error(non_negative_finite_weight, 'Weight'),
			'A content-based rating is not finite' - domain_error(finite_rating, 'Rating'),
			'A selected rating is not positive in rating-weighted profile construction' - domain_error(positive_rating_weight, 'Rating'),
			'Feature fitting produces no vocabulary' - domain_error(non_empty_vocabulary, 'Corpus')
		]
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -compound), one_or_error).
	:- info(learn/2, [
		comment is 'Learns a recommender model from the given rating dataset object using default options.',
		argnames is ['Dataset', 'Recommender'],
		exceptions is [
			'The dataset contains no ratings' - domain_error(non_empty_ratings, 'Dataset'),
			'A user or item identifier is a variable' - instantiation_error,
			'A user or item identifier is not atomic' - type_error(atomic, 'Identifier'),
			'The same ``User``-``Item`` pair is rated more than once' - domain_error(duplicate_rating, 'User'-'Item'),
			'The declared rating count is a variable' - instantiation_error,
			'The declared rating count is neither a variable nor an integer' - type_error(integer, 'DeclaredCount'),
			'The declared rating count is an integer but is not positive' - domain_error(positive_integer, 'DeclaredCount'),
			'The declared and observed rating counts differ' - consistency_error(rating_count, 'DeclaredCount', 'ObservedCount'),
			'A rating value is not a number' - type_error(number, 'Rating'),
			'A scale bound is a variable' - instantiation_error,
			'A scale bound is not numeric' - type_error(number, 'Bound'),
			'Scale bounds are reversed' - domain_error(rating_scale, 'Min'-'Max'),
			'A rating value falls outside the declared rating scale' - domain_error(rating_scale('Min', 'Max'), 'Rating'),
			'A content-based dataset has no catalog items' - domain_error(non_empty_catalog, 'Dataset'),
			'A catalog or content item is declared more than once' - domain_error(duplicate_item, 'Item'),
			'Content declarations do not cover the catalog exactly once' - domain_error(item_content_coverage, 'Dataset'),
			'A rated item is absent from the content catalog' - domain_error(catalog_item, 'Item'),
			'A content descriptor is invalid' - domain_error(item_content, 'Content'),
			'Content representations are mixed' - domain_error(content_representation, 'Content'),
			'A content list is not a proper list' - type_error(list, 'List'),
			'A content descriptor, list, feature, or weight is a variable' - instantiation_error,
			'A vector entry is not a pair' - type_error(pair, 'Entry'),
			'A sparse vector repeats a feature key' - domain_error(duplicate_feature, 'Feature'),
			'A sparse weight is not a number' - type_error(number, 'Weight'),
			'A sparse weight is negative or nonfinite' - domain_error(non_negative_finite_weight, 'Weight'),
			'A content-based rating is not finite' - domain_error(finite_rating, 'Rating'),
			'A selected rating is not positive in rating-weighted profile construction' - domain_error(positive_rating_weight, 'Rating'),
			'Feature fitting produces no vocabulary' - domain_error(non_empty_vocabulary, 'Corpus')
		]
	]).

	:- public(score/4).
	:- mode(score(+compound, +atomic, +atomic, -number), one_or_error).
	:- info(score/4, [
		comment is 'Scores the relevance of ``Item`` to ``User`` using the learned recommender. Higher scores indicate greater relevance. Scores may be predicted ratings or other relevance measures; their ranges and handling of unknown identifiers depend on the implementation.',
		argnames is ['Recommender', 'User', 'Item', 'Score'],
		exceptions is [
			'``Recommender`` is a variable' - instantiation_error,
			'``Recommender`` is not a valid recommender' - domain_error(recommender, 'Recommender'),
			'A user or item identifier is a variable' - instantiation_error,
			'A user or item identifier is not atomic' - type_error(atomic, 'Identifier'),
			'``Item`` is unknown to an implementation requiring catalog membership' - domain_error(catalog_item, 'Item')
		]
	]).

	:- public(recommend/4).
	:- mode(recommend(+compound, +atomic, +positive_integer, -list(pair)), one_or_error).
	:- info(recommend/4, [
		comment is 'Recommends up to ``N`` items to ``User`` using the learned recommender model, as a list of ``Item-Score`` pairs sorted by decreasing score. Whether items already rated by ``User`` are excluded, and how ties and an unknown ``User`` are handled, depends on the implementation.',
		argnames is ['Recommender', 'User', 'N', 'Recommendations'],
		exceptions is [
			'``Recommender`` is a variable' - instantiation_error,
			'``Recommender`` is not a valid recommender' - domain_error(recommender, 'Recommender'),
			'``N`` is a variable' - instantiation_error,
			'``N`` is neither a variable nor an integer' - type_error(integer, 'N'),
			'``N`` is an integer but is not positive' - domain_error(positive_integer, 'N'),
			'``User`` is a variable' - instantiation_error,
			'``User`` is not atomic' - type_error(atomic, 'User')
		]
	]).

	:- public(check_recommender/1).
	:- mode(check_recommender(@compound), one_or_error).
	:- info(check_recommender/1, [
		comment is 'Checks model data and diagnostics for the receiving implementation without instantiating the recommender. Throws an exception when the term is not a valid recommender representation.',
		argnames is ['Recommender'],
		exceptions is [
			'``Recommender`` is a variable' - instantiation_error,
			'``Recommender`` is neither a variable nor a valid recommender' - domain_error(recommender, 'Recommender')
		]
	]).

	:- public(valid_recommender/1).
	:- mode(valid_recommender(@compound), zero_or_one).
	:- info(valid_recommender/1, [
		comment is 'True when a learned recommender term is structurally valid for the receiving implementation. Succeeds iff ``check_recommender/1`` succeeds without throwing an exception.',
		argnames is ['Recommender']
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+compound, -list(compound)), one).
	:- info(diagnostics/2, [
		comment is 'Returns diagnostics metadata for a learned recommender.',
		argnames is ['Recommender', 'Diagnostics']
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+compound, ?compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Enumerates individual diagnostics metadata terms for a learned recommender.',
		argnames is ['Recommender', 'Diagnostic']
	]).

	:- public(recommender_options/2).
	:- mode(recommender_options(+compound, -list(compound)), one).
	:- info(recommender_options/2, [
		comment is 'Returns the effective training options recorded in a learned recommender diagnostics metadata.',
		argnames is ['Recommender', 'Options']
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +atom, -list(clause)), one).
	:- info(export_to_clauses/4, [
		comment is 'Converts a recommender into a list of predicate clauses. ``Functor`` is the functor for the generated predicate clauses. When exporting a serialized recommender term, a noun such as ``recommender`` or ``model`` is usually clearer than a verb such as ``recommend``.',
		argnames is ['Dataset', 'Recommender', 'Functor', 'Clauses']
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +atom, +atom), one).
	:- info(export_to_file/4, [
		comment is 'Exports a recommender to a file. ``Functor`` is the functor for the generated predicate clauses. When exporting a serialized recommender term, a noun such as ``recommender`` or ``model`` is usually clearer than a verb such as ``recommend``.',
		argnames is ['Dataset', 'Recommender', 'Functor', 'File']
	]).

	:- public(print_recommender/1).
	:- mode(print_recommender(+compound), one).
	:- info(print_recommender/1, [
		comment is 'Prints a recommender to the current output stream in a human-readable format.',
		argnames is ['Recommender']
	]).

:- end_protocol.
