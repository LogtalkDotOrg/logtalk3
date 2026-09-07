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


:- protocol(text_vectorizer_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-07,
		comment is 'Protocol for learning and applying text vectorization models.',
		see_also is [text_vectorizer]
	]).

	:- public(learn/2).
	:- mode(learn(+list(list), -compound), one_or_error).
	:- info(learn/2, [
		comment is 'Learns a vectorizer from a non-empty corpus using default options. Documents are lists of ground features.',
		argnames is ['Corpus', 'Vectorizer'],
		exceptions is [
			'``Corpus`` is a variable or a partial list' - instantiation_error,
			'``Corpus`` is neither a variable nor a list' - type_error(list, 'Corpus'),
			'``Corpus`` is empty' - domain_error(non_empty_corpus, 'Corpus'),
			'An element ``Document`` of the list ``Corpus`` is a variable or a partial list' - instantiation_error,
			'An element ``Document`` of the list ``Corpus`` is neither a variable nor a list' - type_error(list, 'Document'),
			'An element ``Feature`` of a document is not ground' - instantiation_error,
			'No corpus feature satisfies the document-frequency filters' - domain_error(non_empty_vocabulary, 'Corpus')
		]
	]).

	:- public(learn/3).
	:- mode(learn(+list(list), -compound, +list(compound)), one_or_error).
	:- info(learn/3, [
		comment is 'Learns a vectorizer from a non-empty corpus using the given options. Documents are lists of ground features.',
		argnames is ['Corpus', 'Vectorizer', 'Options'],
		exceptions is [
			'``Corpus`` is a variable or a partial list' - instantiation_error,
			'``Corpus`` is neither a variable nor a list' - type_error(list, 'Corpus'),
			'``Corpus`` is empty' - domain_error(non_empty_corpus, 'Corpus'),
			'An element ``Document`` of the list ``Corpus`` is a variable or a partial list' - instantiation_error,
			'An element ``Document`` of the list ``Corpus`` is neither a variable nor a list' - type_error(list, 'Document'),
			'An element ``Feature`` of a document is not ground' - instantiation_error,
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The ``maximum_document_frequency(Maximum)`` option value is greater than the corpus document count' - domain_error(option, maximum_document_frequency('Maximum')),
			'The ``minimum_document_frequency(Minimum)`` option value is greater than the effective maximum document frequency' - domain_error(option, minimum_document_frequency('Minimum')),
			'No corpus feature satisfies the document-frequency filters' - domain_error(non_empty_vocabulary, 'Corpus')
		]
	]).

	:- public(transform/3).
	:- mode(transform(+compound, +list, -list(pair)), one_or_error).
	:- info(transform/3, [
		comment is 'Transforms a document into a sparse list of ``Feature-Weight`` pairs in standard term order. Out-of-vocabulary features are ignored.',
		argnames is ['Vectorizer', 'Document', 'Vector'],
		exceptions is [
			'``Vectorizer`` is a variable' - instantiation_error,
			'``Vectorizer`` is neither a variable nor a valid text vectorizer' - domain_error(text_vectorizer, 'Vectorizer'),
			'``Document`` is a variable or a partial list' - instantiation_error,
			'``Document`` is neither a variable nor a list' - type_error(list, 'Document'),
			'An element ``Feature`` of the list ``Document`` is not ground' - instantiation_error
		]
	]).

	:- public(transform_all/3).
	:- mode(transform_all(+compound, +list(list), -list(list(pair))), one_or_error).
	:- info(transform_all/3, [
		comment is 'Transforms all corpus documents into sparse vectors, preserving document order.',
		argnames is ['Vectorizer', 'Corpus', 'Vectors'],
		exceptions is [
			'``Vectorizer`` is a variable' - instantiation_error,
			'``Vectorizer`` is neither a variable nor a valid text vectorizer' - domain_error(text_vectorizer, 'Vectorizer'),
			'``Corpus`` is a variable or a partial list' - instantiation_error,
			'``Corpus`` is neither a variable nor a list' - type_error(list, 'Corpus'),
			'An element ``Document`` of the list ``Corpus`` is a variable or a partial list' - instantiation_error,
			'An element ``Document`` of the list ``Corpus`` is neither a variable nor a list' - type_error(list, 'Document'),
			'An element ``Feature`` of a document is not ground' - instantiation_error
		]
	]).

	:- public(learn_transform/3).
	:- mode(learn_transform(+list(list), -compound, -list(list(pair))), one_or_error).
	:- info(learn_transform/3, [
		comment is 'Learns a vectorizer using default options and transforms the training corpus.',
		argnames is ['Corpus', 'Vectorizer', 'Vectors'],
		exceptions is [
			'``Corpus`` is a variable or a partial list' - instantiation_error,
			'``Corpus`` is neither a variable nor a list' - type_error(list, 'Corpus'),
			'``Corpus`` is empty' - domain_error(non_empty_corpus, 'Corpus'),
			'An element ``Document`` of the list ``Corpus`` is a variable or a partial list' - instantiation_error,
			'An element ``Document`` of the list ``Corpus`` is neither a variable nor a list' - type_error(list, 'Document'),
			'An element ``Feature`` of a document is not ground' - instantiation_error,
			'No corpus feature satisfies the document-frequency filters' - domain_error(non_empty_vocabulary, 'Corpus')
		]
	]).

	:- public(learn_transform/4).
	:- mode(learn_transform(+list(list), -compound, -list(list(pair)), +list(compound)), one_or_error).
	:- info(learn_transform/4, [
		comment is 'Learns a vectorizer using the given options and transforms the training corpus.',
		argnames is ['Corpus', 'Vectorizer', 'Vectors', 'Options'],
		exceptions is [
			'``Corpus`` is a variable or a partial list' - instantiation_error,
			'``Corpus`` is neither a variable nor a list' - type_error(list, 'Corpus'),
			'``Corpus`` is empty' - domain_error(non_empty_corpus, 'Corpus'),
			'An element ``Document`` of the list ``Corpus`` is a variable or a partial list' - instantiation_error,
			'An element ``Document`` of the list ``Corpus`` is neither a variable nor a list' - type_error(list, 'Document'),
			'An element ``Feature`` of a document is not ground' - instantiation_error,
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The ``maximum_document_frequency(Maximum)`` option value is greater than the corpus document count' - domain_error(option, maximum_document_frequency('Maximum')),
			'The ``minimum_document_frequency(Minimum)`` option value is greater than the effective maximum document frequency' - domain_error(option, minimum_document_frequency('Minimum')),
			'No corpus feature satisfies the document-frequency filters' - domain_error(non_empty_vocabulary, 'Corpus')
		]
	]).

	:- public(check_vectorizer/1).
	:- mode(check_vectorizer(@compound), one_or_error).
	:- info(check_vectorizer/1, [
		comment is 'Checks that a term is a structurally valid learned text vectorizer.',
		argnames is ['Vectorizer'],
		exceptions is [
			'``Vectorizer`` is a variable' - instantiation_error,
			'``Vectorizer`` is neither a variable nor a valid text vectorizer' - domain_error(text_vectorizer, 'Vectorizer')
		]
	]).

	:- public(valid_vectorizer/1).
	:- mode(valid_vectorizer(@compound), zero_or_one).
	:- info(valid_vectorizer/1, [
		comment is 'True when a term is a structurally valid learned text vectorizer.',
		argnames is ['Vectorizer']
	]).

	:- public(vocabulary/2).
	:- mode(vocabulary(+compound, -list), one_or_error).
	:- info(vocabulary/2, [
		comment is 'Returns the learned vocabulary in standard term order.',
		argnames is ['Vectorizer', 'Vocabulary'],
		exceptions is [
			'``Vectorizer`` is a variable' - instantiation_error,
			'``Vectorizer`` is neither a variable nor a valid text vectorizer' - domain_error(text_vectorizer, 'Vectorizer')
		]
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+compound, -list(compound)), one_or_error).
	:- info(diagnostics/2, [
		comment is 'Returns the diagnostics and metadata stored in a learned vectorizer.',
		argnames is ['Vectorizer', 'Diagnostics'],
		exceptions is [
			'``Vectorizer`` is a variable' - instantiation_error,
			'``Vectorizer`` is neither a variable nor a valid text vectorizer' - domain_error(text_vectorizer, 'Vectorizer')
		]
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+compound, ?compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Tests or enumerates individual diagnostics terms stored in a learned vectorizer.',
		argnames is ['Vectorizer', 'Diagnostic'],
		exceptions is [
			'``Vectorizer`` is a variable' - instantiation_error,
			'``Vectorizer`` is neither a variable nor a valid text vectorizer' - domain_error(text_vectorizer, 'Vectorizer')
		]
	]).

	:- public(vectorizer_options/2).
	:- mode(vectorizer_options(+compound, -list(compound)), one_or_error).
	:- info(vectorizer_options/2, [
		comment is 'Returns the effective training options stored in a learned vectorizer.',
		argnames is ['Vectorizer', 'Options'],
		exceptions is [
			'``Vectorizer`` is a variable' - instantiation_error,
			'``Vectorizer`` is neither a variable nor a valid text vectorizer' - domain_error(text_vectorizer, 'Vectorizer')
		]
	]).

:- end_protocol.
