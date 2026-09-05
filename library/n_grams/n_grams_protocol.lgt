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


:- protocol(n_grams_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'N-gram generation and counting protocol.',
		see_also is [n_grams(_)]
	]).

	:- public(n_grams/3).
	:- mode(n_grams(+positive_integer, +list(text), -list(list(text))), one_or_error).
	:- info(n_grams/3, [
		comment is 'Generates overlapping n-grams from a list of text tokens.',
		argnames is ['N', 'Tokens', 'NGrams'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'``N`` is a variable' - instantiation_error,
			'``N`` is neither a variable nor a positive integer' - type_error(positive_integer, 'Options'),
			'``Tokens`` is a partial list or a list with an element ``Element`` which is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but a ground element ``Element`` of the list ``Tokens`` is not an atom' - type_error(atom, 'Element'),
			'The ``Representation`` parameter is ``chars`` but an element ``Element`` of the list ``Tokens`` is not a list of characters' - type_error(chars, 'Element'),
			'The ``Representation`` parameter is ``codes`` but an element ``Element`` of the list ``Tokens`` is not a list of character codes' - type_error(codes, 'Element')
		]
	]).

	:- public(n_grams/4).
	:- mode(n_grams(+positive_integer, +list(text), -list(list(text)), +list(compound)), one_or_error).
	:- info(n_grams/4, [
		comment is 'Generates n-grams from a list of text tokens using the given options.',
		argnames is ['N', 'Tokens', 'NGrams', 'Options'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'``N`` is a variable' - instantiation_error,
			'``N`` is neither a variable nor a positive integer' - type_error(positive_integer, 'Options'),
			'``Tokens`` is a partial list or a list with an element ``Element`` which is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but a ground element ``Element`` of the list ``Tokens`` is not an atom' - type_error(atom, 'Element'),
			'The ``Representation`` parameter is ``chars`` but an element ``Element`` of the list ``Tokens`` is not a list of characters' - type_error(chars, 'Element'),
			'The ``Representation`` parameter is ``codes`` but an element ``Element`` of the list ``Tokens`` is not a list of character codes' - type_error(codes, 'Element'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(character_n_grams/3).
	:- mode(character_n_grams(+positive_integer, +text, -list(text)), one_or_error).
	:- info(character_n_grams/3, [
		comment is 'Generates overlapping character n-grams from a text value.',
		argnames is ['N', 'Text', 'NGrams'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'``N`` is a variable' - instantiation_error,
			'``N`` is neither a variable nor a positive integer' - type_error(positive_integer, 'Options'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text')
		]
	]).

	:- public(character_n_grams/4).
	:- mode(character_n_grams(+positive_integer, +text, -list(text), +list(compound)), one_or_error).
	:- info(character_n_grams/4, [
		comment is 'Generates character n-grams from a text value using the given options.',
		argnames is ['N', 'Text', 'NGrams', 'Options'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'``N`` is a variable' - instantiation_error,
			'``N`` is neither a variable nor a positive integer' - type_error(positive_integer, 'Options'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(count/2).
	:- mode(count(+list(text), -list(pair(text,positive_integer))), one).
	:- mode(count(+list(list(text)), -list(pair(list(text),positive_integer))), one).
	:- info(count/2, [
		comment is 'Counts n-gram occurrences, preserving the order of first occurrence. Assumes ``NGrams`` is valid.',
		argnames is ['NGrams', 'Counts']
	]).

	:- public(count/3).
	:- mode(count(+atom, +list(text), -list(pair(text,positive_integer))), one_or_error).
	:- mode(count(+atom, +list(list(text)), -list(pair(list(text),positive_integer))), one_or_error).
	:- info(count/3, [
		comment is 'Counts n-gram occurrences using the requested ordering. Valid orderings are ``first_occurrence``, ``standard``, and ``frequency_descending``. Assumes ``NGrams`` is valid.',
		argnames is ['Order', 'NGrams', 'Counts'],
		exceptions is [
			'``Order`` is a variable' - instantiation_error,
			'``Order`` is neither a variable nor a valid ordering' - domain_error(count_order, 'Order')
		]
	]).

	:- public(bigrams/2).
	:- mode(bigrams(+list(text), -list(list(text))), one_or_error).
	:- info(bigrams/2, [
		comment is 'Generates overlapping bigrams from a list of text tokens.',
		argnames is ['Tokens', 'Bigrams'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'``Tokens`` is a partial list or a list with an element ``Element`` which is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but a ground element ``Element`` of the list ``Tokens`` is not an atom' - type_error(atom, 'Element'),
			'The ``Representation`` parameter is ``chars`` but an element ``Element`` of the list ``Tokens`` is not a list of characters' - type_error(chars, 'Element'),
			'The ``Representation`` parameter is ``codes`` but an element ``Element`` of the list ``Tokens`` is not a list of character codes' - type_error(codes, 'Element')
		]
	]).

	:- public(trigrams/2).
	:- mode(trigrams(+list(text), -list(list(text))), one_or_error).
	:- info(trigrams/2, [
		comment is 'Generates overlapping trigrams from a list of text tokens.',
		argnames is ['Tokens', 'Trigrams'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'``Tokens`` is a partial list or a list with an element ``Element`` which is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but a ground element ``Element`` of the list ``Tokens`` is not an atom' - type_error(atom, 'Element'),
			'The ``Representation`` parameter is ``chars`` but an element ``Element`` of the list ``Tokens`` is not a list of characters' - type_error(chars, 'Element'),
			'The ``Representation`` parameter is ``codes`` but an element ``Element`` of the list ``Tokens`` is not a list of character codes' - type_error(codes, 'Element')
		]
	]).

:- end_protocol.
