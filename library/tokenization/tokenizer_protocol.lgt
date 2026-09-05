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


:- protocol(tokenizer_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'Tokenization protocol for text represented as atoms, lists of characters, or lists of character codes.',
		see_also is [sentence_splitter_protocol, tokenizer_language_protocol, tokenizer(_, _)]
	]).

	:- public(tokenize/2).
	:- mode(tokenize(+text, -list(text)), one_or_error).
	:- info(tokenize/2, [
		comment is 'Tokenizes text using the default options.',
		argnames is ['Text', 'Tokens'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text')
		]
	]).

	:- public(tokenize/3).
	:- mode(tokenize(+text, -list(text), +list(compound)), one_or_error).
	:- info(tokenize/3, [
		comment is 'Tokenizes text using the given options. Recognized options are ``keep_punctuation(Boolean)``, ``lowercase(Boolean)``, ``normalize_quotes(Boolean)``, and ``normalize_dashes(Boolean)``.',
		argnames is ['Text', 'Tokens', 'Options'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
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

	:- public(tokenize_sentences/2).
	:- mode(tokenize_sentences(+text, -list(list(text))), one_or_error).
	:- info(tokenize_sentences/2, [
		comment is 'Splits text into sentences and tokenizes each sentence using the default options.',
		argnames is ['Text', 'TokenizedSentences'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text')
		]
	]).

	:- public(tokenize_sentences/3).
	:- mode(tokenize_sentences(+text, -list(list(text)), +list(compound)), one_or_error).
	:- info(tokenize_sentences/3, [
		comment is 'Splits text into sentences and tokenizes each sentence using the given options.',
		argnames is ['Text', 'TokenizedSentences', 'Options'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
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

:- end_protocol.