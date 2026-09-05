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


:- protocol(lemmatizer_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'Lemmatization protocol for words represented as atoms, lists of characters, or lists of character codes.',
		see_also is [lemmatizer_language_protocol, lemmatizer(_, _)]
	]).

	:- public(lemma/2).
	:- mode(lemma(+text, -text), one_or_more_or_error).
	:- info(lemma/2, [
		comment is 'Enumerates the distinct lemmas of a word in provider preference order. Returns the normalized word when no lemma is recognized.',
		argnames is ['Word', 'Lemma'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'``Word`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Word`` is not an atom' - type_error(atom, 'Word'),
			'The ``Representation`` parameter is ``chars`` but ``Word`` is not a list of characters' - type_error(chars, 'Word'),
			'The ``Representation`` parameter is ``codes`` but ``Word`` is not a list of character codes' - type_error(codes, 'Word')
		]
	]).

	:- public(lemma/3).
	:- mode(lemma(+text, -text, +list(compound)), zero_or_more_or_error).
	:- info(lemma/3, [
		comment is 'Lemmatizes a word using the given options. Recognized options are ``part_of_speech(PartOfSpeech)``, ``ambiguity(Policy)``, where ``Policy`` is ``first`` or ``all``, and ``unknown(Policy)``, where ``Policy`` is ``normalize``, ``preserve``, or ``fail``.',
		argnames is ['Word', 'Lemma', 'Options'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'``Word`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Word`` is not an atom' - type_error(atom, 'Word'),
			'The ``Representation`` parameter is ``chars`` but ``Word`` is not a list of characters' - type_error(chars, 'Word'),
			'The ``Representation`` parameter is ``codes`` but ``Word`` is not a list of character codes' - type_error(codes, 'Word'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'An element of the list ``Options`` is a ``parts_of_speech/1`` option, which is only valid for ``lemmas/3``' - domain_error(option, parts_of_speech(_))
		]
	]).

	:- public(lemmas/2).
	:- mode(lemmas(+list(text), -list(text)), one_or_error).
	:- info(lemmas/2, [
		comment is 'Lemmatizes a list of words, selecting the first lemma for each word and preserving order. Returns normalized words when no lemma is recognized.',
		argnames is ['Words', 'Lemmas'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'``Words`` is a variable or a partial list' - instantiation_error,
			'``Words`` is neither a variable nor a list' - type_error(list, 'Words'),
			'An element ``Word`` of the list ``Words`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but a ground element ``Word`` of the list ``Words`` is not an atom' - type_error(atom, 'Word'),
			'The ``Representation`` parameter is ``chars`` but an element ``Word`` of the list ``Words`` is not a list of characters' - type_error(chars, 'Word'),
			'The ``Representation`` parameter is ``codes`` but an element ``Word`` of the list ``Words`` is not a list of character codes' - type_error(codes, 'Word')
		]
	]).

	:- public(lemmas/3).
	:- mode(lemmas(+list(text), -list(text), +list(compound)), zero_or_more_or_error).
	:- info(lemmas/3, [
		comment is 'Lemmatizes a list of words using the given options. In addition to the scalar options, accepts ``parts_of_speech(PartsOfSpeech)`` for per-word hints. The ``part_of_speech/1`` and ``parts_of_speech/1`` options are mutually exclusive.',
		argnames is ['Words', 'Lemmas', 'Options'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'``Words`` is a variable or a partial list' - instantiation_error,
			'``Words`` is neither a variable nor a list' - type_error(list, 'Words'),
			'An element ``Word`` of the list ``Words`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but a ground element ``Word`` of the list ``Words`` is not an atom' - type_error(atom, 'Word'),
			'The ``Representation`` parameter is ``chars`` but an element ``Word`` of the list ``Words`` is not a list of characters' - type_error(chars, 'Word'),
			'The ``Representation`` parameter is ``codes`` but an element ``Word`` of the list ``Words`` is not a list of character codes' - type_error(codes, 'Word'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The ``part_of_speech(POS)`` and ``parts_of_speech(PsOS)`` options are both present' - consistency_error(mutually_exclusive_options, part_of_speech('POS'), parts_of_speech('PsOS')),
			'The ``Words`` and ``PartsOfSpeech`` lists have different lengths' - consistency_error(same_length, 'Words', 'PartsOfSpeech')
		]
	]).

:- end_protocol.
