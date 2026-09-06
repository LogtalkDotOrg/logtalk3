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


:- protocol(text_normalizer_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Text normalization protocol for text represented as atoms, lists of characters, or lists of character codes.',
		see_also is [text_normalization_profile_protocol, spelling_normalizer_protocol, text_normalizer(_, _)]
	]).

	:- public(normalize_unicode/3).
	:- mode(normalize_unicode(+atom, +text, -text), one_or_error).
	:- info(normalize_unicode/3, [
		comment is 'Normalizes text to the specified Unicode normalization form. Valid forms are ``nfc``, ``nfd``, ``nfkc``, and ``nfkd``.',
		argnames is ['Form', 'Text', 'Normalized'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Form`` is a variable' - instantiation_error,
			'``Form`` is not an atom' - type_error(atom, 'Form'),
			'``Form`` is not a supported Unicode normalization form' - domain_error(unicode_normalization_form, 'Form'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code')
		]
	]).

	:- public(remove_diacritics/2).
	:- mode(remove_diacritics(+text, -text), one_or_error).
	:- info(remove_diacritics/2, [
		comment is 'Canonically decomposes text, removes Unicode mark characters, and returns NFC text.',
		argnames is ['Text', 'Normalized'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code')
		]
	]).

	:- public(fold_diacritics/2).
	:- mode(fold_diacritics(+text, -text), one_or_error).
	:- info(fold_diacritics/2, [
		comment is 'Removes diacritics and applies profile-defined transliterations for characters without canonical decompositions.',
		argnames is ['Text', 'Normalized'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code')
		]
	]).

	:- public(case_fold/2).
	:- mode(case_fold(+text, -text), one_or_error).
	:- info(case_fold/2, [
		comment is 'Applies full default Unicode case folding, unless overridden by the selected profile.',
		argnames is ['Text', 'Folded'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code')
		]
	]).

	:- public(lower_case/2).
	:- mode(lower_case(+text, -text), one_or_error).
	:- info(lower_case/2, [
		comment is 'Converts text to lowercase using the selected profile and Unicode default casing.',
		argnames is ['Text', 'Lowercase'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code')
		]
	]).

	:- public(upper_case/2).
	:- mode(upper_case(+text, -text), one_or_error).
	:- info(upper_case/2, [
		comment is 'Converts text to uppercase using the selected profile and Unicode default casing.',
		argnames is ['Text', 'Uppercase'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code')
		]
	]).

	:- public(title_case/2).
	:- mode(title_case(+text, -text), one_or_error).
	:- info(title_case/2, [
		comment is 'Converts text to title case using the library simple word-boundary definition.',
		argnames is ['Text', 'Titlecase'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code')
		]
	]).

	:- public(decode_entities/2).
	:- mode(decode_entities(+text, -text), one_or_error).
	:- info(decode_entities/2, [
		comment is 'Decodes semicolon-terminated XML, numeric, and profile-defined named character references using default options.',
		argnames is ['Text', 'Decoded'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code'),
			'A numeric character reference denotes a value that is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Value')
		]
	]).

	:- public(decode_entities/3).
	:- mode(decode_entities(+text, -text, +list(compound)), one_or_error).
	:- info(decode_entities/3, [
		comment is 'Decodes semicolon-terminated character references. The recognized option is ``unknown(preserve|error)``.',
		argnames is ['Text', 'Decoded', 'Options'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code'),
			'A numeric character reference denotes a value that is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Value'),
			'A named character reference is unknown and the ``unknown(error)`` option is used' - domain_error(character_reference, 'Reference')
		]
	]).

	:- public(normalize_whitespace/2).
	:- mode(normalize_whitespace(+text, -text), one_or_error).
	:- info(normalize_whitespace/2, [
		comment is 'Normalizes whitespace using default options.',
		argnames is ['Text', 'Normalized'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code')
		]
	]).

	:- public(normalize_whitespace/3).
	:- mode(normalize_whitespace(+text, -text, +list(compound)), one_or_error).
	:- info(normalize_whitespace/3, [
		comment is 'Normalizes whitespace. Recognized options are ``trim(Boolean)``, ``collapse(none|horizontal|all)``, ``line_endings(lf|crlf|cr|preserve)``, and ``controls(preserve|remove)``.',
		argnames is ['Text', 'Normalized', 'Options'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code')
		]
	]).

	:- public(clean/2).
	:- mode(clean(+text, -text), one_or_error).
	:- info(clean/2, [
		comment is 'Cleans text using the default normalization pipeline options.',
		argnames is ['Text', 'Cleaned'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code'),
			'A numeric character reference denotes a value that is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Value')
		]
	]).

	:- public(clean/3).
	:- mode(clean(+text, -text, +list(compound)), one_or_error).
	:- info(clean/3, [
		comment is 'Cleans text by applying entity decoding, case conversion, diacritic handling, Unicode normalization, and whitespace normalization in that order.',
		argnames is ['Text', 'Cleaned', 'Options'],
		exceptions is [
			'The ``Representation`` parameter is a variable' - instantiation_error,
			'The ``Representation`` parameter is neither a variable nor ``atom``, ``chars``, or ``codes``' - domain_error(text_representation, 'Representation'),
			'The ``Profile`` parameter is a variable' - instantiation_error,
			'The ``Profile`` parameter is not an object implementing the ``text_normalization_profile_protocol`` protocol' - domain_error(text_normalization_profile, 'Profile'),
			'``Text`` is not ground' - instantiation_error,
			'The ``Representation`` parameter is ``atom`` but ``Text`` is not an atom' - type_error(atom, 'Text'),
			'The ``Representation`` parameter is ``chars`` but ``Text`` is not a list of characters' - type_error(chars, 'Text'),
			'The ``Representation`` parameter is ``codes`` but ``Text`` is not a list of character codes' - type_error(codes, 'Text'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'An input character code is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code'),
			'A numeric character reference denotes a value that is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Value'),
			'A named character reference is unknown and the ``unknown_entities(error)`` option is used' - domain_error(character_reference, 'Reference')
		]
	]).

:- end_protocol.