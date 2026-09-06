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


:- category(text_unicode,
	extends(unicode_character_data)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Portable Unicode 17.0 normalization over lists of Unicode scalar values.'
	]).

	:- protected(normalize_unicode_codes/3).
	:- mode(normalize_unicode_codes(+atom, +list(integer), -list(integer)), one_or_error).
	:- info(normalize_unicode_codes/3, [
		comment is 'Normalizes code points using the NFC, NFD, NFKC, or NFKD normalization form.',
		argnames is ['Form', 'Codes', 'Normalized'],
		exceptions is [
			'An input element is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code')
		]
	]).

	:- uses(list, [
		append/3, reverse/2
	]).

	normalize_unicode_codes(Form, Codes, Normalized) :-
		check_scalar_values(Codes),
		compatibility_form(Form, Compatibility),
		decompose_codes(Codes, Compatibility, Decomposed),
		canonical_order(Decomposed, Ordered),
		(	composed_form(Form) ->
			compose_codes(Ordered, Normalized)
		;	Normalized = Ordered
		),
		!.

	compatibility_form(nfc, false).
	compatibility_form(nfd, false).
	compatibility_form(nfkc, true).
	compatibility_form(nfkd, true).

	composed_form(nfc).
	composed_form(nfkc).

	check_scalar_values([]).
	check_scalar_values([Code| Codes]) :-
		(	integer(Code), Code >= 0, Code =< 0x10FFFF, (Code < 0xD800; Code > 0xDFFF) ->
			check_scalar_values(Codes)
		;	domain_error(unicode_scalar_value, Code)
		).

	decompose_codes([], _, []).
	decompose_codes([Code| Codes], Compatibility, Decomposed) :-
		decompose_code(Code, Compatibility, Decomposition),
		append(Decomposition, Rest, Decomposed),
		decompose_codes(Codes, Compatibility, Rest).

	decompose_code(Code, Compatibility, Decomposition) :-
		(	hangul_decomposition(Code, Hangul) ->
			decompose_codes(Hangul, Compatibility, Decomposition)
		;	^^canonical_decomposition(Code, Canonical) ->
			decompose_codes(Canonical, Compatibility, Decomposition)
		;	Compatibility == true,
			^^compatibility_decomposition(Code, Compatible) ->
			decompose_codes(Compatible, Compatibility, Decomposition)
		;	Decomposition = [Code]
		).

	hangul_decomposition(Code, Decomposition) :-
		Code >= 44032,
		Code < 55204,
		Index is Code - 44032,
		Leading is 4352 + Index // 588,
		Vowel is 4449 + (Index mod 588) // 28,
		TrailingIndex is Index mod 28,
		(	TrailingIndex =:= 0 ->
			Decomposition = [Leading, Vowel]
		;	Trailing is 4519 + TrailingIndex,
			Decomposition = [Leading, Vowel, Trailing]
		).

	canonical_order(Codes, Ordered) :-
		canonical_order(Codes, [], Reversed),
		reverse(Reversed, Ordered).

	canonical_order([], Ordered, Ordered).
	canonical_order([Code| Codes], Ordered0, Ordered) :-
		combining_class(Code, Class),
		(	Class =:= 0 ->
			Ordered1 = [Code| Ordered0]
		;	insert_combining(Code, Class, Ordered0, Ordered1)
		),
		canonical_order(Codes, Ordered1, Ordered).

	insert_combining(Code, Class, [Existing| Codes], [Existing| Ordered]) :-
		combining_class(Existing, ExistingClass),
		ExistingClass > Class,
		!,
		insert_combining(Code, Class, Codes, Ordered).
	insert_combining(Code, _, Codes, [Code| Codes]).

	combining_class(Code, Class) :-
		(	^^canonical_combining_class(Code, Class0) ->
			Class = Class0
		;	Class = 0
		).

	compose_codes([], []).
	compose_codes([Code| Codes], Composed) :-
		combining_class(Code, Class),
		(	Class =:= 0 ->
			compose_starter(Codes, Code, [], 0, Segment, Rest),
			append(Segment, ComposedRest, Composed),
			compose_codes(Rest, ComposedRest)
		;	take_leading_marks(Codes, [Code], Reversed, Rest),
			reverse(Reversed, Leading),
			append(Leading, ComposedRest, Composed),
			compose_codes(Rest, ComposedRest)
		).

	take_leading_marks([], Marks, Marks, []).
	take_leading_marks([Code| Codes], Marks, Reversed, Rest) :-
		combining_class(Code, Class),
		(	Class =:= 0 ->
			Reversed = Marks,
			Rest = [Code| Codes]
		;	take_leading_marks(Codes, [Code| Marks], Reversed, Rest)
		).

	compose_starter([], Starter, Marks, _, Segment, []) :-
		reverse(Marks, OrderedMarks),
		Segment = [Starter| OrderedMarks].
	compose_starter([Code| Codes], Starter0, Marks0, LastClass0, Segment, Rest) :-
		combining_class(Code, Class),
		(	composition(Starter0, Code, Composite), (LastClass0 =:= 0; LastClass0 < Class) ->
			compose_starter(Codes, Composite, Marks0, LastClass0, Segment, Rest)
		;	Class =:= 0 ->
			reverse(Marks0, OrderedMarks),
			Segment = [Starter0| OrderedMarks],
			Rest = [Code| Codes]
		;	compose_starter(Codes, Starter0, [Code| Marks0], Class, Segment, Rest)
		).

	composition(First, Second, Composite) :-
		(	hangul_composition(First, Second, Composite) ->
			true
		;	^^canonical_composition(First, Second, Composite)
		).

	hangul_composition(Leading, Vowel, Composite) :-
		Leading >= 4352,
		Leading =< 4370,
		Vowel >= 4449,
		Vowel =< 4469,
		Composite is 44032 + (Leading - 4352) * 588 + (Vowel - 4449) * 28.
	hangul_composition(Syllable, Trailing, Composite) :-
		Syllable >= 44032,
		Syllable < 55204,
		(Syllable - 44032) mod 28 =:= 0,
		Trailing >= 4520,
		Trailing =< 4546,
		Composite is Syllable + Trailing - 4519.

:- end_category.
