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


:- object(iso_13616,
	implements(iso_13616_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'ISO 13616 IBAN parser and normalizer based on the public ISO and SWIFT registry structure definitions and MOD-97 checksum algorithm.'
	]).

	:- uses(iso_13616_registry, [
		country_spec/3
	]).

	:- uses(iso_3166, [
		country/4
	]).

	iban(IBAN, CountryAlpha2, CheckDigits, BBAN) :-
		atom(IBAN),
		normalized_iban_characters(IBAN, Characters),
		iban_parts(Characters, CountryCharacters, CheckDigitCharacters, BBANCharacters),
		length(Characters, IBANLength),
		valid_country_code(CountryCharacters, CountryAlpha2),
		country_spec(CountryAlpha2, IBANLength, BBANPattern),
		valid_digit_characters(CheckDigitCharacters),
		valid_bban_pattern(BBANCharacters, BBANPattern),
		valid_checksum(CountryCharacters, CheckDigitCharacters, BBANCharacters),
		!,
		atom_chars(CheckDigits, CheckDigitCharacters),
		atom_chars(BBAN, BBANCharacters).

	canonical_iban(IBAN, Canonical) :-
		iban(IBAN, CountryAlpha2, CheckDigits, BBAN),
		atom_concat(CountryAlpha2, CheckDigits, Prefix),
		atom_concat(Prefix, BBAN, Canonical).

	formatted_iban(IBAN, Formatted) :-
		canonical_iban(IBAN, Canonical),
		atom_chars(Canonical, CanonicalCharacters),
		grouped_characters(CanonicalCharacters, FormattedCharacters),
		atom_chars(Formatted, FormattedCharacters).

	normalized_iban_characters(IBAN, Characters) :-
		atom_chars(IBAN, RawCharacters),
		remove_spaces(RawCharacters, SpaceFreeCharacters),
		uppercase_characters(SpaceFreeCharacters, Characters).

	remove_spaces([], []).
	remove_spaces([' '| Characters], SpaceFreeCharacters) :-
		!,
		remove_spaces(Characters, SpaceFreeCharacters).
	remove_spaces([Character| Characters], [Character| SpaceFreeCharacters]) :-
		remove_spaces(Characters, SpaceFreeCharacters).

	uppercase_character(Character, Uppercase) :-
		char_code(Character, Code),
		(	Code >= 0'a, Code =< 0'z ->
			UpperCode is Code - 32,
			char_code(Uppercase, UpperCode)
		;	Uppercase = Character
		).

	uppercase_characters([], []).
	uppercase_characters([Character| Characters], [Uppercase| UppercaseCharacters]) :-
		uppercase_character(Character, Uppercase),
		uppercase_characters(Characters, UppercaseCharacters).

	iban_parts(Characters, CountryCharacters, CheckDigitCharacters, BBANCharacters) :-
		Characters = [C1, C2, D1, D2| BBANCharacters],
		BBANCharacters \== [],
		length(Characters, Length),
		Length =< 34,
		length(BBANCharacters, BBANLength),
		BBANLength =< 30,
		CountryCharacters = [C1, C2],
		CheckDigitCharacters = [D1, D2].

	valid_country_code(CountryCharacters, CountryAlpha2) :-
		valid_upper_alpha_characters(CountryCharacters),
		atom_chars(CountryAlpha2, CountryCharacters),
		downcase_atom(CountryAlpha2, CountryCode),
		once(country(CountryCode, _, _, _)).

	valid_digit_characters([]).
	valid_digit_characters([Character| Characters]) :-
		digit_character(Character),
		valid_digit_characters(Characters).

	valid_bban_pattern([], []) :-
		!.
	valid_bban_pattern(BBANCharacters, [TypeCharacter-Count| RemainingPattern]) :-
		integer(Count),
		Count >= 0,
		pattern_segment_characters(Count, TypeCharacter, BBANCharacters, RemainingBBANCharacters),
		valid_bban_pattern(RemainingBBANCharacters, RemainingPattern).

	pattern_segment_characters(0, _, Characters, Characters) :-
		!.
	pattern_segment_characters(Count, TypeCharacter, [Character| Characters], RemainingCharacters) :-
		Count > 0,
		valid_pattern_character(TypeCharacter, Character),
		NextCount is Count - 1,
		pattern_segment_characters(NextCount, TypeCharacter, Characters, RemainingCharacters).

	valid_pattern_character(a, Character) :-
		valid_upper_alpha_character(Character).
	valid_pattern_character(c, Character) :-
		upper_alnum_character(Character).
	valid_pattern_character(n, Character) :-
		digit_character(Character).

	valid_upper_alpha_characters([]).
	valid_upper_alpha_characters([Character| Characters]) :-
		'A' @=< Character, Character @=< 'Z',
		valid_upper_alpha_characters(Characters).

	valid_upper_alpha_character(Character) :-
		'A' @=< Character, Character @=< 'Z'.

	digit_character(Character) :-
		char_code(Character, Code),
		Code >= 0'0,
		Code =< 0'9.

	upper_alnum_character(Character) :-
		char_code(Character, Code),
		(	Code >= 0'A, Code =< 0'Z ->
			true
		;	Code >= 0'0,
			Code =< 0'9
		).

	valid_checksum(CountryCharacters, CheckDigitCharacters, BBANCharacters) :-
		concatenate_lists(BBANCharacters, CountryCharacters, RotatedCharacters0),
		concatenate_lists(RotatedCharacters0, CheckDigitCharacters, RotatedCharacters),
		checksum_remainder(RotatedCharacters, 0, Remainder),
		Remainder =:= 1.

	concatenate_lists([], List, List).
	concatenate_lists([Element| List1], List2, [Element| List3]) :-
		concatenate_lists(List1, List2, List3).

	checksum_remainder([], Remainder, Remainder).
	checksum_remainder([Character| Characters], Accumulator0, Remainder) :-
		character_digits(Character, Digits),
		checksum_digits(Digits, Accumulator0, Accumulator),
		checksum_remainder(Characters, Accumulator, Remainder).

	character_digits(Character, Digits) :-
		char_code(Character, Code),
		(	Code >= 0'0, Code =< 0'9 ->
			Digit is Code - 0'0,
			Digits = [Digit]
		;	Code >= 0'A,
			Code =< 0'Z,
			Value is Code - 0'A + 10,
			Tens is Value // 10,
			Units is Value mod 10,
			Digits = [Tens, Units]
		).

	checksum_digits([], Accumulator, Accumulator).
	checksum_digits([Digit| Digits], Accumulator0, Accumulator) :-
		Accumulator1 is (Accumulator0 * 10 + Digit) mod 97,
		checksum_digits(Digits, Accumulator1, Accumulator).

	grouped_characters([A, B, C, D| Characters], [A, B, C, D, ' '| GroupedCharacters]) :-
		Characters \== [],
		!,
		grouped_characters(Characters, GroupedCharacters).
	grouped_characters(Characters, Characters).

	:- if(current_logtalk_flag(prolog_dialect, swi)).

		downcase_atom(Atom, LoweredAtom) :-
			{downcase_atom(Atom, LoweredAtom)}.

	:- else.

		downcase_atom(AnyAtom, Lower) :-
			atom_chars(AnyAtom, AnyChars),
			downcase_atom_(AnyChars, LowerChars),
			atom_chars(Lower, LowerChars).

		% ASCII only and avoiding 0'Char notation that would break some backends!
		downcase_atom_([], []).
		downcase_atom_([AnyChar| AnyChars], [LowerChar| LowerChars]) :-
			(	'A' @=< AnyChar, AnyChar @=< 'Z' ->
				char_code(AnyChar, AnyCode),
				LowerCode is AnyCode + 32,
				char_code(LowerChar, LowerCode)
			;	LowerChar = AnyChar
			),
			downcase_atom_(AnyChars, LowerChars).

	:- endif.

:- end_object.
