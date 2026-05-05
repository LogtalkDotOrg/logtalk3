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


:- object(iso_9362,
	implements(iso_9362_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'ISO 9362 BIC parser and normalizer based on the public Swift and ISO 9362 structure definitions.'
	]).

	:- uses(iso_3166, [
		country/4
	]).

	bic(BIC, Prefix, CountryAlpha2, Suffix, Branch) :-
		atom(BIC),
		atom_chars(BIC, Characters),
		bic_parts(Characters, PrefixCharacters, CountryCharacters, SuffixCharacters, BranchCharacters),
		valid_upper_alnum_characters(PrefixCharacters),
		valid_country_code(CountryCharacters, CountryAlpha2),
		valid_upper_alnum_characters(SuffixCharacters),
		valid_upper_alnum_characters(BranchCharacters),
		atom_chars(Prefix, PrefixCharacters),
		atom_chars(Suffix, SuffixCharacters),
		atom_chars(Branch, BranchCharacters).

	canonical_bic(BIC, Canonical) :-
		bic(BIC, Prefix, CountryAlpha2, Suffix, Branch),
		atom_concat(Prefix, CountryAlpha2, PrefixCountry),
		atom_concat(PrefixCountry, Suffix, PrimaryOffice),
		atom_concat(PrimaryOffice, Branch, Canonical).

	primary_office_bic(BIC) :-
		bic(BIC, _, _, _, 'XXX').

	bic_parts([P1, P2, P3, P4, C1, C2, S1, S2], [P1, P2, P3, P4], [C1, C2], [S1, S2], ['X', 'X', 'X']) :-
		!.
	bic_parts([P1, P2, P3, P4, C1, C2, S1, S2, B1, B2, B3], [P1, P2, P3, P4], [C1, C2], [S1, S2], [B1, B2, B3]).

	valid_country_code(CountryCharacters, CountryAlpha2) :-
		valid_upper_alpha_characters(CountryCharacters),
		atom_chars(CountryAlpha2, CountryCharacters),
		downcase_atom(CountryAlpha2, CountryCode),
		once(country(CountryCode, _, _, _)).

	valid_upper_alpha_characters([]).
	valid_upper_alpha_characters([Character| Characters]) :-
		'A' @=< Character, Character @=< 'Z',
		valid_upper_alpha_characters(Characters).

	valid_upper_alnum_characters([]).
	valid_upper_alnum_characters([Character| Characters]) :-
		upper_alnum_character(Character),
		valid_upper_alnum_characters(Characters).

	upper_alnum_character(Character) :-
		(	'A' @=< Character, Character @=< 'Z' ->
			true
		;	'0' @=< Character, Character @=< '9'
		).

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
