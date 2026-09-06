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


:- category(text_case_folding,
	extends(unicode_character_data)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Portable Unicode 17.0 default case conversion over lists of Unicode scalar values.'
	]).

	:- protected(convert_case_codes/4).
	:- mode(convert_case_codes(+atom, +list(integer), +object_identifier, -list(integer)), one).
	:- info(convert_case_codes/4, [
		comment is 'Applies a profile override or the default ``fold``, ``lower``, ``upper``, or ``title`` case conversion.',
		argnames is ['Mode', 'Codes', 'Profile', 'Converted']
	]).

	:- uses(list, [
		append/3
	]).

	convert_case_codes(Mode, Codes, Profile, Converted) :-
		(	Profile::case_conversion(Mode, Codes, ProfileConverted) ->
			Converted = ProfileConverted
		;	default_case_conversion(Mode, Codes, Converted)
		),
		!.

	default_case_conversion(fold, Codes, Converted) :-
		map_codes(Codes, case_fold, Converted).
	default_case_conversion(lower, Codes, Converted) :-
		lower_codes(Codes, [], Converted).
	default_case_conversion(upper, Codes, Converted) :-
		map_codes(Codes, upper, Converted).
	default_case_conversion(title, Codes, Converted) :-
		title_codes(Codes, true, [], Converted).

	map_codes([], _, []).
	map_codes([Code| Codes], Mapping, Converted) :-
		code_mapping(Mapping, Code, Replacement),
		append(Replacement, Rest, Converted),
		map_codes(Codes, Mapping, Rest).

	code_mapping(case_fold, Code, Mapping) :-
		( ^^case_fold_mapping(Code, Folded) -> Mapping = Folded; Mapping = [Code] ).
	code_mapping(upper, Code, Mapping) :-
		( ^^upper_case_mapping(Code, Upper) -> Mapping = Upper; Mapping = [Code] ).
	code_mapping(title, Code, Mapping) :-
		( ^^title_case_mapping(Code, Title) -> Mapping = Title; Mapping = [Code] ).
	code_mapping(lower, Code, Mapping) :-
		( ^^lower_case_mapping(Code, Lower) -> Mapping = Lower; Mapping = [Code] ).

	lower_codes([], _, []).
	lower_codes([931| Codes], Before, Converted) :-
		final_sigma(Before, Codes),
		!,
		Converted = [962| Rest],
		lower_codes(Codes, [931| Before], Rest).
	lower_codes([Code| Codes], Before, Converted) :-
		code_mapping(lower, Code, Replacement),
		append(Replacement, Rest, Converted),
		lower_codes(Codes, [Code| Before], Rest).

	final_sigma(Before, After) :-
		preceded_by_cased(Before),
		\+ followed_by_cased(After).

	preceded_by_cased([Code| Codes]) :-
		(	case_ignorable(Code) ->
			preceded_by_cased(Codes)
		;	cased(Code)
		).

	followed_by_cased([Code| Codes]) :-
		(	case_ignorable(Code) ->
			followed_by_cased(Codes)
		;	cased(Code)
		).

	title_codes([], _, _, []).
	title_codes([Code| Codes], AtStart, Before, Converted) :-
		(	cased(Code) ->
			(	AtStart == true ->
				code_mapping(title, Code, Replacement)
			;	Code =:= 931, final_sigma(Before, Codes) ->
				Replacement = [962]
			;	code_mapping(lower, Code, Replacement)
			),
			NextStart = false
		;	Replacement = [Code],
			(	case_ignorable(Code) ->
				NextStart = AtStart
			;	NextStart = true
			)
		),
		append(Replacement, Rest, Converted),
		title_codes(Codes, NextStart, [Code| Before], Rest).

	cased(Code) :-
		^^cased_code_range(Start, End),
		Code >= Start,
		Code =< End,
		!.

	case_ignorable(Code) :-
		^^case_ignorable_range(Start, End),
		Code >= Start,
		Code =< End,
		!.

:- end_category.
