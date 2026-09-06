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


:- category(text_diacritics,
	extends(text_unicode)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-06,
		comment is 'Unicode diacritic removal and profile-defined transliteration over code lists.'
	]).

	:- protected(remove_diacritics_codes/2).
	:- mode(remove_diacritics_codes(+list(integer), -list(integer)), one_or_error).
	:- info(remove_diacritics_codes/2, [
		comment is 'Canonically decomposes text, removes all Unicode Mark-category code points, and returns NFC text.',
		argnames is ['Codes', 'Removed'],
		exceptions is [
			'An input element is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code')
		]
	]).

	:- protected(fold_diacritics_codes/3).
	:- mode(fold_diacritics_codes(+list(integer), +object_identifier, -list(integer)), one_or_error).
	:- info(fold_diacritics_codes/3, [
		comment is 'Removes Unicode marks, applies profile transliterations to remaining code points, and returns NFC text.',
		argnames is ['Codes', 'Profile', 'Folded'],
		exceptions is [
			'An input element is not a Unicode scalar value' - domain_error(unicode_scalar_value, 'Code')
		]
	]).

	:- uses(list, [
		append/3
	]).

	:- uses(user, [
		unicode_data_mark_range/2
	]).

	remove_diacritics_codes(Codes, Removed) :-
		^^normalize_unicode_codes(nfd, Codes, Decomposed),
		remove_marks(Decomposed, WithoutMarks),
		^^normalize_unicode_codes(nfc, WithoutMarks, Removed).

	fold_diacritics_codes(Codes, Profile, Folded) :-
		^^normalize_unicode_codes(nfd, Codes, Decomposed),
		fold_codes(Decomposed, Profile, WithoutMarks),
		^^normalize_unicode_codes(nfc, WithoutMarks, Folded).

	remove_marks([], []).
	remove_marks([Code| Codes], Removed) :-
		(	mark(Code) ->
			Removed = Rest
		;	Removed = [Code| Rest]
		),
		remove_marks(Codes, Rest).

	fold_codes([], _, []).
	fold_codes([Code| Codes], Profile, Folded) :-
		(	mark(Code) ->
			Folded = Rest
		;	Profile::diacritic_fold(Code, Replacement) ->
			append(Replacement, Rest, Folded)
		;	Folded = [Code| Rest]
		),
		fold_codes(Codes, Profile, Rest).

	mark(Code) :-
		unicode_data_mark_range(Start, End),
		Code >= Start,
		Code =< End,
		!.

:- end_category.
