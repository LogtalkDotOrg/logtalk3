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


:- category(http_text_helpers).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-05,
		comment is 'Internal shared helpers for ASCII case normalization and HTTP optional whitespace trimming.'
	]).

	:- uses(list, [
		reverse/2
	]).

	:- protected(ows_code/1).
	:- mode(ows_code(+integer), zero_or_one).
	:- info(ows_code/1, [
		comment is 'Succeeds when the code is HTTP optional whitespace (SP or HTAB).',
		argnames is ['Code']
	]).

	:- protected(lowercase_ascii_codes/2).
	:- mode(lowercase_ascii_codes(+list(integer), -list(integer)), one).
	:- info(lowercase_ascii_codes/2, [
		comment is 'Converts uppercase ASCII letter codes in a list to lowercase, leaving all other codes unchanged.',
		argnames is ['Codes', 'LowercaseCodes']
	]).

	:- protected(uppercase_ascii_codes/2).
	:- mode(uppercase_ascii_codes(+list(integer), -list(integer)), one).
	:- info(uppercase_ascii_codes/2, [
		comment is 'Converts lowercase ASCII letter codes in a list to uppercase, leaving all other codes unchanged.',
		argnames is ['Codes', 'UppercaseCodes']
	]).

	:- protected(trim_trailing_ows_codes/2).
	:- mode(trim_trailing_ows_codes(+list(integer), -list(integer)), one).
	:- info(trim_trailing_ows_codes/2, [
		comment is 'Removes trailing HTTP optional whitespace codes from a list.',
		argnames is ['Codes', 'TrimmedCodes']
	]).

	:- protected(trim_ows_codes/2).
	:- mode(trim_ows_codes(+list(integer), -list(integer)), one).
	:- info(trim_ows_codes/2, [
		comment is 'Removes leading and trailing HTTP optional whitespace codes from a list.',
		argnames is ['Codes', 'TrimmedCodes']
	]).

	ows_code(32).
	ows_code(0'\t).

	lowercase_ascii_codes([], []).
	lowercase_ascii_codes([Code| Codes], [LowercaseCode| LowercaseCodes]) :-
		lowercase_ascii_code(Code, LowercaseCode),
		lowercase_ascii_codes(Codes, LowercaseCodes).

	uppercase_ascii_codes([], []).
	uppercase_ascii_codes([Code| Codes], [UppercaseCode| UppercaseCodes]) :-
		uppercase_ascii_code(Code, UppercaseCode),
		uppercase_ascii_codes(Codes, UppercaseCodes).

	trim_trailing_ows_codes(Codes, TrimmedCodes) :-
		reverse(Codes, ReversedCodes),
		trim_leading_ows_codes(ReversedCodes, ReversedTrimmedCodes),
		reverse(ReversedTrimmedCodes, TrimmedCodes).

	trim_ows_codes(Codes, TrimmedCodes) :-
		trim_leading_ows_codes(Codes, LeadingTrimmedCodes),
		trim_trailing_ows_codes(LeadingTrimmedCodes, TrimmedCodes).

	lowercase_ascii_code(Code, LowercaseCode) :-
		Code >= 0'A,
		Code =< 0'Z,
		!,
		LowercaseCode is Code + 32.
	lowercase_ascii_code(Code, Code).

	uppercase_ascii_code(Code, UppercaseCode) :-
		Code >= 0'a,
		Code =< 0'z,
		!,
		UppercaseCode is Code - 32.
	uppercase_ascii_code(Code, Code).

	trim_leading_ows_codes([Code| Codes], TrimmedCodes) :-
		ows_code(Code),
		!,
		trim_leading_ows_codes(Codes, TrimmedCodes).
	trim_leading_ows_codes(Codes, Codes).

:- end_category.
