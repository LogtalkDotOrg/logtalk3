%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2021-08-31,
		comment is 'Unit tests for the ISO Prolog standard numbers syntax plus tests for de facto standard syntax extensions.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.1

	test(iso_number_01, true(T == 123)) :-
		^^set_text_input('123. '),
		{read(T)}.

	test(iso_number_02, true(T == -123)) :-
		^^set_text_input('-123. '),
		{read(T)}.

	test(iso_number_03, true(T == 123.45)) :-
		^^set_text_input('123.45. '),
		{read(T)}.

	test(iso_number_04, true(T == -123.45)) :-
		^^set_text_input('-123.45. '),
		{read(T)}.

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.4.4

	test(iso_number_05, true(T == 83)) :-
		^^set_text_input('83. '),
		{read(T)}.

	test(iso_number_06, true(char_code('S', T))) :-
		^^set_text_input('0\'S. '),
		{read(T)}.

	test(iso_number_07, true(T == 83)) :-
		^^set_text_input('0b1010011. '),
		{read(T)}.

	test(iso_number_08, true(T == 83)) :-
		^^set_text_input('0o123. '),
		{read(T)}.

	test(iso_number_09, true(T == 83)) :-
		^^set_text_input('0x53. '),
		{read(T)}.

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.4.5

	test(iso_number_10, true(T == 123.45)) :-
		^^set_text_input('123.45. '),
		{read(T)}.

	test(iso_number_11, true(T == 1234.5)) :-
		^^set_text_input('123.45e+1. '),
		{read(T)}.

	test(iso_number_12, true(T == 1234.5)) :-
		^^set_text_input('123.45E+1. '),
		{read(T)}.

	test(iso_number_13, true(T == 12.345)) :-
		^^set_text_input('123.45e-1. '),
		{read(T)}.

	test(iso_number_14, true(T == 12.345)) :-
		^^set_text_input('123.45E-1. '),
		{read(T)}.

	% tests for the 0'Char notation: escaped quotes and escaped backslash

	% 0'\' - single quote
	test(lgt_number_15, true(T == 39)) :-
		^^set_text_input('0\'\\\'. '),
		{read(T)}.

	% 0'\" - double quote
	test(lgt_number_16, true(T == 34)) :-
		^^set_text_input('0\'\\". '),
		{read(T)}.

	% 0'\` - back quote
	test(lgt_number_17, true(T == 96)) :-
		^^set_text_input('0\'\\`. '),
		{read(T)}.

	% 0'\\ - back slash
	test(lgt_number_18, true(T == 92)) :-
		^^set_text_input('0\'\\\\. '),
		{read(T)}.

	% tests for the 0'Char number notation: control characters

	% 0'\a - alarm
	test(lgt_number_19, true(T == 7)) :-
		^^set_text_input('0\'\\a. '),
		{read(T)}.

	% 0'\b - backspace
	test(lgt_number_20, true(T == 8)) :-
		^^set_text_input('0\'\\b. '),
		{read(T)}.

	% 0'\t - horizontal tab
	test(lgt_number_21, true(T == 9)) :-
		^^set_text_input('0\'\\t. '),
		{read(T)}.

	% 0'\n - newline
	test(lgt_number_22, true(T == 10)) :-
		^^set_text_input('0\'\\n. '),
		{read(T)}.

	% 0'\v - vertical tab
	test(lgt_number_23, true(T == 11)) :-
		^^set_text_input('0\'\\v. '),
		{read(T)}.

	% 0'\f - form feed
	test(lgt_number_24, true(T == 12)) :-
		^^set_text_input('0\'\\f. '),
		{read(T)}.

	% 0'\r - carriage return
	test(lgt_number_25, true(T == 13)) :-
		^^set_text_input('0\'\\r. '),
		{read(T)}.

	% 0'\e - escape
	test(lgt_number_26, true(T == 27)) :-
		^^set_text_input('0\'\\e. '),
		{read(T)}.

	% 0'\d - delete
	test(lgt_number_27, true(T == 127)) :-
		^^set_text_input('0\'\\d. '),
		{read(T)}.

	% additional tests for the 0'Char number notation

	% 0'\s - space
	test(lgt_number_28, true(T == 32)) :-
		^^set_text_input('0\'\\s. '),
		{read(T)}.

	% tests for the 0'Char number notation: single quote

	% ISO spec
	test(lgt_number_29,  true(T == 39)) :-
		^^set_text_input('0\'\'\'. '),
		{read(T)}.

	% de facto standard
	test(lgt_number_30,  true(T == 39)) :-
		^^set_text_input('0\'\'. '),
		{read(T)}.

	% invalid numbers

	test(lgt_number_31, error(syntax_error(_))) :-
		^^set_text_input('.0. '),
		{read(_)}.

	test(lgt_number_32, error(syntax_error(_))) :-
		^^set_text_input('.33. '),
		{read(_)}.

	test(lgt_number_33, error(syntax_error(_))) :-
		^^set_text_input('1e. '),
		{read(_)}.

	test(lgt_number_34, error(syntax_error(_))) :-
		^^set_text_input('1e33. '),
		{read(_)}.

	test(lgt_number_35, error(syntax_error(_))) :-
		^^set_text_input('1E33. '),
		{read(_)}.

	test(lgt_number_36, error(syntax_error(_))) :-
		^^set_text_input('0b101020. '),
		{read(_)}.

	test(lgt_number_37, error(syntax_error(_))) :-
		^^set_text_input('0B101010. '),
		{read(_)}.

	test(lgt_number_38, error(syntax_error(_))) :-
		^^set_text_input('0o31784. '),
		{read(_)}.

	test(lgt_number_39, error(syntax_error(_))) :-
		^^set_text_input('0O31754. '),
		{read(_)}.

	test(lgt_number_40, error(syntax_error(_))) :-
		^^set_text_input('0x31AG84. '),
		{read(_)}.

	test(lgt_number_41, error(syntax_error(_))) :-
		^^set_text_input('0X31AF84. '),
		{read(_)}.

	% optional sign in exponent in floating-point numbers

	test(lgt_number_42, true(T == 1234.5)) :-
		^^set_text_input('123.45e1. '),
		{read(T)}.

	test(lgt_number_43, true(T == 1234.5)) :-
		^^set_text_input('123.45E1. '),
		{read(T)}.

	cleanup :-
		^^clean_text_input.

:- end_object.
