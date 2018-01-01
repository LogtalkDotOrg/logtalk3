%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2017/01/14,
		comment is 'Unit tests for the ISO Prolog standard numbers syntax plus tests for de facto standard syntax extensions.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.1

	succeeds(iso_number_01) :-
		^^set_text_input('123. '),
		{read(T)},
		T == 123.

	succeeds(iso_number_02) :-
		^^set_text_input('-123. '),
		{read(T)},
		T == -123.

	succeeds(iso_number_03) :-
		^^set_text_input('123.45. '),
		{read(T)},
		T == 123.45.

	succeeds(iso_number_04) :-
		^^set_text_input('-123.45. '),
		{read(T)},
		T == -123.45.

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.4.4

	succeeds(iso_number_05) :-
		^^set_text_input('83. '),
		{read(T)},
		T == 83.

	succeeds(iso_number_06) :-
		^^set_text_input('0\'S. '),
		{read(T)},
		char_code('S', T).

	succeeds(iso_number_07) :-
		^^set_text_input('0b1010011. '),
		{read(T)},
		T == 83.

	succeeds(iso_number_08) :-
		^^set_text_input('0o123. '),
		{read(T)},
		T == 83.

	succeeds(iso_number_09) :-
		^^set_text_input('0x53. '),
		{read(T)},
		T == 83.

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.4.5

	succeeds(iso_number_10) :-
		^^set_text_input('123.45. '),
		{read(T)},
		T == 123.45.

	succeeds(iso_number_11) :-
		^^set_text_input('123.45e+1. '),
		{read(T)},
		T == 1234.5.

	succeeds(iso_number_12) :-
		^^set_text_input('123.45E+1. '),
		{read(T)},
		T == 1234.5.

	succeeds(iso_number_13) :-
		^^set_text_input('123.45e-1. '),
		{read(T)},
		T == 12.345.

	succeeds(iso_number_14) :-
		^^set_text_input('123.45E-1. '),
		{read(T)},
		T == 12.345.

	% additional tests for the 0'Char number notation: control characters

	% 0'\a - alarm
	succeeds(lgt_number_15) :-
		^^set_text_input('0\'\\a. '),
		{read(T)},
		T == 7.

	% 0'\b - backspace
	succeeds(lgt_number_16) :-
		^^set_text_input('0\'\\b. '),
		{read(T)},
		T == 8.

	% 0'\t - horizontal tab
	succeeds(lgt_number_17) :-
		^^set_text_input('0\'\\t. '),
		{read(T)},
		T == 9.

	% 0'\n - newline
	succeeds(lgt_number_18) :-
		^^set_text_input('0\'\\n. '),
		{read(T)},
		T == 10.

	% 0'\v - vertical tab
	succeeds(lgt_number_19) :-
		^^set_text_input('0\'\\v. '),
		{read(T)},
		T == 11.

	% 0'\f - form feed
	succeeds(lgt_number_20) :-
		^^set_text_input('0\'\\f. '),
		{read(T)},
		T == 12.

	% 0'\r - carriage return
	succeeds(lgt_number_21) :-
		^^set_text_input('0\'\\r. '),
		{read(T)},
		T == 13.

	% 0'\e - escape
	succeeds(lgt_number_22) :-
		^^set_text_input('0\'\\e. '),
		{read(T)},
		T == 27.

	% 0'\d - delete
	succeeds(lgt_number_23) :-
		^^set_text_input('0\'\\d. '),
		{read(T)},
		T == 127.	

	% additional tests for the 0'Char number notation: space

	% 0'\s - space
	succeeds(lgt_number_24) :-
		^^set_text_input('0\'\\s. '),
		{read(T)},
		T == 32.

	% additional tests for the 0'Char number notation: single quote

	% ISO spec
	succeeds(lgt_number_25) :-
		^^set_text_input('0\'\'\'. '),
		{read(T)},
		T == 39.

	% de facto standard
	succeeds(lgt_number_26) :-
		^^set_text_input('0\'\'. '),
		{read(T)},
		T == 39.

	cleanup :-
		^^clean_text_input.

:- end_object.
