%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2021-11-30,
		comment is 'Unit tests for the "books" example.'
	]).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(books_01, true(Assertion)) :-
			^^set_text_output(''),
			data_processing::print,
			^^text_output_assertion('The Philosopher''s Stone\r\n  with free quidditch_set at 2000 gr\r\nThe Chamber of Secrets\r\n  with free map\r\nThe Prisoner of Azkaban\r\nThe Goblet of Fire\r\nThe Order of the Phoenix\r\nThe Half-Blood Prince\r\n  with free audio_cd\r\nThe Deathly Hallows\r\n  with free horcrux_set at 1000 gr\r\n', Assertion).

		test(books_02, true(Assertion)) :-
			^^set_text_output(''),
			data_processing::print_kg,
			^^text_output_assertion('The Philosopher''s Stone\r\n  with free quidditch_set at 2 kg\r\nThe Chamber of Secrets\r\n  with free map\r\nThe Prisoner of Azkaban\r\nThe Goblet of Fire\r\nThe Order of the Phoenix\r\nThe Half-Blood Prince\r\n  with free audio_cd\r\nThe Deathly Hallows\r\n  with free horcrux_set at 1 kg\r\n', Assertion).

		test(books_03, true(Assertion)) :-
			^^set_text_output(''),
			data_processing::print_heavy_extras,
			^^text_output_assertion('quidditch_set at 2000 gr\r\nhorcrux_set at 1000 gr\r\n', Assertion).

	:- else.

		test(books_01, true(Assertion)) :-
			^^set_text_output(''),
			data_processing::print,
			^^text_output_assertion('The Philosopher''s Stone\n  with free quidditch_set at 2000 gr\nThe Chamber of Secrets\n  with free map\nThe Prisoner of Azkaban\nThe Goblet of Fire\nThe Order of the Phoenix\nThe Half-Blood Prince\n  with free audio_cd\nThe Deathly Hallows\n  with free horcrux_set at 1000 gr\n', Assertion).

		test(books_02, true(Assertion)) :-
			^^set_text_output(''),
			data_processing::print_kg,
			^^text_output_assertion('The Philosopher''s Stone\n  with free quidditch_set at 2 kg\nThe Chamber of Secrets\n  with free map\nThe Prisoner of Azkaban\nThe Goblet of Fire\nThe Order of the Phoenix\nThe Half-Blood Prince\n  with free audio_cd\nThe Deathly Hallows\n  with free horcrux_set at 1 kg\n', Assertion).

		test(books_03, true(Assertion)) :-
			^^set_text_output(''),
			data_processing::print_heavy_extras,
			^^text_output_assertion('quidditch_set at 2000 gr\nhorcrux_set at 1000 gr\n', Assertion).

	:- endif.

	test(books_04, true(Titles == ['The Philosopher''s Stone', 'The Chamber of Secrets', 'The Half-Blood Prince', 'The Deathly Hallows'])) :-
		data_processing::books_with_extras(Titles).

:- end_object.
