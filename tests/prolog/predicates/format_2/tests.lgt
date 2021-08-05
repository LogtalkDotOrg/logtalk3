%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- set_prolog_flag(double_quotes, codes).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2021-08-05,
		comment is 'Unit tests for the de facto Prolog standard format/2 built-in predicate.'
	]).

	test(lgt_format_2_empty_control_sequence, true(Assertion)) :-
		^^set_text_output(''),
		{format('', [])},
		^^text_output_assertion('', Assertion).

	test(lgt_format_2_empty_arguments, true(Assertion)) :-
		^^set_text_output(''),
		{format('abc', [])},
		^^text_output_assertion('abc', Assertion).

	test(lgt_format_2_tilde, true(Assertion)) :-
		^^set_text_output(''),
		{format('~~', [])},
		^^text_output_assertion('~', Assertion).

	test(lgt_format_2_write, true(Assertion)) :-
		^^set_text_output(''),
		{format('~w', ['ABC'])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_2_quoted, true(Assertion)) :-
		^^set_text_output(''),
		{format('~q', ['ABC'])},
		^^text_output_assertion('\'ABC\'', Assertion).

	test(lgt_format_2_canonical, true(Assertion)) :-
		^^set_text_output(''),
		{format('~k', [(:-a)])},
		^^text_output_assertion(':-(a)', Assertion).

	test(lgt_format_2_atom, true(Assertion)) :-
		^^set_text_output(''),
		{format('~a', [abc])},
		^^text_output_assertion(abc, Assertion).

	test(lgt_format_2_atom_no_quoting, true(Assertion)) :-
		^^set_text_output(''),
		{format('~a', ['ABC'])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_2_code, true(Assertion)) :-
		^^set_text_output(''),
		{format('~c', [65])},
		^^text_output_assertion('A', Assertion).

	test(lgt_format_2_code_n, true(Assertion)) :-
		^^set_text_output(''),
		{format('~8c', [65])},
		^^text_output_assertion('AAAAAAAA', Assertion).

	test(lgt_format_2_string, true(Assertion)) :-
		^^set_text_output(''),
		{format('~s', ["ABC"])},
		^^text_output_assertion('ABC', Assertion).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

	test(lgt_format_2_new_line, true(Assertion)) :-
		^^set_text_output(''),
		{format('~n', [])},
		^^text_output_assertion('\r\n', Assertion).

	test(lgt_format_2_new_line_n, true(Assertion)) :-
		^^set_text_output(''),
		{format('~4n', [])},
		^^text_output_assertion('\r\n\r\n\r\n\r\n', Assertion).

	test(lgt_format_2_new_line_if_not_beginning_of_line, true(Assertion)) :-
		^^set_text_output(''),
		{format('~Nbegin~N~Nend', [])},
		^^text_output_assertion('begin\r\nend', Assertion).

	:- else.

	test(lgt_format_2_new_line, true(Assertion)) :-
		^^set_text_output(''),
		{format('~n', [])},
		^^text_output_assertion('\n', Assertion).

	test(lgt_format_2_new_line_n, true(Assertion)) :-
		^^set_text_output(''),
		{format('~4n', [])},
		^^text_output_assertion('\n\n\n\n', Assertion).

	test(lgt_format_2_new_line_if_not_beginning_of_line, true(Assertion)) :-
		^^set_text_output(''),
		{format('~Nbegin~N~Nend', [])},
		^^text_output_assertion('begin\nend', Assertion).

	:- endif.

	- test(lgt_format_2_tab, true(Assertion)) :-
		^^set_text_output(''),
		{format('~t~a~8|', [abc])},
		^^text_output_assertion('     abc', Assertion).

	test(lgt_format_2_ignore, true(Assertion)) :-
		^^set_text_output(''),
		{format('~a~i~a', [a,b,c])},
		^^text_output_assertion(ac, Assertion).

	test(lgt_format_2_decimal, true(Assertion)) :-
		^^set_text_output(''),
		{format('~d', [123])},
		^^text_output_assertion('123', Assertion).

	test(lgt_format_2_float, true(Assertion)) :-
		^^set_text_output(''),
		{format('~f', [1.0])},
		% default is six decimal places
		^^text_output_assertion('1.000000', Assertion).

	test(lgt_format_2_float_n_places, true(Assertion)) :-
		^^set_text_output(''),
		{format('~4f', [-1.0e-1])},
		^^text_output_assertion('-0.1000', Assertion).

	test(lgt_format_2_float_exponential_notation, true(Assertion)) :-
		^^set_text_output(''),
		{format('~e', [1.333333])},
		% default is six decimal places
		^^text_output_assertion('1.333333e+00', Assertion).

	test(lgt_format_2_float_exponential_notation_n_places, true(Assertion)) :-
		^^set_text_output(''),
		{format('~3e', [1.333333])},
		^^text_output_assertion('1.333e+00', Assertion).

	test(lgt_format_2_float_exponential_notation_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format('~E', [1.333333])},
		% default is six decimal places
		^^text_output_assertion('1.333333E+00', Assertion).

	test(lgt_format_2_float_exponential_notation_n_places_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format('~3E', [1.333333])},
		^^text_output_assertion('1.333E+00', Assertion).

	test(lgt_format_2_float_best_e, true(Assertion)) :-
		^^set_text_output(''),
		{format('~g', [0.00000123])},
		^^text_output_assertion('1.23e-06', Assertion).

	test(lgt_format_2_float_best_f, true(Assertion)) :-
		^^set_text_output(''),
		{format('~g', [0.39265e+3])},
		^^text_output_assertion('392.65', Assertion).

	test(lgt_format_2_float_best_e_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format('~G', [0.00000123])},
		^^text_output_assertion('1.23E-06', Assertion).

	test(lgt_format_2_float_best_f_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format('~G', [0.39265e+3])},
		^^text_output_assertion('392.65', Assertion).

	cleanup :-
		^^clean_text_output.

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, xsb)).
	% workaround XSB atom-based module system
	:- import(from(/(format,2), format)).
:- endif.
