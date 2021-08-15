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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:8:0,
		author is 'Paulo Moura',
		date is 2021-08-15,
		comment is 'Unit tests for the de facto Prolog standard format/3 built-in predicate.'
	]).

	test(lgt_format_3_empty_control_sequence, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '', [])},
		^^text_output_assertion('', Assertion).

	test(lgt_format_3_empty_arguments, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, 'abc', [])},
		^^text_output_assertion('abc', Assertion).

	test(lgt_format_3_tilde, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~~', [])},
		^^text_output_assertion('~', Assertion).

	test(lgt_format_3_write, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~w', ['ABC'])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_3_quoted, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~q', ['ABC'])},
		^^text_output_assertion('\'ABC\'', Assertion).

	test(lgt_format_3_canonical, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~k', [(:-a)])},
		^^text_output_assertion(':-(a)', Assertion).

	test(lgt_format_3_atom, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~a', [abc])},
		^^text_output_assertion(abc, Assertion).

	test(lgt_format_3_atom_no_quoting, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~a', ['ABC'])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_3_code, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~c', [65])},
		^^text_output_assertion('A', Assertion).

	test(lgt_format_3_code_n, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~8c', [65])},
		^^text_output_assertion('AAAAAAAA', Assertion).

	test(lgt_format_3_string, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~s', [[65,66,67]])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_2_string_first_n, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~3s', [[65,66,67,68,69,70,71,72,73]])},
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

	test(lgt_format_3_new_line, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~n', [])},
		^^text_output_assertion('\r\n', Assertion).

	test(lgt_format_3_new_line_n, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~4n', [])},
		^^text_output_assertion('\r\n\r\n\r\n\r\n', Assertion).

	test(lgt_format_3_new_line_if_not_beginning_of_line, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~Nbegin~N~Nend', [])},
		^^text_output_assertion('begin\r\nend', Assertion).

	:- else.

	test(lgt_format_3_new_line, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~n', [])},
		^^text_output_assertion('\n', Assertion).

	test(lgt_format_3_new_line_n, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~4n', [])},
		^^text_output_assertion('\n\n\n\n', Assertion).

	test(lgt_format_3_new_line_if_not_beginning_of_line, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~Nbegin~N~Nend', [])},
		^^text_output_assertion('begin\nend', Assertion).

	:- endif.

	test(lgt_format_3_ignore, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~a~i~a', [a,b,c])},
		^^text_output_assertion(ac, Assertion).

	test(lgt_format_3_decimal, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~d', [123])},
		^^text_output_assertion('123', Assertion).

	test(lgt_format_3_decimal_n, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~1d', [123])},
		^^text_output_assertion('12.3', Assertion).

	test(lgt_format_3_decimal_group, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~D', [1234567890])},
		^^text_output_assertion('1,234,567,890', Assertion).

	test(lgt_format_3_decimal_group_n, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~2D', [1234567890])},
		^^text_output_assertion('12,345,678.90', Assertion).

	test(lgt_format_3_radix_2, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~2r', [127])},
		^^text_output_assertion('1111111', Assertion).

	test(lgt_format_3_radix_8, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~8r', [127])},
		^^text_output_assertion('177', Assertion).

	test(lgt_format_3_radix_16, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~16r', [127])},
		^^text_output_assertion('7f', Assertion).

	test(lgt_format_3_radix_upper_case_2, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~2R', [127])},
		^^text_output_assertion('1111111', Assertion).

	test(lgt_format_3_radix_upper_case_8, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~8R', [127])},
		^^text_output_assertion('177', Assertion).

	test(lgt_format_3_radix_upper_case_16, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~16R', [127])},
		^^text_output_assertion('7F', Assertion).

	test(lgt_format_3_radix_default, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~r', [16])},
		^^text_output_assertion('20', Assertion).

	test(lgt_format_3_radix_default_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~R', [16])},
		^^text_output_assertion('20', Assertion).

	test(lgt_format_3_radix_invalid_1, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~0r', [16])}.

	test(lgt_format_3_radix_invalid_2, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~1R', [16])}.

	test(lgt_format_3_radix_invalid_3, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~37r', [16])}.

	test(lgt_format_3_radix_invalid_4, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~42R', [16])}.

	test(lgt_format_3_float, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~f', [1.0])},
		% default is six decimal places
		^^text_output_assertion('1.000000', Assertion).

	test(lgt_format_3_float_n_places, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~4f', [-1.0e-1])},
		^^text_output_assertion('-0.1000', Assertion).

	test(lgt_format_3_float_exponential_notation, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~e', [1.333333])},
		% default is six decimal places
		^^text_output_assertion('1.333333e+00', Assertion).

	test(lgt_format_3_float_exponential_notation_n_places, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~3e', [1.333333])},
		^^text_output_assertion('1.333e+00', Assertion).

	test(lgt_format_3_float_exponential_notation_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~E', [1.333333])},
		% default is six decimal places
		^^text_output_assertion('1.333333E+00', Assertion).

	test(lgt_format_3_float_exponential_notation_n_places_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~3E', [1.333333])},
		^^text_output_assertion('1.333E+00', Assertion).

	test(lgt_format_3_float_best_e, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~g', [0.00000123])},
		^^text_output_assertion('1.23e-06', Assertion).

	test(lgt_format_3_float_best_f, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~g', [0.39265e+3])},
		^^text_output_assertion('392.65', Assertion).

	test(lgt_format_3_float_best_e_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~G', [0.00000123])},
		^^text_output_assertion('1.23E-06', Assertion).

	test(lgt_format_3_float_best_f_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~G', [0.39265e+3])},
		^^text_output_assertion('392.65', Assertion).

	test(lgt_format_3_tab_atom_left_aligned, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~|~a~t~8+', [abcd])},
		^^text_output_assertion('abcd    ', Assertion).

	test(lgt_format_3_tab_integer_left_aligned, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~|~d~t~8+', [1234])},
		^^text_output_assertion('1234    ', Assertion).

	test(lgt_format_3_tab_atom_center_aligned, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~t~a~t~8+', [abcd])},
		^^text_output_assertion('  abcd  ', Assertion).

	test(lgt_format_3_tab_integer_center_aligned, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~t~d~t~8+', [1234])},
		^^text_output_assertion('  1234  ', Assertion).

	test(lgt_format_3_tab_atom_right_aligned, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~t~a~8|', [abcd])},
		^^text_output_assertion('    abcd', Assertion).

	test(lgt_format_3_tab_integer_right_aligned, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~t~d~8|', [1234])},
		^^text_output_assertion('    1234', Assertion).

	test(lgt_format_3_tab_all_alignments_in, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~t~a~10|~t~a~t~13+~a~t~10+', ['Alpha','and','Omega'])},
		^^text_output_assertion('     Alpha     and     Omega     ', Assertion).

	test(lgt_format_3_tab_all_alignments_out, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~|~a~t~10|~t~a~t~13+~t~a~10+', ['Alpha','and','Omega'])},
		^^text_output_assertion('Alpha          and          Omega', Assertion).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

		test(lgt_format_3_tab_table_left_aligned, true(Assertion)) :-
			^^set_text_output(''),
			current_output(S),
			forall(
				list::member(Line, [['abc','defg','hi'],['j','kl','mnopq']]),
				{format(S, '~|~a~t~8|~a~t~8+~a~t~8+~n', Line)}
			),
			^^text_output_assertion('abc     defg    hi      \r\nj       kl      mnopq   \r\n', Assertion).

		test(lgt_format_3_tab_table_right_aligned, true(Assertion)) :-
			^^set_text_output(''),
			current_output(S),
			forall(
				list::member(Line, [['123','4567','89'],['1','23','45678']]),
				{format(S, '~|~t~a~8|~t~a~8+~t~a~8+~n', Line)}
			),
			^^text_output_assertion('     123    4567      89\r\n       1      23   45678\r\n', Assertion).

	:- else.

		test(lgt_format_3_tab_table_left_aligned, true(Assertion)) :-
			^^set_text_output(''),
			current_output(S),
			forall(
				list::member(Line, [['abc','defg','hi'],['j','kl','mnopq']]),
				{format(S, '~|~a~t~8|~a~t~8+~a~t~8+~n', Line)}
			),
			^^text_output_assertion('abc     defg    hi      \nj       kl      mnopq   \n', Assertion).

		test(lgt_format_3_tab_table_right_aligned, true(Assertion)) :-
			^^set_text_output(''),
			current_output(S),
			forall(
				list::member(Line, [['123','4567','89'],['1','23','45678']]),
				{format(S, '~|~t~a~8|~t~a~8+~t~a~8+~n', Line)}
			),
			^^text_output_assertion('     123    4567      89\n       1      23   45678\n', Assertion).

	:- endif.

	test(lgt_format_3_tab_table_fill_character, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~61t~8|', [])},
		^^text_output_assertion('========', Assertion).

	test(lgt_format_3_unbound_first_argument, error(instantiation_error)) :-
		{format(_, _, [42])}.

	test(lgt_format_3_unbound_second_argument, error(instantiation_error)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, _, [42])}.

	test(lgt_format_3_unbound_third_argument, error(instantiation_error)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~d', _)}.

	test(lgt_format_3_first_argument_wrong_type, error(domain_error(stream_or_alias,3.14))) :-
		{format(3.14, '~d', [42])}.

	test(lgt_format_3_second_argument_wrong_type, error(type_error(_,42))) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, 42, [42])}.

	test(lgt_format_3_third_argument_wrong_type, error(type_error(list,42))) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~d', 42)}.

	test(lgt_format_3_invalid_argument_1, errors([type_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~a', [42])}.

	test(lgt_format_3_invalid_argument_2, errors([type_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~d', [abc])}.

	test(lgt_format_3_not_enough_arguments_1, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~a', [])}.

	test(lgt_format_3_not_enough_arguments_2, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~a ~d ~a', [abc, 42])}.

	cleanup :-
		^^clean_text_output.

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, xsb)).
	% workaround XSB atom-based module system
	:- import(from(/(format,3), format)).
:- endif.
