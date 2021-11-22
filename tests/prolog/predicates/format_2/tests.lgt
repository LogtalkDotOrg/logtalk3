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


	test(lgt_format_2_empty_control_sequence, true(Assertion)) :-
		^^set_text_output(''),
		{format("", [])},
		^^text_output_assertion('', Assertion).

	test(lgt_format_2_empty_arguments, true(Assertion)) :-
		^^set_text_output(''),
		{format("abc", [])},
		^^text_output_assertion('abc', Assertion).

	test(lgt_format_2_tilde, true(Assertion)) :-
		^^set_text_output(''),
		{format("~~", [])},
		^^text_output_assertion('~', Assertion).

	test(lgt_format_2_write, true(Assertion)) :-
		^^set_text_output(''),
		{format("~w", ['ABC'])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_2_write_numbervars, true(Assertion)) :-
		^^set_text_output(''),
		Term = a(_),
		{numbervars(Term, 0, _)},
		{format("~w", [Term])},
		^^text_output_assertion('a(A)', Assertion).

	test(lgt_format_2_write_variable, true(Chars = ['_', _| _])) :-
		^^set_text_output(''),
		A = A,	% avoid singleton warnings
		{format("~w", [A])},
		^^text_output_contents(Chars).

	test(lgt_format_2_quoted, true(Assertion)) :-
		^^set_text_output(''),
		{format("~q", ['ABC'])},
		^^text_output_assertion('\'ABC\'', Assertion).

	test(lgt_format_2_quoted_variable, true(Chars = ['_', _| _])) :-
		^^set_text_output(''),
		A = A,	% avoid singleton warnings
		{format("~q", [A])},
		^^text_output_contents(Chars).

	test(lgt_format_2_canonical, true(Assertion)) :-
		^^set_text_output(''),
		{format("~k", [(:-a)])},
		^^text_output_assertion(':-(a)', Assertion).

	test(lgt_format_2_atom, true(Assertion)) :-
		^^set_text_output(''),
		{format("~a", [abc])},
		^^text_output_assertion(abc, Assertion).

	test(lgt_format_2_atom_no_quoting, true(Assertion)) :-
		^^set_text_output(''),
		{format("~a", ['ABC'])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_2_code, true(Assertion)) :-
		^^set_text_output(''),
		{format("~c", [65])},
		^^text_output_assertion('A', Assertion).

	test(lgt_format_2_code_n, true(Assertion)) :-
		^^set_text_output(''),
		{format("~8c", [65])},
		^^text_output_assertion('AAAAAAAA', Assertion).

	test(lgt_format_2_string, true(Assertion)) :-
		^^set_text_output(''),
		{format("~s", [[65,66,67]])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_2_string_first_n, true(Assertion)) :-
		^^set_text_output(''),
		{format("~3s", [[65,66,67,68,69,70,71,72,73]])},
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
			{format("~n", [])},
			^^text_output_assertion('\r\n', Assertion).

		test(lgt_format_2_new_line_n, true(Assertion)) :-
			^^set_text_output(''),
			{format("~4n", [])},
			^^text_output_assertion('\r\n\r\n\r\n\r\n', Assertion).

		test(lgt_format_2_new_line_if_not_beginning_of_line, true(Assertion)) :-
			^^set_text_output(''),
			{format("~Nbegin~N~Nend", [])},
			^^text_output_assertion('begin\r\nend', Assertion).

	:- else.

		test(lgt_format_2_new_line, true(Assertion)) :-
			^^set_text_output(''),
			{format("~n", [])},
			^^text_output_assertion('\n', Assertion).

		test(lgt_format_2_new_line_n, true(Assertion)) :-
			^^set_text_output(''),
			{format("~4n", [])},
			^^text_output_assertion('\n\n\n\n', Assertion).

		test(lgt_format_2_new_line_if_not_beginning_of_line, true(Assertion)) :-
			^^set_text_output(''),
			{format("~Nbegin~N~Nend", [])},
			^^text_output_assertion('begin\nend', Assertion).

	:- endif.

	test(lgt_format_2_ignore, true(Assertion)) :-
		^^set_text_output(''),
		{format("~a~i~a", [a,b,c])},
		^^text_output_assertion(ac, Assertion).

	test(lgt_format_2_decimal, true(Assertion)) :-
		^^set_text_output(''),
		{format("~d", [123])},
		^^text_output_assertion('123', Assertion).

	test(lgt_format_2_decimal_n, true(Assertion)) :-
		^^set_text_output(''),
		{format("~1d", [123])},
		^^text_output_assertion('12.3', Assertion).

	test(lgt_format_2_decimal_group, true(Assertion)) :-
		^^set_text_output(''),
		{format("~D", [1234567890])},
		^^text_output_assertion('1,234,567,890', Assertion).

	test(lgt_format_2_decimal_group_n, true(Assertion)) :-
		^^set_text_output(''),
		{format("~2D", [1234567890])},
		^^text_output_assertion('12,345,678.90', Assertion).

	test(lgt_format_2_radix_2, true(Assertion)) :-
		^^set_text_output(''),
		{format("~2r", [127])},
		^^text_output_assertion('1111111', Assertion).

	test(lgt_format_2_radix_8, true(Assertion)) :-
		^^set_text_output(''),
		{format("~8r", [127])},
		^^text_output_assertion('177', Assertion).

	test(lgt_format_2_radix_16, true(Assertion)) :-
		^^set_text_output(''),
		{format("~16r", [127])},
		^^text_output_assertion('7f', Assertion).

	test(lgt_format_2_radix_upper_case_2, true(Assertion)) :-
		^^set_text_output(''),
		{format("~2R", [127])},
		^^text_output_assertion('1111111', Assertion).

	test(lgt_format_2_radix_upper_case_8, true(Assertion)) :-
		^^set_text_output(''),
		{format("~8R", [127])},
		^^text_output_assertion('177', Assertion).

	test(lgt_format_2_radix_upper_case_16, true(Assertion)) :-
		^^set_text_output(''),
		{format("~16R", [127])},
		^^text_output_assertion('7F', Assertion).

	test(lgt_format_2_radix_default, true(Assertion)) :-
		^^set_text_output(''),
		{format("~r", [16])},
		^^text_output_assertion('20', Assertion).

	test(lgt_format_2_radix_default_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format("~R", [16])},
		^^text_output_assertion('20', Assertion).

	test(lgt_format_2_radix_invalid_1, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~0r", [16])}.

	test(lgt_format_2_radix_invalid_2, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~1R", [16])}.

	test(lgt_format_2_radix_invalid_3, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~37r", [16])}.

	test(lgt_format_2_radix_invalid_4, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~42R", [16])}.

	test(lgt_format_2_float, true(Assertion)) :-
		^^set_text_output(''),
		{format("~f", [1.0])},
		% default is six decimal places
		^^text_output_assertion('1.000000', Assertion).

	test(lgt_format_2_float_n_places, true(Assertion)) :-
		^^set_text_output(''),
		{format("~4f", [-1.0e-1])},
		^^text_output_assertion('-0.1000', Assertion).

	test(lgt_format_2_float_exponential_notation, true(Assertion)) :-
		^^set_text_output(''),
		{format("~e", [1.333333])},
		% default is six decimal places
		^^text_output_assertion('1.333333e+00', Assertion).

	test(lgt_format_2_float_exponential_notation_n_places, true(Assertion)) :-
		^^set_text_output(''),
		{format("~3e", [1.333333])},
		^^text_output_assertion('1.333e+00', Assertion).

	test(lgt_format_2_float_exponential_notation_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format("~E", [1.333333])},
		% default is six decimal places
		^^text_output_assertion('1.333333E+00', Assertion).

	test(lgt_format_2_float_exponential_notation_n_places_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format("~3E", [1.333333])},
		^^text_output_assertion('1.333E+00', Assertion).

	test(lgt_format_2_float_best_e, true(Assertion)) :-
		^^set_text_output(''),
		{format("~g", [0.00000123])},
		^^text_output_assertion('1.23e-06', Assertion).

	test(lgt_format_2_float_best_f, true(Assertion)) :-
		^^set_text_output(''),
		{format("~g", [0.39265e+3])},
		^^text_output_assertion('392.65', Assertion).

	test(lgt_format_2_float_best_e_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format("~G", [0.00000123])},
		^^text_output_assertion('1.23E-06', Assertion).

	test(lgt_format_2_float_best_f_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format("~G", [0.39265e+3])},
		^^text_output_assertion('392.65', Assertion).

	test(lgt_format_2_tab_atom_left_aligned, true(Assertion)) :-
		^^set_text_output(''),
		{format("~|~a~t~8+", [abcd])},
		^^text_output_assertion('abcd    ', Assertion).

	test(lgt_format_2_tab_integer_left_aligned, true(Assertion)) :-
		^^set_text_output(''),
		{format("~|~d~t~8+", [1234])},
		^^text_output_assertion('1234    ', Assertion).

	test(lgt_format_2_tab_atom_center_aligned, true(Assertion)) :-
		^^set_text_output(''),
		{format("~t~a~t~8+", [abcd])},
		^^text_output_assertion('  abcd  ', Assertion).

	test(lgt_format_2_tab_integer_center_aligned, true(Assertion)) :-
		^^set_text_output(''),
		{format("~t~d~t~8+", [1234])},
		^^text_output_assertion('  1234  ', Assertion).

	test(lgt_format_2_tab_atom_right_aligned, true(Assertion)) :-
		^^set_text_output(''),
		{format("~t~a~8|", [abcd])},
		^^text_output_assertion('    abcd', Assertion).

	test(lgt_format_2_tab_integer_right_aligned, true(Assertion)) :-
		^^set_text_output(''),
		{format("~t~d~8|", [1234])},
		^^text_output_assertion('    1234', Assertion).

	test(lgt_format_2_tab_all_alignments_in, true(Assertion)) :-
		^^set_text_output(''),
		{format("~t~a~10|~t~a~t~13+~a~t~10+", ['Alpha','and','Omega'])},
		^^text_output_assertion('     Alpha     and     Omega     ', Assertion).

	test(lgt_format_2_tab_all_alignments_out, true(Assertion)) :-
		^^set_text_output(''),
		{format("~|~a~t~10|~t~a~t~13+~t~a~10+", ['Alpha','and','Omega'])},
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

		test(lgt_format_2_tab_table_left_aligned, true(Assertion)) :-
			^^set_text_output(''),
			forall(
				list::member(Line, [['abc','defg','hi'],['j','kl','mnopq']]),
				{format("~|~a~t~8|~a~t~8+~a~t~8+~n", Line)}
			),
			^^text_output_assertion('abc     defg    hi      \r\nj       kl      mnopq   \r\n', Assertion).

		test(lgt_format_2_tab_table_right_aligned, true(Assertion)) :-
			^^set_text_output(''),
			forall(
				list::member(Line, [['123','4567','89'],['1','23','45678']]),
				{format("~|~t~a~8|~t~a~8+~t~a~8+~n", Line)}
			),
			^^text_output_assertion('     123    4567      89\r\n       1      23   45678\r\n', Assertion).

	:- else.

		test(lgt_format_2_tab_table_left_aligned, true(Assertion)) :-
			^^set_text_output(''),
			forall(
				list::member(Line, [['abc','defg','hi'],['j','kl','mnopq']]),
				{format("~|~a~t~8|~a~t~8+~a~t~8+~n", Line)}
			),
			^^text_output_assertion('abc     defg    hi      \nj       kl      mnopq   \n', Assertion).

		test(lgt_format_2_tab_table_right_aligned, true(Assertion)) :-
			^^set_text_output(''),
			forall(
				list::member(Line, [['123','4567','89'],['1','23','45678']]),
				{format("~|~t~a~8|~t~a~8+~t~a~8+~n", Line)}
			),
			^^text_output_assertion('     123    4567      89\n       1      23   45678\n', Assertion).

	:- endif.

	test(lgt_format_2_tab_table_fill_character_code, true(Assertion)) :-
		^^set_text_output(''),
		{format("~61t~8|", [])},
		^^text_output_assertion('========', Assertion).

	test(lgt_format_2_tab_table_fill_character, true(Assertion)) :-
		^^set_text_output(''),
		{format("~`0t~2r~16+", [0xFF])},
		^^text_output_assertion('0000000011111111', Assertion).

	test(lgt_format_2_unbound_first_argument, error(instantiation_error)) :-
		^^set_text_output(''),
		{format(_, [42])}.

	test(lgt_format_2_unbound_second_argument, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~d", _)}.

	test(lgt_format_2_first_argument_wrong_type, error(type_error(_,42))) :-
		^^set_text_output(''),
		{format(42, [42])}.

	test(lgt_format_2_second_argument_wrong_type, error(type_error(list,42))) :-
		^^set_text_output(''),
		{format("~d", 42)}.

	test(lgt_format_2_invalid_argument_1, errors([type_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~a", [42])}.

	test(lgt_format_2_invalid_argument_2, errors([type_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~d", [abc])}.

	test(lgt_format_2_not_enough_arguments_1, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~a", [])}.

	test(lgt_format_2_not_enough_arguments_2, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~a ~d ~a", [abc, 42])}.

	cleanup :-
		^^clean_text_output.
