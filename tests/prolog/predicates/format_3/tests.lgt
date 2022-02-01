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


	test(lgt_format_3_empty_control_sequence, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "", [])},
		^^text_output_assertion(out, '', Assertion).

	test(lgt_format_3_empty_arguments, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "abc", [])},
		^^text_output_assertion(out, 'abc', Assertion).

	test(lgt_format_3_tilde, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~~", [])},
		^^text_output_assertion(out, '~', Assertion).

	test(lgt_format_3_write_numbervars, true(Assertion)) :-
		^^set_text_output(out, ''),
		Term = a(_),
		{numbervars(Term, 0, _)},
		{format(out, "~w", [Term])},
		^^text_output_assertion(out, 'a(A)', Assertion).

	test(lgt_format_3_write, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~w", ['ABC'])},
		^^text_output_assertion(out, 'ABC', Assertion).

	test(lgt_format_3_write_variable, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(out, ''),
		A = A,	% avoid singleton warnings
		{format(out, "~w", [A])},
		^^text_output_contents(out, Chars).

	test(lgt_format_3_quoted, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~q", ['ABC'])},
		^^text_output_assertion(out, '\'ABC\'', Assertion).

	test(lgt_format_3_quoted_variable, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(out, ''),
		A = A,	% avoid singleton warnings
		{format(out, "~q", [A])},
		^^text_output_contents(out, Chars).

	test(lgt_format_3_canonical, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~k", [(:-a)])},
		^^text_output_assertion(out, ':-(a)', Assertion).

	test(lgt_format_3_atom, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~a", [abc])},
		^^text_output_assertion(out, abc, Assertion).

	test(lgt_format_3_atom_no_quoting, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~a", ['ABC'])},
		^^text_output_assertion(out, 'ABC', Assertion).

	test(lgt_format_3_code, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~c", [65])},
		^^text_output_assertion(out, 'A', Assertion).

	test(lgt_format_3_code_n, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~8c", [65])},
		^^text_output_assertion(out, 'AAAAAAAA', Assertion).

	test(lgt_format_3_string, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~s", [[65,66,67]])},
		^^text_output_assertion(out, 'ABC', Assertion).

	test(lgt_format_3_string_first_n, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~3s", [[65,66,67,68,69,70,71,72,73]])},
		^^text_output_assertion(out, 'ABC', Assertion).

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
		^^set_text_output(out, ''),
		{format(out, "~n", [])},
		^^text_output_assertion(out, '\r\n', Assertion).

	test(lgt_format_3_new_line_n, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~4n", [])},
		^^text_output_assertion(out, '\r\n\r\n\r\n\r\n', Assertion).

	test(lgt_format_3_new_line_if_not_beginning_of_line, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~Nbegin~N~Nend", [])},
		^^text_output_assertion(out, 'begin\r\nend', Assertion).

	:- else.

	test(lgt_format_3_new_line, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~n", [])},
		^^text_output_assertion(out, '\n', Assertion).

	test(lgt_format_3_new_line_n, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~4n", [])},
		^^text_output_assertion(out, '\n\n\n\n', Assertion).

	test(lgt_format_3_new_line_if_not_beginning_of_line, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~Nbegin~N~Nend", [])},
		^^text_output_assertion(out, 'begin\nend', Assertion).

	:- endif.

	test(lgt_format_3_ignore, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~a~i~a", [a,b,c])},
		^^text_output_assertion(out, ac, Assertion).

	test(lgt_format_3_decimal, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~d", [123])},
		^^text_output_assertion(out, '123', Assertion).

	test(lgt_format_3_decimal_n, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~1d", [123])},
		^^text_output_assertion(out, '12.3', Assertion).

	test(lgt_format_3_decimal_group, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~D", [1234567890])},
		^^text_output_assertion(out, '1,234,567,890', Assertion).

	test(lgt_format_3_decimal_group_n, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~2D", [1234567890])},
		^^text_output_assertion(out, '12,345,678.90', Assertion).

	test(lgt_format_3_radix_2, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~2r", [127])},
		^^text_output_assertion(out, '1111111', Assertion).

	test(lgt_format_3_radix_8, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~8r", [127])},
		^^text_output_assertion(out, '177', Assertion).

	test(lgt_format_3_radix_16, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~16r", [127])},
		^^text_output_assertion(out, '7f', Assertion).

	test(lgt_format_3_radix_upper_case_2, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~2R", [127])},
		^^text_output_assertion(out, '1111111', Assertion).

	test(lgt_format_3_radix_upper_case_8, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~8R", [127])},
		^^text_output_assertion(out, '177', Assertion).

	test(lgt_format_3_radix_upper_case_16, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~16R", [127])},
		^^text_output_assertion(out, '7F', Assertion).

	test(lgt_format_3_radix_default, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~r", [16])},
		^^text_output_assertion(out, '20', Assertion).

	test(lgt_format_3_radix_default_upper_case, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~R", [16])},
		^^text_output_assertion(out, '20', Assertion).

	test(lgt_format_3_radix_invalid_1, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~0r", [16])}.

	test(lgt_format_3_radix_invalid_2, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~1R", [16])}.

	test(lgt_format_3_radix_invalid_3, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~37r", [16])}.

	test(lgt_format_3_radix_invalid_4, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~42R", [16])}.

	test(lgt_format_3_float, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~f", [1.0])},
		% default is six decimal places
		^^text_output_assertion(out, '1.000000', Assertion).

	test(lgt_format_3_float_n_places, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~4f", [-1.0e-1])},
		^^text_output_assertion(out, '-0.1000', Assertion).

	test(lgt_format_3_float_exponential_notation, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~e", [1.333333])},
		% default is six decimal places
		^^text_output_assertion(out, '1.333333e+00', Assertion).

	test(lgt_format_3_float_exponential_notation_n_places, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~3e", [1.333333])},
		^^text_output_assertion(out, '1.333e+00', Assertion).

	test(lgt_format_3_float_exponential_notation_upper_case, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~E", [1.333333])},
		% default is six decimal places
		^^text_output_assertion(out, '1.333333E+00', Assertion).

	test(lgt_format_3_float_exponential_notation_n_places_upper_case, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~3E", [1.333333])},
		^^text_output_assertion(out, '1.333E+00', Assertion).

	test(lgt_format_3_float_best_e, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~g", [0.00000123])},
		^^text_output_assertion(out, '1.23e-06', Assertion).

	test(lgt_format_3_float_best_f, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~g", [0.39265e+3])},
		^^text_output_assertion(out, '392.65', Assertion).

	test(lgt_format_3_float_best_e_upper_case, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~G", [0.00000123])},
		^^text_output_assertion(out, '1.23E-06', Assertion).

	test(lgt_format_3_float_best_f_upper_case, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~G", [0.39265e+3])},
		^^text_output_assertion(out, '392.65', Assertion).

	test(lgt_format_3_tab_atom_left_aligned, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~|~a~t~8+", [abcd])},
		^^text_output_assertion(out, 'abcd    ', Assertion).

	test(lgt_format_3_tab_integer_left_aligned, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~|~d~t~8+", [1234])},
		^^text_output_assertion(out, '1234    ', Assertion).

	test(lgt_format_3_tab_atom_center_aligned, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~t~a~t~8+", [abcd])},
		^^text_output_assertion(out, '  abcd  ', Assertion).

	test(lgt_format_3_tab_integer_center_aligned, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~t~d~t~8+", [1234])},
		^^text_output_assertion(out, '  1234  ', Assertion).

	test(lgt_format_3_tab_atom_right_aligned, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~t~a~8|", [abcd])},
		^^text_output_assertion(out, '    abcd', Assertion).

	test(lgt_format_3_tab_integer_right_aligned, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~t~d~8|", [1234])},
		^^text_output_assertion(out, '    1234', Assertion).

	test(lgt_format_3_tab_all_alignments_in, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~t~a~10|~t~a~t~13+~a~t~10+", ['Alpha','and','Omega'])},
		^^text_output_assertion(out, '     Alpha     and     Omega     ', Assertion).

	test(lgt_format_3_tab_all_alignments_out, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~|~a~t~10|~t~a~t~13+~t~a~10+", ['Alpha','and','Omega'])},
		^^text_output_assertion(out, 'Alpha          and          Omega', Assertion).

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
			^^set_text_output(out, ''),
				forall(
				list::member(Line, [['abc','defg','hi'],['j','kl','mnopq']]),
				{format(out, "~|~a~t~8|~a~t~8+~a~t~8+~n", Line)}
			),
			^^text_output_assertion(out, 'abc     defg    hi      \r\nj       kl      mnopq   \r\n', Assertion).

		test(lgt_format_3_tab_table_right_aligned, true(Assertion)) :-
			^^set_text_output(out, ''),
				forall(
				list::member(Line, [['123','4567','89'],['1','23','45678']]),
				{format(out, "~|~t~a~8|~t~a~8+~t~a~8+~n", Line)}
			),
			^^text_output_assertion(out, '     123    4567      89\r\n       1      23   45678\r\n', Assertion).

	:- else.

		test(lgt_format_3_tab_table_left_aligned, true(Assertion)) :-
			^^set_text_output(out, ''),
				forall(
				list::member(Line, [['abc','defg','hi'],['j','kl','mnopq']]),
				{format(out, "~|~a~t~8|~a~t~8+~a~t~8+~n", Line)}
			),
			^^text_output_assertion(out, 'abc     defg    hi      \nj       kl      mnopq   \n', Assertion).

		test(lgt_format_3_tab_table_right_aligned, true(Assertion)) :-
			^^set_text_output(out, ''),
				forall(
				list::member(Line, [['123','4567','89'],['1','23','45678']]),
				{format(out, "~|~t~a~8|~t~a~8+~t~a~8+~n", Line)}
			),
			^^text_output_assertion(out, '     123    4567      89\n       1      23   45678\n', Assertion).

	:- endif.

	test(lgt_format_3_tab_table_fill_character_code, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~61t~8|", [])},
		^^text_output_assertion(out, '========', Assertion).

	test(lgt_format_3_tab_table_fill_character, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~`0t~2r~16+", [0xFF])},
		^^text_output_assertion(out, '0000000011111111', Assertion).

	test(lgt_format_3_unbound_first_argument, error(instantiation_error)) :-
		{format(_, _, [42])}.

	test(lgt_format_3_unbound_second_argument, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, _, [42])}.

	test(lgt_format_3_unbound_third_argument, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~d", _)}.

	test(lgt_format_3_first_argument_wrong_type, error(domain_error(stream_or_alias,3.14))) :-
		{format(3.14, "~d", [42])}.

	test(lgt_format_3_non_existing_stream, errors([existence_error(stream,foo), domain_error(stream_or_alias,foo)])) :-
		{format(foo, "~d", [42])}.

	test(lgt_format_3_second_argument_wrong_type, error(type_error(_,42))) :-
		^^set_text_output(out, ''),
		{format(out, 42, [42])}.

	test(lgt_format_3_third_argument_wrong_type, error(type_error(list,42))) :-
		^^set_text_output(out, ''),
		{format(out, "~d", 42)}.

	test(lgt_format_3_invalid_argument_1, errors([type_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~a", [42])}.

	test(lgt_format_3_invalid_argument_2, errors([type_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~d", [abc])}.

	test(lgt_format_3_not_enough_arguments_1, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~a", [])}.

	test(lgt_format_3_not_enough_arguments_2, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~a ~d ~a", [abc, 42])}.

	cleanup :-
		^^clean_text_output.
