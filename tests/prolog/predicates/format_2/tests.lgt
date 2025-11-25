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


	test(lgt_format_2_empty_control_sequence, true(Assertion)) :-
		^^set_text_output(''),
		{format("", [])},
		^^text_output_assertion('', Assertion).

	test(lgt_format_2_empty_arguments, true(Assertion)) :-
		^^set_text_output(''),
		{format("abc", [])},
		^^text_output_assertion('abc', Assertion).

	% ~~ control sequence

	test(lgt_format_2_tilde, true(Assertion)) :-
		^^set_text_output(''),
		{format("~~", [])},
		^^text_output_assertion('~', Assertion).

	test(lgt_format_2_tilde_ignore_count, true(Assertion)) :-
		^^set_text_output(''),
		{format("~3~", [])},
		^^text_output_assertion('~', Assertion).

	% ~w control sequence

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

	test(lgt_format_2_write_variable, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(''),
		A = A,	% avoid singleton warnings
		{format("~w", [A])},
		^^text_output_contents(Chars).

	% ~q control sequence

	test(lgt_format_2_quoted, true(Assertion)) :-
		^^set_text_output(''),
		{format("~q", ['ABC'])},
		^^text_output_assertion('\'ABC\'', Assertion).

	test(lgt_format_2_quoted_variable, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(''),
		A = A,	% avoid singleton warnings
		{format("~q", [A])},
		^^text_output_contents(Chars).

	% ~k control sequence

	test(lgt_format_2_canonical_operators, true(Assertion)) :-
		^^set_text_output(''),
		{format("~k", [(:-a)])},
		^^text_output_assertion(':-(a)', Assertion).

	test(lgt_format_2_canonical_quotes, true(Assertion)) :-
		^^set_text_output(''),
		{format("~k", ['A'+'B'])},
		^^text_output_assertion('+(\'A\',\'B\')', Assertion).

	% ~p control sequence

	test(lgt_format_2_print_default, true(Assertion)) :-
		^^set_text_output(''),
		{format("~p", [42])},
		^^text_output_assertion('42', Assertion).

	test(lgt_format_2_print_portray_01, true(Assertion)) :-
		^^set_text_output(''),
		{format("~p", [3.14])},
		^^text_output_assertion('3', Assertion).

	test(lgt_format_2_print_portray_02, true(Assertion)) :-
		^^set_text_output(''),
		{format("~p", [foo])},
		^^text_output_assertion(foofoo, Assertion).

	test(lgt_format_2_print_portray_03, true(Assertion)) :-
		^^set_text_output(''),
		{format("~p", [a(foo)])},
		^^text_output_assertion('a(foofoo)', Assertion).

	test(lgt_format_2_print_portray_04, true(Assertion)) :-
		^^set_text_output(''),
		{format("~p", [a(foo,b(c(foo,3.14)))])},
		^^text_output_assertion('a(foofoo,b(c(foofoo,3)))', Assertion).

	% ~a control sequence

	test(lgt_format_2_atom, true(Assertion)) :-
		^^set_text_output(''),
		{format("~a", [abc])},
		^^text_output_assertion(abc, Assertion).

	test(lgt_format_2_atom_no_quoting, true(Assertion)) :-
		^^set_text_output(''),
		{format("~a", ['ABC'])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_2_atom_empty_list, true(Assertion)) :-
		^^set_text_output(''),
		{format("~a", [[]])},
		^^text_output_assertion('[]', Assertion).

	test(lgt_format_2_atom_empty_curly, true(Assertion)) :-
		^^set_text_output(''),
		{format("~a", [{}])},
		^^text_output_assertion('{}', Assertion).

	test(lgt_format_2_atom_ignore_fixed_count, true(Assertion)) :-
		^^set_text_output(''),
		{format("~3a", [abc])},
		^^text_output_assertion(abc, Assertion).

	test(lgt_format_2_atom_ignore_star_count, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*a", [3,abc])},
		^^text_output_assertion(abc, Assertion).

	test(lgt_format_2_atom_ignore_expression_count, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*a", [1+2,abc])},
		^^text_output_assertion(abc, Assertion).

	test(lgt_format_2_atom_invalid_01, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~a", [_])}.

	test(lgt_format_2_atom_invalid_02, error(type_error(atom,[0'a,0'b,0'c]))) :-
		^^set_text_output(''),
		{format("~a", [[0'a,0'b,0'c]])}.

	test(lgt_format_2_atom_invalid_03, error(type_error(atom,[a,b,c]))) :-
		^^set_text_output(''),
		{format("~a", [[a,b,c]])}.

	test(lgt_format_2_atom_invalid_04, error(type_error(atom,42))) :-
		^^set_text_output(''),
		{format("~a", [42])}.

	test(lgt_format_2_atom_invalid_05, error(type_error(atom,foo(bar)))) :-
		^^set_text_output(''),
		{format("~a", [foo(bar)])}.

	% ~c control sequence

	test(lgt_format_2_code, true(Assertion)) :-
		^^set_text_output(''),
		{format("~c", [0'A])},
		^^text_output_assertion('A', Assertion).

	test(lgt_format_2_code_n, true(Assertion)) :-
		^^set_text_output(''),
		{format("~8c", [0'A])},
		^^text_output_assertion('AAAAAAAA', Assertion).

	test(lgt_format_2_code_star, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*c", [8,0'A])},
		^^text_output_assertion('AAAAAAAA', Assertion).

	test(lgt_format_2_code_expression, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*c", [3+5,0'A])},
		^^text_output_assertion('AAAAAAAA', Assertion).

	test(lgt_format_2_code_invalid_01, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~c", [_])}.

	test(lgt_format_2_code_invalid_02, error(type_error(evaluable,a/0))) :-
		^^set_text_output(''),
		{format("~c", [a])}.

	% ~s control sequence

	test(lgt_format_2_string_codes, true(Assertion)) :-
		^^set_text_output(''),
		{format("~s", [[65,66,67]])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_2_string_chars, true(Assertion)) :-
		^^set_text_output(''),
		{format("~s", [['A','B','C']])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_2_string_atom, true(Assertion)) :-
		^^set_text_output(''),
		{format("~s", ['ABC'])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_2_string_first_n, true(Assertion)) :-
		^^set_text_output(''),
		{format("~3s", [[65,66,67,68,69,70,71,72,73]])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_2_string_first_star, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*s", [3,[65,66,67,68,69,70,71,72,73]])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_2_string_zero, true(Assertion)) :-
		^^set_text_output(''),
		{format("~0s", [[65,66,67,68,69,70,71,72,73]])},
		^^text_output_assertion('', Assertion).

	test(lgt_format_2_string_pad_with_spaces, true(Assertion)) :-
		^^set_text_output(''),
		{format("~6s", [[65,66,67]])},
		^^text_output_assertion('ABC   ', Assertion).

	test(lgt_format_2_string_invalid_01, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~s", [_])}.

	test(lgt_format_2_string_invalid_02, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~s", [65,66|_])}.

	test(lgt_format_2_string_invalid_03, error(type_error(_,42))) :-
		^^set_text_output(''),
		{format("~s", [42])}.

	% ~n and ~N control sequences

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

		test(lgt_format_2_new_line_star, true(Assertion)) :-
			^^set_text_output(''),
			{format("~*n", [4])},
			^^text_output_assertion('\n\n\n\n', Assertion).

		test(lgt_format_2_new_line_if_not_beginning_of_line, true(Assertion)) :-
			^^set_text_output(''),
			{format("~Nbegin~N~Nend", [])},
			^^text_output_assertion('begin\nend', Assertion).

	:- endif.

	% ~i control sequence

	test(lgt_format_2_ignore, true(Assertion)) :-
		^^set_text_output(''),
		{format("~a~i~a", [a,b,c])},
		^^text_output_assertion(ac, Assertion).

	test(lgt_format_2_ignore_fixed_count, true(Assertion)) :-
		^^set_text_output(''),
		{format("~a~3i~a", [a,b,c])},
		^^text_output_assertion(ac, Assertion).

	test(lgt_format_2_ignore_start_count, true(Assertion)) :-
		^^set_text_output(''),
		{format("~a~*i~a", [a,3,b,c])},
		^^text_output_assertion(ac, Assertion).

	test(lgt_format_2_ignore_expression_count, true(Assertion)) :-
		^^set_text_output(''),
		{format("~a~*i~a", [a,1+2,b,c])},
		^^text_output_assertion(ac, Assertion).

	% ~d and ~D control sequences

	test(lgt_format_2_decimal, true(Assertion)) :-
		^^set_text_output(''),
		{format("~d", [123])},
		^^text_output_assertion('123', Assertion).

	test(lgt_format_2_decimal_n, true(Assertion)) :-
		^^set_text_output(''),
		{format("~1d", [123])},
		^^text_output_assertion('12.3', Assertion).

	test(lgt_format_2_decimal_star, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*d", [1,123])},
		^^text_output_assertion('12.3', Assertion).

	test(lgt_format_2_decimal_group, true(Assertion)) :-
		^^set_text_output(''),
		{format("~D", [1234567890])},
		^^text_output_assertion('1,234,567,890', Assertion).

	test(lgt_format_2_decimal_group_n, true(Assertion)) :-
		^^set_text_output(''),
		{format("~2D", [1234567890])},
		^^text_output_assertion('12,345,678.90', Assertion).

	test(lgt_format_2_decimal_group_star, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*D", [2,1234567890])},
		^^text_output_assertion('12,345,678.90', Assertion).

	test(lgt_format_2_decimal_invalid_01, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~d", [_])}.

	test(lgt_format_2_decimal_invalid_02, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~D", [_])}.

	test(lgt_format_2_decimal_invalid_07, error(type_error(integer, 123.0))) :-
		^^set_text_output(''),
		{format("~d", [123.0])}.

	test(lgt_format_2_decimal_invalid_08, error(type_error(integer, 123.0))) :-
		^^set_text_output(''),
		{format("~D", [123.0])}.

	test(lgt_format_2_decimal_invalid_09, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(''),
		{format("~d", [foo(bar)])}.

	test(lgt_format_2_decimal_invalid_10, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(''),
		{format("~D", [foo(bar)])}.

	test(lgt_format_2_decimal_invalid_11, error(domain_error(_,_))) :-
		^^set_text_output(''),
		{format("~*d", [-1,123])}.

	test(lgt_format_2_decimal_invalid_12, error(domain_error(_,_))) :-
		^^set_text_output(''),
		{format("~*D", [-1,123])}.

	% ~r and ~R control sequences

	test(lgt_format_2_radix_2, true(Assertion)) :-
		^^set_text_output(''),
		{format("~2r", [127])},
		^^text_output_assertion('1111111', Assertion).

	test(lgt_format_2_radix_8, true(Assertion)) :-
		^^set_text_output(''),
		{format("~8r", [127])},
		^^text_output_assertion('177', Assertion).

	test(lgt_format_2_radix_10, true(Assertion)) :-
		^^set_text_output(''),
		{format("~10r", [127])},
		^^text_output_assertion('127', Assertion).

	test(lgt_format_2_radix_16, true(Assertion)) :-
		^^set_text_output(''),
		{format("~16r", [127])},
		^^text_output_assertion('7f', Assertion).

	test(lgt_format_2_radix_36, true(Assertion)) :-
		^^set_text_output(''),
		{format("~36r", [127])},
		^^text_output_assertion('3j', Assertion).

	test(lgt_format_2_radix_36_expression, true(Assertion)) :-
		^^set_text_output(''),
		{format("~36r", [36*36*36-1])},
		^^text_output_assertion('zzz', Assertion).

	test(lgt_format_2_radix_star, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*r", [16,127])},
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

	test(lgt_format_2_radix_upper_case_36, true(Assertion)) :-
		^^set_text_output(''),
		{format("~36R", [127])},
		^^text_output_assertion('3J', Assertion).

	test(lgt_format_2_radix_upper_case_36_expression, true(Assertion)) :-
		^^set_text_output(''),
		{format("~36R", [36*36*36-1])},
		^^text_output_assertion('ZZZ', Assertion).

	test(lgt_format_2_radix_upper_case_star, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*R", [16,127])},
		^^text_output_assertion('7F', Assertion).

	test(lgt_format_2_radix_default, true(Assertion)) :-
		^^set_text_output(''),
		{format("~r", [16])},
		^^text_output_assertion('20', Assertion).

	test(lgt_format_2_radix_default_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format("~R", [16])},
		^^text_output_assertion('20', Assertion).

	test(lgt_format_2_radix_invalid_01, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~r", [_])}.

	test(lgt_format_2_radix_invalid_02, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~R", [_])}.

	test(lgt_format_2_radix_invalid_03, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~0r", [16])}.

	test(lgt_format_2_radix_invalid_04, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~1R", [16])}.

	test(lgt_format_2_radix_invalid_05, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~37r", [16])}.

	test(lgt_format_2_radix_invalid_06, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~42R", [16])}.

	test(lgt_format_2_radix_invalid_07, error(type_error(integer, 123.0))) :-
		^^set_text_output(''),
		{format("~r", [123.0])}.

	test(lgt_format_2_radix_invalid_08, error(type_error(integer, 123.0))) :-
		^^set_text_output(''),
		{format("~R", [123.0])}.

	test(lgt_format_2_radix_invalid_09, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(''),
		{format("~r", [foo(bar)])}.

	test(lgt_format_2_radix_invalid_10, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(''),
		{format("~R", [foo(bar)])}.

	test(lgt_format_2_radix_invalid_11, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~*r", [-1,123])}.

	test(lgt_format_2_radix_invalid_12, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~*R", [-1,123])}.

	% ~f control sequence

	test(lgt_format_2_float, true(Assertion)) :-
		^^set_text_output(''),
		{format("~f", [1.0])},
		% default is six decimal places
		^^text_output_assertion('1.000000', Assertion).

	test(lgt_format_2_float_expression, true(Assertion)) :-
		^^set_text_output(''),
		{format("~f", [1+1.0])},
		% default is six decimal places
		^^text_output_assertion('2.000000', Assertion).

	test(lgt_format_2_float_n_places_01, true(Assertion)) :-
		^^set_text_output(''),
		{format("~4f", [-1.0e-1])},
		^^text_output_assertion('-0.1000', Assertion).

	test(lgt_format_2_float_n_places_02, true(Assertion)) :-
		^^set_text_output(''),
		{format("~3f", [12.3456789])},
		^^text_output_assertion('12.346', Assertion).

	test(lgt_format_2_float_n_places_03, true(Assertion)) :-
		^^set_text_output(''),
		{format("~12f", [12.3456789])},
		^^text_output_assertion('12.345678900000', Assertion).

	test(lgt_format_2_float_star_places, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*f", [4,-1.0e-1])},
		^^text_output_assertion('-0.1000', Assertion).

	test(lgt_format_2_float_zero_places, true(Assertion)) :-
		^^set_text_output(''),
		{format("~0f", [123.456])},
		^^text_output_assertion('123', Assertion).

	test(lgt_format_2_float_invalid_01, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~f", [_])}.

	test(lgt_format_2_float_invalid_02, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(''),
		{format("~f", [foo(bar)])}.

	% ~e and ~E control sequences

	test(lgt_format_2_float_exponential_notation, true(Assertion)) :-
		^^set_text_output(''),
		{format("~e", [1.333333])},
		% default is six decimal places
		^^text_output_assertion('1.333333e+00', Assertion).

	test(lgt_format_2_float_exponential_notation_expression, true(Assertion)) :-
		^^set_text_output(''),
		{format("~e", [1.333333+1])},
		% default is six decimal places
		^^text_output_assertion('2.333333e+00', Assertion).

	test(lgt_format_2_float_exponential_notation_n_places, true(Assertion)) :-
		^^set_text_output(''),
		{format("~3e", [1.333333])},
		^^text_output_assertion('1.333e+00', Assertion).

	test(lgt_format_2_float_exponential_notation_star_places, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*e", [3,1.333333])},
		^^text_output_assertion('1.333e+00', Assertion).

	test(lgt_format_2_float_exponential_notation_zero_places, true(Assertion)) :-
		^^set_text_output(''),
		{format("~0e", [1.333333])},
		^^text_output_assertion('1e+00', Assertion).

	test(lgt_format_2_float_exponential_notation_invalid_01, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~e", [_])}.

	test(lgt_format_2_float_exponential_notation_invalid_02, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(''),
		{format("~e", [foo(bar)])}.

	test(lgt_format_2_float_exponential_notation_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format("~E", [1.333333])},
		% default is six decimal places
		^^text_output_assertion('1.333333E+00', Assertion).

	test(lgt_format_2_float_exponential_notation_upper_case_expression, true(Assertion)) :-
		^^set_text_output(''),
		{format("~E", [1.333333+1])},
		% default is six decimal places
		^^text_output_assertion('2.333333E+00', Assertion).

	test(lgt_format_2_float_exponential_notation_n_places_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format("~3E", [1.333333])},
		^^text_output_assertion('1.333E+00', Assertion).

	test(lgt_format_2_float_exponential_notation_upper_case_star_places, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*E", [3,1.333333])},
		^^text_output_assertion('1.333E+00', Assertion).

	test(lgt_format_2_float_exponential_notation_upper_case_zero_places, true(Assertion)) :-
		^^set_text_output(''),
		{format("~0E", [1.333333])},
		^^text_output_assertion('1E+00', Assertion).

	test(lgt_format_2_float_exponential_notation_upper_case_invalid_01, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~E", [_])}.

	test(lgt_format_2_float_exponential_notation_upper_case_invalid_02, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(''),
		{format("~E", [foo(bar)])}.

	% ~g and ~G control sequences

	test(lgt_format_2_float_best_e, true(Assertion)) :-
		^^set_text_output(''),
		{format("~g", [0.00000123])},
		^^text_output_assertion('1.23e-06', Assertion).

	test(lgt_format_2_float_best_ne, true(Assertion)) :-
		^^set_text_output(''),
		{format("~2g", [0.00000123])},
		^^text_output_assertion('1.2e-06', Assertion).

	test(lgt_format_2_float_best_f, true(Assertion)) :-
		^^set_text_output(''),
		{format("~g", [0.39265e+3])},
		^^text_output_assertion('392.65', Assertion).

	test(lgt_format_2_float_best_nf, true(Assertion)) :-
		^^set_text_output(''),
		{format("~2g", [0.39265e+3])},
		^^text_output_assertion('3.9e+02', Assertion).

	test(lgt_format_2_float_best_invalid_01, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~g", [_])}.

	test(lgt_format_2_float_best_invalid_02, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(''),
		{format("~g", [foo(bar)])}.

	test(lgt_format_2_float_best_e_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format("~G", [0.00000123])},
		^^text_output_assertion('1.23E-06', Assertion).

	test(lgt_format_2_float_best_ne_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format("~2G", [0.00000123])},
		^^text_output_assertion('1.2E-06', Assertion).

	test(lgt_format_2_float_best_f_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format("~G", [0.39265e+3])},
		^^text_output_assertion('392.65', Assertion).

	test(lgt_format_2_float_best_nf_upper_case, true(Assertion)) :-
		^^set_text_output(''),
		{format("~2G", [0.39265e+3])},
		^^text_output_assertion('3.9E+02', Assertion).

	test(lgt_format_2_float_best_upper_case_invalid_01, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~G", [_])}.

	test(lgt_format_2_float_best_upper_case_invalid_02, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(''),
		{format("~G", [foo(bar)])}.

	% ~| and ~t control sequences

	test(lgt_format_2_tab_stop_default, true(Assertion)) :-
		^^set_text_output(''),
		{format("~|~a", [abcd])},
		^^text_output_assertion('abcd', Assertion).

	test(lgt_format_2_tab_stop_non_default, true(Assertion)) :-
		^^set_text_output(''),
		{format("~6|~a", [abcd])},
		^^text_output_assertion('      abcd', Assertion).

	test(lgt_format_2_tab_atom_left_aligned_default_column_boundary, true(Assertion)) :-
		^^set_text_output(''),
		{format("~|~a~t~+", [abcd])},
		^^text_output_assertion('abcd    ', Assertion).

	test(lgt_format_2_tab_atom_left_aligned, true(Assertion)) :-
		^^set_text_output(''),
		{format("~|~a~t~8+", [abcd])},
		^^text_output_assertion('abcd    ', Assertion).

	test(lgt_format_2_tab_integer_left_aligned_default_column_boundary, true(Assertion)) :-
		^^set_text_output(''),
		{format("~|~d~t~+", [1234])},
		^^text_output_assertion('1234    ', Assertion).

	test(lgt_format_2_tab_integer_left_aligned, true(Assertion)) :-
		^^set_text_output(''),
		{format("~|~d~t~8+", [1234])},
		^^text_output_assertion('1234    ', Assertion).

	test(lgt_format_2_tab_atom_center_aligned_default_column_boundary, true(Assertion)) :-
		^^set_text_output(''),
		{format("~t~a~t~+", [abcd])},
		^^text_output_assertion('  abcd  ', Assertion).

	test(lgt_format_2_tab_atom_center_aligned, true(Assertion)) :-
		^^set_text_output(''),
		{format("~t~a~t~8+", [abcd])},
		^^text_output_assertion('  abcd  ', Assertion).

	test(lgt_format_2_tab_integer_center_aligned_default_column_boundary, true(Assertion)) :-
		^^set_text_output(''),
		{format("~t~d~t~+", [1234])},
		^^text_output_assertion('  1234  ', Assertion).

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

	test(lgt_format_2_tab_table_pip_0110_01, true(Assertion)) :-
		^^set_text_output(''),
		{format("left~tright", [])},
		^^text_output_assertion('leftright', Assertion).

	test(lgt_format_2_tab_table_pip_0110_02, true(Assertion)) :-
		^^set_text_output(''),
		{format("left~tright~|", [])},
		^^text_output_assertion('leftright', Assertion).

	test(lgt_format_2_tab_table_pip_0110_03, true(Assertion)) :-
		^^set_text_output(''),
		{format("left~tright~8|", [])},
		^^text_output_assertion('leftright', Assertion).

	test(lgt_format_2_tab_table_pip_0110_04, true(Assertion)) :-
		^^set_text_output(''),
		{format("left~tright~20|", [])},
		^^text_output_assertion('left           right', Assertion).

	test(lgt_format_2_tab_table_pip_0110_05, true(Assertion)) :-
		^^set_text_output(''),
		{format("~|left~tright~20|", [])},
		^^text_output_assertion('left           right', Assertion).

	test(lgt_format_2_tab_table_pip_0110_06, true(Assertion)) :-
		^^set_text_output(''),
		{format("left~tright~20+", [])},
		^^text_output_assertion('left           right', Assertion).

	test(lgt_format_2_tab_table_pip_0110_07, true(Assertion)) :-
		^^set_text_output(''),
		{format("left~10+next", [])},
		^^text_output_assertion('left      next', Assertion).

	test(lgt_format_2_tab_table_pip_0110_08, true(Assertion)) :-
		^^set_text_output(''),
		{format("left~t~10+next", [])},
		^^text_output_assertion('left      next', Assertion).

	test(lgt_format_2_tab_table_pip_0110_09, true(Assertion)) :-
		^^set_text_output(''),
		{format("~tright~10+next", [])},
		^^text_output_assertion('     rightnext', Assertion).

	test(lgt_format_2_tab_table_pip_0110_10, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|abc~10+$",[])},
		^^text_output_assertion('^abc       $', Assertion).

	test(lgt_format_2_tab_table_pip_0110_11, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|abc~11|$",[])},
		^^text_output_assertion('^abc       $', Assertion).

	test(lgt_format_2_tab_table_pip_0110_12, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|abc~*+$",[10])},
		^^text_output_assertion('^abc       $', Assertion).

	test(lgt_format_2_tab_table_pip_0110_13, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|abc~*|$",[11])},
		^^text_output_assertion('^abc       $', Assertion).

	test(lgt_format_2_tab_table_pip_0110_14, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|abc~t~10+$",[])},
		^^text_output_assertion('^abc       $', Assertion).

	test(lgt_format_2_tab_table_pip_0110_15, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|~tabc~t~10+$",[])},
		^^text_output_assertion('^   abc    $', Assertion).

	test(lgt_format_2_tab_table_pip_0110_16, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|~t~tabc~t~10+$",[])},
		^^text_output_assertion('^    abc   $', Assertion).

	test(lgt_format_2_tab_table_pip_0110_17, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|~t~t~tabc~t~10+$",[])},
		^^text_output_assertion('^     abc  $', Assertion).

	test(lgt_format_2_tab_table_pip_0110_18, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|~tabc~10+$",[])},
		^^text_output_assertion('^       abc$', Assertion).

	test(lgt_format_2_tab_table_pip_0110_19, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|~ta~tb~tc~t~10+$",[])},
		^^text_output_assertion('^ a  b  c  $', Assertion).

	test(lgt_format_2_tab_table_pip_0110_20, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|a~tb~tc~t~10+$",[])},
		^^text_output_assertion('^a  b  c   $', Assertion).

	test(lgt_format_2_tab_table_pip_0110_21, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|~*tabc~*t~10+$",[0'.,95])},
		^^text_output_assertion('^...abc____$', Assertion).

	test(lgt_format_2_tab_table_pip_0110_22, true(Assertion)) :-
		^^set_text_output(''),
		{format("^~|~`.tabc~95t~10+$",[])},
		^^text_output_assertion('^...abc____$', Assertion).

	test(lgt_format_2_tab_table_pip_0110_23, true(Assertion)) :-
		^^set_text_output(''),
		{format("~*+.~n~3+.~n~*+.~n", [3,3])},
		^^text_output_assertion('   .\n   .\n   .\n', Assertion).

	% ~W control sequence

	test(lgt_format_2_write_term_default, true(Assertion)) :-
		^^set_text_output(''),
		{format("~W", [a+'B',[]])},
		^^text_output_assertion('a+B', Assertion).

	test(lgt_format_2_write_term_quoted, true(Assertion)) :-
		^^set_text_output(''),
		{format("~W", [a+'B',[quoted(true)]])},
		^^text_output_assertion('a+\'B\'', Assertion).

	test(lgt_format_2_write_term_canonical, true(Assertion)) :-
		^^set_text_output(''),
		{format("~W", [a+'B',[quoted(true),ignore_ops(true)]])},
		^^text_output_assertion('+(a,\'B\')', Assertion).

	% errors

	test(lgt_format_2_unbound_first_argument, error(instantiation_error)) :-
		^^set_text_output(''),
		{format(_, [42])}.

	test(lgt_format_2_unbound_second_argument, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~d", _)}.

	test(lgt_format_2_partial_list_second_argument_1, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~d", [42| _])}.

	test(lgt_format_2_partial_list_second_argument_2, error(instantiation_error)) :-
		^^set_text_output(''),
		{format("~d ~d", [42| _])}.

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

	test(lgt_format_2_too_many_arguments_1, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("abc", [def])}.

	test(lgt_format_2_too_many_arguments_2, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(''),
		{format("~a ~d", [abc, 42, def])}.

	cleanup :-
		^^clean_text_output.
