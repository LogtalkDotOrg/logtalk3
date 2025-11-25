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


	test(lgt_format_3_empty_control_sequence, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "", [])},
		^^text_output_assertion(out, '', Assertion).

	test(lgt_format_3_empty_arguments, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "abc", [])},
		^^text_output_assertion(out, 'abc', Assertion).

	% ~~ control sequence

	test(lgt_format_3_tilde, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~~", [])},
		^^text_output_assertion(out, '~', Assertion).

	test(lgt_format_3_tilde_ignore_count, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~3~", [])},
		^^text_output_assertion(out, '~', Assertion).

	% ~w control sequence

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

	% ~q control sequence

	test(lgt_format_3_quoted, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~q", ['ABC'])},
		^^text_output_assertion(out, '\'ABC\'', Assertion).

	test(lgt_format_3_quoted_variable, subsumes(['_', _| _], Chars)) :-
		^^set_text_output(out, ''),
		A = A,	% avoid singleton warnings
		{format(out, "~q", [A])},
		^^text_output_contents(out, Chars).

	% ~k control sequence

	test(lgt_format_3_canonical_operators, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~k", [(:-a)])},
		^^text_output_assertion(out, ':-(a)', Assertion).

	test(lgt_format_3_canonical_quotes, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~k", ['A'+'B'])},
		^^text_output_assertion(out, '+(\'A\',\'B\')', Assertion).

	% ~p control sequence

	test(lgt_format_3_print_default, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~p", [42])},
		^^text_output_assertion(out, '42', Assertion).

	test(lgt_format_3_print_portray_01, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~p", [3.14])},
		^^text_output_assertion(out, '3', Assertion).

	test(lgt_format_3_print_portray_02, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~p", [foo])},
		^^text_output_assertion(out, foofoo, Assertion).

	test(lgt_format_3_print_portray_03, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~p", [a(foo)])},
		^^text_output_assertion(out, 'a(foofoo)', Assertion).

	test(lgt_format_3_print_portray_04, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~p", [a(foo,b(c(foo,3.14)))])},
		^^text_output_assertion(out, 'a(foofoo,b(c(foofoo,3)))', Assertion).

	% ~a control sequence

	test(lgt_format_3_atom, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~a", [abc])},
		^^text_output_assertion(out, abc, Assertion).

	test(lgt_format_3_atom_no_quoting, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~a", ['ABC'])},
		^^text_output_assertion(out, 'ABC', Assertion).

	test(lgt_format_3_atom_empty_list, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~a", [[]])},
		^^text_output_assertion(out, '[]', Assertion).

	test(lgt_format_3_atom_empty_curly, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~a", [{}])},
		^^text_output_assertion(out, '{}', Assertion).

	test(lgt_format_3_atom_ignore_fixed_count, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~3a", [abc])},
		^^text_output_assertion(out, abc, Assertion).

	test(lgt_format_3_atom_ignore_star_count, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*a", [3,abc])},
		^^text_output_assertion(out, abc, Assertion).

	test(lgt_format_3_atom_ignore_expression_count, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*a", [1+2,abc])},
		^^text_output_assertion(out, abc, Assertion).

	test(lgt_format_3_atom_invalid_01, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~a", [_])}.

	test(lgt_format_3_atom_invalid_02, error(type_error(atom,[0'a,0'b,0'c]))) :-
		^^set_text_output(out, ''),
		{format(out, "~a", [[0'a,0'b,0'c]])}.

	test(lgt_format_3_atom_invalid_03, error(type_error(atom,[a,b,c]))) :-
		^^set_text_output(out, ''),
		{format(out, "~a", [[a,b,c]])}.

	test(lgt_format_3_atom_invalid_04, error(type_error(atom,42))) :-
		^^set_text_output(out, ''),
		{format(out, "~a", [42])}.

	test(lgt_format_3_atom_invalid_05, error(type_error(atom,foo(bar)))) :-
		^^set_text_output(out, ''),
		{format(out, "~a", [foo(bar)])}.

	% ~c control sequence

	test(lgt_format_3_code, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~c", [0'A])},
		^^text_output_assertion(out, 'A', Assertion).

	test(lgt_format_3_code_n, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~8c", [0'A])},
		^^text_output_assertion(out, 'AAAAAAAA', Assertion).

	test(lgt_format_3_code_star, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*c", [8,0'A])},
		^^text_output_assertion(out, 'AAAAAAAA', Assertion).

	test(lgt_format_3_code_expression, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*c", [3+5,0'A])},
		^^text_output_assertion(out, 'AAAAAAAA', Assertion).

	test(lgt_format_3_code_invalid_01, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~c", [_])}.

	test(lgt_format_3_code_invalid_02, error(type_error(evaluable,a/0))) :-
		^^set_text_output(out, ''),
		{format(out, "~c", [a])}.

	% ~s control sequence

	test(lgt_format_3_string_codes, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~s", [[65,66,67]])},
		^^text_output_assertion(out, 'ABC', Assertion).

	test(lgt_format_3_string_chars, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~s", [['A','B','C']])},
		^^text_output_assertion(out, 'ABC', Assertion).

	test(lgt_format_3_string_atom, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~s", ['ABC'])},
		^^text_output_assertion(out, 'ABC', Assertion).

	test(lgt_format_3_string_first_n, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~3s", [[65,66,67,68,69,70,71,72,73]])},
		^^text_output_assertion(out, 'ABC', Assertion).

	test(lgt_format_3_string_first_star, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*s", [3,[65,66,67,68,69,70,71,72,73]])},
		^^text_output_assertion(out, 'ABC', Assertion).

	test(lgt_format_3_string_zero, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~0s", [[65,66,67,68,69,70,71,72,73]])},
		^^text_output_assertion(out, '', Assertion).

	test(lgt_format_3_string_pad_with_spaces, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~6s", [[65,66,67]])},
		^^text_output_assertion(out, 'ABC   ', Assertion).

	test(lgt_format_3_string_invalid_01, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~s", [_])}.

	test(lgt_format_3_string_invalid_02, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~s", [65,66|_])}.

	test(lgt_format_3_string_invalid_03, error(type_error(_,42))) :-
		^^set_text_output(out, ''),
		{format(out, "~s", [42])}.

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

	test(lgt_format_3_new_line_star, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*n", [4])},
		^^text_output_assertion(out, '\n\n\n\n', Assertion).

	test(lgt_format_3_new_line_if_not_beginning_of_line, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~Nbegin~N~Nend", [])},
		^^text_output_assertion(out, 'begin\nend', Assertion).

	:- endif.

	% ~i control sequence

	test(lgt_format_3_ignore, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~a~i~a", [a,b,c])},
		^^text_output_assertion(out, ac, Assertion).

	test(lgt_format_3_ignore_fixed_count, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~a~3i~a", [a,b,c])},
		^^text_output_assertion(out, ac, Assertion).

	test(lgt_format_3_ignore_star_count, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~a~*i~a", [a,3,b,c])},
		^^text_output_assertion(out, ac, Assertion).

	test(lgt_format_3_ignore_expresion_count, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~a~*i~a", [a,1+2,b,c])},
		^^text_output_assertion(out, ac, Assertion).

	% ~d and ~D control sequences

	test(lgt_format_3_decimal, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~d", [123])},
		^^text_output_assertion(out, '123', Assertion).

	test(lgt_format_3_decimal_n, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~1d", [123])},
		^^text_output_assertion(out, '12.3', Assertion).

	test(lgt_format_3_decimal_star, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*d", [1,123])},
		^^text_output_assertion(out, '12.3', Assertion).

	test(lgt_format_3_decimal_group, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~D", [1234567890])},
		^^text_output_assertion(out, '1,234,567,890', Assertion).

	test(lgt_format_3_decimal_group_n, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~2D", [1234567890])},
		^^text_output_assertion(out, '12,345,678.90', Assertion).

	test(lgt_format_3_decimal_group_star, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*D", [2,1234567890])},
		^^text_output_assertion(out, '12,345,678.90', Assertion).

	test(lgt_format_3_decimal_invalid_01, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~d", [_])}.

	test(lgt_format_3_decimal_invalid_02, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~D", [_])}.

	test(lgt_format_3_decimal_invalid_07, error(type_error(integer, 123.0))) :-
		^^set_text_output(out, ''),
		{format(out, "~d", [123.0])}.

	test(lgt_format_3_decimal_invalid_08, error(type_error(integer, 123.0))) :-
		^^set_text_output(out, ''),
		{format(out, "~D", [123.0])}.

	test(lgt_format_3_decimal_invalid_09, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(out, ''),
		{format(out, "~d", [foo(bar)])}.

	test(lgt_format_3_decimal_invalid_10, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(out, ''),
		{format(out, "~D", [foo(bar)])}.

	test(lgt_format_3_decimal_invalid_11, error(domain_error(_,_))) :-
		^^set_text_output(out, ''),
		{format(out, "~*d", [-1,123])}.

	test(lgt_format_3_decimal_invalid_12, error(domain_error(_,_))) :-
		^^set_text_output(out, ''),
		{format(out, "~*D", [-1,123])}.

	% ~r and ~R control sequences

	test(lgt_format_3_radix_2, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~2r", [127])},
		^^text_output_assertion(out, '1111111', Assertion).

	test(lgt_format_3_radix_8, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~8r", [127])},
		^^text_output_assertion(out, '177', Assertion).

	test(lgt_format_3_radix_10, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~10r", [127])},
		^^text_output_assertion(out, '127', Assertion).

	test(lgt_format_3_radix_16, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~16r", [127])},
		^^text_output_assertion(out, '7f', Assertion).

	test(lgt_format_3_radix_36, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~36r", [127])},
		^^text_output_assertion(out, '3j', Assertion).

	test(lgt_format_3_radix_36_expression, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~36r", [36*36*36-1])},
		^^text_output_assertion(out, 'zzz', Assertion).

	test(lgt_format_3_radix_star, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*r", [16,127])},
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

	test(lgt_format_3_radix_upper_case_36, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~36R", [127])},
		^^text_output_assertion(out, '3J', Assertion).

	test(lgt_format_3_radix_upper_case_36_expression, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~36R", [36*36*36-1])},
		^^text_output_assertion(out, 'ZZZ', Assertion).

	test(lgt_format_3_radix_upper_case_star, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*R", [16,127])},
		^^text_output_assertion(out, '7F', Assertion).

	test(lgt_format_3_radix_default, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~r", [16])},
		^^text_output_assertion(out, '20', Assertion).

	test(lgt_format_3_radix_default_upper_case, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~R", [16])},
		^^text_output_assertion(out, '20', Assertion).

	test(lgt_format_3_radix_invalid_01, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~r", [_])}.

	test(lgt_format_3_radix_invalid_02, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~R", [_])}.

	test(lgt_format_3_radix_invalid_03, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~0r", [16])}.

	test(lgt_format_3_radix_invalid_04, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~1R", [16])}.

	test(lgt_format_3_radix_invalid_05, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~37r", [16])}.

	test(lgt_format_3_radix_invalid_06, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~42R", [16])}.

	test(lgt_format_3_radix_invalid_07, error(type_error(integer, 123.0))) :-
		^^set_text_output(out, ''),
		{format(out, "~r", [123.0])}.

	test(lgt_format_3_radix_invalid_08, error(type_error(integer, 123.0))) :-
		^^set_text_output(out, ''),
		{format(out, "~R", [123.0])}.

	test(lgt_format_3_radix_invalid_09, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(out, ''),
		{format(out, "~r", [foo(bar)])}.

	test(lgt_format_3_radix_invalid_10, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(out, ''),
		{format(out, "~R", [foo(bar)])}.

	test(lgt_format_3_radix_invalid_11, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~*r", [-1,123])}.

	test(lgt_format_3_radix_invalid_12, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~*R", [-1,123])}.

	% ~f control sequence

	test(lgt_format_3_float, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~f", [1.0])},
		% default is six decimal places
		^^text_output_assertion(out, '1.000000', Assertion).

	test(lgt_format_3_float_expression, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~f", [1+1.0])},
		% default is six decimal places
		^^text_output_assertion(out, '2.000000', Assertion).

	test(lgt_format_3_float_n_places_01, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~4f", [-1.0e-1])},
		^^text_output_assertion(out, '-0.1000', Assertion).

	test(lgt_format_3_float_n_places_02, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~3f", [12.3456789])},
		^^text_output_assertion(out, '12.346', Assertion).

	test(lgt_format_3_float_n_places_03, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~12f", [12.3456789])},
		^^text_output_assertion(out, '12.345678900000', Assertion).

	test(lgt_format_3_float_star_places, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*f", [4,-1.0e-1])},
		^^text_output_assertion(out, '-0.1000', Assertion).

	test(lgt_format_3_float_zero_places, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~0f", [123.456])},
		^^text_output_assertion(out, '123', Assertion).

	test(lgt_format_3_float_invalid_01, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~f", [_])}.

	test(lgt_format_3_float_invalid_02, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(out, ''),
		{format(out, "~f", [foo(bar)])}.

	% ~e and ~E control sequences

	test(lgt_format_3_float_exponential_notation, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~e", [1.333333])},
		% default is six decimal places
		^^text_output_assertion(out, '1.333333e+00', Assertion).

	test(lgt_format_3_float_exponential_notation_expression, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~e", [1.333333+1])},
		% default is six decimal places
		^^text_output_assertion(out, '2.333333e+00', Assertion).

	test(lgt_format_3_float_exponential_notation_n_places, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~3e", [1.333333])},
		^^text_output_assertion(out, '1.333e+00', Assertion).

	test(lgt_format_3_float_exponential_notation_star_places, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*e", [3,1.333333])},
		^^text_output_assertion(out, '1.333e+00', Assertion).

	test(lgt_format_3_float_exponential_notation_zero_places, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~0e", [1.333333])},
		^^text_output_assertion(out, '1e+00', Assertion).

	test(lgt_format_3_float_exponential_notation_invalid_01, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~e", [_])}.

	test(lgt_format_3_float_exponential_notation_invalid_02, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(out, ''),
		{format(out, "~e", [foo(bar)])}.

	test(lgt_format_3_float_exponential_notation_upper_case, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~E", [1.333333])},
		% default is six decimal places
		^^text_output_assertion(out, '1.333333E+00', Assertion).

	test(lgt_format_3_float_exponential_notation_upper_case_expression, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~E", [1.333333+1])},
		% default is six decimal places
		^^text_output_assertion(out, '2.333333E+00', Assertion).

	test(lgt_format_3_float_exponential_notation_n_places_upper_case, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~3E", [1.333333])},
		^^text_output_assertion(out, '1.333E+00', Assertion).

	test(lgt_format_3_float_exponential_notation_upper_case_star_places, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*E", [3,1.333333])},
		^^text_output_assertion(out, '1.333E+00', Assertion).

	test(lgt_format_3_float_exponential_notation_upper_case_zero_places, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~0E", [1.333333])},
		^^text_output_assertion(out, '1E+00', Assertion).

	test(lgt_format_3_float_exponential_notation_upper_case_invalid_01, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~E", [_])}.

	test(lgt_format_3_float_exponential_notation_upper_case_invalid_02, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(out, ''),
		{format(out, "~E", [foo(bar)])}.

	% ~g and ~G control sequences

	test(lgt_format_3_float_best_e, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~g", [0.00000123])},
		^^text_output_assertion(out, '1.23e-06', Assertion).

	test(lgt_format_3_float_best_ne, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~2g", [0.00000123])},
		^^text_output_assertion(out, '1.2e-06', Assertion).

	test(lgt_format_3_float_best_f, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~g", [0.39265e+3])},
		^^text_output_assertion(out, '392.65', Assertion).

	test(lgt_format_3_float_best_nf, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~2g", [0.39265e+3])},
		^^text_output_assertion(out, '3.9e+02', Assertion).

	test(lgt_format_3_float_best_invalid_01, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~g", [_])}.

	test(lgt_format_3_float_best_invalid_02, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(out, ''),
		{format(out, "~g", [foo(bar)])}.

	test(lgt_format_3_float_best_e_upper_case, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~G", [0.00000123])},
		^^text_output_assertion(out, '1.23E-06', Assertion).

	test(lgt_format_3_float_best_ne_upper_case, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~2G", [0.00000123])},
		^^text_output_assertion(out, '1.2E-06', Assertion).

	test(lgt_format_3_float_best_f_upper_case, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~G", [0.39265e+3])},
		^^text_output_assertion(out, '392.65', Assertion).

	test(lgt_format_3_float_best_nf_upper_case, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~2G", [0.39265e+3])},
		^^text_output_assertion(out, '3.9E+02', Assertion).

	test(lgt_format_3_float_best_upper_case_invalid_01, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~G", [_])}.

	test(lgt_format_3_float_best_upper_case_invalid_02, error(type_error(evaluable, foo/1))) :-
		^^set_text_output(out, ''),
		{format(out, "~G", [foo(bar)])}.

	% ~| and ~t control sequences

	test(lgt_format_3_tab_stop_default, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~|~a", [abcd])},
		^^text_output_assertion(out, 'abcd', Assertion).

	test(lgt_format_3_tab_stop_non_default, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~6|~a", [abcd])},
		^^text_output_assertion(out, '      abcd', Assertion).

	test(lgt_format_3_tab_atom_left_aligned_default_column_boundary, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~|~a~t~+", [abcd])},
		^^text_output_assertion(out, 'abcd    ', Assertion).

	test(lgt_format_3_tab_atom_left_aligned, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~|~a~t~8+", [abcd])},
		^^text_output_assertion(out, 'abcd    ', Assertion).

	test(lgt_format_3_tab_integer_left_aligned_default_column_boundary, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~|~d~t~+", [1234])},
		^^text_output_assertion(out, '1234    ', Assertion).

	test(lgt_format_3_tab_integer_left_aligned, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~|~d~t~8+", [1234])},
		^^text_output_assertion(out, '1234    ', Assertion).

	test(lgt_format_3_tab_atom_center_aligned_default_column_boundary, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~t~a~t~+", [abcd])},
		^^text_output_assertion(out, '  abcd  ', Assertion).

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

	test(lgt_format_3_tab_table_pip_0110_01, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "left~tright", [])},
		^^text_output_assertion(out, 'leftright', Assertion).

	test(lgt_format_3_tab_table_pip_0110_02, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "left~tright~|", [])},
		^^text_output_assertion(out, 'leftright', Assertion).

	test(lgt_format_3_tab_table_pip_0110_03, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "left~tright~8|", [])},
		^^text_output_assertion(out, 'leftright', Assertion).

	test(lgt_format_3_tab_table_pip_0110_04, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "left~tright~20|", [])},
		^^text_output_assertion(out, 'left           right', Assertion).

	test(lgt_format_3_tab_table_pip_0110_05, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~|left~tright~20|", [])},
		^^text_output_assertion(out, 'left           right', Assertion).

	test(lgt_format_3_tab_table_pip_0110_06, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "left~tright~20+", [])},
		^^text_output_assertion(out, 'left           right', Assertion).

	test(lgt_format_3_tab_table_pip_0110_07, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "left~10+next", [])},
		^^text_output_assertion(out, 'left      next', Assertion).

	test(lgt_format_3_tab_table_pip_0110_08, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "left~t~10+next", [])},
		^^text_output_assertion(out, 'left      next', Assertion).

	test(lgt_format_3_tab_table_pip_0110_09, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~tright~10+next", [])},
		^^text_output_assertion(out, '     rightnext', Assertion).

	test(lgt_format_3_tab_table_pip_0110_10, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|abc~10+$",[])},
		^^text_output_assertion(out, '^abc       $', Assertion).

	test(lgt_format_3_tab_table_pip_0110_11, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|abc~11|$",[])},
		^^text_output_assertion(out, '^abc       $', Assertion).

	test(lgt_format_3_tab_table_pip_0110_12, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|abc~*+$",[10])},
		^^text_output_assertion(out, '^abc       $', Assertion).

	test(lgt_format_3_tab_table_pip_0110_13, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|abc~*|$",[11])},
		^^text_output_assertion(out, '^abc       $', Assertion).

	test(lgt_format_3_tab_table_pip_0110_14, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|abc~t~10+$",[])},
		^^text_output_assertion(out, '^abc       $', Assertion).

	test(lgt_format_3_tab_table_pip_0110_15, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|~tabc~t~10+$",[])},
		^^text_output_assertion(out, '^   abc    $', Assertion).

	test(lgt_format_3_tab_table_pip_0110_16, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|~t~tabc~t~10+$",[])},
		^^text_output_assertion(out, '^    abc   $', Assertion).

	test(lgt_format_3_tab_table_pip_0110_17, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|~t~t~tabc~t~10+$",[])},
		^^text_output_assertion(out, '^     abc  $', Assertion).

	test(lgt_format_3_tab_table_pip_0110_18, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|~tabc~10+$",[])},
		^^text_output_assertion(out, '^       abc$', Assertion).

	test(lgt_format_3_tab_table_pip_0110_19, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|~ta~tb~tc~t~10+$",[])},
		^^text_output_assertion(out, '^ a  b  c  $', Assertion).

	test(lgt_format_3_tab_table_pip_0110_20, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|a~tb~tc~t~10+$",[])},
		^^text_output_assertion(out, '^a  b  c   $', Assertion).

	test(lgt_format_3_tab_table_pip_0110_21, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|~*tabc~*t~10+$",[0'.,95])},
		^^text_output_assertion(out, '^...abc____$', Assertion).

	test(lgt_format_3_tab_table_pip_0110_22, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "^~|~`.tabc~95t~10+$",[])},
		^^text_output_assertion(out, '^...abc____$', Assertion).

	test(lgt_format_3_tab_table_pip_0110_23, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~*+.~n~3+.~n~*+.~n", [3,3])},
		^^text_output_assertion(out, '   .\n   .\n   .\n', Assertion).

	% ~W control sequence

	test(lgt_format_3_write_term_default, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~W", [a+'B',[]])},
		^^text_output_assertion(out, 'a+B', Assertion).

	test(lgt_format_3_write_term_quoted, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~W", [a+'B',[quoted(true)]])},
		^^text_output_assertion(out, 'a+\'B\'', Assertion).

	test(lgt_format_3_write_term_canonical, true(Assertion)) :-
		^^set_text_output(out, ''),
		{format(out, "~W", [a+'B',[quoted(true),ignore_ops(true)]])},
		^^text_output_assertion(out, '+(a,\'B\')', Assertion).

	% errors

	test(lgt_format_3_unbound_first_argument, error(instantiation_error)) :-
		{format(_, _, [42])}.

	test(lgt_format_3_unbound_second_argument, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, _, [42])}.

	test(lgt_format_3_unbound_third_argument, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~d", _)}.

	test(lgt_format_3_partial_list_second_argument_1, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~d", [42| _])}.

	test(lgt_format_3_partial_list_second_argument_2, error(instantiation_error)) :-
		^^set_text_output(out, ''),
		{format(out, "~d ~d", [42| _])}.

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

	test(lgt_format_3_too_many_arguments_1, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "abc", [def])}.

	test(lgt_format_3_too_many_arguments_2, errors([domain_error(_,_), consistency_error(_,_,_)])) :-
		^^set_text_output(out, ''),
		{format(out, "~a ~d", [abc, 42, def])}.

	cleanup :-
		^^clean_text_output.
