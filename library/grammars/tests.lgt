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


:- object(tests(_Format_),
	extends(lgtunit)).

	:- info([
		version is 0:3:0,
		author is 'Paulo Moura',
		date is 2022-02-14,
		comment is 'Unit tests for the "grammars" library.',
		parnames is ['Format']
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	cover(blank_grammars(_)).
	cover(number_grammars(_)).

	% blank_grammars tests

	test(white_space, deterministic(Output == 'foo')) :-
		convert(' foo', Input),
		phrase(blank_grammars(_Format_)::white_space, Input, Rest),
		convert(Output, Rest).

	test(white_spaces, deterministic(Output == 'foo')) :-
		convert('    foo', Input),
		phrase(blank_grammars(_Format_)::white_spaces, Input, Rest),
		convert(Output, Rest).

	test(space, deterministic(Output == '\tfoo')) :-
		convert(' \tfoo', Input),
		phrase(blank_grammars(_Format_)::space, Input, Rest),
		convert(Output, Rest).

	test(spaces, deterministic(Output == '\tfoo')) :-
		convert('    \tfoo', Input),
		phrase(blank_grammars(_Format_)::spaces, Input, Rest),
		convert(Output, Rest).

	test(tab, deterministic(Output == ' foo')) :-
		convert('\t foo', Input),
		phrase(blank_grammars(_Format_)::tab, Input, Rest),
		convert(Output, Rest).

	test(tabs, deterministic(Output == ' foo')) :-
		convert('\t\t foo', Input),
		phrase(blank_grammars(_Format_)::tabs, Input, Rest),
		convert(Output, Rest).

	test(new_line, deterministic(Output == '\tfoo')) :-
		convert('\n\tfoo', Input),
		phrase(blank_grammars(_Format_)::new_line, Input, Rest),
		convert(Output, Rest).

	test(new_lines, deterministic(Output == '\tfoo')) :-
		convert('\n\n\tfoo', Input),
		phrase(blank_grammars(_Format_)::new_lines, Input, Rest),
		convert(Output, Rest).

	test(blank, deterministic(Output == '\t foo')) :-
		convert('\n\t foo', Input),
		phrase(blank_grammars(_Format_)::blank, Input, Rest),
		convert(Output, Rest).

	test(blanks, deterministic(Output == 'foo')) :-
		convert('\n\n\t foo', Input),
		phrase(blank_grammars(_Format_)::blanks, Input, Rest),
		convert(Output, Rest).

	test(control, deterministic(Output == '\v foo')) :-
		convert('\a\v foo', Input),
		phrase(blank_grammars(_Format_)::control, Input, Rest),
		convert(Output, Rest).

	test(controls, deterministic(Output == ' foo')) :-
		convert('\a\v foo', Input),
		phrase(blank_grammars(_Format_)::controls, Input, Rest),
		convert(Output, Rest).

	% number_grammars tests

	test(bit, deterministic(Bit-Output == 1-'foo')) :-
		convert('1foo', Input),
		phrase(number_grammars(_Format_)::bit(Bit), Input, Rest),
		convert(Output, Rest).

	test(bits, deterministic(Bits-Output == [1,0,1,1,0,1,0,1]-'foo')) :-
		convert('10110101foo', Input),
		phrase(number_grammars(_Format_)::bits(Bits), Input, Rest),
		convert(Output, Rest).

	test(digit, deterministic(Digit-Output == '9'-'foo')) :-
		convert('9foo', Input),
		phrase(number_grammars(_Format_)::digit(Digit0), Input, Rest),
		convert(Digit, [Digit0]),
		convert(Output, Rest).

	test(digits, deterministic(Digits-Output == '9876501234'-'foo')) :-
		convert('9876501234foo', Input),
		phrase(number_grammars(_Format_)::digits(Digits0), Input, Rest),
		convert(Digits, Digits0),
		convert(Output, Rest).

	test(hex_digit, deterministic(Digit-Output == 'b'-'zoo')) :-
		convert('bzoo', Input),
		phrase(number_grammars(_Format_)::hex_digit(Digit0), Input, Rest),
		convert(Digit, [Digit0]),
		convert(Output, Rest).

	test(hex_digits, deterministic(Digits-Output == 'ace98765BDF01234'-'zoo')) :-
		convert('ace98765BDF01234zoo', Input),
		phrase(number_grammars(_Format_)::hex_digits(Digits0), Input, Rest),
		convert(Digits, Digits0),
		convert(Output, Rest).

	test(natural, deterministic(Natural-Output == 1234-'foo')) :-
		convert('1234foo', Input),
		phrase(number_grammars(_Format_)::natural(Natural), Input, Rest),
		convert(Output, Rest).

	test(integer, deterministic(Integer-Output == 1234-'foo')) :-
		convert('1234foo', Input),
		phrase(number_grammars(_Format_)::integer(Integer), Input, Rest),
		convert(Output, Rest).

	test(integer_sign_positive, deterministic(Integer-Output == 1234-'foo')) :-
		convert('+1234foo', Input),
		phrase(number_grammars(_Format_)::integer(Integer), Input, Rest),
		convert(Output, Rest).

	test(integer_sign_negative, deterministic(Integer-Output == -1234-'foo')) :-
		convert('-1234foo', Input),
		phrase(number_grammars(_Format_)::integer(Integer), Input, Rest),
		convert(Output, Rest).

	test(non_float, false) :-
		convert('42foo', Input),
		phrase(number_grammars(_Format_)::float(_), Input, _).

	test(float, deterministic(Float-Output == 12.34-'foo')) :-
		convert('12.34foo', Input),
		phrase(number_grammars(_Format_)::float(Float), Input, Rest),
		convert(Output, Rest).

	test(float_sign_positive, deterministic(Float-Output == 12.34-'foo')) :-
		convert('+12.34foo', Input),
		phrase(number_grammars(_Format_)::float(Float), Input, Rest),
		convert(Output, Rest).

	test(float_sign_negative, deterministic(Float-Output == -12.34-'foo')) :-
		convert('-12.34foo', Input),
		phrase(number_grammars(_Format_)::float(Float), Input, Rest),
		convert(Output, Rest).

	test(float_radix, deterministic((Float =~= 12.34e+7, Output == 'foo'))) :-
		convert('12.34e7foo', Input),
		phrase(number_grammars(_Format_)::float(Float), Input, Rest),
		convert(Output, Rest).

	test(float_radix_sign_positive, deterministic((Float =~= 12.34e-7, Output == 'foo'))) :-
		convert('12.34e-7foo', Input),
		phrase(number_grammars(_Format_)::float(Float), Input, Rest),
		convert(Output, Rest).

	test(float_radix_sign_negative, deterministic((Float =~= 12.34e+7, Output == 'foo'))) :-
		convert('12.34e+7foo', Input),
		phrase(number_grammars(_Format_)::float(Float), Input, Rest),
		convert(Output, Rest).

	test(float_integer_radix, deterministic((Float =~= 42.0e+7, Output == 'foo'))) :-
		convert('42e7foo', Input),
		phrase(number_grammars(_Format_)::float(Float), Input, Rest),
		convert(Output, Rest).

	test(number_integer, deterministic(Float-Output == 42-'foo')) :-
		convert('42foo', Input),
		phrase(number_grammars(_Format_)::number(Float), Input, Rest),
		convert(Output, Rest).

	test(number_float, deterministic(Float-Output == 12.34-'foo')) :-
		convert('12.34foo', Input),
		phrase(number_grammars(_Format_)::number(Float), Input, Rest),
		convert(Output, Rest).

	test(number_sign_positive, deterministic(Float-Output == 12.34-'foo')) :-
		convert('+12.34foo', Input),
		phrase(number_grammars(_Format_)::number(Float), Input, Rest),
		convert(Output, Rest).

	test(number_sign_negative, deterministic(Float-Output == -12.34-'foo')) :-
		convert('-12.34foo', Input),
		phrase(number_grammars(_Format_)::number(Float), Input, Rest),
		convert(Output, Rest).

	test(number_integer_radix, deterministic((Float =~= 42.0e+7, Output == 'foo'))) :-
		convert('42e7foo', Input),
		phrase(number_grammars(_Format_)::number(Float), Input, Rest),
		convert(Output, Rest).

	test(number_float_radix, deterministic((Float =~= 12.34e+7, Output == 'foo'))) :-
		convert('12.34e7foo', Input),
		phrase(number_grammars(_Format_)::number(Float), Input, Rest),
		convert(Output, Rest).

	test(number_radix_sign_positive, deterministic((Float =~= 12.34e-7, Output == 'foo'))) :-
		convert('12.34e-7foo', Input),
		phrase(number_grammars(_Format_)::number(Float), Input, Rest),
		convert(Output, Rest).

	test(number_radix_sign_negative, deterministic((Float =~= 12.34e+7, Output == 'foo'))) :-
		convert('12.34e+7foo', Input),
		phrase(number_grammars(_Format_)::number(Float), Input, Rest),
		convert(Output, Rest).

	% auxiliary predicates

	convert(Atom, List) :-
		convert(_Format_, Atom, List).

	convert(chars, Atom, Chars) :-
		atom_chars(Atom, Chars).
	convert(codes, Atom, Codes) :-
		atom_codes(Atom, Codes).

:- end_object.
