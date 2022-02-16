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
		version is 0:5:0,
		author is 'Paulo Moura',
		date is 2022-02-16,
		comment is 'Unit tests for the "grammars" library.',
		parnames is ['Format']
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	cover(blank_grammars(_)).
	cover(number_grammars(_)).
	cover(ip_grammars(_)).

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

	test(non_blank, deterministic(Atom-Output == f-'oo bar')) :-
		convert('foo bar', Input),
		phrase(blank_grammars(_Format_)::non_blank(NonBlank), Input, Rest),
		convert(Atom, [NonBlank]),
		convert(Output, Rest).

	test(non_blanks, deterministic(Atom-Output == foo-' bar')) :-
		convert('foo bar', Input),
		phrase(blank_grammars(_Format_)::non_blanks(NonBlanks), Input, Rest),
		convert(Atom, NonBlanks),
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

	% ip_grammars tests

	test(ip_ipv4_home, true(List == [127,0,0,1])) :-
		convert('127.0.0.1', Input),
		phrase(ip_grammars(_Format_)::ipv4(List), Input).

	test(ip_ipv4_broadcast, true(List == [255,255,255,255])) :-
		convert('255.255.255.255', Input),
		phrase(ip_grammars(_Format_)::ipv4(List), Input).

	test(ip_ipv6, true(List == [8193,3512,34211,0,0,35374,880,29492])) :-
		convert('2001:0db8:85a3:0000:0000:8a2e:0370:7334', Input),
		phrase(ip_grammars(_Format_)::ipv6(List), Input).

	% auxiliary predicates

	convert(Atom, List) :-
		convert(_Format_, Atom, List).

	convert(chars, Atom, Chars) :-
		atom_chars(Atom, Chars).
	convert(codes, Atom, Codes) :-
		atom_codes(Atom, Codes).

:- end_object.
