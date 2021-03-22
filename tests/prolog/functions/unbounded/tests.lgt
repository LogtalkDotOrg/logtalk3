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
		version is 0:8:0,
		author is 'Paulo Moura',
		date is 2021-03-22,
		comment is 'Unit tests for unbounded integer arithmetic.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	% tests from the Logtalk portability work

	% (+)/1

	test(lgt_unbounded_unary_plus_01, true(N == 123456789012345678901234567890)) :-
		N is +(123456789012345678901234567890).

	% (-)/1

	test(lgt_unbounded_unary_minus_01, true(N == -123456789012345678901234567890)) :-
		N is -(123456789012345678901234567890).

	% (+)/2

	test(lgt_unbounded_addition_01, true(N == 123456789012345678901234567891)) :-
		N is 123456789012345678901234567890 + 1.

	test(lgt_unbounded_addition_02, true(N == 123456789012345678901234567891)) :-
		N is 1 + 123456789012345678901234567890.

	test(lgt_unbounded_addition_03, true(N == 246913578024691357802469135780)) :-
		N is 123456789012345678901234567890 + 123456789012345678901234567890.

	test(lgt_unbounded_addition_04, true(N =~= 1.3420123456789013e+34)) :-
		N is 123456789012345678901234567890 + 13.42e+33.

	% (-)/2

	test(lgt_unbounded_subtraction_01, true(N == 123456789012345678901234567890)) :-
		N is 123456789012345678901234567891 - 1.

	test(lgt_unbounded_subtraction_02, true(N == -123456789012345678901234567890)) :-
		N is 1 - 123456789012345678901234567891.

	test(lgt_unbounded_subtraction_03, true(N == 123456789012345678901234567890)) :-
		N is 246913578024691357802469135780 - 123456789012345678901234567890.

	test(lgt_unbounded_subtraction_04, true(N == -123456789012345678901234567890)) :-
		N is 123456789012345678901234567890 - 246913578024691357802469135780.

	test(lgt_unbounded_subtraction_05, true(N =~= -1.3419876543210988e+34)) :-
		N is 123456789012345678901234567890 - 13.42e+33.

	% (*)/2

	test(lgt_unbounded_multiplication_01, true(N == 370370367037037036703703703670)) :-
		N is 123456789012345678901234567890 * 3.

	test(lgt_unbounded_multiplication_02, true(N == 370370367037037036703703703670)) :-
		N is 3 * 123456789012345678901234567890.

	test(lgt_unbounded_multiplication_03, true(N == 15241578753238836750495351562536198787501905199875019052100)) :-
		N is 123456789012345678901234567890 * 123456789012345678901234567890.

	test(lgt_unbounded_multiplication_04, true(N =~= 1.656790108545679e+63)) :-
		N is 123456789012345678901234567890 * 13.42e+33.

	% (//)/2

	test(lgt_unbounded_integer_division_01, true(N == 41152263004115226300411522630)) :-
		N is 123456789012345678901234567890 // 3.

	test(lgt_unbounded_integer_division_02, true(N == 0)) :-
		N is 3 // 123456789012345678901234567890.

	test(lgt_unbounded_integer_division_03, true(N == 41152263004115226300411522630)) :-
		N is 15241578753238836750495351562536198787501905199875019052100 // 370370367037037036703703703670.

	% (/)/2

	test(lgt_unbounded_float_division_01, true(N =~= 4.115226300411523e+28)) :-
		N is 123456789012345678901234567890 / 3.

	test(lgt_unbounded_float_division_02, true(N =~= 2.4300000218700003e-29)) :-
		N is 3 / 123456789012345678901234567890.

	test(lgt_unbounded_float_division_03, true(N =~= 3.0)) :-
		N is 370370367037037036703703703670 / 123456789012345678901234567890.

	test(lgt_unbounded_float_division_04, true(N =~= 2.7598388005740465e-05)) :-
		N is 370370367037037036703703703670 / 13.42e+33.

	% (div)/2

	test(lgt_unbounded_div_01, true(N == 17636684144620811271604938270)) :-
		N is 123456789012345678901234567890 div 7.

	test(lgt_unbounded_div_02, true(N == 0)) :-
		N is 7 div 123456789012345678901234567890.

	test(lgt_unbounded_div_03, true(N == 1234567890123456788901234657800000)) :-
		N is 15241578753238836750495351562536198787501905199875019052100 div 12345678901234567891234567.

	% (rem)/2

	test(lgt_unbounded_rem_01, true(N == 117)) :-
		N is 123456789012345678901234567890 rem 123.

	test(lgt_unbounded_rem_02, true(N == 123)) :-
		N is 123 rem 123456789012345678901234567890.

	test(lgt_unbounded_rem_03, true(N == 122443787781019052100)) :-
		N is 15241578753238836750495351562536198787501905199875019052100 rem 1234567890123456789123.

	% (mod)/2

	test(lgt_unbounded_mod_01, true(N == 0)) :-
		N is 123456789012345678901234567890 mod 3.

	test(lgt_unbounded_mod_02, true(N == 3)) :-
		N is 3 mod 123456789012345678901234567890.

	test(lgt_unbounded_mod_03, true(N == 122443787781019052100)) :-
		N is 15241578753238836750495351562536198787501905199875019052100 mod 1234567890123456789123.

	% gcd/2
	%
	% the tests use a variable for the function call term to avoid compilation errors
	% with backends that don't provide the de facto standard gcd/2 arithmetic function

	test(lgt_unbounded_gcd_01, true(N == 42), [condition((Function = gcd(2,1), catch(_ is Function, _, fail)))]) :-
		GCD = gcd(987654321098765432109876543210, 42),
		N is GCD.

	test(lgt_unbounded_gcd_02, true(N == 42), [condition((Function = gcd(2,1), catch(_ is Function, _, fail)))]) :-
		GCD = gcd(42, 987654321098765432109876543210),
		N is GCD.

	test(lgt_unbounded_gcd_03, true(N == 9000000000900000000090), [condition((Function = gcd(2,1), catch(_ is Function, _, fail)))]) :-
		GCD = gcd(987654321098765432109876543210, 123456789012345678901234567890),
		N is GCD.

	% (^)/2

	test(lgt_unbounded_power_01, true(N == 5846006549323611672814739330865132078623730171904)) :-
		N is 4 ^ 3 ^ 4.

	test(lgt_unbounded_power_02, true(N == 1881676372353657772546715999894626455109783106026821047606410765129148590562263)) :-
		N is 123456789012345678901234567 ^ 3.

	% (<<)/2

	test(lgt_unbounded_left_shift_01, true(N == 2071261215926550121592655012157194240)) :-
		N is 123456789012345678901234567890 << 24.

	% (>>)/2

	test(lgt_unbounded_right_shift_01, true(N == 123456789012345678901234567890)) :-
		N is 2071261215926550121592655012157194240 >> 24.

	% (/\)/2

	test(lgt_unbounded_and_01, true(N == 16)) :-
		N is 123456789012345678901234567890 /\ 24.

	test(lgt_unbounded_and_02, true(N == 16)) :-
		N is 24 /\ 123456789012345678901234567890.

	test(lgt_unbounded_and_03, true(N == 83599684372040429760890210448)) :-
		N is 123456789012345678901234567890 /\ 98765432109876543210987654321.

	% (\/)/2

	test(lgt_unbounded_or_01, true(N == 123456789012345678901234567898)) :-
		N is 123456789012345678901234567890 \/ 24.

	test(lgt_unbounded_or_02, true(N == 123456789012345678901234567898)) :-
		N is 24 \/ 123456789012345678901234567890.

	test(lgt_unbounded_or_03, true(N == 138622536750181792351332011763)) :-
		N is 123456789012345678901234567890 \/ 98765432109876543210987654321.

	% xor/2

	test(lgt_unbounded_xor_01, true(N == 123456789012345678901234567882)) :-
		N is xor(123456789012345678901234567890, 24).

	test(lgt_unbounded_xor_02, true(N == 123456789012345678901234567882)) :-
		N is xor(24, 123456789012345678901234567890).

	test(lgt_unbounded_xor_03, true(N == 55022852378141362590441801315)) :-
		N is xor(123456789012345678901234567890, 98765432109876543210987654321).

	% min/2

	test(lgt_unbounded_min_01, true(N == 24)) :-
		N is min(123456789012345678901234567890, 24).

	test(lgt_unbounded_min_02, true(N == 24)) :-
		N is min(24, 123456789012345678901234567890).

	test(lgt_unbounded_min_03, true(N == 123456789012345678901234567890)) :-
		N is min(123456789012345678901234567890, 987654321098765432109876543210).

	% max/2

	test(lgt_unbounded_max_01, true(N == 123456789012345678901234567890)) :-
		N is max(123456789012345678901234567890, 24).

	test(lgt_unbounded_max_02, true(N == 123456789012345678901234567890)) :-
		N is max(24, 123456789012345678901234567890).

	test(lgt_unbounded_max_03, true(N == 987654321098765432109876543210)) :-
		N is max(123456789012345678901234567890, 987654321098765432109876543210).

	% float/1

	test(lgt_unbounded_float_01, true(N =~= 1.2345678901234568e+29)) :-
		N is float(123456789012345678901234567890).

	test(lgt_unbounded_float_02, true(N =~= 1.881676372353658e+78)) :-
		N is float(1881676372353657772546715999894626455109783106026821047606410765129148590562263).

	test(lgt_unbounded_float_03, error(evaluation_error(float_overflow))) :-
		_ is float(7^7^7).

	% exp/1

	test(lgt_unbounded_exp_01, error(evaluation_error(float_overflow))) :-
		_ is exp(7^7^7).

	% log/1

	test(lgt_unbounded_log_01, error(evaluation_error(float_overflow))) :-
		_ is log(7^7^7).

	% sqrt/1

	test(lgt_unbounded_sqrt_01, error(evaluation_error(float_overflow))) :-
		_ is sqrt(7^7^7).

	% sign/1

	test(lgt_unbounded_sign_01, true(N == 1)) :-
		N is sign(123456789012345678901234567890).

	test(lgt_unbounded_sign_02, true(N == -1)) :-
		N is sign(-123456789012345678901234567890).

	% abs/1

	test(lgt_unbounded_abs_01, true(N == 123456789012345678901234567890)) :-
		N is abs(123456789012345678901234567890).

	test(lgt_unbounded_abs_02, true(N == 123456789012345678901234567890)) :-
		N is abs(-123456789012345678901234567890).

	% succ/2

	test(lgt_unbounded_succ_01, true(N == 123456789012345678901234567891), [condition(predicate_property(succ(_,_), built_in))]) :-
		{succ(123456789012345678901234567890, N)}.

	test(lgt_unbounded_succ_02, true(N == 123456789012345678901234567890), [condition(predicate_property(succ(_,_), built_in))]) :-
		{succ(N, 123456789012345678901234567891)}.

	% plus/3

	test(lgt_unbounded_plus_01, true(N == 1111111110111111111011111111100), [condition(predicate_property(plus(_,_,_), built_in))]) :-
		{plus(123456789012345678901234567890, 987654321098765432109876543210, N)}.

	test(lgt_unbounded_plus_02, true(N == 987654321098765432109876543210), [condition(predicate_property(plus(_,_,_), built_in))]) :-
		{plus(123456789012345678901234567890, N, 1111111110111111111011111111100)}.

	test(lgt_unbounded_plus_03, true(N == 123456789012345678901234567890), [condition(predicate_property(plus(_,_,_), built_in))]) :-
		{plus(N, 987654321098765432109876543210, 1111111110111111111011111111100)}.

	% (=<)/2

	test(lgt_unbounded_less_or_equal_01, true) :-
		value(Value),
		-1844674407370909797907654848955145546336677616 =< Value.

	test(lgt_unbounded_less_or_equal_02, false) :-
		value(Value),
		1844674407370909797907654848955145546336677616 =< Value.

	test(lgt_unbounded_less_or_equal_03, false) :-
		value(Value),
		 Value =< -1844674407370909797907654848955145546336677616.

	test(lgt_unbounded_less_or_equal_04, true) :-
		value(Value),
		Value =< 1844674407370909797907654848955145546336677616.

	% (<)/2

	test(lgt_unbounded_less_01, true) :-
		value(Value),
		-1844674407370909797907654848955145546336677616 < Value.

	test(lgt_unbounded_less_02, false) :-
		value(Value),
		1844674407370909797907654848955145546336677616 < Value.

	test(lgt_unbounded_less_03, false) :-
		value(Value),
		Value < -1844674407370909797907654848955145546336677616.

	test(lgt_unbounded_less_04, true) :-
		value(Value),
		Value < 1844674407370909797907654848955145546336677616.

	% (>=)/2

	test(lgt_unbounded_greater_or_equal_01, false) :-
		value(Value),
		-1844674407370909797907654848955145546336677616 >= Value.

	test(lgt_unbounded_greater_or_equal_02, true) :-
		value(Value),
		1844674407370909797907654848955145546336677616 >= Value.

	test(lgt_unbounded_greater_or_equal_03, true) :-
		value(Value),
		 Value >= -1844674407370909797907654848955145546336677616.

	test(lgt_unbounded_greater_or_equal_04, false) :-
		value(Value),
		Value >= 1844674407370909797907654848955145546336677616.

	% (>)/2

	test(lgt_unbounded_greater_01, false) :-
		value(Value),
		-1844674407370909797907654848955145546336677616 > Value.

	test(lgt_unbounded_greater_02, true) :-
		value(Value),
		1844674407370909797907654848955145546336677616 > Value.

	test(lgt_unbounded_greater_03, true) :-
		value(Value),
		Value > -1844674407370909797907654848955145546336677616.

	test(lgt_unbounded_greater_04, false) :-
		value(Value),
		Value > 1844674407370909797907654848955145546336677616.

	% (=:=)/2

	test(lgt_unbounded_equal_01, false) :-
		value(Value),
		-1844674407370909797907654848955145546336677616 =:= Value.

	test(lgt_unbounded_equal_02, false) :-
		value(Value),
		Value =:= -1844674407370909797907654848955145546336677616.

	% (=\=)/2

	test(lgt_unbounded_not_equal_01, true) :-
		value(Value),
		1844674407370909797907654848955145546336677616 =\= Value.

	test(lgt_unbounded_not_equal_02, true) :-
		value(Value),
		Value =\= 1844674407370909797907654848955145546336677616.

	% number_chars/2

	test(lgt_unbounded_number_chars_01, true(Chars == ['1','8','4','4','6','7','4','4','0','7','3','7','0','9','0','9','7','9','7','9','0','7','6','5','4','8','4','8','9','5','5','1','4','5','5','4','6','3','3','6','6','7','7','6','1','6'])) :-
		number_chars(1844674407370909797907654848955145546336677616, Chars).

	test(lgt_unbounded_number_chars_02, true(N == 1844674407370909797907654848955145546336677616)) :-
		number_chars(N, ['1','8','4','4','6','7','4','4','0','7','3','7','0','9','0','9','7','9','7','9','0','7','6','5','4','8','4','8','9','5','5','1','4','5','5','4','6','3','3','6','6','7','7','6','1','6']).

	% number_codes/2

	test(lgt_unbounded_number_codes_01, true(Codes == [49,56,52,52,54,55,52,52,48,55,51,55,48,57,48,57,55,57,55,57,48,55,54,53,52,56,52,56,57,53,53,49,52,53,53,52,54,51,51,54,54,55,55,54,49,54])) :-
		number_codes(1844674407370909797907654848955145546336677616, Codes).

	test(lgt_unbounded_number_codes_02, true(N == 1844674407370909797907654848955145546336677616)) :-
		number_codes(N, [49,56,52,52,54,55,52,52,48,55,51,55,48,57,48,57,55,57,55,57,48,55,54,53,52,56,52,56,57,53,53,49,52,53,53,52,54,51,51,54,54,55,55,54,49,54]).

	% auxiliary predicates for delaying tests to runtime

	value(42).

:- end_object.
