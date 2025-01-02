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
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2022-03-30,
		comment is 'Unit tests for the ISO Prolog standard (^)/2 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	% tests from the ISO/IEC 13211-1 Technical Corrigendum 3:2017, section 9.3.10.4

	test(iso_integer_power_2_01, true(X == 1)) :-
		{X is 0^0}.

	test(iso_integer_power_2_02, true(X == 3.0)) :-
		{X is 3^1.0}.

	test(iso_integer_power_2_03, true(X == 27)) :-
		{X is 3^3}.

	test(iso_integer_power_2_04, true(X == 7625597484987)) :-
		{X is 3^27}.

	test(iso_integer_power_2_05, true(X == 7625597484987)) :-
		{X is 3^3^3}.

	test(iso_integer_power_2_06, error(type_error(float,2))) :-
		{_X is 2^(-1)}.

	test(iso_integer_power_2_07, true(X =~= 0.5)) :-
		{X is 2.0^(-1)}.

	test(iso_integer_power_2_08, true(X == 1)) :-
		{X is 1^(-1)}.

	test(iso_integer_power_2_09, true(X =~= 0.353553)) :-
		{X is 2^ -1.5}.

	% tests from the Logtalk portability work

	test(lgt_integer_power_2_10, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is 3^N}.

	test(lgt_integer_power_2_11, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is N^3}.

	test(lgt_integer_power_2_12, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is 3^Foo}.

	test(lgt_integer_power_2_13, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is Foo^3}.

	test(lgt_integer_power_2_14, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is 3^Foo}.

	test(lgt_integer_power_2_15, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is Foo^3}.

	test(lgt_integer_power_2_16, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is 3^Foo}.

	test(lgt_integer_power_2_17, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is Foo^3}.

	test(lgt_integer_power_2_18, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		% try to delay the error to runtime
		current_prolog_flag(max_integer, Max),
		{_X is Max^Max}.

	test(lgt_integer_power_2_19, error(evaluation_error(float_overflow))) :-
		% try to delay the error to runtime
		big_float(Big),
		{_X is Big^1234567}.

	test(lgt_integer_power_2_20, error(evaluation_error(undefined))) :-
		% try to delay the error to runtime
		negative(Negative),
		{_X is Negative ^ 7.5}.

	test(lgt_integer_power_2_21, error(evaluation_error(undefined))) :-
		% try to delay the error to runtime
		zero(Zero),
		{_X is Zero ^ -42}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

	big_float(1.0e+300).

	negative(-42).

	zero(0).

:- end_object.
