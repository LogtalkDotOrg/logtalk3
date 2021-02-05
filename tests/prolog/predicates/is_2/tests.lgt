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
		version is 2:2:1,
		author is 'Paulo Moura',
		date is 2020-11-05,
		comment is 'Unit tests for the ISO Prolog standard is/2 built-in predicate.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.1.7

	test(iso_is_2_01, true(X == 42)) :-
		{X is '+'(7, 35)}.

	test(iso_is_2_02, true(X == 14)) :-
		{X is '+'(0, 3+11)}.

	test(iso_is_2_03, true(X == 14.2)) :-
		{X is '+'(0, 3.2+11)}.

	% in some of the throws/2 tests that follow, try to delay the expected error to runtime

	test(iso_is_2_04, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '+'(77, N)}.

	test(iso_is_2_05, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '+'(Foo, 77)}.

	test(iso_is_2_06, true(X == -7)) :-
		{X is '-'(7)}.

	test(iso_is_2_07, true(X == 8)) :-
		{X is '-'(3-11)}.

	test(iso_is_2_08, true(X == 7.8)) :-
		{X is '-'(3.2-11)}.

	test(iso_is_2_09, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '-'(N)}.

	test(iso_is_2_10, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '-'(Foo)}.

	test(iso_is_2_11, true(X == -28)) :-
		{X is '-'(7, 35)}.

	test(iso_is_2_12, true(X == 6)) :-
		{X is '-'(20, 3+11)}.

	test(iso_is_2_13, true(X == -14.2)) :-
		{X is '-'(0, 3.2+11)}.

	test(iso_is_2_14, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '-'(77, N)}.

	test(iso_is_2_15, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '-'(Foo, 77)}.

	test(iso_is_2_16, true(X == 245)) :-
		{X is '*'(7,35)}.

	test(iso_is_2_17, true(X == 0)) :-
		{X is '*'(0, 3+11)}.

	test(iso_is_2_18, true(X =~= 21.3)) :-
		{X is '*'(1.5, 3.2+11)}.

	test(iso_is_2_19, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '*'(77, N)}.

	test(iso_is_2_20, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '*'(Foo, 77)}.

	test(iso_is_2_21, true(X == 0)) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		{X is '//'(7,35)}.

	test(iso_is_2_22, true(X == 0.2)) :-
		{X is '/'(7.0,35)}.

	test(iso_is_2_23, true(X == 10)) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		{X is '//'(140,3+11)}.

	test(iso_is_2_24, true(X =~= 1.42)) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		{X is '/'(20.164, 3.2+11)}.

	test(iso_is_2_25, true((X == -2; X == -3))) :-
		{X is '//'(7, -3)}.

	test(iso_is_2_26, true((X == -2; X == -3))) :-
		{X is '//'(-7, 3)}.

	test(iso_is_2_27, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '/'(77, N)}.

	test(iso_is_2_28, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '/'(Foo, 77)}.

	test(iso_is_2_29, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is '/'(77, Foo)}.

	test(iso_is_2_30, error(evaluation_error(zero_divisor))) :-
		% try to delay the expected error to runtime
		{G = (_X is '/'(3, 0)), call(G)}.

	test(iso_is_2_31, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		{current_prolog_flag(max_integer, MI), _X is '+'(MI,1)}.

	test(iso_is_2_32, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		{current_prolog_flag(max_integer, MI), _X is '-'('+'(MI,1),1)}.

	test(iso_is_2_33, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		% ISO allows min_integer = -(max_integer + 1)
		{current_prolog_flag(max_integer, MI), _X is '-'(-2,MI)}.

	test(iso_is_2_34, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		{current_prolog_flag(max_integer, MI), _X is '*'(MI,2)}.

	test(iso_is_2_35, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		{current_prolog_flag(max_integer, MI), R is float(MI)*2, _X is floor(R)}.

	% tests from the Logtalk portability work

	% check behavior when the first argument of is/2 is bound

	test(lgt_is_2_36, true) :-
		{2 is 4 - 2}.

	test(lgt_is_2_37, fail) :-
		{2 is 4 - 1}.

	test(lgt_is_2_38, true) :-
		{2.0 is 4.0 - 2.0}.

	test(lgt_is_2_39, fail) :-
		{2.0 is 4.0 - 1.0}.

	test(lgt_is_2_40, fail) :-
		{foo42 is 4 - 2}.

	test(lgt_is_2_41, fail) :-
		{foo42(_) is 4 - 2}.

	% also check zero divisor error for integer division

	test(lgt_is_2_42, error(evaluation_error(zero_divisor))) :-
		% try to delay the expected error to runtime
		{G = (_X is '//'(3, 0)), call(G)}.

	% also check integer overflow for other functions

	test(lgt_is_2_43, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		{current_prolog_flag(max_integer, MI), R is float(MI)*2, _X is truncate(R)}.

	test(lgt_is_2_44, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		{current_prolog_flag(max_integer, MI), R is float(MI)*2, _X is round(R)}.

	test(lgt_is_2_45, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		{current_prolog_flag(max_integer, MI), R is float(MI)*2, _X is ceiling(R)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).

:- end_object.
