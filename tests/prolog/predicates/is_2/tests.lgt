%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.3,
		author is 'Paulo Moura',
		date is 2019-11-08,
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

	test(iso_is_2_29, error(evaluation_error(zero_divisor))) :-
		% try to delay the expected error to runtime
		{G = (_X is '/'(3, 0)), call(G)}.

	test(iso_is_2_30, true(X == 1)) :-
		{X is mod(7, 3)}.

	test(iso_is_2_31, true(X == 0)) :-
		{X is mod(0, 3+11)}.

	test(iso_is_2_32, true(X == -1)) :-
		{X is mod(7,-2)}.

	test(iso_is_2_33, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is mod(77, N)}.

	test(iso_is_2_34, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is mod(Foo, 77)}.

	test(iso_is_2_35, error(type_error(integer,7.5))) :-
		% try to delay the expected error to runtime
		{G = (_X is mod(7.5, 2)), call(G)}.

	test(iso_is_2_36, error(evaluation_error(zero_divisor))) :-
		% try to delay the expected error to runtime
		{G = (_X is mod(7, 0)), call(G)}.

	test(iso_is_2_37, true(X == 7)) :-
		{X is floor(7.4)}.

	test(iso_is_2_38, true(X == -1)) :-
		{X is floor(-0.4)}.

	test(iso_is_2_39, true(X == 8)) :-
		{X is round(7.5)}.

	test(iso_is_2_40, true(X == 8)) :-
		{X is round(7.6)}.

	test(iso_is_2_41, true(X == -1)) :-
		{X is round(-0.6)}.

	test(iso_is_2_42, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is round(N)}.

	test(iso_is_2_43, true(X == 0)) :-
		{X is ceiling(-0.5)}.

	test(iso_is_2_44, true(X == 0)) :-
		{X is truncate(-0.5)}.

	test(iso_is_2_45, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is truncate(Foo)}.

	test(iso_is_2_46, true(X == 7.0)) :-
		{X is float(7)}.

	test(iso_is_2_47, true(X == 7.3)) :-
		{X is float(7.3)}.

	test(iso_is_2_48, true(X == 1.0)) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		{X is float(5//3)}.

	test(iso_is_2_49, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is float(N)}.

	test(iso_is_2_50, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is float(Foo)}.

	test(iso_is_2_51, true(X == 7)) :-
		{X is abs(7)}.

	test(iso_is_2_52, true(X == 8)) :-
		{X is abs(3-11)}.

	test(iso_is_2_53, true(X == 7.8)) :-
		{X is abs(3.2-11.0)}.

	test(iso_is_2_54, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is abs(N)}.

	test(iso_is_2_55, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is abs(Foo)}.

	:- if(current_prolog_flag(bounded, true)).

	test(iso_is_2_56, error(evaluation_error(int_overflow))) :-
		{current_prolog_flag(max_integer, MI), _X is '+'(MI,1)}.

	test(iso_is_2_57, error(evaluation_error(int_overflow))) :-
		{current_prolog_flag(max_integer, MI), _X is '-'('+'(MI,1),1)}.

	test(iso_is_2_58, error(evaluation_error(int_overflow))) :-
		% ISO allows min_integer = -(max_integer + 1)
		{current_prolog_flag(max_integer, MI), _X is '-'(-2,MI)}.

	test(iso_is_2_59, error(evaluation_error(int_overflow))) :-
		{current_prolog_flag(max_integer, MI), _X is '*'(MI,2)}.

	test(iso_is_2_60, error(evaluation_error(int_overflow))) :-
		{current_prolog_flag(max_integer, MI), R is float(MI)*2, _X is floor(R)}.

	:- else.

	test(iso_is_2_56).

	test(iso_is_2_57).

	test(iso_is_2_58).

	test(iso_is_2_59).

	test(iso_is_2_60).

	:- endif.

	% tests from the Logtalk portability work

	test(lgt_is_2_61, error(type_error(evaluable,foo/3))) :-
		% try to delay the error to runtime
		foo(3, Foo),
		{_X is abs(Foo)}.

	% tests from the ECLiPSe test suite

	test(eclipse_is_2_62, true(X == 3)) :-
		{X is floor(3.5)}.

	test(eclipse_is_2_63, true(X == 4)) :-
		{X is ceiling(3.5)}.

	test(eclipse_is_2_64, true(X == 3)) :-
		{X is truncate(3.5)}.

	test(eclipse_is_2_65, true(X == 4)) :-
		{X is round(3.5)}.

	test(eclipse_is_2_66, true(X == 5)) :-
		{X is round(4.5)}.

	test(eclipse_is_2_67, true(X == -4)) :-
		{X is floor(-3.5)}.

	test(eclipse_is_2_68, true(X == -3)) :-
		{X is ceiling(-3.5)}.

	test(eclipse_is_2_69, true(X == -3)) :-
		{X is truncate(-3.5)}.

	test(eclipse_is_2_70, true(X == -3)) :-
		{X is round(-3.5)}.

	test(eclipse_is_2_71, true(X == -4)) :-
		{X is round(-4.5)}.

	test(eclipse_is_2_72, true(X > 0)) :-
		{X = log(9.9)}.

	test(eclipse_is_2_73, true(_ is X + 1)) :-
		{X = log(9.9)}.

	% tests from the Logtalk portability work

	test(lgt_is_2_74, true(X == 2)) :-
		{X is div(7, 3)}.

	test(lgt_is_2_75, true(X == 0)) :-
		{X is div(0, 3+11)}.

	test(lgt_is_2_76, true(X == -4)) :-
		{X is div(7,-2)}.

	test(lgt_is_2_77, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is div(77, N)}.

	test(lgt_is_2_78, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is div(Foo, 77)}.

	test(lgt_is_2_79, error(type_error(integer,7.5))) :-
		% try to delay the expected error to runtime
		{G = (_X is div(7.5, 2)), call(G)}.

	test(lgt_is_2_80, error(evaluation_error(zero_divisor))) :-
		% try to delay the expected error to runtime
		{G = (_X is div(7, 0)), call(G)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).
	foo(3, foo(1,2,3)).

:- end_object.
