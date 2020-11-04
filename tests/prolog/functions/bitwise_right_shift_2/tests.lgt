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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2020-11-04,
		comment is 'Unit tests for the ISO Prolog standard (>>)/1 built-in function.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.4.1.4

	test(iso_bitwise_right_shift_2_01, true(X == 4)) :-
		{X is '>>'(16, 2)}.

	test(iso_bitwise_right_shift_2_02, true(X == 4)) :-
		{X is '>>'(19, 2)}.

	- test(iso_bitwise_right_shift_2_03, true(X == -4), [note('Implementation defined result')]) :-
		% assumes two's complement representation for negative integers
		{X is '>>'(-16, 2)}.

	test(iso_bitwise_right_shift_1_04, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '>>'(77, N)}.

	test(iso_bitwise_right_shift_2_05, error(type_error(evaluable,foo/0))) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '>>'(Foo, 2)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_bitwise_right_shift_2_06, error(type_error(integer,1.0))) :-
		{_X is '>>'(1.0, 2)}.

	% tests from the Logtalk portability work

	test(lgt_bitwise_right_shift_2_07, error(type_error(integer,2.0))) :-
		{_X is '>>'(1, 2.0)}.

	test(lgt_bitwise_right_shift_1_08, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '>>'(N, 77)}.

	test(lgt_bitwise_right_shift_2_09, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '>>'(2, Foo)}.

	test(lgt_bitwise_right_shift_2_10, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is '>>'(Foo, 2)}.

	test(lgt_bitwise_right_shift_2_11, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is '>>'(2, Foo)}.

	test(lgt_bitwise_right_shift_2_12, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is '>>'(Foo, 2)}.

	test(lgt_bitwise_right_shift_2_13, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is '>>'(2, Foo)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.
