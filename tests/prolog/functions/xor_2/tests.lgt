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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2020-11-04,
		comment is 'Unit tests for the ISO Prolog standard xor/2 built-in function.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.4.6.4

	test(iso_xor_2_01, true(X == 6)) :-
		{X is xor(10, 12)}.

	test(iso_xor_2_02, true(X == 130)) :-
		{X is xor(125, 255)}.

	- test(iso_xor_2_03, true(X == -6), [note('Implementation defined result')]) :-
		% assumes two's complement representation for negative integers
		{X is xor(-10, 12)}.

	% tests from the Logtalk portability work

	test(lgt_xor_2_04, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is xor(10, N)}.

	test(lgt_xor_2_05, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is xor(N, 12)}.

	test(lgt_xor_2_06, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is xor(10, Foo)}.

	test(lgt_xor_2_07, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is xor(Foo, 12)}.

	test(lgt_xor_2_08, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is xor(10, Foo)}.

	test(lgt_xor_2_09, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is xor(Foo, 12)}.

	test(lgt_xor_2_10, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is xor(2, Foo)}.

	test(lgt_xor_2_11, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is xor(Foo, 3)}.

	test(lgt_xor_2_12, error(type_error(integer,1.0))) :-
		{_X is xor(1.0, 2)}.

	test(lgt_xor_2_13, error(type_error(integer,2.0))) :-
		{_X is xor(1, 2.0)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.
