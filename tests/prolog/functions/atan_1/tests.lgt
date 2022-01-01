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
		date is 2021-01-26,
		comment is 'Unit tests for the ISO Prolog standard atan/1 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.3.4.4

	test(iso_atan_1_01, true(X == 0.0)) :-
		{X is atan(0.0)}.

	test(iso_atan_1_02, true(PI =~= 3.1415927)) :-
		{PI is atan(1.0)*4}.

	test(iso_atan_1_03, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is atan(N)}.

	test(iso_atan_1_04, true(X == 0.0)) :-
		{X is atan(0)}.

	test(iso_atan_1_05, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is atan(Foo)}.

	% tests from the Logtalk portability work

	test(lgt_atan_1_06, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is atan(Foo)}.

	test(lgt_atan_1_07, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is atan(Foo)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.
