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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2020-11-05,
		comment is 'Unit tests for the ISO Prolog standard (**)/2 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.3.1.4

	test(iso_power_2_01, true(X =~= 125.0)) :-
		{X is '**'(5, 3)}.

	test(iso_power_2_02, true(X =~= -125.0)) :-
		{X is '**'(-5.0, 3)}.

	test(iso_power_2_03, true(X =~= 0.2)) :-
		{X is '**'(5, -1)}.

	test(iso_power_2_04, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '**'(77, N)}.

	test(iso_power_2_05, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '**'(Foo, 3)}.

	test(iso_power_2_06, true(X =~= 125.0)) :-
		{X is '**'(5, 3.0)}.

	test(iso_power_2_07, true(X =~= 1.0)) :-
		{X is '**'(0, 0.0)}.

	test(lgt_power_2_08, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '**'(N, 77)}.

	test(iso_power_2_09, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is '**'(5, Foo)}.

	test(iso_power_2_10, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is '**'(Foo, 3)}.

	test(iso_power_2_11, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is '**'(5, Foo)}.

	test(iso_power_2_12, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is '**'(Foo, 3)}.

	test(iso_power_2_13, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is '**'(5, Foo)}.

	% tests from the Logtalk portability work

	% there is a dispute about the correct error for a zero argument
	% see e.g. http://eclipseclp.org/wiki/Prolog/IsoErrata
	% many Prolog systems even don't throw en error but return infinity
	test(lgt_power_2_14, errors([evaluation_error(undefined), evaluation_error(zero_divisor)])) :-
		% try to delay the error to runtime
		negative(Negative),
		{_X is '**'(0, Negative)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

	negative(-1).

:- end_object.
