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
		date is 2021-01-26,
		comment is 'Unit tests for the ISO Prolog standard atan2/2 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.3.13.4

	test(iso_atan2_2_01, true(X =~= 1.5707963)) :-
		{X is atan2(1, 0)}.

	test(iso_atan2_2_02, true(X =~= 3.1415927)) :-
		{X is atan2(0, -1)}.

	test(iso_atan2_2_03, error(evaluation_error(undefined))) :-
		{_X is atan2(0, 0)}.

	% tests from the Logtalk portability work

	test(lgt_atan2_2_04, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{_X is atan2(1, X)}.

	test(lgt_atan2_2_05, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{_X is atan2(X, 0)}.

	test(lgt_atan2_2_06, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is atan2(1, Foo)}.

	test(lgt_atan2_2_07, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is atan2(Foo, 0)}.

	test(lgt_atan2_2_08, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is atan2(1, Foo)}.

	test(lgt_atan2_2_09, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is atan2(Foo, 0)}.

	test(lgt_atan2_2_10, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is atan2(1, Foo)}.

	test(lgt_atan2_2_11, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is atan2(Foo, 0)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.
