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
		date is 2021-07-05,
		comment is 'Unit tests for the de facto standard log10/1 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	% tests from the Logtalk portability work

	test(lgt_log10_1_01, true(X =~= 1.0)) :-
		{X is log10(10)}.

	test(lgt_log10_1_02, true(X =~= 0.3010299956639812)) :-
		{X is log10(2)}.

	test(lgt_log10_1_03, true(X =~= 0.43429448190325182)) :-
		E is exp(1),
		{X is log10(E)}.

	test(lgt_log10_1_04, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(X),
		{_ is log10(X)}.

	test(lgt_log10_1_05, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is log10(Foo)}.

	test(lgt_log10_1_06, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is log10(Foo)}.

	test(lgt_log10_1_07, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is log10(Foo)}.

	test(lgt_log10_1_08, error(evaluation_error(_))) :-
		{_X is log10(0)}.

	% many Prolog systems don't throw en error but return NaN
	test(lgt_log10_1_09, error(evaluation_error(undefined))) :-
		{_X is log10(-1)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.
