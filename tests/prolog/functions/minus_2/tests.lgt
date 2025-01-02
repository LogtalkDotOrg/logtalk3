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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2022-03-28,
		comment is 'Unit tests for the ISO Prolog standard (-)/2 built-in function.'
	]).

	% tests from the Logtalk portability work

	test(lgt_minus_2_01, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is 3 - N}.

	test(lgt_minus_2_02, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is N - 3}.

	test(lgt_minus_2_03, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is 3 - Foo}.

	test(lgt_minus_2_04, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is Foo - 3}.

	test(lgt_minus_2_05, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is 3 - Foo}.

	test(lgt_minus_2_06, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is Foo - 3}.

	test(lgt_minus_2_07, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is 3 - Foo}.

	test(lgt_minus_2_08, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is Foo - 3}.

	test(lgt_minus_2_09, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		current_prolog_flag(min_integer, Min),
		{_X is Min - 1}.

	test(lgt_minus_2_10, true((C == 35))) :-
		{C is 42 - 7}.

	test(lgt_minus_2_11, true((A == 42, B == 7, C == 35))) :-
		{A = 42, B = 7, C is A - B}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.
