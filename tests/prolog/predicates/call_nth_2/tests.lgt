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


a(1).
a(2).
a(3).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-07-05,
		comment is 'Unit tests for the call_nth/2 predicate found on increasing number of Prolog systems.'
	]).

	% tests from the Logtalk portability work

	test(lgt_call_nth_2_01, error(instantiation_error)) :-
		variable(Variable),
		{call_nth(Variable, _)}.

	test(lgt_call_nth_2_02, error(type_error(callable, 1))) :-
		one(One),
		{call_nth(One, _)}.

	test(lgt_call_nth_2_03, error(type_error(integer, Foo))) :-
		foo(Foo),
		{call_nth(a, Foo)}.

	test(lgt_call_nth_2_04, error(domain_error(not_less_than_zero, -1))) :-
		{call_nth(a, -1)}.

	- test(lgt_call_nth_2_05, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true)), note('skipped as it would require the predicate to evaluate integer expressions')]) :-
		current_prolog_flag(max_integer, Max),
		{call_nth(a, Max + 1)}.

	test(lgt_call_nth_2_06, error(existence_error(procedure, Foo))) :-
		foo(Foo),
		{call_nth(Foo, _)}.

	test(lgt_call_nth_2_07, true(X == 1)) :-
		{call_nth(a(X), _)}.

	test(lgt_call_nth_2_08, deterministic(X == 2)) :-
		{call_nth(a(X), 2)}.

	test(lgt_call_nth_2_09, false) :-
		{call_nth(a(_), 0)}.

	test(lgt_call_nth_2_10, false) :-
		{call_nth(a(_), 7)}.

	test(lgt_call_nth_2_11, true(L == [1-1,2-2,3-3])) :-
		findall(X-N, {call_nth(a(X), N)}, L).

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	one(1).

	foo(foo).

:- end_object.
