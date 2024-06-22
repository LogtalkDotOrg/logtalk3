%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


:- object(context_switch_test_object).

	:- set_logtalk_flag(context_switching_calls, deny).

	p.

:- end_object.


:- object(context_switch_test_object(_)).

	:- set_logtalk_flag(context_switching_calls, allow).

	p(X) :-
		parameter(1, X).

:- end_object.


context_switch_test_object(1).
context_switch_test_object(2).
context_switch_test_object(3).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2024-06-22,
		comment is 'Unit tests for the (<<)/2 built-in control construct.'
	]).

	test(context_switch_2_01, error(instantiation_error)) :-
		% delay the error to runtime
		{_ << goal}.

	test(context_switch_2_02, error(instantiation_error)) :-
		% delay the error to runtime
		{logtalk << _}.

	test(context_switch_2_03, error(type_error(object_identifier, 3))) :-
		% delay the error to runtime
		{3 << goal}.

	test(context_switch_2_04, error(type_error(callable, 3))) :-
		% delay the error to runtime
		{object << 3}.

	test(context_switch_2_05, error(existence_error(procedure, goal/0))) :-
		this(This),
		% delay the error to runtime
		{This << goal}.

	test(context_switch_2_06, error(existence_error(object, foo))) :-
		% delay the error to runtime
		{foo << goal}.

	test(context_switch_2_07, error(permission_error(access, database, p))) :-
		% delay the error to runtime
		{context_switch_test_object << p}.

	test(context_switch_2_08, deterministic) :-
		% delay the error to runtime
		{user << true}.

	test(context_switch_2_09, deterministic) :-
		% delay the error to runtime
		{logtalk << true}.

	test(context_switch_2_10, true(Xs == [1,2,3])) :-
		this(This),
		findall(X, {This << a(X)}, Xs).

	test(context_switch_2_11, true(Xs == [1,2,3])) :-
		create_object(Object, [], [], [a(1),a(2),a(3)]),
		findall(X, {Object << a(X)}, Xs),
		abolish_object(Object).

	test(context_switch_2_12, true(Xs == [1,2,3])) :-
		findall(X, {context_switch_test_object(_)}<<p(X), Xs).

	test(context_switch_2_13, false) :-
		{user << fail}.

	test(context_switch_2_14, false) :-
		{logtalk << fail}.

	test(context_switch_2_15, false) :-
		this(This),
		{This << a(4)}.

	test(context_switch_2_16, false) :-
		{context_switch_test_object(_)}<<p(4).

	test(context_switch_2_17, deterministic(Rest == [3])) :-
		phrase(gr, [1,2,3], Rest).

	a(1). a(2). a(3).

	gr -->
		{this(This)},
		This << a.

	a --> [1, 2].

:- end_object.
