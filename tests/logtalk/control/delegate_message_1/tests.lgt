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


:- object(library).

	:- public(my_call/2).
	:- meta_predicate(my_call(1, *)).
	my_call(Closure, Arg) :-
		call(Closure, Arg).

:- end_object.



:- object(delegate_message_test_object_1).

	:- public(s/1).
	s(X) :-
		sender(X).

:- end_object.



:- object(delegate_message_test_object_2).

	:- public(p/1).
	p(X) :-
		delegate_message_test_object_1::s(X).

	:- public(q/1).
	q(X) :-
		[delegate_message_test_object_1::s(X)].

	:- public(r/1).
	r(X) :-
		call([delegate_message_test_object_1::s], X).

	:- public(s/1).
	s(X) :-
		Closure = [delegate_message_test_object_1::s],
		call(Closure, X).

	:- public(t/1).
	t(X) :-
		library::my_call([delegate_message_test_object_1::s], X).

	:- public(u/0).
	u :-
		[user::foo].

:- end_object.



:- object(test_object_alias).

	:- uses([
		delegate_message_test_object_1 as delegate
	]).

	:- public(w/1).
	w(Sender) :-
		[delegate::s(Sender)].

:- end_object.



:- object(report_execution_context).

	:- public(p/6).
	p(Entity, Sender, This, Self, MetaCallContext, CoinductionStack) :-
		context(Context),
		Context = logtalk(_Head, ExecutionContext),
		logtalk::execution_context(ExecutionContext, Entity, Sender, This, Self, MetaCallContext, CoinductionStack).

	:- public(m/1).
	:- meta_predicate(m(0)).
	m(X) :-
		call(X).

:- end_object.



:- object(test_execution_context).

	:- public(p/6).
	p(Entity, Sender, This, Self, MetaCallContext, CoinductionStack) :-
		[report_execution_context::p(Entity, Sender, This, Self, MetaCallContext, CoinductionStack)].

	:- public(m/1).
	:- meta_predicate(m(0)).
	m(X) :-
		[report_execution_context::m(X)].

	:- public(n/1).
	n(X) :-
		[report_execution_context::m(h(X))].

	h(2).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2024-07-08,
		comment is 'Unit tests for the []/1 built-in control construct.'
	]).

	test(delegate_message_1_01, true(X == delegate_message_test_object_2)) :-
		{delegate_message_test_object_2::p(X)}.

	test(delegate_message_1_02, true(X == user)) :-
		{delegate_message_test_object_2::q(X)}.

	test(delegate_message_1_03, true(X == user)) :-
		{delegate_message_test_object_2::r(X)}.

	test(delegate_message_1_04, true(X == user)) :-
		{delegate_message_test_object_2::s(X)}.

	test(delegate_message_1_05, true(X == user)) :-
		{delegate_message_test_object_2::t(X)}.

	test(delegate_message_1_06, true(X == user)) :-
		{test_object_alias::w(X)}.

	test(delegate_message_1_07, error(instantiation_error)) :-
		% delay the error to runtime
		{logtalk << [_]}.

	test(delegate_message_1_08, error(type_error(callable, 1))) :-
		% delay the error to runtime
		{logtalk << [1]}.

	test(delegate_message_1_09, error(domain_error(message_sending_goal, foo))) :-
		% delay the error to runtime
		{logtalk << [foo]}.

	test(delegate_message_1_10, error(permission_error(access, object, user))) :-
		{delegate_message_test_object_2::u}.

	test(delegate_message_1_11, true(ctx(Entity, Sender, This, Self, MetaCallContext, CoinductionStack) == ctx(report_execution_context,tests,report_execution_context,report_execution_context,[],[]))) :-
		test_execution_context::p(Entity, Sender, This, Self, MetaCallContext, CoinductionStack).

	test(delegate_message_1_12, true(X == 1)) :-
		test_execution_context::m(g(X)).

	test(delegate_message_1_13, true(X == 2)) :-
		test_execution_context::n(X).

	g(1).

:- end_object.
