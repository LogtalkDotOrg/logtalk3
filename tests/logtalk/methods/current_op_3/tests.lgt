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


% plain Prolog database for testing calls in the "user" pseudo-object

:- op(777, yfx, my_test_op).
:- op(600, xfy, override).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2024-07-22,
		comment is 'Unit tests for the current_op/3 built-in directive.'
	]).

	test(current_op_3_01, error(type_error(integer,a))) :-
		this(This),
		{This::current_op(a, _, _)}.

	test(current_op_3_02, error(domain_error(operator_priority,3000))) :-
		this(This),
		{This::current_op(3000, _, _)}.

	test(current_op_3_03, error(type_error(atom,1))) :-
		this(This),
		{This::current_op(_, 1, _)}.

	test(current_op_3_04, error(domain_error(operator_specifier,a))) :-
		this(This),
		{This::current_op(_, a, _)}.

	test(current_op_3_05, error(type_error(atom,1))) :-
		this(This),
		{This::current_op(_, _, 1)}.

	test(current_op_3_06, error(instantiation_error)) :-
		{test_object_1::ie(_)}.

	test(current_op_3_07, error(type_error(object_identifier, 1))) :-
		{test_object_1::te}.

	test(current_op_3_08, true(Operators == [abc, def, ghi])) :-
		setof(Operator, test_object_1<<current_op(501, xfx, Operator), Operators).

	test(current_op_3_09, true(Operators == [abc])) :-
		setof(Operator, test_object_1::current_op(501, xfx, Operator), Operators).

	test(current_op_3_10, true) :-
		test_object_1::current_op(600, xfx, override),
		\+ test_object_1::current_op(600, xfy, override).

	test(current_op_3_11, true) :-
		\+ test_object_2::current_op(600, xfx, override).

	test(current_op_3_12, true(Operators == [abc, def, ghi])) :-
		test_object_1::operators(Operators).

	test(current_op_3_13, true(Operators == [opq, rst])) :-
		test_object_2::operators(Operators).

	% tests for the "user" pseudo-object

	test(current_op_3_14, true(Priority-Associativity == 777-yfx)) :-
		user::current_op(Priority, Associativity, (my_test_op)).

	test(current_op_3_15, exists(op(Priority,Associativity,Operator) == op(777,yfx,(my_test_op)))) :-
		user::current_op(Priority, Associativity, Operator).

	test(current_op_3_16, true(Priority-Associativity == 777-yfx)) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::current_op(Priority, Associativity, (my_test_op)).

	test(current_op_3_17, exists(op(Priority,Associativity,Operator) == op(777,yfx,(my_test_op)))) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::current_op(Priority, Associativity, Operator).

	% test semantics for local calls from multifile predicate clauses

	test(current_op_3_18, true(Priority-Associativity == 601-xfx)) :-
		primary::p(Priority, Associativity, op_public),
		Priority == 601, Associativity == xfx.

	test(current_op_3_19, true(Priority-Associativity == 601-xfx)) :-
		primary::p(Priority, Associativity, op_protected),
		Priority == 601, Associativity == xfx.

	test(current_op_3_20, true(Priority-Associativity == 601-xfx)) :-
		primary::p(Priority, Associativity, op_private).

	test(current_op_3_21, true) :-
		\+ primary::p(_, _, op_local).

	% tests for runtime constructed calls

	test(current_op_3_22, true(Priority-Associativity == 678-xfx), [cleanup(abolish_object(Object))]) :-
		create_object(Object, [], [public(op(678, xfx, oops))], []),
		closure(Closure),
		call(Object::Closure, Priority, Associativity,  oops).

	% auxiliary predicates

	user_object(user).

	closure(current_op).

:- end_object.
