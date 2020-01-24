%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.5,
		author is 'Paulo Moura',
		date is 2018/03/28,
		comment is 'Unit tests for the current_op/3 built-in directive.'
	]).

	throws(current_op_3_01, error(type_error(integer,a), logtalk(This::current_op(a,_,_),_))) :-
		this(This),
		{This::current_op(a, _, _)}.

	throws(current_op_3_02, error(domain_error(operator_priority,3000), logtalk(This::current_op(3000,_,_),_))) :-
		this(This),
		{This::current_op(3000, _, _)}.

	throws(current_op_3_03, error(type_error(atom,1), logtalk(This::current_op(_,1,_),_))) :-
		this(This),
		{This::current_op(_, 1, _)}.

	throws(current_op_3_04, error(domain_error(operator_specifier,a), logtalk(This::current_op(_,a,_),_))) :-
		this(This),
		{This::current_op(_, a, _)}.

	throws(current_op_3_05, error(type_error(atom,1), logtalk(This::current_op(_,_,1),_))) :-
		this(This),
		{This::current_op(_, _, 1)}.

	throws(current_op_3_06, error(instantiation_error, logtalk(_::current_op(_,_,_),_))) :-
		{test_object_1::ie(_)}.

	throws(current_op_3_07, error(type_error(object_identifier, 1), logtalk(1::current_op(_,_,_),_))) :-
		{test_object_1::te}.

	succeeds(current_op_3_08) :-
		setof(Operator, test_object_1<<current_op(501, xfx, Operator), Operators),
		Operators == [abc, def, ghi].

	succeeds(current_op_3_09) :-
		setof(Operator, test_object_1::current_op(501, xfx, Operator), Operators),
		Operators == [abc].

	succeeds(current_op_3_10) :-
		test_object_1::current_op(600, xfx, override),
		\+ test_object_1::current_op(600, xfy, override).

	succeeds(current_op_3_11) :-
		\+ test_object_2::current_op(600, xfx, override).

	succeeds(current_op_3_12) :-
		test_object_1::operators(Operators),
		Operators == [abc, def, ghi].

	succeeds(current_op_3_13) :-
		test_object_2::operators(Operators),
		Operators == [opq, rst].

	% tests for the "user" pseudo-object

	succeeds(current_op_3_14) :-
		user::current_op(Priority, Associativity, my_test_op),
		Priority == 777, Associativity == yfx.

	succeeds(current_op_3_15) :-
		user::current_op(Priority, Associativity, Operator),
		Priority == 777, Associativity == yfx, Operator == (my_test_op).

	succeeds(current_op_3_16) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::current_op(Priority, Associativity, my_test_op),
		Priority == 777, Associativity == yfx.

	succeeds(current_op_3_17) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::current_op(Priority, Associativity, Operator),
		Priority == 777, Associativity == yfx, Operator == (my_test_op).

	% test semantics for local calls from multifile predicate clauses

	succeeds(current_op_3_18) :-
		primary::p(Priority, Associativity, op_public),
		Priority == 601, Associativity == xfx.

	succeeds(current_op_3_19) :-
		primary::p(Priority, Associativity, op_protected),
		Priority == 601, Associativity == xfx.

	succeeds(current_op_3_20) :-
		primary::p(Priority, Associativity, op_private),
		Priority == 601, Associativity == xfx.

	succeeds(current_op_3_21) :-
		\+ primary::p(_, _, op_local).

	% auxiliary predicates

	user_object(user).

:- end_object.
