%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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

a(1).

b(1, 2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2024-07-21,
		comment is 'Unit tests for the current_predicate/1 built-in method.'
	]).

	test(current_predicate_1_01, error(type_error(predicate_indicator, 1))) :-
		this(This),
		{This::current_predicate(1)}.

	test(current_predicate_1_02, error(type_error(atom,1))) :-
		this(This),
		{This::current_predicate(1/b)}.

	test(current_predicate_1_03, error(type_error(integer,b))) :-
		this(This),
		{This::current_predicate(a/b)}.

	test(current_predicate_1_04, error(domain_error(not_less_than_zero, -1))) :-
		this(This),
		{This::current_predicate(a/(-1))}.

	test(current_predicate_1_05, error(instantiation_error)) :-
		{test_object::ie(_)}.

	test(current_predicate_1_06, error(type_error(object_identifier, 1))) :-
		{test_object::te}.

	test(current_predicate_1_07, true) :-
		test_object::current_predicate(ie/1).

	test(current_predicate_1_08, true) :-
		test_object::current_predicate(te/0).

	test(current_predicate_1_09, true(Predicates == [ie/1, te/0])) :-
		setof(Predicate, test_object::current_predicate(Predicate), Predicates).

	test(current_predicate_1_10, false) :-
		test_object::current_predicate(foo/2).

	% tests for the "user" pseudo-object

	test(current_predicate_1_11, true) :-
		user::current_predicate(a/1).

	test(current_predicate_1_12, true(Arity == 2)) :-
		user::current_predicate(b/Arity).

	test(current_predicate_1_13, true) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::current_predicate(a/1).

	test(current_predicate_1_14, true(Arity == 2)) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::current_predicate(b/Arity).

	% test semantics for local calls from multifile predicate clauses

	test(current_predicate_1_15, true(Predicates == [a/1, b/2, c/3])) :-
		setof(Predicate, primary::p(Predicate), Predicates).

	% test semantics for re-declared predicates

	test(current_predicate_1_16, true(Predicates == [foobar/0])) :-
		findall(Predicate, proto::current_predicate(Predicate), Predicates).

	test(current_predicate_1_17, false) :-
		proto::current_predicate(foo/_).

	test(current_predicate_1_18, false) :-
		proto::current_predicate(_/1).

	test(current_predicate_1_19, false) :-
		proto::current_predicate(foo/1).

	test(current_predicate_1_20, false) :-
		proto::current_predicate(bar/_).

	test(current_predicate_1_21, false) :-
		proto::current_predicate(_/2).

	test(current_predicate_1_22, false) :-
		proto::current_predicate(bar/2).

	test(current_predicate_1_23, deterministic) :-
		proto::current_predicate(foobar/0).

	test(current_predicate_1_24, true, [cleanup(abolish_object(Object))]) :-
		create_object(Object, [], [public(p/1)], []),
		closure(Closure),
		call(Object::Closure, p/1).

	% auxiliary predicates

	user_object(user).

	closure(current_predicate).

:- end_object.
