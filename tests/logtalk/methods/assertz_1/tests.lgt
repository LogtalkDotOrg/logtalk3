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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2024-07-20,
		comment is 'Unit tests for the assertz/1 built-in method.'
	]).

	test(assertz_1_01, error(instantiation_error)) :-
		{test_object::assertz(_)}.

	test(assertz_1_02, error(instantiation_error)) :-
		{test_object::assertz((_ :- _))}.

	test(assertz_1_03, error(type_error(callable, 1))) :-
		{test_object::assertz(1)}.

	test(assertz_1_04, error(type_error(callable, 1))) :-
		{test_object::assertz((1 :- _))}.

	test(assertz_1_05, error(type_error(callable, 1))) :-
		{test_object::assertz((p :- 1))}.

	test(assertz_1_06, error(permission_error(modify, protected_predicate, q/2))) :-
		{test_object::assertz(q(_,_))}.

	test(assertz_1_07, error(permission_error(modify, protected_predicate, q/2))) :-
		{test_object::assertz((q(_,_) :- nl))}.

	test(assertz_1_08, error(permission_error(modify, private_predicate, r/3))) :-
		{test_object::assertz(r(_,_,_))}.

	test(assertz_1_09, error(permission_error(modify, private_predicate, r/3))) :-
		{test_object::assertz((r(_,_,_) :- nl))}.

	test(assertz_1_10, error(permission_error(modify, static_predicate, s/4))) :-
		{test_object::assertz(s(_,_,_,_))}.

	test(assertz_1_11, error(permission_error(modify, static_predicate, s/4))) :-
		{test_object::assertz((s(_,_,_,_) :- nl))}.

	test(assertz_1_12, error(permission_error(create, predicate_declaration, new/0))) :-
		{test_object::assertz(new)}.

	test(assertz_1_13, error(instantiation_error)) :-
		{test_object::ie(_)}.

	test(assertz_1_14, error(type_error(object_identifier, 1))) :-
		{test_object::te}.

	test(assertz_1_15, true(Xs-Ys == [1,2,3]-[1,2,3])) :-
		create_object(Object, [], [public(a/1), public(p/1)], []),
		Object::assertz(a(1)),
		a2_clause(A2),
		Object::assertz(A2),
		Object::assertz(a(3)),
		Object::assertz((p(X) :- a(X))),
		findall(X, Object::a(X), Xs),
		findall(Y, Object::p(Y), Ys),
		abolish_object(Object).

	test(assertz_1_16, true(Xs-Ys == [1,2,3]-[1,2,3])) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations, allow)], []),
		Object::assertz(a(1)),
		a2_clause(A2),
		Object::assertz(A2),
		Object::assertz(a(3)),
		Object::assertz((p(X) :- a(X))),
		findall(X, Object::a(X), Xs),
		findall(Y, Object::p(Y), Ys),
		abolish_object(Object).

	% tests for the "user" pseudo-object

	test(assertz_1_17, true({bar})) :-
		user::assertz(bar).

	test(assertz_1_18, true({Baz})) :-
		baz_clause(Baz),
		user::assertz(Baz).

	test(assertz_1_19, true({foobar})) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::assertz(foobar).

	test(assertz_1_20, true({FooBaz})) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		foobaz_clause(FooBaz),
		Object::assertz(FooBaz).

	test(assertz_1_21, true(X == 1), [cleanup(abolish_object(Object))]) :-
		create_object(Object, [], [public(p/1)], []),
		closure(Closure),
		call(Object::Closure, p(1)),
		Object::p(X).

	cleanup :-
		{abolish(bar/0), abolish(baz/0), abolish(foobar/0), abolish(foobaz/0)}.

	% auxiliary predicates

	a2_clause(a(2)).

	baz_clause(baz).

	foobaz_clause(foobaz).

	user_object(user).

	closure(assertz).

:- end_object.
