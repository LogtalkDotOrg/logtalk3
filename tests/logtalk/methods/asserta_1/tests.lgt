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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2024-07-20,
		comment is 'Unit tests for the asserta/1 built-in method.'
	]).

	test(asserta_1_01, error(instantiation_error)) :-
		{test_object::asserta(_)}.

	test(asserta_1_02, error(instantiation_error)) :-
		{test_object::asserta((_ :- _))}.

	test(asserta_1_03, error(type_error(callable, 1))) :-
		{test_object::asserta(1)}.

	test(asserta_1_04, error(type_error(callable, 1))) :-
		{test_object::asserta((1 :- _))}.

	test(asserta_1_05, error(type_error(callable, 1))) :-
		{test_object::asserta((p :- 1))}.

	test(asserta_1_06, error(permission_error(modify, protected_predicate, q/2))) :-
		{test_object::asserta(q(_,_))}.

	test(asserta_1_07, error(permission_error(modify, protected_predicate, q/2))) :-
		{test_object::asserta((q(_,_) :- nl))}.

	test(asserta_1_08, error(permission_error(modify, private_predicate, r/3))) :-
		{test_object::asserta(r(_,_,_))}.

	test(asserta_1_09, error(permission_error(modify, private_predicate, r/3))) :-
		{test_object::asserta((r(_,_,_) :- nl))}.

	test(asserta_1_10, error(permission_error(modify, static_predicate, s/4))) :-
		{test_object::asserta(s(_,_,_,_))}.

	test(asserta_1_11, error(permission_error(modify, static_predicate, s/4))) :-
		{test_object::asserta((s(_,_,_,_) :- nl))}.

	test(asserta_1_12, error(permission_error(create, predicate_declaration, new/0))) :-
		{test_object::asserta(new)}.

	test(asserta_1_13, error(instantiation_error)) :-
		{test_object::ie(_)}.

	test(asserta_1_14, error(type_error(object_identifier, 1))) :-
		{test_object::te}.

	test(asserta_1_15, true(Xs-Ys == [3,2,1]-[3,2,1])) :-
		create_object(Object, [], [public(a/1), public(p/1)], []),
		Object::asserta(a(1)),
		a2_clause(A2),
		Object::asserta(A2),
		Object::asserta(a(3)),
		Object::asserta((p(X) :- a(X))),
		findall(X, Object::a(X), Xs),
		findall(Y, Object::p(Y), Ys),
		abolish_object(Object).

	test(asserta_1_16, true(Xs-Ys == [3,2,1]-[3,2,1])) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations, allow)], []),
		Object::asserta(a(1)),
		a2_clause(A2),
		Object::asserta(A2),
		Object::asserta(a(3)),
		Object::asserta((p(X) :- a(X))),
		findall(X, Object::a(X), Xs),
		findall(Y, Object::p(Y), Ys),
		abolish_object(Object).

	% tests for the "user" pseudo-object

	test(asserta_1_17, true({bar})) :-
		user::asserta(bar).

	test(asserta_1_18, true({Baz})) :-
		baz_clause(Baz),
		user::asserta(Baz).

	test(asserta_1_19, true({foobar})) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::asserta(foobar).

	test(asserta_1_20, true({FooBaz})) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		foobaz_clause(FooBaz),
		Object::asserta(FooBaz).

	test(asserta_1_21, true(X == 1), [cleanup(abolish_object(Object))]) :-
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

	closure(asserta).

:- end_object.
