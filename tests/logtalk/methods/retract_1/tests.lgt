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

:- dynamic(bar/1).
bar(1). bar(2).

:- dynamic(baz/1).
baz(1). baz(2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2024-07-20,
		comment is 'Unit tests for the retract/1 built-in method.'
	]).

	test(retract_1_01, error(instantiation_error)) :-
		{test_object::retract(_)}.

	test(retract_1_02, error(instantiation_error)) :-
		{test_object::retract((_ :- _))}.

	test(retract_1_03, error(type_error(callable, 1))) :-
		{test_object::retract(1)}.

	test(retract_1_04, error(type_error(callable, 1))) :-
		{test_object::retract((1 :- _))}.

	test(retract_1_05, error(permission_error(modify, protected_predicate, q/2))) :-
		{test_object::retract(q(_,_))}.

	test(retract_1_06, error(permission_error(modify, private_predicate, r/3))) :-
		{test_object::retract(r(_,_,_))}.

	test(retract_1_07, error(permission_error(modify, static_predicate, s/4))) :-
		{test_object::retract(s(_,_,_,_))}.

	test(retract_1_08, error(existence_error(predicate_declaration, unknown/1))) :-
		{test_object::retract(unknown(_))}.

	test(retract_1_09, error(existence_error(predicate_declaration, (local)/1))) :-
		{test_object::retract(local(_))}.

	test(retract_1_10, error(instantiation_error)) :-
		{test_object::ie(_)}.

	test(retract_1_11, error(type_error(object_identifier, 1))) :-
		{test_object::te}.

	test(retract_1_12, true(s(X,Body1,Body2) == s(1,t(1),(t(1), t(2))))) :-
		test_object::retract((t(X) :-true)),
		X == 1,
		t2_head(T2),
		test_object::retract((T2 :-Body1)),
		Body1 == t(1),
		test_object::retract((t(3) :-Body2)),
		Body2 == (t(1), t(2)).

	test(retract_1_13, true(Body1-Body2 == t(1)-(t(1), t(2)))) :-
		create_object(Object, [], [public(t/1), dynamic(t/1)], [t(1), (t(2) :-t(1)), (t(3) :-t(1),t(2))]),
		Object::retract((t(X) :-true)),
		X == 1,
		t2_head(T2),
		Object::retract((T2 :-Body1)),
		Object::retract((t(3) :-Body2)),
		abolish_object(Object).

	% tests for the "user" pseudo-object

	test(retract_1_14, true(X == 1)) :-
		user::retract(bar(X)).

	test(retract_1_15, true(Baz == bar(2))) :-
		bar_clause(Baz),
		user::retract(Baz).

	test(retract_1_16, true(X == 1)) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::retract(baz(X)).

	test(retract_1_17, true(Baz == baz(2))) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		baz_clause(Baz),
		Object::retract(Baz).

	test(retract_1_18, true(X-L == 1-[2,1]), [cleanup(abolish_object(Object))]) :-
		create_object(Object, [], [public(p/1), dynamic(p/1)], [p(1),p(2),p(1)]),
		closure(Closure),
		call(Object::Closure, p(X)),
		findall(Y, Object::p(Y), L).

	% auxiliary predicates

	t2_head(t(2)).

	bar_clause(bar(2)).

	baz_clause(baz(2)).

	user_object(user).

	closure(retract).

:- end_object.
