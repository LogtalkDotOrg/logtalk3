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


% plain Prolog database for testing calls in the "user" pseudo-object

:- dynamic(bar/0).
bar.

:- dynamic(baz/0).
baz.

:- dynamic(foobar/0).
foobar.

:- dynamic(foobaz/0).
foobaz.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2024-07-20,
		comment is 'Unit tests for the clause/2 built-in method.'
	]).

	test(clause_2_01, error(instantiation_error)) :-
		{test_object::clause(_, _)}.

	test(clause_2_02, error(type_error(callable, 1))) :-
		{test_object::clause(1, _)}.

	test(clause_2_03, error(type_error(callable, 1))) :-
		{test_object::clause(head, 1)}.

	test(clause_2_04, error(permission_error(access, protected_predicate, q/2))) :-
		{test_object::clause(q(_,_), _)}.

	test(clause_2_05, error(permission_error(access, private_predicate, r/3))) :-
		{test_object::clause(r(_,_,_), _)}.

	test(clause_2_06, error(permission_error(access, static_predicate, s/4))) :-
		{test_object::clause(s(_,_,_,_), _)}.

	test(clause_2_07, error(existence_error(predicate_declaration, unknown/1))) :-
		{test_object::clause(unknown(_), _)}.

	test(clause_2_08, error(instantiation_error)) :-
		{test_object::ie(_)}.

	test(clause_2_09, error(type_error(object_identifier, 1))) :-
		{test_object::te}.

	test(clause_2_10, true(s(X,Body1,Body2) == s(1,t(1),(t(1), t(2))))) :-
		test_object::clause(t(X), true),
		t2_head(T2),
		test_object::clause(T2, Body1),
		test_object::clause(t(3), Body2).

	test(clause_2_11, true(s(X,Body1,Body2) == s(1,t(1),(t(1), t(2))))) :-
		create_object(Object, [], [public(t/1), dynamic(t/1)], [t(1), (t(2) :- t(1)), (t(3) :- t(1),t(2))]),
		Object::clause(t(X), true),
		t2_head(T2),
		Object::clause(T2, Body1),
		Object::clause(t(3), Body2),
		abolish_object(Object).

	% tests for the "user" pseudo-object

	test(clause_2_12, true(Body == true)) :-
		user::clause(bar, Body).

	test(clause_2_13, true(Body == true)) :-
		baz_clause(Baz),
		user::clause(Baz, Body).

	test(clause_2_14, true(Body == true)) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::clause(foobar, Body).

	test(clause_2_15, true(Body == true)) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		foobaz_clause(FooBaz),
		Object::clause(FooBaz, Body).

	test(clause_2_16, true(L == [1-true,2-true,1-true]), [cleanup(abolish_object(Object))]) :-
		create_object(Object, [], [public(p/1), dynamic(p/1)], [p(1),p(2),p(1)]),
		closure(Closure),
		findall(X-Body, call(Object::Closure, p(X), Body), L).

	% auxiliary predicates

	t2_head(t(2)).

	baz_clause(baz).

	foobaz_clause(foobaz).

	user_object(user).

	closure(clause).

:- end_object.
