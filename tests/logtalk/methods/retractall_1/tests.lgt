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
		comment is 'Unit tests for the retractall/1 built-in method.'
	]).

	test(retractall_1_01, error(instantiation_error)) :-
		{test_object::retractall(_)}.

	test(retractall_1_02, error(type_error(callable, 1))) :-
		{test_object::retractall(1)}.

	test(retractall_1_03, error(permission_error(modify, protected_predicate, q/2))) :-
		{test_object::retractall(q(_,_))}.

	test(retractall_1_04, error(permission_error(modify, private_predicate, r/3))) :-
		{test_object::retractall(r(_,_,_))}.

	test(retractall_1_05, error(permission_error(modify, static_predicate, s/4))) :-
		{test_object::retractall(s(_,_,_,_))}.

	test(retractall_1_06, error(existence_error(predicate_declaration, unknown/1))) :-
		{test_object::retractall(unknown(_))}.

	test(retractall_1_07, error(existence_error(predicate_declaration, (local)/1))) :-
		{test_object::retractall(local(_))}.

	test(retractall_1_08, error(instantiation_error)) :-
		{test_object::ie(_)}.

	test(retractall_1_09, error(type_error(object_identifier, 1))) :-
		{test_object::te}.

	test(retractall_1_10, true((\+ test_object::t(3), \+ test_object::t(_)))) :-
		test_object::retractall(t(3)),
		test_object::t(1),
		t2_head(T2),
		test_object::T2,
		test_object::retractall(t(_)).

	test(retractall_1_11, true((\+ Object::t(3), \+ Object::t(_))), [cleanup(abolish_object(Object))]) :-
		create_object(Object, [], [public(t/1), dynamic(t/1)], [t(1), (t(2) :-t(1)), (t(3) :-t(1),t(2))]),
		Object::retractall(t(3)),
		Object::t(1),
		t2_head(T2),
		Object::T2,
		Object::retractall(t(_)).

	% tests for the "user" pseudo-object

	test(retractall_1_12, true({bar(2)})) :-
		user::retractall(bar(1)).

	test(retractall_1_13, true(\+ {bar(_)})) :-
		bar_clause(Baz),
		user::retractall(Baz).

	test(retractall_1_14, true({baz(2)})) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::retractall(baz(1)).

	test(retractall_1_15, true(\+ {baz(_)})) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		baz_clause(Baz),
		Object::retractall(Baz).

	test(retractall_1_16, true(L == [2]), [cleanup(abolish_object(Object))]) :-
		create_object(Object, [], [public(p/1), dynamic(p/1)], [p(1),p(2),p(1)]),
		closure(Closure),
		call(Object::Closure, p(1)),
		findall(X, Object::p(X), L).

	% auxiliary predicates

	t2_head(t(2)).

	bar_clause(bar(2)).

	baz_clause(baz(2)).

	user_object(user).

	closure(retractall).

:- end_object.
