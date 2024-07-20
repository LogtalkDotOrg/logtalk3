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

:- dynamic(bar/1).
bar(1). bar(2).

:- dynamic(baz/1).
baz(1). baz(2).

:- dynamic(foobar/1).
foobar(1).

:- dynamic(foobaz/1).
foobaz(1).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2024-07-20,
		comment is 'Unit tests for the abolish/1 built-in method.'
	]).

	test(abolish_1_01, error(instantiation_error)) :-
		{test_object::abolish(_)}.

	test(abolish_1_02, error(instantiation_error)) :-
		{test_object::abolish(_/1)}.

	test(abolish_1_03, error(instantiation_error)) :-
		{test_object::abolish(foo/_)}.

	test(abolish_1_04, error(instantiation_error)) :-
		{test_object::abolish(foo/_)}.

	test(abolish_1_05, error(type_error(predicate_indicator, foo))) :-
		{test_object::abolish(foo)}.

	test(abolish_1_06, error(type_error(atom, 1))) :-
		{test_object::abolish(1/2)}.

	test(abolish_1_07, error(type_error(integer, bar))) :-
		{test_object::abolish(foo/bar)}.

	test(abolish_1_08, error(permission_error(modify, predicate_declaration, p/1))) :-
		{test_object::abolish(p/1)}.

	test(abolish_1_09, error(permission_error(modify, protected_predicate, q/2))) :-
		{test_object::abolish(q/2)}.

	test(abolish_1_10, error(permission_error(modify, private_predicate, r/3))) :-
		{test_object::abolish(r/3)}.

	test(abolish_1_11, error(permission_error(modify, static_predicate, s/4))) :-
		{test_object::abolish(s/4)}.

	test(abolish_1_12, error(existence_error(predicate_declaration, foo/0))) :-
		{test_object::abolish(foo/0)}.

	test(abolish_1_13, error(existence_error(predicate_declaration, p/1))) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
		Object::assertz(p(1)),
		% clauses exist before abolishing
		Object::abolish(p/1),
		Object::p(_).

	test(abolish_1_14, error(existence_error(predicate_declaration, p/1))) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
		Object::assertz(p(1)),
		Object::retract(p(_)),
		% clauses don't exist before abolishing
		Object::abolish(p/1),
		Object::p(_).

	test(abolish_1_15, error(existence_error(procedure, p/1))) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
		Object::assertz(p(1)),
		% clauses exist before abolishing
		Object::abolish(p/1),
		Object<<p(_).

	test(abolish_1_16, error(existence_error(procedure, p/1))) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
		Object::assertz(p(1)),
		Object::retract(p(_)),
		% clauses don't exist before abolishing
		Object::abolish(p/1),
		Object<<p(_).

	test(abolish_1_17, error(instantiation_error)) :-
		{test_object::ie(_)}.

	test(abolish_1_18, error(type_error(object_identifier, 1))) :-
		{test_object::te}.

	test(abolish_1_19, true((\+ Object::current_predicate(a/1), \+ Object::current_predicate(p/1))), [cleanup(abolish_object(Object))]) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations, allow)], []),
		Object::assertz(a(1)),
		Object::current_predicate(a/1),
		Object::assertz((p(X) :- a(X))),
		Object::current_predicate(p/1),
		a_predicate_indicator(A),
		Object::abolish(A),
		Object::abolish(p/1).

	% tests for the "user" pseudo-object

	test(abolish_1_20, true(\+ {current_predicate(bar/1)})) :-
		user::abolish(bar/1).

	test(abolish_1_21, true(\+ {current_predicate(baz/1)})) :-
		baz_predicate_indicator(Baz),
		user::abolish(Baz).

	test(abolish_1_22, true(\+ {current_predicate(foobar/1)})) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::abolish(foobar/1).

	test(abolish_1_23, true(\+ {current_predicate(foobaz/1)})) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		foobaz_predicate_indicator(FooBaz),
		Object::abolish(FooBaz).

	test(abolish_1_24, true(\+ Object::current_predicate(p/1)), [cleanup(abolish_object(Object))]) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations, allow)], []),
		closure(Closure),
		Object::assertz(p(1)),
		call(Object::Closure, p/1).

	% auxiliary predicates

	a_predicate_indicator(a/1).

	baz_predicate_indicator(baz/1).

	foobaz_predicate_indicator(foobaz/1).

	user_object(user).

	closure(abolish).

:- end_object.
