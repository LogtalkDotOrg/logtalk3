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
		version is 1.4,
		author is 'Paulo Moura',
		date is 2018-07-25,
		comment is 'Unit tests for the abolish/1 built-in method.'
	]).

	throws(abolish_1_01, error(instantiation_error, logtalk(abolish(_),_))) :-
		{test_object::abolish(_)}.

	throws(abolish_1_02, error(instantiation_error, logtalk(abolish(_/1),_))) :-
		{test_object::abolish(_/1)}.

	throws(abolish_1_03, error(instantiation_error, logtalk(abolish(foo/_),_))) :-
		{test_object::abolish(foo/_)}.

	throws(abolish_1_04, error(instantiation_error, logtalk(abolish(foo/_),_))) :-
		{test_object::abolish(foo/_)}.

	throws(abolish_1_05, error(type_error(predicate_indicator, foo), logtalk(test_object::abolish(foo),_))) :-
		{test_object::abolish(foo)}.

	throws(abolish_1_06, error(type_error(atom, 1), logtalk(test_object::abolish(1/2),_))) :-
		{test_object::abolish(1/2)}.

	throws(abolish_1_07, error(type_error(integer, bar), logtalk(test_object::abolish(foo/bar),_))) :-
		{test_object::abolish(foo/bar)}.

	throws(abolish_1_08, error(permission_error(modify, predicate_declaration, p/1), logtalk(abolish(p/1),_))) :-
		{test_object::abolish(p/1)}.

	throws(abolish_1_09, error(permission_error(modify, protected_predicate, q/2), logtalk(abolish(q/2),_))) :-
		{test_object::abolish(q/2)}.

	throws(abolish_1_10, error(permission_error(modify, private_predicate, r/3), logtalk(abolish(r/3),_))) :-
		{test_object::abolish(r/3)}.

	throws(abolish_1_11, error(permission_error(modify, static_predicate, s/4), logtalk(abolish(s/4),_))) :-
		{test_object::abolish(s/4)}.

	throws(abolish_1_12, error(existence_error(predicate_declaration, foo/0), logtalk(abolish(foo/0),_))) :-
		{test_object::abolish(foo/0)}.

	throws(abolish_1_13, error(existence_error(predicate_declaration, p/1), logtalk(_::p(_),_))) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
		Object::assertz(p(1)),
		% clauses exist before abolishing
		Object::abolish(p/1),
		Object::p(_).

	throws(abolish_1_14, error(existence_error(predicate_declaration, p/1), logtalk(_::p(_),_))) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
		Object::assertz(p(1)),
		Object::retract(p(_)),
		% clauses don't exist before abolishing
		Object::abolish(p/1),
		Object::p(_).

	throws(abolish_1_15, error(existence_error(procedure, p/1), logtalk(_<<p(_),_))) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
		Object::assertz(p(1)),
		% clauses exist before abolishing
		Object::abolish(p/1),
		Object<<p(_).

	throws(abolish_1_16, error(existence_error(procedure, p/1), logtalk(_<<p(_),_))) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
		Object::assertz(p(1)),
		Object::retract(p(_)),
		% clauses don't exist before abolishing
		Object::abolish(p/1),
		Object<<p(_).

	throws(abolish_1_17, error(instantiation_error, logtalk(_::abolish(foo/1),_))) :-
		{test_object::ie(_)}.

	throws(abolish_1_18, error(type_error(object_identifier, 1), logtalk(1::abolish(foo/1),_))) :-
		{test_object::te}.

	succeeds(abolish_1_19) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations, allow)], []),
		Object::assertz(a(1)),
		Object::current_predicate(a/1),
		Object::assertz((p(X) :- a(X))),
		Object::current_predicate(p/1),
		a_predicate_indicator(A),
		Object::abolish(A),
		\+ Object::current_predicate(a/1),
		Object::abolish(p/1),
		\+ Object::current_predicate(p/1),
		abolish_object(Object).

	% tests for the "user" pseudo-object

	succeeds(abolish_1_20) :-
		user::abolish(bar/1),
		\+ {current_predicate(bar/1)}.

	succeeds(abolish_1_21) :-
		baz_predicate_indicator(Baz),
		user::abolish(Baz),
		\+ {current_predicate(baz/1)}.

	succeeds(abolish_1_22) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::abolish(foobar/1),
		\+ {current_predicate(foobar/1)}.

	succeeds(abolish_1_23) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		foobaz_predicate_indicator(FooBaz),
		Object::abolish(FooBaz),
		\+ {current_predicate(foobaz/1)}.

	% auxiliary predicates

	a_predicate_indicator(a/1).

	baz_predicate_indicator(baz/1).

	foobaz_predicate_indicator(foobaz/1).

	user_object(user).

:- end_object.
