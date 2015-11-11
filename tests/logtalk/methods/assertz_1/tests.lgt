%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/11/11,
		comment is 'Unit tests for the assertz/1 built-in method.'
	]).

	throws(assertz_1_1, error(instantiation_error, logtalk(test_object::assertz(_),user))) :-
		{test_object::assertz(_)}.

	throws(assertz_1_2, error(instantiation_error, logtalk(test_object::assertz((_:-_)),user))) :-
		{test_object::assertz((_ :- _))}.

	throws(assertz_1_3, error(type_error(callable, 1), logtalk(test_object::assertz(1),user))) :-
		{test_object::assertz(1)}.

	throws(assertz_1_4, error(type_error(callable, 1), logtalk(test_object::assertz((1:-_)),user))) :-
		{test_object::assertz((1 :- _))}.

	throws(assertz_1_5, error(type_error(callable, 1), logtalk(test_object::assertz((p:-1)),user))) :-
		{test_object::assertz((p :- 1))}.

	throws(assertz_1_6, error(permission_error(modify, protected_predicate, q/2), logtalk(test_object::assertz(q(_,_)),user))) :-
		{test_object::assertz(q(_,_))}.

	throws(assertz_1_7, error(permission_error(modify, protected_predicate, q/2), logtalk(test_object::assertz((q(_,_) :-nl)),user))) :-
		{test_object::assertz((q(_,_) :- nl))}.

	throws(assertz_1_8, error(permission_error(modify, private_predicate, r/3), logtalk(test_object::assertz(r(_,_,_)),user))) :-
		{test_object::assertz(r(_,_,_))}.

	throws(assertz_1_9, error(permission_error(modify, private_predicate, r/3), logtalk(test_object::assertz((r(_,_,_) :-nl)),user))) :-
		{test_object::assertz((r(_,_,_) :- nl))}.

	throws(assertz_1_10, error(permission_error(modify, static_predicate, s/4), logtalk(test_object::assertz(s(_,_,_,_)),user))) :-
		{test_object::assertz(s(_,_,_,_))}.

	throws(assertz_1_11, error(permission_error(modify, static_predicate, s/4), logtalk(test_object::assertz((s(_,_,_,_) :-nl)),user))) :-
		{test_object::assertz((s(_,_,_,_) :- nl))}.

	throws(assertz_1_12, error(permission_error(create, predicate_declaration, new/0), logtalk(test_object::assertz(new),user))) :-
		{test_object::assertz(new)}.

	throws(assertz_1_13, error(instantiation_error, logtalk(_::assertz(foo),test_object))) :-
		{test_object::ie(_)}.

	throws(assertz_1_14, error(type_error(object_identifier, 1), logtalk(1::assertz(foo),test_object))) :-
		{test_object::te}.

	succeeds(assertz_1_15) :-
		create_object(Object, [], [public(a/1), public(p/1)], []),
		Object::assertz(a(1)),
		a2_clause(A2),
		Object::assertz(A2),
		Object::assertz(a(3)),
		Object::assertz((p(X) :- a(X))),
		findall(X, Object::a(X), Xs),
		Xs == [1,2,3],
		findall(Y, Object::p(Y), Ys),
		Ys == [1,2,3],
		abolish_object(Object).

	succeeds(assertz_1_16) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations, allow)], []),
		Object::assertz(a(1)),
		a2_clause(A2),
		Object::assertz(A2),
		Object::assertz(a(3)),
		Object::assertz((p(X) :- a(X))),
		findall(X, Object::a(X), Xs),
		Xs == [1,2,3],
		findall(Y, Object::p(Y), Ys),
		Ys == [1,2,3],
		abolish_object(Object).

	% tests for the "user" pseudo-object

	succeeds(assertz_1_17) :-
		user::assertz(bar),
		{bar}.

	succeeds(assertz_1_18) :-
		baz_clause(Baz),
		user::assertz(Baz),
		{Baz}.

	succeeds(assertz_1_19) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::assertz(foobar),
		{foobar}.

	succeeds(assertz_1_20) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		foobaz_clause(FooBaz),
		Object::assertz(FooBaz),
		{FooBaz}.

	cleanup :-
		{abolish(bar/0), abolish(baz/0), abolish(foobar/0), abolish(foobaz/0)}.

	% auxiliary predicates

	a2_clause(a(2)).

	baz_clause(baz).

	foobaz_clause(foobaz).

	user_object(user).

:- end_object.
