%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		comment is 'Unit tests for the asserta/1 built-in method.'
	]).

	throws(asserta_1_1, error(instantiation_error, logtalk(test_object::asserta(_),user))) :-
		{test_object::asserta(_)}.

	throws(asserta_1_2, error(instantiation_error, logtalk(test_object::asserta((_:-_)),user))) :-
		{test_object::asserta((_ :- _))}.

	throws(asserta_1_3, error(type_error(callable, 1), logtalk(test_object::asserta(1),user))) :-
		{test_object::asserta(1)}.

	throws(asserta_1_4, error(type_error(callable, 1), logtalk(test_object::asserta((1:-_)),user))) :-
		{test_object::asserta((1 :- _))}.

	throws(asserta_1_5, error(type_error(callable, 1), logtalk(test_object::asserta((p:-1)),user))) :-
		{test_object::asserta((p :- 1))}.

	throws(asserta_1_6, error(permission_error(modify, protected_predicate, q/2), logtalk(test_object::asserta(q(_,_)),user))) :-
		{test_object::asserta(q(_,_))}.

	throws(asserta_1_7, error(permission_error(modify, protected_predicate, q/2), logtalk(test_object::asserta((q(_,_) :-nl)),user))) :-
		{test_object::asserta((q(_,_) :- nl))}.

	throws(asserta_1_8, error(permission_error(modify, private_predicate, r/3), logtalk(test_object::asserta(r(_,_,_)),user))) :-
		{test_object::asserta(r(_,_,_))}.

	throws(asserta_1_9, error(permission_error(modify, private_predicate, r/3), logtalk(test_object::asserta((r(_,_,_) :-nl)),user))) :-
		{test_object::asserta((r(_,_,_) :- nl))}.

	throws(asserta_1_10, error(permission_error(modify, static_predicate, s/4), logtalk(test_object::asserta(s(_,_,_,_)),user))) :-
		{test_object::asserta(s(_,_,_,_))}.

	throws(asserta_1_11, error(permission_error(modify, static_predicate, s/4), logtalk(test_object::asserta((s(_,_,_,_) :-nl)),user))) :-
		{test_object::asserta((s(_,_,_,_) :- nl))}.

	throws(asserta_1_12, error(permission_error(create, predicate_declaration, new/0), logtalk(test_object::asserta(new),user))) :-
		{test_object::asserta(new)}.

	throws(asserta_1_13, error(instantiation_error, logtalk(_::asserta(foo),test_object))) :-
		{test_object::ie(_)}.

	throws(asserta_1_14, error(type_error(object_identifier, 1), logtalk(1::asserta(foo),test_object))) :-
		{test_object::te}.

	succeeds(asserta_1_15) :-
		create_object(Object, [], [public(a/1), public(p/1)], []),
		Object::asserta(a(1)),
		a2_clause(A2),
		Object::asserta(A2),
		Object::asserta(a(3)),
		Object::asserta((p(X) :- a(X))),
		findall(X, Object::a(X), Xs),
		Xs == [3,2,1],
		findall(Y, Object::p(Y), Ys),
		Ys == [3,2,1],
		abolish_object(Object).

	succeeds(asserta_1_16) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations, allow)], []),
		Object::asserta(a(1)),
		a2_clause(A2),
		Object::asserta(A2),
		Object::asserta(a(3)),
		Object::asserta((p(X) :- a(X))),
		findall(X, Object::a(X), Xs),
		Xs == [3,2,1],
		findall(Y, Object::p(Y), Ys),
		Ys == [3,2,1],
		abolish_object(Object).

	% tests for the "user" pseudo-object

	succeeds(asserta_1_17) :-
		user::asserta(bar),
		{bar}.

	succeeds(asserta_1_18) :-
		baz_clause(Baz),
		user::asserta(Baz),
		{Baz}.

	succeeds(asserta_1_19) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::asserta(foobar),
		{foobar}.

	succeeds(asserta_1_20) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		foobaz_clause(FooBaz),
		Object::asserta(FooBaz),
		{FooBaz}.

	cleanup :-
		{abolish(bar/0), abolish(baz/0), abolish(foobar/0), abolish(foobaz/0)}.

	% auxiliary predicates

	a2_clause(a(2)).

	baz_clause(baz).

	foobaz_clause(foobaz).

	user_object(user).

:- end_object.
