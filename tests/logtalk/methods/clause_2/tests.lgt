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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/11/11,
		comment is 'Unit tests for the clause/2 built-in method.'
	]).

	throws(clause_2_1, error(instantiation_error, logtalk(test_object::clause(_,_),user))) :-
		{test_object::clause(_, _)}.

	throws(clause_2_2, error(type_error(callable, 1), logtalk(test_object::clause(1,_),user))) :-
		{test_object::clause(1, _)}.

	throws(clause_2_3, error(type_error(callable, 1), logtalk(test_object::clause(head,1),user))) :-
		{test_object::clause(head, 1)}.

	throws(clause_2_4, error(permission_error(access, protected_predicate, q/2), logtalk(test_object::clause(q(_,_),_),user))) :-
		{test_object::clause(q(_,_), _)}.

	throws(clause_2_5, error(permission_error(access, private_predicate, r/3), logtalk(test_object::clause(r(_,_,_),_),user))) :-
		{test_object::clause(r(_,_,_), _)}.

	throws(clause_2_6, error(permission_error(access, static_predicate, s/4), logtalk(test_object::clause(s(_,_,_,_),_),user))) :-
		{test_object::clause(s(_,_,_,_), _)}.

	throws(clause_2_7, error(existence_error(predicate_declaration, unknown/1), logtalk(test_object::clause(unknown(_),_),user))) :-
		{test_object::clause(unknown(_), _)}.

	throws(clause_2_8, error(instantiation_error, logtalk(_::clause(foo,_),test_object))) :-
		{test_object::ie(_)}.

	throws(clause_2_9, error(type_error(object_identifier, 1), logtalk(1::clause(foo,_),test_object))) :-
		{test_object::te}.

	succeeds(clause_2_10) :-
		test_object::clause(t(X), true),
		X == 1,
		t2_head(T2),
		test_object::clause(T2, Body1),
		Body1 == t(1),
		test_object::clause(t(3), Body2),
		Body2 == (t(1), t(2)).

	succeeds(clause_2_11) :-
		create_object(Object, [], [public(t/1), dynamic(t/1)], [t(1), (t(2) :- t(1)), (t(3) :- t(1),t(2))]),
		Object::clause(t(X), true),
		X == 1,
		t2_head(T2),
		Object::clause(T2, Body1),
		Body1 == t(1),
		Object::clause(t(3), Body2),
		Body2 == (t(1), t(2)),
		abolish_object(Object).

	% tests for the "user" pseudo-object

	succeeds(clause_2_12) :-
		user::clause(bar, Body),
		Body == true.

	succeeds(clause_2_13) :-
		baz_clause(Baz),
		user::clause(Baz, Body),
		Body == true.

	succeeds(clause_2_14) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::clause(foobar, Body),
		Body == true.

	succeeds(clause_2_15) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		foobaz_clause(FooBaz),
		Object::clause(FooBaz, Body),
		Body == true.

	% auxiliary predicates

	t2_head(t(2)).

	baz_clause(baz).

	foobaz_clause(foobaz).

	user_object(user).

:- end_object.
