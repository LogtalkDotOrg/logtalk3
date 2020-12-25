%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2018-07-25,
		comment is 'Unit tests for the retract/1 built-in method.'
	]).

	throws(retract_1_01, error(instantiation_error, logtalk(retract(_),_))) :-
		{test_object::retract(_)}.

	throws(retract_1_02, error(instantiation_error, logtalk(retract((_:-_)),_))) :-
		{test_object::retract((_ :- _))}.

	throws(retract_1_03, error(type_error(callable, 1), logtalk(test_object::retract(1),_))) :-
		{test_object::retract(1)}.

	throws(retract_1_04, error(type_error(callable, 1), logtalk(test_object::retract((1:-_)),_))) :-
		{test_object::retract((1 :- _))}.

	throws(retract_1_05, error(permission_error(modify, protected_predicate, q/2), logtalk(retract(q(_,_)),_))) :-
		{test_object::retract(q(_,_))}.

	throws(retract_1_06, error(permission_error(modify, private_predicate, r/3), logtalk(retract(r(_,_,_)),_))) :-
		{test_object::retract(r(_,_,_))}.

	throws(retract_1_07, error(permission_error(modify, static_predicate, s/4), logtalk(retract(s(_,_,_,_)),_))) :-
		{test_object::retract(s(_,_,_,_))}.

	throws(retract_1_08, error(existence_error(predicate_declaration, unknown/1), logtalk(retract(unknown(_)),_))) :-
		{test_object::retract(unknown(_))}.

	throws(retract_1_09, error(existence_error(predicate_declaration, (local)/1), logtalk(retract(local(_)),_))) :-
		{test_object::retract(local(_))}.

	throws(retract_1_10, error(instantiation_error, logtalk(_::retract(foo),_))) :-
		{test_object::ie(_)}.

	throws(retract_1_11, error(type_error(object_identifier, 1), logtalk(1::retract(foo),_))) :-
		{test_object::te}.

	succeeds(retract_1_12) :-
		test_object::retract((t(X) :-true)),
		X == 1,
		t2_head(T2),
		test_object::retract((T2 :-Body1)),
		Body1 == t(1),
		test_object::retract((t(3) :-Body2)),
		Body2 == (t(1), t(2)).

	succeeds(retract_1_13) :-
		create_object(Object, [], [public(t/1), dynamic(t/1)], [t(1), (t(2) :-t(1)), (t(3) :-t(1),t(2))]),
		Object::retract((t(X) :-true)),
		X == 1,
		t2_head(T2),
		Object::retract((T2 :-Body1)),
		Body1 == t(1),
		Object::retract((t(3) :-Body2)),
		Body2 == (t(1), t(2)),
		abolish_object(Object).

	% tests for the "user" pseudo-object

	succeeds(retract_1_14) :-
		user::retract(bar(X)),
		X == 1.

	succeeds(retract_1_15) :-
		bar_clause(Baz),
		user::retract(Baz).

	succeeds(retract_1_16) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::retract(baz(X)),
		X == 1.

	succeeds(retract_1_17) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		baz_clause(Baz),
		Object::retract(Baz).

	% auxiliary predicates

	t2_head(t(2)).

	bar_clause(bar(2)).

	baz_clause(baz(2)).

	user_object(user).

:- end_object.
