%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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

a(1).

b(1, 2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/11/10,
		comment is 'Unit tests for the current_predicate/1 built-in method.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	throws(current_predicate_1_01, error(type_error(predicate_indicator, 1), logtalk(This::current_predicate(1),user))) :-
		this(This),
		{This::current_predicate(1)}.

	throws(current_predicate_1_02, error(type_error(atom,1), logtalk(This::current_predicate(1/b),user))) :-
		this(This),
		{This::current_predicate(1/b)}.

	throws(current_predicate_1_03, error(type_error(integer,b), logtalk(This::current_predicate(a/b),user))) :-
		this(This),
		{This::current_predicate(a/b)}.

	throws(current_predicate_1_04, error(domain_error(not_less_than_zero, -1), logtalk(This::current_predicate(a/(-1)),user))) :-
		this(This),
		{This::current_predicate(a/(-1))}.

	throws(current_predicate_1_05, error(instantiation_error, logtalk(_::current_predicate(foo/1),test_object))) :-
		{test_object::ie(_)}.

	throws(current_predicate_1_06, error(type_error(object_identifier, 1), logtalk(1::current_predicate(foo/1),test_object))) :-
		{test_object::te}.

	succeeds(current_predicate_1_07) :-
		test_object::current_predicate(ie/1).

	succeeds(current_predicate_1_08) :-
		test_object::current_predicate(te/0).

	succeeds(current_predicate_1_09) :-
		setof(Predicate, test_object::current_predicate(Predicate), Predicates),
		Predicates == [ie/1, te/0].

	fails(current_predicate_1_10) :-
		test_object::current_predicate(foo/2).

	% tests for the "user" pseudo-object

	succeeds(current_predicate_1_11) :-
		user::current_predicate(a/1).

	succeeds(current_predicate_1_12) :-
		user::current_predicate(b/Arity),
		Arity == 2.

	succeeds(current_predicate_1_13) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::current_predicate(a/1).

	succeeds(current_predicate_1_14) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::current_predicate(b/Arity),
		Arity == 2.

	% test semantics for local calls from multifile predicate clauses

	succeeds(current_predicate_1_15) :-
		setof(Predicate, primary::p(Predicate), Predicates),
		Predicates == [a/1, b/2, c/3].

	% test semantics for re-declared predicates

	succeeds(current_predicate_1_16) :-
		findall(Predicate, proto::current_predicate(Predicate), Predicates),
		Predicates == [foobar/0].

	fails(current_predicate_1_17) :-
		proto::current_predicate(foo/_).

	fails(current_predicate_1_18) :-
		proto::current_predicate(_/1).

	fails(current_predicate_1_19) :-
		proto::current_predicate(foo/1).

	fails(current_predicate_1_20) :-
		proto::current_predicate(bar/_).

	fails(current_predicate_1_21) :-
		proto::current_predicate(_/2).

	fails(current_predicate_1_22) :-
		proto::current_predicate(bar/2).

	% auxiliary predicates

	user_object(user).

:- end_object.
