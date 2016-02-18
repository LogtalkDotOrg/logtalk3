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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/11/12,
		comment is 'Unit tests for the predicate_property/2 built-in method.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	throws(predicate_property_2_01, error(instantiation_error,logtalk(This::predicate_property(_,_),user))) :-
		this(This),
		{This::predicate_property(_, _)}.

	throws(predicate_property_2_02, error(type_error(callable, 1),logtalk(This::predicate_property(1, _),user))) :-
		this(This),
		{This::predicate_property(1, _)}.

	throws(predicate_property_2_03, error(domain_error(predicate_property, bar),logtalk(This::predicate_property(foo, bar),user))) :-
		this(This),
		{This::predicate_property(foo, bar)}.

	throws(predicate_property_2_04, error(instantiation_error, logtalk(_::predicate_property(foo,_),test_object))) :-
		{test_object::ie(_)}.

	throws(predicate_property_2_05, error(type_error(object_identifier, 1), logtalk(1::predicate_property(foo,_),test_object))) :-
		{test_object::te}.

	% Prolog built-in predicates are interpreted as private predicates

	succeeds(predicate_property_2_06) :-
		predicate_property(write(_), scope(Scope)),
		Scope == private.

	succeeds(predicate_property_2_07) :-
		this(This),
		This::predicate_property(write(_), scope(Scope)),
		Scope == private.

	fails(predicate_property_2_08) :-
		this(This),
		{This::predicate_property(write(_), _)}.

	% test properties of a user-defined predicate

	succeeds(predicate_property_2_09) :-
		test_object::predicate_property(ie(_), scope(Scope)),
		Scope == (public).

	succeeds(predicate_property_2_10) :-
		test_object::predicate_property(ie(_), static).

	succeeds(predicate_property_2_11) :-
		test_object::predicate_property(ie(_), logtalk).

	fails(predicate_property_2_12) :-
		test_object::predicate_property(ie(_), prolog).

	fails(predicate_property_2_13) :-
		test_object::predicate_property(ie(_), (dynamic)).

	fails(predicate_property_2_14) :-
		test_object::predicate_property(ie(_), meta_predicate(_)).

	fails(predicate_property_2_15) :-
		test_object::predicate_property(ie(_), non_terminal(_)).

	fails(predicate_property_2_16) :-
		test_object::predicate_property(ie(_), (multifile)).

	fails(predicate_property_2_17) :-
		test_object::predicate_property(ie(_), built_in).

	fails(predicate_property_2_18) :-
		test_object::predicate_property(ie(_), synchronized).

	% test properties of a user-defined meta-predicate

	succeeds(predicate_property_2_19) :-
		test_object::predicate_property(meta(_,_), meta_predicate(Template)),
		Template == meta(0, *).

	% test properties of a user-defined non-terminal

	succeeds(predicate_property_2_20) :-
		test_object::predicate_property(nt(_,_), non_terminal(NonTerminal)),
		NonTerminal == nt//0.

	% tests for the "user" pseudo-object

	succeeds(predicate_property_2_21) :-
		user::predicate_property(a(_), _).

	succeeds(predicate_property_2_22) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::predicate_property(a(_), _).

	% test semantics for local calls from multifile predicate clauses

	succeeds(predicate_property_2_23) :-
		primary::p(a(_), scope(Scope)),
		primary::p(a(_), declared_in(Object)),
		primary::p(a(_), defined_in(Object)),
		Scope == (private), Object == secondary.

	succeeds(predicate_property_2_24) :-
		primary::p(b(_,_), scope(Scope)),
		primary::p(b(_,_), declared_in(Object)),
		primary::p(b(_,_), defined_in(Object)),
		Scope == protected, Object == secondary.

	% auxiliary predicates

	user_object(user).

:- end_object.
