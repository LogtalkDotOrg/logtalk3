%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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

a(1).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:7:0,
		author is 'Paulo Moura',
		date is 2021-11-01,
		comment is 'Unit tests for the predicate_property/2 built-in method.'
	]).

	:- set_logtalk_flag(suspicious_calls, silent).

	test(predicate_property_2_01, ball(error(instantiation_error,logtalk(predicate_property(_,_),_)))) :-
		this(This),
		{This::predicate_property(_, _)}.

	test(predicate_property_2_02, ball(error(type_error(callable, 1),logtalk(This::predicate_property(1, _),_)))) :-
		this(This),
		{This::predicate_property(1, _)}.

	test(predicate_property_2_03, ball(error(domain_error(predicate_property, bar),logtalk(This::predicate_property(foo, bar),_)))) :-
		this(This),
		{This::predicate_property(foo, bar)}.

	test(predicate_property_2_04, ball(error(instantiation_error, logtalk(_::predicate_property(foo,_),_)))) :-
		{test_object_1::ie(_)}.

	test(predicate_property_2_05, ball(error(type_error(object_identifier, 1), logtalk(1::predicate_property(foo,_),_)))) :-
		{test_object_1::te}.

	% Prolog built-in predicates are interpreted as private predicates

	test(predicate_property_2_06, true(Scope == private)) :-
		predicate_property(write(_), scope(Scope)).

	test(predicate_property_2_07, true(Scope == private)) :-
		this(This),
		This::predicate_property(write(_), scope(Scope)).

	test(predicate_property_2_08, false) :-
		this(This),
		{This::predicate_property(write(_), _)}.

	% test properties of a user-defined predicate

	test(predicate_property_2_09, true(Scope == (public))) :-
		test_object_1::predicate_property(ie(_), scope(Scope)).

	test(predicate_property_2_10, true) :-
		test_object_1::predicate_property(ie(_), static).

	test(predicate_property_2_11, true) :-
		test_object_1::predicate_property(ie(_), logtalk).

	test(predicate_property_2_12, false) :-
		test_object_1::predicate_property(ie(_), prolog).

	test(predicate_property_2_13, false) :-
		test_object_1::predicate_property(ie(_), (dynamic)).

	test(predicate_property_2_14, false) :-
		test_object_1::predicate_property(ie(_), meta_predicate(_)).

	test(predicate_property_2_15, false) :-
		test_object_1::predicate_property(ie(_), non_terminal(_)).

	test(predicate_property_2_16, false) :-
		test_object_1::predicate_property(ie(_), (multifile)).

	test(predicate_property_2_17, false) :-
		test_object_1::predicate_property(ie(_), built_in).

	test(predicate_property_2_18, false) :-
		test_object_1::predicate_property(ie(_), synchronized).

	test(predicate_property_2_19, true(N == 1)) :-
		test_object_1::predicate_property(ie(_), number_of_clauses(N)).

	test(predicate_property_2_20, true(N == 1)) :-
		test_object_1::predicate_property(ie(_), number_of_rules(N)).

	% test properties of a user-defined meta-predicate

	test(predicate_property_2_21, true(Template == meta(0, *))) :-
		test_object_1::predicate_property(meta(_,_), meta_predicate(Template)).

	test(predicate_property_2_22, true(N == 0)) :-
		test_object_1::predicate_property(meta(_,_), number_of_clauses(N)).

	test(predicate_property_2_23, true(N == 0)) :-
		test_object_1::predicate_property(meta(_,_), number_of_rules(N)).

	% test properties of a user-defined non-terminal

	test(predicate_property_2_24, true(NonTerminal == nt//0)) :-
		test_object_1::predicate_property(nt(_,_), non_terminal(NonTerminal)).

	test(predicate_property_2_25, true(N == 0)) :-
		test_object_1::predicate_property(nt(_,_), number_of_clauses(N)).

	test(predicate_property_2_26, true(N == 0)) :-
		test_object_1::predicate_property(nt(_,_), number_of_rules(N)).

	% tests for the "user" pseudo-object

	test(predicate_property_2_27, true) :-
		user::predicate_property(a(_), _).

	test(predicate_property_2_28, true) :-
		% ensure that the unification is not optimized away
		user_object(Object),
		Object::predicate_property(a(_), _).

	% test semantics for local calls from multifile predicate clauses

	test(predicate_property_2_29, true(Scope-Object == (private)-secondary)) :-
		primary::p(a(_), scope(Scope)),
		primary::p(a(_), declared_in(Object)),
		primary::p(a(_), defined_in(Object)).

	test(predicate_property_2_30, true(Scope-Object == protected-secondary)) :-
		primary::p(b(_,_), scope(Scope)),
		primary::p(b(_,_), declared_in(Object)),
		primary::p(b(_,_), defined_in(Object)).

	% tests for predicate declaration and definition term position properties

	test(predicate_property_2_31, true(Object-Line == test_object_2-47)) :-
		test_object_2::predicate_property(foo(_), declared_in(Object, Line)).

	test(predicate_property_2_32, true(Object-Line == test_object_2-48)) :-
		test_object_2::predicate_property(foo(_), defined_in(Object, Line)).

	test(predicate_property_2_33, true(Object-Line == test_object_2-50)) :-
		test_object_2::predicate_property(bar, declared_in(Object, Line)).

	test(predicate_property_2_34, true(Object-Line == test_object_2-51)) :-
		test_object_2::predicate_property(bar, defined_in(Object, Line)).

	% auxiliary predicates

	user_object(user).

:- end_object.
