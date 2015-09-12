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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/04/16,
		comment is 'Unit tests for the abolish/1 built-in method.'
	]).

	throws(abolish_1_1, error(instantiation_error, logtalk(test_object::abolish(_),user))) :-
		{test_object::abolish(_)}.

	throws(abolish_1_2, error(instantiation_error, logtalk(test_object::abolish(_/1),user))) :-
		{test_object::abolish(_/1)}.

	throws(abolish_1_3, error(instantiation_error, logtalk(test_object::abolish(foo/_),user))) :-
		{test_object::abolish(foo/_)}.

	throws(abolish_1_4, error(instantiation_error, logtalk(test_object::abolish(foo/_),user))) :-
		{test_object::abolish(foo/_)}.

	throws(abolish_1_5, error(type_error(predicate_indicator, foo), logtalk(test_object::abolish(foo),user))) :-
		{test_object::abolish(foo)}.

	throws(abolish_1_6, error(type_error(atom, 1), logtalk(test_object::abolish(1/2),user))) :-
		{test_object::abolish(1/2)}.

	throws(abolish_1_7, error(type_error(integer, bar), logtalk(test_object::abolish(foo/bar),user))) :-
		{test_object::abolish(foo/bar)}.

	throws(abolish_1_8, error(permission_error(modify, predicate_declaration, p/1), logtalk(test_object::abolish(p/1),user))) :-
		{test_object::abolish(p/1)}.

	throws(abolish_1_9, error(permission_error(modify, protected_predicate, q/2), logtalk(test_object::abolish(q/2),user))) :-
		{test_object::abolish(q/2)}.

	throws(abolish_1_10, error(permission_error(modify, private_predicate, r/3), logtalk(test_object::abolish(r/3),user))) :-
		{test_object::abolish(r/3)}.

	throws(abolish_1_11, error(permission_error(modify, static_predicate, s/4), logtalk(test_object::abolish(s/4),user))) :-
		{test_object::abolish(s/4)}.

	throws(abolish_1_12, error(existence_error(predicate_declaration, foo/0), logtalk(test_object::abolish(foo/0),user))) :-
		{test_object::abolish(foo/0)}.

	throws(abolish_1_13, error(instantiation_error, logtalk(_::abolish(foo/1),test_object))) :-
		{test_object::ie(_)}.

	throws(abolish_1_14, error(type_error(object_identifier, 1), logtalk(1::abolish(foo/1),test_object))) :-
		{test_object::te}.

	succeeds(abolish_1_15) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations, allow)], []),
		Object::assertz(a(1)),
		Object::current_predicate(a/1),
		Object::assertz((p(X) :- a(X))),
		Object::current_predicate(p/1),
		Object::abolish(a/1),
		\+ Object::current_predicate(a/1),
		Object::abolish(p/1),
		\+ Object::current_predicate(p/1),
		abolish_object(Object).

:- end_object.
