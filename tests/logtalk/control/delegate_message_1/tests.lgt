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


:- object(library).

	:- public(my_call/2).
	:- meta_predicate(my_call(1, *)).
	my_call(Closure, Arg) :-
		call(Closure, Arg).

:- end_object.



:- object(delegate_message_test_object_1).

	:- public(s/1).
	s(X) :-
		sender(X).

:- end_object.



:- object(delegate_message_test_object_2).

	:- public(p/1).
	p(X) :-
		delegate_message_test_object_1::s(X).

	:- public(q/1).
	q(X) :-
		[delegate_message_test_object_1::s(X)].

	:- public(r/1).
	r(X) :-
		call([delegate_message_test_object_1::s], X).

	:- public(s/1).
	s(X) :-
		Closure = [delegate_message_test_object_1::s],
		call(Closure, X).

	:- public(t/1).
	t(X) :-
		library::my_call([delegate_message_test_object_1::s], X).

	:- public(u/0).
	u :-
		[user::foo].

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2018/03/26,
		comment is 'Unit tests for the []/1 built-in control construct.'
	]).

	succeeds(delegate_message_1_01) :-
		{delegate_message_test_object_2::p(X)},
		X == delegate_message_test_object_2.

	succeeds(delegate_message_1_02) :-
		{delegate_message_test_object_2::q(X)},
		X == user.

	succeeds(delegate_message_1_03) :-
		{delegate_message_test_object_2::r(X)},
		X == user.

	succeeds(delegate_message_1_04) :-
		{delegate_message_test_object_2::s(X)},
		X == user.

	succeeds(delegate_message_1_05) :-
		{delegate_message_test_object_2::t(X)},
		X == user.

	throws(delegate_message_1_06, error(instantiation_error, logtalk(logtalk<<[_], _))) :-
		% delay the error to runtime
		{logtalk << [_]}.

	throws(delegate_message_1_07, error(type_error(callable, 1), logtalk(logtalk<<[1], _))) :-
		% delay the error to runtime
		{logtalk << [1]}.

	throws(delegate_message_1_08, error(domain_error(message_sending_goal, foo), logtalk(logtalk<<[foo], _))) :-
		% delay the error to runtime
		{logtalk << [foo]}.

	throws(delegate_message_1_09, error(permission_error(access, object, user), logtalk([user::foo], _))) :-
		{delegate_message_test_object_2::u}.

:- end_object.
