%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- object(context_switch_test_object).

	:- set_logtalk_flag(context_switching_calls, deny).

	p.

:- end_object.


:- object(context_switch_test_object(_)).

	:- set_logtalk_flag(context_switching_calls, allow).

	p(X) :-
		parameter(1, X).

:- end_object.


context_switch_test_object(1).
context_switch_test_object(2).
context_switch_test_object(3).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2018-03-26,
		comment is 'Unit tests for the (<<)/2 built-in control construct.'
	]).

	throws(context_switch_2_01, error(instantiation_error, logtalk(_<<goal,_))) :-
		% delay the error to runtime
		{_ << goal}.

	throws(context_switch_2_02, error(instantiation_error, logtalk(logtalk<<_,_))) :-
		% delay the error to runtime
		{logtalk << _}.

	throws(context_switch_2_03, error(type_error(object_identifier, 3), logtalk(3<<goal,_))) :-
		% delay the error to runtime
		{3 << goal}.

	throws(context_switch_2_04, error(type_error(callable, 3), logtalk(object<<3,_))) :-
		% delay the error to runtime
		{object << 3}.

	throws(context_switch_2_05, error(existence_error(procedure, goal/0), logtalk(This<<goal,_))) :-
		this(This),
		% delay the error to runtime
		{This << goal}.

	throws(context_switch_2_06, error(existence_error(object, foo), logtalk(foo<<goal,_))) :-
		% delay the error to runtime
		{foo << goal}.

	throws(context_switch_2_07, error(permission_error(access, database, p), logtalk(context_switch_test_object<<p,_))) :-
		% delay the error to runtime
		{context_switch_test_object << p}.

	succeeds(context_switch_2_08) :-
		% delay the error to runtime
		{user << true}.

	succeeds(context_switch_2_09) :-
		% delay the error to runtime
		{logtalk << true}.

	succeeds(context_switch_2_10) :-
		this(This),
		findall(X, {This << a(X)}, Xs),
		Xs == [1,2,3].

	succeeds(context_switch_2_11) :-
		create_object(Object, [], [], [a(1),a(2),a(3)]),
		findall(X, {Object << a(X)}, Xs),
		Xs == [1,2,3],
		abolish_object(Object).

	succeeds(context_switch_2_12) :-
		findall(X, {context_switch_test_object(_)}<<p(X), Xs),
		Xs == [1,2,3].

	fails(context_switch_2_13) :-
		{user << fail}.

	fails(context_switch_2_14) :-
		{logtalk << fail}.

	fails(context_switch_2_15) :-
		this(This),
		{This << a(4)}.

	fails(context_switch_2_16) :-
		{context_switch_test_object(_)}<<p(4).

	a(1). a(2). a(3).

:- end_object.
