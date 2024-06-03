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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2024-01-25,
		comment is 'Unit tests for the threaded/1 built-in predicate.'
	]).

	:- threaded.

	test(threaded_1_01, error(instantiation_error)) :-
		{threaded(_)}.

	test(threaded_1_02, error(type_error(callable, 1))) :-
		{threaded(1)}.

	test(threaded_1_03, true) :-
		{threaded((true, true, true))}.

	test(threaded_1_04, true) :-
		{threaded((fail; true; fail))}.

	test(threaded_1_05, false) :-
		{threaded((true, fail, true))}.

	test(threaded_1_06, false) :-
		{threaded((fail; fail; fail))}.

	test(threaded_1_07, ball(thing)) :-
		{threaded((true, throw(thing), true))}.

	test(threaded_1_08, true) :-
		{threaded((throw(err); fail; true))}.

	test(threaded_1_09, ball(_)) :-
		{threaded((throw(err1); throw(err2); throw(err3)))}.

	test(threaded_1_10, ball(_)) :-
		{threaded((throw(err1); fail; fail))}.

	test(threaded_1_11, deterministic(s(A,B,C) == s(1,2,3))) :-
		{test_object::p((a(A), b(B), c(C)))}.

	test(threaded_1_12, deterministic((X==1; X==2; X==3))) :-
		{test_object::p((a(X); b(X); c(X)))}.

	test(threaded_1_13, deterministic(s(A,B,C,D) == s(1,2,3,4))) :-
		{test_object::p((a(A,B), b(B,C), c(C,D)))}.

	test(threaded_1_14, deterministic(s(A,B,C,D) == s(1,2,3,4))) :-
		{test_object::p((a(A,B), test_object::p((b(B,C), c(C,D)))))}.

	test(threaded_1_15, deterministic((X==1; X==2; X==3))) :-
		{test_object::p((test_object::p((a(X); b(X))); c(X)))}.

:- end_object.
