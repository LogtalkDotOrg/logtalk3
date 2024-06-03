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
		version is 1:1:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2024-01-15,
		comment is 'Unit tests for the "threads/nondet" example.'
	]).

	:- threaded.

	test(nondet_01, true) :-
		threaded_call(lists::member(_, [1,2,3])).

	test(nondet_02, true(Xs == [1, 2, 3])) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs).

	test(nondet_03, true) :-
		threaded_once(lists::member(_, [1,2,3])).

	test(nondet_04, true(Xs == [1])) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs).

	test(nondet_05, true) :-
		threaded_call(lists::member(_, [1,2,3])),
		threaded_call(lists::member(_, [1,2,3])).

	test(nondet_06, true(Xs == [1, 2, 3])) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs).

	test(nondet_07, true(Xs == [1, 2, 3])) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs).

	test(nondet_08, true(Xs == [1, 2, 3])) :-
		threaded_call(lists::member(_, [1,2,3]), _),
		threaded_call(lists::member(_, [1,2,3]), Tag),
		findall(X, threaded_exit(lists::member(X, [1,2,3]), Tag), Xs).

	test(nondet_09, true) :-
		threaded_call(lists::member(_, [1,2,3,2])).

	test(nondet_10, true(L == [1, 1])) :-
		findall(1, threaded_exit(lists::member(2, [1,2,3,2])), L).

:- end_object.
