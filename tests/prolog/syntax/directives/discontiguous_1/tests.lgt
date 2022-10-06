%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


% test all possible syntaxes for the directive
:- discontiguous(a/0).
:- discontiguous((b/1, c/2)).
:- discontiguous([d/3, e/4]).

a.
b(1).
a.
c(1, 2).
b(2).
d(1, 2, 3).
c(3, 4).
a.
e(1, 2, 3, 4).
d(4, 5, 6).
e(5, 6, 7, 8).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2022-10-06,
		comment is 'Unit tests for the ISO Prolog standard discontiguous/1 directive syntax.'
	]).

	test(discontiguous_1_01, true(L == [a, a, a])) :-
		findall(a, {a}, L).

	test(discontiguous_1_02, true(L == [1, 2])) :-
		findall(X, {b(X)}, L).

	test(discontiguous_1_03, true(L == [1-2, 3-4])) :-
		findall(X-Y, {c(X, Y)}, L).

	test(discontiguous_1_04, true(L == [1-2-3, 4-5-6])) :-
		findall(X-Y-Z, {d(X, Y, Z)}, L).

	test(discontiguous_1_05, true(L == [1-2-3-4, 5-6-7-8])) :-
		findall(X-Y-Z-W, {e(X, Y, Z, W)}, L).

:- end_object.
