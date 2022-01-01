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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2021-05-12,
		comment is 'Unit tests for the de facto Prolog standard findall/4 built-in predicate.'
	]).

	test(commons_findall_4_01, true(S == [1,2,3])) :-
		{findall(X, (X=1;X=2), S, [3])}.

	test(commons_findall_4_02, variant(S, [1+_|_])) :-
		{findall(X+_Y, (X=1), S, _)}.

	test(commons_findall_4_03, true(L == [0])) :-
		{findall(_X, fail, L, [0])}.

	test(commons_findall_4_04, true(S == [1,1])) :-
		{findall(X, (X=1;X=1), S, [])}.

	test(commons_findall_4_05, false) :-
		{findall(X, (X=2;X=1), [1,2], _)}.

	test(commons_findall_4_06, true) :-
		{findall(X, (X=1;X=2), [X,Y,3], T)},
		^^assertion(x(X), X == 1),
		^^assertion(y(Y), Y == 2),
		^^assertion(t(T), T == [3]).

	test(commons_findall_4_07, true(L == [1,2,3])) :-
		{findall(X, (X=1;X=2), L, [Y]), Y = 3}.

	test(commons_findall_4_08, error(instantiation_error)) :-
		{findall(_X, _Goal, _S, _T)}.

	test(commons_findall_4_09, error(type_error(callable,4))) :-
		% try to delay the error to runtime
		four(Four),
		{findall(_X, Four, _S, _T)}.

	test(commons_findall_4_10, error(type_error(list,[A|1]))) :-
		{findall(X, X=1, [A|1], _)}.

	test(commons_findall_4_11, error(type_error(list,12))) :-
		{findall(X, (X=2; X=1), 12, [])}.

	test(commons_findall_4_12, error(type_error(list,[A|1]))) :-
		{findall(X, X=1, _, [A|1])}.

	test(commons_findall_4_13, error(type_error(list,12))) :-
		{findall(X, (X=2; X=1), _, 12)}.

	% auxiliary predicates

	four(4).

:- end_object.
