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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2021-08-24,
		comment is 'Unit tests for the ISO Prolog standard findall/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.10.1.4

	test(iso_findall_3_01, true(S == [1,2])) :-
		{findall(X, (X=1;X=2), S)}.

	test(iso_findall_3_02, variant(S, [1+_])) :-
		{findall(X+_Y, (X=1), S)}.

	test(iso_findall_3_03, true(L == [])) :-
		{findall(_X, fail, L)}.

	test(iso_findall_3_04, true(S == [1,1])) :-
		{findall(X, (X=1;X=1), S)}.

	test(iso_findall_3_05, false) :-
		{findall(X, (X=2;X=1), [1,2])}.

	test(iso_findall_3_06, true(X-Y == 1-2)) :-
		{findall(X, (X=1;X=2), [X,Y])}.

	test(iso_findall_3_07, error(instantiation_error)) :-
		{findall(_X, _Goal, _S)}.

	test(iso_findall_3_08, error(type_error(callable,4))) :-
		% try to delay the error to runtime
		four(Four),
		{findall(_X, Four, _S)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_findall_3_09, error(type_error(list,[A|1]))) :-
		{findall(X, X=1, [A|1])}.

	% tests from the ECLiPSe test suite

	test(eclipse_findall_3_10, error(type_error(list,12))) :-
		{findall(X, (X=2; X=1), 12)}.

	% auxiliary predicates

	four(4).

:- end_object.
