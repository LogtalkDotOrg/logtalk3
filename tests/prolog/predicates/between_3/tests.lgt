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


:- if(current_logtalk_flag(prolog_dialect, xsb)).
	:- import(from(/(between,3), basics)).
:- endif.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2021-05-11,
		comment is 'Unit tests for the de facto Prolog standard between/3 built-in predicate.'
	]).

	test(commons_between_3_01, deterministic) :-
		{between(1,5,1)}.

	test(commons_between_3_02, deterministic) :-
		{between(1,5,3)}.

	test(commons_between_3_03, deterministic) :-
		{between(1,5,5)}.

	test(commons_between_3_04, false) :-
		{between(1,3,0)}.

	test(commons_between_3_05, false) :-
		{between(1,3,5)}.

	test(commons_between_3_06, false) :-
		{between(1, 0, _)}.

	test(commons_between_3_07, true(L == [1, 2, 3])) :-
		findall(N, {between(1,3,N)}, L).

	test(commons_between_3_08, true(L == [1])) :-
		findall(N, {between(1,1,N)}, L).

	test(commons_between_3_09, error(instantiation_error)) :-
		{between(_, 3, _)}.

	test(commons_between_3_10, error(instantiation_error)) :-
		{between(1, _, _)}.

	test(commons_between_3_11, error(type_error(integer,a))) :-
		{between(a, 3, _)}.

	test(commons_between_3_12, error(type_error(integer,a))) :-
		{between(1, a, _)}.

	test(commons_between_3_13, error(type_error(integer,a))) :-
		{between(1, 3, a)}.

:- end_object.
