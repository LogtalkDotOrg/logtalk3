%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2022-03-31,
		comment is 'Unit tests for the de facto Prolog standard plus/3 built-in predicate.'
	]).

	% tests from the Logtalk portability work

	test(lgt_plus_3_01, true(N == 3)) :-
		{plus(1, 2, N)}.

	test(lgt_plus_3_02, true(N == 2)) :-
		{plus(1, N, 3)}.

	test(lgt_plus_3_03, true(N == 1)) :-
		{plus(N, 2, 3)}.

	test(lgt_plus_3_04, fail) :-
		{plus(1, 2, 4)}.

	test(lgt_plus_3_05, deterministic) :-
		{plus(1, 2, 3)}.

	test(lgt_plus_3_06, deterministic) :-
		{plus(_, 2, 3)}.

	test(lgt_plus_3_07, deterministic) :-
		{plus(1, _, 3)}.

	test(lgt_plus_3_08, deterministic) :-
		{plus(1, 2, _)}.

	test(lgt_plus_3_09, error(instantiation_error)) :-
		{plus(_, _, _)}.

	test(lgt_plus_3_10, error(instantiation_error)) :-
		{plus(1, _, _)}.

	test(lgt_plus_3_11, error(instantiation_error)) :-
		{plus(_, 1, _)}.

	test(lgt_plus_3_12, error(instantiation_error)) :-
		{plus(_, _, 1)}.

	test(lgt_plus_3_13, error(type_error(integer,a))) :-
		{plus(a, 1, _)}.

	test(lgt_plus_3_14, error(type_error(integer,a))) :-
		{plus(1, a, _)}.

	test(lgt_plus_3_15, error(type_error(integer,a))) :-
		{plus(_, 1, a)}.

	test(lgt_plus_3_16, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		current_prolog_flag(max_integer, Max),
		{plus(Max, 1, _)}.

:- end_object.
