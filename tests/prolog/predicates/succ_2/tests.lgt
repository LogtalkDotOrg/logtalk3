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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2022-03-31,
		comment is 'Unit tests for the de facto Prolog standard succ/2 built-in predicate.'
	]).

	% tests from the Logtalk portability work

	test(lgt_succ_2_01, true(N == 2)) :-
		{succ(1, N)}.

	test(lgt_succ_2_02, true(N == 1)) :-
		{succ(N, 2)}.

	test(lgt_succ_2_03, fail) :-
		{succ(3, 2)}.

	test(lgt_succ_2_04, fail) :-
		{succ(_, 0)}.

	test(lgt_succ_2_05, deterministic) :-
		{succ(1, 2)}.

	test(lgt_succ_2_06, error(instantiation_error)) :-
		{succ(_, _)}.

	test(lgt_succ_2_07, error(type_error(integer,a))) :-
		{succ(a, _)}.

	test(lgt_succ_2_08, error(type_error(integer,a))) :-
		{succ(_, a)}.

	test(lgt_succ_2_09, error(domain_error(not_less_than_zero, -1))) :-
		{succ(-1, _)}.

	test(lgt_succ_2_10, error(domain_error(not_less_than_zero, -1))) :-
		{succ(_, -1)}.

	test(lgt_succ_2_11, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		current_prolog_flag(max_integer, Max),
		{succ(Max, _)}.

:- end_object.
