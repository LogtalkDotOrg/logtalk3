%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019/12/23,
		comment is 'Unit tests for the de facto Prolog standard succ/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the Logtalk portability work

	test(lgt_succ_2_01, error(instantiation_error)) :-
		{succ(_, _)}.

	test(lgt_succ_2_02, error(type_error(integer,a))) :-
		{succ(a, _)}.

	test(lgt_succ_2_03, error(type_error(integer,a))) :-
		{succ(_, a)}.

	test(lgt_succ_2_04, error(domain_error(not_less_than_zero, -1))) :-
		{succ(-1, _)}.

	test(lgt_succ_2_05, error(domain_error(not_less_than_zero, -1))) :-
		{succ(_, -1)}.

	test(lgt_succ_2_06, true(N == 2)) :-
		{succ(1, N)}.

	test(lgt_succ_2_07, true(N == 1)) :-
		{succ(N, 2)}.

	test(lgt_succ_2_08, fail) :-
		{succ(3, 2)}.

:- end_object.
