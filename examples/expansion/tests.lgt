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
		version is 1:1:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2020-12-26,
		comment is 'Unit tests for the "expansion" example.'
	]).

	test(expansion_01, true(Term == eight)) :-
		exp_public::expand_term(8, Term).

	test(expansion_02, true(EGoal == write_term(Term, [quoted(true)]))) :-
		exp_public::expand_goal(write(Term), EGoal).

	test(expansion_03, true(Term == 8)) :-
		exp_protected::expand_term(8, Term).

	test(expansion_04, true(EGoal == write(Term))) :-
		exp_protected::expand_goal(write(Term), EGoal).

	test(expansion_05, true(Term == 8)) :-
		exp_private::expand_term(8, Term).

	test(expansion_06, true(EGoal == write(Term))) :-
		exp_private::expand_goal(write(Term), EGoal).

	test(expansion_07, true(Term == eight)) :-
		desc_public::test_term_expansion(8, Term).

	test(expansion_08, true(EGoal == write_term(Term, [quoted(true)]))) :-
		desc_public::test_goal_expansion(write(Term), EGoal).

	test(expansion_09, true(Term == eight)) :-
		desc_protected::test_term_expansion(8, Term).

	test(expansion_10, true(EGoal == write_term(Term, [quoted(true)]))) :-
		desc_protected::test_goal_expansion(write(Term), EGoal).

	test(expansion_11, true(Term == 8)) :-
		desc_private::test_term_expansion(8, Term).

	test(expansion_12, true(EGoal == write(Term))) :-
		desc_private::test_goal_expansion(write(Term), EGoal).

	test(expansion_13, true) :-
		cooked << (aa, bb, cc).

	test(expansion_14, true) :-
		cooked << (ha, hb, hc).

	test(expansion_15, true) :-
		cooked << p.

	test(expansion_16, true) :-
		piped << a(key-value).

	test(expansion_17, true) :-
		piped << b(key-value).

	test(expansion_18, true) :-
		piped << c(key-value).

:- end_object.
