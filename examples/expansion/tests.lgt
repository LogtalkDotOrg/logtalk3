%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.01,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/12/14,
		comment is 'Unit tests for the "expansion" example.'
	]).

	test(expansion_01) :-
		exp_public::expand_term(8, Term),
		Term == eight.

	test(expansion_02) :-
		exp_public::expand_goal(write(Term), EGoal),
		EGoal = write_term(Term, [quoted(true)]).

	test(expansion_03) :-
		exp_protected::expand_term(8, Term),
		Term == 8.

	test(expansion_04) :-
		exp_protected::expand_goal(write(Term), EGoal),
		EGoal = write(Term).

	test(expansion_05) :-
		exp_private::expand_term(8, Term),
		Term == 8.

	test(expansion_06) :-
		exp_private::expand_goal(write(Term), EGoal),
		EGoal = write(Term).

	test(expansion_07) :-
		desc_public::test_term_expansion(8, Term),
		Term == eight.

	test(expansion_08) :-
		desc_public::test_goal_expansion(write(Term), EGoal),
		EGoal = write_term(Term, [quoted(true)]).

	test(expansion_09) :-
		desc_protected::test_term_expansion(8, Term),
		Term == eight.

	test(expansion_10) :-
		desc_protected::test_goal_expansion(write(Term), EGoal),
		EGoal = write_term(Term, [quoted(true)]).

	test(expansion_11) :-
		desc_private::test_term_expansion(8, Term),
		Term == 8.

	test(expansion_12) :-
		desc_private::test_goal_expansion(write(Term), EGoal),
		EGoal = write(Term).

	test(expansion_13) :-
		cooked << (aa, bb, cc).

	test(expansion_14) :-
		cooked << (ha, hb, hc).

	test(expansion_15) :-
		cooked << p.

	test(expansion_16) :-
		piped<<a(key-value).

	test(expansion_17) :-
		piped<<b(key-value).

	test(expansion_18) :-
		piped<<c(key-value).

:- end_object.
