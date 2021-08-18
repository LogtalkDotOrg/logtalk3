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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2021-08-18,
		comment is 'Unit tests for the expand_goal/2 built-in method.'
	]).

	test(expand_goal_ol_01, true(Goal == goal)) :-
		obj_ol_01::p(Goal).

	test(expand_goal_ol_02, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_ol_02::p(Goal1), obj_ol_02::q(Goal2).

	test(expand_goal_ol_03, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_ol_03::p(Goal1), obj_ol_03::q(Goal2).

	test(expand_goal_ol_04, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_ol_04::p(Goal1), obj_ol_04::q(Goal2).

	test(expand_goal_ol_05, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_ol_05::p(Goal1), obj_ol_05::q(Goal2).

	test(expand_goal_ol_06, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_ol_06::p(Goal1), obj_ol_06::q(Goal2).

	test(expand_goal_ol_07, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_ol_07::p(Goal1), obj_ol_07::q(Goal2).

	test(expand_goal_ol_08, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_ol_08::p(Goal1), obj_ol_08::q(Goal2).

	test(expand_goal_ol_09, true(Goal1-Goal2 == goal-foo)) :-
		obj_ol_09::p(Goal1), obj_ol_09::q(Goal2).

	test(expand_goal_ol_10, true(Goal1-Goal2 == goal-'FOO')) :-
		obj_ol_10::p(Goal1), obj_ol_10::q(Goal2).

	test(expand_goal_ol_11, true(Goal == goal)) :-
		obj_ol_11::p(Goal).

	test(expand_goal_0l_12, true(Goal == goal)) :-
		obj_ol_12::p(Goal).

	test(expand_goal_cl_01, true(Goal == goal)) :-
		obj_cl_01::p(Goal).

	test(expand_goal_cl_02, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_cl_02::p(Goal1), obj_cl_02::q(Goal2).

	test(expand_goal_cl_03, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_cl_03::p(Goal1), obj_cl_03::q(Goal2).

	test(expand_goal_cl_04, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_cl_04::p(Goal1), obj_cl_04::q(Goal2).

	test(expand_goal_cl_05, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_cl_05::p(Goal1), obj_cl_05::q(Goal2).

	test(expand_goal_cl_06, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_cl_06::p(Goal1), obj_cl_06::q(Goal2).

	test(expand_goal_cl_07, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_cl_07::p(Goal1), obj_cl_07::q(Goal2).

	test(expand_goal_cl_08, true(Goal1-Goal2 == 'GOAL'-foo)) :-
		obj_cl_08::p(Goal1), obj_cl_08::q(Goal2).

	test(expand_goal_cl_09, true(Goal1-Goal2 == goal-foo)) :-
		obj_cl_09::p(Goal1), obj_cl_09::q(Goal2).

	test(expand_goal_cl_10, true(Goal1-Goal2 == goal-'FOO')) :-
		obj_cl_10::p(Goal1), obj_cl_10::q(Goal2),
		Goal1 == goal, Goal2 == 'FOO'.

	test(expand_goal_cl_11, true(Goal == goal)) :-
		obj_cl_11::p(Goal).

	test(expand_goal_cl_12, true(Goal == goal)) :-
		obj_cl_12::p(Goal).

	% expand_goal/2 messages

	test(expand_goal_m_01, true(Goal == goal)) :-
		obj_om_01::expand_goal(goal, Goal).

	test(expand_goal_m_02, true(Goal == goal)) :-
		obj_om_02::expand_goal(goal, Goal).

	test(expand_goal_m_03, true(Goal == 'GOAL')) :-
		obj_om_03::expand_goal(goal, Goal).

	test(expand_goal_m_04, true(Goal == goal)) :-
		obj_om_04::expand_goal(goal, Goal).

	test(expand_goal_m_05, true(Goal == goal)) :-
		obj_om_05::expand_goal(goal, Goal).

	test(expand_goal_m_06, true(Goal == 'GOAL')) :-
		obj_om_06::expand_goal(goal, Goal).

	test(expand_goal_m_07, true(Goal == goal)) :-
		obj_om_07::expand_goal(goal, Goal).

	test(expand_goal_m_08, true(Goal == goal)) :-
		obj_om_08::expand_goal(goal, Goal).

	test(expand_goal_m_09, true(Goal == goal)) :-
		obj_om_09::expand_goal(goal0, Goal).

	test(expand_goal_m_10, true(Goal == goal)) :-
		obj_om_10::expand_goal(goal0, Goal).

	% test semantics for local calls from multifile predicate clauses

	test(expand_goal_multifile_01, true(Expansion == secondary)) :-
		primary::expand(goal, Expansion).

	% test that goal-expansion doesn't go into an infinite loop when the goal
	% to be expanded resulted from a previous expansion of the same goal

	test(expand_goal_fixed_point, true(Expansion == (a->b;c))) :-
		fixed_point::expand_goal(a, Expansion).

:- end_object.
