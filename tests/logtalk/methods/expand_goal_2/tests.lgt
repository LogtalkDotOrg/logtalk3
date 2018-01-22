%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2015/10/28,
		comment is 'Unit tests for the expand_goal/2 built-in method.'
	]).

	test(expand_goal_ol_01) :-
		obj_ol_01::p(Goal),
		Goal == goal.

	test(expand_goal_ol_02) :-
		obj_ol_02::p(Goal1), obj_ol_02::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_ol_03) :-
		obj_ol_03::p(Goal1), obj_ol_03::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_ol_04) :-
		obj_ol_04::p(Goal1), obj_ol_04::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_ol_05) :-
		obj_ol_05::p(Goal1), obj_ol_05::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_ol_06) :-
		obj_ol_06::p(Goal1), obj_ol_06::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_ol_07) :-
		obj_ol_07::p(Goal1), obj_ol_07::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_ol_08) :-
		obj_ol_08::p(Goal1), obj_ol_08::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_ol_09) :-
		obj_ol_09::p(Goal1), obj_ol_09::q(Goal2),
		Goal1 == goal, Goal2 == foo.

	test(expand_goal_ol_10) :-
		obj_ol_10::p(Goal1), obj_ol_10::q(Goal2),
		Goal1 == goal, Goal2 == 'FOO'.

	test(expand_goal_ol_11) :-
		obj_ol_11::p(Goal),
		Goal == goal.

	test(expand_goal_0l_12) :-
		obj_ol_12::p(Goal),
		Goal == goal.

	test(expand_goal_cl_01) :-
		obj_cl_01::p(Goal),
		Goal == goal.

	test(expand_goal_cl_02) :-
		obj_cl_02::p(Goal1), obj_cl_02::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_cl_03) :-
		obj_cl_03::p(Goal1), obj_cl_03::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_cl_04) :-
		obj_cl_04::p(Goal1), obj_cl_04::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_cl_05) :-
		obj_cl_05::p(Goal1), obj_cl_05::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_cl_06) :-
		obj_cl_06::p(Goal1), obj_cl_06::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_cl_07) :-
		obj_cl_07::p(Goal1), obj_cl_07::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_cl_08) :-
		obj_cl_08::p(Goal1), obj_cl_08::q(Goal2),
		Goal1 == 'GOAL', Goal2 == foo.

	test(expand_goal_cl_09) :-
		obj_cl_09::p(Goal1), obj_cl_09::q(Goal2),
		Goal1 == goal, Goal2 == foo.

	test(expand_goal_cl_10) :-
		obj_cl_10::p(Goal1), obj_cl_10::q(Goal2),
		Goal1 == goal, Goal2 == 'FOO'.

	test(expand_goal_cl_11) :-
		obj_cl_11::p(Goal),
		Goal == goal.

	test(expand_goal_cl_12) :-
		obj_cl_12::p(Goal),
		Goal == goal.

	% expand_goal/2 messages

	test(expand_goal_m_01) :-
		obj_om_01::expand_goal(goal, Goal),
		Goal == goal.

	test(expand_goal_m_02) :-
		obj_om_02::expand_goal(goal, Goal),
		Goal == goal.

	test(expand_goal_m_03) :-
		obj_om_03::expand_goal(goal, Goal),
		Goal == 'GOAL'.

	test(expand_goal_m_04) :-
		obj_om_04::expand_goal(goal, Goal),
		Goal == goal.

	test(expand_goal_m_05) :-
		obj_om_05::expand_goal(goal, Goal),
		Goal == goal.

	test(expand_goal_m_06) :-
		obj_om_06::expand_goal(goal, Goal),
		Goal == 'GOAL'.

	test(expand_goal_m_07) :-
		obj_om_07::expand_goal(goal, Goal),
		Goal == goal.

	test(expand_goal_m_08) :-
		obj_om_08::expand_goal(goal, Goal),
		Goal == goal.

	test(expand_goal_m_09) :-
		obj_om_09::expand_goal(goal0, Goal),
		Goal == goal.

	test(expand_goal_m_10) :-
		obj_om_10::expand_goal(goal0, Goal),
		Goal == goal.

	% test semantics for local calls from multifile predicate clauses

	test(expand_goal_multifile_01) :-
		primary::expand(goal, Expansion),
		Expansion == secondary.

:- end_object.
