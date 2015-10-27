%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		date is 2015/10/27,
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

:- end_object.
