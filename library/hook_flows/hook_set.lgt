%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


% define a parametric object that takes a list of hook objects
% interpreted as specifying a set of expansions

:- object(hook_set(_Set),
	implements(expanding)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2024-09-27,
		comment is 'Use a set (represented using a list) of hook objects to expand terms and goals. The hook objects are tried in sequence until one of them succeeds in expanding the current term (goal) into a different term (goal).',
		parameters is ['Set'-'Set (list) of hook objects.'],
		remarks is [
			'Usage' - 'Compile source files that should be expanded using the set of hook objects using the compiler option ``hook(hook_set(Set))``.'
		],
		see_also is [hook_pipeline(_)]
	]).

	term_expansion(Term, Expansion) :-
		parameter(1, Set),
		term_expansion_set(Set, Term, Expansion).

	term_expansion_set([Hook| Hooks], Term, Expansion) :-
		(	nonvar(Term),
			Hook::term_expansion(Term, Expansion),
			Term \== Expansion ->
			true
		;	term_expansion_set(Hooks, Term, Expansion)
		).

	goal_expansion(Goal, ExpandedGoal) :-
		parameter(1, Set),
		goal_expansion_set(Set, Goal, ExpandedGoal).

	goal_expansion_set([Hook| Hooks], Goal, ExpandedGoal) :-
		(	hook_goal_expansion(Hook, Goal, ExpandedGoal, []),
			Goal \== ExpandedGoal ->
			true
		;	goal_expansion_set(Hooks, Goal, ExpandedGoal)
		).

	hook_goal_expansion(Hook, Goal, ExpandedGoal, ExpandedGoals) :-
		(	var(Goal) ->
			ExpandedGoal = Goal
		;	push_if_new(ExpandedGoals, Goal, NewExpandedGoals),
			Hook::goal_expansion(Goal, ExpandedGoal0),
			Goal \== ExpandedGoal0 ->
			hook_goal_expansion(Hook, ExpandedGoal0, ExpandedGoal, NewExpandedGoals)
		;	ExpandedGoal = Goal
		).

	% auxiliary predicate to prevent going into an infinite loop when
	% goal-expansion results in a goal that contains the expanded goal
	%
	% calls to this predicate fail if the goal about to be expanded was
	% the result of a previous goal expansion (tested using term equality)
	push_if_new(ExpandedGoals, Goal, [Goal| ExpandedGoals]) :-
		\+ member_equal(Goal, ExpandedGoals).

	member_equal(Term, [Head| _]) :-
		Term == Head.
	member_equal(Term, [_| Tail]) :-
		member_equal(Term, Tail).

:- end_object.
