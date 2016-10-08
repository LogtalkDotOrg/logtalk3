%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2015/11/16,
		comment is 'Use a set (represented using a list) of hook objects to expand terms and goals. The hook objects are tried in sequence until one of them succeeds in expanding the current term (goal) into a different term (goal).',
		parameters is ['Set'-'Set (list) of hook objects'],
		see_also is [hook_pipeline(_)]
	]).

	term_expansion(Term, Expansion) :-
		parameter(1, Set),
		term_expansion_set(Set, Term, Expansion).

	term_expansion_set([Hook| Hooks], Term, Expansion) :-
		(	Hook::expand_term(Term, Expansion),
			Term \== Expansion ->
			true
		;	term_expansion_set(Hooks, Term, Expansion)
		).

	goal_expansion(Goal, ExpandedGoal) :-
		parameter(1, Set),
		goal_expansion_set(Set, Goal, ExpandedGoal).

	goal_expansion_set([Hook| Hooks], Goal, ExpandedGoal) :-
		(	Hook::expand_goal(Goal, ExpandedGoal),
			Goal \== ExpandedGoal ->
			true
		;	goal_expansion_set(Hooks, Goal, ExpandedGoal)
		).

:- end_object.
