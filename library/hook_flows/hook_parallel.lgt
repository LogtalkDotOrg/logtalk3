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


% define a parametric object that takes a list of hook objects
% interpreted as specifying a pipeline of expansions

:- object(hook_parallel(_Hooks),
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-03-20,
		comment is 'Apply all hook objects to expand terms.',
		parameters is ['Hooks'-'List of hook objects'],
		remarks is [
			'Usage' - 'Compile source files that should be expanded using the pipeline of hook objects using the compiler option ``hook(hook_parallel(Pipeline))``.'
		],
		see_also is [hook_set(_), hook_parallel(_)]
	]).

	term_expansion(Term, Expansion) :-
		parameter(1, Hooks),
		term_expansion_parallel(Hooks, Term, Expansion0),
		flatten(Expansion0, [], Expansion).

	term_expansion_parallel([], Term, Term).
	term_expansion_parallel([Hook| Hooks], Term, [Expansion| Expansions]) :-
		Hook::expand_term(Term, Expansion),
		term_expansion_parallel(Hooks, Term, Expansions).

	goal_expansion(Goal, ExpandedGoal) :-
		parameter(1, Pipeline),
		goal_expansion_parallel(Pipeline, Goal, ExpandedGoal).

	goal_expansion_parallel([], Goal, Goal).
	goal_expansion_parallel([Hook| Hooks], Goal, (ExpandedGoal, ExpandedGoals)) :-
		Hook::expand_goal(Goal, ExpandedGoal),
		goal_expansion_parallel(Hooks, Goal, ExpandedGoals).

	flatten(Var, Tail, Flatted) :-
		var(Var),
		!,
		Flatted = [Var| Tail].
	flatten([], List, Flatted) :-
		!,
		Flatted = List.
	flatten([Head| Tail], List, Flatted) :-
		!,
		flatten(Tail, List, Aux),
		flatten(Head, Aux, Flatted).
	flatten(Head, Tail, [Head| Tail]).

:- end_object.
