%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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

:- object(hook_pipeline(_Pipeline),
	implements(expanding)).

	:- info([
		version is 1:0:3,
		author is 'Paulo Moura',
		date is 2019-09-23,
		comment is 'Use a pipeline (represented using a list) of hook objects to expand terms and goals. The expansion results from a hook object are passed to the next hook object in the pipeline.',
		parameters is ['Pipeline'-'List of hook objects.'],
		remarks is [
			'Usage' - 'Compile source files that should be expanded using the pipeline of hook objects using the compiler option ``hook(hook_pipeline(Pipeline))``.'
		],
		see_also is [hook_set(_)]
	]).

	term_expansion(Term, Expansion) :-
		parameter(1, Pipeline),
		term_expansion_pipeline(Pipeline, Term, Expansion).

	term_expansion_pipeline([], Expansion, Expansion).
	term_expansion_pipeline([Hook| Hooks], Expansion0, Expansion) :-
		term_expansion_pipeline_all(Expansion0, Hook, Expansion1),
		flatten(Expansion1, [], Expansion2),
		term_expansion_pipeline(Hooks, Expansion2, Expansion).

	term_expansion_pipeline_all([], _, []) :-
		!.
	term_expansion_pipeline_all([Term| Terms], Hook, [Expansion| Expansions]) :-
		!,
		Hook::expand_term(Term, Expansion),
		term_expansion_pipeline_all(Terms, Hook, Expansions).
	term_expansion_pipeline_all(Term, Hook, Expansion) :-
		Hook::expand_term(Term, Expansion).

	goal_expansion(Goal, ExpandedGoal) :-
		parameter(1, Pipeline),
		goal_expansion_pipeline(Pipeline, Goal, ExpandedGoal).

	goal_expansion_pipeline([], ExpandedGoal, ExpandedGoal).
	goal_expansion_pipeline([Hook| Hooks], ExpandedGoal0, ExpandedGoal) :-
		Hook::expand_goal(ExpandedGoal0, ExpandedGoal1),
		goal_expansion_pipeline(Hooks, ExpandedGoal1, ExpandedGoal).

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
