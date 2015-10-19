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


% we start by defining three simple hook objects; each hook object defines a
% term_expansion/2 clause that expands facts

:- object(add1,
	implements(expanding)).

	term_expansion(Term, ExpandedTerm) :-
		nonvar(Term),
		\+ functor(Term, (:-), _),
		% not a rule or a directive
		Term =.. [Functor| Args],
		% add an argument in front of all arguments
		ExpandedTerm =.. [Functor, key| Args].

:- end_object.


:- object(add2,
	implements(expanding)).

	term_expansion(Term, ExpandedTerm) :-
		nonvar(Term),
		\+ functor(Term, (:-), _),
		% not a rule or a directive
		Term =.. [Functor, Arg1| Args],
		% add a second argument if there's at least one argument
		ExpandedTerm =.. [Functor, Arg1, value| Args].

:- end_object.


:- object(pair12,
	implements(expanding)).

	term_expansion(Term, ExpandedTerm) :-
		nonvar(Term),
		\+ functor(Term, (:-), _),
		% not a rule or a directive
		Term =.. [Functor, Arg1, Arg2| Args],
		% combine the first two arguments into a pair
		ExpandedTerm =.. [Functor, Arg1-Arg2| Args].

:- end_object.


% define a parametric object that takes a list of hook objects
% interpreted as specifying a pipeline of expansions

:- object(pipeline(_Pipeline),
	implements(expanding)).

	% rename the object and set the context_switching_calls to ensure that the unit tests work
	term_expansion((:- object(facts)), [(:- object(piped)), (:- set_logtalk_flag(context_switching_calls,allow))]) :-
		!.
	term_expansion(Term, Expansion) :-
		parameter(1, Pipeline),
		term_expansion_pipeline(Pipeline, Term, Expansion).

	% implement the term expansion pipeline; in this simple example we assume
	% that a single term is always returned
	term_expansion_pipeline([], Expansion, Expansion).
	term_expansion_pipeline([Hook| Hooks], Expansion0, Expansion) :-
		Hook::expand_term(Expansion0, Expansion1),
		term_expansion_pipeline(Hooks, Expansion1, Expansion).

	% implement the goal expansion pipeline
	goal_expansion(Goal, ExpandedGoal) :-
		parameter(1, Pipeline),
		goal_expansion_pipeline(Pipeline, Goal, ExpandedGoal).

	goal_expansion_pipeline([], ExpandedGoal, ExpandedGoal).
	goal_expansion_pipeline([Hook| Hooks], ExpandedGoal0, ExpandedGoal) :-
		Hook::expand_goal(ExpandedGoal0, ExpandedGoal1),
		goal_expansion_pipeline(Hooks, ExpandedGoal1, ExpandedGoal).

:- end_object.
