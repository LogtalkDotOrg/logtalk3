%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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

:- object(pipeline(Pipeline),
	implements(expanding),
	extends(hook_pipeline(Pipeline))).

	% rename the object and set the context_switching_calls to ensure that the unit tests work
	term_expansion((:- object(facts)), [(:- object(piped)), (:- set_logtalk_flag(context_switching_calls,allow))]) :-
		!.
	term_expansion(Term, Expansion) :-
		^^term_expansion(Term, Expansion).

:- end_object.
