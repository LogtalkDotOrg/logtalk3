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


:- object(hook_debug,
	% built-in protocol for term and goal expansion methods
	implements(expanding)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/04/09,
		comment is 'Compiler hook support for activating debug statements.'
	]).

	goal_expansion(debug(Goal), Goal).

:- end_object.



:- object(hook_production,
	% built-in protocol for term and goal expansion methods
	implements(expanding)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/4/9,
		comment is 'Compiler hook support for discarding debug statements.'
	]).

	goal_expansion(debug(_), true).

:- end_object.



% in alternative to the two hook objects above, you may use a single parametric
% object and set the parameter in the loader files:

:- object(hook(_Mode),
	% built-in protocol for term and goal expansion methods
	implements(expanding)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/04/15,
		comment is 'Expands debug/1 calls. The parameter Mode can be either the atom "debug" or "production".',
		parnames is ['Mode']
	]).

	goal_expansion(debug(Goal), ExpandedGoal) :-
		parameter(1, Mode),
		(	Mode == debug ->
			ExpandedGoal = Goal
		;	ExpandedGoal = true
		).

:- end_object.
