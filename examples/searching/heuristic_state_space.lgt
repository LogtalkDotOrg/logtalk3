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


:- object(heuristic_state_space,
	instantiates(class),
	specializes(state_space)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Heuristic state space.'
	]).

	:- public(next_state/3).
	:- mode(next_state(+nonvar, -nonvar, -number), zero_or_more).
	:- info(next_state/3, [
		comment is 'Generates a state sucessor.',
		argnames is ['State', 'Next', 'Cost']
	]).

	:- public(heuristic/2).
	:- mode(heuristic(+nonvar, -number), one).
	:- info(heuristic/2, [
		comment is 'Estimates state distance to a goal state.',
		argnames is ['State', 'Estimate']
	]).

	next_state(Prev, Next) :-
		::next_state(Prev, Next, _).

:- end_object.
