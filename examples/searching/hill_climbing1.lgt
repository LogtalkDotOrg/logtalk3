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


:- object(hill_climbing(Threshold),
	instantiates(heuristic_search(Threshold))).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2008/6/9,
		comment is 'Hill climbing heuristic state space search strategy.',
		parnames is ['Threshold']
	]).

	:- uses(list, [member/2, reverse/2, sort/2]).

	search(Space, State, Threshold, Solution, Cost) :-
		hill(Space, State, Threshold, [], Path, 0, Cost),
		reverse(Path, Solution).

	hill(Space, State, _, Path, [State| Path], Cost, Cost) :-
		Space::goal_state(State).
	hill(Space, State, Threshold, Path, Solution, SoFar, Total) :-
		findall(
			(Estimate, Cost, Next),
			(Space::next_state(State, Next, Cost),
			 \+ Space::member_path(Next, [State| Path]),
			 Space::heuristic(Next, Guess),
			 Estimate is Guess + Cost),
			States),
		sort(States, SortedStates),
		member((_, Cost2, Next2), SortedStates),
		SoFar2 is SoFar + Cost2,
		SoFar2 =< Threshold,
		hill(Space, Next2, Threshold, [State| Path], Solution, SoFar2, Total).

:- end_object.
