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


:- object(depth_first(Bound),
	instantiates(blind_search(Bound))).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2013/05/10,
		comment is 'Depth first state space search strategy.',
		parnames is ['Bound']
	]).

	search(Space, State, Bound, Solution) :-
		depth(Space, State, Bound, [], Path),
		list::reverse(Path, Solution).

	depth(Space, State, _, Path, [State| Path]) :-
		Space::goal_state(State).
	depth(Space, State, Bound, Path, Solution) :-
		Bound > 0,
		Space::next_state(State, Next),
		\+ Space::member_path(Next, [State| Path]),
		Bound2 is Bound - 1,
		depth(Space, Next, Bound2, [State| Path], Solution).

:- end_object.
