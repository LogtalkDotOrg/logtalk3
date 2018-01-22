%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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


:- object(breadth_first(Bound),
	instantiates(blind_search(Bound))).

	:- info([
		version is 1.5,
		author is 'Ivan Bratko; adapted to Logtalk by Paulo Moura.',
		date is 2013/05/10,
		comment is 'Breadth first state space search strategy.',
		source is 'Code adapted from the book "Prolog Programming for Artificial Intelligence" by Ivan Bratko.',
		parnames is ['Bound']
	]).

	search(Space, State, Bound, Solution) :-
		breadth(Space, l(State), Bound, Path),
		list::reverse(Path, Solution).

	breadth(Space, Tree, Bound, Solution) :-
		expand([], Tree, Tree2, Solved, Solution, Space, Bound),
		(	Solved == true ->
			true
		;	breadth(Space, Tree2, Bound, Solution)
		).

	expand(Path, l(State), _, true, [State| Path], Space, _) :-
		Space::goal_state(State).
	expand(Path, l(State), t(State, Subs), fail, _, Space, Bound) :-
		Bound > 0,
		bagof(l(Next), (Space::next_state(State, Next), \+ Space::member_path(Next, [State| Path])), Subs).
	expand(Path, t(State,Subs), t(State, Subs2), Solved, Solution, Space, Bound) :-
		expandall([State| Path], Subs, [], Subs2, Solved, Solution, Space, Bound).

	expandall(_, [], [Tree| Trees], [Tree| Trees], fail, _, _, _).
	expandall(Path, [Tree| Trees], Trees2, Subs2, Solved, Solution, Space, Bound) :-
		(	Bound > 0,
			Bound2 is Bound - 1,
			expand(Path, Tree, Tree2, Solved2, Solution, Space, Bound2),
			(	Solved2 == true ->
				Solved = true
			;	expandall(Path, Trees, [Tree2| Trees2], Subs2, Solved, Solution, Space, Bound)
			)
		;	expandall(Path, Trees, Trees2, Subs2, Solved, Solution, Space, Bound)
		).

:- end_object.
