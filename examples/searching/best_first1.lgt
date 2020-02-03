%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(best_first(Threshold),
	instantiates(heuristic_search(Threshold))).

	:- info([
		version is 1.4,
		author is 'Ivan Bratko; adapted to Logtalk by Paulo Moura.',
		date is 2013-05-10,
		comment is 'Best first heuristic state space search strategy.',
		source is 'Code adapted from the book "Prolog Programming for Artificial Intelligence" by Ivan Bratko.',
		parnames is ['Threshold']
	]).

	:- private(expand/8).
	:- private(succlist/5).
	:- private(bestf/3).
	:- private(continue/9).
	:- private(f/2).
	:- private(insert/4).

	search(Space, State, Threshold, Solution, Cost) :-
		% to avoid issues with some Prolog backends parsing 0/0 as
		% a rational number, we write the term instead as (0)/(0)
		expand([], l(State, (0)/(0)), Threshold, _, yes, Path, Space, Cost),
		list::reverse(Path, Solution).

	expand(Path, l(State,Cost/_), _, _, yes, [State|Path], Space, Cost) :-
		Space::goal_state(State).
	expand(Path, l(State,F/G), Threshold, Tree, Solved, Solution, Space, Cost) :-
		F =< Threshold,
		(	bagof(Next/Cost2, (Space::next_state(State, Next, Cost2), \+ Space::member_path(Next, Path)), Successors) ->
			succlist(G, Successors, Trees, Threshold, Space),
			bestf(Trees, F2, Threshold),
			expand(Path, t(State, F2/G, Trees), Threshold, Tree, Solved, Solution, Space, Cost)
		;	Solved = never
		).
	expand(Path, t(State, F/G,[Tree| Trees]), Threshold, Tree3, Solved, Solution, Space, Cost) :-
		F =< Threshold,
		bestf(Trees, Threshold2, Threshold),
		expand([State|Path], Tree, Threshold2, Tree2, Solved2, Solution, Space, Cost),
		continue(Path, t(State, F/G, [Tree2| Trees]), Threshold, Tree3, Solved2, Solved, Solution, Space, Cost).
	expand(_, t(_, _, []), _, _, never, _, _, _) :-
		!.
	expand(_, Tree, Threshold, Tree, no, _, _, _) :-
		f(Tree, F),
		F > Threshold.

	continue(_, _, _, _, yes, yes, _, _, _).
	continue(Path, t(State, _/G, [Tree| Trees]), Threshold, Tree2, no, Solved, Solution, Space, Cost) :-
		insert(Tree, Trees, NewTrees, Threshold),
		bestf(NewTrees, F, Threshold),
		expand(Path, t(State, F/G, NewTrees), Threshold, Tree2, Solved, Solution, Space, Cost).
	continue(Path,t(State, _/G, [_| Trees]), Threshold, Tree2, never, Solved, Solution, Space, Cost) :-
		bestf(Trees, F, Threshold),
		expand(Path, t(State, F/G, Trees), Threshold, Tree2, Solved, Solution, Space, Cost).

	succlist(_, [], [], _, _).
	succlist(G0, [State/Cost| Rest], Trees, Threshold, Space) :-
		G is G0 + Cost,
		Space::heuristic(State, H),
		F is G + H,
		succlist(G0, Rest, Trees2, Threshold, Space),
		insert(l(State, F/G), Trees2, Trees, Threshold).

	insert(Tree, [], [Tree], _) :-
		!.
	insert(Tree, Trees, [Tree| Trees], Threshold) :-
		f(Tree, F),
		bestf(Trees, F2, Threshold),
		F =< F2,
		!.
	insert(Tree, [Tree1| Trees], [Tree1| Trees1], Threshold) :-
		insert(Tree, Trees, Trees1, Threshold).

	f(l(_, F/_), F).
	f(t(_, F/_, _), F).

	bestf([Tree| _], F, _) :-
		f(Tree, F).
	bestf([], Threshold, Threshold).

:- end_object.
