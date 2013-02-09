%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(breadth_first(Bound),
	instantiates(blind_search(Bound))).

	:- info([
		version is 1.4,
		author is 'Ivan Bratko; adapted to Logtalk by Paulo Moura.',
		date is 2010/06/23,
		comment is 'Breadth first state space search strategy.',
		source is 'Code adapted from the book "Prolog Programming for Artificial Intelligence" by Ivan Bratko.',
		parnames is ['Bound']
	]).

	:- uses(list, [member/2, reverse/2]).

	search(Space, State, Bound, Solution) :-
		breadth(Space, l(State), Bound, Path),
		reverse(Path, Solution).

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
