%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(mt_hill_climbing(Threshold, _Threads),
	instantiates(heuristic_search(Threshold))).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/6/9,
		comment is 'Multi-threaded hill climbing heuristic state space search strategy.',
		parnames is ['Threshold', 'Threads']]).

	:- threaded.

	:- uses(list, [member/2, reverse/2, sort/2]).

	search(Space, State, Threshold, Solution, Cost) :-
		parameter(2, Threads),
		hill(Threads, Space, State, Threshold, [], Path, 0, Cost),
		reverse(Path, Solution).

	hill(_, Space, State, _, Path, [State| Path], Cost, Cost) :-
		Space::goal_state(State).

	hill(Threads, Space, State, Threshold, Path, Solution, SoFar, Total) :-
		findall(
			(Estimate, Cost, Next),
			(Space::next_state(State, Next, Cost),
             \+ Space::member_path(Next, [State| Path]),
             Space::heuristic(Next, Guess),
             Estimate is Guess + Cost),
			States),
		sort(States, SortedStates),
		hill(Threads, Space, State, SortedStates, Threshold, Path, Solution, SoFar, Total).

	hill(1, Space, State, SortedStates, Threshold, Path, Solution, SoFar, Total) :-
		!,
		member((_, Cost2, Next2), SortedStates),
		SoFar2 is SoFar + Cost2,
		SoFar2 =< Threshold,
		hill(1, Space, Next2, Threshold, [State| Path], Solution, SoFar2, Total).

	hill(Threads, Space, State, SortedStates, Threshold, Path, Solution, SoFar, Total) :-
		Threads > 1,
		Threads2 is Threads//2,
		two_members((_, Cost2, Next2), (_, Cost3, Next3), SortedStates),
		(	Next3 == nil ->
			SoFar2 is SoFar + Cost2,
			SoFar2 =< Threshold,
			hill(Threads, Space, Next2, Threshold, [State| Path], Solution, SoFar2, Total)
		;	SoFar2 is SoFar + Cost2,
			SoFar2 =< Threshold,
			SoFar3 is SoFar + Cost3,
			SoFar3 =< Threshold,
			threaded((
					hill(Threads2, Space, Next2, Threshold, [State| Path], Solution, SoFar2, Total)
				;	hill(Threads2, Space, Next3, Threshold, [State| Path], Solution, SoFar3, Total)
			))
		).

	two_members(S1, S2, [S1, S2| _]).
	two_members(S1, S2, [_, _| States]) :-
		two_members(S1, S2, States).
	two_members(S1, (0, 0, nil), [S1]).

:- end_object.
