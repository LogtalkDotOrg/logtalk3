%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
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
