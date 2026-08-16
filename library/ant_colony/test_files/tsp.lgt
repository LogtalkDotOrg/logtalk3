%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tsp,
	implements(ant_colony_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Small TSP instance (regular hexagon with side length 5) for testing ant colony optimization. Distances are computed from coordinates, avoiding a large distance/3 table that would require multi-indexing for acceptable performance on some backends.'
	]).

	% Coordinates of a regular hexagon centered at the origin with side length 5.
	% Adjacent cities are distance 5; opposite cities are distance 10.
	coordinates(a,  5.0,        0.0).
	coordinates(b,  2.5,  4.330127).
	coordinates(c, -2.5,  4.330127).
	coordinates(d, -5.0,        0.0).
	coordinates(e, -2.5, -4.330127).
	coordinates(f,  2.5, -4.330127).

	distance(City1, City2, Distance) :-
		coordinates(City1, X1, Y1),
		coordinates(City2, X2, Y2),
		DX is X2 - X1,
		DY is Y2 - Y1,
		Distance is sqrt(DX*DX + DY*DY).

	nodes([a, b, c, d, e, f]).

	heuristic(From, To, Eta) :-
		distance(From, To, Dist),
		Eta is 1.0 / Dist.

	solution_cost(Tour, Cost) :-
		tour_length(Tour, Cost).

	tour_length([First| Rest], Length) :-
		tour_length_(Rest, First, First, 0.0, Length).

	tour_length_([], Last, First, Acc, Length) :-
		distance(Last, First, D),
		Length is Acc + D.
	tour_length_([Next| Rest], Prev, First, Acc, Length) :-
		distance(Prev, Next, D),
		Acc1 is Acc + D,
		tour_length_(Rest, Next, First, Acc1, Length).

:- end_object.
