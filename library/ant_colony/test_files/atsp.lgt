%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(atsp,
	implements(ant_colony_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Small asymmetric TSP instance for testing ant colony optimization. Directed distances are derived from coordinates with a directed bias, exercising the directed pheromone model without a large multi-clause distance table.'
	]).

	% Base layout: regular hexagon (side length 5), same as the symmetric tsp object.
	coordinates(a,  5.0,        0.0).
	coordinates(b,  2.5,  4.330127).
	coordinates(c, -2.5,  4.330127).
	coordinates(d, -5.0,        0.0).
	coordinates(e, -2.5, -4.330127).
	coordinates(f,  2.5, -4.330127).

	% Directed distance: Euclidean length plus a small bias that depends on
	% the ordered pair, making From->To generally different from To->From.
	distance(City1, City2, Distance) :-
		coordinates(City1, X1, Y1),
		coordinates(City2, X2, Y2),
		DX is X2 - X1,
		DY is Y2 - Y1,
		Euclidean is sqrt(DX*DX + DY*DY),
		% bias in [0, 1) derived from a simple hash of the atom names
		bias(City1, City2, Bias),
		Distance is Euclidean + Bias.

	bias(City1, City2, Bias) :-
		atom_codes(City1, [C1| _]),
		atom_codes(City2, [C2| _]),
		Bias is abs(C1 - C2) / 100.0.

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
