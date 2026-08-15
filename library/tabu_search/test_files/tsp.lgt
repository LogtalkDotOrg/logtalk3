%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tsp,
	implements(tabu_search_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-15,
		comment is 'Small TSP instance (regular hexagon with side length 5) for testing tabu search. Distances are computed from coordinates, avoiding a large distance/3 table that would require multi-indexing for acceptable performance on some backends.'
	]).

	:- uses(fast_random(xoshiro128pp), [
		between/3
	]).

	:- uses(list, [
		nth1/3, length/2
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

	initial_state([a, b, c, d, e, f]).

	% Swap two distinct positions (a simple neighborhood operator)
	neighbor_state(Tour, Neighbor) :-
		length(Tour, N),
		between(1, N, I),
		between(1, N, J0),
		(	J0 =:= I ->
			J is (I mod N) + 1
		;	J = J0
		),
		nth1(I, Tour, CityI),
		nth1(J, Tour, CityJ),
		set_nth1(I, Tour, CityJ, T1),
		set_nth1(J, T1, CityI, Neighbor).

	set_nth1(1, [_|T], X, [X|T]) :-
		!.
	set_nth1(N, [H|T], X, [H|R]) :-
		N > 0,
		N1 is N - 1,
		set_nth1(N1, T, X, R).

	state_energy(Tour, Energy) :-
		tour_length(Tour, Energy).

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
