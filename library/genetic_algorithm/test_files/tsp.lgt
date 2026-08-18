%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tsp,
	implements(genetic_algorithm_problem_protocol)).

	:- info([
		version is 1:0:5,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Small TSP instance (regular hexagon with side length 5) for testing the genetic algorithm. Distances are computed from coordinates, avoiding a large distance/3 table that would require multi-indexing for acceptable performance on some backends. Uses ordered crossover (OX) and swap mutation.'
	]).

	:- uses(fast_random(xoshiro128pp), [
		between/3, permutation/2
	]).

	:- uses(list, [
		nth1/3, selectchk/3, length/2, member/2, append/3, take/4
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
		City1 \== City2,
		DeltaX is X2 - X1,
		DeltaY is Y2 - Y1,
		Distance is sqrt(DeltaX*DeltaX + DeltaY*DeltaY).

	random_individual(Tour) :-
		permutation([a, b, c, d, e, f], Tour).

	state_energy(Tour, Energy) :-
		tour_length(Tour, Energy).

	tour_length([First| Rest], Length) :-
		tour_length_(Rest, First, First, 0.0, Length).

	tour_length_([], Last, First, Accumulator, Length) :-
		distance(Last, First, Distance),
		Length is Accumulator + Distance.
	tour_length_([Next| Rest], Previous, First, Accumulator, Length) :-
		distance(Previous, Next, Distance),
		Accumulator1 is Accumulator + Distance,
		tour_length_(Rest, Next, First, Accumulator1, Length).

	% Ordered crossover (OX): copy a contiguous segment from the first parent,
	% then fill remaining positions with cities from the second parent in the
	% order they appear there, skipping cities already taken by the segment.
	crossover(Parent1, Parent2, Offspring1, Offspring2) :-
		length(Parent1, Length),
		between(1, Length, Cut1),
		between(1, Length, Cut2),
		Min is min(Cut1, Cut2),
		Max is max(Cut1, Cut2),
		ox(Parent1, Parent2, Min, Max, Offspring1),
		ox(Parent2, Parent1, Min, Max, Offspring2).

	ox(ParentA, ParentB, Start, End, Child) :-
		length(ParentA, Length),
		segment(ParentA, Start, End, Segment),
		exclude_segment(ParentB, Segment, Remaining),
		Before is Start - 1,
		take(Before, Remaining, Prefix, Suffix),
		append(Prefix, Segment, Partial),
		append(Partial, Suffix, Child),
		length(Child, Length).

	segment(List, Start, End, Segment) :-
		segment_(List, 1, Start, End, Segment).

	segment_(_, Position, _, End, []) :-
		Position > End,
		!.
	segment_([_| Cities], Position, Start, End, Segment) :-
		Position < Start,
		!,
		Position1 is Position + 1,
		segment_(Cities, Position1, Start, End, Segment).
	segment_([City| Cities], Position, Start, End, [City| Segment]) :-
		Position1 is Position + 1,
		segment_(Cities, Position1, Start, End, Segment).

	exclude_segment([], _, []).
	exclude_segment([City| Cities], Segment, Remaining) :-
		(	member(City, Segment) ->
			exclude_segment(Cities, Segment, Remaining)
		;	Remaining = [City| Remaining1],
			exclude_segment(Cities, Segment, Remaining1)
		).

	% Swap mutation - swap two distinct positions when the tour is long enough
	mutate(Tour, Tour) :-
		length(Tour, Length),
		Length < 2,
		!.
	mutate(Tour, Mutated) :-
		length(Tour, Length),
		between(1, Length, Index1),
		Length1 is Length - 1,
		between(1, Length1, Index0),
		(	Index0 >= Index1 ->
			Index2 is Index0 + 1
		;	Index2 = Index0
		),
		nth1(Index1, Tour, City1),
		nth1(Index2, Tour, City2),
		set_nth(Index1, Tour, City2, Temporary),
		set_nth(Index2, Temporary, City1, Mutated).

	set_nth(1, [_| Tail], Element, [Element| Tail]) :-
		!.
	set_nth(Index, [Head| Tail], Element, [Head| Rest]) :-
		Index > 1,
		Index1 is Index - 1,
		set_nth(Index1, Tail, Element, Rest).

:- end_object.
