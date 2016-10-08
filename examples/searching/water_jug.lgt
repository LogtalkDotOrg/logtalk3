%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(water_jug,
	instantiates(state_space)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura. Next step predicate comments contributed by Michael Igler.',
		date is 2009/9/6,
		comment is 'Water jug state space search problem.'
	]).

	% states are represented by the compound term (4-gallon jug, 3-gallon jug);
	% in the initial state both jugs are empty:
	initial_state(start, (0, 0)).

	% the goal state is to measure 2 gallons of water:
	goal_state(end1, (2, 0)).
	goal_state(end2, (0, 2)).

	% fill up the 4-gallon jug if it is not already filled:
	next_state((X, Y), (4, Y)) :-
		X < 4.

	% fill up the 3-gallon jug if it is not already filled:
	next_state((X, Y),(X, 3)) :-
		Y < 3.

	% if there is water in the 3-gallon jug Y > 0) and there is room in the 4-gallon jug (X < 4) THEN use it to fill up
	% the 4-gallon jug until it is full	(4-gallon jug = 4 in the new state) and leave the rest in the 3-gallon jug:
	next_state((X, Y), (4, Z)) :-
		Y > 0, X < 4,
		Aux is X + Y, Aux >= 4,
		Z is Y - (4 - X).

	% if there is water in the 4-gallon jug (X > 0) and there is room in the 3-gallon jug (Y < 3) THEN use it to fill up
	% the 3-gallon jug until it is full	(3-gallon jug = 3 in the new state) and leave the rest in the 4-gallon jug:
	next_state((X, Y), (Z, 3)) :-
		X > 0, Y < 3,
		Aux is X + Y, Aux >= 3,
		Z is X - (3 - Y).

	% there is something in the 3-gallon jug (Y > 0) and together with the amount in the 4-gallon jug it fits in the
	% 4-gallon jug (Aux is X + Y, Aux =< 4) THEN fill it all (Y is 0 in the new state) into the 4-gallon jug (Z is Y + X):
	next_state((X, Y),(Z, 0)) :-
		Y > 0,
		Aux is X + Y, Aux =< 4,
		Z is Y + X.

	% there is something in the 4-gallon jug (X > 0) and together with the amount in the 3-gallon jug it fits in the
	% 3-gallon jug (Aux is X + Y, Aux =< 3) THEN fill it all (X is 0 in the new state) into the 3-gallon jug (Z is Y + X):
	next_state((X, Y),(0, Z)) :-
		X > 0,
		Aux is X + Y, Aux =< 3,
		Z is Y + X.

	% empty the 4-gallon jug IF it is not already empty	(X > 0):
	next_state((X, Y), (0, Y)) :-
		X > 0.

	% empty the 3-gallon jug IF it is not already empty	(Y > 0):
	next_state((X, Y), (X, 0)) :-
		Y > 0.

	print_state((X, Y)) :-
		write('4-gallon jug: '), write(X), nl,
		write('3-gallon jug: '), write(Y), nl, nl.

:- end_object.
