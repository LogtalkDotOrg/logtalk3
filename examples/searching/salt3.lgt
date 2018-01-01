%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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


/*
	Salt state-space search problem

	2003 Portuguese National Logical Programming Contest problem
	http://paginas.fe.up.pt/~eol/LP/0304/documents/Exercicios_CNPL.PDF

Introduction:
	Mr Silva sells salt. He has to measure the quantity requested by his 
	customers by using two measures and an accumulator.  Neither has any 
	measuring markers. Those measures can easily be broken and he has to 
	replace them each time it happens.  More, a substitution can be made 
	by a measure with a different capacity than the one being replaced.

Objective:
	To produce a program, given the capacity of two measures and the 
	intended quantity, which helps Mr. Silva knowing if it is possible 
	to obtain the amount requested by his customer, and if so, measuring 
	the intended quantity in the least amount of steps.

Remarks:
	This problem is similar to the Water Jug's' problem. It is more general, 
	seeing that the Water Jug's problem uses static values for the jugs 
	capacities and the final goal.
*/


:- object(salt(_Accumulator, _Measure1, _Measure2),
	instantiates(heuristic_state_space)).

	:- info([
		version is 1.13,
		author is 'Paula Marisa Sampaio',
		date is 2011/04/01,
		comment is 'Salt state-space search problem (updated from the original 1.0 version to support heuristics).',
		parnames is ['Accumulator', 'Measure1', 'Measure2']
	]).

	% each state is represented by a compound term with four arguments: (Accumulator, Measure1, Measure2, Step)
	initial_state(initial, (0, 0, 0, all_empty)).

	% the intended salt quantity must end up on the accumulator
	goal_state(accumulator, (Accumulator, _, _, _)) :-
		parameter(1, Accumulator).

	% state transitions:

	% emptying a measure into the accumulator
	next_state((Acc, X, Y, _), (NewAcc, 0, Y, transfer(m1, acc)), 1) :-
		X > 0,
		NewAcc is Acc + X.
	next_state((Acc, X, Y, _), (NewAcc, X, 0, transfer(m2, acc)), 1) :-
		Y > 0,
		NewAcc is Acc + Y.

	% filling up of one of the measures
	next_state((Acc, X, Y, Step), (Acc, MaxX, Y, fill(m1)), 1) :-
		parameter(2, MaxX),
		X < MaxX,
		Step \= empty(m1).
	next_state((Acc, X, Y, Step), (Acc, X, MaxY, fill(m2)), 1) :-
		parameter(3, MaxY),
		Y < MaxY,
		Step \= empty(m2).

	% either pouring of a measure into the other till it is filled up
	% or all content of a measure into the other one
	next_state((Acc, X, Y, _), (Acc, W, Z, transfer(m2, m1)), 1) :-
		parameter(2, MaxX),
		Y > 0,
		X < MaxX,
		(X + Y >= MaxX ->
			W = MaxX,
			Z is Y - (MaxX - X)
		;	W is X + Y,
			Z = 0
		).
	next_state((Acc, X, Y, _), (Acc, W, Z, transfer(m1, m2)), 1) :-
		parameter(3, MaxY),
		X > 0,
		Y < MaxY,
		(	X + Y >= MaxY ->
			W is X - (MaxY - Y),
			Z = MaxY
		;	W = 0,
			Z is X + Y
		).

	% throwing out the contents of a measure; does not afect the accumulator
	next_state((Acc, X, Y, Step), (Acc, 0, Y, empty(m1)), 1) :-
		X > 0,
		Step \= fill(m1).
	next_state((Acc, X, Y, Step), (Acc, X, 0, empty(m2)), 1) :-
		Y > 0,
		Step \= fill(m2).

	heuristic((Acc, Acc, _, _), 0.1) :-
		parameter(1, Acc),
		!.
	heuristic((Acc, _, Acc, _), 0.1) :-
		parameter(1, Acc),
		!.
	heuristic((Acc, X, Y, _), 0.2) :-
		parameter(1, Acc),
		Acc is abs(X - Y),
		!.
	heuristic((Acc, X, _, _), 0.3) :-
		parameter(1, Acc),
		(	X mod Acc =:= 0 ->
			Cost is X // Acc
		;	Acc mod X =:= 0 ->
			Cost is Acc // X
		),
		!.
	heuristic((Acc, _, Y, _), 0.3) :-
		parameter(1, Acc),
		(	Y mod Acc =:= 0 ->
			Cost is Y // Acc
		;	Acc mod Y =:= 0 ->
			Cost is Acc // Y
		),
		!.
	heuristic((Acc, X, Y, _), 0.4) :-
		parameter(1, Acc),
		Diff is abs(X - Y),
		(	Diff mod Acc =:= 0 ->
			Cost is Diff // Acc
		;	Acc mod Diff =:= 0 ->
			Cost is Acc // Diff
		),
		!.
	heuristic((_, _, _, _), 0.5).

	member_path((Acc, X, Y, _), [(Acc, X, Y, _)| _]) :-
		!.
	member_path(State, [_| Path]) :-
		member_path(State, Path).

	print_state((Acc, X, Y, Step)) :-
		write('('), write((Acc, X, Y)), write(')    '), write(Step), nl.

:- end_object.
