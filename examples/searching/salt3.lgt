%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(salt(_Accumulator_, _Measure1_, _Measure2_),
	instantiates(heuristic_state_space)).

	:- info([
		version is 1:16:0,
		author is 'Paula Marisa Sampaio',
		date is 2021-02-07,
		comment is 'Salt state-space search problem (updated from the original 1.0 version to support heuristics).',
		parnames is ['Accumulator', 'Measure1', 'Measure2']
	]).

	% each state is represented by a compound term with four arguments: (Accumulator, Measure1, Measure2, Step)
	initial_state(initial, s(0, 0, 0, all_empty)).

	% the intended salt quantity must end up on the accumulator
	goal_state(accumulator, s(_Accumulator_, _, _, _)).

	% state transitions:

	% emptying a measure into the accumulator
	next_state(s(Acc, X, Y, _), s(NewAcc, 0, Y, transfer(m1, acc)), 1) :-
		X > 0,
		NewAcc is Acc + X.
	next_state(s(Acc, X, Y, _), s(NewAcc, X, 0, transfer(m2, acc)), 1) :-
		Y > 0,
		NewAcc is Acc + Y.

	% filling up of one of the measures
	next_state(s(Acc, X, Y, Step), s(Acc, _Measure1_, Y, fill(m1)), 1) :-
		X < _Measure1_,
		Step \= empty(m1).
	next_state(s(Acc, X, Y, Step), s(Acc, X, _Measure2_, fill(m2)), 1) :-
		Y < _Measure2_,
		Step \= empty(m2).

	% either pouring of a measure into the other till it is filled up
	% or all content of a measure into the other one
	next_state(s(Acc, X, Y, _), s(Acc, W, Z, transfer(m2, m1)), 1) :-
		Y > 0,
		X < _Measure1_,
		(X + Y >= _Measure1_ ->
			W = _Measure1_,
			Z is Y - (_Measure1_ - X)
		;	W is X + Y,
			Z = 0
		).
	next_state(s(Acc, X, Y, _), s(Acc, W, Z, transfer(m1, m2)), 1) :-
		X > 0,
		Y < _Measure2_,
		(	X + Y >= _Measure2_ ->
			W is X - (_Measure2_ - Y),
			Z = _Measure2_
		;	W = 0,
			Z is X + Y
		).

	% throwing out the contents of a measure; does not affect the accumulator
	next_state(s(Acc, X, Y, Step), s(Acc, 0, Y, empty(m1)), 1) :-
		X > 0,
		Step \= fill(m1).
	next_state(s(Acc, X, Y, Step), s(Acc, X, 0, empty(m2)), 1) :-
		Y > 0,
		Step \= fill(m2).

	heuristic(s(_Accumulator_, _Accumulator_, _, _), 0.1) :-
		!.
	heuristic(s(_Accumulator_, _, _Accumulator_, _), 0.1) :-
		!.
	heuristic(s(_Accumulator_, X, Y, _), 0.2) :-
		_Accumulator_ is abs(X - Y),
		!.
	heuristic(s(_Accumulator_, X, _, _), 0.3) :-
		(	X mod _Accumulator_ =:= 0
		;	_Accumulator_ mod X =:= 0
		),
		!.
	heuristic(s(_Accumulator_, _, Y, _), 0.3) :-
		(	Y mod _Accumulator_ =:= 0
		;	_Accumulator_ mod Y =:= 0
		),
		!.
	heuristic(s(_Accumulator_, X, Y, _), 0.4) :-
		Diff is abs(X - Y),
		(	Diff mod _Accumulator_ =:= 0
		;	_Accumulator_ mod Diff =:= 0
		),
		!.
	heuristic(s(_, _, _, _), 0.5).

	member_path(s(Acc, X, Y, _), [s(Acc, X, Y, _)| _]) :-
		!.
	member_path(State, [_| Path]) :-
		member_path(State, Path).

	print_state(s(Acc, X, Y, Step)) :-
		write(s(Acc, X, Y)), write(' - '), write(Step), nl.

:- end_object.
