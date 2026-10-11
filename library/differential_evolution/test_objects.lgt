%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(sphere,
	implements(differential_evolution_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-13,
		comment is 'Simple 2-D sphere function (minimization).'
	]).

	position_bounds([(-5.0)-5.0, (-5.0)-5.0]).

	fitness([X, Y], Fitness) :-
		Fitness is X*X + Y*Y.

:- end_object.


:- object(differential_evolution_fixture(_Case_),
	implements(differential_evolution_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-11,
		comment is 'Fixed Differential Evolution validation and selection fixtures.',
		parameters is ['Case' - 'Fixture case.']
	]).

	position_bounds(Bounds) :-
		(	bounds(_Case_, Bounds) ->
			true
		;	Bounds = [0-1]
		).

	initial_positions(Positions) :-
		positions(_Case_, Positions).

	fitness(Position, Fitness) :-
		(	_Case_ == fitness ->
			Fitness = invalid
		;	_Case_ == large_fitness ->
			Fitness = 1.0e308
		;	_Case_ == mapped ->
			Position = [Fitness]
		;	_Case_ == later_fitness, Position == [1] ->
			Fitness = invalid
		;	Fitness = 1
		).

	bounds(empty, []).
	bounds(variable, _).
	bounds(open, [0-1| _]).
	bounds(improper, [0-1| invalid]).
	bounds(pair, [invalid]).
	bounds(numeric, [0-invalid]).
	bounds(inverted, [0-1, 2-1]).

	positions(supplied, [[0], [1], [0], [1]]).
	positions(positions_empty, []).
	positions(positions_count, [[0]]).
	positions(positions_improper, [[0]| invalid]).
	positions(positions_variable, [[_], [0], [0], [0]]).
	positions(positions_dimension, [[0,0], [0], [0], [0]]).
	positions(positions_numeric, [[invalid], [0], [0], [0]]).
	positions(positions_bounds, [[2], [0], [0], [0]]).

:- end_object.


:- object(negative_sphere,
	implements(differential_evolution_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-13,
		comment is 'Negated 2-D sphere function (for maximization tests).'
	]).

	position_bounds([(-5.0)-5.0, (-5.0)-5.0]).

	fitness([X, Y], Fitness) :-
		Fitness is -(X*X + Y*Y).

:- end_object.


:- object(constant_fitness,
	implements(differential_evolution_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-13,
		comment is 'Constant fitness landscape (for stagnation tests).'
	]).

	position_bounds([(-1.0)-1.0, (-1.0)-1.0]).

	fitness([_, _], 1.0).

:- end_object.


:- object(sphere_stop,
	implements(differential_evolution_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-13,
		comment is 'Sphere problem with a custom stop condition after 5 generations.'
	]).

	position_bounds([(-5.0)-5.0, (-5.0)-5.0]).

	fitness([X, Y], Fitness) :-
		Fitness is X*X + Y*Y.

	stop_condition(Generation, _BestPosition, _BestFitness) :-
		Generation >= 5.

:- end_object.


:- object(sphere_progress,
	implements(differential_evolution_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-13,
		comment is 'Sphere problem that records ``progress/5`` calls for testing.'
	]).

	:- public(clear_log/0).
	:- public(progress_log/5).

	:- private(log_/5).
	:- dynamic(log_/5).

	position_bounds([(-5.0)-5.0, (-5.0)-5.0]).

	fitness([X, Y], Fitness) :-
		Fitness is X*X + Y*Y.

	progress(Generation, BestPosition, BestFitness, MeanFitness, Diversity) :-
		assertz(log_(Generation, BestPosition, BestFitness, MeanFitness, Diversity)).

	clear_log :-
		retractall(log_(_, _, _, _, _)).

	progress_log(Generation, BestPosition, BestFitness, MeanFitness, Diversity) :-
		log_(Generation, BestPosition, BestFitness, MeanFitness, Diversity).

:- end_object.


:- object(malformed_problem(_Kind_),
	implements(differential_evolution_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-13,
		comment is 'Intentionally malformed problem for error-testing.',
		parameters is ['Kind' - 'Kind of malformation.']
	]).

	position_bounds(Bounds) :-
		(	_Kind_ == invalid_bounds ->
			Bounds = [1.0-(-1.0), (-5.0)-5.0]
		;	Bounds = [(-5.0)-5.0, (-5.0)-5.0]
		).

	fitness([X, Y], Fitness) :-
		Fitness is X*X + Y*Y.

:- end_object.


:- object(differential_evolution_reporting,
	extends(sphere_progress)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-11,
		comment is 'Differential Evolution constant-fitness early-reporting fixture.'
	]).

	fitness(_, 1).

	stop_condition(Generation, _, _) :-
		Generation >= 3.

:- end_object.


:- object(differential_evolution_benchmark(_Dimension_, _Cost_),
	implements(differential_evolution_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-11,
		comment is 'Differential Evolution evaluation-order and performance fixtures.',
		parameters is ['Dimension' - 'Number of coordinates.', 'Cost' - 'Fitness workload.']
	]).

	:- public([clear_log/0, evaluations/1]).
	:- private(evaluation_/2).
	:- dynamic(evaluation_/2).

	position_bounds(Bounds) :-
		bounds(_Dimension_, Bounds).

	fitness(Position, Fitness) :-
		(	_Cost_ == constant ->
			Fitness = 1
		;	squares(Position, 0.0, Fitness),
			(	_Cost_ == counted ->
				fast_random(xoshiro128pp)::get_seed(State),
				assertz(evaluation_(Position, State))
			;	_Cost_ == expensive ->
				workload(100, Fitness, _)
			;	true
			)
		).

	clear_log :-
		retractall(evaluation_(_, _)).

	evaluations(Calls) :-
		findall(Position-State, evaluation_(Position, State), Calls).

	bounds(0, []) :-
		!.
	bounds(Dimension, [(-5.0)-5.0| Bounds]) :-
		Dimension1 is Dimension - 1,
		bounds(Dimension1, Bounds).

	squares([], Fitness, Fitness) :-
		!.
	squares([X| Xs], Sum0, Fitness) :-
		Sum1 is Sum0 + X*X,
		squares(Xs, Sum1, Fitness).

	workload(0, Value, Value) :-
		!.
	workload(Count, Value0, Value) :-
		Value1 is sqrt(Value0 + 1.0),
		Count1 is Count - 1,
		workload(Count1, Value1, Value).

:- end_object.
