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
