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


:- object(rosenbrock,
	implements(local_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is '2-D Rosenbrock function (minimization). Global minimum at (1,1) with value 0.'
	]).

	initial_point([-1.2, 1.0]).

	objective([X, Y], Value) :-
		Value is 100*(Y - X*X)^2 + (1 - X)^2.

	gradient([X, Y], [GX, GY]) :-
		GX is -400*X*(Y - X*X) - 2*(1 - X),
		GY is  200*(Y - X*X).

:- end_object.


:- object(rosenbrock_stop,
	implements(local_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is '2-D Rosenbrock function (minimization). Global minimum at (1,1) with value 0.'
	]).

	initial_point([-1.2, 1.0]).

	objective([X, Y], Value) :-
		Value is 100*(Y - X*X)^2 + (1 - X)^2.

	% analytic gradient for future gradient-based solvers
	gradient([X, Y], [GX, GY]) :-
		GX is -400*X*(Y - X*X) - 2*(1 - X),
		GY is  200*(Y - X*X).

	stop_condition(Iteration, _BestPoint, _BestValue) :-
		Iteration >= 5.

:- end_object.


:- object(sphere,
	implements(local_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is '2-D sphere function (minimization). Global minimum at (0,0) with value 0.'
	]).

	initial_point([3.0, 4.0]).

	objective([X, Y], Value) :-
		Value is X*X + Y*Y.

	gradient([X, Y], [GX, GY]) :-
		GX is 2*X,
		GY is 2*Y.

:- end_object.


:- object(negative_sphere,
	implements(local_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'Negated 2-D sphere (for maximization tests). Global maximum at (0,0) with value 0.'
	]).

	initial_point([3.0, 4.0]).

	objective([X, Y], Value) :-
		Value is -(X*X + Y*Y).

	gradient([X, Y], [GX, GY]) :-
		GX is -2*X,
		GY is -2*Y.

:- end_object.


:- object(bounded_sphere,
	implements(local_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'Sphere restricted to [-1,1]^2. Starting point outside the unconstrained minimum.'
	]).

	initial_point([0.8, 0.8]).

	position_bounds([(-1.0)-1.0, (-1.0)-1.0]).

	objective([X, Y], Value) :-
		Value is X*X + Y*Y.

	gradient([X, Y], [GX, GY]) :-
		GX is 2*X,
		GY is 2*Y.

:- end_object.


:- object(sphere_stop,
	implements(local_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'Sphere with a custom stop condition after 5 iterations.'
	]).

	initial_point([3.0, 4.0]).

	objective([X, Y], Value) :-
		Value is X*X + Y*Y.

	gradient([X, Y], [GX, GY]) :-
		GX is 2*X,
		GY is 2*Y.

	stop_condition(Iteration, _BestPoint, _BestValue) :-
		Iteration >= 5.

:- end_object.


:- object(sphere_no_gradient,
	implements(local_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'Sphere without gradient/2 - used to test gradient-based solvers error handling.'
	]).

	initial_point([3.0, 4.0]).

	objective([X, Y], Value) :-
		Value is X*X + Y*Y.

:- end_object.


:- object(sphere_progress,
	implements(local_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'Sphere that records progress/5 calls for testing.'
	]).

	:- public(clear_log/0).
	:- public(progress_log/5).

	:- private(log_/5).
	:- dynamic(log_/5).

	initial_point([3.0, 4.0]).

	objective([X, Y], Value) :-
		Value is X*X + Y*Y.

	progress(Iteration, BestPoint, BestValue, Measure, Evaluations) :-
		assertz(log_(Iteration, BestPoint, BestValue, Measure, Evaluations)).

	clear_log :-
		retractall(log_(_, _, _, _, _)).

	progress_log(Iteration, BestPoint, BestValue, Measure, Evaluations) :-
		log_(Iteration, BestPoint, BestValue, Measure, Evaluations).

:- end_object.


:- object(malformed_problem(_Kind_),
	implements(local_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'Intentionally malformed problem for error-testing.',
		parameters is ['Kind' - 'Kind of malformation.']
	]).

	initial_point(Point) :-
		(	_Kind_ == empty_point ->
			Point = []
		;	_Kind_ == out_of_bounds_point ->
			Point = [2.0, 2.0]
		;	Point = [1.0, 1.0]
		).

	position_bounds(Bounds) :-
		(	_Kind_ == invalid_bounds ->
			Bounds = [1.0-(-1.0), (-5.0)-5.0]
		;	_Kind_ == out_of_bounds_point ->
			Bounds = [(-1.0)-1.0, (-1.0)-1.0]
		;	fail
		).

	objective([X, Y], Value) :-
		Value is X*X + Y*Y.

:- end_object.
