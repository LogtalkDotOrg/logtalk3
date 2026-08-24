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


:- protocol(local_optimization_problem_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'Protocol for continuous local-optimization problem definitions. A problem object must define the required predicates and may optionally define gradient, Hessian, bounds, stopping, and progress predicates. The same problem object can be used by any solver in the local_optimization library and by the existing global metaheuristics (PSO, DE, ...).',
		see_also is [
			local_optimization_solver(_), barzilai_borwein(_), bfgs(_), conjugate_gradient(_),
			gradient_descent(_), lbfgs(_), lbfgs_b(_), nelder_mead(_), trust_region_newton_cg(_)
		]
	]).

	% required predicates

	:- public(initial_point/1).
	:- mode(initial_point(-list(number)), one).
	:- info(initial_point/1, [
		comment is 'Returns a starting point for the local search. The point must be a non-empty list of numbers. When position bounds are defined, the point must lie inside those bounds.',
		argnames is ['Point']
	]).

	:- public(objective/2).
	:- mode(objective(+list(number), -number), one).
	:- info(objective/2, [
		comment is 'Computes the objective (cost) value of a point. Solvers minimize this value by default; use the ``objective(maximize)`` option to maximize it instead. The returned value must be a number.',
		argnames is ['Point', 'Value']
	]).

	% optional predicates: first- and second-order information

	:- public(gradient/2).
	:- mode(gradient(+list(number), -list(number)), zero_or_one).
	:- info(gradient/2, [
		comment is 'Optional. Computes the gradient of the objective at a point. Required by gradient-based solvers (gradient descent, conjugate gradient, BFGS, ...). When not defined those solvers raise an existence error. The returned vector must have the same length as the point.',
		argnames is ['Point', 'Gradient']
	]).

	:- public(hessian/2).
	:- mode(hessian(+list(number), -list(list(number))), zero_or_one).
	:- info(hessian/2, [
		comment is 'Optional for most solvers. Computes the Hessian matrix of the objective at a point (list of rows). Required by trust-region Newton-CG; when not defined that solver raises an existence error. Not used by any other solver.',
		argnames is ['Point', 'Hessian']
	]).

	% optional predicates: box constraints

	:- public(position_bounds/1).
	:- mode(position_bounds(-list(pair)), zero_or_one).
	:- info(position_bounds/1, [
		comment is 'Optional. Returns one ``Lower-Upper`` numeric bound pair per dimension, with ``Lower =< Upper``. When defined, solvers that support bounds project or clamp trial points onto the box. When not defined the search is treated as unbounded.',
		argnames is ['Bounds']
	]).

	% optional predicates: stopping and progress

	:- public(stop_condition/3).
	:- mode(stop_condition(+non_negative_integer, +list(number), +number), zero_or_one).
	:- info(stop_condition/3, [
		comment is 'Optional. True when the search should stop given the completed iteration count, the current best point, and its objective value. When not defined the solver runs until its own termination criteria (maximum iterations, tolerances, ...) are met.',
		argnames is ['Iteration', 'BestPoint', 'BestValue']
	]).

	:- public(progress/5).
	:- mode(progress(+non_negative_integer, +list(number), +number, +number, +number), zero_or_one).
	:- info(progress/5, [
		comment is 'Optional. Called periodically to report progress. Arguments are the completed iteration count, current best point, best objective value, a solver-specific measure of step size or gradient norm, and the number of objective evaluations so far. A final call is made when the solver terminates if progress reporting is enabled.',
		argnames is ['Iteration', 'BestPoint', 'BestValue', 'Measure', 'Evaluations']
	]).

:- end_protocol.
