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


:- object(tests(_Solver_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'Unit tests for local_optimization solvers. Parameterized by the solver functor (e.g. nelder_mead).',
		parnames is ['Solver']
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(bfgs(_)).
	cover(conjugate_gradient(_)).
	cover(gradient_descent(_)).
	cover(nelder_mead(_)).

	% basic runs

	test(local_opt_run_2, deterministic((list::valid(Point), number(Value)))) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(Point, Value).

	test(local_opt_run_3, deterministic((list::valid(Point), number(Value)))) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(Point, Value, [max_iterations(50)]).

	test(local_opt_sphere_improves, deterministic(Value < 20.0)) :-
		% start at (3,4) → f=25; after optimization should be clearly better
		Solver =.. [_Solver_, sphere],
		Solver::run(_Point, Value, [max_iterations(200)]).

	% statistics

	test(local_opt_statistics, deterministic) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(_Point, _Value, Statistics, [max_iterations(30)]),
		memberchk(iterations(Iterations), Statistics),
		^^assertion(Iterations =< 30),
		memberchk(evaluations(Evaluations), Statistics),
		^^assertion(Evaluations > 0),
		memberchk(final_value(FinalValue), Statistics),
		^^assertion(number(FinalValue)).

	test(local_opt_max_iterations, deterministic(Iterations =:= 10)) :-
		Solver =.. [_Solver_, rosenbrock],
		Solver::run(_Point, _Value, Statistics, [
			max_iterations(10), tol_x(0.0), tol_f(0.0)
		]),
		memberchk(iterations(Iterations), Statistics).

	% objective direction

	test(local_opt_explicit_minimize, deterministic(Value < 20.0)) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(_Point, Value, [
			objective(minimize), max_iterations(100)
		]).

	test(local_opt_maximize, deterministic(Value > -20.0)) :-
		Solver =.. [_Solver_, negative_sphere],
		Solver::run(_Point, Value, [
			objective(maximize), max_iterations(100)
		]).

	% target value stopping

	test(local_opt_target_value, deterministic((Iterations =< 200, Value =< 5.0))) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(_Point, Value, Statistics, [
			max_iterations(200), target_value(5.0)
		]),
		memberchk(iterations(Iterations), Statistics).

	test(local_opt_target_unreachable, deterministic(Iterations =:= 15)) :-
		Solver =.. [_Solver_, rosenbrock],
		Solver::run(_Point, _Value, Statistics, [
			max_iterations(15), target_value(-1.0), tol_x(0.0), tol_f(0.0)
		]),
		memberchk(iterations(Iterations), Statistics).

	% custom stop condition

	test(local_opt_custom_stop_condition, deterministic(Iterations =:= 5)) :-
		Solver =.. [_Solver_, rosenbrock_stop],
		Solver::run(_Point, _Value, Statistics, [
			max_iterations(100), tol_x(0.0), tol_f(0.0)
		]),
		memberchk(iterations(Iterations), Statistics).

	% box constraints

	test(local_opt_bounded_sphere, deterministic) :-
		Solver =.. [_Solver_, bounded_sphere],
		Solver::run(Point, Value, [max_iterations(100)]),
		Point = [X, Y],
		^^assertion((X >= -1.0, X =< 1.0, Y >= -1.0, Y =< 1.0)),
		^^assertion(Value >= 0.0).

	% progress reporting

	test(local_opt_progress_updates_zero, deterministic(Count =:= 0), [condition(no_gradient_solver)]) :-
		sphere_progress::clear_log,
		Solver =.. [_Solver_, sphere_progress],
		Solver::run(_Point, _Value, [
			max_iterations(30), updates(0)
		]),
		findall(1, sphere_progress::progress_log(_, _, _, _, _), Logs),
		length(Logs, Count).

	test(local_opt_progress_updates_count, deterministic(Count > 0), [condition(no_gradient_solver)]) :-
		sphere_progress::clear_log,
		Solver =.. [_Solver_, sphere_progress],
		Solver::run(_Point, _Value, [
			max_iterations(50), updates(5), tol_x(0.0), tol_f(0.0)
		]),
		findall(1, sphere_progress::progress_log(_, _, _, _, _), Logs),
		length(Logs, Count).

	test(local_opt_progress_values, deterministic, [condition(no_gradient_solver)]) :-
		sphere_progress::clear_log,
		Solver =.. [_Solver_, sphere_progress],
		Solver::run(_Point, _Value, [
			max_iterations(50), updates(5), tol_x(0.0), tol_f(0.0)
		]),
		once(sphere_progress::progress_log(Iteration, BestPoint, BestValue, Measure, Evaluations)),
		^^assertion(Iteration > 0),
		^^assertion(list::valid(BestPoint)),
		^^assertion(number(BestValue)),
		^^assertion(number(Measure)),
		^^assertion(Evaluations > 0).

	% option validation

	test(local_opt_invalid_max_iterations, error(domain_error(option, max_iterations(0)))) :-
		Solver =.. [_Solver_, sphere_stop],
		Solver::run(_Point, _Value, [max_iterations(0)]).

	test(local_opt_invalid_objective, error(domain_error(option, objective(optimize)))) :-
		Solver =.. [_Solver_, sphere_stop],
		Solver::run(_Point, _Value, [objective(optimize)]).

	test(local_opt_invalid_tol_x, error(domain_error(option, tol_x(-1.0)))) :-
		Solver =.. [_Solver_, sphere_stop],
		Solver::run(_Point, _Value, [tol_x(-1.0)]).

	test(local_opt_invalid_updates, error(domain_error(option, updates(-1)))) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(_Point, _Value, [updates(-1)]).

	% problem validation

	test(local_opt_invalid_bounds, error(domain_error(position_bounds, [1.0-(-1.0), (-5.0)-5.0]))) :-
		Solver =.. [_Solver_, malformed_problem(invalid_bounds)],
		Solver::run(_Point, _Value).

	% auxiliary predicates

	no_gradient_solver :-
		_Solver_ \== bfgs,
		_Solver_ \== conjugate_gradient,
		_Solver_ \== gradient_descent.

:- end_object.
