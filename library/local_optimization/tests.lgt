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
		date is 2026-09-03,
		comment is 'Unit tests for local_optimization solvers. Parameterized by the solver functor (e.g. nelder_mead).',
		parnames is ['Solver']
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(local_optimization_solver(_)).
	cover(barzilai_borwein(_)).
	cover(bfgs(_)).
	cover(conjugate_gradient(_)).
	cover(gradient_descent(_)).
	cover(lbfgs(_)).
	cover(nelder_mead(_)).
	cover(trust_region_newton_cg(_)).

	% basic runs

	test(local_opt_run_2, deterministic((list::valid(Point), number(Value)))) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(Point, Value).

	test(local_opt_run_3, deterministic((list::valid(Point), number(Value)))) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(Point, Value, [max_iterations(50)]).

	test(local_opt_sphere_improves, deterministic(Value < 20.0)) :-
		% start at (3,4) -> f=25; after optimization should be clearly better
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

	test(nelder_mead_upper_bound_start, deterministic(Value < 0.1), [condition(nelder_mead_solver)]) :-
		Solver =.. [_Solver_, upper_bound_quadratic],
		Solver::run(_Point, Value, [max_iterations(100)]).

	test(nelder_mead_one_dimensional_outside_contraction, deterministic(SimplexSize < 0.75), [condition(nelder_mead_solver)]) :-
		Solver =.. [_Solver_, one_dimensional_quadratic],
		Solver::run(_Point, _Value, Statistics, [
			initial_step(1.0), max_iterations(1), tol_x(0.0), tol_f(0.0)
		]),
		memberchk(final_simplex_size(SimplexSize), Statistics).

	test(nelder_mead_shrink_evaluations, deterministic(Evaluations == 5), [condition(nelder_mead_solver)]) :-
		Solver =.. [_Solver_, one_dimensional_shrink],
		Solver::run(_Point, _Value, Statistics, [
			initial_step(1.0), max_iterations(1), tol_x(0.0), tol_f(0.0)
		]),
		memberchk(evaluations(Evaluations), Statistics).

	test(nelder_mead_adaptive_coefficients, deterministic(AdaptiveValue =\= StandardValue), [condition(nelder_mead_solver)]) :-
		Solver =.. [_Solver_, three_dimensional_sphere],
		Options = [max_iterations(1), tol_x(0.0), tol_f(0.0)],
		Solver::run(_StandardPoint, StandardValue, [adaptive(false)| Options]),
		Solver::run(_AdaptivePoint, AdaptiveValue, [adaptive(true)| Options]).

	test(nelder_mead_orders_by_objective_value, deterministic(Value < 0.1), [condition(nelder_mead_solver)]) :-
		Solver =.. [_Solver_, shifted_one_dimensional_quadratic],
		Solver::run(_Point, Value, [
			initial_step(0.5), max_iterations(1), tol_x(0.0), tol_f(0.0)
		]).

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

	% warm-start (initial_point/1 option)

	test(local_opt_warm_start_uses_point, deterministic) :-
		% start already at the unconstrained minimum; a short run must remain near it
		% (Nelder-Mead builds a small simplex around the point, so a tiny residual is expected)
		Solver =.. [_Solver_, sphere],
		Solver::run(Point, Value, [
			initial_point([0.0, 0.0]),
			max_iterations(5),
			tol_x(0.0), tol_f(0.0), tol_g(0.0)
		]),
		^^assertion(Value < 1.0e-3),
		Point = [X, Y],
		^^assertion(abs(X) < 0.1),
		^^assertion(abs(Y) < 0.1).

	test(local_opt_warm_start_overrides_problem, deterministic(Value < 1.0)) :-
		% problem default start is (3,4) with f=25; warm-start near the origin
		Solver =.. [_Solver_, sphere],
		Solver::run(_Point, Value, [
			initial_point([0.1, 0.1]),
			max_iterations(50)
		]).

	test(local_opt_warm_start_omitted_uses_problem, deterministic(Value < 20.0)) :-
		% no initial_point option -> same behaviour as the original sphere test
		Solver =.. [_Solver_, sphere],
		Solver::run(_Point, Value, [max_iterations(200)]).

	test(local_opt_warm_start_out_of_bounds, error(domain_error(initial_point, [2.0, 2.0]))) :-
		Solver =.. [_Solver_, bounded_sphere],
		Solver::run(_Point, _Value, [initial_point([2.0, 2.0])]).

	test(local_opt_warm_start_empty_point, error(domain_error(option, initial_point([])))) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(_Point, _Value, [initial_point([])]).

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

	test(local_opt_invalid_initial_point_option, error(domain_error(option, initial_point(foo)))) :-
		Solver =.. [_Solver_, sphere_stop],
		Solver::run(_Point, _Value, [initial_point(foo)]).

	% problem validation

	test(local_opt_invalid_bounds, error(domain_error(position_bounds, [1.0-(-1.0), (-5.0)-5.0]))) :-
		Solver =.. [_Solver_, malformed_problem(invalid_bounds)],
		Solver::run(_Point, _Value).

	% auxiliary predicates

	nelder_mead_solver :-
		_Solver_ == nelder_mead.

	no_gradient_solver :-
		_Solver_ \== barzilai_borwein,
		_Solver_ \== bfgs,
		_Solver_ \== conjugate_gradient,
		_Solver_ \== gradient_descent,
		_Solver_ \== lbfgs,
		_Solver_ \== lbfgs_b,
		_Solver_ \== trust_region_newton_cg.

:- end_object.
