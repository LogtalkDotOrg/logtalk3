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


:- object(primal_dual_interior_point_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Unit tests for primal_dual_interior_point(_): a pure inequality-constrained problem starting from an INFEASIBLE point (the defining capability this solver has that log_barrier(_,_) does not), a combined equality-and-inequality problem, an inequality constraint together with box bounds, a genuinely nonlinear equality constraint, the objective(maximize) option, and the existence_error/2 raised when a problem defines a constraint but not its Jacobian.'
	]).

	cover(primal_dual_interior_point(_)).

	:- uses(linear_algebra, [
		euclidean_norm/2, subtract_vectors/3
	]).

	% pure inequality constraint, starting from an INFEASIBLE point
	% (x1+x2=6, violating x1+x2=<2 by 4): log_barrier(_,_) would raise
	% domain_error/2 on this starting point; this solver is specifically
	% designed to handle it. Optimum (1, 1), value 2.

	test(primal_dual_interior_point_infeasible_start, deterministic(Error < 1.0e-4)) :-
		primal_dual_interior_point(lb_test_infeasible_start)::run(X, V, _Statistics, []),
		subtract_vectors(X, [1.0, 1.0], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - 2.0),
		Error is max(ErrorX, ErrorV).

	% equality constraint combined with inequality constraints (the
	% same lb_test_eq_and_ineq used by log_barrier_tests.lgt); optimum
	% (1, 1, 1), value 0.5

	test(primal_dual_interior_point_equality_and_inequality, deterministic(Error < 1.0e-4)) :-
		primal_dual_interior_point(lb_test_eq_and_ineq)::run(X, V, _Statistics, []),
		subtract_vectors(X, [1.0, 1.0, 1.0], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - 0.5),
		Error is max(ErrorX, ErrorV).

	% general inequality constraint together with position_bounds/1,
	% folded directly into the same KKT system (not delegated to an
	% inner solver's own box-constraint projection, unlike
	% augmented_lagrangian(_,_)/quadratic_penalty(_,_)/log_barrier(_,_));
	% optimum (1, 1), value 2

	test(primal_dual_interior_point_inequality_with_bounds, deterministic(Error < 1.0e-4)) :-
		primal_dual_interior_point(sqp_test_inequality_bounds)::run(X, V, Statistics, []),
		list::memberchk(termination_reason(kkt_singular), Statistics),
		subtract_vectors(X, [1.0, 1.0], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - 2.0),
		Error is max(ErrorX, ErrorV).

	% genuinely nonlinear equality constraint; optimum
	% (-1/sqrt(2), -1/sqrt(2)), value -sqrt(2)

	test(primal_dual_interior_point_nonlinear_circle, deterministic(Error < 1.0e-4)) :-
		primal_dual_interior_point(sqp_test_circle)::run(X, V, _Statistics, [max_iterations(60)]),
		HalfSqrt2 is -1.0 / sqrt(2.0),
		subtract_vectors(X, [HalfSqrt2, HalfSqrt2], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - (-sqrt(2.0))),
		Error is max(ErrorX, ErrorV).

	% objective(maximize): mirrors sqp_active_set_maximize /
	% augmented_lagrangian_maximize / quadratic_penalty_maximize /
	% log_barrier_maximize

	test(primal_dual_interior_point_maximize, deterministic(Error < 1.0e-4)) :-
		primal_dual_interior_point(sqp_test_maximize)::run(X, V, _Statistics, [objective(maximize)]),
		subtract_vectors(X, [0.5, 0.5], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - (-0.5)),
		Error is max(ErrorX, ErrorV).

	test(primal_dual_interior_point_initial_point_option, deterministic(Error < 1.0e-12)) :-
		primal_dual_interior_point(initial_point_override_test)::run(X, V, _Statistics, [initial_point([0.0, 0.0])]),
		euclidean_norm(X, ErrorX),
		Error is max(ErrorX, abs(V)).

	test(primal_dual_interior_point_initial_point_option_infeasible, deterministic(Error < 1.0e-4)) :-
		primal_dual_interior_point(al_test_inequality_bounds)::run(X, V, _Statistics, [initial_point([3.0, 3.0])]),
		subtract_vectors(X, [1.0, 1.0], DiffX),
		euclidean_norm(DiffX, ErrorX),
		Error is max(ErrorX, abs(V - 2.0)).

	test(primal_dual_interior_point_initial_point_fallback, deterministic(X == [1.0, -1.0])) :-
		primal_dual_interior_point(initial_point_fallback_test)::run(X, _V, _Statistics, [max_iterations(1)]).

	test(primal_dual_interior_point_target_requires_feasibility, deterministic(Violation =< 1.0e-6)) :-
		primal_dual_interior_point(sqp_test_quadratic_equality)::run(X, _V, Statistics, [target_value(1.0), tol_constraint(1.0e-6)]),
		list::memberchk(termination_reason(target_reached), Statistics),
		sqp_test_quadratic_equality::equality_constraints(X, [Residual]),
		Violation is abs(Residual).

	test(primal_dual_interior_point_reports_max_iterations, deterministic) :-
		primal_dual_interior_point(sqp_test_circle)::run(_X, _V, Statistics, [max_iterations(1)]),
		list::memberchk(termination_reason(max_iterations), Statistics).

	test(primal_dual_interior_point_reports_stop_condition, deterministic) :-
		primal_dual_interior_point(stop_condition_test)::run(_X, _V, Statistics, []),
		list::memberchk(termination_reason(stop_condition), Statistics).

	test(primal_dual_interior_point_invalid_bounds, error(domain_error(position_bounds, _))) :-
		primal_dual_interior_point(invalid_bounds_test)::run(_X, _V, _Statistics, []).

	test(primal_dual_interior_point_defined_empty_constraints, deterministic(Error < 1.0e-8)) :-
		primal_dual_interior_point(empty_constraints_test)::run(X, V, _Statistics, []),
		euclidean_norm(X, ErrorX),
		Error is max(ErrorX, abs(V)).

	test(primal_dual_interior_point_box_only, deterministic(Error < 1.0e-4)) :-
		primal_dual_interior_point(box_only_test)::run([X], V, _Statistics, []),
		Error is max(abs(X - 1.0), abs(V - 1.0)).

	% a problem defining equality_constraints/2 but not
	% equality_jacobian/2 must raise existence_error/2

	test(primal_dual_interior_point_missing_jacobian_raises, error(existence_error(procedure, equality_jacobian/2))) :-
		primal_dual_interior_point(sqp_test_missing_jacobian)::run(_X, _V, _Statistics, []).

	test(primal_dual_interior_point_invalid_constraint_jacobian, error(domain_error(equality_constraint_data, _))) :-
		primal_dual_interior_point(invalid_constraint_jacobian_test)::run(_X, _V, _Statistics, []).

:- end_object.
