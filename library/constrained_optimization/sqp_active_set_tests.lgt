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


:- object(sqp_active_set_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Unit tests for sqp_active_set(_): a linear-equality-constrained quadratic (exact in one iteration), a genuinely nonlinear equality constraint (multiple iterations), a general inequality constraint combined with box bounds in the same subproblem, the objective(maximize) option, and the existence_error/2 raised when a problem defines a constraint but not its Jacobian.'
	]).

	cover(sqp_active_set(_)).

	:- uses(linear_algebra, [
		euclidean_norm/2, subtract_vectors/3
	]).

	% quadratic objective + linear equality constraint: the linearized
	% QP subproblem is exact, so this should converge in a single
	% iteration to (0.5, 0.5), value 0.5

	test(sqp_active_set_quadratic_equality, deterministic(Error < 1.0e-6)) :-
		sqp_active_set(sqp_test_quadratic_equality)::run(X, V, Statistics, []),
		Statistics = [iterations(Iterations)| _],
		Iterations =< 1,
		subtract_vectors(X, [0.5, 0.5], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - 0.5),
		Error is max(ErrorX, ErrorV).

	% genuinely nonlinear (curved) equality constraint, needing several
	% iterations of Jacobian relinearization and damped-BFGS updates to
	% reach (-1/sqrt(2), -1/sqrt(2)), value -sqrt(2)

	test(sqp_active_set_nonlinear_circle, deterministic(Error < 1.0e-5)) :-
		sqp_active_set(sqp_test_circle)::run(X, V, _Statistics, []),
		HalfSqrt2 is -1.0 / sqrt(2.0),
		subtract_vectors(X, [HalfSqrt2, HalfSqrt2], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - (-sqrt(2.0))),
		Error is max(ErrorX, ErrorV).

	% A loose objective-change tolerance must not terminate the solver
	% while the nonlinear equality constraint is still violated.

	test(sqp_active_set_tol_f_requires_feasibility, deterministic(Violation =< 1.0e-6)) :-
		sqp_active_set(sqp_test_circle)::run(X, _V, _Statistics, [tol_f(1.0e6), tol_constraint(1.0e-6)]),
		sqp_test_circle::equality_constraints(X, [Residual]),
		Violation is abs(Residual).

	test(sqp_active_set_target_requires_feasibility, deterministic(Violation =< 1.0e-6)) :-
		sqp_active_set(sqp_test_quadratic_equality)::run(X, _V, Statistics, [target_value(1.0), tol_constraint(1.0e-6)]),
		list::memberchk(termination_reason(target_reached), Statistics),
		sqp_test_quadratic_equality::equality_constraints(X, [Residual]),
		Violation is abs(Residual).

	test(sqp_active_set_reports_max_iterations, deterministic) :-
		sqp_active_set(sqp_test_circle)::run(_X, _V, Statistics, [max_iterations(1)]),
		list::memberchk(termination_reason(max_iterations), Statistics).

	test(sqp_active_set_initial_point_option, deterministic(Error < 1.0e-12)) :-
		sqp_active_set(initial_point_override_test)::run(X, V, _Statistics, [initial_point([0.0, 0.0])]),
		euclidean_norm(X, ErrorX),
		Error is max(ErrorX, abs(V)).

	test(sqp_active_set_initial_point_fallback, deterministic(X == [1.0, -1.0])) :-
		sqp_active_set(initial_point_fallback_test)::run(X, _V, _Statistics, [max_iterations(1)]).

	test(sqp_active_set_reports_stop_condition, deterministic) :-
		sqp_active_set(stop_condition_test)::run(_X, _V, Statistics, []),
		list::memberchk(termination_reason(stop_condition), Statistics).

	test(sqp_active_set_defined_empty_constraints, deterministic(Error < 1.0e-8)) :-
		sqp_active_set(empty_constraints_test)::run(X, V, _Statistics, []),
		euclidean_norm(X, ErrorX),
		Error is max(ErrorX, abs(V)).

	test(sqp_active_set_box_only, deterministic(Error < 1.0e-8)) :-
		sqp_active_set(box_only_test)::run([X], V, _Statistics, []),
		Error is max(abs(X - 1.0), abs(V - 1.0)).

	test(sqp_active_set_armijo_exhaustion_fails, deterministic) :-
		\+ sqp_active_set(inconsistent_gradient_test)::run(_X, _V, _Statistics, [armijo_max_backtracks(3)]).

	% general inequality constraint together with position_bounds/1 in
	% the same QP subproblem, both linear so also exact in one
	% iteration; optimum (1, 1), value 2

	test(sqp_active_set_inequality_with_bounds, deterministic(Error < 1.0e-6)) :-
		sqp_active_set(sqp_test_inequality_bounds)::run(X, V, Statistics, []),
		Statistics = [iterations(Iterations)| _],
		Iterations =< 1,
		subtract_vectors(X, [1.0, 1.0], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - 2.0),
		Error is max(ErrorX, ErrorV).

	% objective(maximize): same feasible line as
	% sqp_active_set_quadratic_equality, same optimal point, reached by
	% maximizing -(x1^2+x2^2) instead of minimizing x1^2+x2^2

	test(sqp_active_set_maximize, deterministic(Error < 1.0e-6)) :-
		sqp_active_set(sqp_test_maximize)::run(X, V, _Statistics, [objective(maximize)]),
		subtract_vectors(X, [0.5, 0.5], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - (-0.5)),
		Error is max(ErrorX, ErrorV).

	% a problem defining equality_constraints/2 but not
	% equality_jacobian/2 must raise existence_error/2, not proceed
	% silently or crash with an unrelated error

	test(sqp_active_set_missing_jacobian_raises, error(existence_error(procedure, equality_jacobian/2))) :-
		sqp_active_set(sqp_test_missing_jacobian)::run(_X, _V, _Statistics, []).

	test(sqp_active_set_invalid_constraint_values, error(domain_error(equality_constraint_data, _))) :-
		sqp_active_set(invalid_constraint_values_test)::run(_X, _V, _Statistics, []).

	test(sqp_active_set_invalid_constraint_jacobian, error(domain_error(equality_constraint_data, _))) :-
		sqp_active_set(invalid_constraint_jacobian_test)::run(_X, _V, _Statistics, []).

:- end_object.
