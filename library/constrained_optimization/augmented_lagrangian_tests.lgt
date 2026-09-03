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


:- object(augmented_lagrangian_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Unit tests for augmented_lagrangian(_,_): a linear-equality-constrained quadratic with bfgs as the inner solver, a genuinely nonlinear equality constraint with lbfgs as the inner solver, an inequality constraint that is already inactive (satisfied) at the starting point together with box bounds (the case that exposed an early bug where the outer loop stopped before doing any optimization), and the existence_error/2 raised when a problem defines a constraint but not its Jacobian.'
	]).

	cover(augmented_lagrangian(_, _)).
	cover(sub_problem(_, _, _, _, _, _, _)).

	:- uses(linear_algebra, [
		euclidean_norm/2, subtract_vectors/3
	]).

	% quadratic objective + linear equality constraint, bfgs inner
	% solver; optimum (0.5, 0.5), value 0.5

	test(augmented_lagrangian_quadratic_equality_bfgs, deterministic(Error < 1.0e-4)) :-
		augmented_lagrangian(sqp_test_quadratic_equality, bfgs)::run(X, V, _Statistics, []),
		subtract_vectors(X, [0.5, 0.5], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - 0.5),
		Error is max(ErrorX, ErrorV).

	% genuinely nonlinear equality constraint, lbfgs inner solver;
	% optimum (-1/sqrt(2), -1/sqrt(2)), value -sqrt(2). The starting
	% point [1,0] lies exactly on the constraint (zero violation from
	% the very first check), which is deliberately what exercises the
	% "must still optimize even though already feasible" fix.

	test(augmented_lagrangian_nonlinear_circle_lbfgs, deterministic(Error < 1.0e-4)) :-
		augmented_lagrangian(sqp_test_circle, lbfgs)::run(X, V, _Statistics, []),
		HalfSqrt2 is -1.0 / sqrt(2.0),
		subtract_vectors(X, [HalfSqrt2, HalfSqrt2], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - (-sqrt(2.0))),
		Error is max(ErrorX, ErrorV).

	% inequality constraint already inactive at the starting point,
	% combined with box bounds; optimum (1, 1), value 2. Regression
	% test for the bug where the outer loop mistook "already feasible"
	% for "already optimal" and returned the starting point unchanged.

	test(augmented_lagrangian_inactive_inequality_with_bounds, deterministic(Error < 1.0e-4)) :-
		augmented_lagrangian(al_test_inequality_bounds, bfgs)::run(X, V, _Statistics, []),
		subtract_vectors(X, [1.0, 1.0], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - 2.0),
		Error is max(ErrorX, ErrorV).

	% objective(maximize): mirrors sqp_active_set_maximize

	test(augmented_lagrangian_maximize, deterministic(Error < 1.0e-4)) :-
		augmented_lagrangian(sqp_test_maximize, bfgs)::run(X, V, _Statistics, [objective(maximize)]),
		subtract_vectors(X, [0.5, 0.5], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - (-0.5)),
		Error is max(ErrorX, ErrorV).

	test(augmented_lagrangian_target_requires_feasibility, deterministic(Violation =< 1.0e-6)) :-
		augmented_lagrangian(sqp_test_quadratic_equality, bfgs)::run(X, _V, Statistics, [target_value(1.0), outer_tolerance(1.0e-6)]),
		list::memberchk(termination_reason(target_reached), Statistics),
		sqp_test_quadratic_equality::equality_constraints(X, [Residual]),
		Violation is abs(Residual).

	test(augmented_lagrangian_initial_point_option, deterministic(Error < 1.0e-12)) :-
		augmented_lagrangian(initial_point_override_test, bfgs)::run(X, V, _Statistics, [initial_point([0.0, 0.0])]),
		euclidean_norm(X, ErrorX),
		Error is max(ErrorX, abs(V)).

	test(augmented_lagrangian_initial_point_fallback, deterministic(X == [1.0, -1.0])) :-
		augmented_lagrangian(initial_point_fallback_test, bfgs)::run(X, _V, _Statistics, [max_outer_iterations(1)]).

	test(augmented_lagrangian_reports_max_iterations, deterministic) :-
		augmented_lagrangian(sqp_test_circle, lbfgs)::run(_X, _V, Statistics, [max_outer_iterations(1), outer_tolerance(0.0)]),
		list::memberchk(termination_reason(max_iterations), Statistics).

	test(augmented_lagrangian_progress_disabled, deterministic) :-
		constrained_progress_test::clear_log,
		augmented_lagrangian(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, []),
		\+ constrained_progress_test::outer_progress_log(_, _, _, _, _),
		\+ constrained_progress_test::inner_progress_log(_, _, _, _, _, _).

	test(augmented_lagrangian_outer_progress, deterministic) :-
		constrained_progress_test::clear_log,
		augmented_lagrangian(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, [updates(1)]),
		constrained_progress_test::outer_progress_log(1, _, _, _, _),
		\+ constrained_progress_test::inner_progress_log(_, _, _, _, _, _).

	test(augmented_lagrangian_inner_progress, deterministic) :-
		constrained_progress_test::clear_log,
		augmented_lagrangian(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, [inner_updates(1)]),
		once(constrained_progress_test::inner_progress_log(outer(1), _, _, _, _, _)),
		\+ constrained_progress_test::outer_progress_log(_, _, _, _, _).

	test(augmented_lagrangian_invalid_inner_updates, error(domain_error(option, inner_updates(-1)))) :-
		augmented_lagrangian(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, [inner_updates(-1)]).

	% a problem defining equality_constraints/2 but not
	% equality_jacobian/2 must raise existence_error/2

	test(augmented_lagrangian_missing_jacobian_raises, error(existence_error(procedure, equality_jacobian/2))) :-
		augmented_lagrangian(sqp_test_missing_jacobian, bfgs)::run(_X, _V, _Statistics, []).

	test(augmented_lagrangian_invalid_constraint_values, error(domain_error(equality_constraint_data, _))) :-
		augmented_lagrangian(invalid_constraint_values_test, bfgs)::run(_X, _V, _Statistics, []).

	test(augmented_lagrangian_invalid_constraint_jacobian, error(domain_error(equality_constraint_data, _))) :-
		augmented_lagrangian(invalid_constraint_jacobian_test, bfgs)::run(_X, _V, _Statistics, []).

:- end_object.
