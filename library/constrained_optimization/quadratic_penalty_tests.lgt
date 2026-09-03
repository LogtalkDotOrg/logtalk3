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


:- object(quadratic_penalty_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Unit tests for quadratic_penalty(_,_): a linear-equality-constrained quadratic with bfgs as the inner solver, a genuinely nonlinear equality constraint with lbfgs as the inner solver, an inequality constraint already inactive at the starting point together with box bounds, the objective(maximize) option, and the existence_error/2 raised when a problem defines a constraint but not its Jacobian. Uses looser tolerances than augmented_lagrangian_tests.lgt: without multiplier correction, quadratic_penalty(_,_) only reaches feasibility in the limit rho -> infinity, so its default outer_tolerance leaves a larger residual violation (and correspondingly larger error in the returned point) for the same problems.'
	]).

	cover(quadratic_penalty(_, _)).
	cover(penalty_sub_problem(_, _, _, _, _)).

	:- uses(linear_algebra, [
		euclidean_norm/2, subtract_vectors/3
	]).

	% quadratic objective + linear equality constraint, bfgs inner
	% solver; optimum (0.5, 0.5), value 0.5

	test(quadratic_penalty_quadratic_equality_bfgs, deterministic(Error < 1.0e-3)) :-
		quadratic_penalty(sqp_test_quadratic_equality, bfgs)::run(X, V, _Statistics, []),
		subtract_vectors(X, [0.5, 0.5], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - 0.5),
		Error is max(ErrorX, ErrorV).

	% genuinely nonlinear equality constraint, lbfgs inner solver;
	% optimum (-1/sqrt(2), -1/sqrt(2)), value -sqrt(2)

	test(quadratic_penalty_nonlinear_circle_lbfgs, deterministic(Error < 1.0e-3)) :-
		quadratic_penalty(sqp_test_circle, lbfgs)::run(X, V, _Statistics, []),
		HalfSqrt2 is -1.0 / sqrt(2.0),
		subtract_vectors(X, [HalfSqrt2, HalfSqrt2], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - (-sqrt(2.0))),
		Error is max(ErrorX, ErrorV).

	% inequality constraint already inactive at the starting point,
	% combined with box bounds; optimum (1, 1), value 2. Same starting
	% configuration that exposed the "already feasible mistaken for
	% already optimal" bug during augmented_lagrangian(_,_) development
	% (see its NOTES.md entry); quadratic_penalty(_,_) was written with
	% that fix already in place, so this is a regression test rather
	% than a bug reproduction.

	test(quadratic_penalty_inactive_inequality_with_bounds, deterministic(Error < 1.0e-3)) :-
		quadratic_penalty(al_test_inequality_bounds, bfgs)::run(X, V, _Statistics, []),
		subtract_vectors(X, [1.0, 1.0], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - 2.0),
		Error is max(ErrorX, ErrorV).

	% objective(maximize): mirrors augmented_lagrangian_maximize

	test(quadratic_penalty_maximize, deterministic(Error < 1.0e-3)) :-
		quadratic_penalty(sqp_test_maximize, bfgs)::run(X, V, _Statistics, [objective(maximize)]),
		subtract_vectors(X, [0.5, 0.5], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - (-0.5)),
		Error is max(ErrorX, ErrorV).

	test(quadratic_penalty_initial_point_option, deterministic(Error < 1.0e-12)) :-
		quadratic_penalty(initial_point_override_test, bfgs)::run(X, V, _Statistics, [initial_point([0.0, 0.0])]),
		euclidean_norm(X, ErrorX),
		Error is max(ErrorX, abs(V)).

	test(quadratic_penalty_initial_point_fallback, deterministic(X == [1.0, -1.0])) :-
		quadratic_penalty(initial_point_fallback_test, bfgs)::run(X, _V, _Statistics, [max_outer_iterations(1)]).

	test(quadratic_penalty_target_requires_feasibility, deterministic(Violation =< 1.0e-6)) :-
		quadratic_penalty(sqp_test_quadratic_equality, bfgs)::run(X, _V, Statistics, [target_value(1.0), outer_tolerance(1.0e-6)]),
		list::memberchk(termination_reason(target_reached), Statistics),
		sqp_test_quadratic_equality::equality_constraints(X, [Residual]),
		Violation is abs(Residual).

	test(quadratic_penalty_reports_max_iterations, deterministic) :-
		quadratic_penalty(sqp_test_circle, lbfgs)::run(_X, _V, Statistics, [max_outer_iterations(1), outer_tolerance(0.0)]),
		list::memberchk(termination_reason(max_iterations), Statistics).

	test(quadratic_penalty_progress_disabled, deterministic) :-
		constrained_progress_test::clear_log,
		quadratic_penalty(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, []),
		\+ constrained_progress_test::outer_progress_log(_, _, _, _, _),
		\+ constrained_progress_test::inner_progress_log(_, _, _, _, _, _).

	test(quadratic_penalty_outer_progress, deterministic) :-
		constrained_progress_test::clear_log,
		quadratic_penalty(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, [updates(1)]),
		constrained_progress_test::outer_progress_log(1, _, _, _, _),
		\+ constrained_progress_test::inner_progress_log(_, _, _, _, _, _).

	test(quadratic_penalty_inner_progress, deterministic) :-
		constrained_progress_test::clear_log,
		quadratic_penalty(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, [inner_updates(1)]),
		once(constrained_progress_test::inner_progress_log(outer(1), _, _, _, _, _)),
		\+ constrained_progress_test::outer_progress_log(_, _, _, _, _).

	test(quadratic_penalty_invalid_inner_updates, error(domain_error(option, inner_updates(-1)))) :-
		quadratic_penalty(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, [inner_updates(-1)]).

	% a problem defining equality_constraints/2 but not
	% equality_jacobian/2 must raise existence_error/2

	test(quadratic_penalty_missing_jacobian_raises, error(existence_error(procedure, equality_jacobian/2))) :-
		quadratic_penalty(sqp_test_missing_jacobian, bfgs)::run(_X, _V, _Statistics, []).

	test(quadratic_penalty_invalid_constraint_values, error(domain_error(equality_constraint_data, _))) :-
		quadratic_penalty(invalid_constraint_values_test, bfgs)::run(_X, _V, _Statistics, []).

:- end_object.
