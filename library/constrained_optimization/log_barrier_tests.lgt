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


:- object(log_barrier_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Unit tests for log_barrier(_,_): a pure inequality-constrained problem, a combined equality-and-inequality problem, the phase-1 heuristic rescuing an infeasible-but-non-empty-feasible-set starting point, domain_error/2 still being raised for a genuinely empty feasible set, the objective(maximize) option, the existence_error/2 raised when a problem defines a constraint but not its Jacobian, and a direct check that the internal barrier_sub_problem safety fallback returns finite values (rather than crashing on log of a non-positive number) both past the feasible boundary and exactly on it.'
	]).

	cover(log_barrier(_, _)).
	cover(barrier_sub_problem(_, _, _, _, _, _)).
	cover(phase1_sub_problem(_, _)).

	:- uses(linear_algebra, [
		euclidean_norm/2, subtract_vectors/3
	]).

	% pure inequality constraint, already strictly feasible at the
	% starting point [0,0]; optimum (1, 1), value 2

	test(log_barrier_pure_inequality, deterministic(Error < 1.0e-4)) :-
		log_barrier(al_test_inequality_bounds, bfgs)::run(X, V, _Statistics, []),
		subtract_vectors(X, [1.0, 1.0], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - 2.0),
		Error is max(ErrorX, ErrorV).

	% equality constraint (handled by the quadratic-penalty fallback,
	% not the barrier) combined with inequality constraints (handled by
	% the barrier); optimum (1, 1, 1), value 0.5

	test(log_barrier_equality_and_inequality, deterministic(Error < 1.0e-4)) :-
		log_barrier(lb_test_eq_and_ineq, bfgs)::run(X, V, _Statistics, []),
		subtract_vectors(X, [1.0, 1.0, 1.0], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - 0.5),
		Error is max(ErrorX, ErrorV).

	% an initial_point/1 that violates an inequality constraint must
	% raise domain_error/2, not silently proceed

	% initial_point/1 that violates an inequality constraint, but whose
	% feasible set is non-empty: the phase-1 heuristic must rescue it,
	% converging to the same optimum (1, 1), value 2, that
	% log_barrier_pure_inequality reaches from an already-feasible start

	test(log_barrier_phase1_rescues_infeasible_start, deterministic(Error < 1.0e-4)) :-
		log_barrier(lb_test_infeasible_start, bfgs)::run(X, V, _Statistics, []),
		subtract_vectors(X, [1.0, 1.0], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - 2.0),
		Error is max(ErrorX, ErrorV).

	% an empty feasible set (x1 >= 1 and x1 =< 0 simultaneously): no
	% phase-1 heuristic can rescue this, so domain_error/2 must still be
	% raised, exactly as before phase 1 existed

	test(log_barrier_genuinely_infeasible_raises, error(domain_error(strictly_feasible_initial_point, _))) :-
		log_barrier(lb_test_genuinely_infeasible, bfgs)::run(_X, _V, _Statistics, []).

	% objective(maximize): mirrors augmented_lagrangian_maximize /
	% quadratic_penalty_maximize (sqp_test_maximize has no inequality
	% constraints, so the strict-feasibility check is trivially met)

	test(log_barrier_maximize, deterministic(Error < 1.0e-4)) :-
		log_barrier(sqp_test_maximize, bfgs)::run(X, V, _Statistics, [objective(maximize)]),
		subtract_vectors(X, [0.5, 0.5], DiffX),
		euclidean_norm(DiffX, ErrorX),
		ErrorV is abs(V - (-0.5)),
		Error is max(ErrorX, ErrorV).

	test(log_barrier_initial_point_option, deterministic(Error < 1.0e-12)) :-
		log_barrier(initial_point_override_test, bfgs)::run(X, V, _Statistics, [initial_point([0.0, 0.0])]),
		euclidean_norm(X, ErrorX),
		Error is max(ErrorX, abs(V)).

	test(log_barrier_initial_point_option_uses_phase1, deterministic(Error < 1.0e-4)) :-
		log_barrier(al_test_inequality_bounds, bfgs)::run(X, V, _Statistics, [initial_point([3.0, 3.0])]),
		subtract_vectors(X, [1.0, 1.0], DiffX),
		euclidean_norm(DiffX, ErrorX),
		Error is max(ErrorX, abs(V - 2.0)).

	test(log_barrier_initial_point_fallback, deterministic(X == [1.0, -1.0])) :-
		log_barrier(initial_point_fallback_test, bfgs)::run(X, _V, _Statistics, [max_outer_iterations(1)]).

	test(log_barrier_target_requires_equality_feasibility, deterministic(Violation =< 1.0e-6)) :-
		log_barrier(lb_test_eq_and_ineq, bfgs)::run(X, _V, Statistics, [target_value(10.0), outer_tolerance(1.0e-6)]),
		list::memberchk(termination_reason(converged), Statistics),
		lb_test_eq_and_ineq::equality_constraints(X, [Residual]),
		Violation is abs(Residual).

	test(log_barrier_reports_max_iterations, deterministic) :-
		log_barrier(lb_test_eq_and_ineq, bfgs)::run(_X, _V, Statistics, [max_outer_iterations(1), outer_tolerance(0.0)]),
		list::memberchk(termination_reason(max_iterations), Statistics).

	test(log_barrier_progress_disabled, deterministic) :-
		constrained_progress_test::clear_log,
		log_barrier(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, []),
		\+ constrained_progress_test::outer_progress_log(_, _, _, _, _),
		\+ constrained_progress_test::inner_progress_log(_, _, _, _, _, _).

	test(log_barrier_outer_progress, deterministic) :-
		constrained_progress_test::clear_log,
		log_barrier(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, [updates(1)]),
		constrained_progress_test::outer_progress_log(1, _, _, _, _),
		\+ constrained_progress_test::inner_progress_log(_, _, _, _, _, _).

	test(log_barrier_inner_progress, deterministic) :-
		constrained_progress_test::clear_log,
		log_barrier(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, [inner_updates(1)]),
		once(constrained_progress_test::inner_progress_log(outer(1), _, _, _, _, _)),
		\+ constrained_progress_test::outer_progress_log(_, _, _, _, _).

	test(log_barrier_phase1_progress, deterministic) :-
		constrained_progress_test::clear_log,
		log_barrier(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, [initial_point([3.0, 3.0]), inner_updates(1)]),
		once(constrained_progress_test::inner_progress_log(phase1, _, _, _, _, _)).

	test(log_barrier_invalid_inner_updates, error(domain_error(option, inner_updates(-1)))) :-
		log_barrier(constrained_progress_test, bfgs)::run(_X, _V, _Statistics, [inner_updates(-1)]).

	% a problem defining equality_constraints/2 but not
	% equality_jacobian/2 must raise existence_error/2

	test(log_barrier_missing_jacobian_raises, error(existence_error(procedure, equality_jacobian/2))) :-
		log_barrier(sqp_test_missing_jacobian, bfgs)::run(_X, _V, _Statistics, []).

	test(log_barrier_invalid_constraint_values, error(domain_error(equality_constraint_data, _))) :-
		log_barrier(invalid_constraint_values_test, bfgs)::run(_X, _V, _Statistics, []).

	% the safety fallback: evaluating the internal subproblem's
	% objective/gradient at a point far past the feasible boundary, and
	% exactly on it, must return finite numbers rather than raising an
	% arithmetic exception (log of a non-positive number). This is the
	% mechanism that makes it safe to delegate to a plain
	% local_optimization inner solver whose line search has no notion
	% of the barrier's domain.

	test(log_barrier_fallback_past_boundary_is_finite, deterministic) :-
		barrier_sub_problem(al_test_inequality_bounds, 1, [0.0,0.0], 1.0, 1.0, 1.0)::objective([5.0,5.0], V),
		barrier_sub_problem(al_test_inequality_bounds, 1, [0.0,0.0], 1.0, 1.0, 1.0)::gradient([5.0,5.0], G),
		number(V),
		V < 1.0e300,
		G = [G1, G2],
		number(G1),
		number(G2),
		G1 < 1.0e300,
		G2 < 1.0e300.

	test(log_barrier_fallback_on_boundary_is_finite, deterministic) :-
		barrier_sub_problem(al_test_inequality_bounds, 1, [0.0,0.0], 1.0, 1.0, 1.0)::objective([1.0,1.0], V),
		number(V),
		V < 1.0e300.

:- end_object.
