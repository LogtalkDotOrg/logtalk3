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


:- object(qp_active_set_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Unit tests for qp_active_set: pure equality-constrained, pure inequality-constrained, and combined QPs against hand-derived optima and Lagrange multipliers (via KKT conditions worked out independently of the implementation); the multiplier-drop and constraint-addition branches of the active-set loop; the unconstrained case; and dimension/infeasibility error handling.'
	]).

	cover(qp_active_set).

	:- uses(linear_algebra, [
		euclidean_norm/2, subtract_vectors/3
	]).

	% --- pure inequality constraint, active at the optimum ---
	%
	% minimize 0.5*(x1^2 + x2^2) subject to x1 + x2 >= 2 (as -x1-x2 =< -2)
	% unconstrained optimum (0,0) violates the constraint; by symmetry
	% the constrained optimum is the closest point on the line
	% x1 + x2 = 2 to the origin: (1,1). KKT stationarity at (1,1),
	% grad f = (1,1), row = (-1,-1), gives multiplier lambda = 1.
	% Compared with a tolerance since solve_linear_system/3 and
	% pseudo_inverse/2 are not exact arithmetic, so exact list equality
	% would be too strict.

	test(qp_active_set_inequality_active, deterministic(Error < 1.0e-6)) :-
		qp_active_set::solve(
			[[1.0,0.0],[0.0,1.0]], [0.0,0.0],
			[], [],
			[[-1.0,-1.0]], [-2.0],
			X, Lambda
		),
		subtract_vectors(X, [1.0,1.0], DiffX),
		euclidean_norm(DiffX, ErrorX),
		subtract_vectors(Lambda, [1.0], DiffL),
		euclidean_norm(DiffL, ErrorL),
		Error is max(ErrorX, ErrorL).

	% --- pure equality constraint ---
	%
	% minimize 0.5*(x1^2 + x2^2) - 2*x2 subject to x1 - x2 = 0
	% Lagrangian stationarity gives x1 = -lambda, x2 = 2+lambda, and
	% x1 = x2 forces lambda = -1, hence x1 = x2 = 1.

	test(qp_active_set_equality_only, deterministic(Error < 1.0e-6)) :-
		qp_active_set::solve(
			[[1.0,0.0],[0.0,1.0]], [0.0,-2.0],
			[[1.0,-1.0]], [0.0],
			[], [],
			X, _Lambda
		),
		subtract_vectors(X, [1.0,1.0], Diff),
		euclidean_norm(Diff, Error).

	% --- inequality inactive at the optimum, box constraint active ---
	%
	% minimize 0.5*(x1^2 + x2^2) - x1 - 3*x2 subject to
	% x1 + x2 =< 2, x1 >= 0, x2 >= 0. Unconstrained optimum (1,3)
	% violates x1+x2=<2; on that boundary the constrained optimum is
	% (0,2) (x1 >= 0 also active, x2 >= 0 inactive).

	test(qp_active_set_multiple_inequalities, deterministic(Error < 1.0e-6)) :-
		qp_active_set::solve(
			[[1.0,0.0],[0.0,1.0]], [-1.0,-3.0],
			[], [],
			[[1.0,1.0],[-1.0,0.0],[0.0,-1.0]], [2.0,0.0,0.0],
			X, _Lambda
		),
		subtract_vectors(X, [0.0,2.0], Diff),
		euclidean_norm(Diff, Error).

	% --- equality and inequality constraints combined ---
	%
	% minimize 0.5*(x1^2+x2^2+x3^2) - x3 subject to x1+x2+x3 = 3,
	% x1 >= 0, x2 >= 0, x3 =< 1. Solving the equality-only KKT system
	% gives x3 = 5/3, violating x3 =< 1; with x3 = 1 fixed, minimizing
	% 0.5*(x1^2+x2^2) subject to x1+x2 = 2 gives x1 = x2 = 1 by symmetry.

	test(qp_active_set_equality_and_inequality, deterministic(Error < 1.0e-6)) :-
		qp_active_set::solve(
			[[1.0,0.0,0.0],[0.0,1.0,0.0],[0.0,0.0,1.0]], [0.0,0.0,-1.0],
			[[1.0,1.0,1.0]], [3.0],
			[[-1.0,0.0,0.0],[0.0,-1.0,0.0],[0.0,0.0,1.0]], [0.0,0.0,1.0],
			X, _Lambda
		),
		subtract_vectors(X, [1.0,1.0,1.0], Diff),
		euclidean_norm(Diff, Error).

	% --- inactive box constraints: the solver must not over-constrain ---
	%
	% minimize 0.5*(x1^2+x2^2) subject to x1 =< 5, x2 =< 5, x1+x2 >= 1.
	% Only the last constraint is active at the optimum (0.5,0.5).

	test(qp_active_set_inactive_bounds_ignored, deterministic(Error < 1.0e-6)) :-
		qp_active_set::solve(
			[[1.0,0.0],[0.0,1.0]], [0.0,0.0],
			[], [],
			[[1.0,0.0],[0.0,1.0],[-1.0,-1.0]], [5.0,5.0,-1.0],
			X, _Lambda
		),
		subtract_vectors(X, [0.5,0.5], Diff),
		euclidean_norm(Diff, Error).

	% --- multiplier-drop branch ---
	%
	% minimize 0.5*(x1^2+x2^2) - 2*x1 - 3*x2 subject to x1 >= 1. The
	% unconstrained optimum (2,3) is feasible with the constraint
	% inactive, but the phase-1 heuristic starts at the origin, which
	% violates x1 >= 1, forcing it active first; the solver must detect
	% the resulting negative multiplier and drop the constraint again
	% to reach (2,3).

	test(qp_active_set_drops_spurious_active_constraint, deterministic(Error < 1.0e-6)) :-
		qp_active_set::solve(
			[[1.0,0.0],[0.0,1.0]], [-2.0,-3.0],
			[], [],
			[[-1.0,0.0]], [-1.0],
			X, _Lambda
		),
		subtract_vectors(X, [2.0,3.0], Diff),
		euclidean_norm(Diff, Error).

	% --- multiplier-drop branch with a genuine tie ---
	%
	% minimize 0.5*(x1^2+x2^2) - 10*x1 - 10*x2 subject to x1 >= 2,
	% x2 >= 2. The unconstrained optimum (10,10) is feasible with both
	% constraints inactive, but the phase-1 heuristic starts at the
	% origin, which violates both, forcing both active together at
	% (2,2); at that point both multipliers are exactly -8 (a genuine
	% tie, verified by hand and by tracing the solver during
	% development), exercising the tie-breaking behaviour of the
	% leaving-variable rule in check_drop/2 (Bland's rule: the first,
	% i.e. lowest-index, negative-multiplier constraint is dropped, not
	% necessarily the most negative one). The final answer does not depend
	% on which tied constraint is dropped first, but reaching it
	% correctly despite the tie is exactly what this test checks.

	test(qp_active_set_drops_tied_multipliers, deterministic(Error < 1.0e-6)) :-
		qp_active_set::solve(
			[[1.0,0.0],[0.0,1.0]], [-10.0,-10.0],
			[], [],
			[[-1.0,0.0],[0.0,-1.0]], [-2.0,-2.0],
			X, _Lambda
		),
		subtract_vectors(X, [10.0,10.0], Diff),
		euclidean_norm(Diff, Error).

	% --- fully unconstrained ---
	%
	% minimize 0.5*(x1^2+x2^2) - 4*x1 - 6*x2, no constraints at all;
	% the KKT system collapses to a single unconstrained Newton step.

	test(qp_active_set_unconstrained, deterministic(Error < 1.0e-6)) :-
		qp_active_set::solve(
			[[1.0,0.0],[0.0,1.0]], [-4.0,-6.0],
			[], [],
			[], [],
			X, _Lambda
		),
		subtract_vectors(X, [4.0,6.0], Diff),
		euclidean_norm(Diff, Error).

	% --- infeasible constraints ---
	%
	% x1 >= 1 and x1 =< 0 cannot both hold; solve/8 must fail rather
	% than return a spurious point.

	test(qp_active_set_infeasible_fails, deterministic) :-
		\+ qp_active_set::solve(
			[[1.0,0.0],[0.0,1.0]], [0.0,0.0],
			[], [],
			[[-1.0,0.0],[1.0,0.0]], [-1.0,0.0],
			_X, _Lambda
		).

	% --- malformed input ---
	%
	% C has 3 components but H is 2x2: solve/8 must raise a
	% domain_error rather than proceeding with mismatched matrices.

	test(qp_active_set_mismatched_dimensions, error(domain_error(qp_h_matrix, _))) :-
		qp_active_set::solve(
			[[1.0,0.0],[0.0,1.0]], [1.0,2.0,3.0],
			[], [],
			[], [],
			_X, _Lambda
		).

	test(qp_active_set_nonnumeric_coefficients, error(domain_error(qp_c_vector, _))) :-
		qp_active_set::solve(
			[[1.0,0.0],[0.0,1.0]], [invalid,0.0],
			[], [],
			[], [],
			_X, _Lambda
		).

:- end_object.
