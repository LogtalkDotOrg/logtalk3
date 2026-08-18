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


:- object(lbfgs_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'Unit tests specific to the lbfgs(_) solver: option validation, restart/memory behavior, and cross-solver consistency/performance checks against gradient_descent(_) and bfgs(_). The shared tests(lbfgs) and gradient_tests(lbfgs) suites cover the common solver API and are not repeated here.'
	]).

	cover(lbfgs(_)).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(linear_algebra, [
		difference_norm/3
	]).

	% restart(1) clears the pair history before every step, so the
	% two-loop recursion falls back to steepest descent (H_0 = I) on
	% every iteration, exactly like bfgs(_) with restart(1). Both
	% should retrace gradient_descent's Armijo trajectory.

	test(lbfgs_restart_1_matches_gradient_descent_minimize, deterministic) :-
		Options = [max_iterations(20), tol_x(0.0), tol_f(0.0), restart(1)],
		lbfgs(rosenbrock)::run(LbfgsPoint, LbfgsValue, Options),
		gradient_descent(rosenbrock)::run(GdPoint, GdValue, [max_iterations(20), tol_x(0.0), tol_f(0.0)]),
		difference_norm(LbfgsPoint, GdPoint, PointDiff),
		^^assertion(PointDiff < 1.0e-6),
		^^assertion(abs(LbfgsValue - GdValue) < 1.0e-6).

	test(lbfgs_restart_1_matches_gradient_descent_maximize, deterministic) :-
		Options = [
			objective(maximize), max_iterations(20), tol_x(0.0), tol_f(0.0), restart(1)
		],
		lbfgs(negative_sphere)::run(LbfgsPoint, LbfgsValue, Options),
		gradient_descent(negative_sphere)::run(GdPoint, GdValue, [
			objective(maximize), max_iterations(20), tol_x(0.0), tol_f(0.0)
		]),
		difference_norm(LbfgsPoint, GdPoint, PointDiff),
		^^assertion(PointDiff < 1.0e-6),
		^^assertion(abs(LbfgsValue - GdValue) < 1.0e-6).

	% NOTE: L-BFGS with a large memory_size is *not* expected to
	% retrace bfgs(_)'s exact trajectory, even though both converge to
	% the same solution. The two-loop recursion rescales the initial
	% Hessian by gamma_k = (s.y)/(y.y) from the most recent pair on
	% every iteration (the standard Nocedal & Wright heuristic),
	% whereas bfgs(_) keeps H_0 = I fixed for the whole run and only
	% ever accumulates from it. That is an intentional algorithmic
	% difference, not a bug, so it is not asserted here.

	test(lbfgs_restart_none_matches_explicit_default, deterministic) :-
		% restart(none) is the default; passing it explicitly must not
		% change the trajectory.
		Options = [max_iterations(30), tol_g(1.0e-10)],
		lbfgs(rosenbrock)::run(DefaultPoint, DefaultValue, Options),
		lbfgs(rosenbrock)::run(ExplicitPoint, ExplicitValue, [restart(none)| Options]),
		difference_norm(DefaultPoint, ExplicitPoint, PointDiff),
		^^assertion(PointDiff < 1.0e-12),
		^^assertion(abs(DefaultValue - ExplicitValue) < 1.0e-12).

	% curvature (quasi-Newton) advantage over plain gradient descent

	test(lbfgs_converges_faster_than_gradient_descent, deterministic(LbfgsIterations < GdIterations)) :-
		Options = [max_iterations(300), tol_g(1.0e-8)],
		lbfgs(rosenbrock)::run(_LbfgsPoint, _LbfgsValue, LbfgsStats, Options),
		gradient_descent(rosenbrock)::run(_GdPoint, _GdValue, GdStats, Options),
		memberchk(iterations(LbfgsIterations), LbfgsStats),
		memberchk(iterations(GdIterations), GdStats).

	test(lbfgs_better_value_within_shared_budget, deterministic(LbfgsValue < GdValue)) :-
		Options = [max_iterations(50)],
		lbfgs(rosenbrock)::run(_LbfgsPoint, LbfgsValue, Options),
		gradient_descent(rosenbrock)::run(_GdPoint, GdValue, Options).

	% regression test: with a plain Armijo (sufficient-decrease-only)
	% line search, nothing guarantees y . s > 0, and on a few of the
	% early iterations on Rosenbrock's curved valley it does go
	% negative. Before the pair history was cleared on such a curvature
	% failure (rather than left stale), the solver got stuck repeating
	% the same near-zero-progress direction and needed the full
	% max_iterations budget without truly converging. Requiring
	% completion in well under the budget catches a regression to that
	% stale-history behavior.

	test(lbfgs_recovers_from_curvature_failure, deterministic(Iterations < 100)) :-
		lbfgs(rosenbrock)::run(_Point, _Value, Stats, [
			max_iterations(300), tol_g(1.0e-8)
		]),
		memberchk(iterations(Iterations), Stats).

	% a tiny memory should still converge (just possibly more slowly
	% than the default), confirming history capping does not break
	% the two-loop recursion

	test(lbfgs_memory_size_1_converges, deterministic(Value < 1.0e-4)) :-
		lbfgs(sphere)::run(_Point, Value, [
			memory_size(1), max_iterations(200), tol_g(1.0e-10)
		]).

	% periodic restart still converges (sanity check; restart(dimension)
	% mirrors conjugate_gradient's and bfgs(_)'s default reset interval)

	test(lbfgs_restart_dimension_converges, deterministic(Value < 1.0e-6)) :-
		lbfgs(sphere)::run(_Point, Value, [
			restart(dimension), max_iterations(50), tol_g(1.0e-10)
		]).

	% option validation

	test(lbfgs_invalid_memory_size_zero, error(domain_error(option, memory_size(0)))) :-
		lbfgs(sphere)::run(_Point, _Value, [memory_size(0)]).

	test(lbfgs_invalid_memory_size_float, error(domain_error(option, memory_size(2.5)))) :-
		lbfgs(sphere)::run(_Point, _Value, [memory_size(2.5)]).

	test(lbfgs_invalid_restart_negative, error(domain_error(option, restart(-1)))) :-
		lbfgs(sphere)::run(_Point, _Value, [restart(-1)]).

	test(lbfgs_invalid_restart_atom, error(domain_error(option, restart(always)))) :-
		lbfgs(sphere)::run(_Point, _Value, [restart(always)]).

:- end_object.
