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


:- object(bfgs_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'Unit tests specific to the bfgs(_) solver: option validation, restart behavior, and cross-solver consistency/performance checks against gradient_descent(_). The shared tests(bfgs) and gradient_tests(bfgs) suites cover the common solver API and are not repeated here.'
	]).

	cover(bfgs(_)).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(linear_algebra, [
		difference_norm/3
	]).

	% restart(1) collapses BFGS to plain steepest descent every
	% iteration (H is reset to the identity before each step), so it
	% should retrace gradient_descent's Armijo trajectory exactly,
	% for both minimization and maximization.

	test(bfgs_restart_1_matches_gradient_descent_minimize, deterministic) :-
		Options = [max_iterations(20), tol_x(0.0), tol_f(0.0), restart(1)],
		bfgs(rosenbrock)::run(BfgsPoint, BfgsValue, Options),
		gradient_descent(rosenbrock)::run(GdPoint, GdValue, [max_iterations(20), tol_x(0.0), tol_f(0.0)]),
		difference_norm(BfgsPoint, GdPoint, PointDiff),
		^^assertion(PointDiff < 1.0e-6),
		^^assertion(abs(BfgsValue - GdValue) < 1.0e-6).

	test(bfgs_restart_1_matches_gradient_descent_maximize, deterministic) :-
		Options = [
			objective(maximize), max_iterations(20), tol_x(0.0), tol_f(0.0), restart(1)
		],
		bfgs(negative_sphere)::run(BfgsPoint, BfgsValue, Options),
		gradient_descent(negative_sphere)::run(GdPoint, GdValue, [
			objective(maximize), max_iterations(20), tol_x(0.0), tol_f(0.0)
		]),
		difference_norm(BfgsPoint, GdPoint, PointDiff),
		^^assertion(PointDiff < 1.0e-6),
		^^assertion(abs(BfgsValue - GdValue) < 1.0e-6).

	test(bfgs_restart_none_matches_explicit_default, deterministic) :-
		% restart(none) is the default; passing it explicitly must not
		% change the trajectory.
		Options = [max_iterations(30), tol_g(1.0e-10)],
		bfgs(rosenbrock)::run(DefaultPoint, DefaultValue, Options),
		bfgs(rosenbrock)::run(ExplicitPoint, ExplicitValue, [restart(none)| Options]),
		difference_norm(DefaultPoint, ExplicitPoint, PointDiff),
		^^assertion(PointDiff < 1.0e-12),
		^^assertion(abs(DefaultValue - ExplicitValue) < 1.0e-12).

	% curvature (quasi-Newton) advantage over plain gradient descent

	test(bfgs_converges_faster_than_gradient_descent, deterministic(BfgsIterations < GdIterations)) :-
		Options = [max_iterations(300), tol_g(1.0e-8)],
		bfgs(rosenbrock)::run(_BfgsPoint, _BfgsValue, BfgsStats, Options),
		gradient_descent(rosenbrock)::run(_GdPoint, _GdValue, GdStats, Options),
		memberchk(iterations(BfgsIterations), BfgsStats),
		memberchk(iterations(GdIterations), GdStats).

	test(bfgs_better_value_within_shared_budget, deterministic(BfgsValue < GdValue)) :-
		% under an iteration budget too small for plain gradient
		% descent to make much headway on Rosenbrock's curved valley,
		% BFGS's curvature model should still make clear progress.
		Options = [max_iterations(50)],
		bfgs(rosenbrock)::run(_BfgsPoint, BfgsValue, Options),
		gradient_descent(rosenbrock)::run(_GdPoint, GdValue, Options).

	% periodic restart still converges (sanity check; restart(dimension)
	% mirrors conjugate_gradient's default reset interval)

	test(bfgs_restart_dimension_converges, deterministic(Value < 1.0e-6)) :-
		bfgs(sphere)::run(_Point, Value, [
			restart(dimension), max_iterations(50), tol_g(1.0e-10)
		]).

	% option validation

	test(bfgs_invalid_restart_negative, error(domain_error(option, restart(-1)))) :-
		bfgs(sphere)::run(_Point, _Value, [restart(-1)]).

	test(bfgs_invalid_restart_atom, error(domain_error(option, restart(always)))) :-
		bfgs(sphere)::run(_Point, _Value, [restart(always)]).

:- end_object.
