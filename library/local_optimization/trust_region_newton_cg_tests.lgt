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


:- object(trust_region_newton_cg_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-19,
		comment is 'Unit tests specific to the trust_region_newton_cg(_) solver: option validation, negative-curvature (Steihaug boundary step) handling, and convergence-speed checks against gradient_descent(_) and bfgs(_). This solver has no Armijo-related options, so it does not share gradient_tests(_) and is not included in that suite; the shared tests(trust_region_newton_cg) suite covers the common solver API.'
	]).

	cover(trust_region_newton_cg(_)).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(linear_algebra, [
		euclidean_norm/2
	]).

	% exact second-order information means a single Newton step solves
	% an unconstrained quadratic exactly whenever it lies inside the
	% trust region; growing the radius from the default takes only a
	% couple of extra iterations, so sphere should converge in well
	% under ten.

	test(trust_region_newton_cg_exact_on_quadratic, deterministic(Iterations < 10)) :-
		trust_region_newton_cg(sphere)::run(Point, Value, Stats, [
			tol_g(1.0e-10), tol_x(1.0e-10), tol_f(1.0e-10)
		]),
		memberchk(iterations(Iterations), Stats),
		^^assertion(Value < 1.0e-8),
		euclidean_norm(Point, PointNorm),
		^^assertion(PointNorm < 1.0e-4).

	% same problem, maximizing instead: Newton's method should be just
	% as exact and just as fast when the internal sign flip is correct.

	test(trust_region_newton_cg_exact_on_quadratic_maximize, deterministic(Iterations < 10)) :-
		trust_region_newton_cg(negative_sphere)::run(_Point, Value, Stats, [
			objective(maximize), tol_g(1.0e-10), tol_x(1.0e-10), tol_f(1.0e-10)
		]),
		memberchk(iterations(Iterations), Stats),
		^^assertion(Value > -1.0e-8).

	% negative-curvature handling: double_well starts at a saddle point
	% (negative curvature along x, positive along y). A correct
	% Steihaug-CG boundary step escapes along the negative-curvature
	% direction instead of stalling or diverging, reaching one of the
	% two symmetric global minima (value -1).

	test(trust_region_newton_cg_escapes_saddle, deterministic(Iterations < 20)) :-
		trust_region_newton_cg(double_well)::run(_Point, Value, Stats, [
			tol_g(1.0e-8)
		]),
		memberchk(iterations(Iterations), Stats),
		^^assertion(abs(Value - (-1.0)) < 1.0e-4).

	% quasi-Newton (bfgs) vs full Newton (trust_region_newton_cg): with
	% exact second-order information, this solver should reach a tight
	% gradient tolerance on Rosenbrock in noticeably fewer iterations.

	test(trust_region_newton_cg_faster_than_bfgs, deterministic(NewtonIterations < BfgsIterations)) :-
		Options = [max_iterations(100), tol_g(1.0e-8)],
		trust_region_newton_cg(rosenbrock)::run(_NewtonPoint, _NewtonValue, NewtonStats, Options),
		bfgs(rosenbrock)::run(_BfgsPoint, _BfgsValue, BfgsStats, Options),
		memberchk(iterations(NewtonIterations), NewtonStats),
		memberchk(iterations(BfgsIterations), BfgsStats).

	test(trust_region_newton_cg_faster_than_gradient_descent, deterministic(NewtonIterations < GdIterations)) :-
		Options = [max_iterations(300), tol_g(1.0e-8)],
		trust_region_newton_cg(rosenbrock)::run(_NewtonPoint, _NewtonValue, NewtonStats, Options),
		gradient_descent(rosenbrock)::run(_GdPoint, _GdValue, GdStats, Options),
		memberchk(iterations(NewtonIterations), NewtonStats),
		memberchk(iterations(GdIterations), GdStats).

	% cg_max_iterations caps how much curvature information the inner
	% Steihaug-CG can use per outer step; capping it at 1 (steepest
	% descent inside the trust region, effectively) should need
	% noticeably more outer iterations than the dimension-sized default
	% to reach the same tolerance on Rosenbrock.

	test(trust_region_newton_cg_cg_max_iterations_matters, deterministic(DefaultIterations < CappedIterations)) :-
		Options = [max_iterations(300), tol_g(1.0e-8)],
		trust_region_newton_cg(rosenbrock)::run(_DefaultPoint, _DefaultValue, DefaultStats, Options),
		trust_region_newton_cg(rosenbrock)::run(_CappedPoint, _CappedValue, CappedStats, [
			cg_max_iterations(1)| Options
		]),
		memberchk(iterations(DefaultIterations), DefaultStats),
		memberchk(iterations(CappedIterations), CappedStats).

	% missing gradient/hessian error handling

	test(trust_region_newton_cg_missing_gradient, error(existence_error(procedure, gradient/2))) :-
		trust_region_newton_cg(sphere_no_gradient)::run(_Point, _Value).

	test(trust_region_newton_cg_missing_hessian, error(existence_error(procedure, hessian/2))) :-
		trust_region_newton_cg(sphere_no_hessian)::run(_Point, _Value).

	% option validation

	test(trust_region_newton_cg_invalid_trust_radius_initial, error(domain_error(option, trust_radius_initial(-1.0)))) :-
		trust_region_newton_cg(sphere)::run(_Point, _Value, [trust_radius_initial(-1.0)]).

	test(trust_region_newton_cg_invalid_trust_radius_max, error(domain_error(option, trust_radius_max(0.0)))) :-
		trust_region_newton_cg(sphere)::run(_Point, _Value, [trust_radius_max(0.0)]).

	test(trust_region_newton_cg_initial_radius_exceeds_max, error(domain_error(option, trust_radius_initial(10.0)))) :-
		trust_region_newton_cg(sphere)::run(_Point, _Value, [
			trust_radius_initial(10.0), trust_radius_max(1.0)
		]).

	test(trust_region_newton_cg_invalid_eta, error(domain_error(option, eta(0.25)))) :-
		trust_region_newton_cg(sphere)::run(_Point, _Value, [eta(0.25)]).

	test(trust_region_newton_cg_invalid_cg_tol, error(domain_error(option, cg_tol(1.0)))) :-
		trust_region_newton_cg(sphere)::run(_Point, _Value, [cg_tol(1.0)]).

	test(trust_region_newton_cg_invalid_cg_max_iterations, error(domain_error(option, cg_max_iterations(0)))) :-
		trust_region_newton_cg(sphere)::run(_Point, _Value, [cg_max_iterations(0)]).

:- end_object.
