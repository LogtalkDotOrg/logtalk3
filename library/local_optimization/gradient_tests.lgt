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


:- object(gradient_tests(_Solver_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-19,
		comment is 'Unit tests specific to gradient-based local_optimization solvers (gradient descent, conjugate gradient, BFGS, ...). Parameterized by the solver functor.',
		parnames is ['Solver']
	]).

	:- uses(list, [
		memberchk/2
	]).

	% basic convergence on smooth problems with analytic gradient

	test(grad_sphere_converges, deterministic((Value < 1.0e-4, list::valid(Point)))) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(Point, Value, [
			max_iterations(500),
			tol_g(1.0e-8),
			tol_x(1.0e-10)
		]).

	test(grad_rosenbrock_improves, deterministic(Value < 5.0)) :-
		% classic start (-1.2, 1.0) -> f≈24.2; should improve substantially
		Solver =.. [_Solver_, rosenbrock],
		Solver::run(_Point, Value, [
			max_iterations(1000),
			tol_g(1.0e-6)
		]).

	test(grad_near_optimum_sphere, deterministic) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(Point, Value, [
			max_iterations(500),
			tol_g(1.0e-10)
		]),
		Point = [X, Y],
		^^assertion(abs(X) < 1.0e-3),
		^^assertion(abs(Y) < 1.0e-3),
		^^assertion(Value < 1.0e-5).

	% maximization with gradient

	test(grad_maximize_negative_sphere, deterministic(Value > -1.0e-4)) :-
		Solver =.. [_Solver_, negative_sphere],
		Solver::run(_Point, Value, [
			objective(maximize),
			max_iterations(500),
			tol_g(1.0e-8)
		]).

	% statistics expected from gradient-based solvers

	test(grad_statistics, deterministic) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(_Point, _Value, Statistics, [max_iterations(50)]),
		memberchk(iterations(Iterations), Statistics),
		^^assertion(Iterations =< 50),
		memberchk(evaluations(Evaluations), Statistics),
		^^assertion(Evaluations > 0),
		% gradient-based solvers should also report gradient evaluations
		(	memberchk(gradient_evaluations(GradEvals), Statistics) ->
			^^assertion(GradEvals > 0)
		;	true		% tolerate solvers that fold them into evaluations/1
		),
		memberchk(final_value(FinalValue), Statistics),
		^^assertion(number(FinalValue)).

	test(grad_tol_g_stops, deterministic(Iterations < 500)) :-
		% with a loose gradient tolerance the solver should stop early
		Solver =.. [_Solver_, sphere],
		Solver::run(_Point, _Value, Statistics, [
			max_iterations(500),
			tol_g(1.0)		% very loose
		]),
		memberchk(iterations(Iterations), Statistics).

	% missing gradient must be reported clearly

	test(grad_missing_gradient_error, error(_)) :-
		% any error is acceptable; existence_error or domain_error preferred
		Solver =.. [_Solver_, sphere_no_gradient],
		Solver::run(_Point, _Value, [max_iterations(10)]).

	% box constraints with gradient projection

	test(grad_bounded_sphere, deterministic) :-
		Solver =.. [_Solver_, bounded_sphere],
		Solver::run(Point, Value, [max_iterations(200), tol_g(1.0e-8)]),
		Point = [X, Y],
		^^assertion((X >= -1.0, X =< 1.0)),
		^^assertion((Y >= -1.0, Y =< 1.0)),
		^^assertion(Value >= 0.0),
		^^assertion(Value < 0.1).

	% option validation (gradient-specific options when present)

	test(grad_invalid_step_size, error(domain_error(option, step_size(-0.1)))) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(_Point, _Value, [step_size(-0.1)]).

	test(grad_invalid_tol_g, error(domain_error(option, tol_g(-1.0)))) :-
		Solver =.. [_Solver_, sphere],
		Solver::run(_Point, _Value, [tol_g(-1.0)]).

:- end_object.
