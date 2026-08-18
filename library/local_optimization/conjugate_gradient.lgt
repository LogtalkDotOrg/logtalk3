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


:- object(conjugate_gradient(_Problem_),
	imports(local_optimization_solver)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'Nonlinear conjugate-gradient local optimizer (Fletcher-Reeves and Polak-Ribière). Requires the problem to define ``gradient/2``. Supports optional box constraints via projection, minimization and maximization, and periodic or automatic restarts.',
		parameters is [
			'Problem' - 'Problem object implementing ``local_optimization_problem_protocol`` and defining ``gradient/2``.'
		],
		remarks is [
			'Beta formulas' - 'The ``beta(fletcher_reeves)`` and ``beta(polak_ribiere)`` options select the conjugacy coefficient. Polak-Ribière uses the standard non-negative truncation ``max(Beta, 0)``.',
			'Restarts' - 'The search direction is reset to steepest descent every ``restart(N)`` iterations (default: dimension) and whenever the new direction is insufficiently downhill.',
			'Line search' - 'Backtracking Armijo line search (same parameters as gradient descent).',
			'Bounds' - 'When the problem defines ``position_bounds/1``, trial points are projected onto the box after each step.'
		],
		see_also is [
			local_optimization_problem_protocol, local_optimization_solver, bfgs(_), gradient_descent(_), nelder_mead(_)
		]
	]).

	:- uses(_Problem_, [
		initial_point/1, objective/2, gradient/2, position_bounds/1, stop_condition/3, progress/5
	]).

	:- uses(linear_algebra, [
		dot_product/3, euclidean_norm/2, add_vectors/3, scale_vector/3, subtract_vectors/3
	]).

	:- uses(list, [
		length/2, member/2
	]).

	% public entry point

	run(BestPoint, BestValue, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(objective(ObjDir), Options),
		^^option(target_value(Target), Options),
		^^option(max_iterations(MaxIterations), Options),
		^^option(tol_x(TolX), Options),
		^^option(tol_f(TolF), Options),
		^^option(tol_g(TolG), Options),
		^^option(updates(Updates), Options),
		^^option(beta(BetaFormula), Options),
		^^option(restart(RestartOpt), Options),
		^^option(step_size(StepSize), Options),
		^^option(armijo_c(ArmijoC), Options),
		^^option(armijo_tau(ArmijoTau), Options),
		^^option(armijo_max_backtracks(MaxBT), Options),
		(	Updates > 0 ->
			UpdateInterval is max(1, (MaxIterations - 1) // Updates)
		;	UpdateInterval = 0
		),
		initial_point(Point0),
		(	position_bounds(Bounds) ->
			^^validate_bounds(Bounds),
			^^validate_point(Point0, Bounds)
		;	Bounds = [],
			^^validate_point(Point0, [])
		),
		length(Point0, Dimension),
		(	RestartOpt == dimension ->
			Restart = Dimension
		;	Restart = RestartOpt
		),
		require_gradient,
		objective(Point0, Value0),
		(	number(Value0) -> true ; domain_error(objective, Value0) ),
		gradient(Point0, Grad0),
		validate_gradient(Point0, Grad0),
		euclidean_norm(Grad0, GradNorm0),
		% initial direction = steepest descent
		steepest_direction(ObjDir, Grad0, Dir0),
		loop(
			0, MaxIterations, UpdateInterval, Dimension, Bounds, ObjDir, Target,
			BetaFormula, Restart, StepSize, ArmijoC, ArmijoTau, MaxBT,
			TolX, TolF, TolG,
			Point0, Value0, Grad0, GradNorm0, Dir0,
			1, 1,		% Evals, GradEvals
			BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, FinalGradNorm
		),
		Statistics = [
			iterations(Iterations),
			evaluations(Evaluations),
			gradient_evaluations(GradEvaluations),
			final_gradient_norm(FinalGradNorm),
			final_value(BestValue)
		].

	% main loop

	loop(
		Iter, MaxIterations, UpdInt, _Dim, _Bounds, _ObjDir, _Target,
		_Beta, _Restart, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, _TolG,
		Point, Value, _Grad, GradNorm, _Dir,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		Iter >= MaxIterations,
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, ObjDir, Target,
		_Beta, _Restart, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, _TolG,
		Point, Value, _Grad, GradNorm, _Dir,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		^^target_reached(ObjDir, Value, Target),
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, _ObjDir, _Target,
		_Beta, _Restart, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, _TolG,
		Point, Value, _Grad, GradNorm, _Dir,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		stop_condition(Iter, Point, Value),
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, _ObjDir, _Target,
		_Beta, _Restart, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, TolG,
		Point, Value, _Grad, GradNorm, _Dir,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		GradNorm =< TolG,
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, MaxIterations, UpdInt, Dim, Bounds, ObjDir, Target,
		BetaFormula, Restart, StepSize, ArmijoC, ArmijoTau, MaxBT,
		TolX, TolF, TolG,
		Point0, Value0, Grad0, _GradNorm0, Dir0,
		Evals0, GradEvals0,
		BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, FinalGradNorm
	) :-
		% line search along current conjugate direction
		armijo_line_search(
			Point0, Value0, Grad0, Dir0, Bounds, ObjDir,
			StepSize, ArmijoC, ArmijoTau, MaxBT,
			Point1, Value1, Evals0, Evals1
		),
		gradient(Point1, Grad1),
		validate_gradient(Point1, Grad1),
		GradEvals1 is GradEvals0 + 1,
		euclidean_norm(Grad1, GradNorm1),
		subtract_vectors(Point1, Point0, DeltaX),
		euclidean_norm(DeltaX, StepNorm),
		AbsDf is abs(Value1 - Value0),
		Iter1 is Iter + 1,
		^^report_progress(Iter1, UpdInt, Point1, Value1, GradNorm1, Evals1),
		(	(StepNorm =< TolX ; AbsDf =< TolF) ->
			^^report_final(Iter1, UpdInt, Point1, Value1, GradNorm1),
			BestPoint = Point1,
			BestValue = Value1,
			Iterations = Iter1,
			Evaluations = Evals1,
			GradEvaluations = GradEvals1,
			FinalGradNorm = GradNorm1
		;	% compute new conjugate direction
			new_direction(
				Iter1, Dim, Restart, BetaFormula, ObjDir,
				Grad0, Grad1, GradNorm1, Dir0, Dir1
			),
			loop(
				Iter1, MaxIterations, UpdInt, Dim, Bounds, ObjDir, Target,
				BetaFormula, Restart, StepSize, ArmijoC, ArmijoTau, MaxBT,
				TolX, TolF, TolG,
				Point1, Value1, Grad1, GradNorm1, Dir1,
				Evals1, GradEvals1,
				BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, FinalGradNorm
			)
		).

	% direction update

	steepest_direction(minimize, Grad, Dir) :-
		scale_vector(Grad, -1.0, Dir).
	steepest_direction(maximize, Grad, Dir) :-
		Dir = Grad.

	new_direction(Iter, _Dim, Restart, _BetaFormula, ObjDir, _Grad0, Grad1, _GradNorm1, _Dir0, Dir1) :-
		% periodic restart -> steepest descent
		Restart > 0,
		Iter mod Restart =:= 0,
		!,
		steepest_direction(ObjDir, Grad1, Dir1).
	new_direction(_Iter, _Dim, _Restart, BetaFormula, ObjDir, Grad0, Grad1, GradNorm1, Dir0, Dir1) :-
		beta(BetaFormula, Grad0, Grad1, GradNorm1, Beta0),
		(	BetaFormula == polak_ribiere ->
			Beta is max(0.0, Beta0)		% standard PR+ truncation
		;	Beta = Beta0
		),
		% d_new = -g_new + beta d_old   (minimize)
		% d_new = +g_new + beta d_old   (maximize)
		steepest_direction(ObjDir, Grad1, Steep),
		scale_vector(Dir0, Beta, ScaledDir),
		add_vectors(Steep, ScaledDir, DirCandidate),
		% ensure the direction is downhill / uphill enough; otherwise restart
		dot_product(Grad1, DirCandidate, Slope),
		(	ObjDir == minimize ->
			(	Slope < 0.0 ->
				Dir1 = DirCandidate
			;	Dir1 = Steep
			)
		;	(	Slope > 0.0 ->
				Dir1 = DirCandidate
			;	Dir1 = Steep
			)
		).

	% Fletcher–Reeves: beta = ||g1||^2 / ||g0||^2
	beta(fletcher_reeves, Grad0, _Grad1, GradNorm1, Beta) :-
		euclidean_norm(Grad0, GradNorm0),
		(	GradNorm0 =:= 0 ->
			Beta = 0.0
		;	Beta is (GradNorm1 * GradNorm1) / (GradNorm0 * GradNorm0)
		).

	% Polak–Ribière: beta = g1·(g1 - g0) / ||g0||^2
	beta(polak_ribiere, Grad0, Grad1, _GradNorm1, Beta) :-
		subtract_vectors(Grad1, Grad0, Diff),
		dot_product(Grad1, Diff, Num),
		euclidean_norm(Grad0, GradNorm0),
		(	GradNorm0 =:= 0 ->
			Beta = 0.0
		;	Beta is Num / (GradNorm0 * GradNorm0)
		).

	% Armijo line search (shared logic with gradient_descent)

	armijo_line_search(
		Point0, Value0, Grad0, Direction, Bounds, ObjDir,
		Step0, C, Tau, MaxBT,
		Point1, Value1, Evals0, Evals1
	) :-
		dot_product(Grad0, Direction, DirDeriv),
		armijo_backtrack(
			0, MaxBT, Point0, Value0, Direction, Bounds, ObjDir,
			Step0, C, Tau, DirDeriv,
			Point1, Value1, Evals0, Evals1
		).

	armijo_backtrack(
		BT, MaxBT, Point0, Value0, Direction, Bounds, ObjDir,
		Step, C, Tau, DirDeriv,
		Point1, Value1, Evals0, Evals1
	) :-
		scale_vector(Direction, Step, Scaled),
		add_vectors(Point0, Scaled, Trial0),
		^^project_to_bounds(Trial0, Bounds, Trial),
		objective(Trial, TrialVal),
		(	number(TrialVal) -> true ; domain_error(objective, TrialVal) ),
		Evals1_try is Evals0 + 1,
		Sufficient is Value0 + C * Step * DirDeriv,
		(	(	ObjDir == minimize,
				TrialVal =< Sufficient
			;	ObjDir == maximize,
				TrialVal >= Sufficient
			) ->
			Point1 = Trial,
			Value1 = TrialVal,
			Evals1 = Evals1_try
		;	BT1 is BT + 1,
			(	BT1 >= MaxBT ->
				Point1 = Trial,
				Value1 = TrialVal,
				Evals1 = Evals1_try
			;	Step1 is Step * Tau,
				armijo_backtrack(
					BT1, MaxBT, Point0, Value0, Direction, Bounds, ObjDir,
					Step1, C, Tau, DirDeriv,
					Point1, Value1, Evals1_try, Evals1
				)
			)
		).

	% auxiliary predicates

	require_gradient :-
		(	_Problem_::predicate_property(gradient(_, _), defined_in(_)) ->
			true
		;	existence_error(procedure, gradient/2)
		).

	validate_gradient(Point, Gradient) :-
		length(Point, D),
		length(Gradient, D),
		!,
		(	forall(member(G, Gradient), number(G)) ->
			true
		;	domain_error(gradient, Gradient)
		).
	validate_gradient(_Point, Gradient) :-
		domain_error(gradient, Gradient).

	progress_hook(Iteration, BestPoint, BestValue, Measure, Evaluations) :-
		ignore(progress(Iteration, BestPoint, BestValue, Measure, Evaluations)).

	% conjugate-gradient specific options

	default_option(beta(polak_ribiere)).
	default_option(restart(dimension)).
	default_option(step_size(1.0)).
	default_option(armijo_c(1.0e-4)).
	default_option(armijo_tau(0.5)).
	default_option(armijo_max_backtracks(20)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(beta(Beta)) :-
		once((Beta == fletcher_reeves ; Beta == polak_ribiere)).
	valid_option(restart(Restart)) :-
		once((Restart == dimension ; integer(Restart), Restart >= 0)).
	valid_option(step_size(StepSize)) :-
		number(StepSize), StepSize > 0.0.
	valid_option(armijo_c(C)) :-
		number(C), C > 0.0, C < 1.0.
	valid_option(armijo_tau(Tau)) :-
		number(Tau), Tau > 0.0, Tau < 1.0.
	valid_option(armijo_max_backtracks(N)) :-
		integer(N), N >= 1.
	valid_option(Option) :-
		^^valid_option(Option).

:- end_object.
