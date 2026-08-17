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


:- object(bfgs(_Problem_),
	imports(local_optimization_solver)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'BFGS (Broyden-Fletcher-Goldfarb-Shanno) dense quasi-Newton local optimizer with backtracking Armijo line search. Requires the problem to define ``gradient/2``. Supports optional box constraints via projection, minimization and maximization.',
		parameters is [
			'Problem' - 'Problem object implementing ``local_optimization_problem_protocol`` and defining ``gradient/2``.'
		],
		remarks is [
			'Update' - 'Maintains a dense approximation to the inverse Hessian, updated after every accepted step with the standard BFGS rank-two formula. The approximation starts at the identity matrix.',
			'Internal minimization form' - 'Maximization is handled by internally minimizing the negated objective and gradient; the quasi-Newton direction, curvature test, and Armijo condition are therefore always expressed in minimization form, which avoids sign errors in the line search.',
			'Curvature safeguard' - 'The inverse-Hessian update is skipped (the previous approximation is kept) whenever the curvature condition ``y . s > 0`` is not comfortably satisfied, which keeps the approximation positive definite.',
			'Restarts' - 'The ``restart(N)`` option (off by default) periodically resets the inverse-Hessian approximation to the identity matrix, exactly as ``conjugate_gradient(_)`` resets its search direction.',
			'Bounds' - 'When the problem defines ``position_bounds/1``, trial points are projected onto the box after each step. Projection can weaken the quasi-Newton model; a pure bound-constrained formulation (L-BFGS-B style) is not implemented.'
		],
		see_also is [
			local_optimization_problem_protocol, local_optimization_solver, gradient_descent(_), conjugate_gradient(_), nelder_mead(_)
		]
	]).

	:- uses(_Problem_, [
		initial_point/1, objective/2, gradient/2, position_bounds/1, stop_condition/3, progress/5
	]).

	:- uses(linear_algebra, [
		add_vectors/3, add_matrices/3, dot_product/3, euclidean_norm/2,
		identity_matrix/2, matrix_matrix_product/3, matrix_vector_product/3,
		outer_product/3, scale_matrix/3, scale_vector/3,
		subtract_matrices/3, subtract_vectors/3
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
		^^option(step_size(StepSize), Options),
		^^option(armijo_c(ArmijoC), Options),
		^^option(armijo_tau(ArmijoTau), Options),
		^^option(armijo_max_backtracks(MaxBT), Options),
		^^option(restart(RestartOpt), Options),
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
		Dimension >= 1,
		(	RestartOpt == none ->
			Restart = 0
		;	RestartOpt == dimension ->
			Restart = Dimension
		;	Restart = RestartOpt
		),
		require_gradient,
		objective(Point0, Value0),
		(	number(Value0) -> true ; domain_error(objective, Value0) ),
		gradient(Point0, Grad0),
		validate_gradient(Point0, Grad0),
		direction_sign(ObjDir, Sign),
		scale_vector(Grad0, Sign, PhiGrad0),
		euclidean_norm(PhiGrad0, GradNorm0),
		identity_matrix(Dimension, H0),
		loop(
			0, MaxIterations, UpdateInterval, Dimension, Bounds, Sign, Target,
			Restart, StepSize, ArmijoC, ArmijoTau, MaxBT,
			TolX, TolF, TolG,
			Point0, Value0, PhiGrad0, GradNorm0, H0,
			1, 1,		% Evaluations, GradientEvaluations
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
	%
	% the objective direction is folded into a single "Sign" factor
	% (1.0 for minimize, -1.0 for maximize): PhiGrad = Sign * Grad is
	% always the gradient of the function actually being *minimized*,
	% so the quasi-Newton direction, curvature test, and line search
	% below never need to branch on ObjDir.

	loop(
		Iter, MaxIterations, UpdInt, _Dim, _Bounds, _Sign, _Target,
		_Restart, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, _TolG,
		Point, Value, _PhiGrad, GradNorm, _H,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		Iter >= MaxIterations,
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, Sign, Target,
		_Restart, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, _TolG,
		Point, Value, _PhiGrad, GradNorm, _H,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		objective_direction(Sign, ObjDir),
		^^target_reached(ObjDir, Value, Target),
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, _Sign, _Target,
		_Restart, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, _TolG,
		Point, Value, _PhiGrad, GradNorm, _H,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		stop_condition(Iter, Point, Value),
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, _Sign, _Target,
		_Restart, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, TolG,
		Point, Value, _PhiGrad, GradNorm, _H,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		GradNorm =< TolG,
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, MaxIterations, UpdInt, Dim, Bounds, Sign, Target,
		Restart, StepSize, ArmijoC, ArmijoTau, MaxBT,
		TolX, TolF, TolG,
		Point0, Value0, PhiGrad0, _GradNorm0, H0,
		Evals0, GradEvals0,
		BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, FinalGradNorm
	) :-
		% quasi-Newton direction on the (always-minimize) phi surface
		matrix_vector_product(H0, PhiGrad0, Hg),
		scale_vector(Hg, -1.0, PhiDirection0),
		dot_product(PhiGrad0, PhiDirection0, DirDeriv0),
		(	DirDeriv0 < 0.0 ->
			% genuine descent direction on phi
			PhiDirection = PhiDirection0,
			DirDeriv = DirDeriv0
		;	% approximation lost descent (rare, e.g. after a bad restart);
			% fall back to steepest descent on phi for this step
			scale_vector(PhiGrad0, -1.0, PhiDirection),
			dot_product(PhiGrad0, PhiDirection, DirDeriv)
		),
		phi_armijo_line_search(
			Point0, Value0, Sign, PhiDirection, DirDeriv, Bounds,
			StepSize, ArmijoC, ArmijoTau, MaxBT,
			Point1, Value1, Evals0, Evals1
		),
		gradient(Point1, Grad1),
		validate_gradient(Point1, Grad1),
		GradEvals1 is GradEvals0 + 1,
		scale_vector(Grad1, Sign, PhiGrad1),
		euclidean_norm(PhiGrad1, GradNorm1),
		subtract_vectors(Point1, Point0, S),
		euclidean_norm(S, StepNorm),
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
		;	subtract_vectors(PhiGrad1, PhiGrad0, Y),
			update_inverse_hessian(Iter1, Dim, Restart, S, Y, H0, H1),
			loop(
				Iter1, MaxIterations, UpdInt, Dim, Bounds, Sign, Target,
				Restart, StepSize, ArmijoC, ArmijoTau, MaxBT,
				TolX, TolF, TolG,
				Point1, Value1, PhiGrad1, GradNorm1, H1,
				Evals1, GradEvals1,
				BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, FinalGradNorm
			)
		).

	% objective-direction / phi-space helpers

	direction_sign(minimize, 1.0).
	direction_sign(maximize, -1.0).

	objective_direction(1.0, minimize).
	objective_direction(-1.0, maximize).

	% Armijo backtracking line search, expressed entirely in phi
	% (always-minimize) terms: Direction and DirDeriv are already
	% signed so that DirDeriv < 0.0, so a single sufficient-decrease
	% test covers both minimization and maximization of the original
	% objective.

	phi_armijo_line_search(
		Point0, Value0, Sign, Direction, DirDeriv, Bounds,
		Step0, C, Tau, MaxBT,
		Point1, Value1, Evals0, Evals1
	) :-
		phi_armijo_backtrack(
			0, MaxBT, Point0, Value0, Sign, Direction, Bounds,
			Step0, C, Tau, DirDeriv,
			Point1, Value1, Evals0, Evals1
		).

	phi_armijo_backtrack(
		BT, MaxBT, Point0, Value0, Sign, Direction, Bounds,
		Step, C, Tau, DirDeriv,
		Point1, Value1, Evals0, Evals1
	) :-
		scale_vector(Direction, Step, Scaled),
		add_vectors(Point0, Scaled, Trial0),
		^^project_to_bounds(Trial0, Bounds, Trial),
		objective(Trial, TrialVal),
		(	number(TrialVal) -> true ; domain_error(objective, TrialVal) ),
		Evals1_try is Evals0 + 1,
		PhiValue0 is Sign * Value0,
		PhiTrial is Sign * TrialVal,
		Sufficient is PhiValue0 + C * Step * DirDeriv,
		(	PhiTrial =< Sufficient ->
			Point1 = Trial,
			Value1 = TrialVal,
			Evals1 = Evals1_try
		;	BT1 is BT + 1,
			(	BT1 >= MaxBT ->
				% accept the last trial even if Armijo failed
				Point1 = Trial,
				Value1 = TrialVal,
				Evals1 = Evals1_try
			;	Step1 is Step * Tau,
				phi_armijo_backtrack(
					BT1, MaxBT, Point0, Value0, Sign, Direction, Bounds,
					Step1, C, Tau, DirDeriv,
					Point1, Value1, Evals1_try, Evals1
				)
			)
		).

	% inverse-Hessian approximation update

	update_inverse_hessian(Iter, Dim, Restart, _S, _Y, _H0, H1) :-
		Restart > 0,
		Iter mod Restart =:= 0,
		!,
		identity_matrix(Dim, H1).
	update_inverse_hessian(_Iter, _Dim, _Restart, S, Y, H0, H1) :-
		dot_product(Y, S, YdotS),
		(	YdotS > 1.0e-10 ->
			bfgs_update(S, Y, YdotS, H0, H1)
		;	% curvature condition not (comfortably) satisfied: keep H0
			% rather than risk losing positive definiteness
			H1 = H0
		).

	% standard BFGS rank-two update (Nocedal & Wright, eq. 6.17):
	%   rho   = 1 / (y . s)
	%   A     = I - rho * s y^T
	%   H_new = A H A^T + rho * s s^T

	bfgs_update(S, Y, YdotS, H0, H1) :-
		Rho is 1.0 / YdotS,
		length(H0, Dim),
		identity_matrix(Dim, I),
		outer_product(S, Y, SYt),
		scale_matrix(SYt, Rho, ScaledSYt),
		subtract_matrices(I, ScaledSYt, A),
		outer_product(Y, S, YSt),
		scale_matrix(YSt, Rho, ScaledYSt),
		subtract_matrices(I, ScaledYSt, At),
		matrix_matrix_product(A, H0, AH),
		matrix_matrix_product(AH, At, AHAt),
		outer_product(S, S, SSt),
		scale_matrix(SSt, Rho, ScaledSSt),
		add_matrices(AHAt, ScaledSSt, H1).

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

	% progress hook → problem progress/5
	progress_hook(Iteration, BestPoint, BestValue, Measure, Evaluations) :-
		ignore(progress(Iteration, BestPoint, BestValue, Measure, Evaluations)).

	% options specific to BFGS

	default_option(step_size(1.0)).
	default_option(armijo_c(1.0e-4)).
	default_option(armijo_tau(0.5)).
	default_option(armijo_max_backtracks(20)).
	default_option(restart(none)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(step_size(StepSize)) :-
		number(StepSize), StepSize > 0.0.
	valid_option(armijo_c(C)) :-
		number(C), C > 0.0, C < 1.0.
	valid_option(armijo_tau(Tau)) :-
		number(Tau), Tau > 0.0, Tau < 1.0.
	valid_option(armijo_max_backtracks(N)) :-
		integer(N), N >= 1.
	valid_option(restart(Restart)) :-
		once((Restart == none ; Restart == dimension ; integer(Restart), Restart >= 0)).
	valid_option(Option) :-
		^^valid_option(Option).

:- end_object.
