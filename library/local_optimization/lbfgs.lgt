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


:- object(lbfgs(_Problem_),
	imports(local_optimization_solver(_Problem_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-24,
		comment is 'L-BFGS (limited-memory Broyden-Fletcher-Goldfarb-Shanno) quasi-Newton local optimizer with backtracking Armijo line search. Requires the problem to define ``gradient/2``. Supports optional box constraints via projection, minimization and maximization.',
		parameters is [
			'Problem' - 'Problem object implementing ``local_optimization_problem_protocol`` and defining ``gradient/2``.'
		],
		remarks is [
			'Update' - 'Instead of maintaining a dense inverse-Hessian approximation like ``bfgs(_)``, only the last ``memory_size(M)`` step/gradient-difference pairs ``(s, y)`` are kept, and the search direction is recovered from them with the standard two-loop recursion (Nocedal and Wright, Algorithm 7.4). Memory and per-iteration cost are ``O(M*n)`` instead of ``bfgs(_)``''s ``O(n^2)``.',
			'Internal minimization form' - 'Maximization is handled by internally minimizing the negated objective and gradient, so the two-loop recursion, curvature test, and Armijo condition are always expressed in minimization form, which avoids sign errors in the line search.',
			'Curvature safeguard' - 'Whenever the curvature condition ``y . s > 0`` is not comfortably satisfied (possible here since the line search only enforces sufficient decrease, not a Wolfe curvature condition), the pair history is cleared and the next step falls back to steepest descent, rather than keeping a stale history that would otherwise keep producing the same near-zero-progress direction.',
			'Restarts' - 'The ``restart(N)`` option (off by default) periodically clears the pair history, exactly as ``bfgs(_)`` resets its inverse-Hessian approximation to the identity.',
			'Bounds' - 'When the problem defines ``position_bounds/1``, trial points are projected onto the box after each step. Projection can weaken the quasi-Newton model; a pure bound-constrained formulation (L-BFGS-B style) is not implemented.'
		],
		see_also is [
			local_optimization_problem_protocol, local_optimization_solver(_), gradient_descent(_), conjugate_gradient(_), bfgs(_)
		]
	]).

	:- uses(_Problem_, [
		objective/2, gradient/2, position_bounds/1, stop_condition/3, progress/5
	]).

	:- uses(linear_algebra, [
		add_vectors/3, dot_product/3, euclidean_norm/2, scale_vector/3, subtract_vectors/3
	]).

	:- uses(list, [
		append/3, length/2, member/2, reverse/2
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
		^^option(memory_size(MemorySize), Options),
		^^option(step_size(StepSize), Options),
		^^option(armijo_c(ArmijoC), Options),
		^^option(armijo_tau(ArmijoTau), Options),
		^^option(armijo_max_backtracks(MaxBT), Options),
		^^option(restart(RestartOpt), Options),
		(	Updates > 0 ->
			UpdateInterval is max(1, (MaxIterations - 1) // Updates)
		;	UpdateInterval = 0
		),
		^^initial_point(Options, Point0),
		(	position_bounds(Bounds) ->
			^^check_bounds(Bounds),
			^^check_point(Point0, Bounds)
		;	Bounds = [],
			^^check_point(Point0, [])
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
		loop(
			0, MaxIterations, UpdateInterval, Dimension, Bounds, Sign, Target,
			Restart, MemorySize, StepSize, ArmijoC, ArmijoTau, MaxBT,
			TolX, TolF, TolG,
			Point0, Value0, PhiGrad0, GradNorm0, [],
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
	% as in bfgs(_), the objective direction is folded into a single
	% "Sign" factor (1.0 for minimize, -1.0 for maximize): PhiGrad =
	% Sign * Grad is always the gradient of the function actually
	% being *minimized*, so the two-loop recursion, curvature test,
	% and line search below never need to branch on ObjDir.
	%
	% History is a list of s(S, Y, Rho) pairs, most recent first,
	% holding at most MemorySize entries.

	loop(
		Iter, MaxIterations, UpdInt, _Dim, _Bounds, _Sign, _Target,
		_Restart, _Memory, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, _TolG,
		Point, Value, _PhiGrad, GradNorm, _History,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		Iter >= MaxIterations,
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, Sign, Target,
		_Restart, _Memory, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, _TolG,
		Point, Value, _PhiGrad, GradNorm, _History,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		objective_direction(Sign, ObjDir),
		^^target_reached(ObjDir, Value, Target),
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, _Sign, _Target,
		_Restart, _Memory, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, _TolG,
		Point, Value, _PhiGrad, GradNorm, _History,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		stop_condition(Iter, Point, Value),
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, _Sign, _Target,
		_Restart, _Memory, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, TolG,
		Point, Value, _PhiGrad, GradNorm, _History,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		GradNorm =< TolG,
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, MaxIterations, UpdInt, Dim, Bounds, Sign, Target,
		Restart, MemorySize, StepSize, ArmijoC, ArmijoTau, MaxBT,
		TolX, TolF, TolG,
		Point0, Value0, PhiGrad0, _GradNorm0, History0,
		Evals0, GradEvals0,
		BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, FinalGradNorm
	) :-
		% quasi-Newton direction on the (always-minimize) phi surface
		two_loop_recursion(History0, PhiGrad0, PhiDirection0),
		dot_product(PhiGrad0, PhiDirection0, DirDeriv0),
		(	DirDeriv0 < 0.0 ->
			% genuine descent direction on phi
			PhiDirection = PhiDirection0,
			DirDeriv = DirDeriv0
		;	% two-loop recursion lost descent (rare, e.g. right after a
			% restart with an ill-conditioned pair); fall back to
			% steepest descent on phi for this step
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
			update_history(Iter1, Restart, MemorySize, S, Y, History0, History1),
			loop(
				Iter1, MaxIterations, UpdInt, Dim, Bounds, Sign, Target,
				Restart, MemorySize, StepSize, ArmijoC, ArmijoTau, MaxBT,
				TolX, TolF, TolG,
				Point1, Value1, PhiGrad1, GradNorm1, History1,
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

	% two-loop recursion (Nocedal & Wright, Algorithm 7.4): recovers
	% the product H_k * grad without ever forming H_k explicitly.
	% History is most-recent-first; an empty history returns the
	% steepest-descent direction (H_0 = I).

	two_loop_recursion([], PhiGrad, Direction) :-
		!,
		scale_vector(PhiGrad, -1.0, Direction).
	two_loop_recursion(History, PhiGrad, Direction) :-
		forward_pass(History, PhiGrad, Q, Alphas),
		initial_scaling(History, Gamma),
		scale_vector(Q, Gamma, R0),
		reverse(History, HistoryOldestFirst),
		reverse(Alphas, AlphasOldestFirst),
		backward_pass(HistoryOldestFirst, AlphasOldestFirst, R0, R),
		scale_vector(R, -1.0, Direction).

	forward_pass([], Q, Q, []).
	forward_pass([s(S, Y, Rho)| Rest], Q0, Q, [Alpha| Alphas]) :-
		dot_product(S, Q0, SQ),
		Alpha is Rho * SQ,
		scale_vector(Y, Alpha, ScaledY),
		subtract_vectors(Q0, ScaledY, Q1),
		forward_pass(Rest, Q1, Q, Alphas).

	initial_scaling([s(S, Y, _)| _], Gamma) :-
		dot_product(Y, Y, YY),
		(	YY > 0.0 ->
			dot_product(S, Y, SY),
			Gamma is SY / YY
		;	Gamma = 1.0
		).

	backward_pass([], [], R, R).
	backward_pass([s(S, Y, Rho)| Rest], [Alpha| Alphas], R0, R) :-
		dot_product(Y, R0, YR),
		Beta is Rho * YR,
		Coefficient is Alpha - Beta,
		scale_vector(S, Coefficient, ScaledS),
		add_vectors(R0, ScaledS, R1),
		backward_pass(Rest, Alphas, R1, R).

	% pair-history maintenance

	update_history(Iter, Restart, _MemorySize, _S, _Y, _History0, []) :-
		Restart > 0,
		Iter mod Restart =:= 0,
		!.
	update_history(_Iter, _Restart, MemorySize, S, Y, History0, History1) :-
		dot_product(Y, S, YdotS),
		(	YdotS > 1.0e-10 ->
			Rho is 1.0 / YdotS,
			cap_history([s(S, Y, Rho)| History0], MemorySize, History1)
		;	% curvature condition not (comfortably) satisfied: this
			% line search only enforces sufficient decrease, not a
			% Wolfe curvature condition, so this can legitimately
			% happen (e.g. on Rosenbrock's curved valley). Clearing
			% the history, rather than keeping the stale one, avoids
			% repeatedly recomputing the same near-zero-progress
			% direction from an approximation that is no longer
			% consistent with the local curvature; an empty history
			% falls back to steepest descent for the next step.
			History1 = []
		).

	cap_history(History, MemorySize, History) :-
		length(History, Length),
		Length =< MemorySize,
		!.
	cap_history(History, MemorySize, Capped) :-
		length(Capped, MemorySize),
		append(Capped, _Discarded, History).

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

	% progress hook - problem progress/5
	progress_hook(Iteration, BestPoint, BestValue, Measure, Evaluations) :-
		ignore(progress(Iteration, BestPoint, BestValue, Measure, Evaluations)).

	% options specific to L-BFGS

	default_option(memory_size(10)).
	default_option(step_size(1.0)).
	default_option(armijo_c(1.0e-4)).
	default_option(armijo_tau(0.5)).
	default_option(armijo_max_backtracks(20)).
	default_option(restart(none)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(memory_size(MemorySize)) :-
		integer(MemorySize), MemorySize >= 1.
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
