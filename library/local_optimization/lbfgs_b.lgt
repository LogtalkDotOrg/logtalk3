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


:- object(lbfgs_b(_Problem_),
	imports(local_optimization_solver)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-18,
		comment is 'L-BFGS-B bound-constrained limited-memory quasi-Newton optimizer. Uses a projected-gradient free-set identification, feasible-step limiting, and the standard L-BFGS two-loop recursion on the free variables. Requires ``gradient/2`` and ``position_bounds/1``.',
		parameters is [
			'Problem' - 'Problem object implementing ``local_optimization_problem_protocol`` with ``gradient/2`` and ``position_bounds/1``.'
		],
		remarks is [
			'Algorithm' - 'Each iteration (1) projects the current point onto the box, (2) builds the projected gradient and free set, (3) computes an L-BFGS direction via the two-loop recursion and zeroes components that would leave the feasible region, (4) takes the largest feasible step length as the Armijo upper bound, and (5) updates the limited-memory history only when the curvature condition holds.',
			'Versus ``lbfgs(_)``' - 'Plain ``lbfgs(_)`` only clamps trial points after an unconstrained step, which can weaken the quasi-Newton model. This solver never proposes an infeasible step and stops on the projected gradient norm.',
			'Internal minimization form' - 'Maximization is handled by minimizing the negated objective and gradient (phi-space), as in ``bfgs(_)`` and ``lbfgs(_)``.',
			'Unbounded problems' - 'When the problem does not define ``position_bounds/1``, the solver behaves like unconstrained ``lbfgs(_)`` (no free-set masking). Prefer ``lbfgs(_)`` for purely unconstrained work; prefer this solver when box constraints are present.'
		],
		see_also is [
			local_optimization_problem_protocol, local_optimization_solver, lbfgs(_), bfgs(_), gradient_descent(_)
		]
	]).

	:- uses(_Problem_, [
		initial_point/1, objective/2, gradient/2, position_bounds/1,
		stop_condition/3, progress/5
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
		(	position_bounds(Bounds) ->
			^^validate_bounds(Bounds)
		;	Bounds = []
		),
		initial_point(PointIn),
		^^validate_point(PointIn, Bounds),
		^^project_to_bounds(PointIn, Bounds, Point0),
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
		projected_gradient(Point0, PhiGrad0, Bounds, ProjGrad0),
		euclidean_norm(ProjGrad0, GradNorm0),
		loop(
			0, MaxIterations, UpdateInterval, Dimension, Bounds, Sign, Target,
			Restart, MemorySize, StepSize, ArmijoC, ArmijoTau, MaxBT,
			TolX, TolF, TolG,
			Point0, Value0, PhiGrad0, GradNorm0, [],
			1, 1,
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
		Point0, Value0, PhiGrad0, GradNorm0, History0,
		Evals0, GradEvals0,
		BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, FinalGradNorm
	) :-
		% L-BFGS direction, then restrict to the free face
		two_loop_recursion(History0, PhiGrad0, Dir0),
		mask_direction(Point0, PhiGrad0, Dir0, Bounds, Direction0),
		dot_product(PhiGrad0, Direction0, DirDeriv0),
		(	DirDeriv0 < 0.0 ->
			Direction = Direction0,
			DirDeriv = DirDeriv0
		;	% fall back to projected steepest descent on phi
			projected_gradient(Point0, PhiGrad0, Bounds, ProjG),
			scale_vector(ProjG, -1.0, Direction),
			dot_product(PhiGrad0, Direction, DirDeriv)
		),
		max_feasible_step(Point0, Direction, Bounds, StepSize, AlphaMax),
		(	AlphaMax =< 0.0 ->
			% no feasible descent direction; stop
			^^report_final(Iter, UpdInt, Point0, Value0, GradNorm0),
			BestPoint = Point0,
			BestValue = Value0,
			Iterations = Iter,
			Evaluations = Evals0,
			GradEvaluations = GradEvals0,
			FinalGradNorm = GradNorm0
		;	phi_armijo_line_search(
				Point0, Value0, Sign, Direction, DirDeriv, Bounds,
				AlphaMax, ArmijoC, ArmijoTau, MaxBT,
				Point1, Value1, Evals0, Evals1
			),
			gradient(Point1, Grad1),
			validate_gradient(Point1, Grad1),
			GradEvals1 is GradEvals0 + 1,
			scale_vector(Grad1, Sign, PhiGrad1),
			projected_gradient(Point1, PhiGrad1, Bounds, ProjGrad1),
			euclidean_norm(ProjGrad1, GradNorm1),
			subtract_vectors(Point1, Point0, S),
			euclidean_norm(S, StepNorm),
			AbsDf is abs(Value1 - Value0),
			Iter1 is Iter + 1,
			^^report_progress(Iter1, UpdInt, Point1, Value1, GradNorm1, Evals1),
			(	StepNorm =< TolX, AbsDf =< TolF ->
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
			)
		).

	direction_sign(minimize, 1.0).
	direction_sign(maximize, -1.0).

	objective_direction(1.0, minimize).
	objective_direction(-1.0, maximize).

	% bound-constrained predicates

	% unconstrained: projected gradient is the full gradient
	projected_gradient(Point, PhiGrad, [], PhiGrad) :-
		!,
		length(Point, _).
	% projected gradient (Bertsekas): zero components that point out of
	% the feasible box. PhiGrad is the gradient of the function being
	% *minimized*
	projected_gradient([], [], [], []).
	projected_gradient([X| Xs], [G| Gs], [L-U| Bounds], [PG| PGs]) :-
		(	X =< L, G > 0.0 ->
			PG = 0.0
		;	X >= U, G < 0.0 ->
			PG = 0.0
		;	PG = G
		),
		projected_gradient(Xs, Gs, Bounds, PGs).

	mask_direction(_Point, _G, Direction, [], Direction) :-
		!.
	% zero direction components that would leave the feasible set, and
	% zero components that are fixed by the projected-gradient rule
	mask_direction([], [], [], [], []).
	mask_direction([X| Xs], [G| Gs], [D| Ds], [L-U| Bounds], [MD| MDs]) :-
		(	X =< L, G > 0.0 ->
			MD = 0.0
		;	X >= U, G < 0.0 ->
			MD = 0.0
		;	X =< L, D < 0.0 ->
			MD = 0.0
		;	X >= U, D > 0.0 ->
			MD = 0.0
		;	MD = D
		),
		mask_direction(Xs, Gs, Ds, Bounds, MDs).

	% unconstrained: step capped only by step_size
	max_feasible_step(_Point, _Direction, [], StepCap, StepCap) :-
		!.
	% largest alpha > 0 such that x + alpha*d stays inside the box
	max_feasible_step(Point, Direction, Bounds, StepCap, AlphaMax) :-
		max_feasible_step_(Point, Direction, Bounds, StepCap, AlphaMax0),
		AlphaMax is max(0.0, AlphaMax0).

	max_feasible_step_([], [], [], Cap, Cap).
	max_feasible_step_([X| Xs], [D| Ds], [L-U| Bounds], Cap0, Cap) :-
		(	D > 1.0e-16 ->
			AlphaU is (U - X) / D,
			Cap1 is min(Cap0, AlphaU)
		;	D < -1.0e-16 ->
			AlphaL is (L - X) / D,
			Cap1 is min(Cap0, AlphaL)
		;	Cap1 = Cap0
		),
		max_feasible_step_(Xs, Ds, Bounds, Cap1, Cap).

	% Armijo (phi-space) with feasible upper bound

	phi_armijo_line_search(
		Point0, Value0, Sign, Direction, DirDeriv, Bounds,
		AlphaMax, C, Tau, MaxBT,
		Point1, Value1, Evals0, Evals1
	) :-
		phi_armijo_backtrack(
			0, MaxBT, Point0, Value0, Sign, Direction, Bounds,
			AlphaMax, C, Tau, DirDeriv,
			Point1, Value1, Evals0, Evals1
		).

	phi_armijo_backtrack(
		BT, MaxBT, Point0, Value0, Sign, Direction, Bounds,
		Step, C, Tau, DirDeriv,
		Point1, Value1, Evals0, Evals1
	) :-
		scale_vector(Direction, Step, Scaled),
		add_vectors(Point0, Scaled, Trial0),
		% numerical safety clamp (should already be feasible)
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

	% L-BFGS two-loop recursion (same as lbfgs)

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

	update_history(Iter, Restart, _MemorySize, _S, _Y, _History0, []) :-
		Restart > 0,
		Iter mod Restart =:= 0,
		!.
	update_history(_Iter, _Restart, MemorySize, S, Y, History0, History1) :-
		dot_product(Y, S, YdotS),
		(	YdotS > 1.0e-10 ->
			Rho is 1.0 / YdotS,
			cap_history([s(S, Y, Rho)| History0], MemorySize, History1)
		;	History1 = []
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

	progress_hook(Iteration, BestPoint, BestValue, Measure, Evaluations) :-
		ignore(progress(Iteration, BestPoint, BestValue, Measure, Evaluations)).

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
