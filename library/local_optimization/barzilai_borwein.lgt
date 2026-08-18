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


:- object(barzilai_borwein(_Problem_),
	imports(local_optimization_solver)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-18,
		comment is 'Barzilai-Borwein (BB) gradient method with adaptive step sizes. Requires the problem to define ``gradient/2``. Supports box constraints via projection, minimization and maximization, and BB1 / BB2 / alternating formulas.',
		parameters is [
			'Problem' - 'Problem object implementing ``local_optimization_problem_protocol`` and defining ``gradient/2``.'
		],
		remarks is [
			'Step formulas' - '``formula(bb1)`` uses ``alpha = (s*s)/(s*y)`` (long step). ``formula(bb2)`` uses ``alpha = (s*y)/(y*y)`` (short step). ``formula(alternate)`` (default) switches between BB1 and BB2 each iteration.',
			'Safeguards' - 'When the denominator is near zero or the computed step is outside ``[step_min, step_max]``, the previous accepted step (or ``step_size`` on the first iteration) is reused.',
			'Line search' - 'Optional non-monotone Armijo acceptance controlled by ``line_search(none|armijo)``. The default ``none`` accepts the pure BB step.',
			'Bounds' - 'When the problem defines ``position_bounds/1``, trial points are projected onto the box after each step.'
		],
		see_also is [
			local_optimization_problem_protocol,
			local_optimization_solver,
			gradient_descent(_),
			conjugate_gradient(_),
			nelder_mead(_)
		]
	]).

	:- uses(_Problem_, [
		initial_point/1, objective/2, gradient/2, position_bounds/1,
		stop_condition/3, progress/5
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
		^^option(max_iterations(MaxIter), Options),
		^^option(tol_x(TolX), Options),
		^^option(tol_f(TolF), Options),
		^^option(tol_g(TolG), Options),
		^^option(updates(Updates), Options),
		^^option(formula(Formula), Options),
		^^option(step_size(Step0), Options),
		^^option(step_min(StepMin), Options),
		^^option(step_max(StepMax), Options),
		^^option(line_search(LineSearch), Options),
		^^option(armijo_c(ArmijoC), Options),
		^^option(armijo_tau(ArmijoTau), Options),
		^^option(armijo_max_backtracks(MaxBT), Options),
		(	Updates > 0 ->
			UpdateInterval is max(1, (MaxIter - 1) // Updates)
		;	UpdateInterval = 0
		),
		initial_point(Point0),
		(	position_bounds(Bounds) ->
			^^validate_bounds(Bounds),
			^^validate_point(Point0, Bounds)
		;	Bounds = [],
			^^validate_point(Point0, [])
		),
		require_gradient,
		objective(Point0, Value0),
		(	number(Value0) -> true ; domain_error(objective, Value0) ),
		gradient(Point0, Grad0),
		validate_gradient(Point0, Grad0),
		% work in phi-space (always minimize): Sign = 1 for minimize,
		% Sign = -1 for maximize. PhiGrad = Sign * Grad is the gradient
		% of the function being minimized, so BB formulas and the search
		% direction never need an explicit maximize branch
		direction_sign(ObjDir, Sign),
		scale_vector(Grad0, Sign, PhiGrad0),
		euclidean_norm(PhiGrad0, GradNorm0),
		loop(
			0, MaxIter, UpdateInterval, Bounds, Sign, Target,
			Formula, Step0, StepMin, StepMax,
			LineSearch, ArmijoC, ArmijoTau, MaxBT,
			TolX, TolF, TolG,
			Point0, Value0, PhiGrad0, GradNorm0,
			% no previous s, y on first iteration
			[], [], Step0, 0,
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
		Iter, MaxIter, UpdInt, _Bounds, _Sign, _Target,
		_Formula, _Step, _StepMin, _StepMax, _LS, _C, _Tau, _MaxBT,
		_TolX, _TolF, _TolG,
		Point, Value, _PhiGrad, GradNorm, _S, _Y, _Alpha, _Parity,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		Iter >= MaxIter,
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIter, UpdInt, _Bounds, Sign, Target,
		_Formula, _Step, _StepMin, _StepMax, _LS, _C, _Tau, _MaxBT,
		_TolX, _TolF, _TolG,
		Point, Value, _PhiGrad, GradNorm, _S, _Y, _Alpha, _Parity,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		objective_direction(Sign, ObjDir),
		^^target_reached(ObjDir, Value, Target),
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIter, UpdInt, _Bounds, _Sign, _Target,
		_Formula, _Step, _StepMin, _StepMax, _LS, _C, _Tau, _MaxBT,
		_TolX, _TolF, _TolG,
		Point, Value, _PhiGrad, GradNorm, _S, _Y, _Alpha, _Parity,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		stop_condition(Iter, Point, Value),
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIter, UpdInt, _Bounds, _Sign, _Target,
		_Formula, _Step, _StepMin, _StepMax, _LS, _C, _Tau, _MaxBT,
		_TolX, _TolF, TolG,
		Point, Value, _PhiGrad, GradNorm, _S, _Y, _Alpha, _Parity,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		GradNorm =< TolG,
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, MaxIter, UpdInt, Bounds, Sign, Target,
		Formula, _PrevAlpha, StepMin, StepMax,
		LineSearch, ArmijoC, ArmijoTau, MaxBT,
		TolX, TolF, TolG,
		Point0, Value0, PhiGrad0, _GradNorm0,
		S0, Y0, Alpha0, Parity0,
		Evals0, GradEvals0,
		BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, FinalGradNorm
	) :-
		% descent direction on the (always-minimize) phi surface
		scale_vector(PhiGrad0, -1.0, Direction),
		bb_step(Formula, Parity0, S0, Y0, Alpha0, StepMin, StepMax, Alpha),
		take_step(
			LineSearch, Point0, Value0, Sign, Direction, Bounds,
			Alpha, ArmijoC, ArmijoTau, MaxBT,
			Point1, Value1, AlphaUsed, Evals0, Evals1
		),
		gradient(Point1, Grad1),
		validate_gradient(Point1, Grad1),
		GradEvals1 is GradEvals0 + 1,
		scale_vector(Grad1, Sign, PhiGrad1),
		euclidean_norm(PhiGrad1, GradNorm1),
		% s = x1 - x0, y = phi_g1 - phi_g0  (curvature on the minimize surface)
		subtract_vectors(Point1, Point0, S1),
		subtract_vectors(PhiGrad1, PhiGrad0, Y1),
		euclidean_norm(S1, StepNorm),
		AbsDf is abs(Value1 - Value0),
		Iter1 is Iter + 1,
		Parity1 is Parity0 + 1,
		^^report_progress(Iter1, UpdInt, Point1, Value1, GradNorm1, Evals1),
		% require BOTH a tiny step and a tiny function change. A pure BB
		% step can leave f unchanged (e.g. sphere from [3,4] to [-3,-4]
		% with alpha=1) while still far from a stationary point; treating
		% AbsDf alone as convergence would stop after that first step
		(	StepNorm =< TolX, AbsDf =< TolF ->
			^^report_final(Iter1, UpdInt, Point1, Value1, GradNorm1),
			BestPoint = Point1,
			BestValue = Value1,
			Iterations = Iter1,
			Evaluations = Evals1,
			GradEvaluations = GradEvals1,
			FinalGradNorm = GradNorm1
		;	loop(
				Iter1, MaxIter, UpdInt, Bounds, Sign, Target,
				Formula, AlphaUsed, StepMin, StepMax,
				LineSearch, ArmijoC, ArmijoTau, MaxBT,
				TolX, TolF, TolG,
				Point1, Value1, PhiGrad1, GradNorm1,
				S1, Y1, AlphaUsed, Parity1,
				Evals1, GradEvals1,
				BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, FinalGradNorm
			)
		).

	direction_sign(minimize, 1.0).
	direction_sign(maximize, -1.0).

	objective_direction(1.0, minimize).
	objective_direction(-1.0, maximize).

	% Barzilai-Borwein step

	% first iteration or empty history -> use fallback Alpha0
	bb_step(_Formula, _Parity, [], _, Alpha0, StepMin, StepMax, Alpha) :-
		!,
		clamp_step(Alpha0, StepMin, StepMax, Alpha).
	bb_step(_Formula, _Parity, _, [], Alpha0, StepMin, StepMax, Alpha) :-
		!,
		clamp_step(Alpha0, StepMin, StepMax, Alpha).

	bb_step(bb1, _Parity, S, Y, Alpha0, StepMin, StepMax, Alpha) :-
		!,
		bb1_step(S, Y, Alpha0, StepMin, StepMax, Alpha).
	bb_step(bb2, _Parity, S, Y, Alpha0, StepMin, StepMax, Alpha) :-
		!,
		bb2_step(S, Y, Alpha0, StepMin, StepMax, Alpha).
	bb_step(alternate, Parity, S, Y, Alpha0, StepMin, StepMax, Alpha) :-
		(	Parity mod 2 =:= 0 ->
			bb1_step(S, Y, Alpha0, StepMin, StepMax, Alpha)
		;	bb2_step(S, Y, Alpha0, StepMin, StepMax, Alpha)
		).

	% alpha = (s*s) / (s*y)
	bb1_step(S, Y, Alpha0, StepMin, StepMax, Alpha) :-
		dot_product(S, S, Num),
		dot_product(S, Y, Den),
		(	abs(Den) < 1.0e-16 ->
			clamp_step(Alpha0, StepMin, StepMax, Alpha)
		;	AlphaRaw is Num / Den,
			(	AlphaRaw > 0.0 ->
				clamp_step(AlphaRaw, StepMin, StepMax, Alpha)
			;	clamp_step(Alpha0, StepMin, StepMax, Alpha)
			)
		).

	% alpha = (s*y) / (y*y)
	bb2_step(S, Y, Alpha0, StepMin, StepMax, Alpha) :-
		dot_product(S, Y, Num),
		dot_product(Y, Y, Den),
		(	abs(Den) < 1.0e-16 ->
			clamp_step(Alpha0, StepMin, StepMax, Alpha)
		;	AlphaRaw is Num / Den,
			(	AlphaRaw > 0.0 ->
				clamp_step(AlphaRaw, StepMin, StepMax, Alpha)
			;	clamp_step(Alpha0, StepMin, StepMax, Alpha)
			)
		).

	clamp_step(Alpha, StepMin, StepMax, Clamped) :-
		(	Alpha < StepMin -> Clamped = StepMin
		;	Alpha > StepMax -> Clamped = StepMax
		;	Clamped = Alpha
		).

	% step acceptance

	take_step(
		none, Point0, _Value0, _Sign, Direction, Bounds,
		Alpha, _C, _Tau, _MaxBT,
		Point1, Value1, Alpha, Evals0, Evals1
	) :-
		!,
		scale_vector(Direction, Alpha, Step),
		add_vectors(Point0, Step, Trial0),
		^^project_to_bounds(Trial0, Bounds, Point1),
		objective(Point1, Value1),
		(	number(Value1) -> true ; domain_error(objective, Value1) ),
		Evals1 is Evals0 + 1.

	% Armijo expressed in phi-space: Direction already points downhill on
	% phi, so a single sufficient-decrease test covers both objective
	% directions of the original problem.
	take_step(
		armijo, Point0, Value0, Sign, Direction, Bounds,
		Alpha0, C, Tau, MaxBT,
		Point1, Value1, AlphaUsed, Evals0, Evals1
	) :-
		% Direction = -PhiGrad, so DirDeriv = PhiGrad * Direction < 0
		scale_vector(Direction, -1.0, PhiGrad),
		dot_product(PhiGrad, Direction, DirDeriv),
		phi_armijo_backtrack(
			0, MaxBT, Point0, Value0, Sign, Direction, Bounds,
			Alpha0, C, Tau, DirDeriv,
			Point1, Value1, AlphaUsed, Evals0, Evals1
		).

	phi_armijo_backtrack(
		BT, MaxBT, Point0, Value0, Sign, Direction, Bounds,
		Step, C, Tau, DirDeriv,
		Point1, Value1, StepUsed, Evals0, Evals1
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
			StepUsed = Step,
			Evals1 = Evals1_try
		;	BT1 is BT + 1,
			(	BT1 >= MaxBT ->
				Point1 = Trial,
				Value1 = TrialVal,
				StepUsed = Step,
				Evals1 = Evals1_try
			;	Step1 is Step * Tau,
				phi_armijo_backtrack(
					BT1, MaxBT, Point0, Value0, Sign, Direction, Bounds,
					Step1, C, Tau, DirDeriv,
					Point1, Value1, StepUsed, Evals1_try, Evals1
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

	% options

	default_option(formula(alternate)).
	default_option(step_size(1.0)).
	default_option(step_min(1.0e-10)).
	default_option(step_max(1.0e10)).
	default_option(line_search(none)).
	default_option(armijo_c(1.0e-4)).
	default_option(armijo_tau(0.5)).
	default_option(armijo_max_backtracks(20)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(formula(F)) :-
		once((F == bb1 ; F == bb2 ; F == alternate)).
	valid_option(step_size(S)) :-
		number(S), S > 0.0.
	valid_option(step_min(S)) :-
		number(S), S > 0.0.
	valid_option(step_max(S)) :-
		number(S), S > 0.0.
	valid_option(line_search(LS)) :-
		once((LS == none ; LS == armijo)).
	valid_option(armijo_c(C)) :-
		number(C), C > 0.0, C < 1.0.
	valid_option(armijo_tau(T)) :-
		number(T), T > 0.0, T < 1.0.
	valid_option(armijo_max_backtracks(N)) :-
		integer(N), N >= 1.
	valid_option(Option) :-
		^^valid_option(Option).

:- end_object.
