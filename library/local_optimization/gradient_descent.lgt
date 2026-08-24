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


:- object(gradient_descent(_Problem_),
	imports(local_optimization_solver(_Problem_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-24,
		comment is 'Gradient descent (steepest descent) local optimizer with optional backtracking Armijo line search. Requires the problem to define ``gradient/2``. Supports box constraints via projection, minimization and maximization.',
		parameters is [
			'Problem' - 'Problem object implementing ``local_optimization_problem_protocol`` and defining ``gradient/2``.'
		],
		remarks is [
			'Line search' - 'The ``line_search(armijo)`` option (default) uses backtracking Armijo line search. The ``line_search(fixed)`` option uses a constant step size given by ``step_size(S)``.',
			'Bounds' - 'When the problem defines ``position_bounds/1``, the gradient step is projected onto the box (projected gradient descent).',
			'Direction' - 'For minimization the search direction is ``-gradient``; for maximization it is ``+gradient``.'
		],
		see_also is [
			local_optimization_problem_protocol, local_optimization_solver(_), bfgs(_), conjugate_gradient(_), nelder_mead(_)
		]
	]).

	:- uses(_Problem_, [
		objective/2, gradient/2, position_bounds/1, stop_condition/3, progress/5
	]).

	:- uses(linear_algebra, [
		add_vectors/3, dot_product/3, euclidean_norm/2, scale_vector/3, subtract_vectors/3
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
		^^option(line_search(LineSearch), Options),
		^^option(step_size(StepSize), Options),
		^^option(armijo_c(ArmijoC), Options),
		^^option(armijo_tau(ArmijoTau), Options),
		^^option(armijo_max_backtracks(MaxBT), Options),
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
		require_gradient,
		objective(Point0, Value0),
		(	number(Value0) -> true ; domain_error(objective, Value0) ),
		gradient(Point0, Grad0),
		validate_gradient(Point0, Grad0),
		euclidean_norm(Grad0, GradNorm0),
		loop(
			0, MaxIterations, UpdateInterval, Bounds, ObjDir, Target,
			LineSearch, StepSize, ArmijoC, ArmijoTau, MaxBT,
			TolX, TolF, TolG,
			Point0, Value0, Grad0, GradNorm0,
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

	loop(
		Iter, MaxIterations, UpdInt, _Bounds, _ObjDir, _Target,
		_LS, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, _TolG,
		Point, Value, _Grad, GradNorm,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		Iter >= MaxIterations,
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Bounds, ObjDir, Target,
		_LS, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, _TolG,
		Point, Value, _Grad, GradNorm,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		^^target_reached(ObjDir, Value, Target),
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Bounds, _ObjDir, _Target,
		_LS, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, _TolG,
		Point, Value, _Grad, GradNorm,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		stop_condition(Iter, Point, Value),
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Bounds, _ObjDir, _Target,
		_LS, _Step, _C, _Tau, _MaxBT, _TolX, _TolF, TolG,
		Point, Value, _Grad, GradNorm,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals, GradNorm
	) :-
		GradNorm =< TolG,
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, MaxIterations, UpdInt, Bounds, ObjDir, Target,
		LineSearch, StepSize, ArmijoC, ArmijoTau, MaxBT,
		TolX, TolF, TolG,
		Point0, Value0, Grad0, _GradNorm0,
		Evals0, GradEvals0,
		BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, FinalGradNorm
	) :-
		% search direction: -g for minimize, +g for maximize
		(	ObjDir == minimize ->
			scale_vector(Grad0, -1.0, Direction)
		;	Direction = Grad0
		),
		line_search(
			LineSearch, Point0, Value0, Grad0, Direction, Bounds, ObjDir,
			StepSize, ArmijoC, ArmijoTau, MaxBT,
			Point1, Value1, _StepUsed, Evals0, Evals1
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
			% step or function change too small – stop
			^^report_final(Iter1, UpdInt, Point1, Value1, GradNorm1),
			BestPoint = Point1,
			BestValue = Value1,
			Iterations = Iter1,
			Evaluations = Evals1,
			GradEvaluations = GradEvals1,
			FinalGradNorm = GradNorm1
		;	loop(
				Iter1, MaxIterations, UpdInt, Bounds, ObjDir, Target,
				LineSearch, StepSize, ArmijoC, ArmijoTau, MaxBT,
				TolX, TolF, TolG,
				Point1, Value1, Grad1, GradNorm1,
				Evals1, GradEvals1,
				BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, FinalGradNorm
			)
		).

	% line search

	line_search(fixed, Point0, _Value0, _Grad0, Direction, Bounds, _ObjDir,
			StepSize, _C, _Tau, _MaxBT,
			Point1, Value1, StepSize, Evals0, Evals1) :-
		!,
		scale_vector(Direction, StepSize, Step),
		add_vectors(Point0, Step, Point1_0),
		^^project_to_bounds(Point1_0, Bounds, Point1),
		objective(Point1, Value1),
		(	number(Value1) -> true ; domain_error(objective, Value1) ),
		Evals1 is Evals0 + 1.

	line_search(armijo, Point0, Value0, Grad0, Direction, Bounds, ObjDir,
			Step0, C, Tau, MaxBT,
			Point1, Value1, StepUsed, Evals0, Evals1) :-
		% directional derivative: g·d  (already signed for min/max)
		dot_product(Grad0, Direction, DirDeriv),
		armijo_backtrack(
			0, MaxBT, Point0, Value0, Direction, Bounds, ObjDir,
			Step0, C, Tau, DirDeriv,
			Point1, Value1, StepUsed, Evals0, Evals1
		).

	armijo_backtrack(
		BT, MaxBT, Point0, Value0, Direction, Bounds, ObjDir,
		Step, C, Tau, DirDeriv,
		Point1, Value1, StepUsed, Evals0, Evals1
	) :-
		scale_vector(Direction, Step, Scaled),
		add_vectors(Point0, Scaled, Trial0),
		^^project_to_bounds(Trial0, Bounds, Trial),
		objective(Trial, TrialVal),
		(	number(TrialVal) -> true ; domain_error(objective, TrialVal) ),
		Evals1_try is Evals0 + 1,
		% Armijo condition (minimization form):
		%   f(x + alpha d) =< f(x) + c · alpha · (g·d)
		Sufficient is Value0 + C * Step * DirDeriv,
		(	(	ObjDir == minimize,
				TrialVal =< Sufficient
			;	ObjDir == maximize,
				TrialVal >= Sufficient
			) ->
			Point1 = Trial,
			Value1 = TrialVal,
			StepUsed = Step,
			Evals1 = Evals1_try
		;	BT1 is BT + 1,
			(	BT1 >= MaxBT ->
				% accept the last trial even if Armijo failed
				Point1 = Trial,
				Value1 = TrialVal,
				StepUsed = Step,
				Evals1 = Evals1_try
			;	Step1 is Step * Tau,
				armijo_backtrack(
					BT1, MaxBT, Point0, Value0, Direction, Bounds, ObjDir,
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

	% progress hook -> problem progress/5
	progress_hook(Iteration, BestPoint, BestValue, Measure, Evaluations) :-
		ignore(progress(Iteration, BestPoint, BestValue, Measure, Evaluations)).

	% options specific to gradient descent

	default_option(line_search(armijo)).
	default_option(step_size(1.0)).
	default_option(armijo_c(1.0e-4)).
	default_option(armijo_tau(0.5)).
	default_option(armijo_max_backtracks(20)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(line_search(LineSearch)) :-
		once((LineSearch == armijo ; LineSearch == fixed)).
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
