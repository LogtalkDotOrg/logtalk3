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


:- object(log_barrier(_Problem_, _InnerSolver_),
	imports([constrained_optimization_solver, local_optimization_solver(_Problem_)])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Interior-point log-barrier method for inequality constraints, optionally combined with a quadratic penalty for equality constraints.',
		parameters is [
			'Problem' - 'Problem object implementing ``constrained_optimization_problem_protocol``. The initial point need not be strictly feasible.',
			'InnerSolver' - 'Atom naming a local_optimization solver class taking a single problem parameter, e.g. ``bfgs`` or ``lbfgs``. Constructed dynamically each outer iteration as ``InnerSolver(SubProblem)``.'
		],
		see_also is [
			constrained_optimization_problem_protocol, augmented_lagrangian(_, _), quadratic_penalty(_, _),
			sqp_active_set(_), local_optimization_solver(_), bfgs(_), lbfgs(_)
		]
	]).

	:- uses(_Problem_, [
		objective/2, progress/5, equality_constraints/2, inequality_constraints/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	% public entry point

	run(BestPoint, BestValue, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(objective(ObjDir), Options),
		^^option(target_value(Target), Options),
		^^option(max_iterations(InnerMaxIterations), Options),
		^^option(tol_x(InnerTolX), Options),
		^^option(tol_f(InnerTolF), Options),
		^^option(tol_g(InnerTolG), Options),
		^^option(updates(OuterUpdates), Options),
		^^option(inner_updates(InnerUpdates), Options),
		^^option(mu0(Mu0), Options),
		^^option(mu_scale(MuScale), Options),
		^^option(rho0(Rho0), Options),
		^^option(rho_scale(RhoScale), Options),
		^^option(outer_violation_decrease(ViolDecrease), Options),
		^^option(max_outer_iterations(MaxOuter), Options),
		^^option(outer_tolerance(OuterTol), Options),
		InnerOptions = [
			objective(minimize), max_iterations(InnerMaxIterations),
			tol_x(InnerTolX), tol_f(InnerTolF), tol_g(InnerTolG), updates(InnerUpdates)
		],
		^^initial_point(Options, Point0),
		^^direction_sign(ObjDir, Sign),
		ensure_strictly_feasible(Point0, InnerOptions, StartPoint),
		equality_violation(StartPoint, EqViol0),
		outer_loop(
			0, MaxOuter, OuterUpdates, OuterTol, Mu0, MuScale, Rho0, RhoScale, ViolDecrease, Sign, Target, InnerOptions,
			StartPoint, EqViol0,
			0, 0, 0,
			BestPoint, BestValue, OuterIterations, InnerIterations, Evaluations, GradEvaluations, FinalMu, FinalEqViolation
		),
		outer_termination_reason(OuterIterations, MaxOuter, ObjDir, Target, BestValue, FinalMu, FinalEqViolation, OuterTol, TerminationReason),
		Statistics = [
			outer_iterations(OuterIterations),
			inner_iterations(InnerIterations),
			evaluations(Evaluations),
			gradient_evaluations(GradEvaluations),
			final_mu(FinalMu),
			final_equality_violation(FinalEqViolation),
			termination_reason(TerminationReason),
			final_value(BestValue)
		].

	outer_termination_reason(Iterations, MaxIterations, ObjDir, Target, Value, Mu, EqViolation, Tolerance, Reason) :-
		(	Iterations >= MaxIterations, (Mu > Tolerance ; EqViolation > Tolerance) ->
			Reason = max_iterations
		;	Mu =< Tolerance, EqViolation =< Tolerance ->
			Reason = converged
		;	^^target_reached(ObjDir, Value, Target), EqViolation =< Tolerance ->
			Reason = target_reached
		;	Reason = max_iterations
		).

	% initial_point/1 need not already be strictly feasible: if it
	% is not, a heuristic phase 1 (minimizing the total squared
	% inequality violation, pulled slightly past zero so a successful
	% minimizer lands strictly inside the barrier's domain rather than
	% exactly on its boundary) is attempted first, delegating to the
	% same InnerSolver the main algorithm uses. This is a heuristic,
	% not a guaranteed method - the same caveat, and for the same
	% underlying reason (an unconstrained minimization can converge to
	% a local rather than global minimum of the violation, or simply
	% fail to reach zero on a genuinely infeasible problem), as
	% qp_active_set's own phase-1 heuristic. If it does not produce a
	% strictly feasible point, domain_error/2 is raised exactly as
	% before phase 1 existed, so this is a purely additive capability:
	% every problem this predicate used to accept, it still accepts,
	% unchanged, since the strict-feasibility check on Point0 itself is
	% tried first and phase 1 only runs when that check fails.

	ensure_strictly_feasible(Point0, InnerOptions, StartPoint) :-
		(	inequality_constraints(Point0, InequalityValues0) ->
			(	all_strictly_interior(InequalityValues0) ->
				StartPoint = Point0
			;	phase1_find_feasible(Point0, InnerOptions, StartPoint)
			)
		;	StartPoint = Point0
		).

	phase1_find_feasible(Point0, InnerOptions, StartPoint) :-
		^^build_inner(_InnerSolver_, phase1_sub_problem(_Problem_, Point0), InnerObj),
		(	InnerObj::run(Candidate, _Value, _Stats, InnerOptions),
			inequality_constraints(Candidate, InequalityValues1),
			all_strictly_interior(InequalityValues1) ->
			StartPoint = Candidate
		;	domain_error(strictly_feasible_initial_point, Point0)
		).

	% initial_point/1 must be strictly feasible: every inequality
	% constraint value must be comfortably negative, using the same
	% -1.0e-10 margin the internal subproblem uses to decide when to
	% fall back away from the true (undefined-at-the-boundary) barrier
	% term; see the log_barrier(_, _) subsection in NOTES.md.

	all_strictly_interior([]).
	all_strictly_interior([V| Vs]) :-
		V < -1.0e-10,
		all_strictly_interior(Vs).

	% outer loop
	%
	% As in augmented_lagrangian(_,_)/quadratic_penalty(_,_), every call
	% except the MaxOuter-reached base case performs exactly one inner
	% solve before deciding whether to stop.

	outer_loop(
		Iter, MaxOuter, _OuterUpdates, _OuterTol, Mu, _MuScale, _Rho, _RhoScale, _ViolDecrease, _Sign, _Target, _InnerOptions,
		Point, EqViol,
		InnerIters, Evals, GradEvals,
		Point, Value, Iter, InnerIters, Evals, GradEvals, Mu, EqViol
	) :-
		Iter >= MaxOuter,
		!,
		objective(Point, Value).

	outer_loop(
		Iter, MaxOuter, OuterUpdates, OuterTol, Mu0, MuScale, Rho0, RhoScale, ViolDecrease, Sign, Target, InnerOptions,
		Point0, EqViol0,
		InnerIters0, Evals0, GradEvals0,
		BestPoint, BestValue, OuterIterations, InnerIterations, Evaluations, GradEvaluations, FinalMu, FinalEqViolation
	) :-
		Iter < MaxOuter,
		OuterIteration is Iter + 1,
		^^build_inner(_InnerSolver_, barrier_sub_problem(_Problem_, OuterIteration, Point0, Mu0, Rho0, Sign), InnerObj),
		InnerObj::run(Point1, _SubValue1, InnerStats, InnerOptions),
		memberchk(iterations(InnerIter), InnerStats),
		memberchk(evaluations(InnerEval), InnerStats),
		memberchk(gradient_evaluations(InnerGradEval), InnerStats),
		InnerIters1 is InnerIters0 + InnerIter,
		Evals1 is Evals0 + InnerEval,
		GradEvals1 is GradEvals0 + InnerGradEval,
		Mu1 is Mu0 * MuScale,
		equality_violation(Point1, EqViol1),
		^^update_penalty(EqViol0, EqViol1, ViolDecrease, Rho0, RhoScale, Rho1),
		objective(Point1, Value1),
		Iter1 is Iter + 1,
		report_outer(Iter1, OuterUpdates, Point1, Value1, Mu1, EqViol1, Evals1),
		^^objective_direction(Sign, ObjDir),
		(	(Mu1 =< OuterTol, EqViol1 =< OuterTol ; (^^target_reached(ObjDir, Value1, Target), EqViol1 =< OuterTol)) ->
			BestPoint = Point1,
			BestValue = Value1,
			OuterIterations = Iter1,
			InnerIterations = InnerIters1,
			Evaluations = Evals1,
			GradEvaluations = GradEvals1,
			FinalMu = Mu1,
			FinalEqViolation = EqViol1
		;	outer_loop(
				Iter1, MaxOuter, OuterUpdates, OuterTol, Mu1, MuScale, Rho1, RhoScale, ViolDecrease, Sign, Target, InnerOptions,
				Point1, EqViol1,
				InnerIters1, Evals1, GradEvals1,
				BestPoint, BestValue, OuterIterations, InnerIterations, Evaluations, GradEvaluations, FinalMu, FinalEqViolation
			)
		).

	report_outer(Iter, Updates, Point, Value, Mu, EqViol, Evals) :-
		(	Updates > 0, Iter > 0, Iter mod Updates =:= 0 ->
			ignore(progress(Iter, Point, Value, mu(Mu)-equality_violation(EqViol), Evals))
		;	true
		).

	% equality violation (on the ORIGINAL problem) and penalty growth;
	% no inequality-violation counterpart is needed

	equality_violation(Point, Violation) :-
		(	equality_constraints(Point, EqualityValues) ->
			^^validate_numeric_vector(EqualityValues, equality_constraint_data),
			^^abs_max(EqualityValues, Violation)
		;	Violation = 0.0
		).

	% options specific to log_barrier

	default_option(mu0(1.0)).
	default_option(mu_scale(0.2)).
	default_option(rho0(1.0)).
	default_option(rho_scale(10.0)).
	default_option(outer_violation_decrease(0.25)).
	default_option(max_outer_iterations(30)).
	default_option(outer_tolerance(1.0e-6)).
	default_option(inner_updates(0)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(mu0(Mu)) :-
		number(Mu), Mu > 0.0.
	valid_option(mu_scale(Scale)) :-
		number(Scale), Scale > 0.0, Scale < 1.0.
	valid_option(rho0(Rho)) :-
		number(Rho), Rho > 0.0.
	valid_option(rho_scale(Scale)) :-
		number(Scale), Scale > 1.0.
	valid_option(outer_violation_decrease(D)) :-
		number(D), D > 0.0, D < 1.0.
	valid_option(max_outer_iterations(N)) :-
		integer(N), N >= 1.
	valid_option(outer_tolerance(T)) :-
		number(T), T >= 0.0.
	valid_option(inner_updates(N)) :-
		integer(N), N >= 0.
	valid_option(Option) :-
		^^valid_option(Option).

:- end_object.


:- object(barrier_sub_problem(_Problem_, _OuterIteration_, _StartPoint_, _Mu_, _Rho_, _Sign_),
	implements(local_optimization_problem_protocol),
	imports(constrained_optimization_solver)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Internal log-barrier subproblem: objective(x) = Sign*f(x) - mu*sum_j log(-h_j(x)) + (rho/2)*||g(x)||^2, with the barrier term replaced, per inequality constraint, by a smooth large-coefficient quadratic penalty whenever that constraint is not comfortably strictly satisfied. Instantiated fresh by log_barrier(_,_) every outer iteration, starting from StartPoint (the previous outer iterate); not meant to be used directly.',
		parameters is [
			'Problem' - 'The original constrained problem.',
			'OuterIteration' - 'One-based outer iteration number.',
			'StartPoint' - 'Point the inner solve should start from (the current outer iterate, always strictly feasible with respect to the inequality constraints).',
			'Mu' - 'Current barrier weight.',
			'Rho' - 'Current equality-penalty weight.',
			'Sign' - '1.0 to minimize the original objective, -1.0 to maximize it.'
		]
	]).

	:- uses(_Problem_, [
		position_bounds/1, stop_condition/3, equality_constraints/2, equality_jacobian/2,
		inequality_constraints/2, inequality_jacobian/2
	]).

	:- uses(linear_algebra, [
		add_vectors/3, dot_product/3, matrix_vector_product/3, new_vector/3, scale_vector/3,
		transpose_matrix/2
	]).

	:- uses(list, [
		length/2
	]).

	initial_point(_StartPoint_).

	objective(Point, Value) :-
		_Problem_::objective(Point, Value0),
		barrier_value(Point, BarrierValue),
		equality_penalty_value(Point, PenaltyValue),
		Value is _Sign_ * Value0 + BarrierValue + PenaltyValue.

	gradient(Point, Gradient) :-
		_Problem_::gradient(Point, Gradient0),
		scale_vector(Gradient0, _Sign_, ScaledGradient0),
		barrier_gradient(Point, BarrierGradient),
		equality_penalty_gradient(Point, EqualityGradient),
		add_vectors(ScaledGradient0, BarrierGradient, ObjectiveAndBarrierGradient),
		add_vectors(ObjectiveAndBarrierGradient, EqualityGradient, Gradient).

	progress(Iteration, Point, Value, Measure, Evaluations) :-
		ignore(_Problem_::inner_progress(outer(_OuterIteration_), Iteration, Point, Value, Measure, Evaluations)).

	% barrier value, per constraint: the true term -mu*log(-h_j(x)) when
	% h_j(x) < -1.0e-10 (comfortably inside the barrier's domain),
	% otherwise a smooth quadratic fallback with a large fixed
	% coefficient (not mu) that a line search can always backtrack out
	% of safely

	barrier_value(Point, BarrierValue) :-
		(	inequality_constraints(Point, InequalityValues) ->
			^^validate_numeric_vector(InequalityValues, inequality_constraint_data),
			per_constraint_barrier_value(InequalityValues, BarrierValue)
		;	BarrierValue = 0.0
		).

	per_constraint_barrier_value(Values, BarrierValue) :-
		per_constraint_barrier_value(Values, 0.0, BarrierValue).

	per_constraint_barrier_value([], BarrierValue, BarrierValue).
	per_constraint_barrier_value([Value| Values], BarrierValue0, BarrierValue) :-
		(	Value < -1.0e-10 ->
			U is -Value,
			Term is -_Mu_ * log(U)
		;	Shifted is Value + 1.0e-10,
			Active is max(0.0, Shifted),
			Term is 1.0e8 * Active * Active
		),
		BarrierValue1 is BarrierValue0 + Term,
		per_constraint_barrier_value(Values, BarrierValue1, BarrierValue).

	barrier_gradient(Point, Gradient) :-
		length(Point, N),
		(	inequality_constraints(Point, InequalityValues) ->
			(	inequality_jacobian(Point, InequalityJacobian) ->
				true
			;	existence_error(procedure, inequality_jacobian/2)
			),
			^^validate_constraint_data(Point, InequalityValues, InequalityJacobian, inequality_constraint_data),
			per_constraint_barrier_coefficients(InequalityValues, Coefficients),
			transpose_matrix(InequalityJacobian, InequalityJacobianTranspose),
			matrix_vector_product(InequalityJacobianTranspose, Coefficients, Gradient)
		;	new_vector(N, 0.0, Gradient)
		).

	per_constraint_barrier_coefficients([], []).
	per_constraint_barrier_coefficients([Value| Values], [Coefficient| Coefficients]) :-
		( Value < -1.0e-10 ->
			U is -Value,
			Coefficient is _Mu_ / U
		; Shifted is Value + 1.0e-10,
			Active is max(0.0, Shifted),
			Coefficient is 2.0 * 1.0e8 * Active
		),
		per_constraint_barrier_coefficients(Values, Coefficients).

	% equality penalty: identical formula to quadratic_penalty(_,_)'s
	% own eq_penalty_value/eq_penalty_gradient

	equality_penalty_value(Point, PenaltyValue) :-
		(	equality_constraints(Point, EqualityValues) ->
			^^validate_numeric_vector(EqualityValues, equality_constraint_data),
			dot_product(EqualityValues, EqualityValues, SquaredViolation),
			PenaltyValue is 0.5 * _Rho_ * SquaredViolation
		;	PenaltyValue = 0.0
		).

	equality_penalty_gradient(Point, Gradient) :-
		length(Point, N),
		(	equality_constraints(Point, EqualityValues) ->
			(	equality_jacobian(Point, EqualityJacobian) ->
				true
			;	existence_error(procedure, equality_jacobian/2)
			),
			^^validate_constraint_data(Point, EqualityValues, EqualityJacobian, equality_constraint_data),
			scale_vector(EqualityValues, _Rho_, PenaltyCoefficients),
			transpose_matrix(EqualityJacobian, EqualityJacobianTranspose),
			matrix_vector_product(EqualityJacobianTranspose, PenaltyCoefficients, Gradient)
		;	new_vector(N, 0.0, Gradient)
		).

:- end_object.


:- object(phase1_sub_problem(_Problem_, _StartPoint_),
	implements(local_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Internal phase-1 feasibility subproblem for log_barrier(_,_): minimizes sum_j max(0, h_j(x) + margin)^2 (margin = 1.0e-6), an ordinary smooth, everywhere-defined unconstrained objective, so a minimizer that reaches (near) zero gives a point with every h_j(x) < -margin, comfortably inside the barrier''s domain rather than exactly on its boundary. Instantiated once, when initial_point/1 is not already strictly feasible; not meant to be used directly. See log_barrier(_,_)''s "ensure_strictly_feasible/3" for how the result is validated and how this heuristic falls back to raising domain_error/2 when it does not succeed.',
		parameters is [
			'Problem' - 'The original constrained problem.',
			'StartPoint' - 'The point log_barrier(_,_) was actually given (Problem''s own initial_point/1); phase 1 starts its search from here.'
		]
	]).

	:- uses(_Problem_, [
		position_bounds/1, inequality_constraints/2, inequality_jacobian/2
	]).

	:- uses(linear_algebra, [
		matrix_vector_product/3, transpose_matrix/2
	]).

	initial_point(_StartPoint_).

	objective(Point, Value) :-
		inequality_constraints(Point, InequalityValues),
		violation_value(InequalityValues, Value).

	gradient(Point, Gradient) :-
		inequality_constraints(Point, InequalityValues),
		(	inequality_jacobian(Point, InequalityJacobian) ->
			true
		;	existence_error(procedure, inequality_jacobian/2)
		),
		violation_coefficients(InequalityValues, Coefficients),
		transpose_matrix(InequalityJacobian, InequalityJacobianTranspose),
		matrix_vector_product(InequalityJacobianTranspose, Coefficients, Gradient).

	progress(Iteration, Point, Value, Measure, Evaluations) :-
		ignore(_Problem_::inner_progress(phase1, Iteration, Point, Value, Measure, Evaluations)).

	violation_value(Values, Value) :-
		violation_value(Values, 0.0, Value).

	violation_value([], Value, Value).
	violation_value([InequalityValue| InequalityValues], Value0, Value) :-
		Shifted is InequalityValue + 1.0e-6,
		Active is max(0.0, Shifted),
		Value1 is Value0 + Active * Active,
		violation_value(InequalityValues, Value1, Value).

	violation_coefficients([], []).
	violation_coefficients([InequalityValue| InequalityValues], [Coefficient| Coefficients]) :-
		Shifted is InequalityValue + 1.0e-6,
		Active is max(0.0, Shifted),
		Coefficient is 2.0 * Active,
		violation_coefficients(InequalityValues, Coefficients).

:- end_object.
