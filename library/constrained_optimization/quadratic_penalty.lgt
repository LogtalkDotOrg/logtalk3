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


:- object(quadratic_penalty(_Problem_, _InnerSolver_),
	imports([constrained_optimization_solver, local_optimization_solver(_Problem_)])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Quadratic penalty method for problems with general equality and inequality constraints, delegating each penalized subproblem to an existing local optimization solver.',
		parameters is [
			'Problem' - 'Problem object implementing ``constrained_optimization_problem_protocol``.',
			'InnerSolver' - 'Atom naming a local_optimization solver class taking a single problem parameter, e.g. ``bfgs`` or ``lbfgs``. Constructed dynamically each outer iteration as ``InnerSolver(SubProblem)``.'
		],
		see_also is [
			constrained_optimization_problem_protocol, augmented_lagrangian(_, _), sqp_active_set(_),
			local_optimization_solver(_), bfgs(_), lbfgs(_)
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
		constraint_violation(Point0, Violation0),
		outer_loop(
			0, MaxOuter, OuterUpdates, OuterTol, RhoScale, ViolDecrease, Sign, Target, InnerOptions,
			Point0, Rho0, Violation0,
			0, 0, 0,
			BestPoint, BestValue, OuterIterations, InnerIterations, Evaluations, GradEvaluations, FinalViolation
		),
		outer_termination_reason(OuterIterations, MaxOuter, ObjDir, Target, BestValue, FinalViolation, OuterTol, TerminationReason),
		Statistics = [
			outer_iterations(OuterIterations),
			inner_iterations(InnerIterations),
			evaluations(Evaluations),
			gradient_evaluations(GradEvaluations),
			final_violation(FinalViolation),
			termination_reason(TerminationReason),
			final_value(BestValue)
		].

	outer_termination_reason(Iterations, MaxIterations, ObjDir, Target, Value, Violation, Tolerance, Reason) :-
		(	Iterations >= MaxIterations, Violation > Tolerance ->
			Reason = max_iterations
		;	^^target_reached(ObjDir, Value, Target), Violation =< Tolerance ->
			Reason = target_reached
		;	Reason = converged
		).

	% outer loop
	%
	% As in augmented_lagrangian(_,_), every call except the
	% MaxOuter-reached base case performs exactly one inner solve before
	% deciding whether to stop, judged from the violation (and, if a
	% target was requested, the objective value) *after* that solve -
	% never from the violation the outer iteration started from, which
	% would wrongly treat an already-feasible-but-not-yet-optimal
	% starting point as already solved (see augmented_lagrangian.lgt's
	% NOTES.md entry for how that bug first showed up).

	outer_loop(
		Iter, MaxOuter, _OuterUpdates, _OuterTol, _RhoScale, _ViolDecrease, _Sign, _Target, _InnerOptions,
		Point, _Rho, Violation,
		InnerIters, Evals, GradEvals,
		Point, Value, Iter, InnerIters, Evals, GradEvals, Violation
	) :-
		Iter >= MaxOuter,
		!,
		objective(Point, Value).

	outer_loop(
		Iter, MaxOuter, OuterUpdates, OuterTol, RhoScale, ViolDecrease, Sign, Target, InnerOptions,
		Point0, Rho0, Violation0,
		InnerIters0, Evals0, GradEvals0,
		BestPoint, BestValue, OuterIterations, InnerIterations, Evaluations, GradEvaluations, FinalViolation
	) :-
		Iter < MaxOuter,
		OuterIteration is Iter + 1,
		^^build_inner(_InnerSolver_, penalty_sub_problem(_Problem_, OuterIteration, Point0, Rho0, Sign), InnerObj),
		InnerObj::run(Point1, _SubValue1, InnerStats, InnerOptions),
		memberchk(iterations(InnerIter), InnerStats),
		memberchk(evaluations(InnerEval), InnerStats),
		memberchk(gradient_evaluations(InnerGradEval), InnerStats),
		InnerIters1 is InnerIters0 + InnerIter,
		Evals1 is Evals0 + InnerEval,
		GradEvals1 is GradEvals0 + InnerGradEval,
		constraint_violation(Point1, Violation1),
		^^update_penalty(Violation0, Violation1, ViolDecrease, Rho0, RhoScale, Rho1),
		objective(Point1, Value1),
		Iter1 is Iter + 1,
		report_outer(Iter1, OuterUpdates, Point1, Value1, Violation1, Evals1),
		^^objective_direction(Sign, ObjDir),
		(	(Violation1 =< OuterTol ; (^^target_reached(ObjDir, Value1, Target), Violation1 =< OuterTol)) ->
			BestPoint = Point1,
			BestValue = Value1,
			OuterIterations = Iter1,
			InnerIterations = InnerIters1,
			Evaluations = Evals1,
			GradEvaluations = GradEvals1,
			FinalViolation = Violation1
		;	outer_loop(
				Iter1, MaxOuter, OuterUpdates, OuterTol, RhoScale, ViolDecrease, Sign, Target, InnerOptions,
				Point1, Rho1, Violation1,
				InnerIters1, Evals1, GradEvals1,
				BestPoint, BestValue, OuterIterations, InnerIterations, Evaluations, GradEvaluations, FinalViolation
			)
		).

	report_outer(Iter, Updates, Point, Value, Violation, Evals) :-
		( Updates > 0, Iter > 0, Iter mod Updates =:= 0 ->
			ignore(progress(Iter, Point, Value, Violation, Evals))
		; true
		).

	% constraint violation (on the ORIGINAL problem) and penalty growth

	constraint_violation(Point, Violation) :-
		(	equality_constraints(Point, EqualityValues) ->
			^^validate_numeric_vector(EqualityValues, equality_constraint_data),
			^^abs_max(EqualityValues, EqViol)
		;	EqViol = 0.0
		),
		(	inequality_constraints(Point, InequalityValues) ->
			^^validate_numeric_vector(InequalityValues, inequality_constraint_data),
			^^positive_max(InequalityValues, IneqViol)
		;	IneqViol = 0.0
		),
		Violation is max(EqViol, IneqViol).

	% options specific to quadratic_penalty (same names/defaults as
	% augmented_lagrangian(_,_) for consistency across this library)

	default_option(rho0(1.0)).
	default_option(rho_scale(10.0)).
	default_option(outer_violation_decrease(0.25)).
	default_option(max_outer_iterations(30)).
	default_option(outer_tolerance(1.0e-6)).
	default_option(inner_updates(0)).
	default_option(Option) :-
		^^default_option(Option).

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


:- object(penalty_sub_problem(_Problem_, _OuterIteration_, _StartPoint_, _Rho_, _Sign_),
	implements(local_optimization_problem_protocol),
	imports(constrained_optimization_solver)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Internal quadratic-penalty subproblem: objective(x) = Sign*f(x) + (rho/2)*(||g(x)||^2 + ||max(0,h(x))||^2). Instantiated fresh by quadratic_penalty(_,_) every outer iteration, starting from StartPoint (the previous outer iterate, so each inner solve warm-starts from where the outer loop left off); not meant to be used directly.',
		parameters is [
			'Problem' - 'The original constrained problem.',
			'OuterIteration' - 'One-based outer iteration number.',
			'StartPoint' - 'Point the inner solve should start from (the current outer iterate).',
			'Rho' - 'Current penalty parameter.',
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

	% initial_point/1, position_bounds/1, and stop_condition/3 are not
	% redefined here, only forwarded, so position_bounds/1 and
	% stop_condition/3 stay in the uses(_Problem_, ...) directive above
	% and are called by their bare name below; objective/2 and
	% gradient/2 must NOT appear in that directive since they genuinely
	% are redefined here (Logtalk does not allow a predicate to be both
	% declared via uses/2 and locally defined - it is a compile-time
	% error, not a runtime ambiguity), so they call _Problem_::
	% explicitly instead.

	initial_point(_StartPoint_).

	objective(Point, Value) :-
		_Problem_::objective(Point, Value0),
		penalty_value(Point, PenaltyValue),
		Value is _Sign_ * Value0 + PenaltyValue.

	gradient(Point, Gradient) :-
		_Problem_::gradient(Point, Gradient0),
		scale_vector(Gradient0, _Sign_, ScaledGradient0),
		penalty_gradient(Point, PenaltyGradient),
		add_vectors(ScaledGradient0, PenaltyGradient, Gradient).

	progress(Iteration, Point, Value, Measure, Evaluations) :-
		ignore(_Problem_::inner_progress(outer(_OuterIteration_), Iteration, Point, Value, Measure, Evaluations)).

	% penalty value: (rho/2)*sum(g_i(x)^2) + (rho/2)*sum(max(0,h_j(x))^2)

	penalty_value(Point, PenaltyValue) :-
		(	equality_constraints(Point, EqualityValues) ->
			^^validate_numeric_vector(EqualityValues, equality_constraint_data),
			dot_product(EqualityValues, EqualityValues, SquaredViolation),
			EqualityPenaltyValue is 0.5 * _Rho_ * SquaredViolation
		; EqualityPenaltyValue = 0.0
		),
		(	inequality_constraints(Point, InequalityValues) ->
			^^validate_numeric_vector(InequalityValues, inequality_constraint_data),
			positive_square_sum(InequalityValues, SquaredInequalityViolation),
			InequalityPenaltyValue is 0.5 * _Rho_ * SquaredInequalityViolation
		; InequalityPenaltyValue = 0.0
		),
		PenaltyValue is EqualityPenaltyValue + InequalityPenaltyValue.

	positive_square_sum(Values, Sum) :-
		positive_square_sum(Values, 0.0, Sum).

	positive_square_sum([], Sum, Sum).
	positive_square_sum([V| Vs], Sum0, Sum) :-
		Active is max(0.0, V),
		Sum1 is Sum0 + Active * Active,
		positive_square_sum(Vs, Sum1, Sum).

	% penalty gradient: rho * J_g(x)^T . g(x) for equalities,
	% rho * J_h(x)^T . max(0,h(x)) for inequalities

	penalty_gradient(Point, PenaltyGradient) :-
		length(Point, N),
		(	equality_constraints(Point, EqualityValues) ->
			(	equality_jacobian(Point, EqualityJacobian) ->
				true
			;	existence_error(procedure, equality_jacobian/2)
			),
			^^validate_constraint_data(Point, EqualityValues, EqualityJacobian, equality_constraint_data),
			scale_vector(EqualityValues, _Rho_, EqualityPenaltyCoefficients),
			transpose_matrix(EqualityJacobian, EqualityJacobianTranspose),
			matrix_vector_product(EqualityJacobianTranspose, EqualityPenaltyCoefficients, EqualityPenaltyGradient)
		;	new_vector(N, 0.0, EqualityPenaltyGradient)
		),
		(	inequality_constraints(Point, InequalityValues) ->
			(	inequality_jacobian(Point, InequalityJacobian) ->
				true
			;	existence_error(procedure, inequality_jacobian/2)
			),
			^^validate_constraint_data(Point, InequalityValues, InequalityJacobian, inequality_constraint_data),
			positive_coefficients(InequalityValues, _Rho_, InequalityPenaltyCoefficients),
			transpose_matrix(InequalityJacobian, InequalityJacobianTranspose),
			matrix_vector_product(InequalityJacobianTranspose, InequalityPenaltyCoefficients, InequalityPenaltyGradient)
		;	new_vector(N, 0.0, InequalityPenaltyGradient)
		),
		add_vectors(EqualityPenaltyGradient, InequalityPenaltyGradient, PenaltyGradient).

	positive_coefficients([], _Rho, []).
	positive_coefficients([V| Vs], Rho, [Coefficient| Coefficients]) :-
		Coefficient is Rho * max(0.0, V),
		positive_coefficients(Vs, Rho, Coefficients).

:- end_object.
