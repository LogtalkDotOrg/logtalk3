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


:- object(sqp_active_set(_Problem_),
	imports([constrained_optimization_solver, local_optimization_solver(_Problem_)])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Sequential Quadratic Programming local optimizer for problems with general equality and inequality constraints. Requires the problem to define ``gradient/2``; constraint predicates and their Jacobians are optional.',
		parameters is [
			'Problem' - 'Problem object implementing ``constrained_optimization_problem_protocol`` and defining ``gradient/2``.'
		],
		see_also is [
			constrained_optimization_problem_protocol, qp_solver_protocol, qp_active_set,
			local_optimization_solver(_), bfgs(_)
		]
	]).

	:- uses(_Problem_, [
		objective/2, gradient/2, position_bounds/1, stop_condition/3, progress/5, equality_constraints/2,
		equality_jacobian/2, inequality_constraints/2, inequality_jacobian/2
	]).

	:- uses(linear_algebra, [
		add_matrices/3, add_vectors/3, dot_product/3, euclidean_norm/2, identity_matrix/2,
		matrix_vector_product/3, outer_product/3, scale_matrix/3, scale_vector/3, subtract_vectors/3,
		transpose_matrix/2
	]).

	:- uses(list, [
		append/3, length/2, member/2
	]).

	% public entry point

	run(BestPoint, BestValue, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(objective(Objective), Options),
		^^option(target_value(Target), Options),
		^^option(max_iterations(MaxIterations), Options),
		^^option(tol_x(TolX), Options),
		^^option(tol_f(TolF), Options),
		^^option(tol_g(TolG), Options),
		^^option(tol_constraint(TolConstraint), Options),
		^^option(updates(Updates), Options),
		^^option(merit_penalty0(Mu0), Options),
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
		length(Point0, Dimension),
		Dimension >= 1,
		require_gradient,
		objective(Point0, Value0),
		(	number(Value0) ->
			true
		;	domain_error(objective, Value0)
		),
		gradient(Point0, Grad0),
		validate_gradient(Point0, Grad0),
		^^direction_sign(Objective, Sign),
		scale_vector(Grad0, Sign, PhiGrad0),
		constraint_data(Point0, Constraints0),
		identity_matrix(Dimension, B0),
		loop(
			0, MaxIterations, UpdateInterval, Dimension, Bounds, Sign, Target,
			ArmijoC, ArmijoTau, MaxBT, TolX, TolF, TolG, TolConstraint,
			Mu0, B0,
			Point0, Value0, PhiGrad0, Constraints0,
			1, 1, 0,	% Evaluations, GradientEvaluations, QPSolves
			BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, QPSolves,
			FinalStepNorm, FinalViolation
		),
		termination_reason(Iterations, MaxIterations, Objective, Target, BestPoint, BestValue, FinalViolation, TolConstraint, TerminationReason),
		Statistics = [
			iterations(Iterations),
			evaluations(Evaluations),
			gradient_evaluations(GradEvaluations),
			qp_solves(QPSolves),
			final_step_norm(FinalStepNorm),
			final_constraint_violation(FinalViolation),
			termination_reason(TerminationReason),
			final_value(BestValue)
		].

	termination_reason(Iterations, MaxIterations, Objective, Target, Point, Value, Violation, TolConstraint, Reason) :-
		(	Iterations >= MaxIterations ->
			Reason = max_iterations
		;	^^target_reached(Objective, Value, Target), Violation =< TolConstraint ->
			Reason = target_reached
		;	stop_condition(Iterations, Point, Value) ->
			Reason = stop_condition
		;	Reason = converged
		).

	% main loop
	%
	% Constraints is a constraints(EqualityValues, EqualityJacobian, InequalityValues, InequalityJacobian)
	% compound bundling the general (non-box) constraint data at the
	% current point, threaded as a single argument to keep the already
	% long parameter list from growing further; box constraints are
	% handled separately (see box_constraint_rows/6) since they need no
	% such bundling (no Jacobian, no violation bookkeeping).

	loop(
		Iter, MaxIterations, UpdInt, _Dim, _Bounds, _Sign, _Target,
		_ArmC, _ArmTau, _MaxBT, _TolX, _TolF, _TolG, _TolK,
		_Mu, _B,
		Point, Value, _PhiGrad, Constraints,
		Evals, GradEvals, QPSolves,
		Point, Value, Iter, Evals, GradEvals, QPSolves, 0.0, Violation
	) :-
		Iter >= MaxIterations,
		!,
		constraint_violation_inf(Constraints, Violation),
		^^report_final(Iter, UpdInt, Point, Value, Violation).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, Sign, Target,
		_ArmC, _ArmTau, _MaxBT, _TolX, _TolF, _TolG, TolK,
		_Mu, _B,
		Point, Value, _PhiGrad, Constraints,
		Evals, GradEvals, QPSolves,
		Point, Value, Iter, Evals, GradEvals, QPSolves, 0.0, Violation
	) :-
		% use once/1 to workaround an indexing bug in ECLiPSe and GNU Prolog
		once(^^objective_direction(Sign, Objective)),
		^^target_reached(Objective, Value, Target),
		constraint_violation_inf(Constraints, Violation),
		Violation =< TolK,
		!,
		^^report_final(Iter, UpdInt, Point, Value, Violation).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, _Sign, _Target,
		_ArmC, _ArmTau, _MaxBT, _TolX, _TolF, _TolG, _TolK,
		_Mu, _B,
		Point, Value, _PhiGrad, Constraints,
		Evals, GradEvals, QPSolves,
		Point, Value, Iter, Evals, GradEvals, QPSolves, 0.0, Violation
	) :-
		stop_condition(Iter, Point, Value),
		!,
		constraint_violation_inf(Constraints, Violation),
		^^report_final(Iter, UpdInt, Point, Value, Violation).

	loop(
		Iter, MaxIterations, UpdInt, Dim, Bounds, Sign, Target,
		ArmC, ArmTau, MaxBT, TolX, TolF, TolG, TolK,
		Mu0, B0,
		Point0, Value0, PhiGrad0, Constraints0,
		Evals0, GradEvals0, QPSolves0,
		BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, QPSolvesOut,
		FinalStepNorm, FinalViolation
	) :-
		build_qp(Dim, Point0, Bounds, B0, PhiGrad0, Constraints0, H, C, Aeq, Beq, Aineq, Bineq),
		qp_active_set::solve(H, C, Aeq, Beq, Aineq, Bineq, Step, Lambda),
		QPSolves1 is QPSolves0 + 1,
		euclidean_norm(Step, StepNorm),
		constraint_violation_inf(Constraints0, Violation0),
		(	StepNorm =< TolG,
			Violation0 =< TolK ->
			BestPoint = Point0,
			BestValue = Value0,
			Iterations = Iter,
			Evaluations = Evals0,
			GradEvaluations = GradEvals0,
			QPSolvesOut = QPSolves1,
			FinalStepNorm = StepNorm,
			FinalViolation = Violation0,
			^^report_final(Iter, UpdInt, Point0, Value0, StepNorm)
		;	split_lambda(Constraints0, Lambda, EqLambda, GeneralIneqLambda),
			update_penalty(Mu0, EqLambda, GeneralIneqLambda, Mu1),
			constraint_violation_l1(Constraints0, ViolationL1_0),
			dot_product(PhiGrad0, Step, ObjectiveDirectionalDerivative),
			MeritDirectionalDerivative is ObjectiveDirectionalDerivative - Mu1 * ViolationL1_0,
			merit_line_search(
				Point0, Value0, Sign, Step, MeritDirectionalDerivative, Mu1, ViolationL1_0,
				ArmC, ArmTau, MaxBT,
				Point1, Value1, Evals0, Evals1
			),
			gradient(Point1, Grad1),
			validate_gradient(Point1, Grad1),
			GradEvals1 is GradEvals0 + 1,
			scale_vector(Grad1, Sign, PhiGrad1),
			constraint_data(Point1, Constraints1),
			subtract_vectors(Point1, Point0, PointChange),
			euclidean_norm(PointChange, MoveNorm),
			AbsoluteValueChange is abs(Value1 - Value0),
			constraint_violation_inf(Constraints1, Violation1),
			Iter1 is Iter + 1,
			^^report_progress(Iter1, UpdInt, Point1, Value1, StepNorm, Evals1),
			(	(MoveNorm =< TolX ; AbsoluteValueChange =< TolF), Violation1 =< TolK ->
				^^report_final(Iter1, UpdInt, Point1, Value1, StepNorm),
				BestPoint = Point1,
				BestValue = Value1,
				Iterations = Iter1,
				Evaluations = Evals1,
				GradEvaluations = GradEvals1,
				QPSolvesOut = QPSolves1,
				FinalStepNorm = StepNorm,
				FinalViolation = Violation1
			;	lagrangian_gradient(PhiGrad0, Constraints0, EqLambda, GeneralIneqLambda, GradL0),
				lagrangian_gradient(PhiGrad1, Constraints1, EqLambda, GeneralIneqLambda, GradL1),
				subtract_vectors(GradL1, GradL0, LagrangianGradientChange),
				damped_bfgs_update(PointChange, LagrangianGradientChange, B0, B1),
				loop(
					Iter1, MaxIterations, UpdInt, Dim, Bounds, Sign, Target,
					ArmC, ArmTau, MaxBT, TolX, TolF, TolG, TolK,
					Mu1, B1,
					Point1, Value1, PhiGrad1, Constraints1,
					Evals1, GradEvals1, QPSolves1,
					BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, QPSolvesOut,
					FinalStepNorm, FinalViolation
				)
			)
		).

	% QP subproblem construction

	build_qp(Dim, Point, Bounds, B, PhiGrad, constraints(EqualityValues, EqualityJacobian, InequalityValues, InequalityJacobian), H, C, Aeq, Beq, Aineq, Bineq) :-
		H = B,
		C = PhiGrad,
		(	EqualityValues == [] ->
			Aeq = [],
			Beq = []
		;	Aeq = EqualityJacobian,
			scale_vector(EqualityValues, -1.0, Beq)
		),
		(	InequalityValues == [] ->
			GeneralAineq = [],
			GeneralBineq = []
		;	GeneralAineq = InequalityJacobian,
			scale_vector(InequalityValues, -1.0, GeneralBineq)
		),
		box_constraint_rows(Dim, Point, Bounds, BoxAineq, BoxBineq),
		append(GeneralAineq, BoxAineq, Aineq),
		append(GeneralBineq, BoxBineq, Bineq).

	box_constraint_rows(Dim, Point, Bounds, Aineq, Bineq) :-
		(	Bounds == [] ->
			Aineq = [],
			Bineq = []
		;	identity_matrix(Dim, Identity),
			box_constraint_rows_(Point, Bounds, Identity, Aineq, Bineq)
		).

	box_constraint_rows_([], [], [], [], []).
	box_constraint_rows_([Xi| Xs], [Low-High| Bs], [UnitRow| Rows], [LowerRow, UnitRow| Aineq], [LowerB, UpperB| Bineq]) :-
		scale_vector(UnitRow, -1.0, LowerRow),
		LowerB is Xi - Low,
		UpperB is High - Xi,
		box_constraint_rows_(Xs, Bs, Rows, Aineq, Bineq).

	% constraint data, violation, and Lagrangian-gradient predicates

	constraint_data(Point, constraints(EqualityValues, EqualityJacobian, InequalityValues, InequalityJacobian)) :-
		(	equality_constraints(Point, EqualityValues) ->
			(	equality_jacobian(Point, EqualityJacobian) ->
				true
			;	existence_error(procedure, equality_jacobian/2)
			)
		;	EqualityValues = [],
			EqualityJacobian = []
		),
		(	inequality_constraints(Point, InequalityValues) ->
			(	inequality_jacobian(Point, InequalityJacobian) ->
				true
			;	existence_error(procedure, inequality_jacobian/2)
			)
		;	InequalityValues = [],
			InequalityJacobian = []
		),
		^^validate_constraint_data(Point, EqualityValues, EqualityJacobian, equality_constraint_data),
		^^validate_constraint_data(Point, InequalityValues, InequalityJacobian, inequality_constraint_data).

	% multipliers returned by qp_active_set are ordered [equalities |
	% general inequalities | box inequalities]; split off the first two
	% groups (the box group is not needed past this point)

	split_lambda(constraints(EqualityValues, _EqualityJacobian, InequalityValues, _InequalityJacobian), Lambda, EqLambda, GeneralIneqLambda) :-
		length(EqualityValues, Meq),
		length(InequalityValues, Mineq),
		^^split_at(Meq, Lambda, EqLambda, Rest),
		^^split_at(Mineq, Rest, GeneralIneqLambda, _BoxLambda).

	% mu := max(mu, safety * ||lambda||_inf), a monotonically
	% non-decreasing penalty update that is the standard sufficient
	% condition for the QP step to be a descent direction of the L1
	% merit function (Nocedal and Wright, section 18.5)

	update_penalty(Mu0, EqLambda, IneqLambda, Mu1) :-
		^^abs_max(EqLambda, EqMax),
		^^abs_max(IneqLambda, IneqMax),
		Needed is 1.1 * max(EqMax, IneqMax),
		Mu1 is max(Mu0, Needed).

	constraint_violation_l1(constraints(EqualityValues, _EqualityJacobian, InequalityValues, _InequalityJacobian), Violation) :-
		^^abs_sum(EqualityValues, EqViol),
		positive_sum(InequalityValues, 0.0, IneqViol),
		Violation is EqViol + IneqViol.

	constraint_violation_inf(constraints(EqualityValues, _EqualityJacobian, InequalityValues, _InequalityJacobian), Violation) :-
		^^abs_max(EqualityValues, EqViol),
		^^positive_max(InequalityValues, IneqViol),
		Violation is max(EqViol, IneqViol).

	% grad_L(x) = PhiGrad(x) + EqualityJacobian(x)^T . EqLambda + InequalityJacobian(x)^T . IneqLambda
	% (general constraints only: box constraints are exactly linear and
	% contribute nothing to the curvature being modelled here)

	lagrangian_gradient(PhiGrad, constraints(EqualityValues, EqualityJacobian, InequalityValues, InequalityJacobian), EqLambda, IneqLambda, GradL) :-
		(	EqualityValues == [] ->
			GradL1 = PhiGrad
		;	transpose_matrix(EqualityJacobian, EqualityJacobianTranspose),
			matrix_vector_product(EqualityJacobianTranspose, EqLambda, EqTerm),
			add_vectors(PhiGrad, EqTerm, GradL1)
		),
		(	InequalityValues == [] ->
			GradL = GradL1
		;	transpose_matrix(InequalityJacobian, InequalityJacobianT),
			matrix_vector_product(InequalityJacobianT, IneqLambda, IneqTerm),
			add_vectors(GradL1, IneqTerm, GradL)
		).

	% L1/L-infinity reductions over a (possibly empty) numeric list

	positive_sum([], Sum, Sum).
	positive_sum([V| Vs], Sum0, Sum) :-
		Sum1 is Sum0 + max(0.0, V),
		positive_sum(Vs, Sum1, Sum).

	% merit-function backtracking line search
	%
	% As in bfgs(_)'s Armijo search, everything is expressed in
	% phi-space (Sign * original objective) so a single sufficient-
	% decrease test covers both minimize and maximize. Unlike bfgs(_),
	% the trial point is not additionally projected to bounds: the box
	% rows already folded into the QP guarantee Point0 + t*Step stays
	% within bounds for every t in [0, 1], since the feasible region
	% for the QP step is convex and both Step and the zero step satisfy
	% those rows exactly.

	merit_line_search(
		Point0, Value0, Sign, Step, MeritDirectionalDerivative, Mu, Violation0,
		ArmC, ArmTau, MaxBT,
		Point1, Value1, Evals0, Evals1
	) :-
		MeritDirectionalDerivative < -1.0e-12,
		merit_backtrack(
			0, MaxBT, Point0, Value0, Sign, Step, Mu, Violation0,
			1.0, ArmC, ArmTau, MeritDirectionalDerivative,
			Point1, Value1, Evals0, Evals1
		).

	merit_backtrack(
		BT, MaxBT, Point0, Value0, Sign, Step, Mu, Violation0,
		StepSize, ArmC, ArmTau, MeritDirectionalDerivative,
		Point1, Value1, Evals0, Evals1
	) :-
		scale_vector(Step, StepSize, Scaled),
		add_vectors(Point0, Scaled, Trial),
		objective(Trial, TrialVal),
		(	number(TrialVal) ->
			true
		;	domain_error(objective, TrialVal)
		),
		Evals1_try is Evals0 + 1,
		trial_constraint_violation_l1(Trial, TrialViolationL1),
		PhiValue0 is Sign * Value0,
		PhiTrial is Sign * TrialVal,
		Merit0 is PhiValue0 + Mu * Violation0,
		MeritTrial is PhiTrial + Mu * TrialViolationL1,
		Sufficient is Merit0 + ArmC * StepSize * MeritDirectionalDerivative,
		(	MeritTrial =< Sufficient ->
			Point1 = Trial,
			Value1 = TrialVal,
			Evals1 = Evals1_try
		;	BT1 is BT + 1,
			(	BT1 >= MaxBT ->
				fail
			;	StepSize1 is StepSize * ArmTau,
				merit_backtrack(
					BT1, MaxBT, Point0, Value0, Sign, Step, Mu, Violation0,
					StepSize1, ArmC, ArmTau, MeritDirectionalDerivative,
					Point1, Value1, Evals1_try, Evals1
				)
			)
		).

	% re-derives just the L1 violation at a trial point, without the
	% Jacobians the full constraint_data/2 also computes, since the
	% line search only needs the merit-function value, not a new QP

	trial_constraint_violation_l1(Point, Violation) :-
		(	equality_constraints(Point, EqualityValues) ->
			^^abs_sum(EqualityValues, EqViol)
		;	EqViol = 0.0
		),
		(	inequality_constraints(Point, InequalityValues) ->
			positive_sum(InequalityValues, 0.0, IneqViol)
		;	IneqViol = 0.0
		),
		Violation is EqViol + IneqViol.

	% damped BFGS update of the Hessian-of-the-Lagrangian approximation
	% (Powell, 1978; Nocedal and Wright, "Numerical Optimization",
	% procedure 18.2)

	damped_bfgs_update(PointChange, LagrangianGradientChange, B0, B1) :-
		matrix_vector_product(B0, PointChange, Bs),
		dot_product(PointChange, Bs, SBs),
		(	SBs =< 1.0e-12 ->
			% PointChange carries (numerically) no information in a direction B0
			% currently models as curved; skip the update rather than
			% divide by a near-zero denominator
			B1 = B0
		;	dot_product(PointChange, LagrangianGradientChange, SY),
			(	SY >= 0.2 * SBs ->
				Ydamped = LagrangianGradientChange
			;	Theta is (0.8 * SBs) / (SBs - SY),
				OneMinusTheta is 1.0 - Theta,
				scale_vector(LagrangianGradientChange, Theta, ScaledGradientChange),
				scale_vector(Bs, OneMinusTheta, ScaledHessianStep),
				add_vectors(ScaledGradientChange, ScaledHessianStep, Ydamped)
			),
			dot_product(PointChange, Ydamped, SYdamped),
			(	SYdamped =< 1.0e-12 ->
				B1 = B0
			;	outer_product(Bs, Bs, BssBt),
				NegInvSBs is -1.0 / SBs,
				scale_matrix(BssBt, NegInvSBs, Term1),
				outer_product(Ydamped, Ydamped, YYt),
				InvSYdamped is 1.0 / SYdamped,
				scale_matrix(YYt, InvSYdamped, Term2),
				add_matrices(B0, Term1, Tmp),
				add_matrices(Tmp, Term2, B1)
			)
		).

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

	progress_hook(Iteration, Point, Value, Measure, Evaluations) :-
		ignore(progress(Iteration, Point, Value, Measure, Evaluations)).

	% options specific to sqp_active_set

	default_option(merit_penalty0(10.0)).
	default_option(armijo_c(1.0e-4)).
	default_option(armijo_tau(0.5)).
	default_option(armijo_max_backtracks(20)).
	default_option(tol_constraint(1.0e-6)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(merit_penalty0(Mu)) :-
		number(Mu), Mu > 0.0.
	valid_option(armijo_c(C)) :-
		number(C), C > 0.0, C < 1.0.
	valid_option(armijo_tau(Tau)) :-
		number(Tau), Tau > 0.0, Tau < 1.0.
	valid_option(armijo_max_backtracks(N)) :-
		integer(N), N >= 1.
	valid_option(tol_constraint(T)) :-
		number(T), T >= 0.0.
	valid_option(Option) :-
		^^valid_option(Option).

:- end_object.
