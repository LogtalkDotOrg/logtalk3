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


:- object(trust_region_newton_cg(_Problem_),
	imports(local_optimization_solver(_Problem_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-24,
		comment is 'Trust-region Newton-CG local optimizer (Steihaug-CG for the subproblem). Requires the problem to define ``gradient/2`` and ``hessian/2``. Supports optional box constraints via projection, minimization and maximization.',
		parameters is [
			'Problem' - 'Problem object implementing ``local_optimization_problem_protocol`` and defining ``gradient/2`` and ``hessian/2``.'
		],
		remarks is [
			'Subproblem' - 'At each outer iteration, the step is obtained by approximately minimizing the local quadratic model within a ball of radius ``trust_radius``, using the Steihaug-CG method (Nocedal and Wright, Algorithm 7.2): plain conjugate gradient on the model, terminated early either by a negative-curvature direction or by reaching the trust-region boundary, in which case the step is extended to the boundary along the current CG direction.',
			'No line search' - 'Unlike the other gradient-based solvers in this library, this solver never backtracks a step size; the trust-region radius itself is grown or shrunk each iteration based on how well the quadratic model predicted the actual objective change, and a step is accepted only when that agreement is good enough.',
			'Internal minimization form' - 'Maximization is handled by internally minimizing the negated objective, gradient, and Hessian, so the subproblem and acceptance test are always expressed in minimization form, which avoids sign errors.',
			'Convergence' - 'Because it uses exact second-order information, this solver typically converges in far fewer iterations than ``gradient_descent(_)``, ``bfgs(_)``, or ``lbfgs(_)`` on well-behaved problems, at the cost of requiring an explicit ``hessian/2``.',
			'Bounds' - 'When the problem defines ``position_bounds/1``, trial points are projected onto the box after each step. Projection can weaken the trust-region model agreement (the accepted step may differ from the one the subproblem solved for), which can trigger more radius shrinkage than an unconstrained problem would; a pure bound-constrained formulation is not implemented.'
		],
		see_also is [
			local_optimization_problem_protocol, local_optimization_solver(_), gradient_descent(_), conjugate_gradient(_), bfgs(_), lbfgs(_)
		]
	]).

	:- uses(_Problem_, [
		objective/2, gradient/2, hessian/2, position_bounds/1, stop_condition/3, progress/5
	]).

	:- uses(linear_algebra, [
		add_vectors/3, dot_product/3, euclidean_norm/2, matrix_vector_product/3,
		scale_matrix/3, scale_vector/3, subtract_vectors/3
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
		^^option(trust_radius_initial(Radius0), Options),
		^^option(trust_radius_max(RadiusMax), Options),
		^^option(eta(Eta), Options),
		^^option(cg_tol(CgTol), Options),
		^^option(cg_max_iterations(CgMaxIterOpt), Options),
		(	Radius0 =< RadiusMax ->
			true
		;	domain_error(option, trust_radius_initial(Radius0))
		),
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
		(	CgMaxIterOpt == dimension ->
			CgMaxIterations = Dimension
		;	CgMaxIterations = CgMaxIterOpt
		),
		require_gradient,
		require_hessian,
		objective(Point0, Value0),
		(	number(Value0) -> true ; domain_error(objective, Value0) ),
		gradient(Point0, Grad0),
		validate_gradient(Point0, Grad0),
		hessian(Point0, Hessian0),
		validate_hessian(Point0, Hessian0),
		direction_sign(ObjDir, Sign),
		scale_vector(Grad0, Sign, PhiGrad0),
		scale_matrix(Hessian0, Sign, PhiHessian0),
		euclidean_norm(PhiGrad0, GradNorm0),
		loop(
			0, MaxIterations, UpdateInterval, Dimension, Bounds, Sign, Target,
			Radius0, RadiusMax, Eta, CgTol, CgMaxIterations,
			TolX, TolF, TolG,
			Point0, Value0, PhiGrad0, PhiHessian0, GradNorm0,
			1, 1, 1,	% Evaluations, GradientEvaluations, HessianEvaluations
			BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, HessianEvaluations, FinalGradNorm
		),
		Statistics = [
			iterations(Iterations),
			evaluations(Evaluations),
			gradient_evaluations(GradEvaluations),
			hessian_evaluations(HessianEvaluations),
			final_gradient_norm(FinalGradNorm),
			final_value(BestValue)
		].

	% main loop
	%
	% as in bfgs(_) and lbfgs(_), the objective direction is folded
	% into a single "Sign" factor (1.0 for minimize, -1.0 for
	% maximize): PhiGrad = Sign * Grad and PhiHessian = Sign * Hessian
	% are always the gradient and Hessian of the function actually
	% being *minimized*, so the subproblem and acceptance test below
	% never need to branch on ObjDir.

	loop(
		Iter, MaxIterations, UpdInt, _Dim, _Bounds, _Sign, _Target,
		_Radius, _RadiusMax, _Eta, _CgTol, _CgMaxIter, _TolX, _TolF, _TolG,
		Point, Value, _PhiGrad, _PhiHessian, GradNorm,
		Evals, GradEvals, HessEvals,
		Point, Value, Iter, Evals, GradEvals, HessEvals, GradNorm
	) :-
		Iter >= MaxIterations,
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, Sign, Target,
		_Radius, _RadiusMax, _Eta, _CgTol, _CgMaxIter, _TolX, _TolF, _TolG,
		Point, Value, _PhiGrad, _PhiHessian, GradNorm,
		Evals, GradEvals, HessEvals,
		Point, Value, Iter, Evals, GradEvals, HessEvals, GradNorm
	) :-
		objective_direction(Sign, ObjDir),
		^^target_reached(ObjDir, Value, Target),
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, _Sign, _Target,
		_Radius, _RadiusMax, _Eta, _CgTol, _CgMaxIter, _TolX, _TolF, _TolG,
		Point, Value, _PhiGrad, _PhiHessian, GradNorm,
		Evals, GradEvals, HessEvals,
		Point, Value, Iter, Evals, GradEvals, HessEvals, GradNorm
	) :-
		stop_condition(Iter, Point, Value),
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, _Sign, _Target,
		_Radius, _RadiusMax, _Eta, _CgTol, _CgMaxIter, _TolX, _TolF, TolG,
		Point, Value, _PhiGrad, _PhiHessian, GradNorm,
		Evals, GradEvals, HessEvals,
		Point, Value, Iter, Evals, GradEvals, HessEvals, GradNorm
	) :-
		GradNorm =< TolG,
		!,
		^^report_final(Iter, UpdInt, Point, Value, GradNorm).

	loop(
		Iter, MaxIterations, UpdInt, Dim, Bounds, Sign, Target,
		Radius0, RadiusMax, Eta, CgTol, CgMaxIterations,
		TolX, TolF, TolG,
		Point0, Value0, PhiGrad0, PhiHessian0, GradNorm0,
		Evals0, GradEvals0, HessEvals0,
		BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, HessianEvaluations, FinalGradNorm
	) :-
		steihaug_cg(PhiHessian0, PhiGrad0, Radius0, CgTol, CgMaxIterations, P),
		add_vectors(Point0, P, Trial0),
		^^project_to_bounds(Trial0, Bounds, Trial),
		subtract_vectors(Trial, Point0, ActualP),
		objective(Trial, TrialVal),
		(	number(TrialVal) -> true ; domain_error(objective, TrialVal) ),
		Evals1 is Evals0 + 1,
		PhiValue0 is Sign * Value0,
		PhiTrialVal is Sign * TrialVal,
		matrix_vector_product(PhiHessian0, ActualP, HActualP),
		dot_product(PhiGrad0, ActualP, GradDotP),
		dot_product(ActualP, HActualP, PDotHP),
		ModelChange is GradDotP + 0.5 * PDotHP,
		PredictedReduction is -ModelChange,
		ActualReduction is PhiValue0 - PhiTrialVal,
		euclidean_norm(ActualP, ActualPNorm),
		(	PredictedReduction > 1.0e-12 ->
			Rho is ActualReduction / PredictedReduction
		;	Rho = -1.0
		),
		% trust-region radius update (Nocedal & Wright, Algorithm 4.1)
		(	Rho < 0.25 ->
			Radius1 is 0.25 * ActualPNorm
		;	(	Rho > 0.75,
				ActualPNorm >= Radius0 - 1.0e-10
			->	Radius2 is 2.0 * Radius0,
				Radius1 is min(Radius2, RadiusMax)
			;	Radius1 = Radius0
			)
		),
		Iter1 is Iter + 1,
		(	Rho > Eta ->
			% step accepted: move, and refresh gradient/Hessian
			gradient(Trial, Grad1),
			validate_gradient(Trial, Grad1),
			hessian(Trial, Hessian1),
			validate_hessian(Trial, Hessian1),
			GradEvals1 is GradEvals0 + 1,
			HessEvals1 is HessEvals0 + 1,
			scale_vector(Grad1, Sign, PhiGrad1),
			scale_matrix(Hessian1, Sign, PhiHessian1),
			euclidean_norm(PhiGrad1, GradNorm1),
			AbsDf is abs(TrialVal - Value0),
			^^report_progress(Iter1, UpdInt, Trial, TrialVal, GradNorm1, Evals1),
			(	(ActualPNorm =< TolX ; AbsDf =< TolF) ->
				^^report_final(Iter1, UpdInt, Trial, TrialVal, GradNorm1),
				BestPoint = Trial,
				BestValue = TrialVal,
				Iterations = Iter1,
				Evaluations = Evals1,
				GradEvaluations = GradEvals1,
				HessianEvaluations = HessEvals1,
				FinalGradNorm = GradNorm1
			;	loop(
					Iter1, MaxIterations, UpdInt, Dim, Bounds, Sign, Target,
					Radius1, RadiusMax, Eta, CgTol, CgMaxIterations,
					TolX, TolF, TolG,
					Trial, TrialVal, PhiGrad1, PhiHessian1, GradNorm1,
					Evals1, GradEvals1, HessEvals1,
					BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, HessianEvaluations, FinalGradNorm
				)
			)
		;	% step rejected: stay at Point0, only the radius shrinks;
			% this still counts as a completed outer iteration
			^^report_progress(Iter1, UpdInt, Point0, Value0, GradNorm0, Evals1),
			(	Radius1 =< TolX ->
				% the trust region has collapsed below the step-size
				% tolerance: no further progress is achievable here
				^^report_final(Iter1, UpdInt, Point0, Value0, GradNorm0),
				BestPoint = Point0,
				BestValue = Value0,
				Iterations = Iter1,
				Evaluations = Evals1,
				GradEvaluations = GradEvals0,
				HessianEvaluations = HessEvals0,
				FinalGradNorm = GradNorm0
			;	loop(
					Iter1, MaxIterations, UpdInt, Dim, Bounds, Sign, Target,
					Radius1, RadiusMax, Eta, CgTol, CgMaxIterations,
					TolX, TolF, TolG,
					Point0, Value0, PhiGrad0, PhiHessian0, GradNorm0,
					Evals1, GradEvals0, HessEvals0,
					BestPoint, BestValue, Iterations, Evaluations, GradEvaluations, HessianEvaluations, FinalGradNorm
				)
			)
		).

	% objective-direction / phi-space predicates

	direction_sign(minimize, 1.0).
	direction_sign(maximize, -1.0).

	objective_direction(1.0, minimize).
	objective_direction(-1.0, maximize).

	% Steihaug-CG (Nocedal & Wright, Algorithm 7.2): approximately
	% solves min_p  g.p + 0.5 p.H.p  subject to  ||p|| =< Radius,
	% starting from p = 0. Terminates early, extending the step to the
	% trust-region boundary, either on a direction of non-positive
	% curvature or once the iterate would leave the ball; otherwise it
	% is plain linear CG on the model, so an unconstrained Newton step
	% is recovered whenever it lies inside the trust region.

	steihaug_cg(Hessian, Grad, Radius, CgTol, MaxIter, P) :-
		euclidean_norm(Grad, Grad0Norm),
		(	Grad0Norm =< 1.0e-14 ->
			% already stationary: no step needed
			scale_vector(Grad, 0.0, P)
		;	scale_vector(Grad, 0.0, Z0),
			scale_vector(Grad, -1.0, D0),
			steihaug_iterate(0, MaxIter, Hessian, Z0, Grad, D0, Radius, CgTol, Grad0Norm, P)
		).

	steihaug_iterate(J, MaxIter, _Hessian, Z, _R, _D, _Radius, _CgTol, _R0Norm, Z) :-
		J >= MaxIter,
		!.
	steihaug_iterate(J, MaxIter, Hessian, Z, R, D, Radius, CgTol, R0Norm, P) :-
		matrix_vector_product(Hessian, D, HD),
		dot_product(D, HD, DHD),
		(	DHD =< 0.0 ->
			% non-positive curvature along D: move to the boundary
			boundary_step(Z, D, Radius, P)
		;	dot_product(R, R, RR),
			Alpha is RR / DHD,
			scale_vector(D, Alpha, AlphaD),
			add_vectors(Z, AlphaD, Z1),
			euclidean_norm(Z1, Z1Norm),
			(	Z1Norm >= Radius ->
				boundary_step(Z, D, Radius, P)
			;	scale_vector(HD, Alpha, AlphaHD),
				add_vectors(R, AlphaHD, R1),
				euclidean_norm(R1, R1Norm),
				(	R1Norm =< CgTol * R0Norm ->
					P = Z1
				;	dot_product(R1, R1, R1R1),
					Beta is R1R1 / RR,
					scale_vector(D, Beta, BetaD),
					scale_vector(R1, -1.0, NegR1),
					add_vectors(NegR1, BetaD, D1),
					J1 is J + 1,
					steihaug_iterate(J1, MaxIter, Hessian, Z1, R1, D1, Radius, CgTol, R0Norm, P)
				)
			)
		).

	% extends z along d to the trust-region boundary: finds the
	% non-negative root tau of ||z + tau*d|| = Radius

	boundary_step(Z, D, Radius, P) :-
		dot_product(D, D, DD),
		(	DD =< 0.0 ->
			P = Z
		;	dot_product(Z, D, ZD),
			dot_product(Z, Z, ZZ),
			Discriminant0 is ZD * ZD - DD * (ZZ - Radius * Radius),
			(	Discriminant0 < 0.0 ->
				Discriminant = 0.0	% numerical guard: should not happen given ||Z|| =< Radius
			;	Discriminant = Discriminant0
			),
			SqrtDiscriminant is sqrt(Discriminant),
			Tau is (-ZD + SqrtDiscriminant) / DD,
			scale_vector(D, Tau, TauD),
			add_vectors(Z, TauD, P)
		).

	% auxiliary predicates

	require_gradient :-
		(	_Problem_::predicate_property(gradient(_, _), defined_in(_)) ->
			true
		;	existence_error(procedure, gradient/2)
		).

	require_hessian :-
		(	_Problem_::predicate_property(hessian(_, _), defined_in(_)) ->
			true
		;	existence_error(procedure, hessian/2)
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

	validate_hessian(Point, Hessian) :-
		length(Point, D),
		length(Hessian, D),
		forall(member(Row, Hessian), length(Row, D)),
		!,
		(	forall(member(Row, Hessian), forall(member(X, Row), number(X))) ->
			true
		;	domain_error(hessian, Hessian)
		).
	validate_hessian(_Point, Hessian) :-
		domain_error(hessian, Hessian).

	% progress hook -> problem progress/5
	progress_hook(Iteration, BestPoint, BestValue, Measure, Evaluations) :-
		ignore(progress(Iteration, BestPoint, BestValue, Measure, Evaluations)).

	% options specific to trust-region Newton-CG

	default_option(trust_radius_initial(1.0)).
	default_option(trust_radius_max(100.0)).
	default_option(eta(0.15)).
	default_option(cg_tol(0.1)).
	default_option(cg_max_iterations(dimension)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(trust_radius_initial(Radius)) :-
		number(Radius), Radius > 0.0.
	valid_option(trust_radius_max(Radius)) :-
		number(Radius), Radius > 0.0.
	valid_option(eta(Eta)) :-
		number(Eta), Eta >= 0.0, Eta < 0.25.
	valid_option(cg_tol(CgTol)) :-
		number(CgTol), CgTol > 0.0, CgTol < 1.0.
	valid_option(cg_max_iterations(MaxIterations)) :-
		(	MaxIterations == dimension ->
			true
		;	integer(MaxIterations),
			MaxIterations >= 1
		).
	valid_option(Option) :-
		^^valid_option(Option).

:- end_object.
