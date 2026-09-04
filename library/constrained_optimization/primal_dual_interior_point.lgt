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


:- object(primal_dual_interior_point(_Problem_),
	imports([constrained_optimization_solver, local_optimization_solver(_Problem_)])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Infeasible-start primal-dual interior-point method with Mehrotra predictor-corrector for general equality, inequality, and box constraints.',
		parameters is [
			'Problem' - 'Problem object implementing ``constrained_optimization_problem_protocol`` and defining ``gradient/2``.'
		],
		see_also is [
			constrained_optimization_problem_protocol, sqp_active_set(_), log_barrier(_, _),
			local_optimization_solver(_), linear_algebra
		]
	]).

	:- uses(_Problem_, [
		objective/2, gradient/2, position_bounds/1, stop_condition/3, progress/5, equality_constraints/2,
		equality_jacobian/2, inequality_constraints/2, inequality_jacobian/2
	]).

	:- uses(linear_algebra, [
		add_matrices/3, add_vectors/3, dot_product/3, euclidean_norm/2, identity_matrix/2,
		matrix_matrix_product/3, matrix_vector_product/3, new_matrix/4, outer_product/3, scale_matrix/3,
		scale_vector/3, solve_linear_system/3, subtract_vectors/3, transpose_matrix/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, take/4
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
		^^option(tol_constraint(TolK), Options),
		^^option(updates(Updates), Options),
		^^option(merit_penalty0(MeritPenalty0), Options),
		^^option(armijo_c(ArmijoC), Options),
		^^option(armijo_tau(ArmijoTau), Options),
		^^option(armijo_max_backtracks(MaxBT), Options),
		(	Updates > 0 ->
			UpdateInterval is max(1, (MaxIterations - 1) // Updates)
		;	UpdateInterval = 0
		),
		^^initial_point(Options, Point0),
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
		equality_data(Point0, EqualityValues0, EqualityJacobian0),
		inequality_data(Point0, InequalityValues0, InequalityJacobian0),
		init_slacks(InequalityValues0, S0),
		init_multipliers_z(InequalityValues0, Z0),
		init_multipliers_y(EqualityValues0, Y0),
		identity_matrix(Dimension, B0),
		loop(
			0, MaxIterations, UpdateInterval, Sign, Target,
			ArmijoC, ArmijoTau, MaxBT, TolX, TolF, TolG, TolK,
			MeritPenalty0, B0,
			Point0, Value0, PhiGrad0, EqualityValues0, EqualityJacobian0, InequalityValues0, InequalityJacobian0, S0, Y0, Z0,
			1, 1,
			BestPoint, BestValue, Iterations, Evaluations, GradEvaluations,
			FinalStatRes, FinalPrimalInf, FinalCompGap
		),
		termination_reason(
			Iterations, MaxIterations, Objective, Target, BestPoint, BestValue,
			FinalStatRes, FinalPrimalInf, FinalCompGap, TolG, TolK, TerminationReason
		),
		Statistics = [
			iterations(Iterations),
			evaluations(Evaluations),
			gradient_evaluations(GradEvaluations),
			final_stationarity_residual(FinalStatRes),
			final_primal_infeasibility(FinalPrimalInf),
			final_complementarity_gap(FinalCompGap),
			termination_reason(TerminationReason),
			final_value(BestValue)
		].

	termination_reason(Iterations, MaxIterations, Objective, Target, Point, Value, StatRes, PrimalInf, CompGap, TolG, TolK, Reason) :-
		(	Iterations >= MaxIterations ->
			Reason = max_iterations
		;	^^target_reached(Objective, Value, Target), PrimalInf =< TolK ->
			Reason = target_reached
		;	stop_condition(Iterations, Point, Value) ->
			Reason = stop_condition
		;	StatRes =< TolG, PrimalInf =< TolK, CompGap =< TolK ->
			Reason = converged
		;	Reason = kkt_singular
		).

	% initialization predicates

	init_slacks([], []).
	init_slacks([H| Hs], [S| Ss]) :-
		S is max(1.0, -H),
		init_slacks(Hs, Ss).

	init_multipliers_z([], []).
	init_multipliers_z([_| InequalityValues], [1.0| Multipliers]) :-
		init_multipliers_z(InequalityValues, Multipliers).

	init_multipliers_y([], []).
	init_multipliers_y([_| EqualityValues], [0.0| Multipliers]) :-
		init_multipliers_y(EqualityValues, Multipliers).

	% combined constraint data: general constraints (if any) plus box
	% bounds (if any) folded in as extra inequality rows with constant
	% Jacobians (+-1 on the diagonal); values still depend on the
	% current point, Jacobians do not

	equality_data(Point, EqualityValues, EqualityJacobian) :-
		(	equality_constraints(Point, EqualityValues) ->
			(	equality_jacobian(Point, EqualityJacobian) ->
				true
			;	existence_error(procedure, equality_jacobian/2)
			)
		;	EqualityValues = [],
			EqualityJacobian = []
		),
		^^validate_constraint_data(Point, EqualityValues, EqualityJacobian, equality_constraint_data).

	inequality_data(Point, InequalityValues, InequalityJacobian) :-
		(	inequality_constraints(Point, GeneralVals) ->
			(	inequality_jacobian(Point, GeneralJac) ->
				true
			;	existence_error(procedure, inequality_jacobian/2)
			)
		;	GeneralVals = [],
			GeneralJac = []
		),
		^^validate_constraint_data(Point, GeneralVals, GeneralJac, inequality_constraint_data),
		(	position_bounds(Bounds) ->
			^^check_bounds(Bounds),
			length(Point, Dimension),
			(	length(Bounds, Dimension) ->
				true
			;	domain_error(position_bounds, Bounds)
			),
			identity_matrix(Dimension, Identity),
			box_inequality_data(Point, Bounds, Identity, BoxVals, BoxJac)
		;	BoxVals = [],
			BoxJac = []
		),
		append(GeneralVals, BoxVals, InequalityValues),
		append(GeneralJac, BoxJac, InequalityJacobian).

	box_inequality_data([], [], [], [], []).
	box_inequality_data([Xi| Xs], [Low-High| Bs], [UnitRow| Rows], [LowerH, UpperH| Values], [LowerRow, UnitRow| Jac]) :-
		LowerH is Low - Xi,
		UpperH is Xi - High,
		scale_vector(UnitRow, -1.0, LowerRow),
		box_inequality_data(Xs, Bs, Rows, Values, Jac).

	% main loop

	loop(
		Iter, MaxIterations, UpdInt, _Sign, _Target,
		_ArmC, _ArmTau, _MaxBT, _TolX, _TolF, _TolG, _TolK,
		_MeritPenalty, _B,
		Point, Value, _PhiGrad, EqualityValues, _EqualityJacobian, InequalityValues, _InequalityJacobian, S, Y, Z,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals,
		StatRes, PrimalInf, CompGap
	) :-
		Iter >= MaxIterations,
		!,
		lagrangian_gradient_norm(Point, Y, Z, EqualityValues, InequalityValues, StatRes),
		primal_infeasibility(EqualityValues, InequalityValues, S, PrimalInf),
		complementarity_gap(S, Z, CompGap),
		^^report_final(Iter, UpdInt, Point, Value, StatRes).

	loop(
		Iter, _MaxIterations, UpdInt, Sign, Target,
		_ArmC, _ArmTau, _MaxBT, _TolX, _TolF, _TolG, TolK,
		_MeritPenalty, _B,
		Point, Value, _PhiGrad, EqualityValues, _EqualityJacobian, InequalityValues, _InequalityJacobian, S, Y, Z,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals,
		StatRes, PrimalInf, CompGap
	) :-
		% use once/1 to workaround an indexing bug in ECLiPSe and GNU Prolog
		once(^^objective_direction(Sign, Objective)),
		^^target_reached(Objective, Value, Target),
		primal_infeasibility(EqualityValues, InequalityValues, S, PrimalInf),
		PrimalInf =< TolK,
		!,
		lagrangian_gradient_norm(Point, Y, Z, EqualityValues, InequalityValues, StatRes),
		complementarity_gap(S, Z, CompGap),
		^^report_final(Iter, UpdInt, Point, Value, StatRes).

	loop(
		Iter, _MaxIterations, UpdInt, _Sign, _Target,
		_ArmC, _ArmTau, _MaxBT, _TolX, _TolF, _TolG, _TolK,
		_MeritPenalty, _B,
		Point, Value, _PhiGrad, EqualityValues, _EqualityJacobian, InequalityValues, _InequalityJacobian, S, Y, Z,
		Evals, GradEvals,
		Point, Value, Iter, Evals, GradEvals,
		StatRes, PrimalInf, CompGap
	) :-
		stop_condition(Iter, Point, Value),
		!,
		lagrangian_gradient_norm(Point, Y, Z, EqualityValues, InequalityValues, StatRes),
		primal_infeasibility(EqualityValues, InequalityValues, S, PrimalInf),
		complementarity_gap(S, Z, CompGap),
		^^report_final(Iter, UpdInt, Point, Value, StatRes).

	loop(
		Iter, MaxIterations, UpdInt, Sign, Target,
		ArmC, ArmTau, MaxBT, TolX, TolF, TolG, TolK,
		MeritPenalty0, B0,
		Point0, Value0, PhiGrad0, EqualityValues0, EqualityJacobian0, InequalityValues0, InequalityJacobian0, S0, Y0, Z0,
		Evals0, GradEvals0,
		BestPoint, BestValue, Iterations, Evaluations, GradEvaluations,
		FinalStatRes, FinalPrimalInf, FinalCompGap
	) :-
		lagrangian_gradient(PhiGrad0, EqualityJacobian0, Y0, InequalityJacobian0, Z0, RStat0),
		primal_infeasibility(EqualityValues0, InequalityValues0, S0, PrimalInf0),
		euclidean_norm(RStat0, StatResNorm0),
		complementarity_gap(S0, Z0, CompGap0),
		(	StatResNorm0 =< TolG,
			PrimalInf0 =< TolK,
			CompGap0 =< TolK ->
			BestPoint = Point0,
			BestValue = Value0,
			Iterations = Iter,
			Evaluations = Evals0,
			GradEvaluations = GradEvals0,
			FinalStatRes = StatResNorm0,
			FinalPrimalInf = PrimalInf0,
			FinalCompGap = CompGap0,
			^^report_final(Iter, UpdInt, Point0, Value0, StatResNorm0)
		;	mehrotra_step(B0, RStat0, EqualityJacobian0, EqualityValues0, InequalityJacobian0, S0, Z0, InequalityValues0, PointStep, EqualityMultiplierStep, SlackStep, InequalityMultiplierStep) ->
			euclidean_norm(PointStep, StepNorm),
			fraction_to_boundary(S0, SlackStep, AlphaS),
			fraction_to_boundary(Z0, InequalityMultiplierStep, AlphaZ),
			AlphaMax is min(AlphaS, AlphaZ),
			update_merit_penalty(MeritPenalty0, Y0, Z0, MeritPenalty1),
			constraint_violation_l1(EqualityValues0, InequalityValues0, S0, ViolationL1_0),
			dot_product(PhiGrad0, PointStep, ObjectiveDirectionalDerivative),
			MeritDirectionalDerivative is ObjectiveDirectionalDerivative - MeritPenalty1 * ViolationL1_0,
			merit_line_search(
				Point0, Value0, S0, Sign, PointStep, SlackStep, MeritDirectionalDerivative, MeritPenalty1, ViolationL1_0,
				ArmC, ArmTau, MaxBT, AlphaMax,
				Point1, Value1, S1, Alpha, Evals0, Evals1
			),
			take_dual_step(Y0, EqualityMultiplierStep, Alpha, Y1),
			take_dual_step(Z0, InequalityMultiplierStep, Alpha, Z1),
			gradient(Point1, Grad1),
			validate_gradient(Point1, Grad1),
			GradEvals1 is GradEvals0 + 1,
			scale_vector(Grad1, Sign, PhiGrad1),
			equality_data(Point1, EqualityValues1, EqualityJacobian1),
			inequality_data(Point1, InequalityValues1, InequalityJacobian1),
			lagrangian_gradient(PhiGrad1, EqualityJacobian1, Y1, InequalityJacobian1, Z1, RStat1),
			subtract_vectors(Point1, Point0, Sx),
			Iter1 is Iter + 1,
			^^report_progress(Iter1, UpdInt, Point1, Value1, StepNorm, Evals1),
			subtract_vectors(RStat1, RStat0, Ybfgs),
			damped_bfgs_update(Sx, Ybfgs, B0, B1),
			loop(
				Iter1, MaxIterations, UpdInt, Sign, Target,
				ArmC, ArmTau, MaxBT, TolX, TolF, TolG, TolK,
				MeritPenalty1, B1,
				Point1, Value1, PhiGrad1, EqualityValues1, EqualityJacobian1, InequalityValues1, InequalityJacobian1, S1, Y1, Z1,
				Evals1, GradEvals1,
				BestPoint, BestValue, Iterations, Evaluations, GradEvaluations,
				FinalStatRes, FinalPrimalInf, FinalCompGap
			)
		;	% the KKT system for a further step is (numerically)
			% singular, which near a boundary in IEEE double precision
			% usually means the iterates are already about as close to
			% the true solution as floating point allows; report the
			% best point found so far rather than failing outright
			BestPoint = Point0,
			BestValue = Value0,
			Iterations = Iter,
			Evaluations = Evals0,
			GradEvaluations = GradEvals0,
			FinalStatRes = StatResNorm0,
			FinalPrimalInf = PrimalInf0,
			FinalCompGap = CompGap0,
			^^report_final(Iter, UpdInt, Point0, Value0, StatResNorm0)
		).

	% Mehrotra predictor-corrector: an affine-scaling predictor step
	% (target complementarity 0, i.e. RComp_i = S_i*Z_i) determines how
	% much progress toward exact complementarity is achievable this
	% iteration, from which an adaptive centering parameter sigma is
	% derived (the standard cubic heuristic); the corrector step then
	% re-solves the same KKT system with both that centering term and a
	% second-order correction for the affine step's own curvature
	% (Ds_aff_i * Dz_aff_i). This is the standard fix for the naive
	% primal-dual Newton method's tendency to let z run away as s
	% approaches zero (a fixed or blindly-shrinking mu schedule was
	% tried first and found to diverge on exactly that pattern). With no
	% inequality constraints (S0 == []), there is no
	% complementarity condition to center and this degenerates to a
	% single plain Newton step.

	mehrotra_step(B, RStat, EqualityJacobian, EqualityValues, InequalityJacobian, S, Z, InequalityValues, PointStep, EqualityMultiplierStep, SlackStep, InequalityMultiplierStep) :-
		(	S == [] ->
			newton_step(B, RStat, EqualityJacobian, EqualityValues, InequalityJacobian, S, Z, InequalityValues, [], PointStep, EqualityMultiplierStep, SlackStep, InequalityMultiplierStep)
		;	residual_complementarity(S, Z, 0.0, RCompAff),
			newton_step(B, RStat, EqualityJacobian, EqualityValues, InequalityJacobian, S, Z, InequalityValues, RCompAff, _DxAff, _DyAff, DsAff, DzAff),
			fraction_to_boundary_tau(S, DsAff, 1.0, AlphaAffS),
			fraction_to_boundary_tau(Z, DzAff, 1.0, AlphaAffZ),
			AlphaAff is min(AlphaAffS, AlphaAffZ),
			step_preview(S, DsAff, AlphaAff, SAff),
			step_preview(Z, DzAff, AlphaAff, ZAff),
			dot_product(SAff, ZAff, MuAffTotal),
			length(S, M),
			MuAff is MuAffTotal / M,
			dot_product(S, Z, MuTotal),
			Mu is MuTotal / M,
			(	Mu =< 0.0 ->
				Sigma = 0.0
			;	Ratio is MuAff / Mu,
				RatioClamped is max(0.0, min(1.0, Ratio)),
				Sigma is RatioClamped * RatioClamped * RatioClamped
			),
			MuTarget is Sigma * Mu,
			second_order_complementarity(S, Z, DsAff, DzAff, MuTarget, RComp),
			newton_step(B, RStat, EqualityJacobian, EqualityValues, InequalityJacobian, S, Z, InequalityValues, RComp, PointStep, EqualityMultiplierStep, SlackStep, InequalityMultiplierStep)
		).

	step_preview([], _Dvec, _Alpha, []).
	step_preview([V| Vs], [D|Ds], Alpha, [P|Ps]) :-
		P is V + Alpha * D,
		step_preview(Vs, Ds, Alpha, Ps).

	% RComp_i = S_i*Z_i + DsAff_i*DzAff_i - MuTarget

	second_order_complementarity([], _Z, _DsAff, _DzAff, _MuTarget, []).
	second_order_complementarity([Si| Ss], [Zi| Zs], [DSi| DSs], [DZi| DZs], MuTarget, [Ri| Rs]) :-
		Ri is Si * Zi + DSi * DZi - MuTarget,
		second_order_complementarity(Ss, Zs, DSs, DZs, MuTarget, Rs).

	% KKT residuals

	% grad_L(x,y,z) = PhiGrad + EqualityJacobian^T.y + InequalityJacobian^T.z

	lagrangian_gradient(PhiGrad, EqualityJacobian, Y, InequalityJacobian, Z, GradL) :-
		(	Y == [] ->
			GradL1 = PhiGrad
		;	transpose_matrix(EqualityJacobian, EqualityJacobianTranspose),
			matrix_vector_product(EqualityJacobianTranspose, Y, EqTerm),
			add_vectors(PhiGrad, EqTerm, GradL1)
		),
		(	Z == [] ->
			GradL = GradL1
		;	transpose_matrix(InequalityJacobian, InequalityJacobianT),
			matrix_vector_product(InequalityJacobianT, Z, IneqTerm),
			add_vectors(GradL1, IneqTerm, GradL)
		).

	% used only by the MaxIterations/target/stop_condition early-exit
	% clauses, which do not otherwise touch the Lagrangian gradient

	lagrangian_gradient_norm(Point, Y, Z, EqualityValues, InequalityValues, Norm) :-
		gradient(Point, Grad),
		equality_data(Point, EqualityValues, EqualityJacobian),
		inequality_data(Point, InequalityValues, InequalityJacobian),
		lagrangian_gradient(Grad, EqualityJacobian, Y, InequalityJacobian, Z, GradL),
		euclidean_norm(GradL, Norm).

	residual_complementarity([], [], _Mu, []).
	residual_complementarity([S| Ss], [Z| Zs], Mu, [C| Cs]) :-
		C is S * Z - Mu,
		residual_complementarity(Ss, Zs, Mu, Cs).

	primal_infeasibility(EqualityValues, InequalityValues, S, Infeasibility) :-
		(	EqualityValues == [] ->
			EqNorm = 0.0
		;	^^abs_max(EqualityValues, EqNorm)
		),
		(	InequalityValues == [] ->
			IneqNorm = 0.0
		;	add_vectors(InequalityValues, S, InequalityResidual),
			^^abs_max(InequalityResidual, IneqNorm)
		),
		Infeasibility is max(EqNorm, IneqNorm).

	complementarity_gap(S, Z, Gap) :-
		(	S == [] ->
			Gap = 0.0
		;	dot_product(S, Z, SZ),
			length(S, M),
			Gap is SZ / M
		).

	constraint_violation_l1(EqualityValues, InequalityValues, S, Violation) :-
		(	EqualityValues == [] ->
			EqViol = 0.0
		;	^^abs_sum(EqualityValues, EqViol)
		),
		(	InequalityValues == [] ->
			IneqViol = 0.0
		;	add_vectors(InequalityValues, S, InequalityResidual),
			^^abs_sum(InequalityResidual, IneqViol)
		),
		Violation is EqViol + IneqViol.

	% Newton step: build and solve the reduced KKT system
	%
	%   [ B + InequalityJacobian^T diag(Z/S) InequalityJacobian    EqualityJacobian^T ] [dx]   [ rhs_x ]
	%   [ EqualityJacobian                                0     ] [dy] = [ -EqualityValues ]
	%
	% where rhs_x = -RStat - InequalityJacobian^T*c_z, c_z_i = (Z_i*(InequalityValues_i+S_i) -
	% RComp_i)/S_i, and ds/dz are recovered from dx afterwards. See the
	% primal_dual_interior_point(_) subsection in NOTES.md. Fails on a
	% singular KKT system, per the same "fails rather than raises"
	% convention as qp_active_set::solve/8.

	newton_step(B, RStat, EqualityJacobian, EqualityValues, InequalityJacobian, S, Z, InequalityValues, RComp, PointStep, EqualityMultiplierStep, SlackStep, InequalityMultiplierStep) :-
		length(RStat, N),
		(	InequalityValues == [] ->
			HBlock = B,
			RhsX = RStat
		;	add_vectors(InequalityValues, S, InequalityResidual),
			diag_ratio(Z, S, MultiplierSlackRatios),
			scale_rows(MultiplierSlackRatios, InequalityJacobian, DInequalityJacobian),
			transpose_matrix(InequalityJacobian, InequalityJacobianT),
			matrix_matrix_product(InequalityJacobianT, DInequalityJacobian, WeightedInequalityHessian),
			add_matrices(B, WeightedInequalityHessian, HBlock),
			build_cz(Z, InequalityResidual, S, RComp, ComplementarityCorrection),
			matrix_vector_product(InequalityJacobianT, ComplementarityCorrection, InequalityCorrectionGradient),
			add_vectors(RStat, InequalityCorrectionGradient, RhsX)
		),
		scale_vector(RhsX, -1.0, NegRhsX),
		(	EqualityValues == [] ->
			catch(solve_linear_system(HBlock, NegRhsX, PointStep), error(evaluation_error(zero_divisor), _), fail),
			EqualityMultiplierStep = []
		;	transpose_matrix(EqualityJacobian, EqualityJacobianTranspose),
			append_rows(HBlock, EqualityJacobianTranspose, TopRows),
			length(EqualityValues, P),
			new_matrix(P, P, 0.0, ZeroPP),
			append_rows(EqualityJacobian, ZeroPP, BottomRows),
			append(TopRows, BottomRows, KKT),
			scale_vector(EqualityValues, -1.0, NegEqualityValues),
			append(NegRhsX, NegEqualityValues, RHS),
			catch(solve_linear_system(KKT, RHS, Sol), error(evaluation_error(zero_divisor), _), fail),
			take(N, Sol, PointStep, EqualityMultiplierStep)
		),
		(	InequalityValues == [] ->
			SlackStep = [],
			InequalityMultiplierStep = []
		;	matrix_vector_product(InequalityJacobian, PointStep, LinearizedInequalityChange),
			add_vectors(InequalityValues, LinearizedInequalityChange, LinearizedInequalityValues),
			add_vectors(LinearizedInequalityValues, S, LinearizedInequalityResidual),
			scale_vector(LinearizedInequalityResidual, -1.0, SlackStep),
			recover_dz(S, Z, RComp, SlackStep, InequalityMultiplierStep)
		).

	diag_ratio([], [], []).
	diag_ratio([Z| Zs], [S| Ss], [D| Ds]) :-
		D is Z / S,
		diag_ratio(Zs, Ss, Ds).

	build_cz([], [], [], [], []).
	build_cz([Z| Zs], [H| Hs], [S| Ss], [R| Rs], [C| Cs]) :-
		C is (Z * H - R) / S,
		build_cz(Zs, Hs, Ss, Rs, Cs).

	recover_dz([], [], [], [], []).
	recover_dz([S| Ss], [Z| Zs], [R| Rs], [Ds| Dss], [Dz| Dzs]) :-
		Dz is (-R - Z * Ds) / S,
		recover_dz(Ss, Zs, Rs, Dss, Dzs).

	% fraction-to-boundary rule: the largest alpha in (0,1] keeping
	% every component of Vec + alpha*Dvec at least (1-tau) times its
	% current value. fraction_to_boundary/3 uses tau = 0.995, the
	% standard choice for the corrector step; fraction_to_boundary_tau/4
	% takes tau explicitly (mehrotra_step/12 uses tau = 1.0, unrestricted
	% up to the boundary itself, for the affine-scaling predictor step,
	% since that step is only used to measure achievable progress, not
	% actually taken).

	fraction_to_boundary(Vec, Dvec, Alpha) :-
		fraction_to_boundary_tau(Vec, Dvec, 0.995, Alpha).

	fraction_to_boundary_tau(Vec, Dvec, Tau, Alpha) :-
		(	Vec == [] ->
			Alpha = 1.0
		;	fraction_to_boundary_(Vec, Dvec, Tau, 1.0, Alpha)
		).

	fraction_to_boundary_([], [], _Tau, Alpha, Alpha).
	fraction_to_boundary_([V| Vs], [D| Ds], Tau, AlphaIn, Alpha) :-
		(	D < 0.0 ->
			Cand is -Tau * V / D,
			AlphaNext is min(AlphaIn, Cand)
		;	AlphaNext = AlphaIn
		),
		fraction_to_boundary_(Vs, Ds, Tau, AlphaNext, Alpha).

	% merit-function backtracking line search (same structure as
	% sqp_active_set(_)'s, extended to also move S along Ds and to
	% report back the accepted Alpha, which take_dual_step/4 then
	% reuses for the Y/Z steps)

	merit_line_search(
		Point0, Value0, S0, Sign, PointStep, SlackStep, MeritDirectionalDerivative, MeritPenalty, Violation0,
		ArmC, ArmTau, MaxBT, AlphaMax,
		Point1, Value1, S1, Alpha, Evals0, Evals1
	) :-
		(	MeritDirectionalDerivative < -1.0e-12 ->
			merit_backtrack(
				0, MaxBT, Point0, Value0, S0, Sign, PointStep, SlackStep, MeritPenalty, Violation0,
				AlphaMax, ArmC, ArmTau, MeritDirectionalDerivative,
				Point1, Value1, S1, Alpha, Evals0, Evals1
			)
		;	Alpha = AlphaMax,
			scale_vector(PointStep, Alpha, ScaledPointStep),
			add_vectors(Point0, ScaledPointStep, Point1),
			(	S0 == [] ->
				S1 = []
			;	scale_vector(SlackStep, Alpha, ScaledSlackStep),
				add_vectors(S0, ScaledSlackStep, S1)
			),
			objective(Point1, Value1),
			(	number(Value1) ->
				true
			;	domain_error(objective, Value1)
			),
			Evals1 is Evals0 + 1
		).

	merit_backtrack(
		BT, MaxBT, Point0, Value0, S0, Sign, PointStep, SlackStep, MeritPenalty, Violation0,
		Alpha, ArmC, ArmTau, MeritDirectionalDerivative,
		Point1, Value1, S1, AlphaOut, Evals0, Evals1
	) :-
		scale_vector(PointStep, Alpha, ScaledPointStep),
		add_vectors(Point0, ScaledPointStep, Trial),
		(	S0 == [] ->
			STrial = []
		;	scale_vector(SlackStep, Alpha, ScaledSlackStep),
			add_vectors(S0, ScaledSlackStep, STrial)
		),
		objective(Trial, TrialVal),
		(	number(TrialVal) ->
			true
		;	domain_error(objective, TrialVal)
		),
		Evals1_try is Evals0 + 1,
		trial_violation_l1(Trial, STrial, TrialViolationL1),
		PhiValue0 is Sign * Value0,
		PhiTrial is Sign * TrialVal,
		Merit0 is PhiValue0 + MeritPenalty * Violation0,
		MeritTrial is PhiTrial + MeritPenalty * TrialViolationL1,
		Sufficient is Merit0 + ArmC * Alpha * MeritDirectionalDerivative,
		(	MeritTrial =< Sufficient ->
			Point1 = Trial,
			Value1 = TrialVal,
			S1 = STrial,
			AlphaOut = Alpha,
			Evals1 = Evals1_try
		;	BT1 is BT + 1,
			(	BT1 >= MaxBT ->
				Point1 = Trial,
				Value1 = TrialVal,
				S1 = STrial,
				AlphaOut = Alpha,
				Evals1 = Evals1_try
			;	Alpha1 is Alpha * ArmTau,
				merit_backtrack(
					BT1, MaxBT, Point0, Value0, S0, Sign, PointStep, SlackStep, MeritPenalty, Violation0,
					Alpha1, ArmC, ArmTau, MeritDirectionalDerivative,
					Point1, Value1, S1, AlphaOut, Evals1_try, Evals1
				)
			)
		).

	trial_violation_l1(Point, S, Violation) :-
		(	equality_constraints(Point, EqualityValues) ->
			^^abs_sum(EqualityValues, EqViol)
		;	EqViol = 0.0
		),
		combined_inequality_values(Point, InequalityValues),
		(	InequalityValues == [] ->
			IneqViol = 0.0
		;	add_vectors(InequalityValues, S, InequalityResidual),
			^^abs_sum(InequalityResidual, IneqViol)
		),
		Violation is EqViol + IneqViol.

	% inequality values only (no Jacobian), for the merit line search,
	% which does not need the Jacobian at trial points

	combined_inequality_values(Point, InequalityValues) :-
		(	inequality_constraints(Point, GeneralVals) ->
			true
		;	GeneralVals = []
		),
		(	position_bounds(Bounds) ->
			box_inequality_values(Point, Bounds, BoxVals)
		;	BoxVals = []
		),
		append(GeneralVals, BoxVals, InequalityValues).

	box_inequality_values([], [], []).
	box_inequality_values([Xi| Xs], [Low-High| Bs], [LowerH, UpperH| Values]) :-
		LowerH is Low - Xi,
		UpperH is Xi - High,
		box_inequality_values(Xs, Bs, Values).

	% dual (y, z) step at the same Alpha the merit line search settled
	% on for (x, s)

	take_dual_step(V0, Dv, Alpha, V1) :-
		(	V0 == [] ->
			V1 = []
		;	scale_vector(Dv, Alpha, ScaledDv),
			add_vectors(V0, ScaledDv, V1)
		).

	% damped BFGS update of the Hessian-of-the-Lagrangian approximation
	% (identical formula to sqp_active_set(_)'s)

	damped_bfgs_update(PointChange, LagrangianGradientChange, B0, B1) :-
		matrix_vector_product(B0, PointChange, Bs),
		dot_product(PointChange, Bs, SBs),
		(	SBs =< 1.0e-12 ->
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

	update_merit_penalty(MeritPenalty0, Y, Z, MeritPenalty1) :-
		^^abs_max(Y, YMax),
		^^abs_max(Z, ZMax),
		Needed is 1.1 * max(YMax, ZMax),
		MeritPenalty1 is max(MeritPenalty0, Needed).

	% auxiliary predicates

	scale_rows([], [], []).
	scale_rows([C| Cs], [Row| Rows], [ScaledRow| ScaledRows]) :-
		scale_vector(Row, C, ScaledRow),
		scale_rows(Cs, Rows, ScaledRows).

	append_rows([], [], []).
	append_rows([R1| R1s], [R2| R2s], [R| Rs]) :-
		append(R1, R2, R),
		append_rows(R1s, R2s, Rs).

	require_gradient :-
		(	_Problem_::predicate_property(gradient(_, _), defined_in(_)) ->
			true
		;	existence_error(procedure, gradient/2)
		).

	validate_gradient(Point, Gradient) :-
		length(Point, Length),
		length(Gradient, Length),
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

	% options specific to primal_dual_interior_point

	default_option(merit_penalty0(10.0)).
	default_option(armijo_c(1.0e-4)).
	default_option(armijo_tau(0.5)).
	default_option(armijo_max_backtracks(20)).
	default_option(tol_constraint(1.0e-6)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(merit_penalty0(P)) :-
		number(P), P > 0.0.
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
