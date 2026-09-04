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


:- object(qp_active_set,
	implements(qp_solver_protocol),
	imports(constrained_optimization_solver)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Dense active-set solver for convex quadratic programs. Intended as a subroutine for sqp_active_set(_); also usable standalone for small dense QPs.',
		parameters is [],
		see_also is [qp_solver_protocol, sqp_active_set(_), linear_algebra]
	]).

	:- uses(linear_algebra, [
		add_vectors/3, dot_product/3, euclidean_norm/2, matrix_vector_product/3, new_matrix/4, new_vector/3,
		pseudo_inverse/2, scale_vector/3, solve_linear_system/3, transpose_matrix/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, nth0/3, take/4
	]).

	% public entry point
	%
	% Lambda = EqLambda ++ IneqLambda: the first Meq elements are the
	% equality-constraint multipliers (always defined, equalities are
	% always in the working set); the remaining Mineq elements are the
	% inequality-constraint multipliers, in the same row order as
	% Aineq/Bineq, with 0.0 for every inequality not active at the
	% returned X (standard convention: an inactive "=<" constraint has
	% multiplier 0 at a KKT point).

	solve(H, C, Aeq, Beq, Aineq, Bineq, X, Lambda) :-
		validate_dimensions(H, C, Aeq, Beq, Aineq, Bineq, N, Mineq),
		find_feasible_point(N, Aeq, Beq, Aineq, Bineq, X0, IneqActive0),
		equality_indices(Aeq, EqIdx),
		length(EqIdx, Meq),
		max_active_set_iterations(N, Mineq, MaxIter),
		active_set_loop(
			0, MaxIter, H, C, Aeq, Beq, Aineq, Bineq,
			X0, EqIdx, IneqActive0, X, FinalIneqActive, FinalLambda
		),
		feasible_point(Aeq, Beq, Aineq, Bineq, X),
		take(Meq, FinalLambda, EqLambda, IneqLambdaActive),
		expand_multipliers(0, Mineq, FinalIneqActive, IneqLambdaActive, IneqLambda),
		append(EqLambda, IneqLambda, Lambda).

	% dimension validation: fails via domain_error/1 on malformed input
	% rather than proceeding with mismatched matrices, consistent with
	% how bfgs(_) validates its gradient shape before starting its loop

	validate_dimensions(H, C, Aeq, Beq, Aineq, Bineq, N, Mineq) :-
		^^validate_numeric_vector(C, qp_c_vector),
		length(C, N),
		N >= 1,
		(	length(H, N) ->
			true
		;	domain_error(qp_h_matrix, H)
		),
		valid_matrix_rows(H, N),
		length(Aeq, Meq),
		valid_matrix_rows(Aeq, N),
		^^validate_numeric_vector(Beq, qp_beq_vector),
		(	length(Beq, Meq) ->
			true
		;	domain_error(qp_beq_vector, Beq)
		),
		length(Aineq, Mineq),
		valid_matrix_rows(Aineq, N),
		^^validate_numeric_vector(Bineq, qp_bineq_vector),
		(	length(Bineq, Mineq) ->
			true
		;	domain_error(qp_bineq_vector, Bineq)
		).

	valid_matrix_rows([], _N).
	valid_matrix_rows([Row| Rows], N) :-
		(	length(Row, N) ->
			true
		;	domain_error(qp_matrix_row, Row)
		),
		^^validate_numeric_vector(Row, qp_matrix_row),
		valid_matrix_rows(Rows, N).

	equality_indices(Aeq, EqIdx) :-
		length(Aeq, Meq),
		count_up(0, Meq, EqIdx).

	count_up(I, Meq, Idxs) :-
		(	I >= Meq ->
			Idxs = []
		;	Idxs = [I|Idxs1],
			I1 is I + 1,
			count_up(I1, Meq, Idxs1)
		).

	% an iteration cap that scales with problem size, a second,
	% independent safety net alongside Bland's rule; not currently
	% user-configurable

	max_active_set_iterations(N, Mineq, MaxIter) :-
		MaxIter is 50 * (N + Mineq + 1).

	% phase 1: heuristic feasible starting point

	find_feasible_point(N, Aeq, Beq, Aineq, Bineq, X, ActiveIneq) :-
		(	Aeq == [] ->
			new_vector(N, 0.0, X0)
		;	least_norm_solve(Aeq, Beq, X0)
		),
		length(Aineq, Mineq),
		Budget is 10 * (Mineq + 1),
		feasibility_fix(Budget, Aeq, Beq, Aineq, Bineq, X0, [], X, ActiveIneq),
		feasible_point(Aeq, Beq, Aineq, Bineq, X).

	feasibility_fix(Budget, Aeq, Beq, Aineq, Bineq, X0, Active0, X, Active) :-
		(	Budget =< 0 ->
			X = X0,
			Active = Active0
		;	violated_indices(Aineq, Bineq, X0, Viol),
			(	Viol == [] ->
				X = X0,
				Active = Active0
			;	merge_indices(Viol, Active0, Active1),
				select_rows(Active1, Aineq, AW),
				select_elems(Active1, Bineq, BW),
				(	Aeq == [] ->
					AllA = AW,
					AllB = BW
				;	append(Aeq, AW, AllA),
					append(Beq, BW, AllB)
				),
				least_norm_solve(AllA, AllB, X1),
				Budget1 is Budget - 1,
				feasibility_fix(Budget1, Aeq, Beq, Aineq, Bineq, X1, Active1, X, Active)
			)
		).

	violated_indices(Aineq, Bineq, X, Viol) :-
		violated_indices(Aineq, Bineq, X, 0, Viol).

	violated_indices([], [], _X, _I, []).
	violated_indices([Row| Rows], [Bi| Bs], X, I, Viol) :-
		dot_product(Row, X, V),
		I1 is I + 1,
		(	V > Bi + 1.0e-8 ->
			Viol = [I| Viol1]
		;	Viol = Viol1
		),
		violated_indices(Rows, Bs, X, I1, Viol1).

	feasible_point(Aeq, Beq, Aineq, Bineq, X) :-
		equalities_satisfied(Aeq, Beq, X),
		violated_indices(Aineq, Bineq, X, Viol),
		Viol == [].

	equalities_satisfied([], [], _X).
	equalities_satisfied([Row| Rows], [Bi| Bs], X) :-
		dot_product(Row, X, V),
		abs(V - Bi) =< 1.0e-8,
		equalities_satisfied(Rows, Bs, X).

	merge_indices([], Active, Active).
	merge_indices([V| Vs], Active0, Active) :-
		(	member(V, Active0) ->
			Active1 = Active0
		;	append(Active0, [V], Active1)
		),
		merge_indices(Vs, Active1, Active).

	% main active-set loop
	%
	% EqIdx is fixed for the whole run (all equality-row indices, always
	% in the working set). IneqActive is the current subset of Aineq row
	% indices also in the working set.

	active_set_loop(Iter, MaxIter, H, C, Aeq, Beq, Aineq, Bineq, X, EqIdx, IneqActive, XOut, IneqActiveOut, LambdaOut) :-
		(	Iter >= MaxIter ->
			fail
		;	build_working_set(Aeq, Aineq, EqIdx, IneqActive, AW),
			matrix_vector_product(H, X, HX),
			add_vectors(HX, C, G),
			kkt_step(H, G, AW, P, Lambda),
			euclidean_norm(P, PNorm),
			Iter1 is Iter + 1,
			(	PNorm < 1.0e-8 ->
				length(EqIdx, Meq),
				inequality_multipliers(IneqActive, Lambda, Meq, MuList),
				check_drop(MuList, Drop),
				(	Drop = drop(WorstIdx) ->
					remove_active(IneqActive, WorstIdx, IneqActive1),
					active_set_loop(Iter1, MaxIter, H, C, Aeq, Beq, Aineq, Bineq, X, EqIdx, IneqActive1, XOut, IneqActiveOut, LambdaOut)
				;	XOut = X,
					IneqActiveOut = IneqActive,
					LambdaOut = Lambda
				)
			;	step_length(Aineq, Bineq, X, P, IneqActive, Alpha, Blocking),
				scale_vector(P, Alpha, AP),
				add_vectors(X, AP, X1),
				(	Blocking == none ->
					active_set_loop(Iter1, MaxIter, H, C, Aeq, Beq, Aineq, Bineq, X1, EqIdx, IneqActive, XOut, IneqActiveOut, LambdaOut)
				;	add_active(IneqActive, Blocking, IneqActive1),
					active_set_loop(Iter1, MaxIter, H, C, Aeq, Beq, Aineq, Bineq, X1, EqIdx, IneqActive1, XOut, IneqActiveOut, LambdaOut)
				)
			)
		).

	build_working_set(Aeq, Aineq, EqIdx, IneqActive, AW) :-
		(	EqIdx == [] ->
			AW0 = []
		;	AW0 = Aeq
		),
		select_rows(IneqActive, Aineq, AineqRows),
		append(AW0, AineqRows, AW).

	% KKT system for the current working set AW (stacked equality and
	% active-inequality rows):
	%
	%   [ H   AW^T ] [ p      ]   [ -g ]
	%   [ AW  0    ] [ lambda ] = [  0 ]
	%
	% p is the step in x; lambda are the multipliers of the equality-
	% constrained subproblem "minimize 0.5*p^T*H*p + g^T*p s.t. AW*p = 0"
	% and are used to decide which working-set inequality (if any) to
	% drop once p is (numerically) zero. A singular KKT matrix (linearly
	% dependent active constraints) makes solve_linear_system/3 raise
	% evaluation_error(zero_divisor); that is caught here and turned
	% into failure, per this object's documented "fails rather than
	% raises" contract.

	kkt_step(H, G, AW, P, Lambda) :-
		(	AW == [] ->
			Lambda = [],
			scale_vector(G, -1.0, NegG),
			catch(solve_linear_system(H, NegG, P), error(evaluation_error(zero_divisor), _), fail)
		;	length(H, N),
			length(AW, M),
			transpose_matrix(AW, AWt),
			append_rows(H, AWt, TopRows),
			new_matrix(M, M, 0.0, ZeroBlock),
			append_rows(AW, ZeroBlock, BottomRows),
			append(TopRows, BottomRows, KKT),
			scale_vector(G, -1.0, NegG),
			new_vector(M, 0.0, ZerosM),
			append(NegG, ZerosM, RHS),
			catch(solve_linear_system(KKT, RHS, Sol), error(evaluation_error(zero_divisor), _), fail),
			take(N, Sol, P, Lambda)
		).

	% multiplier of each active inequality: the K-th element of
	% IneqActive corresponds to Lambda position Meq+K (the equality
	% multipliers occupy positions 0..Meq-1)

	inequality_multipliers(IneqActive, Lambda, Meq, Pairs) :-
		inequality_multipliers(IneqActive, Lambda, Meq, 0, Pairs).

	inequality_multipliers([], _Lambda, _Meq, _K, []).
	inequality_multipliers([Idx| Idxs], Lambda, Meq, K, [Idx-Mu| Pairs]) :-
		KK is Meq + K,
		nth0(KK, Lambda, Mu),
		K1 is K + 1,
		inequality_multipliers(Idxs, Lambda, Meq, K1, Pairs).

	% expands the multipliers for the constraints in the final working
	% set into a full length-Mineq vector, in Aineq row order, with 0.0
	% for every inequality that ended up inactive

	expand_multipliers(I, Mineq, ActiveIdxs, ActiveLambdas, Full) :-
		(	I >= Mineq ->
			Full = []
		;	(	nth0(Pos, ActiveIdxs, I) ->
				nth0(Pos, ActiveLambdas, Mu)
			;	Mu = 0.0
			),
			Full = [Mu| Full1],
			I1 is I + 1,
			expand_multipliers(I1, Mineq, ActiveIdxs, ActiveLambdas, Full1)
		).

	% Bland's rule (lowest-index rule) for the leaving-variable choice:
	% drop the first (lowest working-set-order) inequality with a
	% negative multiplier, rather than the most negative one. This is
	% weaker in the sense that it can take more iterations in practice
	% than always dropping the most negative multiplier (a Dantzig-style
	% choice, which is what an earlier version of this predicate did),
	% but it is what guarantees active_set_loop/12 terminates rather
	% than cycling on a degenerate problem.

	check_drop(Pairs, Drop) :-
		lowest_negative_index(Pairs, none, Drop).

	lowest_negative_index([], Drop, Drop).
	lowest_negative_index([Idx-Val| Pairs], Current, Drop) :-
		(	Val < -1.0e-8 ->
			(	Current = drop(CurrentIdx), CurrentIdx < Idx ->
				Next = Current
			;	Next = drop(Idx)
			)
		;	Next = Current
		),
		lowest_negative_index(Pairs, Next, Drop).

	remove_active(Active, Idx, Active1) :-
		Active = [H1|Rest],
		(	H1 == Idx ->
			Active1 = Rest
		;	Active1 = [H1|Rest1],
			remove_active(Rest, Idx, Rest1)
		).

	add_active(IneqActive, Blocking, IneqActive1) :-
		(	member(Blocking, IneqActive) ->
			IneqActive1 = IneqActive
		;	append(IneqActive, [Blocking], IneqActive1)
		).

	% step length to the nearest inactive inequality constraint that P
	% would violate; Blocking is `none` when the full step (Alpha = 1.0)
	% stays feasible with respect to every inactive inequality

	step_length(Aineq, Bineq, X, P, IneqActive, Alpha, Blocking) :-
		step_length(Aineq, Bineq, X, P, IneqActive, 0, none, 1.0, Blocking, Alpha).

	step_length([], [], _X, _P, _IneqActive, _I, Blocking, Alpha, Blocking, Alpha).
	step_length([Row| Rows], [Bi| Bs], X, P, IneqActive, I, BlockingIn, AlphaIn, Blocking, Alpha) :-
		(	member(I, IneqActive) ->
			BlockingNext = BlockingIn,
			AlphaNext = AlphaIn
		;	dot_product(Row, P, ApRow),
			(	ApRow > 1.0e-10 ->
				dot_product(Row, X, AxRow),
				Ratio is (Bi - AxRow) / ApRow,
				(	Ratio < AlphaIn ->
					BlockingNext = I,
					AlphaNext = Ratio
				;	BlockingNext = BlockingIn,
					AlphaNext = AlphaIn
				)
			;	BlockingNext = BlockingIn,
				AlphaNext = AlphaIn
			)
		),
		I1 is I + 1,
		step_length(Rows, Bs, X, P, IneqActive, I1, BlockingNext, AlphaNext, Blocking, Alpha).

	% small structural auxiliary predicates (block-matrix / row-set bookkeeping) not
	% provided by the "linear_algebra" library, which operates on whole matrices

	append_rows([], [], []).
	append_rows([R1| R1s], [R2| R2s], [R| Rs]) :-
		append(R1, R2, R),
		append_rows(R1s, R2s, Rs).

	select_rows([], _Matrix, []).
	select_rows([I| Is], Matrix, [Row| Rows]) :-
		nth0(I, Matrix, Row),
		select_rows(Is, Matrix, Rows).

	select_elems([], _Vector, []).
	select_elems([I| Is], Vector, [Element| Elements]) :-
		nth0(I, Vector, Element),
		select_elems(Is, Vector, Elements).

	% least-norm solution of A*x = b for A with (assumed) full row rank
	% m =< n, via the Moore-Penrose pseudo-inverse: x = A^+ * b

	least_norm_solve(A, B, X) :-
		pseudo_inverse(A, APlus),
		matrix_vector_product(APlus, B, X).

:- end_object.
