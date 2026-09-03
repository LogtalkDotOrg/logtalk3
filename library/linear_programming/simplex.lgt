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


:- object(simplex,
	imports(linear_programming_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Portable dense two-phase simplex solver for small continuous linear programs.',
		remarks is [
			'Variable types' - 'This backend supports continuous variables only. Integer and binary variables are accepted by the shared model API for use by future MILP backends but rejected when solving.',
			'Pivoting' - 'Bland\'s rule is used by default. Dantzig\'s rule is also available, with the lowest column index breaking ties. Minimum-ratio ties use the lowest-indexed basic variable.'
		],
		see_also is [linear_programming_protocol]
	]).

	:- uses(linear_algebra, [
		new_vector/3, new_vector_like/2, scale_vector/3
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, nth1/3
	]).

	solve(Problem, Result) :-
		::solve(Problem, Result, []).

	solve(Problem, Result, UserOptions) :-
		^^check_problem(Problem),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(max_iterations(MaxIterations), Options),
		^^option(tolerance(Tolerance), Options),
		^^option(pivot_rule(PivotRule), Options),
		Problem = linear_program(Variables, Constraints, Objective),
		check_solvable_problem(Variables, Objective),
		prepare_variables(Variables, 1, NextColumn, Recoveries, BoundRows),
		TransformedVariableCount is NextColumn - 1,
		transform_constraints(Constraints, Recoveries, TransformedVariableCount, Rows0),
		append(Rows0, BoundRows, Rows1),
		normalize_rows(Rows1, Rows),
		Objective = objective(ObjectiveExpression, ObjectiveSense),
		transform_expression(ObjectiveExpression, Recoveries, TransformedVariableCount, ObjectiveCoefficients0, _ObjectiveConstant),
		objective_coefficients(ObjectiveSense, ObjectiveCoefficients0, ObjectiveCoefficients),
		build_tableau(Rows, TransformedVariableCount, Tableau0, Basis0, ArtificialColumns, TotalColumns),
		phase_one_costs(TotalColumns, ArtificialColumns, PhaseOneCosts),
		build_objective_row(PhaseOneCosts, Tableau0, Basis0, PhaseOneObjective0),
		simplex_loop(Tableau0, Basis0, PhaseOneObjective0, ArtificialColumns, PivotRule, Tolerance, MaxIterations, 0, PhaseOneStatus, Tableau1, Basis1, PhaseOneObjective, PhaseOneIterations),
		finish_phase_one(PhaseOneStatus, PhaseOneObjective, Tableau1, Basis1, ArtificialColumns, Tolerance, PhaseOneFinish, Tableau2, Basis2),
		continue_with_phase_two(PhaseOneFinish, Problem, Recoveries, TransformedVariableCount, ObjectiveCoefficients, Tableau2, Basis2, ArtificialColumns, PivotRule, Tolerance, MaxIterations, PhaseOneIterations, Result).

	default_option(max_iterations(10000)).
	default_option(tolerance(1.0e-9)).
	default_option(pivot_rule(bland)).

	valid_option(max_iterations(MaxIterations)) :-
		integer(MaxIterations),
		MaxIterations > 0.
	valid_option(tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance > 0.
	valid_option(pivot_rule(PivotRule)) :-
		once((PivotRule == bland; PivotRule == dantzig)).

	check_solvable_problem([], _Objective) :-
		domain_error(linear_programming_problem, empty).
	check_solvable_problem(_Variables, none) :-
		domain_error(linear_programming_problem, missing_objective).
	check_solvable_problem(Variables, objective(_Expression, _Sense)) :-
		check_continuous_variables(Variables).

	check_continuous_variables([]) :-
		!.
	check_continuous_variables([variable(_Name, continuous, _Lower, _Upper)| Variables]) :-
		!,
		check_continuous_variables(Variables).
	check_continuous_variables([variable(Name, Type, _Lower, _Upper)| _Variables]) :-
		domain_error(simplex_variable_type, Name-Type).

	prepare_variables([], NextColumn, NextColumn, [], []).
	prepare_variables([variable(Name, continuous, Lower, Upper)| Variables], Column0, Column, [recovery(Name, Constant, Terms)| Recoveries], BoundRows) :-
		variable_transformation(Lower, Upper, Column0, Column1, Constant, Terms, VariableBoundRows),
		prepare_variables(Variables, Column1, Column, Recoveries, RemainingBoundRows),
		append(VariableBoundRows, RemainingBoundRows, BoundRows).

	variable_transformation(Lower, Upper, Column, Column, Lower, [], []) :-
		number(Lower), number(Upper), Lower =:= Upper,
		!.
	variable_transformation(Lower, inf, Column0, Column, Lower, [1*Column0], []) :-
		number(Lower),
		!,
		Column is Column0 + 1.
	variable_transformation(Lower, Upper, Column0, Column, Lower, [1*Column0], [raw_row([1*Column0], (=<), Width)]) :-
		number(Lower), number(Upper),
		!,
		Width is Upper - Lower,
		Column is Column0 + 1.
	variable_transformation(-inf, Upper, Column0, Column, Upper, [-1*Column0], []) :-
		number(Upper),
		!,
		Column is Column0 + 1.
	variable_transformation(-inf, inf, Column0, Column, 0, [1*Column0, -1*Column1], []) :-
		Column1 is Column0 + 1,
		Column is Column1 + 1.

	transform_constraints([], _Recoveries, _VariableCount, []).
	transform_constraints([constraint(Expression, Sense, RightHandSide)| Constraints], Recoveries, VariableCount, [raw_row(Coefficients, Sense, TransformedRightHandSide)| Rows]) :-
		transform_expression(Expression, Recoveries, VariableCount, Coefficients, Constant),
		TransformedRightHandSide is RightHandSide - Constant,
		transform_constraints(Constraints, Recoveries, VariableCount, Rows).

	transform_expression(Expression, Recoveries, VariableCount, Coefficients, Constant) :-
		new_vector(VariableCount, 0.0, ZeroCoefficients),
		transform_expression_terms(Expression, Recoveries, ZeroCoefficients, Coefficients, 0, Constant).

	transform_expression_terms([], _Recoveries, Coefficients, Coefficients, Constant, Constant).
	transform_expression_terms([Coefficient*Name| Expression], Recoveries, Coefficients0, Coefficients, Constant0, Constant) :-
		recovery(Name, Recoveries, VariableConstant, Terms),
		Constant1 is Constant0 + Coefficient * VariableConstant,
		add_transformed_terms(Terms, Coefficient, Coefficients0, Coefficients1),
		transform_expression_terms(Expression, Recoveries, Coefficients1, Coefficients, Constant1, Constant).

	recovery(Name, [recovery(StoredName, Constant, Terms)| _Recoveries], Constant, Terms) :-
		Name == StoredName,
		!.
	recovery(Name, [_Recovery| Recoveries], Constant, Terms) :-
		recovery(Name, Recoveries, Constant, Terms).

	add_transformed_terms([], _Scale, Coefficients, Coefficients).
	add_transformed_terms([Factor*Column| Terms], Scale, Coefficients0, Coefficients) :-
		Delta is Scale * Factor,
		add_at(Column, Delta, Coefficients0, Coefficients1),
		add_transformed_terms(Terms, Scale, Coefficients1, Coefficients).

	add_at(1, Delta, [Value| Values], [NewValue| Values]) :-
		!,
		NewValue is Value + Delta.
	add_at(Index, Delta, [Value| Values], [Value| NewValues]) :-
		NextIndex is Index - 1,
		add_at(NextIndex, Delta, Values, NewValues).

	normalize_rows([], []).
	normalize_rows([raw_row(SparseCoefficients, Sense, RightHandSide)| Rows], [row(NormalizedCoefficients, NormalizedSense, NormalizedRightHandSide)| NormalizedRows]) :-
		dense_bound_row(SparseCoefficients, Coefficients),
		(	RightHandSide < 0 ->
			scale_vector(Coefficients, -1, NormalizedCoefficients),
			flip_sense(Sense, NormalizedSense),
			NormalizedRightHandSide is -RightHandSide
		;	NormalizedSense = Sense,
			NormalizedRightHandSide = RightHandSide,
			NormalizedCoefficients = Coefficients
		),
		normalize_rows(Rows, NormalizedRows).

	dense_bound_row(Coefficients, Coefficients) :-
		Coefficients = [],
		!.
	dense_bound_row([_Coefficient*_Column| _Terms]=Terms, Coefficients) :-
		maximum_column(Terms, 0, Length),
		new_vector(Length, 0.0, Zeroes),
		add_sparse_terms(Terms, Zeroes, Coefficients).
	dense_bound_row(Coefficients, Coefficients).

	maximum_column([], Maximum, Maximum).
	maximum_column([_Coefficient*Column| Terms], Maximum0, Maximum) :-
		(	Column > Maximum0 ->
			Maximum1 = Column
		;	Maximum1 = Maximum0
		),
		maximum_column(Terms, Maximum1, Maximum).

	add_sparse_terms([], Coefficients, Coefficients).
	add_sparse_terms([Coefficient*Column| Terms], Coefficients0, Coefficients) :-
		add_at(Column, Coefficient, Coefficients0, Coefficients1),
		add_sparse_terms(Terms, Coefficients1, Coefficients).

	flip_sense((=<), (>=)).
	flip_sense((>=), (=<)).
	flip_sense((=), (=)).

	objective_coefficients(maximize, Coefficients, Coefficients).
	objective_coefficients(minimize, Coefficients, MaximizationCoefficients) :-
		scale_vector(Coefficients, -1, MaximizationCoefficients).

	build_tableau(Rows, VariableCount, Tableau, Basis, ArtificialColumns, TotalColumns) :-
		count_auxiliary_columns(Rows, 0, AuxiliaryCount),
		TotalColumns is VariableCount + AuxiliaryCount,
		NextColumn is VariableCount + 1,
		build_tableau_rows(Rows, VariableCount, TotalColumns, NextColumn, Tableau, Basis, ArtificialColumns).

	count_auxiliary_columns([], Count, Count).
	count_auxiliary_columns([row(_Coefficients, Sense, _RightHandSide)| Rows], Count0, Count) :-
		(	Sense == (=<) ->
			Increment = 1
		;	Sense == (>=) ->
			Increment = 2
		;	Increment = 1
		),
		Count1 is Count0 + Increment,
		count_auxiliary_columns(Rows, Count1, Count).

	build_tableau_rows([], _VariableCount, _TotalColumns, _NextColumn, [], [], []).
	build_tableau_rows([row(Coefficients0, Sense, RightHandSide)| Rows], VariableCount, TotalColumns, NextColumn0, [tableau_row(Coefficients, RightHandSide)| Tableau], [Basic| Basis], ArtificialColumns) :-
		pad_vector(Coefficients0, VariableCount, OriginalCoefficients),
		pad_vector(OriginalCoefficients, TotalColumns, PaddedCoefficients),
		add_auxiliary_columns(Sense, NextColumn0, PaddedCoefficients, Coefficients, Basic, NextColumn, RowArtificialColumns),
		build_tableau_rows(Rows, VariableCount, TotalColumns, NextColumn, Tableau, Basis, RemainingArtificialColumns),
		append(RowArtificialColumns, RemainingArtificialColumns, ArtificialColumns).

	pad_vector(Vector, Length, Padded) :-
		length(Vector, CurrentLength),
		Padding is Length - CurrentLength,
		new_vector(Padding, 0.0, Zeroes),
		append(Vector, Zeroes, Padded).

	add_auxiliary_columns((=<), NextColumn0, Coefficients0, Coefficients, NextColumn0, NextColumn, []) :-
		add_at(NextColumn0, 1, Coefficients0, Coefficients),
		NextColumn is NextColumn0 + 1.
	add_auxiliary_columns((>=), NextColumn0, Coefficients0, Coefficients, ArtificialColumn, NextColumn, [ArtificialColumn]) :-
		add_at(NextColumn0, -1, Coefficients0, Coefficients1),
		ArtificialColumn is NextColumn0 + 1,
		add_at(ArtificialColumn, 1, Coefficients1, Coefficients),
		NextColumn is ArtificialColumn + 1.
	add_auxiliary_columns((=), NextColumn0, Coefficients0, Coefficients, NextColumn0, NextColumn, [NextColumn0]) :-
		add_at(NextColumn0, 1, Coefficients0, Coefficients),
		NextColumn is NextColumn0 + 1.

	phase_one_costs(TotalColumns, ArtificialColumns, Costs) :-
		phase_one_costs(1, TotalColumns, ArtificialColumns, Costs).

	phase_one_costs(Index, TotalColumns, _ArtificialColumns, []) :-
		Index > TotalColumns,
		!.
	phase_one_costs(Index, TotalColumns, ArtificialColumns, [Cost| Costs]) :-
		(	member(Index, ArtificialColumns) ->
			Cost = -1
		;	Cost = 0
		),
		NextIndex is Index + 1,
		phase_one_costs(NextIndex, TotalColumns, ArtificialColumns, Costs).

	build_objective_row(Costs, Tableau, Basis, objective_row(ObjectiveCoefficients, ObjectiveValue)) :-
		scale_vector(Costs, -1, InitialCoefficients),
		canonicalize_objective(Tableau, Basis, Costs, InitialCoefficients, 0, ObjectiveCoefficients, ObjectiveValue).

	canonicalize_objective([], [], _Costs, Coefficients, Value, Coefficients, Value).
	canonicalize_objective([tableau_row(Row, RightHandSide)| Rows], [Basic| Basis], Costs, Coefficients0, Value0, Coefficients, Value) :-
		nth1(Basic, Costs, BasicCost),
		add_scaled_vector(Row, BasicCost, Coefficients0, Coefficients1),
		Value1 is Value0 + BasicCost * RightHandSide,
		canonicalize_objective(Rows, Basis, Costs, Coefficients1, Value1, Coefficients, Value).

	add_scaled_vector([], _Scale, [], []).
	add_scaled_vector([Value| Values], Scale, [Base| Bases], [Result| Results]) :-
		Result is Base + Scale * Value,
		add_scaled_vector(Values, Scale, Bases, Results).

	simplex_loop(Tableau, Basis, Objective, Forbidden, PivotRule, Tolerance, MaxIterations, Iterations0, Status, FinalTableau, FinalBasis, FinalObjective, Iterations) :-
		Objective = objective_row(ObjectiveCoefficients, _ObjectiveValue),
		(	entering_column(PivotRule, ObjectiveCoefficients, Forbidden, Tolerance, Entering) ->
			(	Iterations0 >= MaxIterations ->
				Status = iteration_limit,
				FinalTableau = Tableau,
				FinalBasis = Basis,
				FinalObjective = Objective,
				Iterations = Iterations0
			;	leaving_row(Tableau, Basis, Entering, Tolerance, Leaving) ->
				pivot(Tableau, Basis, Objective, Leaving, Entering, PivotedTableau, PivotedBasis, PivotedObjective),
				Iterations1 is Iterations0 + 1,
				simplex_loop(PivotedTableau, PivotedBasis, PivotedObjective, Forbidden, PivotRule, Tolerance, MaxIterations, Iterations1, Status, FinalTableau, FinalBasis, FinalObjective, Iterations)
			;	Status = unbounded,
				FinalTableau = Tableau,
				FinalBasis = Basis,
				FinalObjective = Objective,
				Iterations = Iterations0
			)
		;	Status = optimal,
			FinalTableau = Tableau,
			FinalBasis = Basis,
			FinalObjective = Objective,
			Iterations = Iterations0
		).

	entering_column(bland, Coefficients, Forbidden, Tolerance, Entering) :-
		bland_entering_column(Coefficients, Forbidden, Tolerance, 1, Entering).
	entering_column(dantzig, Coefficients, Forbidden, Tolerance, Entering) :-
		dantzig_entering_column(Coefficients, Forbidden, Tolerance, 1, none, Entering).

	bland_entering_column([], _Forbidden, _Tolerance, _Index, _Entering) :-
		fail.
	bland_entering_column([Coefficient| _Coefficients], Forbidden, Tolerance, Index, Index) :-
		Coefficient < -Tolerance,
		\+ member(Index, Forbidden),
		!.
	bland_entering_column([_Coefficient| Coefficients], Forbidden, Tolerance, Index, Entering) :-
		NextIndex is Index + 1,
		bland_entering_column(Coefficients, Forbidden, Tolerance, NextIndex, Entering).

	dantzig_entering_column([], _Forbidden, _Tolerance, _Index, none, _Entering) :-
		fail.
	dantzig_entering_column([], _Forbidden, _Tolerance, _Index, Entering- _Coefficient, Entering).
	dantzig_entering_column([Coefficient| Coefficients], Forbidden, Tolerance, Index, Best0, Entering) :-
		(	Coefficient < -Tolerance,
			\+ member(Index, Forbidden) ->
			better_dantzig_candidate(Best0, Index, Coefficient, Best)
		;	Best = Best0
		),
		NextIndex is Index + 1,
		dantzig_entering_column(Coefficients, Forbidden, Tolerance, NextIndex, Best, Entering).

	better_dantzig_candidate(none, Index, Coefficient, Index-Coefficient) :-
		!.
	better_dantzig_candidate(_BestIndex-BestCoefficient, Index, Coefficient, Index-Coefficient) :-
		Coefficient < BestCoefficient,
		!.
	better_dantzig_candidate(Best, _Index, _Coefficient, Best).

	leaving_row(Tableau, Basis, Entering, Tolerance, Leaving) :-
		leaving_row(Tableau, Basis, Entering, Tolerance, 1, none, Leaving- _Ratio- _Basic).

	leaving_row([], [], _Entering, _Tolerance, _RowIndex, Best, Best) :-
		Best \== none.
	leaving_row([tableau_row(Coefficients, RightHandSide)| Rows], [Basic| Basis], Entering, Tolerance, RowIndex, Best0, Best) :-
		nth1(Entering, Coefficients, PivotCoefficient),
		(	PivotCoefficient > Tolerance ->
			Ratio is RightHandSide / PivotCoefficient,
			better_leaving_candidate(Best0, RowIndex, Ratio, Basic, Tolerance, Best1)
		;	Best1 = Best0
		),
		NextRowIndex is RowIndex + 1,
		leaving_row(Rows, Basis, Entering, Tolerance, NextRowIndex, Best1, Best).

	better_leaving_candidate(none, RowIndex, Ratio, Basic, _Tolerance, RowIndex-Ratio-Basic).
	better_leaving_candidate(BestRow-BestRatio-BestBasic, RowIndex, Ratio, Basic, Tolerance, Best) :-
		(	Ratio < BestRatio - Tolerance ->
			Best = RowIndex-Ratio-Basic
		;	abs(Ratio - BestRatio) =< Tolerance, Basic < BestBasic ->
			Best = RowIndex-Ratio-Basic
		;	Best = BestRow-BestRatio-BestBasic
		).

	pivot(Tableau, Basis, Objective, Leaving, Entering, PivotedTableau, PivotedBasis, PivotedObjective) :-
		nth1(Leaving, Tableau, PivotRow0),
		PivotRow0 = tableau_row(PivotCoefficients0, _PivotRightHandSide0),
		nth1(Entering, PivotCoefficients0, PivotValue),
		scale_tableau_row(PivotRow0, PivotValue, PivotRow),
		eliminate_tableau(Tableau, Leaving, Entering, PivotRow, 1, PivotedTableau),
		replace_at(Leaving, Entering, Basis, PivotedBasis),
		eliminate_objective(Objective, Entering, PivotRow, PivotedObjective).

	scale_tableau_row(tableau_row(Coefficients, RightHandSide), Scale, tableau_row(ScaledCoefficients, ScaledRightHandSide)) :-
		InverseScale is 1.0 / Scale,
		scale_vector(Coefficients, InverseScale, ScaledCoefficients),
		ScaledRightHandSide is RightHandSide * InverseScale.

	eliminate_tableau([], _Leaving, _Entering, _PivotRow, _RowIndex, []).
	eliminate_tableau([Row| Rows], Leaving, Entering, PivotRow, RowIndex, [NewRow| NewRows]) :-
		(	RowIndex =:= Leaving ->
			NewRow = PivotRow
		;	eliminate_row(Row, Entering, PivotRow, NewRow)
		),
		NextRowIndex is RowIndex + 1,
		eliminate_tableau(Rows, Leaving, Entering, PivotRow, NextRowIndex, NewRows).

	eliminate_row(tableau_row(Coefficients, RightHandSide), Entering, tableau_row(PivotCoefficients, PivotRightHandSide), tableau_row(NewCoefficients, NewRightHandSide)) :-
		nth1(Entering, Coefficients, Factor),
		Scale is -Factor,
		add_scaled_vector(PivotCoefficients, Scale, Coefficients, NewCoefficients),
		NewRightHandSide is RightHandSide + Scale * PivotRightHandSide.

	eliminate_objective(objective_row(Coefficients, Value), Entering, tableau_row(PivotCoefficients, PivotRightHandSide), objective_row(NewCoefficients, NewValue)) :-
		nth1(Entering, Coefficients, Factor),
		Scale is -Factor,
		add_scaled_vector(PivotCoefficients, Scale, Coefficients, NewCoefficients),
		NewValue is Value + Scale * PivotRightHandSide.

	replace_at(1, Value, [_Old| Values], [Value| Values]) :-
		!.
	replace_at(Index, Value, [Old| Values], [Old| NewValues]) :-
		NextIndex is Index - 1,
		replace_at(NextIndex, Value, Values, NewValues).

	finish_phase_one(iteration_limit, _Objective, Tableau, Basis, _ArtificialColumns, _Tolerance, iteration_limit, Tableau, Basis).
	finish_phase_one(unbounded, _Objective, Tableau, Basis, _ArtificialColumns, _Tolerance, numerical_error, Tableau, Basis).
	finish_phase_one(optimal, objective_row(_Coefficients, Value), Tableau0, Basis0, ArtificialColumns, Tolerance, Status, Tableau, Basis) :-
		( Value < -Tolerance ->
			Status = infeasible,
			Tableau = Tableau0,
			Basis = Basis0
		; remove_artificial_basics(Tableau0, Basis0, ArtificialColumns, Tolerance, Status, Tableau, Basis)
		).

	remove_artificial_basics(Tableau0, Basis0, ArtificialColumns, Tolerance, Status, Tableau, Basis) :-
		( artificial_basic(Basis0, ArtificialColumns, 1, RowIndex) ->
			nth1(RowIndex, Tableau0, tableau_row(Coefficients, RightHandSide)),
			(	replacement_column(Coefficients, ArtificialColumns, Tolerance, 1, Entering) ->
				zero_vector_for_objective(Coefficients, Objective),
				pivot(Tableau0, Basis0, Objective, RowIndex, Entering, Tableau1, Basis1, _),
				remove_artificial_basics(Tableau1, Basis1, ArtificialColumns, Tolerance, Status, Tableau, Basis)
			;	abs(RightHandSide) =< Tolerance ->
				remove_at(RowIndex, Tableau0, Tableau1),
				remove_at(RowIndex, Basis0, Basis1),
				remove_artificial_basics(Tableau1, Basis1, ArtificialColumns, Tolerance, Status, Tableau, Basis)
			;	Status = numerical_error,
				Tableau = Tableau0,
				Basis = Basis0
			)
		;	Status = ready,
			Tableau = Tableau0,
			Basis = Basis0
		).

	artificial_basic([], _ArtificialColumns, _RowIndex, _Found) :-
		fail.
	artificial_basic([Basic| _Basis], ArtificialColumns, RowIndex, RowIndex) :-
		member(Basic, ArtificialColumns),
		!.
	artificial_basic([_Basic| Basis], ArtificialColumns, RowIndex, Found) :-
		NextRowIndex is RowIndex + 1,
		artificial_basic(Basis, ArtificialColumns, NextRowIndex, Found).

	replacement_column([], _ArtificialColumns, _Tolerance, _Index, _Entering) :-
		fail.
	replacement_column([Coefficient| _Coefficients], ArtificialColumns, Tolerance, Index, Index) :-
		abs(Coefficient) > Tolerance,
		\+ member(Index, ArtificialColumns),
		!.
	replacement_column([_Coefficient| Coefficients], ArtificialColumns, Tolerance, Index, Entering) :-
		NextIndex is Index + 1,
		replacement_column(Coefficients, ArtificialColumns, Tolerance, NextIndex, Entering).

	zero_vector_for_objective(Coefficients, objective_row(Zeroes, 0)) :-
		new_vector_like(Coefficients, Zeroes).

	remove_at(1, [_Element| Elements], Elements) :-
		!.
	remove_at(Index, [Element| Elements], [Element| Remaining]) :-
		NextIndex is Index - 1,
		remove_at(NextIndex, Elements, Remaining).

	continue_with_phase_two(iteration_limit, _Problem, _Recoveries, _VariableCount, _ObjectiveCoefficients, _Tableau, _Basis, _ArtificialColumns, _PivotRule, _Tolerance, _MaxIterations, PhaseOneIterations, linear_programming_result(iteration_limit, none, [], [iterations(PhaseOneIterations),phase_one_iterations(PhaseOneIterations),phase_two_iterations(0)])).
	continue_with_phase_two(infeasible, _Problem, _Recoveries, _VariableCount, _ObjectiveCoefficients, _Tableau, _Basis, _ArtificialColumns, _PivotRule, _Tolerance, _MaxIterations, PhaseOneIterations, linear_programming_result(infeasible, none, [], [iterations(PhaseOneIterations),phase_one_iterations(PhaseOneIterations),phase_two_iterations(0)])).
	continue_with_phase_two(numerical_error, _Problem, _Recoveries, _VariableCount, _ObjectiveCoefficients, _Tableau, _Basis, _ArtificialColumns, _PivotRule, _Tolerance, _MaxIterations, PhaseOneIterations, linear_programming_result(numerical_error, none, [], [iterations(PhaseOneIterations),phase_one_iterations(PhaseOneIterations),phase_two_iterations(0)])).
	continue_with_phase_two(ready, Problem, Recoveries, VariableCount, ObjectiveCoefficients, Tableau0, Basis0, ArtificialColumns, PivotRule, Tolerance, MaxIterations, PhaseOneIterations, Result) :-
		Tableau0 = [tableau_row(AllCoefficients, _)| _],
		!,
		length(AllCoefficients, TotalColumns),
		pad_vector(ObjectiveCoefficients, TotalColumns, PhaseTwoCosts),
		build_objective_row(PhaseTwoCosts, Tableau0, Basis0, PhaseTwoObjective0),
		simplex_loop(Tableau0, Basis0, PhaseTwoObjective0, ArtificialColumns, PivotRule, Tolerance, MaxIterations, PhaseOneIterations, PhaseTwoStatus, Tableau, Basis, _PhaseTwoObjective, TotalIterations),
		PhaseTwoIterations is TotalIterations - PhaseOneIterations,
		finish_result(PhaseTwoStatus, Problem, Recoveries, VariableCount, Tableau, Basis, TotalIterations, PhaseOneIterations, PhaseTwoIterations, Result).
	continue_with_phase_two(ready, Problem, Recoveries, VariableCount, ObjectiveCoefficients, [], [], ArtificialColumns, PivotRule, Tolerance, MaxIterations, PhaseOneIterations, Result) :-
		!,
		build_objective_row(ObjectiveCoefficients, [], [], PhaseTwoObjective0),
		simplex_loop([], [], PhaseTwoObjective0, ArtificialColumns, PivotRule, Tolerance, MaxIterations, PhaseOneIterations, PhaseTwoStatus, Tableau, Basis, _PhaseTwoObjective, TotalIterations),
		PhaseTwoIterations is TotalIterations - PhaseOneIterations,
		finish_result(PhaseTwoStatus, Problem, Recoveries, VariableCount, Tableau, Basis, TotalIterations, PhaseOneIterations, PhaseTwoIterations, Result).

	finish_result(optimal, Problem, Recoveries, VariableCount, Tableau, Basis, TotalIterations, PhaseOneIterations, PhaseTwoIterations, linear_programming_result(optimal, ObjectiveValue, Values, Statistics)) :-
		!,
		column_values(VariableCount, Tableau, Basis, TransformedValues),
		recover_values(Recoveries, TransformedValues, Values),
		Problem = linear_program(_Variables, _Constraints, objective(Expression, _Sense)),
		evaluate_expression(Expression, Values, 0, ObjectiveValue),
		Statistics = [iterations(TotalIterations),phase_one_iterations(PhaseOneIterations),phase_two_iterations(PhaseTwoIterations)].
	finish_result(Status, _Problem, _Recoveries, _VariableCount, _Tableau, _Basis, TotalIterations, PhaseOneIterations, PhaseTwoIterations, linear_programming_result(Status, none, [], Statistics)) :-
		Status \== optimal,
		Statistics = [iterations(TotalIterations),phase_one_iterations(PhaseOneIterations),phase_two_iterations(PhaseTwoIterations)].

	column_values(VariableCount, Tableau, Basis, Values) :-
		column_values(1, VariableCount, Tableau, Basis, Values).

	column_values(Index, VariableCount, _Tableau, _Basis, []) :-
		Index > VariableCount,
		!.
	column_values(Index, VariableCount, Tableau, Basis, [Value| Values]) :-
		( basic_row(Index, Basis, 1, RowIndex) ->
			nth1(RowIndex, Tableau, tableau_row(_Coefficients, Value))
		; Value = 0
		),
		NextIndex is Index + 1,
		column_values(NextIndex, VariableCount, Tableau, Basis, Values).

	basic_row(_Column, [], _RowIndex, _Found) :-
		fail.
	basic_row(Column, [Basic| _Basis], RowIndex, RowIndex) :-
		Column =:= Basic,
		!.
	basic_row(Column, [_Basic| Basis], RowIndex, Found) :-
		NextRowIndex is RowIndex + 1,
		basic_row(Column, Basis, NextRowIndex, Found).

	recover_values([], _TransformedValues, []).
	recover_values([recovery(Name, Constant, Terms)| Recoveries], TransformedValues, [Name-Value| Values]) :-
		evaluate_transformed_terms(Terms, TransformedValues, Constant, Value),
		recover_values(Recoveries, TransformedValues, Values).

	evaluate_transformed_terms([], _Values, Value, Value).
	evaluate_transformed_terms([Coefficient*Column| Terms], Values, Value0, Value) :-
		nth1(Column, Values, ColumnValue),
		Value1 is Value0 + Coefficient * ColumnValue,
		evaluate_transformed_terms(Terms, Values, Value1, Value).

	evaluate_expression([], _Values, Value, Value).
	evaluate_expression([Coefficient*Name| Expression], Values, Value0, Value) :-
		memberchk(Name-VariableValue, Values),
		Value1 is Value0 + Coefficient * VariableValue,
		evaluate_expression(Expression, Values, Value1, Value).

:- end_object.
