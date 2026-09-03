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


:- category(linear_programming_common,
	implements(linear_programming_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Shared immutable model construction, validation, matrix conversion, result inspection, and printing for linear-programming solvers.'
	]).

	:- protected(check_problem/1).
	:- mode(check_problem(@term), one_or_error).
	:- info(check_problem/1, [
		comment is 'Checks that the argument is a structurally valid linear-program problem.',
		argnames is ['Problem']
	]).

	:- protected(check_result/1).
	:- mode(check_result(@term), one_or_error).
	:- info(check_result/1, [
		comment is 'Checks that the argument is a structurally valid linear-programming result.',
		argnames is ['Result']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	new_problem(linear_program([], [], none)).

	variable(Name, Type, Problem0, Problem) :-
		(	Type == binary ->
			Upper = 1
		;	Upper = inf
		),
		::variable(Name, Type, 0, Upper, Problem0, Problem).

	variable(Name, Type, Lower, Upper, Problem0, linear_program(Variables, Constraints, Objective)) :-
		check_problem(Problem0),
		Problem0 = linear_program(Variables0, Constraints, Objective),
		check_variable_name(Name),
		check_variable_type(Type),
		check_bounds(Type, Lower, Upper),
		(	variable_declared(Name, Variables0) ->
			domain_error(linear_programming_variable, Name)
		;	append_variable(Variables0, variable(Name, Type, Lower, Upper), Variables)
		).

	constraint(Coefficients, Sense, RightHandSide, Problem0, linear_program(Variables, Constraints, Objective)) :-
		check_problem(Problem0),
		Problem0 = linear_program(Variables, Constraints0, Objective),
		check_constraint_sense(Sense),
		check_number(RightHandSide),
		canonical_expression(Coefficients, Variables, Canonical),
		append_constraint(Constraints0, constraint(Canonical, Sense, RightHandSide), Constraints).

	constraints(Constraints, Problem0, Problem) :-
		(	var(Constraints) ->
			instantiation_error
		;	constraints_(Constraints, Problem0, Problem) ->
			true
		;	type_error(linear_programming_constraints, Constraints)
		).

	constraints_([], Problem, Problem).
	constraints_([constraint(Coefficients, Sense, RightHandSide)| Constraints], Problem0, Problem) :-
		::constraint(Coefficients, Sense, RightHandSide, Problem0, Problem1),
		constraints_(Constraints, Problem1, Problem).

	objective(Coefficients, Sense, Problem0, linear_program(Variables, Constraints, objective(Canonical, Sense))) :-
		check_problem(Problem0),
		Problem0 = linear_program(Variables, Constraints, Objective0),
		(	Objective0 == none ->
			true
		;	domain_error(linear_programming_objective, Problem0)
		),
		check_objective_sense(Sense),
		canonical_expression(Coefficients, Variables, Canonical).

	problem_from_matrices(Objective, ObjectiveSense, EqualityMatrix, EqualityRightHandSide, InequalityMatrix, InequalityRightHandSide, Bounds, Problem) :-
		check_numeric_vector(Objective, Objective),
		length(Objective, VariableCount),
		check_matrix(EqualityMatrix, EqualityMatrix, VariableCount),
		check_numeric_vector(EqualityRightHandSide, EqualityRightHandSide),
		length(EqualityMatrix, EqualityCount),
		check_length(EqualityRightHandSide, EqualityCount, Objective),
		check_matrix(InequalityMatrix, InequalityMatrix, VariableCount),
		check_numeric_vector(InequalityRightHandSide, InequalityRightHandSide),
		length(InequalityMatrix, InequalityCount),
		check_length(InequalityRightHandSide, InequalityCount, Objective),
		check_matrix_bounds(Bounds, VariableCount),
		new_problem(Problem0),
		matrix_variables(Bounds, 1, Problem0, Problem1),
		indexed_expression(Objective, 1, ObjectiveExpression),
		::objective(ObjectiveExpression, ObjectiveSense, Problem1, Problem2),
		matrix_constraints(EqualityMatrix, EqualityRightHandSide, (=), Problem2, Problem3),
		matrix_constraints(InequalityMatrix, InequalityRightHandSide, (=<), Problem3, Problem).

	status(Result, Status) :-
		check_result(Result),
		Result = linear_programming_result(Status, _Objective, _Values, _Statistics).

	objective_value(Result, Value) :-
		check_result(Result),
		Result = linear_programming_result(optimal, Value, _Values, _Statistics).

	variable_value(Result, Variable, Value) :-
		check_result(Result),
		Result = linear_programming_result(optimal, _Objective, Values, _Statistics),
		memberchk(Variable-Value, Values).

	statistics(Result, Statistics) :-
		check_result(Result),
		Result = linear_programming_result(_Status, _Objective, _Values, Statistics).

	print_problem(Problem) :-
		check_problem(Problem),
		Problem = linear_program(Variables, Constraints, Objective),
		format('Variables: ~w~n', [Variables]),
		format('Constraints: ~w~n', [Constraints]),
		format('Objective: ~w~n', [Objective]).

	print_solution(Result) :-
		check_result(Result),
		Result = linear_programming_result(Status, Objective, Values, Statistics),
		format('Status: ~w~n', [Status]),
		format('Objective: ~w~n', [Objective]),
		format('Values: ~w~n', [Values]),
		format('Statistics: ~w~n', [Statistics]).

	check_problem(Problem) :-
		(	var(Problem) ->
			instantiation_error
		;	valid_problem(Problem) ->
			true
		;	type_error(linear_program, Problem)
		).

	check_result(Result) :-
		(	var(Result) ->
			instantiation_error
		;	valid_result(Result) ->
			true
		;	type_error(linear_programming_result, Result)
		).

	valid_problem(linear_program(Variables, Constraints, Objective)) :-
		ground(Variables-Constraints-Objective),
		valid_variables(Variables),
		valid_constraints(Constraints, Variables),
		valid_objective(Objective, Variables).

	valid_variables([]).
	valid_variables([variable(Name, Type, Lower, Upper)| Variables]) :-
		ground(Name),
		valid_variable_type(Type),
		valid_bounds(Type, Lower, Upper),
		\+ variable_declared(Name, Variables),
		valid_variables(Variables).

	valid_constraints([], _Variables).
	valid_constraints([constraint(Expression, Sense, RightHandSide)| Constraints], Variables) :-
		valid_canonical_expression(Expression, Variables),
		valid_constraint_sense(Sense),
		number(RightHandSide),
		valid_constraints(Constraints, Variables).

	valid_objective(none, _Variables).
	valid_objective(objective(Expression, Sense), Variables) :-
		valid_canonical_expression(Expression, Variables),
		valid_objective_sense(Sense).

	valid_result(linear_programming_result(Status, Objective, Values, Statistics)) :-
		valid_status(Status),
		valid_result_data(Status, Objective, Values),
		ground(Statistics),
		valid_statistics(Statistics).

	valid_statistics([]).
	valid_statistics([_Statistic| Statistics]) :-
		valid_statistics(Statistics).

	valid_status(optimal).
	valid_status(infeasible).
	valid_status(unbounded).
	valid_status(iteration_limit).
	valid_status(numerical_error).

	valid_result_data(optimal, Objective, Values) :-
		number(Objective),
		valid_values(Values).
	valid_result_data(Status, none, []) :-
		Status \== optimal.

	valid_values([]).
	valid_values([Name-Value| Values]) :-
		ground(Name),
		number(Value),
		valid_values(Values).

	check_variable_name(Name) :-
		(	var(Name) ->
			instantiation_error
		;	ground(Name) ->
			true
		;	instantiation_error
		).

	check_variable_type(Type) :-
		(	var(Type) ->
			instantiation_error
		;	valid_variable_type(Type) ->
			true
		;	domain_error(linear_programming_variable_type, Type)
		).

	valid_variable_type(continuous).
	valid_variable_type(integer).
	valid_variable_type(binary).

	check_bounds(Type, Lower, Upper) :-
		(	var(Lower) ->
			instantiation_error
		;	var(Upper) ->
			instantiation_error
		;	valid_bounds(Type, Lower, Upper) ->
			true
		;	domain_error(linear_programming_bounds, Lower-Upper)
		).

	valid_bounds(binary, Lower, Upper) :-
		number(Lower), number(Upper),
		Lower >= 0, Upper =< 1, Lower =< Upper.
	valid_bounds(Type, Lower, Upper) :-
		Type \== binary,
		valid_lower_bound(Lower),
		valid_upper_bound(Upper),
		ordered_bounds(Lower, Upper).

	valid_lower_bound(Lower) :-
		number(Lower).
	valid_lower_bound(Lower) :-
		Lower == -inf.

	valid_upper_bound(Upper) :-
		number(Upper).
	valid_upper_bound(Upper) :-
		Upper == inf.

	ordered_bounds(Lower, Upper) :-
		(	Lower == -inf ->
			true
		;	Upper == inf ->
			true
		;	Lower =< Upper
		).

	check_constraint_sense(Sense) :-
		(	var(Sense) ->
			instantiation_error
		;	valid_constraint_sense(Sense) ->
			true
		;	domain_error(linear_programming_constraint_sense, Sense)
		).

	valid_constraint_sense(=<).
	valid_constraint_sense(>=).
	valid_constraint_sense(=).

	check_objective_sense(Sense) :-
		(	var(Sense) ->
			instantiation_error
		;	valid_objective_sense(Sense) ->
			true
		;	domain_error(linear_programming_objective_sense, Sense)
		).

	valid_objective_sense(minimize).
	valid_objective_sense(maximize).

	check_number(Number) :-
		(	var(Number) ->
			instantiation_error
		;	number(Number) ->
			true
		;	type_error(number, Number)
		).

	canonical_expression(Expression, Variables, Canonical) :-
		(	var(Expression) ->
			instantiation_error
		;	valid_expression_terms(Expression, Variables) ->
			canonical_coefficients(Variables, Expression, Canonical)
		;	type_error(linear_expression, Expression)
		).

	valid_expression_terms([], _Variables).
	valid_expression_terms([Coefficient*Variable| Terms], Variables) :-
		number(Coefficient),
		ground(Variable),
		(	variable_declared(Variable, Variables) ->
			valid_expression_terms(Terms, Variables)
		;	domain_error(linear_programming_variable, Variable)
		).

	canonical_coefficients([], _Expression, []).
	canonical_coefficients([variable(Name, _Type, _Lower, _Upper)| Variables], Expression, Canonical) :-
		sum_variable_coefficients(Expression, Name, 0, Coefficient),
		(	Coefficient =:= 0 ->
			Canonical = Rest
		;	Canonical = [Coefficient*Name| Rest]
		),
		canonical_coefficients(Variables, Expression, Rest).

	sum_variable_coefficients([], _Name, Sum, Sum).
	sum_variable_coefficients([Coefficient*Variable| Terms], Name, Sum0, Sum) :-
		(	Variable == Name ->
			Sum1 is Sum0 + Coefficient
		;	Sum1 = Sum0
		),
		sum_variable_coefficients(Terms, Name, Sum1, Sum).

	valid_canonical_expression([], _Variables).
	valid_canonical_expression([Coefficient*Name| Expression], Variables) :-
		number(Coefficient),
		Coefficient =\= 0,
		variable_declared(Name, Variables),
		valid_canonical_expression(Expression, Variables).

	variable_declared(Name, [variable(Declared, _Type, _Lower, _Upper)| _Variables]) :-
		Name == Declared,
		!.
	variable_declared(Name, [_Variable| Variables]) :-
		variable_declared(Name, Variables).

	append_variable([], Variable, [Variable]).
	append_variable([Variable| Variables], NewVariable, [Variable| NewVariables]) :-
		append_variable(Variables, NewVariable, NewVariables).

	append_constraint([], Constraint, [Constraint]).
	append_constraint([Constraint| Constraints], NewConstraint, [Constraint| NewConstraints]) :-
		append_constraint(Constraints, NewConstraint, NewConstraints).

	check_numeric_vector(Vector, Culprit) :-
		(	var(Vector) ->
			instantiation_error
		;	valid_numeric_vector(Vector) ->
			true
		;	type_error(linear_programming_matrix_problem, Culprit)
		).

	valid_numeric_vector([]).
	valid_numeric_vector([Number| Numbers]) :-
		number(Number),
		valid_numeric_vector(Numbers).

	check_matrix(Matrix, Matrix, ColumnCount) :-
		(	var(Matrix) ->
			instantiation_error
		;	valid_numeric_matrix(Matrix) ->
			check_matrix_columns(Matrix, ColumnCount, Matrix)
		;	type_error(linear_programming_matrix_problem, Matrix)
		).

	check_matrix_columns([], _ColumnCount, _Matrix).
	check_matrix_columns([Row| Rows], ColumnCount, Matrix) :-
		(	length(Row, ColumnCount) ->
			check_matrix_columns(Rows, ColumnCount, Matrix)
		;	domain_error(linear_programming_dimensions, Matrix)
		).

	check_length(List, Length, Culprit) :-
		(	length(List, Length) ->
			true
		;	domain_error(linear_programming_dimensions, Culprit)
		).

	valid_numeric_matrix([]).
	valid_numeric_matrix([Row| Rows]) :-
		valid_numeric_vector(Row),
		valid_numeric_matrix(Rows).

	check_matrix_bounds(Bounds, VariableCount) :-
		(	var(Bounds) ->
			instantiation_error
		;	valid_matrix_bounds(Bounds) ->
			check_length(Bounds, VariableCount, Bounds)
		;	type_error(linear_programming_matrix_problem, Bounds)
		).

	valid_matrix_bounds([]).
	valid_matrix_bounds([Lower-Upper| Bounds]) :-
		valid_bounds(continuous, Lower, Upper),
		valid_matrix_bounds(Bounds).

	matrix_variables([], _Index, Problem, Problem).
	matrix_variables([Lower-Upper| Bounds], Index, Problem0, Problem) :-
		::variable(Index, continuous, Lower, Upper, Problem0, Problem1),
		NextIndex is Index + 1,
		matrix_variables(Bounds, NextIndex, Problem1, Problem).

	indexed_expression([], _Index, []).
	indexed_expression([Coefficient| Coefficients], Index, [Coefficient*Index| Expression]) :-
		NextIndex is Index + 1,
		indexed_expression(Coefficients, NextIndex, Expression).

	matrix_constraints([], [], _Sense, Problem, Problem).
	matrix_constraints([Row| Rows], [RightHandSide| RightHandSides], Sense, Problem0, Problem) :-
		indexed_expression(Row, 1, Expression),
		::constraint(Expression, Sense, RightHandSide, Problem0, Problem1),
		matrix_constraints(Rows, RightHandSides, Sense, Problem1, Problem).

:- end_category.
