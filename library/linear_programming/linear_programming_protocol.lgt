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


:- protocol(linear_programming_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Protocol for immutable linear-program construction, solving, and result inspection.'
	]).

	:- public(new_problem/1).
	:- mode(new_problem(-compound), one).
	:- info(new_problem/1, [
		comment is 'Creates an empty linear-program problem.',
		argnames is ['Problem']
	]).

	:- public(variable/4).
	:- mode(variable(+term, +atom, +compound, -compound), one_or_error).
	:- info(variable/4, [
		comment is 'Adds a variable with default bounds. Continuous and integer variables default to zero and positive infinity; binary variables default to zero and one. The variable type is ``continuous``, ``integer``, or ``binary``.',
		argnames is ['Name', 'Type', 'Problem0', 'Problem'],
		exceptions is [
			'An argument is insufficiently instantiated' - instantiation_error,
			'``Problem0`` is not a linear-program problem' - type_error(linear_program, 'Problem0'),
			'``Type`` is not a supported variable type' - domain_error(linear_programming_variable_type, 'Type'),
			'``Name`` is already declared' - domain_error(linear_programming_variable, 'Name')
		]
	]).

	:- public(variable/6).
	:- mode(variable(+term, +atom, +term, +term, +compound, -compound), one_or_error).
	:- info(variable/6, [
		comment is 'Adds a variable with explicit lower and upper bounds. A bound is a number, ``inf``, or ``-inf`` as appropriate.',
		argnames is ['Name', 'Type', 'Lower', 'Upper', 'Problem0', 'Problem'],
		exceptions is [
			'An argument is insufficiently instantiated' - instantiation_error,
			'``Problem0`` is not a linear-program problem' - type_error(linear_program, 'Problem0'),
			'``Type`` is not a supported variable type' - domain_error(linear_programming_variable_type, 'Type'),
			'``Name`` is already declared' - domain_error(linear_programming_variable, 'Name'),
			'The bounds are invalid or inconsistent' - domain_error(linear_programming_bounds, 'Lower'-'Upper')
		]
	]).

	:- public(constraint/5).
	:- mode(constraint(+list(compound), +atom, +number, +compound, -compound), one_or_error).
	:- info(constraint/5, [
		comment is 'Adds a linear constraint. ``Coefficients`` is a list of ``Coefficient*Variable`` terms and ``Sense`` is ``=<``, ``>=``, or ``=``.',
		argnames is ['Coefficients', 'Sense', 'RightHandSide', 'Problem0', 'Problem'],
		exceptions is [
			'An argument is insufficiently instantiated' - instantiation_error,
			'``Problem0`` is not a linear-program problem' - type_error(linear_program, 'Problem0'),
			'``Coefficients`` is not a list of coefficient-variable terms' - type_error(linear_expression, 'Coefficients'),
			'``Sense`` is not a supported constraint sense' - domain_error(linear_programming_constraint_sense, 'Sense'),
			'A coefficient references an undeclared variable' - domain_error(linear_programming_variable, 'Variable')
		]
	]).

	:- public(constraints/3).
	:- mode(constraints(+list(compound), +compound, -compound), one_or_error).
	:- info(constraints/3, [
		comment is 'Adds a list of ``constraint(Coefficients, Sense, RightHandSide)`` terms.',
		argnames is ['Constraints', 'Problem0', 'Problem'],
		exceptions is [
			'An argument is insufficiently instantiated' - instantiation_error,
			'``Constraints`` is not a list of valid constraint terms' - type_error(linear_programming_constraints, 'Constraints'),
			'``Problem0`` is not a linear-program problem' - type_error(linear_program, 'Problem0')
		]
	]).

	:- public(objective/4).
	:- mode(objective(+list(compound), +atom, +compound, -compound), one_or_error).
	:- info(objective/4, [
		comment is 'Sets the linear objective. ``Coefficients`` is a list of ``Coefficient*Variable`` terms and ``Sense`` is ``minimize`` or ``maximize``.',
		argnames is ['Coefficients', 'Sense', 'Problem0', 'Problem'],
		exceptions is [
			'An argument is insufficiently instantiated' - instantiation_error,
			'``Problem0`` is not a linear-program problem' - type_error(linear_program, 'Problem0'),
			'``Coefficients`` is not a list of coefficient-variable terms' - type_error(linear_expression, 'Coefficients'),
			'``Sense`` is not a supported objective sense' - domain_error(linear_programming_objective_sense, 'Sense'),
			'The problem already has an objective' - domain_error(linear_programming_objective, 'Problem0'),
			'A coefficient references an undeclared variable' - domain_error(linear_programming_variable, 'Variable')
		]
	]).

	:- public(problem_from_matrices/8).
	:- mode(problem_from_matrices(+list(number), +atom, +list(list(number)), +list(number), +list(list(number)), +list(number), +list(pair), -compound), one_or_error).
	:- info(problem_from_matrices/8, [
		comment is 'Creates a problem from a dense objective vector, objective sense, equality rows and right-hand sides, less-than-or-equal rows and right-hand sides, and variable bounds. Variables are named by their one-based column indices.',
		argnames is ['Objective', 'ObjectiveSense', 'EqualityMatrix', 'EqualityRightHandSide', 'InequalityMatrix', 'InequalityRightHandSide', 'Bounds', 'Problem'],
		exceptions is [
			'An argument is insufficiently instantiated' - instantiation_error,
			'The matrix and vector dimensions are inconsistent' - domain_error(linear_programming_dimensions, 'Objective'),
			'An input is not a numeric vector, numeric matrix, or valid bounds list' - type_error(linear_programming_matrix_problem, 'Objective')
		]
	]).

	:- public(solve/2).
	:- mode(solve(+compound, -compound), one_or_error).
	:- info(solve/2, [
		comment is 'Solves a linear-program problem using default options and returns a result term. Solver statuses include ``optimal``, ``infeasible``, ``unbounded``, ``iteration_limit``, ``node_limit``, and ``numerical_error`` as applicable to the backend.',
		argnames is ['Problem', 'Result'],
		exceptions is [
			'``Problem`` is a variable' - instantiation_error,
			'``Problem`` is not a linear-program problem' - type_error(linear_program, 'Problem'),
			'``Problem`` has no variables' - domain_error(linear_programming_problem, empty),
			'``Problem`` has no objective' - domain_error(linear_programming_problem, missing_objective),
			'``Problem`` contains a variable type unsupported by the backend' - domain_error(simplex_variable_type, 'Variable'-'Type'),
			'A discrete variable does not have finite bounds' - domain_error(milp_finite_integer_bounds, 'Variable'-('Lower'-'Upper')),
			'A discrete variable domain contains no integer' - domain_error(milp_integer_domain, 'Variable'-('Lower'-'Upper'))
		]
	]).

	:- public(solve/3).
	:- mode(solve(+compound, -compound, +list(compound)), one_or_error).
	:- info(solve/3, [
		comment is 'Solves a linear-program problem using the specified options and returns a result term. Solver statuses include ``optimal``, ``infeasible``, ``unbounded``, ``iteration_limit``, ``node_limit``, and ``numerical_error`` as applicable to the backend.',
		argnames is ['Problem', 'Result', 'Options'],
		exceptions is [
			'``Problem`` is a variable' - instantiation_error,
			'``Problem`` is not a linear-program problem' - type_error(linear_program, 'Problem'),
			'``Problem`` has no variables' - domain_error(linear_programming_problem, empty),
			'``Problem`` has no objective' - domain_error(linear_programming_problem, missing_objective),
			'``Problem`` contains a variable type unsupported by the backend' - domain_error(simplex_variable_type, 'Variable'-'Type'),
			'A discrete variable does not have finite bounds' - domain_error(milp_finite_integer_bounds, 'Variable'-('Lower'-'Upper')),
			'A discrete variable domain contains no integer' - domain_error(milp_integer_domain, 'Variable'-('Lower'-'Upper')),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(status/2).
	:- mode(status(+compound, -atom), one_or_error).
	:- info(status/2, [
		comment is 'Returns the result status.',
		argnames is ['Result', 'Status'],
		exceptions is [
			'``Result`` is a variable' - instantiation_error,
			'``Result`` is not a linear-programming result' - type_error(linear_programming_result, 'Result')
		]
	]).

	:- public(objective_value/2).
	:- mode(objective_value(+compound, -number), zero_or_one_or_error).
	:- info(objective_value/2, [
		comment is 'Returns the optimal objective value. Fails when the result status is not ``optimal``.',
		argnames is ['Result', 'Value'],
		exceptions is [
			'``Result`` is a variable' - instantiation_error,
			'``Result`` is not a linear-programming result' - type_error(linear_programming_result, 'Result')
		]
	]).

	:- public(variable_value/3).
	:- mode(variable_value(+compound, +term, -number), zero_or_one_or_error).
	:- info(variable_value/3, [
		comment is 'Returns an optimal variable value. Fails when the result status is not ``optimal`` or the variable is absent.',
		argnames is ['Result', 'Variable', 'Value'],
		exceptions is [
			'``Result`` is a variable' - instantiation_error,
			'``Result`` is not a linear-programming result' - type_error(linear_programming_result, 'Result')
		]
	]).

	:- public(statistics/2).
	:- mode(statistics(+compound, -list(compound)), one_or_error).
	:- info(statistics/2, [
		comment is 'Returns solver statistics stored in a result.',
		argnames is ['Result', 'Statistics'],
		exceptions is [
			'``Result`` is a variable' - instantiation_error,
			'``Result`` is not a linear-programming result' - type_error(linear_programming_result, 'Result')
		]
	]).

	:- public(print_problem/1).
	:- mode(print_problem(+compound), one_or_error).
	:- info(print_problem/1, [
		comment is 'Prints a linear-program problem to the current output stream.',
		argnames is ['Problem'],
		exceptions is [
			'``Problem`` is a variable' - instantiation_error,
			'``Problem`` is not a linear-program problem' - type_error(linear_program, 'Problem')
		]
	]).

	:- public(print_solution/1).
	:- mode(print_solution(+compound), one_or_error).
	:- info(print_solution/1, [
		comment is 'Prints a linear-program result to the current output stream.',
		argnames is ['Result'],
		exceptions is [
			'``Result`` is a variable' - instantiation_error,
			'``Result`` is not a linear-programming result' - type_error(linear_programming_result, 'Result')
		]
	]).

:- end_protocol.
