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


:- object(external_tests(_Solver_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-27,
		comment is 'Unit tests for the external solver backends of the "linear_programming" library (conditional on the solver executable being available).',
		parnames is ['Solver']
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(os, [
		resolve_command_path/2
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2, nth1/3
	]).

	:- uses(numberlist, [
		max/2, min/2, sum/2
	]).

	cover(glpk).
	cover(cbc).
	cover(highs).
	cover(scip).
	cover(linear_programming_cli).

	% linear programs

	test(lp_classic_example, deterministic, [condition(solver_available)]) :-
		build([v(x, continuous, 0, inf), v(y, continuous, 0, inf)], [c([1*x, 2*y], =<, 14), c([3*x, -1*y], >=, 0), c([1*x, -1*y], =<, 2)], o([3*x, 4*y], maximize), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Objective, _, Statistics),
		assertion(Status == optimal),
		assertion(close(Objective, 34)),
		assertion(value(Result, x, 6)),
		assertion(value(Result, y, 4)),
		assertion(memberchk(solver(_), Statistics)).

	test(lp_minimization, deterministic, [condition(solver_available)]) :-
		build([v(x, continuous, 0, inf), v(y, continuous, 0, inf)], [c([1*x, 1*y], >=, 4), c([1*x, 3*y], >=, 6)], o([2*x, 3*y], minimize), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Objective, _, _),
		assertion(Status == optimal),
		assertion(close(Objective, 9)).

	test(lp_equality_constraint, deterministic, [condition(solver_available)]) :-
		build([v(x, continuous, 0, inf), v(y, continuous, 0, inf)], [c([1*x, 1*y], =, 10)], o([1*x, 2*y], maximize), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Objective, _, _),
		assertion(Status == optimal),
		assertion(close(Objective, 20)),
		assertion(value(Result, y, 10)).

	test(lp_free_and_bounded_variables, deterministic, [condition(solver_available)]) :-
		build([v(x, continuous, -inf, inf), v(y, continuous, -inf, 5), v(z, continuous, 2, 2)], [c([1*x], >=, -3)], o([1*x, -1*y, 1*z], minimize), Problem),
		_Solver_::solve(Problem, Result),
		% x = -3, y = 5, z = 2 (fixed variable)
		Result = linear_programming_result(Status, Objective, _, _),
		assertion(Status == optimal),
		assertion(value(Result, x, -3)),
		assertion(close(Objective, -3 - 5 + 2)).

	test(lp_unused_variable_is_zero, deterministic, [condition(solver_available)]) :-
		build([v(x, continuous, 0, 4), v(unused, continuous, 0, 9)], [c([1*x], =<, 3)], o([1*x], maximize), Problem),
		_Solver_::solve(Problem, Result),
		assertion(value(Result, unused, 0)),
		assertion(value(Result, x, 3)).

	test(lp_without_constraints, deterministic, [condition(solver_available)]) :-
		build([v(x, continuous, 0, 5)], [], o([1*x], maximize), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Objective, _, _),
		assertion(Status == optimal),
		assertion(close(Objective, 5)).

	test(lp_empty_constraint, deterministic, [condition(solver_available)]) :-
		build([v(x, continuous, 0, 5)], [c([], =<, 5)], o([1*x], maximize), Problem),
		_Solver_::solve(Problem, Result),
		assertion(value(Result, x, 5)).

	test(lp_structured_variable_names, deterministic, [condition(solver_available)]) :-
		build([v(x(1,2), continuous, 0, inf), v(y('A b'), continuous, 0, inf)], [c([1*x(1,2), 1*y('A b')], =<, 6)], o([1*x(1,2), 2*y('A b')], maximize), Problem),
		_Solver_::solve(Problem, Result),
		assertion(value(Result, x(1,2), 0)),
		assertion(value(Result, y('A b'), 6)),
		_Solver_::objective_value(Result, Objective),
		assertion(close(Objective, 12)).

	test(lp_infeasible, deterministic, [condition(solver_available)]) :-
		build([v(x, continuous, 0, inf), v(y, continuous, 0, inf)], [c([1*x, 1*y], >=, 5), c([1*x, 1*y], =<, 3)], o([1*x, 1*y], minimize), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Objective, Values, _),
		assertion(memberchk(Status, [infeasible, infeasible_or_unbounded])),
		assertion(Objective == none),
		assertion(Values == []).

	test(lp_unbounded, deterministic, [condition(solver_available)]) :-
		build([v(x, continuous, 0, inf), v(y, continuous, 0, inf)], [c([1*x, -1*y], =<, 3)], o([1*x, 1*y], maximize), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Objective, Values, _),
		assertion(memberchk(Status, [unbounded, infeasible_or_unbounded])),
		assertion(Objective == none),
		assertion(Values == []).

	% mixed-integer programs

	test(milp_rounding, deterministic, [condition(solver_available)]) :-
		build([v(x, integer, 0, inf), v(y, integer, 0, inf)], [c([2*x, 2*y], =<, 7)], o([1*x, 1*y], maximize), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Objective, _, _),
		assertion(Status == optimal),
		assertion(close(Objective, 3)),
		_Solver_::variable_value(Result, x, X),
		_Solver_::variable_value(Result, y, Y),
		assertion(integer(X)),
		assertion(integer(Y)),
		assertion(X + Y =:= 3).

	test(milp_knapsack, deterministic, [condition(solver_available)]) :-
		Variables = [v(a, binary, 0, 1), v(b, binary, 0, 1), v(c, binary, 0, 1), v(d, binary, 0, 1), v(e, binary, 0, 1)],
		Constraints = [c([12*a, 7*b, 11*c, 8*d, 9*e], =<, 26)],
		Objective = o([24*a, 13*b, 23*c, 15*d, 16*e], maximize),
		build(Variables, Constraints, Objective, Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Value, _, _),
		assertion(Status == optimal),
		% the best selection is b, c, and d
		assertion(close(Value, 51)),
		assertion(value(Result, b, 1)),
		assertion(value(Result, c, 1)),
		assertion(value(Result, d, 1)),
		assertion(value(Result, a, 0)),
		assertion(value(Result, e, 0)).

	test(milp_mixed_variables, deterministic, [condition(solver_available)]) :-
		% maximize 2x + y with x + y =< 6.5 and x >= y: x = 6 and y = 0.5
		build([v(x, integer, 0, 10), v(y, continuous, 0, 10)], [c([1*x, 1*y], =<, 6.5), c([1*x, -1*y], >=, 0)], o([2*x, 1*y], maximize), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Value, _, _),
		assertion(Status == optimal),
		assertion(close(Value, 12.5)),
		assertion(value(Result, x, 6)),
		assertion(value(Result, y, 0.5)).

	test(lp_multiple_upper_bounds, deterministic, [condition(solver_available)]) :-
		% minimize -2x + y with 0 =< x =< 11 and 0 =< y =< 6: x = 11 and y = 0
		build([v(x, continuous, 0, 11), v(y, continuous, 0, 6)], [c([1*x, 1*y], =<, 100)], o([-2*x, 1*y], minimize), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Value, _, _),
		assertion(Status == optimal),
		assertion(close(Value, -22)),
		assertion(value(Result, x, 11)),
		assertion(value(Result, y, 0)).

	test(lp_lower_and_upper_bounds, deterministic, [condition(solver_available)]) :-
		% minimize -2x + y with 1 =< x =< 11 and 3 =< y =< 6: x = 11 and y = 3
		build([v(x, continuous, 1, 11), v(y, continuous, 3, 6)], [c([1*x, 1*y], =<, 100)], o([-2*x, 1*y], minimize), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Value, _, _),
		assertion(Status == optimal),
		assertion(close(Value, -19)),
		assertion(value(Result, x, 11)),
		assertion(value(Result, y, 3)).

	test(milp_infeasible, deterministic, [condition(solver_available)]) :-
		build([v(x, integer, 0, 10)], [c([2*x], =, 3)], o([1*x], minimize), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Objective, Values, _),
		assertion(memberchk(Status, [infeasible, infeasible_or_unbounded])),
		assertion(Objective == none),
		assertion(Values == []).

	test(milp_unbounded, deterministic, [condition(solver_available)]) :-
		build([v(x, integer, 0, inf), v(y, integer, 0, inf)], [c([1*x, -1*y], =<, 3)], o([1*x], maximize), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, _, _, _),
		assertion(memberchk(Status, [unbounded, infeasible_or_unbounded])).

	% options

	test(option_time_limit, deterministic, [condition(solver_available)]) :-
		build([v(x, continuous, 0, 5)], [], o([1*x], maximize), Problem),
		_Solver_::solve(Problem, Result, [time_limit(60)]),
		assertion(value(Result, x, 5)).

	test(option_mip_gap, deterministic, [condition(solver_available)]) :-
		build([v(x, integer, 0, 5)], [c([2*x], =<, 7)], o([1*x], maximize), Problem),
		(	_Solver_ == highs ->
			catch(_Solver_::solve(Problem, _, [mip_gap(0.01)]), error(Error, _), true),
			assertion(Error == domain_error(option, mip_gap(0.01)))
		;	_Solver_::solve(Problem, Result, [mip_gap(0.01)]),
			assertion(value(Result, x, 3))
		).

	test(option_executable, deterministic, [condition(solver_available)]) :-
		executable_name(_Solver_, Name),
		resolve_command_path(Name, Path),
		build([v(x, continuous, 0, 5)], [], o([1*x], maximize), Problem),
		_Solver_::solve(Problem, Result, [executable(Path)]),
		assertion(value(Result, x, 5)).

	test(option_missing_executable, error(existence_error(command, logtalk_missing_lp_solver))) :-
		build([v(x, continuous, 0, 5)], [], o([1*x], maximize), Problem),
		_Solver_::solve(Problem, _, [executable(logtalk_missing_lp_solver)]).

	test(option_invalid_time_limit, error(domain_error(option, time_limit(-1)))) :-
		build([v(x, continuous, 0, 5)], [], o([1*x], maximize), Problem),
		_Solver_::solve(Problem, _, [time_limit(-1)]).

	test(option_invalid_mip_gap, error(domain_error(option, mip_gap(-0.5)))) :-
		build([v(x, continuous, 0, 5)], [], o([1*x], maximize), Problem),
		_Solver_::solve(Problem, _, [mip_gap(-0.5)]).

	test(option_invalid_executable, error(domain_error(option, executable('')))) :-
		build([v(x, continuous, 0, 5)], [], o([1*x], maximize), Problem),
		_Solver_::solve(Problem, _, [executable('')]).

	test(problem_without_variables, error(domain_error(linear_programming_problem, empty))) :-
		simplex::new_problem(Problem),
		_Solver_::solve(Problem, _).

	test(problem_without_objective, error(domain_error(linear_programming_problem, missing_objective))) :-
		simplex::new_problem(Problem0),
		simplex::variable(x, continuous, Problem0, Problem),
		_Solver_::solve(Problem, _).

	test(problem_invalid, error(_)) :-
		_Solver_::solve(foo, _).

	% results

	test(result_feasible_accessors, deterministic) :-
		Result = linear_programming_result(feasible, 5, [x-1, y-2], []),
		_Solver_::status(Result, Status),
		_Solver_::objective_value(Result, Objective),
		_Solver_::variable_value(Result, y, Value),
		assertion(Status == feasible),
		assertion(Objective == 5),
		assertion(Value == 2).

	test(result_time_limit_has_no_solution, deterministic) :-
		Result = linear_programming_result(time_limit, none, [], []),
		_Solver_::status(Result, time_limit),
		\+ _Solver_::objective_value(Result, _).

	% random problems: linear programs are compared with the pure Prolog simplex backend (using
	% explicit constraints for the upper bounds) and integer programs with an exhaustive enumeration

	test(random_linear_programs, deterministic, [condition(solver_available)]) :-
		random_count(25, Count),
		random_linear_programs(Count, 4711).

	test(random_integer_programs, deterministic, [condition(solver_available)]) :-
		random_count(20, Count),
		random_integer_programs(Count, 1174).

	% time limit on a harder knapsack-like problem: either solved or a (valid) best solution is returned

	test(time_limit_hard_problem, deterministic, [condition(solver_available)]) :-
		hard_problem(Problem, Constraints),
		_Solver_::solve(Problem, Result, [time_limit(1)]),
		Result = linear_programming_result(Status, Objective, Values, _),
		assertion(memberchk(Status, [optimal, feasible, time_limit])),
		(	(	Status == optimal
			;	Status == feasible
			) ->
			assertion(number(Objective)),
			assertion(binary_values(Values)),
			assertion(constraints_satisfied(Constraints, Values))
		;	assertion(Objective == none)
		).

	% auxiliary predicates

	solver_available :-
		executable_name(_Solver_, Name),
		resolve_command_path(Name, _).

	indexed_name(Prefix, Index, Name) :-
		number_codes(Index, IndexCodes),
		atom_codes(Prefix, PrefixCodes),
		append(PrefixCodes, IndexCodes, NameCodes),
		atom_codes(Name, NameCodes).

	executable_name(glpk, glpsol).
	executable_name(cbc, cbc).
	executable_name(highs, highs).
	executable_name(scip, scip).

	close(Value, Expected) :-
		abs(Value - Expected) =< 1.0e-5 * max(1, abs(Expected)).

	value(Result, Variable, Expected) :-
		_Solver_::variable_value(Result, Variable, Value),
		close(Value, Expected).

	build(Variables, Constraints, o(Expression, Sense), Problem) :-
		simplex::new_problem(Problem0),
		add_variables(Variables, Problem0, Problem1),
		add_constraints(Constraints, Problem1, Problem2),
		simplex::objective(Expression, Sense, Problem2, Problem).

	add_variables([], Problem, Problem).
	add_variables([v(Name, Type, Lower, Upper)| Variables], Problem0, Problem) :-
		simplex::variable(Name, Type, Lower, Upper, Problem0, Problem1),
		add_variables(Variables, Problem1, Problem).

	add_constraints([], Problem, Problem).
	add_constraints([c(Expression, Sense, RightHandSide)| Constraints], Problem0, Problem) :-
		simplex::constraint(Expression, Sense, RightHandSide, Problem0, Problem1),
		add_constraints(Constraints, Problem1, Problem).

	% fewer random problems on GNU Prolog, whose small default global stack is
	% exhausted by the tests running in debug mode with coverage
	:- if(current_logtalk_flag(prolog_dialect, gnu)).
		random_count(Count0, Count) :-
			Count is max(2, Count0 // 5).
	:- else.
		random_count(Count, Count).
	:- endif.

	% deterministic pseudo-random problems (linear congruential generator)

	next_seed(Seed0, Seed) :-
		% Split the seed into 16-bit parts to avoid overflow before reduction modulo 2^31.
		Low is Seed0 mod 65536,
		High is ((Seed0 // 65536) * 1103515245) mod 32768,
		Seed is (Low * 1103515245 + High * 65536 + 12345) mod 2147483648.

	random_int(Low, High, Seed0, Seed, Value) :-
		next_seed(Seed0, Seed),
		Value is Low + (Seed // 65536) mod (High - Low + 1).

	random_linear_programs(0, _) :-
		!.
	random_linear_programs(Count, Seed0) :-
		random_int(2, 4, Seed0, Seed1, VariableCount),
		random_int(1, 4, Seed1, Seed2, ConstraintCount),
		random_lp_variables(VariableCount, Seed2, Seed3, Variables, Names, UpperConstraints),
		random_constraints(ConstraintCount, Names, Seed3, Seed4, Constraints0),
		append(Constraints0, UpperConstraints, Constraints),
		random_expression(Names, Seed4, Seed5, Expression),
		random_int(0, 1, Seed5, Seed, SenseIndex),
		(	SenseIndex =:= 0 -> Sense = maximize ; Sense = minimize ),
		build(Variables, Constraints, o(Expression, Sense), Problem),
		_Solver_::solve(Problem, Result),
		simplex::solve(Problem, Expected),
		Result = linear_programming_result(Status, Objective, _, _),
		Expected = linear_programming_result(ExpectedStatus, ExpectedObjective, _, _),
		(	ExpectedStatus == optimal ->
			assertion(Status == optimal),
			assertion(close(Objective, ExpectedObjective))
		;	assertion(memberchk(Status, [infeasible, infeasible_or_unbounded]))
		),
		Count1 is Count - 1,
		random_linear_programs(Count1, Seed).

	% continuous variables with a non-negative lower bound; the upper bounds are constraints
	random_lp_variables(0, Seed, Seed, [], [], []) :-
		!.
	random_lp_variables(Count, Seed0, Seed, [v(Name, continuous, Lower, inf)| Variables], [Name| Names], [c([1*Name], =<, Upper)| Constraints]) :-
		indexed_name(v, Count, Name),
		random_int(0, 3, Seed0, Seed1, Lower),
		random_int(4, 12, Seed1, Seed2, Upper),
		Count1 is Count - 1,
		random_lp_variables(Count1, Seed2, Seed, Variables, Names, Constraints).

	random_integer_programs(0, _) :-
		!.
	random_integer_programs(Count, Seed0) :-
		random_int(2, 4, Seed0, Seed1, VariableCount),
		random_int(1, 3, Seed1, Seed2, ConstraintCount),
		random_ip_variables(VariableCount, Seed2, Seed3, Variables, Names),
		random_constraints(ConstraintCount, Names, Seed3, Seed4, Constraints),
		random_expression(Names, Seed4, Seed5, Expression),
		random_int(0, 1, Seed5, Seed, SenseIndex),
		(	SenseIndex =:= 0 ->
			Sense = maximize
		;	Sense = minimize
		),
		build(Variables, Constraints, o(Expression, Sense), Problem),
		_Solver_::solve(Problem, Result),
		Result = linear_programming_result(Status, Objective, Values, _),
		enumerate(Variables, Constraints, Expression, Sense, Best),
		(	Best == none ->
			assertion(memberchk(Status, [infeasible, infeasible_or_unbounded]))
		;	assertion(Status == optimal),
			assertion(close(Objective, Best)),
			assertion(constraints_satisfied(Constraints, Values))
		),
		Count1 is Count - 1,
		random_integer_programs(Count1, Seed).

	random_ip_variables(0, Seed, Seed, [], []) :-
		!.
	random_ip_variables(Count, Seed0, Seed, [v(Name, integer, Lower, Upper)| Variables], [Name| Names]) :-
		indexed_name(v, Count, Name),
		random_int(-2, 1, Seed0, Seed1, Lower),
		random_int(2, 4, Seed1, Seed2, Upper),
		Count1 is Count - 1,
		random_ip_variables(Count1, Seed2, Seed, Variables, Names).

	% exhaustive enumeration of the integer points: the best objective value (or none if infeasible)
	enumerate(Variables, Constraints, Expression, Sense, Best) :-
		findall(
			Objective,
			(	assignment(Variables, Values),
				constraints_satisfied(Constraints, Values),
				expression_value(Expression, Values, 0, Objective)
			),
			Objectives
		),
		(	Objectives == [] ->
			Best = none
		;	Sense == maximize ->
			max(Objectives, Best)
		;	min(Objectives, Best)
		).

	assignment([], []).
	assignment([v(Name, _, Lower, Upper)| Variables], [Name-Value| Values]) :-
		between(Lower, Upper, Value),
		assignment(Variables, Values).

	random_constraints(0, _, Seed, Seed, []) :-
		!.
	random_constraints(Count, Names, Seed0, Seed, [c(Expression, Sense, RightHandSide)| Constraints]) :-
		random_expression(Names, Seed0, Seed1, Expression),
		random_int(0, 2, Seed1, Seed2, SenseIndex),
		sense(SenseIndex, Sense),
		random_int(0, 30, Seed2, Seed3, RightHandSide),
		Count1 is Count - 1,
		random_constraints(Count1, Names, Seed3, Seed, Constraints).

	sense(0, =<).
	sense(1, =<).
	sense(2, >=).

	random_expression([], Seed, Seed, []).
	random_expression([Name| Names], Seed0, Seed, [Coefficient*Name| Expression]) :-
		random_int(-3, 4, Seed0, Seed1, Coefficient),
		random_expression(Names, Seed1, Seed, Expression).

	% a multidimensional knapsack with 70 binary variables and 6 constraints

	hard_problem(Problem, Constraints) :-
		hard_size(Count, Rows),
		numbered_names(Count, Names),
		hard_weights(Rows, Count, 90210, Seed, WeightRows),
		hard_values(Names, WeightRows, Seed, Values),
		hard_constraints(WeightRows, Names, Constraints),
		hard_variables(Names, Variables),
		pair_expression(Names, Values, Expression),
		build(Variables, Constraints, o(Expression, maximize), Problem).

	:- if(current_logtalk_flag(prolog_dialect, gnu)).
		hard_size(40, 4).
	:- else.
		hard_size(70, 6).
	:- endif.

	numbered_names(Count, Names) :-
		numbered_names(1, Count, Names).

	numbered_names(Index, Count, Names) :-
		(	Index > Count ->
			Names = []
		;	indexed_name(item, Index, Name),
			Names = [Name| Names1],
			Next is Index + 1,
			numbered_names(Next, Count, Names1)
		).

	hard_weights(0, _, Seed, Seed, []) :-
		!.
	hard_weights(Rows, Count, Seed0, Seed, [Row| Rows1]) :-
		random_row(Count, Seed0, Seed1, Row),
		Rows0 is Rows - 1,
		hard_weights(Rows0, Count, Seed1, Seed, Rows1).

	random_row(0, Seed, Seed, []) :-
		!.
	random_row(Count, Seed0, Seed, [Weight| Weights]) :-
		random_int(10, 100, Seed0, Seed1, Weight),
		Count1 is Count - 1,
		random_row(Count1, Seed1, Seed, Weights).

	% values correlated with the weights make the problem harder
	hard_values(Names, WeightRows, Seed0, Values) :-
		length(WeightRows, Rows),
		hard_values(Names, 1, WeightRows, Rows, Seed0, Values).

	hard_values([], _, _, _, _, []).
	hard_values([_| Names], Index, WeightRows, Rows, Seed0, [Value| Values]) :-
		column_sum(WeightRows, Index, 0, Sum),
		random_int(0, 10, Seed0, Seed, Noise),
		Value is Sum // Rows + Noise,
		Next is Index + 1,
		hard_values(Names, Next, WeightRows, Rows, Seed, Values).

	column_sum([], _, Sum, Sum).
	column_sum([Row| Rows], Index, Sum0, Sum) :-
		nth1(Index, Row, Weight),
		Sum1 is Sum0 + Weight,
		column_sum(Rows, Index, Sum1, Sum).

	hard_constraints([], _, []).
	hard_constraints([Row| Rows], Names, [c(Expression, =<, Capacity)| Constraints]) :-
		pair_expression(Names, Row, Expression),
		sum(Row, Total),
		Capacity is Total // 2,
		hard_constraints(Rows, Names, Constraints).

	pair_expression([], [], []).
	pair_expression([Name| Names], [Coefficient| Coefficients], [Coefficient*Name| Expression]) :-
		pair_expression(Names, Coefficients, Expression).

	hard_variables([], []).
	hard_variables([Name| Names], [v(Name, binary, 0, 1)| Variables]) :-
		hard_variables(Names, Variables).

	binary_values([]).
	binary_values([_-Value| Values]) :-
		(	Value =:= 0
		;	Value =:= 1
		),
		!,
		binary_values(Values).

	constraints_satisfied([], _).
	constraints_satisfied([c(Expression, Sense, RightHandSide)| Constraints], Values) :-
		expression_value(Expression, Values, 0, Value),
		(	Sense == (=<) ->
			Value =< RightHandSide + 1.0e-6
		;	Sense == (>=) ->
			Value >= RightHandSide - 1.0e-6
		;	abs(Value - RightHandSide) =< 1.0e-6
		),
		constraints_satisfied(Constraints, Values).

	expression_value([], _, Value, Value).
	expression_value([Coefficient*Name| Expression], Values, Value0, Value) :-
		memberchk(Name-Variable, Values),
		Value1 is Value0 + Coefficient * Variable,
		expression_value(Expression, Values, Value1, Value).

:- end_object.
