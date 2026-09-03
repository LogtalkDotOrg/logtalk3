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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Unit tests for the "linear_programming" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(simplex).
	cover(milp_branch_and_bound).

	test(linear_programming_new_problem_1, deterministic(Problem == linear_program([], [], none))) :-
		simplex::new_problem(Problem).

	test(linear_programming_canonical_expression, deterministic(Problem == linear_program([variable(x,continuous,0,inf)],[],objective([3*x],maximize)))) :-
		simplex::new_problem(Problem0),
		simplex::variable(x, continuous, Problem0, Problem1),
		simplex::objective([1*x, 4*x, -2*x, 0*x], maximize, Problem1, Problem).

	test(linear_programming_bounded_maximum, deterministic((Status == optimal, X =~= 14.0, Y =~= 0.0, Value =~= 42.0))) :-
		sample_problem(Problem),
		simplex::solve(Problem, Result),
		simplex::status(Result, Status),
		simplex::variable_value(Result, x, X),
		simplex::variable_value(Result, y, Y),
		simplex::objective_value(Result, Value).

	test(linear_programming_minimum, deterministic((X =~= 2.0, Value =~= 2.0))) :-
		one_variable_problem(0, inf, [constraint([1*x], >=, 2)], [1*x], minimize, Problem),
		simplex::solve(Problem, Result),
		simplex::variable_value(Result, x, X),
		simplex::objective_value(Result, Value).

	test(linear_programming_equality, deterministic(X =~= 3.0)) :-
		one_variable_problem(0, inf, [constraint([1*x], =, 3)], [1*x], minimize, Problem),
		simplex::solve(Problem, Result),
		simplex::variable_value(Result, x, X).

	test(linear_programming_negative_rhs, deterministic(X =~= 2.0)) :-
		one_variable_problem(0, inf, [constraint([-1*x], =<, -2),constraint([1*x], =<, 5)], [1*x], minimize, Problem),
		simplex::solve(Problem, Result),
		simplex::variable_value(Result, x, X).

	test(linear_programming_finite_bounds, deterministic(X =~= 5.0)) :-
		one_variable_problem(2, 5, [], [1*x], maximize, Problem),
		simplex::solve(Problem, Result),
		simplex::variable_value(Result, x, X).

	test(linear_programming_upper_bound_only, deterministic(X =~= 3.0)) :-
		one_variable_problem(-inf, 3, [], [1*x], maximize, Problem),
		simplex::solve(Problem, Result),
		simplex::variable_value(Result, x, X).

	test(linear_programming_free_variable, deterministic(X =~= -2.0)) :-
		one_variable_problem(-inf, inf, [constraint([1*x], =, -2)], [1*x], minimize, Problem),
		simplex::solve(Problem, Result),
		simplex::variable_value(Result, x, X).

	test(linear_programming_fixed_variable, deterministic((X =~= 4.0, Value =~= 8.0))) :-
		one_variable_problem(4, 4, [], [2*x], maximize, Problem),
		simplex::solve(Problem, Result),
		simplex::variable_value(Result, x, X),
		simplex::objective_value(Result, Value).

	test(linear_programming_lower_bound_objective, deterministic((X =~= 5.5, Value =~= 11.0))) :-
		one_variable_problem(5.5, inf, [], [2*x], minimize, Problem),
		simplex::solve(Problem, Result),
		simplex::variable_value(Result, x, X),
		simplex::objective_value(Result, Value).

	test(linear_programming_redundant_equalities, deterministic((Status == optimal, X =~= 2.0))) :-
		one_variable_problem(0, inf, [constraint([1*x], =, 2),constraint([2*x], =, 4)], [1*x], minimize, Problem),
		simplex::solve(Problem, Result),
		simplex::status(Result, Status),
		simplex::variable_value(Result, x, X).

	test(linear_programming_empty_constraint_true, deterministic(Status == optimal)) :-
		one_variable_problem(0, 1, [constraint([], =<, 0)], [1*x], minimize, Problem),
		simplex::solve(Problem, Result),
		simplex::status(Result, Status).

	test(linear_programming_empty_constraint_false, deterministic(Status == infeasible)) :-
		one_variable_problem(0, 1, [constraint([], >=, 1)], [1*x], minimize, Problem),
		simplex::solve(Problem, Result),
		simplex::status(Result, Status).

	test(linear_programming_zero_objective, deterministic((Status == optimal, Value =~= 0.0))) :-
		one_variable_problem(-inf, inf, [constraint([1*x], =, 3)], [], minimize, Problem),
		simplex::solve(Problem, Result),
		simplex::status(Result, Status),
		simplex::objective_value(Result, Value).

	test(linear_programming_multiple_free_variables, deterministic((X =~= -1.0, Y =~= 2.0, Value =~= 1.0))) :-
		simplex::new_problem(Problem0),
		simplex::variable(x, continuous, -inf, inf, Problem0, Problem1),
		simplex::variable(y, continuous, -inf, inf, Problem1, Problem2),
		simplex::constraint([1*x], =, -1, Problem2, Problem3),
		simplex::constraint([1*y], =, 2, Problem3, Problem4),
		simplex::objective([1*x,1*y], minimize, Problem4, Problem),
		simplex::solve(Problem, Result),
		simplex::variable_value(Result, x, X),
		simplex::variable_value(Result, y, Y),
		simplex::objective_value(Result, Value).

	test(linear_programming_infeasible, deterministic(Status == infeasible)) :-
		one_variable_problem(0, inf, [constraint([1*x], =<, 0),constraint([1*x], >=, 1)], [1*x], minimize, Problem),
		simplex::solve(Problem, Result),
		simplex::status(Result, Status).

	test(linear_programming_unbounded, deterministic(Status == unbounded)) :-
		one_variable_problem(0, inf, [], [1*x], maximize, Problem),
		simplex::solve(Problem, Result),
		simplex::status(Result, Status).

	test(linear_programming_iteration_limit, deterministic(Status == iteration_limit)) :-
		sample_problem(Problem),
		simplex::solve(Problem, Result, [max_iterations(1)]),
		simplex::status(Result, Status).

	test(linear_programming_dantzig_pivot_rule, deterministic((BlandStatus == iteration_limit, DantzigStatus == optimal, X =~= 0.0, Y =~= 1.0, Value =~= 2.0))) :-
		pivot_rule_problem(Problem),
		simplex::solve(Problem, BlandResult, [max_iterations(1)]),
		simplex::status(BlandResult, BlandStatus),
		simplex::solve(Problem, DantzigResult, [max_iterations(1),pivot_rule(dantzig)]),
		simplex::status(DantzigResult, DantzigStatus),
		simplex::variable_value(DantzigResult, x, X),
		simplex::variable_value(DantzigResult, y, Y),
		simplex::objective_value(DantzigResult, Value).

	test(linear_programming_dantzig_pivot_rule_tie, deterministic((X =~= 1.0, Y =~= 0.0))) :-
		simplex::new_problem(Problem0),
		simplex::variable(x, continuous, Problem0, Problem1),
		simplex::variable(y, continuous, Problem1, Problem2),
		simplex::constraint([1*x,1*y], =<, 1, Problem2, Problem3),
		simplex::objective([1*x,1*y], maximize, Problem3, Problem),
		simplex::solve(Problem, Result, [pivot_rule(dantzig)]),
		simplex::variable_value(Result, x, X),
		simplex::variable_value(Result, y, Y).

	test(linear_programming_dantzig_phase_one, deterministic((Status == optimal, X =~= 2.0, Value =~= 2.0))) :-
		one_variable_problem(0, inf, [constraint([1*x], >=, 2)], [1*x], minimize, Problem),
		simplex::solve(Problem, Result, [pivot_rule(dantzig)]),
		simplex::status(Result, Status),
		simplex::variable_value(Result, x, X),
		simplex::objective_value(Result, Value).

	test(linear_programming_invalid_pivot_rule, error(domain_error(option, pivot_rule(random)))) :-
		sample_problem(Problem),
		simplex::solve(Problem, _Result, [pivot_rule(random)]).

	test(linear_programming_matrix_form, deterministic((X =~= 14.0, Y =~= 0.0, Value =~= 42.0))) :-
		simplex::problem_from_matrices([3,4], maximize, [], [], [[1,2],[-3,1]], [14,0], [0-inf,0-inf], Problem),
		simplex::solve(Problem, Result),
		simplex::variable_value(Result, 1, X),
		simplex::variable_value(Result, 2, Y),
		simplex::objective_value(Result, Value).

	test(linear_programming_statistics, deterministic) :-
		sample_problem(Problem),
		simplex::solve(Problem, Result),
		simplex::statistics(Result, [iterations(_),phase_one_iterations(_),phase_two_iterations(_)]).

	test(linear_programming_objective_value_non_optimal, fail) :-
		one_variable_problem(0, inf, [], [1*x], maximize, Problem),
		simplex::solve(Problem, Result),
		simplex::objective_value(Result, _Value).

	test(linear_programming_duplicate_variable, error(domain_error(linear_programming_variable, x))) :-
		simplex::new_problem(Problem0),
		simplex::variable(x, continuous, Problem0, Problem1),
		simplex::variable(x, continuous, Problem1, _Problem).

	test(linear_programming_undeclared_variable, error(domain_error(linear_programming_variable, y))) :-
		simplex::new_problem(Problem0),
		simplex::variable(x, continuous, Problem0, Problem1),
		simplex::objective([1*y], maximize, Problem1, _Problem).

	test(linear_programming_invalid_expression, error(type_error(linear_expression, [x+y]))) :-
		simplex::new_problem(Problem0),
		simplex::variable(x, continuous, Problem0, Problem1),
		simplex::objective([x+y], maximize, Problem1, _Problem).

	test(linear_programming_invalid_bounds, error(domain_error(linear_programming_bounds, 2-1))) :-
		simplex::new_problem(Problem0),
		simplex::variable(x, continuous, 2, 1, Problem0, _Problem).

	test(linear_programming_matrix_dimensions, error(domain_error(linear_programming_dimensions, [[1,2]]))) :-
		simplex::problem_from_matrices([1], maximize, [], [], [[1,2]], [1], [0-inf], _Problem).

	test(linear_programming_integer_backend_error, error(domain_error(simplex_variable_type, x-integer))) :-
		simplex::new_problem(Problem0),
		simplex::variable(x, integer, Problem0, Problem1),
		simplex::objective([1*x], maximize, Problem1, Problem),
		simplex::solve(Problem, _Result).

	test(linear_programming_milp_binary_fractional_relaxation, deterministic((X + Y =:= 1, Value =~= 1.0))) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, binary, Problem0, Problem1),
		milp_branch_and_bound::variable(y, binary, Problem1, Problem2),
		milp_branch_and_bound::constraint([2*x,2*y], =<, 3, Problem2, Problem3),
		milp_branch_and_bound::objective([1*x,1*y], maximize, Problem3, Problem),
		milp_branch_and_bound::solve(Problem, Result),
		milp_branch_and_bound::variable_value(Result, x, X),
		milp_branch_and_bound::variable_value(Result, y, Y),
		milp_branch_and_bound::objective_value(Result, Value).

	test(linear_programming_milp_integer_minimum, deterministic((X =:= 2, Value =~= 2.0))) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, integer, 0, 10, Problem0, Problem1),
		milp_branch_and_bound::constraint([2*x], >=, 3, Problem1, Problem2),
		milp_branch_and_bound::objective([1*x], minimize, Problem2, Problem),
		milp_branch_and_bound::solve(Problem, Result),
		milp_branch_and_bound::variable_value(Result, x, X),
		milp_branch_and_bound::objective_value(Result, Value).

	test(linear_programming_milp_integer_infeasible, deterministic(Status == infeasible)) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, integer, 0, 1, Problem0, Problem1),
		milp_branch_and_bound::constraint([1*x], =, 0.5, Problem1, Problem2),
		milp_branch_and_bound::objective([1*x], minimize, Problem2, Problem),
		milp_branch_and_bound::solve(Problem, Result),
		milp_branch_and_bound::status(Result, Status).

	test(linear_programming_milp_negative_domain, deterministic((X =:= -1, Value =~= -1.0))) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, integer, -3, -1, Problem0, Problem1),
		milp_branch_and_bound::objective([1*x], maximize, Problem1, Problem),
		milp_branch_and_bound::solve(Problem, Result),
		milp_branch_and_bound::variable_value(Result, x, X),
		milp_branch_and_bound::objective_value(Result, Value).

	test(linear_programming_milp_continuous_only, deterministic((X =~= 2.5, Value =~= 2.5))) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, continuous, 0, 2.5, Problem0, Problem1),
		milp_branch_and_bound::objective([1*x], maximize, Problem1, Problem),
		milp_branch_and_bound::solve(Problem, Result),
		milp_branch_and_bound::variable_value(Result, x, X),
		milp_branch_and_bound::objective_value(Result, Value).

	test(linear_programming_milp_mixed_variables, deterministic((X =:= 2, Y =~= 0.5, Value =~= 6.5))) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, integer, 0, 2, Problem0, Problem1),
		milp_branch_and_bound::variable(y, continuous, 0, 3, Problem1, Problem2),
		milp_branch_and_bound::constraint([2*x,2*y], =<, 5, Problem2, Problem3),
		milp_branch_and_bound::objective([3*x,1*y], maximize, Problem3, Problem),
		milp_branch_and_bound::solve(Problem, Result),
		milp_branch_and_bound::variable_value(Result, x, X),
		milp_branch_and_bound::variable_value(Result, y, Y),
		milp_branch_and_bound::objective_value(Result, Value).

	test(linear_programming_milp_unbounded_continuous_leaf, deterministic(Status == unbounded)) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, integer, 0, 0, Problem0, Problem1),
		milp_branch_and_bound::variable(y, continuous, Problem1, Problem2),
		milp_branch_and_bound::objective([1*y], maximize, Problem2, Problem),
		milp_branch_and_bound::solve(Problem, Result),
		milp_branch_and_bound::status(Result, Status).

	test(linear_programming_milp_node_limit, deterministic(Status == node_limit)) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, binary, Problem0, Problem1),
		milp_branch_and_bound::variable(y, binary, Problem1, Problem2),
		milp_branch_and_bound::constraint([2*x,2*y], =<, 3, Problem2, Problem3),
		milp_branch_and_bound::objective([1*x,1*y], maximize, Problem3, Problem),
		milp_branch_and_bound::solve(Problem, Result, [max_nodes(1)]),
		milp_branch_and_bound::status(Result, Status).

	test(linear_programming_milp_statistics, subsumes([nodes(_),lp_iterations(_),bound_pruned_nodes(_),infeasible_nodes(_),incumbent_updates(_)], Statistics)) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, binary, Problem0, Problem1),
		milp_branch_and_bound::objective([1*x], maximize, Problem1, Problem),
		milp_branch_and_bound::solve(Problem, Result),
		milp_branch_and_bound::statistics(Result, Statistics).

	test(linear_programming_milp_invalid_max_nodes, error(domain_error(option, max_nodes(0)))) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, binary, Problem0, Problem1),
		milp_branch_and_bound::objective([1*x], maximize, Problem1, Problem),
		milp_branch_and_bound::solve(Problem, _Result, [max_nodes(0)]).

	test(linear_programming_milp_simplex_pivot_rule, deterministic((Status == optimal, X =~= 0.0, Y =~= 1.0, Value =~= 2.0))) :-
		pivot_rule_problem(Problem),
		milp_branch_and_bound::solve(Problem, Result, [simplex_max_iterations(1),simplex_pivot_rule(dantzig)]),
		milp_branch_and_bound::status(Result, Status),
		milp_branch_and_bound::variable_value(Result, x, X),
		milp_branch_and_bound::variable_value(Result, y, Y),
		milp_branch_and_bound::objective_value(Result, Value).

	test(linear_programming_milp_invalid_simplex_pivot_rule, error(domain_error(option, simplex_pivot_rule(random)))) :-
		pivot_rule_problem(Problem),
		milp_branch_and_bound::solve(Problem, _Result, [simplex_pivot_rule(random)]).

	test(linear_programming_milp_most_fractional_branching, deterministic((FirstUpdates == 0, MostUpdates == 1))) :-
		branching_rule_problem(Problem),
		milp_branch_and_bound::solve(Problem, FirstResult, [max_nodes(2)]),
		milp_branch_and_bound::statistics(FirstResult, FirstStatistics),
		memberchk(incumbent_updates(FirstUpdates), FirstStatistics),
		milp_branch_and_bound::solve(Problem, MostResult, [max_nodes(2),branching_rule(most_fractional)]),
		milp_branch_and_bound::statistics(MostResult, MostStatistics),
		memberchk(incumbent_updates(MostUpdates), MostStatistics).

	test(linear_programming_milp_most_fractional_tie, deterministic(Updates == 0)) :-
		branching_rule_tie_problem(Problem),
		milp_branch_and_bound::solve(Problem, Result, [max_nodes(2),branching_rule(most_fractional)]),
		milp_branch_and_bound::statistics(Result, Statistics),
		memberchk(incumbent_updates(Updates), Statistics).

	test(linear_programming_milp_invalid_branching_rule, error(domain_error(option, branching_rule(random)))) :-
		branching_rule_problem(Problem),
		milp_branch_and_bound::solve(Problem, _Result, [branching_rule(random)]).

	test(linear_programming_milp_branch_order, deterministic((LowerUpdates == 1, UpperUpdates == 0))) :-
		branch_order_problem(Problem),
		milp_branch_and_bound::solve(Problem, LowerResult, [max_nodes(2)]),
		milp_branch_and_bound::statistics(LowerResult, LowerStatistics),
		memberchk(incumbent_updates(LowerUpdates), LowerStatistics),
		milp_branch_and_bound::solve(Problem, UpperResult, [max_nodes(2),branch_order(upper_first)]),
		milp_branch_and_bound::statistics(UpperResult, UpperStatistics),
		memberchk(incumbent_updates(UpperUpdates), UpperStatistics).

	test(linear_programming_milp_branch_order_same_optimum, deterministic((LowerX =:= 0, UpperX =:= 0, LowerValue =~= 0.0, UpperValue =~= 0.0))) :-
		branch_order_problem(Problem),
		milp_branch_and_bound::solve(Problem, LowerResult),
		milp_branch_and_bound::variable_value(LowerResult, x, LowerX),
		milp_branch_and_bound::objective_value(LowerResult, LowerValue),
		milp_branch_and_bound::solve(Problem, UpperResult, [branch_order(upper_first)]),
		milp_branch_and_bound::variable_value(UpperResult, x, UpperX),
		milp_branch_and_bound::objective_value(UpperResult, UpperValue).

	test(linear_programming_milp_invalid_branch_order, error(domain_error(option, branch_order(random)))) :-
		branch_order_problem(Problem),
		milp_branch_and_bound::solve(Problem, _Result, [branch_order(random)]).

	test(linear_programming_milp_finite_bounds_required, error(domain_error(milp_finite_integer_bounds, x-(-inf-inf)))) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, integer, -inf, inf, Problem0, Problem1),
		milp_branch_and_bound::objective([1*x], maximize, Problem1, Problem),
		milp_branch_and_bound::solve(Problem, _Result).

	test(linear_programming_milp_empty_integer_domain, error(domain_error(milp_integer_domain, x-(0.2-0.8)))) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, integer, 0.2, 0.8, Problem0, Problem1),
		milp_branch_and_bound::objective([1*x], maximize, Problem1, Problem),
		milp_branch_and_bound::solve(Problem, _Result).

	% auxiliary predicates

	sample_problem(Problem) :-
		simplex::new_problem(Problem0),
		simplex::variable(x, continuous, Problem0, Problem1),
		simplex::variable(y, continuous, Problem1, Problem2),
		simplex::constraint([1*x,2*y], =<, 14, Problem2, Problem3),
		simplex::constraint([3*x,-1*y], >=, 0, Problem3, Problem4),
		simplex::objective([3*x,4*y], maximize, Problem4, Problem).

	pivot_rule_problem(Problem) :-
		simplex::new_problem(Problem0),
		simplex::variable(x, continuous, Problem0, Problem1),
		simplex::variable(y, continuous, Problem1, Problem2),
		simplex::constraint([1*x,1*y], =<, 1, Problem2, Problem3),
		simplex::objective([1*x,2*y], maximize, Problem3, Problem).

	branching_rule_problem(Problem) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, binary, Problem0, Problem1),
		milp_branch_and_bound::variable(y, binary, Problem1, Problem2),
		milp_branch_and_bound::constraint([1*y], =<, 0.5, Problem2, Problem3),
		milp_branch_and_bound::constraint([1*x,-0.4*y], =<, 0, Problem3, Problem4),
		milp_branch_and_bound::objective([1*x,1*y], maximize, Problem4, Problem).

	branching_rule_tie_problem(Problem) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, binary, Problem0, Problem1),
		milp_branch_and_bound::variable(y, binary, Problem1, Problem2),
		milp_branch_and_bound::constraint([1*x], =<, 0.5, Problem2, Problem3),
		milp_branch_and_bound::constraint([1*y], =<, 0.5, Problem3, Problem4),
		milp_branch_and_bound::constraint([1*x,-1*y], =<, 0, Problem4, Problem5),
		milp_branch_and_bound::objective([1*x,1*y], maximize, Problem5, Problem).

	branch_order_problem(Problem) :-
		milp_branch_and_bound::new_problem(Problem0),
		milp_branch_and_bound::variable(x, binary, Problem0, Problem1),
		milp_branch_and_bound::constraint([1*x], =<, 0.5, Problem1, Problem2),
		milp_branch_and_bound::objective([1*x], maximize, Problem2, Problem).

	one_variable_problem(Lower, Upper, Constraints, Objective, Sense, Problem) :-
		simplex::new_problem(Problem0),
		simplex::variable(x, continuous, Lower, Upper, Problem0, Problem1),
		simplex::constraints(Constraints, Problem1, Problem2),
		simplex::objective(Objective, Sense, Problem2, Problem).

:- end_object.
