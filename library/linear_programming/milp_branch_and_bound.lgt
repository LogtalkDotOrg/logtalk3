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


:- object(milp_branch_and_bound,
	imports(linear_programming_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Portable depth-first branch-and-bound solver for small mixed-integer linear programs.',
		remarks is [
			'Discrete domains' - 'Integer and binary variables must have finite bounds. No cutting planes or primal heuristics are used.',
			'LP relaxations' - 'Each search node is solved using the simplex object.',
			'Branching' - 'The solver can branch on the first fractional variable or the most fractional variable, with declaration order breaking ties. The lower or upper branch can be explored first.'
		],
		see_also is [simplex, linear_programming_protocol]
	]).

	:- uses(list, [
		memberchk/2
	]).

	solve(Problem, Result) :-
		::solve(Problem, Result, []).

	solve(Problem, Result, UserOptions) :-
		^^check_problem(Problem),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Problem = linear_program(Variables, _Constraints, Objective),
		check_solvable_problem(Variables, Objective),
		^^discrete_variables(Problem, DiscreteVariables),
		check_discrete_domains(DiscreteVariables),
		initial_state(State0),
		search_node(Problem, Options, State0, State),
		result_from_state(Problem, State, Result).

	default_option(max_nodes(10000)).
	default_option(integrality_tolerance(1.0e-9)).
	default_option(simplex_max_iterations(10000)).
	default_option(simplex_tolerance(1.0e-9)).
	default_option(simplex_pivot_rule(bland)).
	default_option(branching_rule(first_fractional)).
	default_option(branch_order(lower_first)).

	valid_option(max_nodes(MaxNodes)) :-
		integer(MaxNodes),
		MaxNodes > 0.
	valid_option(integrality_tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance > 0.
	valid_option(simplex_max_iterations(MaxIterations)) :-
		integer(MaxIterations),
		MaxIterations > 0.
	valid_option(simplex_tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance > 0.
	valid_option(simplex_pivot_rule(PivotRule)) :-
		once((PivotRule == bland; PivotRule == dantzig)).
	valid_option(branching_rule(BranchingRule)) :-
		once((BranchingRule == first_fractional; BranchingRule == most_fractional)).
	valid_option(branch_order(BranchOrder)) :-
		once((BranchOrder == lower_first; BranchOrder == upper_first)).

	check_solvable_problem([], _Objective) :-
		domain_error(linear_programming_problem, empty).
	check_solvable_problem(_Variables, none) :-
		domain_error(linear_programming_problem, missing_objective).
	check_solvable_problem([_Variable| _Variables], objective(_Expression, _Sense)).

	check_discrete_domains([]).
	check_discrete_domains([variable(Name, _Type, Lower, Upper)| Variables]) :-
		(	number(Lower), number(Upper) ->
			(	integer(Lower) ->
				IntegerLower is Lower
			;	IntegerLower is ceiling(Lower)
			),
			(	integer(Upper) ->
				IntegerUpper is Upper
			;	IntegerUpper is floor(Upper)
			),
			(	IntegerLower =< IntegerUpper ->
				check_discrete_domains(Variables)
			;	domain_error(milp_integer_domain, Name-(Lower-Upper))
			)
		;	domain_error(milp_finite_integer_bounds, Name-(Lower-Upper))
		).

	initial_state(search_state(none, [], 0, 0, 0, 0, 0, none)).

	search_node(_Problem, Options, State0, State) :-
		State0 = search_state(_Incumbent, _Values, Nodes, _Iterations, _Pruned, _Infeasible, _Updates, none),
		^^option(max_nodes(MaxNodes), Options),
		Nodes >= MaxNodes,
		!,
		set_stop_status(State0, node_limit, State).
	search_node(Problem, Options, State0, State) :-
		increment_nodes(State0, State1),
		^^relax_problem(Problem, Relaxation),
		^^option(simplex_max_iterations(MaxIterations), Options),
		^^option(simplex_tolerance(SimplexTolerance), Options),
		^^option(simplex_pivot_rule(PivotRule), Options),
		simplex::solve(Relaxation, RelaxationResult, [max_iterations(MaxIterations), tolerance(SimplexTolerance), pivot_rule(PivotRule)]),
		add_relaxation_iterations(RelaxationResult, State1, State2),
		handle_relaxation(RelaxationResult, Problem, Options, State2, State).

	handle_relaxation(linear_programming_result(infeasible, none, [], _Statistics), _Problem, _Options, State0, State) :-
		!,
		increment_infeasible(State0, State).
	handle_relaxation(linear_programming_result(iteration_limit, none, [], _Statistics), _Problem, _Options, State0, State) :-
		!,
		set_stop_status(State0, iteration_limit, State).
	handle_relaxation(linear_programming_result(numerical_error, none, [], _Statistics), _Problem, _Options, State0, State) :-
		!,
		set_stop_status(State0, numerical_error, State).
	handle_relaxation(linear_programming_result(unbounded, none, [], _Statistics), Problem, Options, State0, State) :-
		!,
		^^discrete_variables(Problem, DiscreteVariables),
		(	branchable_domain(DiscreteVariables, Name, Lower, Upper, Split) ->
			branch_on_domain(Problem, Name, Lower, Upper, Split, Options, State0, State)
		;	set_stop_status(State0, unbounded, State)
		).
	handle_relaxation(linear_programming_result(optimal, Objective, Values, _Statistics), Problem, Options, State0, State) :-
		Problem = linear_program(_Variables, _Constraints, objective(_Expression, Sense)),
		^^option(simplex_tolerance(ObjectiveTolerance), Options),
		(	bound_pruned(Sense, Objective, ObjectiveTolerance, State0) ->
			increment_pruned(State0, State)
		;	^^discrete_variables(Problem, DiscreteVariables),
			^^option(integrality_tolerance(IntegralityTolerance), Options),
			^^option(branching_rule(BranchingRule), Options),
			( fractional_variable(BranchingRule, DiscreteVariables, Values, IntegralityTolerance, Name, Lower, Upper, Value) ->
				(	integer(Value) ->
					Floor is Value,
					Ceiling is Value
				;	Floor is floor(Value),
					Ceiling is ceiling(Value)
				),
				branch_on_value(Problem, Name, Lower, Upper, Floor, Ceiling, Options, State0, State)
			;	snap_discrete_values(DiscreteVariables, Values, SnappedValues),
				evaluate_problem_objective(Problem, SnappedValues, SnappedObjective),
				update_incumbent(Sense, SnappedObjective, SnappedValues, State0, State)
			)
		).

	bound_pruned(maximize, Objective, Tolerance, search_state(Incumbent, _Values, _Nodes, _Iterations, _Pruned, _Infeasible, _Updates, _Stop)) :-
		number(Incumbent),
		Objective =< Incumbent + Tolerance.
	bound_pruned(minimize, Objective, Tolerance, search_state(Incumbent, _Values, _Nodes, _Iterations, _Pruned, _Infeasible, _Updates, _Stop)) :-
		number(Incumbent),
		Objective >= Incumbent - Tolerance.

	fractional_variable(first_fractional, Variables, Values, Tolerance, Name, Lower, Upper, Value) :-
		!,
		first_fractional_variable(Variables, Values, Tolerance, Name, Lower, Upper, Value).
	fractional_variable(most_fractional, Variables, Values, Tolerance, Name, Lower, Upper, Value) :-
		most_fractional_variable(Variables, Values, Tolerance, none, variable(Name, _Type, Lower, Upper)-Value- _Fractionality).

	first_fractional_variable([variable(Name, _Type, Lower, Upper)| _Variables], Values, Tolerance, Name, Lower, Upper, Value) :-
		memberchk(Name-Value, Values),
		fractionality(Value, Fractionality),
		Fractionality > Tolerance,
		!.
	first_fractional_variable([_Variable| Variables], Values, Tolerance, Name, Lower, Upper, Value) :-
		first_fractional_variable(Variables, Values, Tolerance, Name, Lower, Upper, Value).

	most_fractional_variable([], _Values, _Tolerance, Best, Best) :-
		Best \== none.
	most_fractional_variable([Variable| Variables], Values, Tolerance, Best0, Best) :-
		Variable = variable(Name, _Type, _Lower, _Upper),
		memberchk(Name-Value, Values),
		fractionality(Value, Fractionality),
		(	Fractionality > Tolerance ->
			more_fractional_candidate(Best0, Variable, Value, Fractionality, Best1)
		;	Best1 = Best0
		),
		most_fractional_variable(Variables, Values, Tolerance, Best1, Best).

	more_fractional_candidate(none, Variable, Value, Fractionality, Variable-Value-Fractionality) :-
		!.
	more_fractional_candidate(_BestVariable-_BestValue-BestFractionality, Variable, Value, Fractionality, Variable-Value-Fractionality) :-
		Fractionality > BestFractionality,
		!.
	more_fractional_candidate(Best, _Variable, _Value, _Fractionality, Best).

	fractionality(Value, 0) :-
		integer(Value),
		!.
	fractionality(Value, Fractionality) :-
		Nearest is round(Value),
		Fractionality is abs(Value - Nearest).

	branchable_domain([variable(Name, _Type, Lower, Upper)| _Variables], Name, Lower, Upper, Split) :-
		(	integer(Lower) ->
			IntegerLower is Lower
		;	IntegerLower is ceiling(Lower)
		),
		(	integer(Upper) ->
			IntegerUpper is Upper
		;	IntegerUpper is floor(Upper)
		),
		IntegerLower < IntegerUpper,
		!,
		Split is (IntegerLower + IntegerUpper) // 2.
	branchable_domain([_Variable| Variables], Name, Lower, Upper, Split) :-
		branchable_domain(Variables, Name, Lower, Upper, Split).

	branch_on_value(Problem, Name, Lower, Upper, Floor, Ceiling, Options, State0, State) :-
		branch_on_bounds(Problem, Name, Lower, Floor, Ceiling, Upper, Options, State0, State).

	branch_on_domain(Problem, Name, Lower, Upper, Split, Options, State0, State) :-
		Next is Split + 1,
		branch_on_bounds(Problem, Name, Lower, Split, Next, Upper, Options, State0, State).

	branch_on_bounds(Problem, Name, LeftLower, LeftUpper, RightLower, RightUpper, Options, State0, State) :-
		^^option(branch_order(BranchOrder), Options),
		branch_in_order(BranchOrder, Problem, Name, LeftLower, LeftUpper, RightLower, RightUpper, Options, State0, State).

	branch_in_order(lower_first, Problem, Name, LeftLower, LeftUpper, RightLower, RightUpper, Options, State0, State) :-
		!,
		search_optional_branch(Problem, Name, LeftLower, LeftUpper, Options, State0, State1),
		(	stopped(State1) ->
			State = State1
		;	search_optional_branch(Problem, Name, RightLower, RightUpper, Options, State1, State)
		).
	branch_in_order(upper_first, Problem, Name, LeftLower, LeftUpper, RightLower, RightUpper, Options, State0, State) :-
		search_optional_branch(Problem, Name, RightLower, RightUpper, Options, State0, State1),
		(	stopped(State1) ->
			State = State1
		;	search_optional_branch(Problem, Name, LeftLower, LeftUpper, Options, State1, State)
		).

	search_optional_branch(_Problem, _Name, Lower, Upper, _Options, State, State) :-
		Lower > Upper,
		!.
	search_optional_branch(Problem, Name, Lower, Upper, Options, State0, State) :-
		^^tighten_variable_bounds(Name, Lower, Upper, Problem, Branch),
		search_node(Branch, Options, State0, State).

	stopped(search_state(_Incumbent, _Values, _Nodes, _Iterations, _Pruned, _Infeasible, _Updates, Stop)) :-
		Stop \== none.

	snap_discrete_values([], Values, Values).
	snap_discrete_values([variable(Name, _Type, _Lower, _Upper)| Variables], Values0, Values) :-
		memberchk(Name-Value, Values0),
		(	integer(Value) ->
			Integer is Value
		;	Integer is round(Value)
		),
		replace_value(Values0, Name, Integer, Values1),
		snap_discrete_values(Variables, Values1, Values).

	replace_value([Name0-_Value| Values], Name, Integer, [Name0-Integer| Values]) :-
		Name == Name0,
		!.
	replace_value([Pair| Values0], Name, Integer, [Pair| Values]) :-
		replace_value(Values0, Name, Integer, Values).

	evaluate_problem_objective(linear_program(_Variables, _Constraints, objective(Expression, _Sense)), Values, Objective) :-
		evaluate_expression(Expression, Values, 0, Objective).

	evaluate_expression([], _Values, Objective, Objective).
	evaluate_expression([Coefficient*Name| Expression], Values, Objective0, Objective) :-
		memberchk(Name-Value, Values),
		Objective1 is Objective0 + Coefficient * Value,
		evaluate_expression(Expression, Values, Objective1, Objective).

	update_incumbent(_Sense, Objective, Values, search_state(none, _OldValues, Nodes, Iterations, Pruned, Infeasible, Updates0, Stop), search_state(Objective, Values, Nodes, Iterations, Pruned, Infeasible, Updates, Stop)) :-
		!,
		Updates is Updates0 + 1.
	update_incumbent(Sense, Objective, Values, State0, State) :-
		State0 = search_state(Incumbent, _OldValues, Nodes, Iterations, Pruned, Infeasible, Updates0, Stop),
		(	better_objective(Sense, Objective, Incumbent) ->
			Updates is Updates0 + 1,
			State = search_state(Objective, Values, Nodes, Iterations, Pruned, Infeasible, Updates, Stop)
		;	State = State0
		).

	better_objective(maximize, Objective, Incumbent) :- Objective > Incumbent.
	better_objective(minimize, Objective, Incumbent) :- Objective < Incumbent.

	add_relaxation_iterations(linear_programming_result(_Status, _Objective, _Values, Statistics), search_state(Incumbent, IncumbentValues, Nodes, Iterations0, Pruned, Infeasible, Updates, Stop), search_state(Incumbent, IncumbentValues, Nodes, Iterations, Pruned, Infeasible, Updates, Stop)) :-
		memberchk(iterations(NodeIterations), Statistics),
		Iterations is Iterations0 + NodeIterations.

	increment_nodes(search_state(Incumbent, Values, Nodes0, Iterations, Pruned, Infeasible, Updates, Stop), search_state(Incumbent, Values, Nodes, Iterations, Pruned, Infeasible, Updates, Stop)) :-
		Nodes is Nodes0 + 1.

	increment_pruned(search_state(Incumbent, Values, Nodes, Iterations, Pruned0, Infeasible, Updates, Stop), search_state(Incumbent, Values, Nodes, Iterations, Pruned, Infeasible, Updates, Stop)) :-
		Pruned is Pruned0 + 1.

	increment_infeasible(search_state(Incumbent, Values, Nodes, Iterations, Pruned, Infeasible0, Updates, Stop), search_state(Incumbent, Values, Nodes, Iterations, Pruned, Infeasible, Updates, Stop)) :-
		Infeasible is Infeasible0 + 1.

	set_stop_status(search_state(Incumbent, Values, Nodes, Iterations, Pruned, Infeasible, Updates, _Stop), Stop, search_state(Incumbent, Values, Nodes, Iterations, Pruned, Infeasible, Updates, Stop)).

	result_from_state(_Problem, search_state(_Incumbent, _Values, Nodes, Iterations, Pruned, Infeasible, Updates, Stop), linear_programming_result(Stop, none, [], Statistics)) :-
		Stop \== none,
		!,
		Statistics = [nodes(Nodes),lp_iterations(Iterations),bound_pruned_nodes(Pruned),infeasible_nodes(Infeasible),incumbent_updates(Updates)].
	result_from_state(_Problem, search_state(none, _Values, Nodes, Iterations, Pruned, Infeasible, Updates, none), linear_programming_result(infeasible, none, [], Statistics)) :-
		!,
		Statistics = [nodes(Nodes),lp_iterations(Iterations),bound_pruned_nodes(Pruned),infeasible_nodes(Infeasible),incumbent_updates(Updates)].
	result_from_state(_Problem, search_state(Objective, Values, Nodes, Iterations, Pruned, Infeasible, Updates, none), linear_programming_result(optimal, Objective, Values, Statistics)) :-
		Statistics = [nodes(Nodes),lp_iterations(Iterations),bound_pruned_nodes(Pruned),infeasible_nodes(Infeasible),incumbent_updates(Updates)].

:- end_object.
