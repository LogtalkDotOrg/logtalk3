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


:- object(sqp_test_quadratic_equality,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'minimize x1^2 + x2^2 subject to x1 + x2 = 1. Objective is quadratic and the constraint linear, so the first QP subproblem is exact: sqp_active_set(_) should converge in a single iteration. Global minimum at (0.5, 0.5), value 0.5.'
	]).

	initial_point([0.0, 0.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is X1*X1 + X2*X2.

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*X1,
		G2 is 2.0*X2,
		Gradient = [G1, G2].

	equality_constraints(Point, Values) :-
		Point = [X1, X2],
		G1 is X1 + X2 - 1.0,
		Values = [G1].

	equality_jacobian(_Point, [[1.0, 1.0]]).

:- end_object.


:- object(constrained_progress_test,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Equality-constrained quadratic recording outer and inner progress callbacks.'
	]).

	:- public(clear_log/0).

	:- public(outer_progress_log/5).

	:- public(inner_progress_log/6).

	:- private(outer_log_/5).
	:- dynamic(outer_log_/5).

	:- private(inner_log_/6).
	:- dynamic(inner_log_/6).

	initial_point([0.0, 0.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is X1*X1 + X2*X2.

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*X1,
		G2 is 2.0*X2,
		Gradient = [G1, G2].

	equality_constraints(Point, Values) :-
		Point = [X1, X2],
		Value is X1 + X2 - 1.0,
		Values = [Value].

	equality_jacobian(_, [[1.0, 1.0]]).

	inequality_constraints(Point, Values) :-
		Point = [X1, _],
		Value is X1 - 2.0,
		Values = [Value].

	inequality_jacobian(_, [[1.0, 0.0]]).

	progress(Iteration, Point, Value, Measure, Evaluations) :-
		assertz(outer_log_(Iteration, Point, Value, Measure, Evaluations)).

	inner_progress(Stage, Iteration, Point, Value, Measure, Evaluations) :-
		assertz(inner_log_(Stage, Iteration, Point, Value, Measure, Evaluations)).

	clear_log :-
		retractall(outer_log_(_, _, _, _, _)),
		retractall(inner_log_(_, _, _, _, _, _)).

	outer_progress_log(Iteration, Point, Value, Measure, Evaluations) :-
		outer_log_(Iteration, Point, Value, Measure, Evaluations).

	inner_progress_log(Stage, Iteration, Point, Value, Measure, Evaluations) :-
		inner_log_(Stage, Iteration, Point, Value, Measure, Evaluations).

:- end_object.


:- object(initial_point_override_test,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Unconstrained quadratic whose problem-defined initial point raises an error, allowing tests to verify that an initial_point/1 option takes precedence.'
	]).

	initial_point(_) :-
		domain_error(unexpected_problem_initial_point, initial_point).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is X1*X1 + X2*X2.

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*X1,
		G2 is 2.0*X2,
		Gradient = [G1, G2].

:- end_object.


:- object(initial_point_fallback_test,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Unconstrained quadratic with a distinctive problem-defined initial point for fallback tests.'
	]).

	initial_point([1.0, -1.0]).

	objective(_, 2.0).

	gradient(_, [0.0, 0.0]).

:- end_object.


:- object(empty_constraints_test,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Unconstrained quadratic that explicitly defines empty equality and inequality vectors and Jacobians.'
	]).

	initial_point([1.0, -1.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is X1*X1 + X2*X2.

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*X1,
		G2 is 2.0*X2,
		Gradient = [G1, G2].

	equality_constraints(_, []).

	equality_jacobian(_, []).

	inequality_constraints(_, []).

	inequality_jacobian(_, []).

:- end_object.


:- object(box_only_test,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'One-dimensional box-only problem with unconstrained minimizer 2.0 and constrained minimizer 1.0.'
	]).

	initial_point([0.0]).

	position_bounds([0.0-1.0]).

	objective(Point, Value) :-
		Point = [X],
		Value is (X-2.0)*(X-2.0).

	gradient(Point, Gradient) :-
		Point = [X],
		G is 2.0*(X-2.0),
		Gradient = [G].

:- end_object.


:- object(inconsistent_gradient_test,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Quadratic objective with a deliberately reversed gradient, forcing SQP Armijo backtracking to exhaust its budget.'
	]).

	initial_point([1.0]).

	objective(Point, Value) :-
		Point = [X],
		Value is X*X.

	gradient(Point, Gradient) :-
		Point = [X],
		G is -2.0*X,
		Gradient = [G].

:- end_object.


:- object(stop_condition_test,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Unconstrained quadratic with an immediate stop condition for termination-reason tests.'
	]).

	initial_point([1.0, 1.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is X1*X1 + X2*X2.

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*X1,
		G2 is 2.0*X2,
		Gradient = [G1, G2].

	stop_condition(0, _, _).

:- end_object.


:- object(invalid_bounds_test,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Two-dimensional quadratic with a one-dimensional bounds list for validation tests.'
	]).

	initial_point([0.0, 0.0]).

	position_bounds([0.0-1.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is X1*X1 + X2*X2.

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*X1,
		G2 is 2.0*X2,
		Gradient = [G1, G2].

:- end_object.


:- object(invalid_constraint_values_test,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Malformed problem returning a nonnumeric equality-constraint value for validation tests.'
	]).

	initial_point([0.0, 0.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is X1*X1 + X2*X2.

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*X1,
		G2 is 2.0*X2,
		Gradient = [G1, G2].

	equality_constraints(_, [invalid]).

	equality_jacobian(_, [[1.0, 1.0]]).

:- end_object.


:- object(invalid_constraint_jacobian_test,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Malformed problem returning a wrong-width equality Jacobian row for validation tests.'
	]).

	initial_point([0.0, 0.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is X1*X1 + X2*X2.

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*X1,
		G2 is 2.0*X2,
		Gradient = [G1, G2].

	equality_constraints(_, [0.0]).

	equality_jacobian(_, [[1.0]]).

:- end_object.


:- object(sqp_test_circle,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'minimize x1 + x2 subject to x1^2 + x2^2 = 1. A genuinely nonlinear constraint (curved Jacobian), needing multiple SQP iterations. Global minimum at (-1/sqrt(2), -1/sqrt(2)), value -sqrt(2).'
	]).

	initial_point([1.0, 0.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is X1 + X2.

	gradient(_Point, Gradient) :-
		Gradient = [1.0, 1.0].

	equality_constraints(Point, Values) :-
		Point = [X1, X2],
		G1 is X1*X1 + X2*X2 - 1.0,
		Values = [G1].

	equality_jacobian(Point, Jacobian) :-
		Point = [X1, X2],
		J1 is 2.0*X1,
		J2 is 2.0*X2,
		Jacobian = [[J1, J2]].

:- end_object.


:- object(sqp_test_inequality_bounds,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'minimize (x1-2)^2 + (x2-2)^2 subject to x1 + x2 =< 2, plus generous box bounds x1, x2 in [0, 1000]. Exercises a general inequality constraint alongside position_bounds/1 in the same QP subproblem; both objective and constraint are already quadratic/linear, so this also converges in one iteration. Global minimum at (1, 1), value 2.'
	]).

	initial_point([0.0, 0.0]).

	position_bounds([0.0-1000.0, 0.0-1000.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is (X1-2.0)*(X1-2.0) + (X2-2.0)*(X2-2.0).

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*(X1-2.0),
		G2 is 2.0*(X2-2.0),
		Gradient = [G1, G2].

	inequality_constraints(Point, Values) :-
		Point = [X1, X2],
		H1 is X1 + X2 - 2.0,
		Values = [H1].

	inequality_jacobian(_Point, [[1.0, 1.0]]).

:- end_object.


:- object(sqp_test_maximize,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'maximize -(x1^2 + x2^2) subject to x1 + x2 = 1: the same feasible region and the same optimal point as sqp_test_quadratic_equality, reached by maximizing instead of minimizing, to exercise the objective(maximize) option. Global maximum at (0.5, 0.5), value -0.5.'
	]).

	initial_point([0.0, 0.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is -(X1*X1 + X2*X2).

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is -2.0*X1,
		G2 is -2.0*X2,
		Gradient = [G1, G2].

	equality_constraints(Point, Values) :-
		Point = [X1, X2],
		G1 is X1 + X2 - 1.0,
		Values = [G1].

	equality_jacobian(_Point, [[1.0, 1.0]]).

:- end_object.


:- object(al_test_inequality_bounds,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'minimize (x1-2)^2 + (x2-2)^2 subject to x1 + x2 =< 2, plus box bounds x1, x2 in [0, 1000]. The initial point [0,0] already satisfies the inequality constraint (it is inactive there), which is exactly the case that exposed an early version of augmented_lagrangian(_,_) stopping before doing any work. Global minimum at (1, 1), value 2.'
	]).

	initial_point([0.0, 0.0]).

	position_bounds([0.0-1000.0, 0.0-1000.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is (X1-2.0)*(X1-2.0) + (X2-2.0)*(X2-2.0).

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*(X1-2.0),
		G2 is 2.0*(X2-2.0),
		Gradient = [G1, G2].

	inequality_constraints(Point, Values) :-
		Point = [X1, X2],
		H1 is X1 + X2 - 2.0,
		Values = [H1].

	inequality_jacobian(_Point, [[1.0, 1.0]]).

:- end_object.


:- object(lb_test_eq_and_ineq,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'minimize 0.5*(x1^2+x2^2+x3^2) - x3 subject to x1+x2+x3 = 3, x1 >= 0, x2 >= 0, x3 =< 1. Combines an equality constraint with inequality constraints in the same problem, for solvers (log_barrier(_,_)) that treat the two differently. The starting point [0.5,0.5,0.5] is strictly feasible with respect to the inequalities (as log_barrier(_,_) requires) but does not satisfy the equality exactly. Global minimum at (1, 1, 1), value 0.5.'
	]).

	initial_point([0.5, 0.5, 0.5]).

	objective(Point, Value) :-
		Point = [X1, X2, X3],
		Value is 0.5*(X1*X1 + X2*X2 + X3*X3) - X3.

	gradient(Point, Gradient) :-
		Point = [X1, X2, X3],
		G3 is X3 - 1.0,
		Gradient = [X1, X2, G3].

	equality_constraints(Point, Values) :-
		Point = [X1, X2, X3],
		G is X1 + X2 + X3 - 3.0,
		Values = [G].

	equality_jacobian(_Point, [[1.0, 1.0, 1.0]]).

	inequality_constraints(Point, Values) :-
		Point = [X1, X2, X3],
		H1 is -X1,
		H2 is -X2,
		H3 is X3 - 1.0,
		Values = [H1, H2, H3].

	inequality_jacobian(_Point, [[-1.0,0.0,0.0], [0.0,-1.0,0.0], [0.0,0.0,1.0]]).

:- end_object.


:- object(lb_test_infeasible_start,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Same feasible region as al_test_inequality_bounds (x1 + x2 =< 2) but initial_point/1 is [3,3], which violates it (h = 4 > 0), to exercise log_barrier(_,_) raising domain_error(strictly_feasible_initial_point, _) rather than proceeding.'
	]).

	initial_point([3.0, 3.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is (X1-2.0)*(X1-2.0) + (X2-2.0)*(X2-2.0).

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*(X1-2.0),
		G2 is 2.0*(X2-2.0),
		Gradient = [G1, G2].

	inequality_constraints(Point, Values) :-
		Point = [X1, X2],
		H1 is X1 + X2 - 2.0,
		Values = [H1].

	inequality_jacobian(_Point, [[1.0, 1.0]]).

:- end_object.


:- object(sqp_test_missing_jacobian,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Defines equality_constraints/2 but deliberately not equality_jacobian/2, to exercise sqp_active_set(_) raising existence_error(procedure, equality_jacobian/2) instead of silently proceeding without a Jacobian.'
	]).

	initial_point([0.0, 0.0]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is X1*X1 + X2*X2.

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*X1,
		G2 is 2.0*X2,
		Gradient = [G1, G2].

	equality_constraints(Point, Values) :-
		Point = [X1, X2],
		G1 is X1 + X2 - 1.0,
		Values = [G1].

:- end_object.


:- object(lb_test_genuinely_infeasible,
	implements(constrained_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'x1 >= 1 and x1 =< 0 simultaneously: an empty feasible set, so no starting point (feasible or not) can ever be rescued by a phase-1 heuristic. Exercises log_barrier(_,_) still correctly raising domain_error(strictly_feasible_initial_point, _) when phase 1 itself cannot succeed, as opposed to lb_test_infeasible_start, whose feasible set is non-empty and which phase 1 does successfully rescue.'
	]).

	initial_point([0.5, 0.5]).

	objective(Point, Value) :-
		Point = [X1, X2],
		Value is X1*X1 + X2*X2.

	gradient(Point, Gradient) :-
		Point = [X1, X2],
		G1 is 2.0*X1,
		G2 is 2.0*X2,
		Gradient = [G1, G2].

	inequality_constraints(Point, Values) :-
		Point = [X1, _],
		H1 is 1.0 - X1,
		H2 is X1,
		Values = [H1, H2].

	inequality_jacobian(_Point, [[-1.0,0.0], [1.0,0.0]]).

:- end_object.
