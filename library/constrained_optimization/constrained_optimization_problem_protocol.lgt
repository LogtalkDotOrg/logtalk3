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


:- protocol(constrained_optimization_problem_protocol,
	extends(local_optimization_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Extends ``local_optimization_problem_protocol`` with general equality and inequality constraints. Box constraints continue to be expressed via the inherited ``position_bounds/1``. A problem that defines neither ``equality_constraints/2`` nor ``inequality_constraints/2`` is a plain ``local_optimization_problem_protocol`` problem and can still be solved by any ``constrained_optimization`` solver.',
		remarks is [
			'Equality constraints' - '``g(x) = 0``, one component per row of ``equality_constraints/2``.',
			'Inequality constraints' - '``h(x) =< 0``, one component per row of ``inequality_constraints/2``. Constraints of the form ``h(x) >= 0`` or ``a =< h(x) =< b`` must be restated in this form by the problem (e.g. negate, or split into two rows) before being reported here.',
			'Jacobians' - '``equality_jacobian/2`` and ``inequality_jacobian/2`` are required by ``sqp_active_set(_)`` and ``primal_dual_interior_point(_)``, and by ``augmented_lagrangian(_,_)``, ``quadratic_penalty(_,_)``, and ``log_barrier(_,_)`` when their selected inner solver uses gradients. Solvers that need a Jacobian a problem does not define raise an existence error rather than silently falling back to finite differences.'
		],
		see_also is [
			local_optimization_problem_protocol, qp_active_set, sqp_active_set(_), augmented_lagrangian(_, _),
			quadratic_penalty(_, _), log_barrier(_, _), primal_dual_interior_point(_)
		]
	]).

	:- public(equality_constraints/2).
	:- mode(equality_constraints(+list(number), -list(number)), zero_or_one).
	:- info(equality_constraints/2, [
		comment is 'Computes ``g(x)``, the vector of equality constraint values at a point; feasibility requires every component to equal zero. Optional: when not defined, the problem has no equality constraints.',
		argnames is ['Point', 'Values']
	]).

	:- public(equality_jacobian/2).
	:- mode(equality_jacobian(+list(number), -list(list(number))), zero_or_one).
	:- info(equality_jacobian/2, [
		comment is 'Computes the Jacobian of ``equality_constraints/2`` at a point, one row per constraint, one column per variable. Optional unless required by the solver in use (see the "Jacobians" remark above).',
		argnames is ['Point', 'Jacobian']
	]).

	:- public(inequality_constraints/2).
	:- mode(inequality_constraints(+list(number), -list(number)), zero_or_one).
	:- info(inequality_constraints/2, [
		comment is 'Computes ``h(x)``, the vector of inequality constraint values at a point; feasibility requires every component to be ``=< 0``. Optional: when not defined, the problem has no general inequality constraints (position_bounds/1 may still apply).',
		argnames is ['Point', 'Values']
	]).

	:- public(inequality_jacobian/2).
	:- mode(inequality_jacobian(+list(number), -list(list(number))), zero_or_one).
	:- info(inequality_jacobian/2, [
		comment is 'Computes the Jacobian of ``inequality_constraints/2`` at a point. Optional unless required by the solver in use (see the "Jacobians" remark above).',
		argnames is ['Point', 'Jacobian']
	]).

	:- public(inner_progress/6).
	:- mode(inner_progress(+term, +non_negative_integer, +list(number), +number, +number, +non_negative_integer), zero_or_one).
	:- info(inner_progress/6, [
		comment is 'Optional callback reporting progress from an inner solver used by a delegated constrained solver. The stage is ``outer(N)`` for an outer iteration or ``phase1`` for a feasibility search. The value and measure are those of the transformed inner subproblem.',
		argnames is ['Stage', 'Iteration', 'Point', 'Value', 'Measure', 'Evaluations']
	]).

:- end_protocol.
