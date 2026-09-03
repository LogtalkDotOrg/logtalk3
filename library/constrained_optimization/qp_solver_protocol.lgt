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


:- protocol(qp_solver_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Protocol for solvers of convex quadratic programs in standard form: minimize 0.5*x^T*H*x + c^T*x subject to Aeq*x = beq and Aineq*x =< bineq. This is a numerical subroutine protocol, not a local_optimization_problem_protocol/constrained_optimization_problem_protocol solver: it operates on plain matrices and vectors, not on problem objects, since it is meant to be called once per outer/SQP iteration on a freshly linearized subproblem.',
		see_also is [qp_active_set, sqp_active_set(_)]
	]).

	:- public(solve/8).
	:- mode(solve(+list(list(number)), +list(number), +list(list(number)), +list(number), +list(list(number)), +list(number), -list(number), -list(number)), zero_or_one).
	:- info(solve/8, [
		comment is 'Solves min 0.5*x^T*H*x + c^T*x s.t. Aeq*x = beq, Aineq*x =< bineq, also returning the Lagrange multipliers Lambda at the solution: the first length(Beq) elements are the equality-constraint multipliers, followed by one element per row of Aineq/Bineq (in that order), each 0.0 if that inequality is inactive at X. H must be symmetric positive semi-definite over the feasible set for X to be a global minimizer. Fails (rather than raising an error) when no feasible point is found or the problem is unbounded below on the feasible set, since this predicate is meant to be used as a subroutine inside a larger iterative solver (such as sqp_active_set(_), which needs the multipliers to update its Hessian-of-the-Lagrangian approximation and merit-function penalty) that must react to that failure, not crash on it.',
		argnames is ['H', 'C', 'Aeq', 'Beq', 'Aineq', 'Bineq', 'X', 'Lambda']
	]).

:- end_protocol.
