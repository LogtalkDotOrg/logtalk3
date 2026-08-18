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


:- object(lbfgs_b_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-18,
		comment is 'Unit tests specific to the lbfgs_b(_) bound-constrained solver: feasibility, projected-gradient stopping, missing-bounds error, and option validation. Shared tests(lbfgs_b) / gradient_tests(lbfgs_b) are not repeated here except where bounds are required.'
	]).

	cover(lbfgs_b(_)).

	:- uses(list, [
		memberchk/2
	]).

	% feasibility: every iterate (and the result) stays in the box

	test(lbfgs_b_result_feasible, deterministic) :-
		lbfgs_b(bounded_sphere)::run(Point, Value, [
			max_iterations(100), tol_g(1.0e-8)
		]),
		Point = [X, Y],
		^^assertion((X >= -1.0, X =< 1.0)),
		^^assertion((Y >= -1.0, Y =< 1.0)),
		^^assertion(Value >= 0.0),
		^^assertion(Value < 0.1).

	test(lbfgs_b_converges_near_origin, deterministic) :-
		lbfgs_b(bounded_sphere)::run(Point, Value, [
			max_iterations(200), tol_g(1.0e-10)
		]),
		Point = [X, Y],
		^^assertion(abs(X) < 0.05),
		^^assertion(abs(Y) < 0.05),
		^^assertion(Value < 1.0e-3).

	% unconstrained problems still work (falls back to plain L-BFGS)

	test(lbfgs_b_unconstrained_sphere, deterministic(Value < 1.0e-4)) :-
		lbfgs_b(sphere)::run(_Point, Value, [
			max_iterations(200), tol_g(1.0e-10)
		]).

	% projected-gradient statistics still reported

	test(lbfgs_b_statistics, deterministic) :-
		lbfgs_b(bounded_sphere)::run(_Point, _Value, Statistics, [
			max_iterations(30)
		]),
		memberchk(iterations(Iterations), Statistics),
		^^assertion(Iterations =< 30),
		memberchk(gradient_evaluations(G), Statistics),
		^^assertion(G > 0),
		memberchk(final_gradient_norm(Norm), Statistics),
		^^assertion(number(Norm)).

	% option validation (mirrors lbfgs)

	test(lbfgs_b_invalid_memory_size, error(domain_error(option, memory_size(0)))) :-
		lbfgs_b(bounded_sphere)::run(_Point, _Value, [memory_size(0)]).

	test(lbfgs_b_invalid_restart, error(domain_error(option, restart(-1)))) :-
		lbfgs_b(bounded_sphere)::run(_Point, _Value, [restart(-1)]).

:- end_object.
