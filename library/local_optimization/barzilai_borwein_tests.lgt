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


:- object(barzilai_borwein_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Unit tests specific to the barzilai_borwein(_) solver: formula variants, line-search modes, step clamping, and option validation. The shared tests(barzilai_borwein) and gradient_tests(barzilai_borwein) suites cover the common solver API and are not repeated here.'
	]).

	cover(barzilai_borwein(_)).

	% formula variants all converge on the sphere

	test(bb_formula_bb1_converges, deterministic(Value < 1.0e-6)) :-
		barzilai_borwein(sphere)::run(_Point, Value, [
			formula(bb1), max_iterations(100), tol_g(1.0e-10)
		]).

	test(bb_formula_bb2_converges, deterministic(Value < 1.0e-6)) :-
		barzilai_borwein(sphere)::run(_Point, Value, [
			formula(bb2), max_iterations(100), tol_g(1.0e-10)
		]).

	test(bb_formula_alternate_converges, deterministic(Value < 1.0e-6)) :-
		barzilai_borwein(sphere)::run(_Point, Value, [
			formula(alternate), max_iterations(100), tol_g(1.0e-10)
		]).

	% pure BB must not stop after a zero-change first step on the sphere
	% ([3,4] --alpha=1--> [-3,-4] keeps f=25); the solver must continue

	test(bb_survives_zero_change_first_step, deterministic(Value < 1.0)) :-
		barzilai_borwein(sphere)::run(_Point, Value, [
			formula(bb1), line_search(none), step_size(1.0),
			max_iterations(50), tol_g(1.0e-8)
		]).

	% optional Armijo line search

	test(bb_armijo_line_search_converges, deterministic(Value < 1.0e-4)) :-
		barzilai_borwein(sphere)::run(_Point, Value, [
			line_search(armijo), max_iterations(100), tol_g(1.0e-8)
		]).

	test(bb_armijo_rosenbrock_improves, deterministic(Value < 5.0)) :-
		barzilai_borwein(rosenbrock)::run(_Point, Value, [
			line_search(armijo), max_iterations(500), tol_g(1.0e-6)
		]).

	% step clamping: a tiny step_max forces very short steps but should
	% still make progress under a generous iteration budget

	test(bb_step_max_clamping, deterministic(Value < 20.0)) :-
		barzilai_borwein(sphere)::run(_Point, Value, [
			step_max(0.1), max_iterations(500), tol_g(1.0e-6)
		]).

	% maximization with alternate formula

	test(bb_maximize_alternate, deterministic(Value > -1.0e-4)) :-
		barzilai_borwein(negative_sphere)::run(_Point, Value, [
			objective(maximize), formula(alternate),
			max_iterations(100), tol_g(1.0e-8)
		]).

	% option validation

	test(bb_invalid_formula, error(domain_error(option, formula(bb3)))) :-
		barzilai_borwein(sphere)::run(_Point, _Value, [formula(bb3)]).

	test(bb_invalid_step_min, error(domain_error(option, step_min(-1.0)))) :-
		barzilai_borwein(sphere)::run(_Point, _Value, [step_min(-1.0)]).

	test(bb_invalid_step_max, error(domain_error(option, step_max(0.0)))) :-
		barzilai_borwein(sphere)::run(_Point, _Value, [step_max(0.0)]).

	test(bb_invalid_step_range, error(domain_error(option, step_min(2.0)))) :-
		barzilai_borwein(sphere)::run(_Point, _Value, [step_min(2.0), step_max(1.0)]).

	test(bb_invalid_line_search, error(domain_error(option, line_search(wolfe)))) :-
		barzilai_borwein(sphere)::run(_Point, _Value, [line_search(wolfe)]).

:- end_object.
