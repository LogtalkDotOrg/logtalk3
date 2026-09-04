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


:- object(root_finding_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Root-finding tests.'
	]).

	cover(root_finder(_)).
	cover(bisection_root_finder(_)).
	cover(brent_root_finder(_)).
	cover(secant_root_finder(_)).
	cover(newton_root_finder(_)).

	:- uses(list, [
		memberchk/2
	]).

	test(bisection_default, deterministic(abs(Root - 1.4142135623730951) < 1.0e-9)) :-
		bisection_root_finder(square_minus_two)::find_root(bracket(0.0, 2.0), Root).

	test(bisection_statistics, deterministic) :-
		bisection_root_finder(square_minus_two)::find_root(bracket(0.0, 2.0), _Root, Statistics, []),
		memberchk(converged(true), Statistics),
		memberchk(termination_reason(Reason), Statistics),
		^^assertion((Reason == function_tolerance; Reason == position_tolerance)),
		memberchk(evaluations(Evaluations), Statistics),
		^^assertion(Evaluations > 2).

	test(bisection_endpoint_root, deterministic(abs(Root - 1.0) < 1.0e-15)) :-
		bisection_root_finder(endpoint_root)::find_root(bracket(1.0, 3.0), Root).

	test(bisection_iteration_limit, deterministic) :-
		bisection_root_finder(square_minus_two)::find_root(bracket(0.0, 2.0), _Root, Statistics, [tol_x(0.0), tol_f(0.0), max_iterations(2)]),
		memberchk(converged(false), Statistics),
		memberchk(termination_reason(max_iterations), Statistics),
		memberchk(iterations(2), Statistics).

	test(bisection_invalid_bracket, error(domain_error(root_bracket, bracket(0.0, 1.0)))) :-
		bisection_root_finder(no_real_root)::find_root(bracket(0.0, 1.0), _).

	test(bisection_invalid_interval, error(domain_error(root_bracket, bracket(2.0, 1.0)))) :-
		bisection_root_finder(square_minus_two)::find_root(bracket(2.0, 1.0), _).

	test(bisection_invalid_option, error(domain_error(option, tol_x(-1.0)))) :-
		bisection_root_finder(square_minus_two)::find_root(bracket(0.0, 2.0), _, [tol_x(-1.0)]).

	test(bisection_malformed_callback, error(domain_error(function_value, not_a_number))) :-
		bisection_root_finder(malformed_function)::find_root(bracket(0.0, 2.0), _).

	test(brent_default, deterministic(abs(Root - 1.4142135623730951) < 1.0e-9)) :-
		brent_root_finder(square_minus_two)::find_root(bracket(0.0, 2.0), Root).

	test(brent_preserves_bracket, deterministic((Root > 1.0, Root < 2.0))) :-
		brent_root_finder(square_minus_two)::find_root(bracket(1.0, 2.0), Root).

	test(brent_transcendental, deterministic(abs(Root - 0.7390851332151607) < 1.0e-10)) :-
		brent_root_finder(cosine_minus_identity)::find_root(bracket(0.0, 1.0), Root).

	test(brent_invalid_bracket, error(domain_error(root_bracket, bracket(0.0, 1.0)))) :-
		brent_root_finder(no_real_root)::find_root(bracket(0.0, 1.0), _).

	test(secant_default, deterministic(abs(Root - 1.4142135623730951) < 1.0e-9)) :-
		secant_root_finder(square_minus_two)::find_root(guesses(1.0, 2.0), Root).

	test(secant_zero_denominator, deterministic) :-
		secant_root_finder(no_real_root)::find_root(guesses(-1.0, 1.0), _Root, Statistics, []),
		memberchk(converged(false), Statistics),
		memberchk(termination_reason(zero_denominator), Statistics).

	test(secant_invalid_guesses, error(domain_error(root_guesses, guesses(1.0, 1.0)))) :-
		secant_root_finder(square_minus_two)::find_root(guesses(1.0, 1.0), _).

	test(newton_default, deterministic(abs(Root - 1.4142135623730951) < 1.0e-9)) :-
		newton_root_finder(square_minus_two)::find_root(guess(1.0), Root).

	test(newton_zero_derivative, deterministic) :-
		newton_root_finder(zero_derivative)::find_root(guess(0.0), _Root, Statistics, []),
		memberchk(converged(false), Statistics),
		memberchk(termination_reason(zero_derivative), Statistics).

	test(newton_missing_derivative, error(existence_error(procedure, derivative/2))) :-
		newton_root_finder(endpoint_root)::find_root(guess(2.0), _).

	test(newton_invalid_guess, error(domain_error(root_guess, guess(foo)))) :-
		newton_root_finder(square_minus_two)::find_root(guess(foo), _).

:- end_object.
