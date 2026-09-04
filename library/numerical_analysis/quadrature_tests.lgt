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


:- object(quadrature_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Quadrature tests.'
	]).

	cover(quadrature(_)).
	cover(adaptive_simpson_quadrature(_)).
	cover(gauss_legendre_quadrature(_)).

	:- uses(list, [memberchk/2]).

	test(adaptive_simpson_sine, deterministic(abs(Integral - 2.0) < 1.0e-9)) :-
		Pi is acos(-1.0),
		adaptive_simpson_quadrature(sine_function)::integrate(0.0, Pi, Integral).

	test(adaptive_simpson_cubic, deterministic(abs(Integral - 0.25) < 1.0e-12)) :-
		adaptive_simpson_quadrature(cubic_function)::integrate(0.0, 1.0, Integral).

	test(adaptive_simpson_reversed, deterministic(abs(Integral + 0.25) < 1.0e-12)) :-
		adaptive_simpson_quadrature(cubic_function)::integrate(1.0, 0.0, Integral).

	test(adaptive_simpson_zero_interval, deterministic) :-
		adaptive_simpson_quadrature(malformed_function)::integrate(2.0, 2.0, Integral, Statistics, []),
		^^assertion(Integral =:= 0.0),
		memberchk(evaluations(0), Statistics),
		memberchk(termination_reason(zero_interval), Statistics).

	test(adaptive_simpson_statistics, deterministic) :-
		Pi is acos(-1.0),
		adaptive_simpson_quadrature(sine_function)::integrate(0.0, Pi, _Integral, Statistics, []),
		memberchk(converged(true), Statistics),
		memberchk(evaluations(Evaluations), Statistics),
		^^assertion(Evaluations >= 5),
		memberchk(estimated_error(Error), Statistics),
		^^assertion(Error >= 0.0).

	test(adaptive_simpson_max_subdivisions, deterministic) :-
		Pi is acos(-1.0),
		adaptive_simpson_quadrature(sine_function)::integrate(0.0, Pi, _Integral, Statistics, [tol_abs(0.0), tol_rel(0.0), max_subdivisions(1)]),
		memberchk(converged(false), Statistics),
		memberchk(termination_reason(max_subdivisions), Statistics).

	test(adaptive_simpson_invalid_bound, error(type_error(number, foo))) :-
		adaptive_simpson_quadrature(sine_function)::integrate(foo, 1.0, _).

	test(adaptive_simpson_invalid_option, error(domain_error(option, max_subdivisions(0)))) :-
		adaptive_simpson_quadrature(sine_function)::integrate(0.0, 1.0, _, [max_subdivisions(0)]).

	test(gauss_legendre_cubic_exact, deterministic(abs(Integral - 0.25) < 1.0e-14)) :-
		gauss_legendre_quadrature(cubic_function)::integrate(0.0, 1.0, Integral, [order(2)]).

	test(gauss_legendre_sine, deterministic(abs(Integral - 2.0) < 1.0e-14)) :-
		Pi is acos(-1.0),
		gauss_legendre_quadrature(sine_function)::integrate(0.0, Pi, Integral, [order(16)]).

	test(gauss_legendre_default_order, deterministic) :-
		gauss_legendre_quadrature(cubic_function)::integrate(0.0, 1.0, Integral, Statistics, []),
		^^assertion(abs(Integral - 0.25) < 1.0e-14),
		memberchk(order(8), Statistics),
		memberchk(estimated_error(unavailable), Statistics).

	test(gauss_legendre_reversed, deterministic(abs(Integral + 0.25) < 1.0e-14)) :-
		gauss_legendre_quadrature(cubic_function)::integrate(1.0, 0.0, Integral, [order(4)]).

	test(gauss_legendre_invalid_order, error(domain_error(option, order(3)))) :-
		gauss_legendre_quadrature(cubic_function)::integrate(0.0, 1.0, _, [order(3)]).

	test(quadrature_malformed_callback, error(domain_error(function_value, not_a_number))) :-
		adaptive_simpson_quadrature(malformed_function)::integrate(0.0, 1.0, _).

:- end_object.
