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


:- object(interpolation_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Interpolation tests.'
	]).

	cover(interpolator).
	cover(piecewise_linear_interpolator).
	cover(barycentric_interpolator).
	cover(cubic_spline_interpolator).

	test(piecewise_unsorted_points, deterministic(abs(Value - 3.0) < 1.0e-14)) :-
		piecewise_linear_interpolator::fit([2.0-4.0, 0.0-0.0, 1.0-2.0], Model),
		piecewise_linear_interpolator::evaluate(Model, 1.5, Value).

	test(piecewise_exact_last_knot, deterministic(abs(Value - 4.0) < 1.0e-14)) :-
		piecewise_linear_interpolator::fit([0.0-0.0, 1.0-2.0, 2.0-4.0], Model),
		piecewise_linear_interpolator::evaluate(Model, 2.0, Value).

	test(piecewise_invalid_option, error(domain_error(option, boundary(natural)))) :-
		piecewise_linear_interpolator::fit([0.0-0.0, 1.0-1.0], _, [boundary(natural)]).

	test(interpolation_duplicate_abscissa, error(domain_error(duplicate_abscissa, 1.0))) :-
		piecewise_linear_interpolator::fit([0.0-0.0, 1.0-1.0, 1.0-2.0], _).

	test(interpolation_insufficient_points, error(domain_error(interpolation_points, [0.0-0.0]))) :-
		piecewise_linear_interpolator::fit([0.0-0.0], _).

	test(interpolation_non_numeric_point, error(domain_error(interpolation_points, [0.0-0.0, 1.0-foo]))) :-
		piecewise_linear_interpolator::fit([0.0-0.0, 1.0-foo], _).

	test(interpolation_outside_domain, error(domain_error(interpolation_domain, 3.0))) :-
		piecewise_linear_interpolator::fit([0.0-0.0, 2.0-4.0], Model),
		piecewise_linear_interpolator::evaluate(Model, 3.0, _).

	test(interpolation_wrong_model, error(domain_error(interpolation_model, wrong_model))) :-
		piecewise_linear_interpolator::evaluate(wrong_model, 1.0, _).

	test(interpolation_cross_model_rejected, error(domain_error(interpolation_model, _))) :-
		barycentric_interpolator::fit([0.0-0.0, 1.0-1.0], Model),
		piecewise_linear_interpolator::evaluate(Model, 0.5, _).

	test(barycentric_quadratic, deterministic(abs(Value - 2.25) < 1.0e-13)) :-
		barycentric_interpolator::fit([0.0-0.0, 1.0-1.0, 2.0-4.0], Model),
		barycentric_interpolator::evaluate(Model, 1.5, Value).

	test(barycentric_exact_knot, deterministic(abs(Value - 1.0) < 1.0e-14)) :-
		barycentric_interpolator::fit([0.0-0.0, 1.0-1.0, 2.0-4.0], Model),
		barycentric_interpolator::evaluate(Model, 1.0, Value).

	test(barycentric_invalid_option, error(domain_error(option, boundary(natural)))) :-
		barycentric_interpolator::fit([0.0-0.0, 1.0-1.0], _, [boundary(natural)]).

	test(natural_spline_linear, deterministic(abs(Value - 3.0) < 1.0e-13)) :-
		cubic_spline_interpolator::fit([0.0-0.0, 1.0-2.0, 2.0-4.0], Model),
		cubic_spline_interpolator::evaluate(Model, 1.5, Value).

	test(natural_spline_endpoint_second_derivatives, deterministic) :-
		cubic_spline_interpolator::fit([0.0-0.0, 1.0-1.0, 2.0-4.0], Model),
		cubic_spline_interpolator::derivative(Model, 0.0, 2, First),
		cubic_spline_interpolator::derivative(Model, 2.0, 2, Last),
		^^assertion(abs(First) < 1.0e-13),
		^^assertion(abs(Last) < 1.0e-13).

	test(clamped_spline_reproduces_cubic, deterministic) :-
		cubic_spline_interpolator::fit(
			[0.0-0.0, 1.0-1.0, 2.0-8.0, 3.0-27.0], Model,
			[boundary(clamped(0.0, 27.0))]
		),
		cubic_spline_interpolator::evaluate(Model, 1.5, Value),
		cubic_spline_interpolator::derivative(Model, 1.5, 1, Derivative),
		^^assertion(abs(Value - 3.375) < 1.0e-12),
		^^assertion(abs(Derivative - 6.75) < 1.0e-12).

	test(cubic_spline_first_derivative_continuity, deterministic(abs(Left - Right) < 1.0e-6)) :-
		cubic_spline_interpolator::fit([0.0-0.0, 1.0-1.0, 2.0-0.0], Model),
		cubic_spline_interpolator::derivative(Model, 0.9999999, 1, Left),
		cubic_spline_interpolator::derivative(Model, 1.0000001, 1, Right).

	test(cubic_spline_invalid_boundary, error(domain_error(option, boundary(periodic)))) :-
		cubic_spline_interpolator::fit([0.0-0.0, 1.0-1.0], _, [boundary(periodic)]).

	test(cubic_spline_invalid_derivative_order, error(domain_error(derivative_order, 3))) :-
		cubic_spline_interpolator::fit([0.0-0.0, 1.0-1.0], Model),
		cubic_spline_interpolator::derivative(Model, 0.5, 3, _).

:- end_object.
