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


:- object(barycentric_interpolator,
	imports((interpolator, options))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Barycentric polynomial interpolation with weights precomputed during fitting.',
		see_also is [interpolator_protocol, piecewise_linear_interpolator, cubic_spline_interpolator]
	]).

	:- uses(list, [
		last/2
	]).

	fit(Points, barycentric_model(Lower, Upper, Abscissas, Ordinates, Weights), Options) :-
		^^check_options(Options),
		^^prepare_points(Points, Abscissas, Ordinates),
		Abscissas = [Lower| _],
		last(Abscissas, Upper),
		weights(Abscissas, Abscissas, Weights).

	evaluate(Model, _, _) :-
		var(Model),
		instantiation_error.
	evaluate(barycentric_model(Lower, Upper, Abscissas, Ordinates, Weights), Argument, Value) :-
		!,
		^^check_argument(Argument, Lower, Upper),
		(	knot_value(Argument, Abscissas, Ordinates, Value) ->
			true
		;	barycentric_sum(Abscissas, Ordinates, Weights, Argument, 0.0, 0.0, Numerator, Denominator),
			Value is Numerator / Denominator
		).
	evaluate(Model, _, _) :-
		domain_error(interpolation_model, Model).

	weights([], _, []).
	weights([X| Xs], All, [Weight| Weights]) :-
		weight_product(All, X, 1.0, Product),
		Weight is 1.0 / Product,
		weights(Xs, All, Weights).

	weight_product([], _, Product, Product).
	weight_product([Other| Others], X, Product0, Product) :-
		(	X =:= Other ->
			Product1 = Product0
		;	Product1 is Product0 * (X - Other)
		),
		weight_product(Others, X, Product1, Product).

	knot_value(X, [Xi| _], [Yi| _], Yi) :-
		X =:= Xi,
		!.
	knot_value(X, [_| Xs], [_| Ys], Value) :-
		knot_value(X, Xs, Ys, Value).

	barycentric_sum([], [], [], _, Numerator, Denominator, Numerator, Denominator) :-
		!.
	barycentric_sum([Xi| Xs], [Yi| Ys], [Weight| Weights], X, Numerator0, Denominator0, Numerator, Denominator) :-
		Term is Weight / (X - Xi),
		Numerator1 is Numerator0 + Term * Yi,
		Denominator1 is Denominator0 + Term,
		barycentric_sum(Xs, Ys, Weights, X, Numerator1, Denominator1, Numerator, Denominator).

	valid_option(_) :-
		fail.

:- end_object.
