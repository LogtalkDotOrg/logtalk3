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


:- object(piecewise_linear_interpolator,
	imports((interpolator, options))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Piecewise-linear interpolation over a sorted set of distinct abscissas.',
		see_also is [interpolator_protocol, barycentric_interpolator, cubic_spline_interpolator]
	]).

	:- uses(list, [
		last/2
	]).

	fit(Points, piecewise_linear_model(Lower, Upper, Intervals), Options) :-
		^^check_options(Options),
		^^prepare_points(Points, Abscissas, Ordinates),
		Abscissas = [Lower| _],
		last(Abscissas, Upper),
		linear_intervals(Abscissas, Ordinates, Intervals).

	evaluate(Model, _, _) :-
		var(Model),
		instantiation_error.
	evaluate(piecewise_linear_model(Lower, Upper, Intervals), Argument, Value) :-
		!,
		^^check_argument(Argument, Lower, Upper),
		^^select_interval(Argument, Intervals, interval(X0, _X1, Y0, Slope)),
		Value is Y0 + Slope * (Argument - X0).
	evaluate(Model, _, _) :-
		domain_error(interpolation_model, Model).

	linear_intervals([_], [_], []) :-
		!.
	linear_intervals([X0, X1| Xs], [Y0, Y1| Ys], [interval(X0, X1, Y0, Slope)| Intervals]) :-
		Slope is (Y1 - Y0) / (X1 - X0),
		linear_intervals([X1| Xs], [Y1| Ys], Intervals).

	valid_option(_) :-
		fail.

:- end_object.
