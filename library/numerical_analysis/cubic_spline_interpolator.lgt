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


:- object(cubic_spline_interpolator,
	imports([interpolator, options])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Natural and clamped cubic-spline interpolation using a tridiagonal solve.',
		see_also is [interpolator_protocol, piecewise_linear_interpolator, barycentric_interpolator]
	]).

	:- public(derivative/4).
	:- mode(derivative(+compound, +number, +integer, -number), one_or_error).
	:- info(derivative/4, [
		comment is 'Evaluates the first or second derivative of a fitted cubic spline.',
		argnames is ['Model', 'Argument', 'Order', 'Value'],
		exceptions is [
			'``Order`` is not one or two' - domain_error(derivative_order, 'Order')
		]
	]).

	:- uses(list, [
		append/3, last/2, reverse/2
	]).

	fit(Points, cubic_spline_model(Lower, Upper, Intervals), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(boundary(Boundary), Options),
		^^prepare_points(Points, Abscissas, Ordinates),
		Abscissas = [Lower| _],
		last(Abscissas, Upper),
		differences(Abscissas, Ordinates, Steps, Slopes),
		tridiagonal_system(Boundary, Steps, Slopes, LowerDiagonal, Diagonal, UpperDiagonal, RightHandSide),
		solve_tridiagonal(LowerDiagonal, Diagonal, UpperDiagonal, RightHandSide, SecondDerivatives),
		spline_intervals(Abscissas, Ordinates, SecondDerivatives, Intervals).

	evaluate(Model, _, _) :-
		var(Model),
		instantiation_error.
	evaluate(cubic_spline_model(Lower, Upper, Intervals), Argument, Value) :-
		!,
		^^check_argument(Argument, Lower, Upper),
		^^select_interval(Argument, Intervals, interval(X0, _X1, A, B, C, D)),
		Offset is Argument - X0,
		Value is A + Offset * (B + Offset * (C + Offset * D)).
	evaluate(Model, _, _) :-
		domain_error(interpolation_model, Model).

	derivative(cubic_spline_model(Lower, Upper, Intervals), Argument, Order, Value) :-
		!,
		^^check_argument(Argument, Lower, Upper),
		(	Order == 1 ->
			^^select_interval(Argument, Intervals, interval(X0, _X1, _A, B, C, D)),
			Offset is Argument - X0,
			Value is B + Offset * (2.0 * C + 3.0 * D * Offset)
		;	Order == 2 ->
			^^select_interval(Argument, Intervals, interval(X0, _X1, _A, _B, C, D)),
			Offset is Argument - X0,
			Value is 2.0 * C + 6.0 * D * Offset
		;	domain_error(derivative_order, Order)
		).
	derivative(Model, _, _, _) :-
		domain_error(interpolation_model, Model).

	differences([_], [_], [], []) :-
		!.
	differences([X0, X1| Xs], [Y0, Y1| Ys], [Step| Steps], [Slope| Slopes]) :-
		Step is X1 - X0,
		Slope is (Y1 - Y0) / Step,
		differences([X1| Xs], [Y1| Ys], Steps, Slopes).

	tridiagonal_system(natural, Steps, Slopes, Lower, Diagonal, Upper, RightHandSide) :-
		interior_system(Steps, Slopes, InteriorDiagonal, InteriorRightHandSide),
		replace_last(Steps, 0.0, Lower),
		Steps = [_| TailSteps],
		Upper = [0.0| TailSteps],
		append(InteriorDiagonal, [1.0], DiagonalTail),
		Diagonal = [1.0| DiagonalTail],
		append(InteriorRightHandSide, [0.0], RightTail),
		RightHandSide = [0.0| RightTail].
	tridiagonal_system(clamped(FirstDerivative, LastDerivative), Steps, Slopes, Steps, Diagonal, Steps, RightHandSide) :-
		Steps = [FirstStep| _],
		Slopes = [FirstSlope| _],
		last(Steps, LastStep),
		last(Slopes, LastSlope),
		interior_system(Steps, Slopes, InteriorDiagonal, InteriorRightHandSide),
		FirstDiagonal is 2.0 * FirstStep,
		LastDiagonal is 2.0 * LastStep,
		append(InteriorDiagonal, [LastDiagonal], DiagonalTail),
		Diagonal = [FirstDiagonal| DiagonalTail],
		FirstRight is 6.0 * (FirstSlope - FirstDerivative),
		LastRight is 6.0 * (LastDerivative - LastSlope),
		append(InteriorRightHandSide, [LastRight], RightTail),
		RightHandSide = [FirstRight| RightTail].

	interior_system([_], [_], [], []) :-
		!.
	interior_system([FirstStep, SecondStep| Steps], [FirstSlope, SecondSlope| Slopes], [Diagonal| Diagonals], [Right| Rights]) :-
		Diagonal is 2.0 * (FirstStep + SecondStep),
		Right is 6.0 * (SecondSlope - FirstSlope),
		interior_system([SecondStep| Steps], [SecondSlope| Slopes], Diagonals, Rights).

	solve_tridiagonal(Lower, [FirstDiagonal| Diagonals], [FirstUpper| Uppers], [FirstRight| Rights], Solution) :-
		FirstModifiedUpper is FirstUpper / FirstDiagonal,
		FirstModifiedRight is FirstRight / FirstDiagonal,
		forward_elimination(Diagonals, Lower, Uppers, Rights, FirstModifiedUpper, FirstModifiedRight, ModifiedUppers, ModifiedRights),
		reverse(ModifiedUppers, ReversedUppers),
		reverse(ModifiedRights, ReversedRights),
		back_substitution(ReversedRights, ReversedUppers, Solution).

	forward_elimination([Diagonal], [Lower], [], [Right], PreviousUpper, PreviousRight, [PreviousUpper, 0.0], [PreviousRight, ModifiedRight]) :-
		Denominator is Diagonal - Lower * PreviousUpper,
		ModifiedRight is (Right - Lower * PreviousRight) / Denominator,
		!.
	forward_elimination([Diagonal| Diagonals], [Lower| Lowers], [Upper| Uppers], [Right| Rights], PreviousUpper, PreviousRight, [PreviousUpper| ModifiedUppers], [PreviousRight| ModifiedRights]) :-
		Denominator is Diagonal - Lower * PreviousUpper,
		ModifiedUpper is Upper / Denominator,
		ModifiedRight is (Right - Lower * PreviousRight) / Denominator,
		forward_elimination(Diagonals, Lowers, Uppers, Rights, ModifiedUpper, ModifiedRight, ModifiedUppers, ModifiedRights).

	back_substitution([LastRight| Rights], [_| Uppers], Solution) :-
		back_substitution(Rights, Uppers, LastRight, [LastRight], Solution).

	back_substitution([], [], _, Solution, Solution).
	back_substitution([Right| Rights], [Upper| Uppers], Next, Solution0, Solution) :-
		Current is Right - Upper * Next,
		back_substitution(Rights, Uppers, Current, [Current| Solution0], Solution).

	spline_intervals([_], [_], [_], []) :-
		!.
	spline_intervals([X0, X1| Xs], [Y0, Y1| Ys], [M0, M1| Ms], [interval(X0, X1, Y0, B, C, D)| Intervals]) :-
		Step is X1 - X0,
		B is (Y1 - Y0) / Step - Step * (2.0 * M0 + M1) / 6.0,
		C is M0 / 2.0,
		D is (M1 - M0) / (6.0 * Step),
		spline_intervals([X1| Xs], [Y1| Ys], [M1| Ms], Intervals).

	replace_last([_], Replacement, [Replacement]) :-
		!.
	replace_last([Element| Elements], Replacement, [Element| Replaced]) :-
		replace_last(Elements, Replacement, Replaced).

	default_option(boundary(natural)).

	valid_option(boundary(Boundary)) :-
		(	Boundary == natural ->
			true
		;	Boundary = clamped(FirstDerivative, LastDerivative),
			number(FirstDerivative),
			number(LastDerivative)
		).

:- end_object.
