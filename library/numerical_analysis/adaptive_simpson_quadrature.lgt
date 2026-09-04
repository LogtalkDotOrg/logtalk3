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


:- object(adaptive_simpson_quadrature(_Function_),
	imports(quadrature(_Function_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Adaptive Simpson quadrature with absolute and relative error tolerances.',
		parameters is [
			'Function' - 'Object implementing ``univariate_function_protocol``.'
		],
		see_also is [quadrature_protocol, univariate_function_protocol]
	]).

	integrate(Lower, Upper, Integral, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(tol_abs(TolAbs), Options),
		^^option(tol_rel(TolRel), Options),
		^^option(max_subdivisions(MaxSubdivisions), Options),
		^^check_bounds(Lower, Upper),
		(	Lower =:= Upper ->
			Integral = 0.0,
			Evaluations = 0,
			Subdivisions = 0,
			EstimatedError = 0.0,
			Converged = true,
			Reason = zero_interval
		;	Lower < Upper ->
			integrate_ordered(Lower, Upper, TolAbs, TolRel, MaxSubdivisions, Integral, Evaluations, Subdivisions, EstimatedError, Converged, Reason)
		;	integrate_ordered(Upper, Lower, TolAbs, TolRel, MaxSubdivisions, PositiveIntegral, Evaluations, Subdivisions, EstimatedError, Converged, Reason),
			Integral is -PositiveIntegral
		),
		Statistics = [
			evaluations(Evaluations),
			subdivisions(Subdivisions),
			estimated_error(EstimatedError),
			converged(Converged),
			termination_reason(Reason)
		].

	integrate_ordered(Lower, Upper, TolAbs, TolRel, MaxSubdivisions, Integral, Evaluations, Subdivisions, EstimatedError, Converged, Reason) :-
		Middle is Lower + (Upper - Lower) / 2.0,
		^^evaluate_integrand(Lower, LowerValue),
		^^evaluate_integrand(Middle, MiddleValue),
		^^evaluate_integrand(Upper, UpperValue),
		simpson(Lower, Upper, LowerValue, MiddleValue, UpperValue, Whole),
		Tolerance is max(TolAbs, TolRel * abs(Whole)),
		adaptive([panel(Lower, Middle, Upper, LowerValue, MiddleValue, UpperValue, Whole, Tolerance)], MaxSubdivisions, 1, true, 0.0, 0.0, 3, Integral, EstimatedError, Evaluations, Subdivisions, Converged),
		( Converged == true -> Reason = tolerance; Reason = max_subdivisions ).

	adaptive([], _MaxSubdivisions, Subdivisions, Converged, Integral, EstimatedError, Evaluations, Integral, EstimatedError, Evaluations, Subdivisions, Converged).
	adaptive([panel(Lower, Middle, Upper, LowerValue, MiddleValue, UpperValue, Whole, Tolerance)| Panels], MaxSubdivisions, Subdivisions0, MaySplit, Integral0, Error0, Evaluations0, Integral, EstimatedError, Evaluations, Subdivisions, Converged) :-
		LeftMiddle is Lower + (Middle - Lower) / 2.0,
		RightMiddle is Middle + (Upper - Middle) / 2.0,
		^^evaluate_integrand(LeftMiddle, LeftMiddleValue),
		^^evaluate_integrand(RightMiddle, RightMiddleValue),
		simpson(Lower, Middle, LowerValue, LeftMiddleValue, MiddleValue, LeftIntegral),
		simpson(Middle, Upper, MiddleValue, RightMiddleValue, UpperValue, RightIntegral),
		Combined is LeftIntegral + RightIntegral,
		Correction is (Combined - Whole) / 15.0,
		PanelError is abs(Correction),
		Evaluations1 is Evaluations0 + 2,
		(	PanelError =< Tolerance ->
			Integral1 is Integral0 + Combined + Correction,
			Error1 is Error0 + PanelError,
			adaptive(Panels, MaxSubdivisions, Subdivisions0, MaySplit, Integral1, Error1, Evaluations1, Integral, EstimatedError, Evaluations, Subdivisions, Converged)
		;	MaySplit == true, Subdivisions0 < MaxSubdivisions ->
			HalfTolerance is Tolerance / 2.0,
			Subdivisions1 is Subdivisions0 + 1,
			adaptive([
				panel(Lower, LeftMiddle, Middle, LowerValue, LeftMiddleValue, MiddleValue, LeftIntegral, HalfTolerance),
				panel(Middle, RightMiddle, Upper, MiddleValue, RightMiddleValue, UpperValue, RightIntegral, HalfTolerance)
			| Panels], MaxSubdivisions, Subdivisions1, true, Integral0, Error0, Evaluations1, Integral, EstimatedError, Evaluations, Subdivisions, Converged)
		;	Integral1 is Integral0 + Combined + Correction,
			Error1 is Error0 + PanelError,
			adaptive(Panels, MaxSubdivisions, Subdivisions0, false, Integral1, Error1, Evaluations1, Integral, EstimatedError, Evaluations, Subdivisions, _),
			Converged = false
		).

	simpson(Lower, Upper, LowerValue, MiddleValue, UpperValue, Integral) :-
		Integral is (Upper - Lower) * (LowerValue + 4.0 * MiddleValue + UpperValue) / 6.0.

	default_option(tol_abs(1.0e-10)).
	default_option(tol_rel(1.0e-8)).
	default_option(max_subdivisions(10000)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(tol_abs(Tolerance)) :-
		number(Tolerance), Tolerance >= 0.0.
	valid_option(tol_rel(Tolerance)) :-
		number(Tolerance), Tolerance >= 0.0.
	valid_option(max_subdivisions(Subdivisions)) :-
		integer(Subdivisions), Subdivisions > 0.
	valid_option(Option) :-
		^^valid_option(Option).

:- end_object.
