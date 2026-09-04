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


:- category(quadrature(_Function_),
	implements(quadrature_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Common option handling and validation for scalar quadrature algorithms.',
		parameters is [
			'Function' - 'Object implementing ``univariate_function_protocol``.'
		]
	]).

	:- protected(evaluate_integrand/2).
	:- mode(evaluate_integrand(+number, -number), one_or_error).
	:- info(evaluate_integrand/2, [
		comment is 'Evaluates the integrand and checks that it returns a number.',
		argnames is ['Argument', 'Value'],
		exceptions is [
			'The integrand returns a non-numeric value' - domain_error(function_value, 'Value')
		]
	]).

	:- protected(check_bounds/2).
	:- mode(check_bounds(+number, +number), one_or_error).
	:- info(check_bounds/2, [
		comment is 'Checks that both integration bounds are instantiated numbers.',
		argnames is ['Lower', 'Upper'],
		exceptions is [
			'An integration bound is a variable' - instantiation_error,
			'An integration bound is not a number' - type_error(number, 'Bound')
		]
	]).

	:- uses(_Function_, [
		evaluate/2
	]).

	:- uses(type, [
		check/3
	]).

	integrate(Lower, Upper, Integral) :-
		::integrate(Lower, Upper, Integral, _Statistics, []).

	integrate(Lower, Upper, Integral, UserOptions) :-
		::integrate(Lower, Upper, Integral, _Statistics, UserOptions).

	evaluate_integrand(Argument, Value) :-
		evaluate(Argument, Value),
		(	number(Value) ->
			true
		;	domain_error(function_value, Value)
		).

	% reversed bounds are intentionally supported
	check_bounds(Lower, Upper) :-
		context(Context),
		check(number, Lower, Context),
		check(number, Upper, Context).

:- end_category.
