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


:- protocol(interpolator_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Protocol for fitting and evaluating one-dimensional interpolants.'
	]).

	:- public(fit/2).
	:- mode(fit(+list(pair), -compound), one_or_error).
	:- info(fit/2, [
		comment is 'Fits an interpolant to a list of ``X-Y`` points using default options.',
		argnames is ['Points', 'Model'],
		exceptions is [
			'``Points`` is a variable' - instantiation_error,
			'``Points`` is not a valid list of at least two numeric points with distinct abscissas' - domain_error(interpolation_points, 'Points')
		]
	]).

	:- public(fit/3).
	:- mode(fit(+list(pair), -compound, +list(compound)), one_or_error).
	:- info(fit/3, [
		comment is 'Fits an interpolant to a list of ``X-Y`` points using the given options.',
		argnames is ['Points', 'Model', 'Options'],
		exceptions is [
			'``Points`` or ``Options`` is a variable' - instantiation_error,
			'``Points`` is not a valid list of at least two numeric points with distinct abscissas' - domain_error(interpolation_points, 'Points'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(evaluate/3).
	:- mode(evaluate(+compound, +number, -number), one_or_error).
	:- info(evaluate/3, [
		comment is 'Evaluates a fitted interpolant inside its data domain.',
		argnames is ['Model', 'Argument', 'Value'],
		exceptions is [
			'``Model`` is a variable' - instantiation_error,
			'``Model`` is not a model for the receiving interpolator' - domain_error(interpolation_model, 'Model'),
			'``Argument`` is a variable' - instantiation_error,
			'``Argument`` is not a number' - type_error(number, 'Argument'),
			'``Argument`` lies outside the fitted domain' - domain_error(interpolation_domain, 'Argument')
		]
	]).

:- end_protocol.
