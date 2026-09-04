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


:- protocol(quadrature_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Protocol for finite-interval scalar numerical quadrature algorithms.'
	]).

	:- public(integrate/3).
	:- mode(integrate(+number, +number, -number), one_or_error).
	:- info(integrate/3, [
		comment is 'Integrates the function over the given finite interval using default options.',
		argnames is ['Lower', 'Upper', 'Integral'],
		exceptions is [
			'An integration bound is a variable' - instantiation_error,
			'An integration bound is not a number' - type_error(number, 'Bound'),
			'The function returns a non-numeric value' - domain_error(function_value, 'Value')
		]
	]).

	:- public(integrate/4).
	:- mode(integrate(+number, +number, -number, +list(compound)), one_or_error).
	:- info(integrate/4, [
		comment is 'Integrates the function over the given finite interval using the given options.',
		argnames is ['Lower', 'Upper', 'Integral', 'Options'],
		exceptions is [
			'An integration bound or ``Options`` is a variable' - instantiation_error,
			'An integration bound is not a number' - type_error(number, 'Bound'),
			'``Options`` is not a list or contains an invalid option' - domain_error(options, 'Options'),
			'The function returns a non-numeric value' - domain_error(function_value, 'Value')
		]
	]).

	:- public(integrate/5).
	:- mode(integrate(+number, +number, -number, -list(compound), +list(compound)), one_or_error).
	:- info(integrate/5, [
		comment is 'Integrates the function and returns run statistics using the given options.',
		argnames is ['Lower', 'Upper', 'Integral', 'Statistics', 'Options'],
		exceptions is [
			'An integration bound or ``Options`` is a variable' - instantiation_error,
			'An integration bound is not a number' - type_error(number, 'Bound'),
			'``Options`` is not a list or contains an invalid option' - domain_error(options, 'Options'),
			'The function returns a non-numeric value' - domain_error(function_value, 'Value')
		]
	]).

:- end_protocol.
