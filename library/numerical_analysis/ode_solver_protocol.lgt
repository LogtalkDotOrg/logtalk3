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


:- protocol(ode_solver_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Protocol for non-stiff initial-value ODE solvers.'
	]).

	:- public(solve/4).
	:- mode(solve(+number, +list(number), +number, -list(pair)), one_or_error).
	:- info(solve/4, [
		comment is 'Solves an initial-value problem using default options.',
		argnames is ['InitialTime', 'InitialState', 'FinalTime', 'Trajectory'],
		exceptions is [
			'An input argument is a variable' - instantiation_error,
			'A time is not a number' - type_error(number, 'Time'),
			'``InitialState`` is not a non-empty numeric list' - domain_error(ode_state, 'InitialState'),
			'The system returns an invalid derivative' - domain_error(ode_derivative, 'Derivative')
		]
	]).

	:- public(solve/5).
	:- mode(solve(+number, +list(number), +number, -list(pair), +list(compound)), one_or_error).
	:- info(solve/5, [
		comment is 'Solves an initial-value problem using the given options.',
		argnames is ['InitialTime', 'InitialState', 'FinalTime', 'Trajectory', 'Options'],
		exceptions is [
			'An input argument or ``Options`` is a variable' - instantiation_error,
			'A time is not a number' - type_error(number, 'Time'),
			'``InitialState`` is not a non-empty numeric list' - domain_error(ode_state, 'InitialState'),
			'The system returns an invalid derivative' - domain_error(ode_derivative, 'Derivative'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(solve/6).
	:- mode(solve(+number, +list(number), +number, -list(pair), -list(compound), +list(compound)), one_or_error).
	:- info(solve/6, [
		comment is 'Solves an initial-value problem and returns run statistics using the given options.',
		argnames is ['InitialTime', 'InitialState', 'FinalTime', 'Trajectory', 'Statistics', 'Options'],
		exceptions is [
			'An input argument or ``Options`` is a variable' - instantiation_error,
			'A time is not a number' - type_error(number, 'Time'),
			'``InitialState`` is not a non-empty numeric list' - domain_error(ode_state, 'InitialState'),
			'The system returns an invalid derivative' - domain_error(ode_derivative, 'Derivative'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

:- end_protocol.
