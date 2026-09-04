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


:- protocol(root_finder_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Protocol for scalar root-finding algorithms.'
	]).

	:- public(find_root/2).
	:- mode(find_root(+compound, -number), one_or_error).
	:- info(find_root/2, [
		comment is 'Finds a root using default options.',
		argnames is ['Initial', 'Root'],
		exceptions is [
			'``Initial`` is a variable' - instantiation_error,
			'``Initial`` is not valid for the root finder' - domain_error(root_initialization, 'Initial'),
			'The function returns a non-numeric value' - domain_error(function_value, 'Value')
		]
	]).

	:- public(find_root/3).
	:- mode(find_root(+compound, -number, +list(compound)), one_or_error).
	:- info(find_root/3, [
		comment is 'Finds a root using the given options.',
		argnames is ['Initial', 'Root', 'Options'],
		exceptions is [
			'``Initial`` or ``Options`` is a variable' - instantiation_error,
			'``Initial`` is not valid for the root finder' - domain_error(root_initialization, 'Initial'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The function returns a non-numeric value' - domain_error(function_value, 'Value')
		]
	]).

	:- public(find_root/4).
	:- mode(find_root(+compound, -number, -list(compound), +list(compound)), one_or_error).
	:- info(find_root/4, [
		comment is 'Finds a root and returns run statistics using the given options.',
		argnames is ['Initial', 'Root', 'Statistics', 'Options'],
		exceptions is [
			'``Initial`` or ``Options`` is a variable' - instantiation_error,
			'``Initial`` is not valid for the root finder' - domain_error(root_initialization, 'Initial'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The function returns a non-numeric value' - domain_error(function_value, 'Value')
		]
	]).

:- end_protocol.
