%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(problog_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-12,
		comment is 'Process-backed ProbLog exact inference, source generation, and collected query probabilities.'
	]).

	:- public(solve/2).
	:- mode(solve(+compound, -compound), one_or_error).
	:- info(solve/2, [
		comment is 'Runs exact inference with default options. Valid sources are ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, and ``file(Path)``. Returns a ground collected result after cleanup.',
		argnames is ['Source', 'Result'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but invalid' - domain_error(problog_source, 'Source'),
			'An executable cannot be resolved' - existence_error(command, 'Command'),
			'A transport operation fails' - problog_transport('Stage', 'Status', 'Details'),
			'Inference fails or produces invalid output' - problog_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(solve/3).
	:- mode(solve(+compound, -compound, +list(compound)), one_or_error).
	:- info(solve/3, [
		comment is 'Runs exact inference with the given options options. Repeated options use the first occurrence.',
		argnames is ['Source', 'Result', 'Options'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but invalid' - domain_error(problog_source, 'Source'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'An executable cannot be resolved' - existence_error(command, 'Command'),
			'A transport operation fails' - problog_transport('Stage', 'Status', 'Details'),
			'Inference fails or produces invalid output' - problog_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(program/1).
	:- mode(program(+compound), one_or_error).
	:- info(program/1, [
		comment is 'Prints source to the current output without invoking ProbLog.',
		argnames is ['Source'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but invalid' - domain_error(problog_source, 'Source')
		]
	]).

	:- public(program/2).
	:- mode(program(+compound, -list(integer)), one_or_error).
	:- info(program/2, [
		comment is 'Returns source character codes without invoking ProbLog. Native file and codes sources are not parsed.',
		argnames is ['Source', 'Codes'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but invalid' - domain_error(problog_source, 'Source')
		]
	]).

	:- public(print_program/1).
	:- mode(print_program(+list(integer)), one_or_error).
	:- info(print_program/1, [
		comment is 'Prints character codes to the current output without adding a newline.',
		argnames is ['Codes'],
		exceptions is [
			'``Codes`` is a partial list or a list with an element ``Element`` which is a variable' - instantiation_error,
			'``Codes`` is is neither a partial list nor a list' - type_error(codes, 'Codes'),
			'An element ``Element`` of the ``Codes`` list is neither a variable nor an integer' - type_error(integer, 'Element'),
			'An element ``Element`` of the ``Codes`` list is an integer but not a valid character code' - domain_error(character_code, 'Element')
		]
	]).

	:- public(probability/3).
	:- mode(probability(+compound, ?callable, ?number), zero_or_more).
	:- info(probability/3, [
		comment is 'Enumerates ground query terms and numeric probabilities in a collected result. Query order follows the CLI, not a sorting contract.',
		argnames is ['Result', 'Query', 'Probability'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is ground but invalid' - domain_error(problog_result, 'Result')
		]
	]).

	:- public(result_property/2).
	:- mode(result_property(+compound, ?compound), zero_or_more).
	:- info(result_property/2, [
		comment is 'Enumerates raw_status/1, process_status/1, and diagnostics/1 properties. Diagnostics are captured ``stderr character codes.',
		argnames is ['Result', 'Property']
	]).

	:- public(print_result/1).
	:- mode(print_result(+compound), one_or_error).
	:- info(print_result/1, [
		comment is 'Prints the collected query probabilities without invoking ProbLog.',
		argnames is ['Result'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is ground but invalid' - domain_error(problog_result, 'Result')
		]
	]).

:- end_protocol.
