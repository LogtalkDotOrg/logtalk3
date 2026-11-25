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


:- protocol(wmc_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-24,
		comment is 'Protocol for weighted model counting using external Ganak and SharpSAT-TD CLI counters or a reference counter written in Logtalk, CNF source generation, and result inspection.'
	]).

	:- public(solve/2).
	:- mode(solve(+compound, -compound), one_or_error).
	:- info(solve/2, [
		comment is 'Computes the weighted model count of a source with default options and returns a collected result after cleanup. Valid sources are ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, and ``file(Path)``.',
		argnames is ['Source', 'Result'],
		exceptions is [
			'``Source`` is not a ground term' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``wmc_problem_protocol``' - domain_error(wmc_source, 'Source'),
			'A statement is not a valid statement' - domain_error(wmc_statement, 'Statement'),
			'The selected solver executable or a shell dependency does not exist' - existence_error(command, 'Command'),
			'A launcher operation fails' - wmc_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - wmc_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(solve/3).
	:- mode(solve(+compound, -compound, +list(compound)), one_or_error).
	:- info(solve/3, [
		comment is 'Computes the weighted model count of a source using the ``solver/1``, ``executable/1``, ``time_limit/1``, ``decomposition_time/1``, and ``embed_variables/1`` options. Repeated options use the first occurrence. Valid sources are ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, and ``file(Path)``. File sources are read and passed to the solver as a file named ``problem.cnf``.',
		argnames is ['Source', 'Result', 'Options'],
		exceptions is [
			'``Source`` is not a ground term' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``wmc_problem_protocol``' - domain_error(wmc_source, 'Source'),
			'A statement is not a valid statement' - domain_error(wmc_statement, 'Statement'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The selected solver executable or a shell dependency (e.g., the ``timeout`` command) does not exist' - existence_error(command, 'Command'),
			'A launcher operation fails' - wmc_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - wmc_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(program/1).
	:- mode(program(+compound), one_or_error).
	:- info(program/1, [
		comment is 'Generates and prints CNF source to the current output using default options, without invoking a solver.',
		argnames is ['Source']
	]).

	:- public(program/2).
	:- mode(program(+compound, -list(integer)), one_or_error).
	:- info(program/2, [
		comment is 'Returns CNF source codes from ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, or ``file(Path)`` using default options, without invoking a solver.',
		argnames is ['Source', 'Codes']
	]).

	:- public(program/3).
	:- mode(program(+compound, -list(integer), +list(compound)), one_or_error).
	:- info(program/3, [
		comment is 'Returns CNF source codes using the ``embed_variables/1`` option, without invoking a solver.',
		argnames is ['Source', 'Codes', 'Options']
	]).

	:- public(print_program/1).
	:- mode(print_program(+list(integer)), one_or_error).
	:- info(print_program/1, [
		comment is 'Prints CNF character codes to the current output without adding a newline or invoking a solver.',
		argnames is ['Codes']
	]).

	:- public(value/2).
	:- mode(value(+compound, -number), zero_or_one).
	:- info(value/2, [
		comment is 'Returns the weighted model count of a collected result: an integer for unweighted problems, otherwise a float. Fails when the result has no value.',
		argnames is ['Result', 'Value']
	]).

	:- public(result_property/2).
	:- mode(result_property(+compound, ?compound), zero_or_more).
	:- info(result_property/2, [
		comment is 'Enumerates, by backtracking, ``status(Status)`` and the stored result properties (``number_type/1``, ``representation/1``, ``value_text/1``, ``exact_fraction/1``, ``log10_estimate/1``, ``solver/1``, ``termination/1``, ``variables/1``, ``variable_count/1``, ``diagnostics/1``, ``source_map/1``, ``raw_status/1``, ``process_status/1``, and ``raw_output/1``).',
		argnames is ['Result', 'Property']
	]).

	:- public(print_result/1).
	:- mode(print_result(+compound), one_or_error).
	:- info(print_result/1, [
		comment is 'Prints the status and value of a collected result using Model Counting Competition style ``s`` and ``c s exact`` labels, without invoking a solver.',
		argnames is ['Result']
	]).

:- end_protocol.
