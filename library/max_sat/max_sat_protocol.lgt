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


:- protocol(max_sat_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-23,
		comment is 'Protocol for process-backed RC2, Open-WBO, and Z3 CLI weighted partial MaxSAT solving, WCNF source generation, and result inspection.'
	]).

	:- public(solve/2).
	:- mode(solve(+compound, -compound), one_or_error).
	:- info(solve/2, [
		comment is 'Solves a source with default options using the external MaxSAT solver CLI and returns a collected result after cleanup. Valid sources are ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, and ``file(Path)``.',
		argnames is ['Source', 'Result'],
		exceptions is [
			'``Source`` is not a ground term' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``max_sat_problem_protocol``' - domain_error(max_sat_source, 'Source'),
			'A statement is not a valid statement' - domain_error(max_sat_statement, 'Statement'),
			'The selected solver executable or a shell dependency does not exist' - existence_error(command, 'Command'),
			'A launcher operation fails' - max_sat_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - max_sat_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(solve/3).
	:- mode(solve(+compound, -compound, +list(compound)), one_or_error).
	:- info(solve/3, [
		comment is 'Solves a source using the ``solver/1``, ``executable/1``, ``time_limit/1``, ``anytime/1``, and ``wcnf_format/1`` options. Repeated options use the first occurrence. Valid sources are ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, and ``file(Path)``. File sources are read and passed to the solver as a ``.wcnf`` file as some solvers select the input format from the file name extension.',
		argnames is ['Source', 'Result', 'Options'],
		exceptions is [
			'``Source`` is not a ground term' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``max_sat_problem_protocol``' - domain_error(max_sat_source, 'Source'),
			'A statement is not a valid statement' - domain_error(max_sat_statement, 'Statement'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The selected solver executable or a shell dependency does not exist' - existence_error(command, 'Command'),
			'A launcher operation fails' - max_sat_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - max_sat_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(program/1).
	:- mode(program(+compound), one_or_error).
	:- info(program/1, [
		comment is 'Generates and prints WCNF source to the current output using default options, without invoking the solver.',
		argnames is ['Source']
	]).

	:- public(program/2).
	:- mode(program(+compound, -list(integer)), one_or_error).
	:- info(program/2, [
		comment is 'Returns WCNF source codes from ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, or ``file(Path)`` using default options, without invoking the solver.',
		argnames is ['Source', 'Codes']
	]).

	:- public(program/3).
	:- mode(program(+compound, -list(integer), +list(compound)), one_or_error).
	:- info(program/3, [
		comment is 'Returns WCNF source codes using the ``embed_variables/1`` and ``wcnf_format/1`` options, without invoking the solver.',
		argnames is ['Source', 'Codes', 'Options']
	]).

	:- public(print_program/1).
	:- mode(print_program(+list(integer)), one_or_error).
	:- info(print_program/1, [
		comment is 'Prints WCNF character codes to the current output without adding a newline or invoking the solver.',
		argnames is ['Codes']
	]).

	:- public(literal/2).
	:- mode(literal(+compound, -compound), zero_or_more).
	:- info(literal/2, [
		comment is 'Enumerates, by backtracking, the collected model literals (``+Term`` or ``-Term``) without starting a process. Fails when the result has no model.',
		argnames is ['Result', 'Literal']
	]).

	:- public(cost/2).
	:- mode(cost(+compound, -integer), zero_or_one).
	:- info(cost/2, [
		comment is 'Returns the cost (sum of the weights of the falsified soft clauses) of the collected model. The cost is optimal for an ``optimal`` result and an upper bound for a ``satisfiable`` (best-known) result. Fails when the result has no cost.',
		argnames is ['Result', 'Cost']
	]).

	:- public(result_property/2).
	:- mode(result_property(+compound, ?compound), zero_or_more).
	:- info(result_property/2, [
		comment is 'Enumerates, by backtracking, ``status(Status)`` and the stored result properties (``cost/1``, ``solver/1``, ``termination/1``, ``anytime/1``, ``variables/1``, ``variable_count/1``, ``diagnostics/1``, ``source_map/1``, ``raw_status/1``, ``process_status/1``, and ``raw_output/1``).',
		argnames is ['Result', 'Property']
	]).

	:- public(print_result/1).
	:- mode(print_result(+compound), one_or_error).
	:- info(print_result/1, [
		comment is 'Prints the collected cost, status, and, when present, the model literals to the current output using MaxSAT Evaluation style ``o``/``s``/``v`` labels, without invoking the solver.',
		argnames is ['Result']
	]).

:- end_protocol.
