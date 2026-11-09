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


:- protocol(smt_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-08,
		comment is 'Protocol for process-backed Z3, cvc5, Bitwuzla, and Yices 2 CLI solving, SMT-LIB 2 source generation, and collected result inspection.'
	]).

	:- public(solve/2).
	:- mode(solve(+compound, -list(compound)), one_or_error).
	:- info(solve/2, [
		comment is 'Solves a source with default options using the external SMT solver CLI and returns a collected result per ``check-sat`` command, in script order, after cleanup.',
		argnames is ['Source', 'Results'],
		exceptions is [
			'``Source`` or a provider statement is insufficiently instantiated' - instantiation_error,
			'A source list has an invalid type' - type_error(list, 'Input'),
			'``Source`` is not supported' - domain_error(smt_source, 'Source'),
			'A launcher operation fails' - smt_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - smt_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(solve/3).
	:- mode(solve(+compound, -list(compound), +list(compound)), one_or_error).
	:- info(solve/3, [
		comment is 'Solves a source using ``solver/1``, ``executable/1``, and ``time_limit/1`` options. Repeated options use the first occurrence.',
		argnames is ['Source', 'Results', 'Options'],
		exceptions is [
			'An input is insufficiently instantiated' - instantiation_error,
			'A source or options list has an invalid type' - type_error(list, 'Input'),
			'An option is not a compound term' - type_error(compound, 'Option'),
			'``Codes`` is not a character-code list' - type_error(codes, 'Codes'),
			'``Path`` is not an atom' - type_error(atom, 'Path'),
			'``Source`` is not supported' - domain_error(smt_source, 'Source'),
			'A provider statement is invalid' - domain_error(smt_statement, 'Statement'),
			'A provider origin is invalid' - domain_error(smt_origins, 'Origins'),
			'An option is invalid' - domain_error(option, 'Option'),
			'A launcher argument contains a null character' - domain_error(smt_shell_argument, 'Argument'),
			'The selected solver or a shell dependency cannot be resolved' - existence_error(command, 'Command'),
			'A source or capture file cannot be accessed' - existence_error(source_sink, 'Path'),
			'A source or capture file cannot be opened' - permission_error(open, source_sink, 'Path'),
			'An owned directory cannot be found during cleanup' - existence_error(directory, 'Directory'),
			'An owned file or directory cannot be removed' - permission_error(delete, source_sink, 'Path'),
			'A character sequence is not valid UTF-8' - representation_error(utf_8),
			'A launcher operation fails' - smt_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - smt_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(program/1).
	:- mode(program(+compound), one_or_error).
	:- info(program/1, [
		comment is 'Generates and prints SMT-LIB 2 source to the current output, equivalent to ``program/2`` followed by ``print_program/1``, without invoking the solver.',
		argnames is ['Source'],
		exceptions is [
			'``Source`` or a provider statement is insufficiently instantiated' - instantiation_error,
			'``Statements`` is not a list' - type_error(list, 'Statements'),
			'``Codes`` is not a character-code list' - type_error(codes, 'Codes'),
			'``Path`` is not an atom' - type_error(atom, 'Path'),
			'``Source`` is not a supported source or its object does not implement the provider protocol' - domain_error(smt_source, 'Source'),
			'A provider statement is invalid' - domain_error(smt_statement, 'Statement'),
			'A provider origin is invalid' - domain_error(smt_origins, 'Origins'),
			'``Path`` does not exist' - existence_error(source_sink, 'Path'),
			'``Path`` cannot be opened for reading' - permission_error(open, source_sink, 'Path'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

	:- public(program/2).
	:- mode(program(+compound, -list(integer)), one_or_error).
	:- info(program/2, [
		comment is 'Returns SMT-LIB 2 source codes from ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, or ``file(Path)`` without invoking the solver.',
		argnames is ['Source', 'Codes'],
		exceptions is [
			'``Source`` or a provider statement is insufficiently instantiated' - instantiation_error,
			'``Statements`` is not a list' - type_error(list, 'Statements'),
			'``Codes`` is not a character-code list' - type_error(codes, 'Codes'),
			'``Path`` is not an atom' - type_error(atom, 'Path'),
			'``Source`` is not a supported source or its object does not implement the provider protocol' - domain_error(smt_source, 'Source'),
			'A provider statement is invalid' - domain_error(smt_statement, 'Statement'),
			'A provider origin is invalid' - domain_error(smt_origins, 'Origins'),
			'``Path`` does not exist' - existence_error(source_sink, 'Path'),
			'``Path`` cannot be opened for reading' - permission_error(open, source_sink, 'Path')
		]
	]).

	:- public(print_program/1).
	:- mode(print_program(+list(integer)), one_or_error).
	:- info(print_program/1, [
		comment is 'Prints SMT-LIB 2 character codes to the current output without adding a newline or invoking the solver.',
		argnames is ['Codes'],
		exceptions is [
			'``Codes`` is insufficiently instantiated' - instantiation_error,
			'``Codes`` is not a character-code list' - type_error(codes, 'Codes'),
			'A character code is not an integer' - type_error(integer, 'Code'),
			'A character code is outside the supported range' - domain_error(character_code, 'Code'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

	:- public(status/2).
	:- mode(status(+compound, -atom), one_or_error).
	:- info(status/2, [
		comment is 'Returns the collected status (``satisfiable``, ``unsatisfiable``, or ``unknown``) of a single ``check-sat`` result.',
		argnames is ['Result', 'Status'],
		exceptions is [
			'``Result`` is insufficiently instantiated' - instantiation_error,
			'``Result`` is not a collected SMT solver result' - domain_error(smt_result, 'Result')
		]
	]).

	:- public(model_binding/2).
	:- mode(model_binding(+compound, -pair), zero_or_more).
	:- info(model_binding/2, [
		comment is 'Enumerates, by backtracking, the collected model bindings (``Symbol-Value`` pairs) without starting a process. Fails when the result has no model.',
		argnames is ['Result', 'Binding'],
		exceptions is [
			'``Result`` is insufficiently instantiated' - instantiation_error,
			'``Result`` is not a collected SMT solver result' - domain_error(smt_result, 'Result')
		]
	]).

	:- public(model_value/3).
	:- mode(model_value(+compound, +atom, -term), zero_or_one).
	:- info(model_value/3, [
		comment is 'Looks up the collected model value bound to ``Symbol``. Fails when the result has no model or no binding for ``Symbol``.',
		argnames is ['Result', 'Symbol', 'Value'],
		exceptions is [
			'An input is insufficiently instantiated' - instantiation_error,
			'``Symbol`` is not an atom' - type_error(atom, 'Symbol'),
			'``Result`` is not a collected SMT solver result' - domain_error(smt_result, 'Result')
		]
	]).

	:- public(unsat_core_name/2).
	:- mode(unsat_core_name(+compound, -atom), zero_or_more).
	:- info(unsat_core_name/2, [
		comment is 'Enumerates, by backtracking, the named assertions in the collected unsatisfiable core without starting a process. Fails when the result has no core.',
		argnames is ['Result', 'Name'],
		exceptions is [
			'``Result`` is insufficiently instantiated' - instantiation_error,
			'``Result`` is not a collected SMT solver result' - domain_error(smt_result, 'Result')
		]
	]).

	:- public(result_property/2).
	:- mode(result_property(+compound, ?compound), zero_or_more).
	:- info(result_property/2, [
		comment is 'Enumerates, by backtracking, ``status(Status)`` and the stored result properties.',
		argnames is ['Result', 'Property'],
		exceptions is [
			'``Result`` is insufficiently instantiated' - instantiation_error,
			'``Result`` is not a collected SMT solver result' - domain_error(smt_result, 'Result')
		]
	]).

	:- public(print_result/1).
	:- mode(print_result(+compound), one_or_error).
	:- info(print_result/1, [
		comment is 'Prints a single collected result (status and, when present, model bindings and unsat core names) to the current output, without invoking the solver.',
		argnames is ['Result'],
		exceptions is [
			'``Result`` is insufficiently instantiated' - instantiation_error,
			'``Result`` is not a collected SMT solver result' - domain_error(smt_result, 'Result'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

	:- public(print_results/1).
	:- mode(print_results(+list(compound)), one_or_error).
	:- info(print_results/1, [
		comment is 'Prints each collected result in ``Results``, in order, separating consecutive results with a blank line.',
		argnames is ['Results'],
		exceptions is [
			'``Results`` is insufficiently instantiated' - instantiation_error,
			'``Results`` is not a list' - type_error(list, 'Results'),
			'A result is not a collected SMT solver result' - domain_error(smt_result, 'Result'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

	:- public(capability/2).
	:- mode(capability(?atom, ?atom), zero_or_more).
	:- info(capability/2, [
		comment is 'Enumerates, by backtracking, advisory ``Solver``-``Feature`` capability pairs. Purely informational: the library does not enforce these pairs and a solver call may still succeed, fail, or report ``unknown`` for a feature outside this table.',
		argnames is ['Solver', 'Feature'],
		remarks is [
			'Feature' - 'One of ``quantifiers``, ``strings``, ``linear_arithmetic``, ``non_linear_arithmetic``, ``unsat_cores``, ``incremental``, ``bit_vectors``, ``arrays``, ``floating_point``, or ``algebraic_datatypes``.'
		]
	]).

:- end_protocol.
