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
		date is 2026-11-09,
		comment is 'Protocol for process-backed Z3, cvc5, Bitwuzla, and Yices 2 CLI solving, SMT-LIB 2 source generation, and collected result inspection.'
	]).

	:- public(solve/2).
	:- mode(solve(+compound, -list(compound)), one_or_error).
	:- info(solve/2, [
		comment is 'Solves a source with default options using the external SMT solver CLI and returns a collected result per ``check-sat`` command, in script order, after cleanup. Valid sources are ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, and ``file(Path)``.',
		argnames is ['Source', 'Results'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``smt_problem_protocol``' - domain_error(smt_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(smt_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(smt_origins, 'Origins'),
			'``Codes`` is ground but not a list of character codes' - type_error(codes, 'Codes'),
			'An element ``Code`` of the list ``Codes`` is ground but not an integer' - type_error(integer, 'Code'),
			'An element ``Code`` of the list ``Codes`` is an integer but not a valid character code' - domain_error(character_code, 'Code'),
			'``Path`` is ground but not an atom' - type_error(atom, 'Path'),
			'``Path`` does not exist' - existence_error(source_sink, 'Path'),
			'``Path`` cannot be opened for reading' - permission_error(open, source_sink, 'Path'),
			'A launcher argument contains a null character' - domain_error(smt_shell_argument, 'Argument'),
			'The selected solver executable or a shell dependency does not exist' - existence_error(command, 'Command'),
			'An owned directory cannot be found during cleanup' - existence_error(directory, 'Directory'),
			'An owned file or directory cannot be removed' - permission_error(delete, source_sink, 'Path'),
			'A character sequence is not valid UTF-8' - representation_error(utf_8),
			'A launcher operation fails' - smt_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - smt_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(solve/3).
	:- mode(solve(+compound, -list(compound), +list(compound)), one_or_error).
	:- info(solve/3, [
		comment is 'Solves a source using ``solver/1``, ``executable/1``, and ``time_limit/1`` options. Repeated options use the first occurrence. Valid sources are ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, and ``file(Path)``.',
		argnames is ['Source', 'Results', 'Options'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``smt_problem_protocol``' - domain_error(smt_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(smt_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(smt_origins, 'Origins'),
			'``Codes`` is ground but not a list of character codes' - type_error(codes, 'Codes'),
			'An element ``Code`` of the list ``Codes`` is ground but not an integer' - type_error(integer, 'Code'),
			'An element ``Code`` of the list ``Codes`` is an integer but not a valid character code' - domain_error(character_code, 'Code'),
			'``Path`` is ground but not an atom' - type_error(atom, 'Path'),
			'``Path`` does not exist' - existence_error(source_sink, 'Path'),
			'``Path`` cannot be opened for reading' - permission_error(open, source_sink, 'Path'),
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'A launcher argument contains a null character' - domain_error(smt_shell_argument, 'Argument'),
			'The selected solver executable or a shell dependency does not exist' - existence_error(command, 'Command'),
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
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``smt_problem_protocol``' - domain_error(smt_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(smt_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(smt_origins, 'Origins'),
			'``Codes`` is ground but not a list of character codes' - type_error(codes, 'Codes'),
			'An element ``Code`` of the list ``Codes`` is ground but not an integer' - type_error(integer, 'Code'),
			'An element ``Code`` of the list ``Codes`` is an integer but not a valid character code' - domain_error(character_code, 'Code'),
			'``Path`` is ground but not an atom' - type_error(atom, 'Path'),
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
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``smt_problem_protocol``' - domain_error(smt_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(smt_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(smt_origins, 'Origins'),
			'``Codes`` is ground but not a list of character codes' - type_error(codes, 'Codes'),
			'An element ``Code`` of the list ``Codes`` is ground but not an integer' - type_error(integer, 'Code'),
			'An element ``Code`` of the list ``Codes`` is an integer but not a valid character code' - domain_error(character_code, 'Code'),
			'``Path`` is ground but not an atom' - type_error(atom, 'Path'),
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
			'``Codes`` is not ground' - instantiation_error,
			'``Codes`` is ground but not a list of character codes' - type_error(codes, 'Codes'),
			'An element ``Code`` of the list ``Codes`` is ground but not an integer' - type_error(integer, 'Code'),
			'An element ``Code`` of the list ``Codes`` is an integer but not a valid character code' - domain_error(character_code, 'Code'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

	:- public(status/2).
	:- mode(status(+compound, -atom), one_or_error).
	:- info(status/2, [
		comment is 'Returns the collected status (``satisfiable``, ``unsatisfiable``, or ``unknown``) of a single ``check-sat`` result.',
		argnames is ['Result', 'Status'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is not a valid collected SMT solver result' - domain_error(smt_result, 'Result')
		]
	]).

	:- public(model_binding/2).
	:- mode(model_binding(+compound, -pair), zero_or_more).
	:- info(model_binding/2, [
		comment is 'Enumerates, by backtracking, the collected model bindings (``Symbol-Value`` pairs) without starting a process. Fails when the result has no model.',
		argnames is ['Result', 'Binding'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is not a valid collected SMT solver result' - domain_error(smt_result, 'Result')
		]
	]).

	:- public(model_value/3).
	:- mode(model_value(+compound, +atom, -term), zero_or_one).
	:- info(model_value/3, [
		comment is 'Looks up the collected model value bound to ``Symbol``. Fails when the result has no model or no binding for ``Symbol``.',
		argnames is ['Result', 'Symbol', 'Value'],
		exceptions is [
			'``Result`` or ``Symbol`` is a variable' - instantiation_error,
			'``Symbol`` is neither a variable nor an atom' - type_error(atom, 'Symbol'),
			'``Result`` is not a valid collected SMT solver result' - domain_error(smt_result, 'Result')
		]
	]).

	:- public(unsat_core_name/2).
	:- mode(unsat_core_name(+compound, -atom), zero_or_more).
	:- info(unsat_core_name/2, [
		comment is 'Enumerates, by backtracking, the named assertions in the collected unsatisfiable core without starting a process. Fails when the result has no core.',
		argnames is ['Result', 'Name'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is not a valid collected SMT solver result' - domain_error(smt_result, 'Result')
		]
	]).

	:- public(result_property/2).
	:- mode(result_property(+compound, ?compound), zero_or_more).
	:- info(result_property/2, [
		comment is 'Enumerates, by backtracking, ``status(Status)`` and the stored result properties.',
		argnames is ['Result', 'Property'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is not a valid collected SMT solver result' - domain_error(smt_result, 'Result')
		]
	]).

	:- public(print_result/1).
	:- mode(print_result(+compound), one_or_error).
	:- info(print_result/1, [
		comment is 'Prints a single collected result (status and, when present, model bindings and unsat core names) to the current output, without invoking the solver.',
		argnames is ['Result'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is not a valid collected SMT solver result' - domain_error(smt_result, 'Result'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

	:- public(print_results/1).
	:- mode(print_results(+list(compound)), one_or_error).
	:- info(print_results/1, [
		comment is 'Prints each collected result in ``Results``, in order, separating consecutive results with a blank line.',
		argnames is ['Results'],
		exceptions is [
			'``Results`` is not ground' - instantiation_error,
			'``Results`` is ground but not a list' - type_error(list, 'Results'),
			'An element of the list ``Results`` is not a valid collected SMT solver result' - domain_error(smt_result, 'Result'),
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
