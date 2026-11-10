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


:- protocol(sat_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-10,
		comment is 'Protocol for process-backed Kissat, CaDiCaL, Glucose, and Z3 CLI solving, source generation, and result inspection.'
	]).

	:- public(solve/2).
	:- mode(solve(+compound, -compound), one_or_error).
	:- info(solve/2, [
		comment is 'Solves a source with default options using the external SAT solver CLI and returns a collected result after cleanup. Valid sources are ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, and ``file(Path)``.',
		argnames is ['Source', 'Result'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``sat_problem_protocol``' - domain_error(sat_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(sat_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(sat_origins, 'Origins'),
			'``Codes`` is ground but not a list of character codes' - type_error(codes, 'Codes'),
			'An element ``Code`` of the list ``Codes`` is ground but not an integer' - type_error(integer, 'Code'),
			'An element ``Code`` of the list ``Codes`` is an integer but not a valid character code' - domain_error(character_code, 'Code'),
			'``Path`` is ground but not an atom' - type_error(atom, 'Path'),
			'``Path`` does not exist' - existence_error(source_sink, 'Path'),
			'``Path`` cannot be opened for reading' - permission_error(open, source_sink, 'Path'),
			'A launcher argument contains a null character' - domain_error(sat_shell_argument, 'Argument'),
			'The selected solver executable or a shell dependency does not exist' - existence_error(command, 'Command'),
			'An owned directory cannot be found during cleanup' - existence_error(directory, 'Directory'),
			'An owned file or directory cannot be removed' - permission_error(delete, source_sink, 'Path'),
			'A character sequence is not valid UTF-8' - representation_error(utf_8),
			'A launcher operation fails' - sat_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - sat_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(solve/3).
	:- mode(solve(+compound, -compound, +list(compound)), one_or_error).
	:- info(solve/3, [
		comment is 'Solves a source using ``solver/1``, ``executable/1``, ``time_limit/1``, ``conflicts/1``, ``decisions/1``, ``seed/1``, and ``witness/1`` options. Repeated options use the first occurrence. Valid sources are ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, and ``file(Path)``.',
		argnames is ['Source', 'Result', 'Options'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``sat_problem_protocol``' - domain_error(sat_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(sat_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(sat_origins, 'Origins'),
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
			'A launcher argument contains a null character' - domain_error(sat_shell_argument, 'Argument'),
			'The selected solver executable or a shell dependency does not exist' - existence_error(command, 'Command'),
			'An owned directory cannot be found during cleanup' - existence_error(directory, 'Directory'),
			'An owned file or directory cannot be removed' - permission_error(delete, source_sink, 'Path'),
			'A character sequence is not valid UTF-8' - representation_error(utf_8),
			'A launcher operation fails' - sat_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - sat_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(program/1).
	:- mode(program(+compound), one_or_error).
	:- info(program/1, [
		comment is 'Generates and prints DIMACS CNF source to the current output using default options, equivalent to ``program/2`` followed by ``print_program/1``, without invoking the solver.',
		argnames is ['Source'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``sat_problem_protocol``' - domain_error(sat_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(sat_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(sat_origins, 'Origins'),
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
		comment is 'Returns DIMACS CNF source codes from ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, or ``file(Path)`` using default options, without invoking the solver.',
		argnames is ['Source', 'Codes'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``sat_problem_protocol``' - domain_error(sat_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(sat_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(sat_origins, 'Origins'),
			'``Codes`` is ground but not a list of character codes' - type_error(codes, 'Codes'),
			'An element ``Code`` of the list ``Codes`` is ground but not an integer' - type_error(integer, 'Code'),
			'An element ``Code`` of the list ``Codes`` is an integer but not a valid character code' - domain_error(character_code, 'Code'),
			'``Path`` is ground but not an atom' - type_error(atom, 'Path'),
			'``Path`` does not exist' - existence_error(source_sink, 'Path'),
			'``Path`` cannot be opened for reading' - permission_error(open, source_sink, 'Path')
		]
	]).

	:- public(program/3).
	:- mode(program(+compound, -list(integer), +list(compound)), one_or_error).
	:- info(program/3, [
		comment is 'Returns DIMACS CNF source codes from ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, or ``file(Path)``, using the ``embed_variables/1`` option, without invoking the solver.',
		argnames is ['Source', 'Codes', 'Options'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``sat_problem_protocol``' - domain_error(sat_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(sat_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(sat_origins, 'Origins'),
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
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(print_program/1).
	:- mode(print_program(+list(integer)), one_or_error).
	:- info(print_program/1, [
		comment is 'Prints DIMACS CNF character codes to the current output without adding a newline or invoking the solver.',
		argnames is ['Codes'],
		exceptions is [
			'``Codes`` is not ground' - instantiation_error,
			'``Codes`` is ground but not a list of character codes' - type_error(codes, 'Codes'),
			'An element ``Code`` of the list ``Codes`` is ground but not an integer' - type_error(integer, 'Code'),
			'An element ``Code`` of the list ``Codes`` is an integer but not a valid character code' - domain_error(character_code, 'Code'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

	:- public(literal/2).
	:- mode(literal(+compound, -compound), zero_or_more).
	:- info(literal/2, [
		comment is 'Enumerates, by backtracking, the collected model literals (``+Term`` or ``-Term``) without starting a process. Fails when the result has no witness.',
		argnames is ['Result', 'Literal'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is not a valid collected SAT solver result' - domain_error(sat_result, 'Result')
		]
	]).

	:- public(result_property/2).
	:- mode(result_property(+compound, ?compound), zero_or_more).
	:- info(result_property/2, [
		comment is 'Enumerates, by backtracking, ``status(Status)`` and the stored result properties.',
		argnames is ['Result', 'Property'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is not a valid collected SAT solver result' - domain_error(sat_result, 'Result')
		]
	]).

	:- public(print_result/1).
	:- mode(print_result(+compound), one_or_error).
	:- info(print_result/1, [
		comment is 'Prints the collected status and, when present, the witness literals to the current output using Kissat-style ``s``/``v`` labels, without invoking the solver.',
		argnames is ['Result'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is not a valid collected SAT solver result' - domain_error(sat_result, 'Result'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

:- end_protocol.
