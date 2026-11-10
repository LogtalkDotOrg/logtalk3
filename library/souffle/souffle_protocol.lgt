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


:- protocol(souffle_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-10,
		comment is 'Protocol for process-backed Souffle Datalog solving, source generation, and collected result inspection.'
	]).

	:- public(solve/2).
	:- mode(solve(+compound, -compound), one_or_error).
	:- info(solve/2, [
		comment is 'Solves a source with default options using the external Souffle CLI and returns a collected result after cleanup. Valid sources are ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, and ``file(Path)``. No ``.output``/``.printsize`` relations are decoded unless requested via the ``outputs/1``/``printsizes/1`` options of ``solve/3``.',
		argnames is ['Source', 'Result'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``souffle_problem_protocol``' - domain_error(souffle_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(souffle_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(souffle_origins, 'Origins'),
			'``Codes`` is ground but not a list of character codes' - type_error(codes, 'Codes'),
			'An element ``Code`` of the list ``Codes`` is ground but not an integer' - type_error(integer, 'Code'),
			'An element ``Code`` of the list ``Codes`` is an integer but not a valid character code' - domain_error(character_code, 'Code'),
			'``Path`` is ground but not an atom' - type_error(atom, 'Path'),
			'``Path`` does not exist' - existence_error(source_sink, 'Path'),
			'``Path`` cannot be opened for reading' - permission_error(open, source_sink, 'Path'),
			'A launcher argument contains a null character' - domain_error(souffle_shell_argument, 'Argument'),
			'The ``souffle`` executable or a shell dependency does not exist' - existence_error(command, 'Command'),
			'An owned directory cannot be found during cleanup' - existence_error(directory, 'Directory'),
			'An owned file or directory cannot be removed' - permission_error(delete, source_sink, 'Path'),
			'A character sequence is not valid UTF-8' - representation_error(utf_8),
			'A launcher operation fails' - souffle_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - souffle_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(solve/3).
	:- mode(solve(+compound, -compound, +list(compound)), one_or_error).
	:- info(solve/3, [
		comment is 'Solves a source using executable, parallelism, warning, compilation, EDB facts, and decoded-relation options. Repeated options use the first occurrence. Valid sources are ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, and ``file(Path)``. The ``outputs/1`` option lists which ``.output`` relations to decode and their column types; the ``printsizes/1`` option lists which ``.printsize`` relations to decode.',
		argnames is ['Source', 'Result', 'Options'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``souffle_problem_protocol``' - domain_error(souffle_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(souffle_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(souffle_origins, 'Origins'),
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
			'Both ``facts/1`` and ``facts_data/1`` are given with non-default values' - domain_error(souffle_facts_option, 'facts(Path)'-'facts_data(Pairs)'),
			'An element of ``facts_data/1`` has a symbol column value containing a tab or newline character, or a value that is not an integer, float, or atom' - domain_error(souffle_facts_value, 'Value'),
			'A launcher argument contains a null character' - domain_error(souffle_shell_argument, 'Argument'),
			'The ``souffle`` executable or a shell dependency does not exist' - existence_error(command, 'Command'),
			'An owned directory cannot be found during cleanup' - existence_error(directory, 'Directory'),
			'An owned file or directory cannot be removed' - permission_error(delete, source_sink, 'Path'),
			'A character sequence is not valid UTF-8' - representation_error(utf_8),
			'A launcher operation fails' - souffle_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - souffle_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(program/1).
	:- mode(program(+compound), one_or_error).
	:- info(program/1, [
		comment is 'Generates and prints Datalog source to the current output, equivalent to ``program/2`` followed by ``print_program/1``, without invoking Souffle.',
		argnames is ['Source'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``souffle_problem_protocol``' - domain_error(souffle_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(souffle_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(souffle_origins, 'Origins'),
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
		comment is 'Returns Datalog source codes from ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, or ``file(Path)`` without invoking Souffle.',
		argnames is ['Source', 'Codes'],
		exceptions is [
			'``Source`` is not ground' - instantiation_error,
			'``Source`` is ground but not a supported source, or is ``object(Object)`` where ``Object`` does not implement ``souffle_problem_protocol``' - domain_error(souffle_source, 'Source'),
			'``Statements`` is ground but not a list' - type_error(list, 'Statements'),
			'A statement enumerated by the provider object is not a valid statement' - domain_error(souffle_statement, 'Statement'),
			'An origin enumerated by the provider object is invalid, or its origins and statements differ in length' - domain_error(souffle_origins, 'Origins'),
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
		comment is 'Prints Datalog character codes to the current output without adding a newline or invoking Souffle.',
		argnames is ['Codes'],
		exceptions is [
			'``Codes`` is not ground' - instantiation_error,
			'``Codes`` is ground but not a list of character codes' - type_error(codes, 'Codes'),
			'An element ``Code`` of the list ``Codes`` is ground but not an integer' - type_error(integer, 'Code'),
			'An element ``Code`` of the list ``Codes`` is an integer but not a valid character code' - domain_error(character_code, 'Code'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

	:- public(relation/4).
	:- mode(relation(+compound, ?atom, ?list(atom), ?list(list)), zero_or_more).
	:- info(relation/4, [
		comment is 'Enumerates, by backtracking, decoded ``.output`` relations collected in a result: the relation name, its requested column types, and its rows (each row a list of decoded Prolog values, one per column).',
		argnames is ['Result', 'Name', 'Columns', 'Rows'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is not a valid collected Souffle result' - domain_error(souffle_result, 'Result')
		]
	]).

	:- public(size/3).
	:- mode(size(+compound, ?atom, ?integer), zero_or_more).
	:- info(size/3, [
		comment is 'Enumerates, by backtracking, decoded ``.printsize`` relation counts collected in a result.',
		argnames is ['Result', 'Name', 'Count'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is not a valid collected Souffle result' - domain_error(souffle_result, 'Result')
		]
	]).

	:- public(result_property/2).
	:- mode(result_property(+compound, ?compound), zero_or_more).
	:- info(result_property/2, [
		comment is 'Enumerates, by backtracking, the stored result properties (``raw_status/1``, ``process_status/1``, and ``diagnostics/1``).',
		argnames is ['Result', 'Property'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is not a valid collected Souffle result' - domain_error(souffle_result, 'Result')
		]
	]).

	:- public(print_result/1).
	:- mode(print_result(+compound), one_or_error).
	:- info(print_result/1, [
		comment is 'Prints collected relations and printsize counts to the current output, without invoking Souffle.',
		argnames is ['Result'],
		exceptions is [
			'``Result`` is not ground' - instantiation_error,
			'``Result`` is not a valid collected Souffle result' - domain_error(souffle_result, 'Result'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

:- end_protocol.
