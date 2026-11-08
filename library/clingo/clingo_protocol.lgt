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


:- protocol(clingo_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-08,
		comment is 'Protocol for process-backed Clingo CLI solving, source generation, and result inspection.'
	]).

	:- public(solve/2).
	:- mode(solve(+compound, -compound), one_or_error).
	:- info(solve/2, [
		comment is 'Solves a source with default options using the external Clingo CLI and returns a collected result after cleanup.',
		argnames is ['Source', 'Result'],
		exceptions is [
			'``Source`` or a provider statement is insufficiently instantiated' - instantiation_error,
			'A source list has an invalid type' - type_error(list, 'Input'),
			'``Codes`` is not a character-code list' - type_error(codes, 'Codes'),
			'``Path`` is not an atom' - type_error(atom, 'Path'),
			'``Source`` is not supported' - domain_error(clingo_source, 'Source'),
			'A provider statement is invalid' - domain_error(clingo_statement, 'Statement'),
			'A provider origin is invalid' - domain_error(clingo_origins, 'Origins'),
			'A launcher argument contains a null character' - domain_error(clingo_shell_argument, 'Argument'),
			'Clingo or a shell dependency cannot be resolved' - existence_error(command, 'Command'),
			'A source or capture file cannot be accessed' - existence_error(source_sink, 'Path'),
			'A source or capture file cannot be opened' - permission_error(open, source_sink, 'Path'),
			'An owned directory cannot be found during cleanup' - existence_error(directory, 'Directory'),
			'An owned file or directory cannot be removed' - permission_error(delete, source_sink, 'Path'),
			'A character sequence is not valid UTF-8' - representation_error(utf_8),
			'A launcher operation fails' - clingo_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - clingo_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(solve/3).
	:- mode(solve(+compound, -compound, +list(compound)), one_or_error).
	:- info(solve/3, [
		comment is 'Solves a source using ``executable/1``, ``models/1``, ``time_limit/1``, ``opt_mode/1``, ``statistics/1``, and ``constants/1`` options. Repeated options use the first occurrence.',
		argnames is ['Source', 'Result', 'Options'],
		exceptions is [
			'An input is insufficiently instantiated' - instantiation_error,
			'A source or options list has an invalid type' - type_error(list, 'Input'),
			'An option is not a compound term' - type_error(compound, 'Option'),
			'``Codes`` is not a character-code list' - type_error(codes, 'Codes'),
			'``Path`` is not an atom' - type_error(atom, 'Path'),
			'``Source`` is not supported' - domain_error(clingo_source, 'Source'),
			'A provider statement is invalid' - domain_error(clingo_statement, 'Statement'),
			'A provider origin is invalid' - domain_error(clingo_origins, 'Origins'),
			'An option is invalid' - domain_error(option, 'Option'),
			'A launcher argument contains a null character' - domain_error(clingo_shell_argument, 'Argument'),
			'Clingo or a shell dependency cannot be resolved' - existence_error(command, 'Command'),
			'A source or capture file cannot be accessed' - existence_error(source_sink, 'Path'),
			'A source or capture file cannot be opened' - permission_error(open, source_sink, 'Path'),
			'An owned directory cannot be found during cleanup' - existence_error(directory, 'Directory'),
			'An owned file or directory cannot be removed' - permission_error(delete, source_sink, 'Path'),
			'A character sequence is not valid UTF-8' - representation_error(utf_8),
			'A launcher operation fails' - clingo_transport('Stage', 'Status', 'Details'),
			'Solver output is invalid or inconsistent with its exit status' - clingo_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(program/1).
	:- mode(program(+compound), one_or_error).
	:- info(program/1, [
		comment is 'Generates and prints ASP source to the current output, equivalent to ``program/2`` followed by ``print_program/1``, without invoking Clingo.',
		argnames is ['Source'],
		exceptions is [
			'``Source`` or a provider statement is insufficiently instantiated' - instantiation_error,
			'``Statements`` is not a list' - type_error(list, 'Statements'),
			'``Codes`` is not a character-code list' - type_error(codes, 'Codes'),
			'A character code is not an integer' - type_error(integer, 'Code'),
			'``Path`` is not an atom' - type_error(atom, 'Path'),
			'A character code is outside the supported range' - domain_error(character_code, 'Code'),
			'``Source`` is not a supported source or its object does not implement the provider protocol' - domain_error(clingo_source, 'Source'),
			'A provider statement is invalid' - domain_error(clingo_statement, 'Statement'),
			'A provider origin is invalid' - domain_error(clingo_origins, 'Origins'),
			'``Path`` does not exist' - existence_error(source_sink, 'Path'),
			'``Path`` cannot be opened for reading' - permission_error(open, source_sink, 'Path'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

	:- public(program/2).
	:- mode(program(+compound, -list(integer)), one_or_error).
	:- info(program/2, [
		comment is 'Returns ASP source codes from ``object(Object)``, ``terms(Statements)``, ``codes(Codes)``, or ``file(Path)`` without invoking Clingo.',
		argnames is ['Source', 'Codes'],
		exceptions is [
			'``Source`` or a provider statement is insufficiently instantiated' - instantiation_error,
			'``Statements`` is not a list' - type_error(list, 'Statements'),
			'``Codes`` is not a character-code list' - type_error(codes, 'Codes'),
			'``Path`` is not an atom' - type_error(atom, 'Path'),
			'``Source`` is not a supported source or its object does not implement the provider protocol' - domain_error(clingo_source, 'Source'),
			'A provider statement is invalid' - domain_error(clingo_statement, 'Statement'),
			'A provider origin is invalid' - domain_error(clingo_origins, 'Origins'),
			'``Path`` does not exist' - existence_error(source_sink, 'Path'),
			'``Path`` cannot be opened for reading' - permission_error(open, source_sink, 'Path')
		]
	]).

	:- public(print_program/1).
	:- mode(print_program(+list(integer)), one_or_error).
	:- info(print_program/1, [
		comment is 'Prints ASP character codes to the current output without adding a newline or invoking Clingo.',
		argnames is ['Codes'],
		exceptions is [
			'``Codes`` is insufficiently instantiated' - instantiation_error,
			'``Codes`` is not a character-code list' - type_error(codes, 'Codes'),
			'A character code is not an integer' - type_error(integer, 'Code'),
			'A character code is outside the supported range' - domain_error(character_code, 'Code'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

	:- public(model/2).
	:- mode(model(+compound, -compound), zero_or_more).
	:- info(model/2, [
		comment is 'Enumerates, by backtracking, collected ``model(Symbols, Costs, RawValues)`` records without starting a process.',
		argnames is ['Result', 'Model'],
		exceptions is [
			'``Result`` is insufficiently instantiated' - instantiation_error,
			'``Result`` is not a collected Clingo result' - domain_error(clingo_result, 'Result')
		]
	]).

	:- public(result_property/2).
	:- mode(result_property(+compound, ?compound), zero_or_more).
	:- info(result_property/2, [
		comment is 'Enumerates, by backtracking, ``status(Status)`` and the stored result properties.',
		argnames is ['Result', 'Property'],
		exceptions is [
			'``Result`` is insufficiently instantiated' - instantiation_error,
			'``Result`` is not a collected Clingo result' - domain_error(clingo_result, 'Result')
		]
	]).

	:- public(print_result/1).
	:- mode(print_result(+compound), one_or_error).
	:- info(print_result/1, [
		comment is 'Prints collected answers, optimization costs, solver status, and summary metadata to the current output using Clingo-style labels, without invoking the solver.',
		argnames is ['Result'],
		exceptions is [
			'``Result`` is insufficiently instantiated' - instantiation_error,
			'``Result`` is not a collected Clingo result' - domain_error(clingo_result, 'Result'),
			'The current output stream does not allow writing' - permission_error(output, stream, 'Stream')
		]
	]).

:- end_protocol.
