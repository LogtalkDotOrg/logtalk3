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


:- protocol(pattern_miner_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-29,
		comment is 'Protocol for machine learning pattern-mining algorithms.',
		see_also is [transaction_dataset_protocol, sequence_dataset_protocol]
	]).

	:- public(mine/3).
	:- mode(mine(+object_identifier, -compound, +list(compound)), one).
	:- info(mine/3, [
		comment is 'Mines a pattern miner result from the given dataset object using the specified options.',
		argnames is ['Dataset', 'PatternMiner', 'Options']
	]).

	:- public(mine/2).
	:- mode(mine(+object_identifier, -compound), one).
	:- info(mine/2, [
		comment is 'Mines a pattern miner result from the given dataset object using default options.',
		argnames is ['Dataset', 'PatternMiner']
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+compound, -list(compound)), one).
	:- info(diagnostics/2, [
		comment is 'Returns diagnostics and metadata associated with a mined pattern miner in a representation-independent way.',
		argnames is ['PatternMiner', 'Diagnostics']
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+compound, ?compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Tests or enumerates individual diagnostics metadata terms for a mined pattern miner.',
		argnames is ['PatternMiner', 'Diagnostic']
	]).

	:- public(pattern_miner_options/2).
	:- mode(pattern_miner_options(+compound, -list(compound)), zero_or_one).
	:- info(pattern_miner_options/2, [
		comment is 'Returns the effective mining options recorded in a mined pattern miner diagnostics list.',
		argnames is ['PatternMiner', 'Options']
	]).

	:- public(check_pattern_miner/1).
	:- mode(check_pattern_miner(@compound), one_or_error).
	:- info(check_pattern_miner/1, [
		comment is 'Checks that the argument is a structurally valid mined pattern miner term for the receiving pattern miner implementation, throwing an exception on invalid input when applicable.',
		argnames is ['PatternMiner']
	]).

	:- public(valid_pattern_miner/1).
	:- mode(valid_pattern_miner(@compound), zero_or_one).
	:- info(valid_pattern_miner/1, [
		comment is 'True when ``check_pattern_miner/1`` succeeds for the argument without throwing an exception.',
		argnames is ['PatternMiner']
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +callable, -list(clause)), one).
	:- info(export_to_clauses/4, [
		comment is 'Converts a pattern miner result into a list of predicate clauses. ``Functor`` is the functor for the generated predicate clauses.',
		argnames is ['Dataset', 'PatternMiner', 'Functor', 'Clauses']
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +callable, +atom), one).
	:- info(export_to_file/4, [
		comment is 'Exports a pattern miner result to a file. ``Functor`` is the functor for the generated predicate clauses.',
		argnames is ['Dataset', 'PatternMiner', 'Functor', 'File']
	]).

	:- public(print_pattern_miner/1).
	:- mode(print_pattern_miner(+compound), one).
	:- info(print_pattern_miner/1, [
		comment is 'Prints a pattern miner result to the current output stream in a human-readable format.',
		argnames is ['PatternMiner']
	]).

:- end_protocol.
