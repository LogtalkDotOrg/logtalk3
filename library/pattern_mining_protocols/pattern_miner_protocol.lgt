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
		date is 2026-04-22,
		comment is 'Protocol for machine learning pattern-mining algorithms.',
		see_also is [transaction_dataset_protocol, sequence_dataset_protocol]
	]).

	:- public(mine/2).
	:- mode(mine(+object_identifier, -compound), one).
	:- info(mine/2, [
		comment is 'Mines a pattern miner result from the given dataset object.',
		argnames is ['Dataset', 'PatternMiner']
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
