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


:- protocol(association_rule_miner_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Protocol for association rule miners deriving scored rules from mined frequent patterns.',
		see_also is [pattern_miner_protocol, transaction_dataset_protocol, sequence_dataset_protocol]
	]).

	:- public(mine/4).
	:- mode(mine(+object_identifier, +compound, -compound, +list(compound)), one).
	:- info(mine/4, [
		comment is 'Derives association rules from a mined pattern result and its dataset using the specified options.',
		argnames is ['Dataset', 'PatternMiner', 'AssociationRuleMiner', 'Options']
	]).

	:- public(mine/3).
	:- mode(mine(+object_identifier, +compound, -compound), one).
	:- info(mine/3, [
		comment is 'Derives association rules from a mined pattern result and its dataset using default options.',
		argnames is ['Dataset', 'PatternMiner', 'AssociationRuleMiner']
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+compound, -list(compound)), one).
	:- info(diagnostics/2, [
		comment is 'Returns diagnostics and metadata associated with an association rule miner result.',
		argnames is ['AssociationRuleMiner', 'Diagnostics']
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+compound, ?compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Tests or enumerates individual diagnostics metadata terms.',
		argnames is ['AssociationRuleMiner', 'Diagnostic']
	]).

	:- public(association_rule_miner_options/2).
	:- mode(association_rule_miner_options(+compound, -list(compound)), one).
	:- info(association_rule_miner_options/2, [
		comment is 'Returns the effective options recorded in an association rule miner result.',
		argnames is ['AssociationRuleMiner', 'Options']
	]).

	:- public(rule_metric/3).
	:- mode(rule_metric(+compound, +compound, ?compound), zero_or_more).
	:- info(rule_metric/3, [
		comment is 'Computes, tests, or enumerates on-demand metrics for a rule in an association rule miner result. The supported metrics are ``leverage(Value)``, ``jaccard(Value)``, ``cosine(Value)``, and ``kulczynski(Value)``.',
		argnames is ['AssociationRuleMiner', 'Rule', 'Metric']
	]).

	:- public(check_association_rule_miner/1).
	:- mode(check_association_rule_miner(@compound), one_or_error).
	:- info(check_association_rule_miner/1, [
		comment is 'Checks that the argument is a structurally valid association rule miner result, throwing an exception otherwise.',
		argnames is ['AssociationRuleMiner'],
		exceptions is [
			'``AssociationRuleMiner`` is a variable' - instantiation_error,
			'``AssociationRuleMiner`` is neither a variable nor a valid association rule miner result' - domain_error(association_rule_miner, 'AssociationRuleMiner')
		]
	]).

	:- public(valid_association_rule_miner/1).
	:- mode(valid_association_rule_miner(@compound), zero_or_one).
	:- info(valid_association_rule_miner/1, [
		comment is 'True when ``check_association_rule_miner/1`` succeeds for the argument without throwing an exception.',
		argnames is ['AssociationRuleMiner']
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +callable, -list(clause)), one).
	:- info(export_to_clauses/4, [
		comment is 'Converts an association rule miner result into a list of predicate clauses.',
		argnames is ['Dataset', 'AssociationRuleMiner', 'Functor', 'Clauses']
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +callable, +atom), one).
	:- info(export_to_file/4, [
		comment is 'Exports an association rule miner result to a file.',
		argnames is ['Dataset', 'AssociationRuleMiner', 'Functor', 'File']
	]).

	:- public(print_association_rule_miner/1).
	:- mode(print_association_rule_miner(+compound), one).
	:- info(print_association_rule_miner/1, [
		comment is 'Prints an association rule miner result to the current output stream in a human-readable format.',
		argnames is ['AssociationRuleMiner']
	]).

:- end_protocol.
