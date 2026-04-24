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


:- protocol(clusterer_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-23,
		comment is 'Protocol for machine learning clusterers.',
		see_also is [clustering_dataset_protocol]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a clusterer from the given dataset object using the specified options.',
		argnames is ['Dataset', 'Clusterer', 'Options']
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -compound), one).
	:- info(learn/2, [
		comment is 'Learns a clusterer from the given dataset object using default options.',
		argnames is ['Dataset', 'Clusterer']
	]).

	:- public(cluster/3).
	:- mode(cluster(+compound, +list, -ground), one).
	:- info(cluster/3, [
		comment is 'Assigns a new instance to a cluster using the learned clusterer. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['Clusterer', 'Instance', 'Cluster']
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+compound, -list(compound)), one).
	:- info(diagnostics/2, [
		comment is 'Returns diagnostics and metadata associated with a learned clusterer in a representation-independent way.',
		argnames is ['Clusterer', 'Diagnostics']
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+compound, ?compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Tests or enumerates individual diagnostics metadata terms for a learned clusterer.',
		argnames is ['Clusterer', 'Diagnostic']
	]).

	:- public(clusterer_options/2).
	:- mode(clusterer_options(+compound, -list(compound)), one).
	:- info(clusterer_options/2, [
		comment is 'Returns the effective training options recorded in a learned clusterer diagnostics list.',
		argnames is ['Clusterer', 'Options']
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +callable, -list(clause)), one).
	:- info(export_to_clauses/4, [
		comment is 'Converts a clusterer into a list of predicate clauses. ``Functor`` is the functor for the generated predicate clauses.',
		argnames is ['Dataset', 'Clusterer', 'Functor', 'Clauses']
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +callable, +atom), one).
	:- info(export_to_file/4, [
		comment is 'Exports a clusterer to a file. ``Functor`` is the functor for the generated ``Functor(Clusterer)`` predicate clause.',
		argnames is ['Dataset', 'Clusterer', 'Functor', 'File']
	]).

	:- public(print_clusterer/1).
	:- mode(print_clusterer(+compound), one).
	:- info(print_clusterer/1, [
		comment is 'Prints a clusterer to the current output stream in a human-readable format.',
		argnames is ['Clusterer']
	]).

:- end_protocol.
