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


:- protocol(ranker_protocol).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-04-20,
		comment is 'Protocol for machine learning rankers.',
		see_also is [bradley_terry, ranking_dataset_protocol, pairwise_ranking_dataset_protocol]
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -compound), one).
	:- info(learn/2, [
		comment is 'Learns a ranker from the given dataset object.',
		argnames is ['Dataset', 'Ranker']
	]).

	:- public(rank/3).
	:- mode(rank(+compound, +list, -list), one).
	:- info(rank/3, [
		comment is 'Ranks a list of candidate items using the learned ranker and returns the candidates ordered from most preferred to least preferred.',
		argnames is ['Ranker', 'Candidates', 'Ranking']
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+compound, -list(compound)), one).
	:- info(diagnostics/2, [
		comment is 'Returns diagnostics and metadata associated with a learned ranker in a representation-independent way.',
		argnames is ['Ranker', 'Diagnostics']
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+compound, ?compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Tests or enumerates individual diagnostics metadata terms for a learned ranker.',
		argnames is ['Ranker', 'Diagnostic']
	]).

	:- public(ranker_options/2).
	:- mode(ranker_options(+compound, -list(compound)), zero_or_one).
	:- info(ranker_options/2, [
		comment is 'Returns the effective training options recorded in a learned ranker diagnostics list.',
		argnames is ['Ranker', 'Options']
	]).

	:- public(ranker_to_clauses/4).
	:- mode(ranker_to_clauses(+object_identifier, +compound, +callable, -list(clause)), one).
	:- info(ranker_to_clauses/4, [
		comment is 'Converts a ranker into a list of predicate clauses. ``Functor`` is the functor for the generated predicate clauses. When exporting a serialized ranker term, a noun such as ``ranker`` or ``model`` is usually clearer than a verb or result label.',
		argnames is ['Dataset', 'Ranker', 'Functor', 'Clauses']
	]).

	:- public(ranker_to_file/4).
	:- mode(ranker_to_file(+object_identifier, +compound, +callable, +atom), one).
	:- info(ranker_to_file/4, [
		comment is 'Exports a ranker to a file. ``Functor`` is the functor for the generated predicate clauses. When exporting a serialized ranker term, a noun such as ``ranker`` or ``model`` is usually clearer than a verb or result label.',
		argnames is ['Dataset', 'Ranker', 'Functor', 'File']
	]).

	:- public(print_ranker/1).
	:- mode(print_ranker(+compound), one).
	:- info(print_ranker/1, [
		comment is 'Prints a ranker to the current output stream in a human-readable format.',
		argnames is ['Ranker']
	]).

:- end_protocol.
