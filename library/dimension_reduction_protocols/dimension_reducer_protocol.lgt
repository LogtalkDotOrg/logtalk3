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


:- protocol(dimension_reducer_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-20,
		comment is 'Protocol for machine learning dimension reducers.',
		see_also is [dimension_reduction_dataset_protocol, supervised_dimension_reduction_dataset_protocol, target_supervised_dimension_reduction_dataset_protocol]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, ++list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a dimension reducer from the given dataset object using the given options.',
		argnames is ['Dataset', 'DimensionReducer', 'Options']
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -compound), one).
	:- info(learn/2, [
		comment is 'Learns a dimension reducer from the given dataset object using default options.',
		argnames is ['Dataset', 'DimensionReducer']
	]).

	:- public(transform/3).
	:- mode(transform(+compound, +list, -list(pair)), one).
	:- info(transform/3, [
		comment is 'Transforms a new instance into a reduced representation using the learned dimension reducer. The instance is a list of ``Attribute-Value`` pairs and the reduced representation is a list of ``Component-Value`` pairs.',
		argnames is ['DimensionReducer', 'Instance', 'ReducedInstance']
	]).

	:- public(check_dimension_reducer/1).
	:- mode(check_dimension_reducer(+compound), one).
	:- info(check_dimension_reducer/1, [
		comment is 'Checks that a learned dimension reducer term is structurally valid for the receiving implementation. Throws an exception when the term is not a valid dimension reducer representation.',
		argnames is ['DimensionReducer']
	]).

	:- public(valid_dimension_reducer/1).
	:- mode(valid_dimension_reducer(+compound), zero_or_one).
	:- info(valid_dimension_reducer/1, [
		comment is 'True when a learned dimension reducer term is structurally valid for the receiving implementation. Succeeds iff ``check_dimension_reducer/1`` succeeds without throwing an exception.',
		argnames is ['DimensionReducer']
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+compound, -list(compound)), one).
	:- info(diagnostics/2, [
		comment is 'Returns diagnostics and metadata associated with a learned dimension reducer in a representation-independent way.',
		argnames is ['DimensionReducer', 'Diagnostics']
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+compound, ?compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Tests or enumerates individual diagnostics metadata terms for a learned dimension reducer.',
		argnames is ['DimensionReducer', 'Diagnostic']
	]).

	:- public(dimension_reducer_options/2).
	:- mode(dimension_reducer_options(+compound, -list(compound)), zero_or_one).
	:- info(dimension_reducer_options/2, [
		comment is 'Returns the effective training options recorded in a learned dimension reducer diagnostics list.',
		argnames is ['DimensionReducer', 'Options']
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +callable, -list(clause)), one).
	:- info(export_to_clauses/4, [
		comment is 'Converts a dimension reducer into a list of predicate clauses. ``Functor`` is the functor for the generated single-argument predicate clauses whose argument is the serialized dimension reducer term.',
		argnames is ['Dataset', 'DimensionReducer', 'Functor', 'Clauses']
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +callable, +atom), one).
	:- info(export_to_file/4, [
		comment is 'Exports a dimension reducer to a file. ``Functor`` is the functor for the generated single-argument predicate clauses whose argument is the serialized dimension reducer term.',
		argnames is ['Dataset', 'DimensionReducer', 'Functor', 'File']
	]).

	:- public(print_dimension_reducer/1).
	:- mode(print_dimension_reducer(+compound), one).
	:- info(print_dimension_reducer/1, [
		comment is 'Prints a dimension reducer to the current output stream in a human-readable format.',
		argnames is ['DimensionReducer']
	]).

:- end_protocol.
