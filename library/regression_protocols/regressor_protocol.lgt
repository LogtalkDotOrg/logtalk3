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


:- protocol(regressor_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-29,
		comment is 'Protocol for machine learning regressors.',
		see_also is [linear_regression, regression_dataset_protocol]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a regressor from the given dataset object using the specified options.',
		argnames is ['Dataset', 'Regressor', 'Options']
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -compound), one).
	:- info(learn/2, [
		comment is 'Learns a regressor from the given dataset object using default options.',
		argnames is ['Dataset', 'Regressor']
	]).

	:- public(predict/3).
	:- mode(predict(+compound, +list, -number), one).
	:- info(predict/3, [
		comment is 'Predicts the numeric target value for a new instance using the learned regressor. The instance is a list of ``Attribute-Value`` pairs.',
		argnames is ['Regressor', 'Instance', 'Target']
	]).

	:- public(check_regressor/1).
	:- mode(check_regressor(@compound), one_or_error).
	:- info(check_regressor/1, [
		comment is 'Checks that a learned regressor term is structurally valid for the receiving implementation. Throws an exception when the term is not a valid regressor representation.',
		argnames is ['Regressor']
	]).

	:- public(valid_regressor/1).
	:- mode(valid_regressor(@compound), zero_or_one).
	:- info(valid_regressor/1, [
		comment is 'True when a learned regressor term is structurally valid for the receiving implementation. Succeeds iff ``check_regressor/1`` succeeds without throwing an exception.',
		argnames is ['Regressor']
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +callable, -list(clause)), one).
	:- info(export_to_clauses/4, [
		comment is 'Converts a regressor into a list of predicate clauses. ``Functor`` is the functor for the generated predicate clauses. When exporting a serialized regressor term, a noun such as ``regressor`` or ``model`` is usually clearer than a verb such as ``predict``.',
		argnames is ['Dataset', 'Regressor', 'Functor', 'Clauses']
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +callable, +atom), one).
	:- info(export_to_file/4, [
		comment is 'Exports a regressor to a file. ``Functor`` is the functor for the generated predicate clauses. When exporting a serialized regressor term, a noun such as ``regressor`` or ``model`` is usually clearer than a verb such as ``predict``.',
		argnames is ['Dataset', 'Regressor', 'Functor', 'File']
	]).

	:- public(print_regressor/1).
	:- mode(print_regressor(+compound), one).
	:- info(print_regressor/1, [
		comment is 'Prints a regressor to the current output stream in a human-readable format.',
		argnames is ['Regressor']
	]).

:- end_protocol.
