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


:- protocol(forecaster_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Protocol for time series forecasting models.',
		see_also is [time_series_dataset_protocol]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a forecasting model from the given time series dataset object using the specified options.',
		argnames is ['Dataset', 'Forecaster', 'Options']
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -compound), one).
	:- info(learn/2, [
		comment is 'Learns a forecasting model from the given time series dataset object using default options.',
		argnames is ['Dataset', 'Forecaster']
	]).

	:- public(forecast/3).
	:- mode(forecast(+compound, +non_negative_integer, -list(number)), one_or_error).
	:- info(forecast/3, [
		comment is 'Forecasts the next ``Horizon`` values beyond the training series using the learned forecasting model. A zero horizon returns an empty list.',
		argnames is ['Forecaster', 'Horizon', 'Forecasts'],
		exceptions is [
			'``Horizon`` is a variable' - instantiation_error,
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is an integer but is negative' - domain_error(non_negative_integer, 'Horizon')
		]
	]).

	:- public(check_forecaster/1).
	:- mode(check_forecaster(@compound), one_or_error).
	:- info(check_forecaster/1, [
		comment is 'Checks that a learned forecaster term is structurally valid for the receiving implementation. Throws an exception when the term is not a valid forecaster representation.',
		argnames is ['Forecaster'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is neither a variable nor a valid forecaster' - domain_error(forecaster, 'Forecaster')
		]
	]).

	:- public(valid_forecaster/1).
	:- mode(valid_forecaster(@compound), zero_or_one).
	:- info(valid_forecaster/1, [
		comment is 'True when a learned forecaster term is structurally valid for the receiving implementation. Succeeds iff ``check_forecaster/1`` succeeds without throwing an exception.',
		argnames is ['Forecaster']
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+compound, -list(compound)), one).
	:- info(diagnostics/2, [
		comment is 'Returns diagnostics metadata for a learned forecaster.',
		argnames is ['Forecaster', 'Diagnostics']
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+compound, ?compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Enumerates individual diagnostics metadata terms for a learned forecaster.',
		argnames is ['Forecaster', 'Diagnostic']
	]).

	:- public(forecaster_options/2).
	:- mode(forecaster_options(+compound, -list(compound)), one).
	:- info(forecaster_options/2, [
		comment is 'Returns the effective training options recorded in a learned forecaster diagnostics metadata.',
		argnames is ['Forecaster', 'Options']
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +atom, -list(clause)), one).
	:- info(export_to_clauses/4, [
		comment is 'Converts a forecaster into a list of predicate clauses. ``Functor`` is the functor for the generated predicate clauses. When exporting a serialized forecaster term, a noun such as ``forecaster`` or ``model`` is usually clearer than a verb such as ``forecast``.',
		argnames is ['Dataset', 'Forecaster', 'Functor', 'Clauses']
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +atom, +atom), one).
	:- info(export_to_file/4, [
		comment is 'Exports a forecaster to a file. ``Functor`` is the functor for the generated predicate clauses. When exporting a serialized forecaster term, a noun such as ``forecaster`` or ``model`` is usually clearer than a verb such as ``forecast``.',
		argnames is ['Dataset', 'Forecaster', 'Functor', 'File']
	]).

	:- public(print_forecaster/1).
	:- mode(print_forecaster(+compound), one).
	:- info(print_forecaster/1, [
		comment is 'Prints a forecaster to the current output stream in a human-readable format.',
		argnames is ['Forecaster']
	]).

:- end_protocol.
