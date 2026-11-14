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


:- protocol(omt_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-14,
		comment is 'Protocol for single-objective optimization modulo theories.'
	]).

	:- public(solve/2).
	:- mode(solve(+compound, -list(compound)), one_or_error).
	:- info(solve/2, [
		comment is 'Solves an OMT source with default options and returns one result per check.',
		argnames is ['Source', 'Results']
	]).

	:- public(solve/3).
	:- mode(solve(+compound, -list(compound), +list(compound)), one_or_error).
	:- info(solve/3, [
		comment is 'Solves with solver/1, executable/1, and time_limit/1 options. OptiMathSAT rejects nonzero time limits.',
		argnames is ['Source', 'Results', 'Options'],
		exceptions is [
			'An OptiMathSAT time limit is nonzero' - domain_error(optimathsat_time_limit, 'Seconds'),
			'Solver output or process status is invalid' - omt_solver_error('Formal', 'Status', 'Diagnostics', 'SourceMap')
		]
	]).

	:- public(program/1).
	:- mode(program(+compound), one_or_error).
	:- info(program/1, [
		comment is 'Generates and prints a source without invoking a solver.',
		argnames is ['Source']
	]).

	:- public(program/2).
	:- mode(program(+compound, -list(integer)), one_or_error).
	:- info(program/2, [
		comment is 'Generates source character codes without invoking a solver.',
		argnames is ['Source', 'Codes']
	]).

	:- public(print_program/1).
	:- mode(print_program(+list(integer)), one_or_error).
	:- info(print_program/1, [
		comment is 'Prints source character codes.',
		argnames is ['Codes']
	]).

	:- public(status/2).
	:- mode(status(+compound, -atom), one_or_error).
	:- info(status/2, [
		comment is 'Returns the status of a result.',
		argnames is ['Result', 'Status']
	]).

	:- public(model_binding/2).
	:- mode(model_binding(+compound, -pair), zero_or_more).
	:- info(model_binding/2, [
		comment is 'Enumerates model bindings.',
		argnames is ['Result', 'Binding']
	]).

	:- public(model_value/3).
	:- mode(model_value(+compound, +atom, -term), zero_or_one).
	:- info(model_value/3, [
		comment is 'Returns a named model value.',
		argnames is ['Result', 'Name', 'Value']
	]).

	:- public(unsat_core_name/2).
	:- mode(unsat_core_name(+compound, -atom), zero_or_more).
	:- info(unsat_core_name/2, [
		comment is 'Enumerates unsatisfiable core names.',
		argnames is ['Result', 'Name']
	]).

	:- public(result_property/2).
	:- mode(result_property(+compound, ?compound), zero_or_more).
	:- info(result_property/2, [
		comment is 'Enumerates result properties.',
		argnames is ['Result', 'Property']
	]).

	:- public(print_result/1).
	:- mode(print_result(+compound), one_or_error).
	:- info(print_result/1, [
		comment is 'Prints a result and its objectives.',
		argnames is ['Result']
	]).

	:- public(print_results/1).
	:- mode(print_results(+list(compound)), one_or_error).
	:- info(print_results/1, [
		comment is 'Prints a list of results.',
		argnames is ['Results']
	]).

	:- public(capability/2).
	:- mode(capability(?atom, ?atom), zero_or_more).
	:- info(capability/2, [
		comment is 'Enumerates advisory solver capabilities.',
		argnames is ['Solver', 'Feature']
	]).

	:- public(objective/3).
	:- mode(objective(+compound, -term, -term), zero_or_more).
	:- info(objective/3, [
		comment is 'Enumerates objective expressions and exact solver-reported bounds from a get-objectives response.',
		argnames is ['Result', 'Expression', 'Bound']
	]).

:- end_protocol.
