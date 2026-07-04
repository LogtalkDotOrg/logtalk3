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


:- protocol(json_path_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-03,
		comment is 'JSONPath parser, generator, and evaluator protocol.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --compound), zero_or_one_or_error).
	:- info(parse/2, [
		comment is 'Parses a JSONPath query from the given source (``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) into a parsed query term. Fails if the query cannot be parsed.',
		argnames is ['Source', 'Query'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(json_path_source, 'Source')
		]
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++compound), one_or_error).
	:- info(generate/2, [
		comment is 'Generates a JSONPath query using the representation specified in the first argument (``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) for the parsed query term in the second argument.',
		argnames is ['Sink', 'Query'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Query`` is neither a variable nor a valid parsed query term' - domain_error(json_path_query, 'Query'),
			'``Sink`` cannot be generated' - domain_error(json_path_sink, 'Sink')
		]
	]).

	:- public(evaluate/3).
	:- mode(evaluate(++compound, ++term, --list), zero_or_one_or_error).
	:- info(evaluate/3, [
		comment is 'Evaluates a parsed JSONPath query against a JSON term and returns the selected values.',
		argnames is ['Query', 'JSON', 'Values'],
		exceptions is [
			'``Query`` is a variable' - instantiation_error,
			'``JSON`` is a variable' - instantiation_error,
			'``Query`` is neither a variable nor a valid parsed query term' - domain_error(json_path_query, 'Query')
		]
	]).

	:- public(paths/3).
	:- mode(paths(++compound, ++term, --list), zero_or_one_or_error).
	:- info(paths/3, [
		comment is 'Evaluates a parsed JSONPath query against a JSON term and returns the selected normalized paths.',
		argnames is ['Query', 'JSON', 'Paths'],
		exceptions is [
			'``Query`` is a variable' - instantiation_error,
			'``JSON`` is a variable' - instantiation_error,
			'``Query`` is neither a variable nor a valid parsed query term' - domain_error(json_path_query, 'Query')
		]
	]).

	:- public(nodes/3).
	:- mode(nodes(++compound, ++term, --list), zero_or_one_or_error).
	:- info(nodes/3, [
		comment is 'Evaluates a parsed JSONPath query against a JSON term and returns the selected nodes as ``node(Path, Value)`` terms.',
		argnames is ['Query', 'JSON', 'Nodes'],
		exceptions is [
			'``Query`` is a variable' - instantiation_error,
			'``JSON`` is a variable' - instantiation_error,
			'``Query`` is neither a variable nor a valid parsed query term' - domain_error(json_path_query, 'Query')
		]
	]).

	:- public(query/3).
	:- mode(query(++compound, ++term, --list), zero_or_one_or_error).
	:- info(query/3, [
		comment is 'Parses a JSONPath query source and evaluates it against a JSON term, returning the selected values.',
		argnames is ['Source', 'JSON', 'Values'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``JSON`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(json_path_source, 'Source')
		]
	]).

:- end_protocol.
