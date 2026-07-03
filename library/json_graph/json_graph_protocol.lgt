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


:- protocol(json_graph_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-03,
		comment is 'JSON Graph Format v2 parser and generator protocol.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --list), one_or_error).
	:- info(parse/2, [
		comment is 'Parses the graph contents read from the given source (``codes(List)``, ``stream(Stream)``, ``file(Path)``, ``chars(List)``, ``atom(Atom)``, or ``object(Object)``) into a list of canonical graph terms. Fails if the contents cannot be parsed.',
		argnames is ['Source', 'Terms'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid JSON graph source term' - domain_error(json_graph_source, 'Source'),
			'``Source`` is ``object(Object)`` but ``Object`` does not conform to the graph data protocol' - domain_error(json_graph_source, 'Source')
		]
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++list), one_or_error).
	:- info(generate/2, [
		comment is 'Generates the graph contents using the representation specified in the first argument (``codes(List)``, ``stream(Stream)``, ``file(Path)``, ``chars(List)``, ``atom(Atom)``, or ``object(Object)``) for the list of canonical graph terms in the second argument.',
		argnames is ['Sink', 'Terms'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid JSON graph sink term' - domain_error(json_graph_sink, 'Sink'),
			'``Terms`` is neither a variable nor a valid list of canonical graph terms' - domain_error(json_graph_terms, 'Terms')
		]
	]).

:- end_protocol.
