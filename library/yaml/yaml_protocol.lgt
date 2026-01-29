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


:- protocol(yaml_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-25,
		comment is 'YAML parser and generator protocol.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --ground), one_or_error).
	:- info(parse/2, [
		comment is 'Parses YAML content read from the given source (``file(Path)``, ``stream(Stream)``, ``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) into a ground term with the ``yaml`` functor representing the parsed YAML data.',
		argnames is ['Source', 'YAML'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(yaml_source, 'Source')
		]
	]).

	:- public(parse_all/2).
	:- mode(parse_all(++compound, --list(ground)), one_or_error).
	:- info(parse_all/2, [
		comment is 'Parses all YAML documents from the given source (``file(Path)``, ``stream(Stream)``, ``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) into a list of ground terms. Documents are separated by ``---`` markers and optionally terminated by ``...`` markers.',
		argnames is ['Source', 'YAMLs'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(yaml_source, 'Source')
		]
	]).

	:- public(generate/2).
	:- mode(generate(++compound, +ground), one_or_error).
	:- info(generate/2, [
		comment is 'Generates YAML output using the representation specified in the first argument (``file(Path)``, ``stream(Stream)``, ``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) from the ground YAML term in the second argument (with the ``yaml`` functor).',
		argnames is ['Sink', 'YAML'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``YAML`` is a variable' - instantiation_error,
			'``YAML`` is not a valid YAML term' - domain_error(yaml_term, 'YAML'),
			'``Sink`` cannot be generated' - domain_error(yaml_sink, 'Sink')
		]
	]).

	:- public(generate_all/2).
	:- mode(generate_all(++compound, +list(ground)), one_or_error).
	:- info(generate_all/2, [
		comment is 'Generates YAML output with multiple documents separated by ``---`` markers using the representation specified in the first argument (``file(Path)``, ``stream(Stream)``, ``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) from the list of ground YAML terms in the second argument.',
		argnames is ['Sink', 'YAMLs'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``YAMLs`` is a variable' - instantiation_error,
			'``YAMLs`` is not a list' - type_error(list, 'YAMLs'),
			'An element of ``YAMLs`` is not a valid YAML term' - domain_error(yaml_term, 'Term'),
			'``Sink`` cannot be generated' - domain_error(yaml_sink, 'Sink')
		]
	]).

:- end_protocol.
