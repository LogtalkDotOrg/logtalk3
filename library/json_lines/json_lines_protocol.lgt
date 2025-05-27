%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(json_lines_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-05-27,
		comment is 'JSON Lines parser and generator protocol.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --list(ground)), one_or_error).
	:- info(parse/2, [
		comment is 'Parses the JSON Lines contents read from the given source (``file(Path)``, ``stream(Stream)``, ``line(Stream)``, ``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) into a list of ground terms.',
		argnames is ['Source', 'Terms'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(json_lines_source, 'Source')
		]
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++list(ground)), one_or_error).
	:- info(generate/2, [
		comment is 'Generates the content using the representation specified in the first argument (``file(Path)``, ``stream(Stream)``, ``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) for the list of ground terms in the second argument.',
		argnames is ['Sink', 'Terms'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Terms`` is a variable' - instantiation_error,
			'``Terms`` is neither a variable nor a list' - type_error(list, 'Terms'),
			'``Term`` is a non-ground element of the list ``Terms``' - instantiation_error,
			'``Term`` is an element of the list ``Terms`` but not a valid JSON term' - domain_error(json_term, 'Term'),
			'``Sink`` cannot be generated' - domain_error(json_lines_sink, 'Sink')
		]
	]).

:- end_protocol.
