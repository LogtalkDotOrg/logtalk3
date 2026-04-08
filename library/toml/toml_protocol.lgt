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


:- protocol(toml_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-09,
		comment is 'TOML parser and generator protocol.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --ground), zero_or_one).
	:- info(parse/2, [
		comment is 'Parses TOML content read from the given source (``file(Path)``, ``stream(Stream)``, ``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) into a ground term representing the parsed TOML document.',
		argnames is ['Source', 'TOML'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(toml_source, 'Source')
		]
	]).

	:- public(generate/2).
	:- mode(generate(++compound, +ground), one_or_error).
	:- info(generate/2, [
		comment is 'Generates TOML output using the representation specified in the first argument (``file(Path)``, ``stream(Stream)``, ``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) from the ground TOML term in the second argument.',
		argnames is ['Sink', 'TOML'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``TOML`` is a variable' - instantiation_error,
			'``TOML`` is not a valid TOML term' - domain_error(toml_term, 'TOML'),
			'``Sink`` cannot be generated' - domain_error(toml_sink, 'Sink')
		]
	]).

:- end_protocol.
