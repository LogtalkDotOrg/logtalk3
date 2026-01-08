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


:- protocol(toon_protocol).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2025-12-15,
		comment is 'TOON (Token-Oriented Object Notation) parser and generator protocol.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --term), one_or_error).
	:- info(parse/2, [
		comment is 'Parses the TOON contents read from the given source (``codes(List)``, ``stream(Stream)``, ``file(Path)``, ``chars(List)``, or ``atom(Atom)``) into a term. Throws an error if the TOON contents cannot be parsed.',
		argnames is ['Source', 'Term'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(toon_source, 'Source')
		]
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++term), one_or_error).
	:- info(generate/2, [
		comment is 'Generates the content using the representation specified in the first argument (``codes(List)``, ``stream(Stream)``, ``file(Path)``, ``chars(List)``, or ``atom(Atom)``) for the term in the second argument. Throws an error if this term cannot be processed.',
		argnames is ['Sink', 'Term'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid sink' - domain_error(toon_sink, 'Sink')
		]
	]).

:- end_protocol.
