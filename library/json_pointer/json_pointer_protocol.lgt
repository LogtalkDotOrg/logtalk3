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


:- protocol(json_pointer_protocol).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'JSON Pointer (RFC 6901) parser, generator, and evaluator protocol.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --list(ground)), zero_or_one_or_error).
	:- info(parse/2, [
		comment is 'Parses a JSON Pointer from the given source (``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) into a list of reference tokens. Fails if the pointer cannot be parsed.',
		argnames is ['Source', 'Pointer'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(json_pointer_source, 'Source')
		]
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++list(ground)), one_or_error).
	:- info(generate/2, [
		comment is 'Generates a JSON Pointer using the representation specified in the first argument (``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) for the list of reference tokens in the second argument.',
		argnames is ['Sink', 'Pointer'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Pointer`` is neither a variable nor a list' - type_error(list, 'Pointer'),
			'``Pointer`` contains an invalid reference token' - domain_error(json_pointer_token, 'Token'),
			'``Sink`` cannot be generated' - domain_error(json_pointer_sink, 'Sink')
		]
	]).

	:- public(parse_fragment/2).
	:- mode(parse_fragment(++compound, --list(ground)), zero_or_one_or_error).
	:- info(parse_fragment/2, [
		comment is 'Parses a URI fragment representation of a JSON Pointer from the given source (``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) into a list of reference tokens. Fails if the fragment cannot be parsed.',
		argnames is ['Source', 'Pointer'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(json_pointer_fragment_source, 'Source')
		]
	]).

	:- public(generate_fragment/2).
	:- mode(generate_fragment(+compound, ++list(ground)), one_or_error).
	:- info(generate_fragment/2, [
		comment is 'Generates the URI fragment representation of a JSON Pointer using the representation specified in the first argument (``codes(Codes)``, ``chars(Chars)``, or ``atom(Atom)``) for the list of reference tokens in the second argument.',
		argnames is ['Sink', 'Pointer'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Pointer`` is neither a variable nor a list' - type_error(list, 'Pointer'),
			'``Pointer`` contains an invalid reference token' - domain_error(json_pointer_token, 'Token'),
			'``Sink`` cannot be generated' - domain_error(json_pointer_fragment_sink, 'Sink')
		]
	]).

	:- public(evaluate/3).
	:- mode(evaluate(++list(ground), ++term, ?term), zero_or_one_or_error).
	:- info(evaluate/3, [
		comment is 'Evaluates a parsed JSON Pointer against a JSON term. Fails if the pointer does not identify a value in the given JSON term.',
		argnames is ['Pointer', 'JSON', 'Value'],
		exceptions is [
			'``Pointer`` is a variable' - instantiation_error,
			'``JSON`` is a variable' - instantiation_error,
			'``Pointer`` is neither a variable nor a list' - type_error(list, 'Pointer'),
			'``Pointer`` contains an invalid reference token' - domain_error(json_pointer_token, 'Token')
		]
	]).

:- end_protocol.
