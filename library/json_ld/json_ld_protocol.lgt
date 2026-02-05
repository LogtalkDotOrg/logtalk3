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


:- protocol(json_ld_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-05,
		comment is 'JSON-LD 1.1 parser, generator, and processor protocol.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --term), one_or_error).
	:- info(parse/2, [
		comment is 'Parses a JSON-LD document from the given source (``file(Path)``, ``stream(Stream)``, ``codes(List)``, ``chars(List)``, or ``atom(Atom)``) into a term.',
		argnames is ['Source', 'Term'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(json_ld_source, 'Source')
		]
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++term), one_or_error).
	:- info(generate/2, [
		comment is 'Generates the content using the representation specified in the first argument (``file(Path)``, ``stream(Stream)``, ``codes(List)``, ``chars(List)``, or ``atom(Atom)``) for the term in the second argument.',
		argnames is ['Sink', 'Term'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` cannot be generated' - domain_error(json_ld_sink, 'Sink')
		]
	]).

	:- public(expand/2).
	:- mode(expand(+term, --list), one_or_error).
	:- info(expand/2, [
		comment is 'Expands a parsed JSON-LD document. Expansion removes the context and represents all properties and types as full IRIs. The result is a list of node objects in expanded document form.',
		argnames is ['Document', 'Expanded'],
		exceptions is [
			'``Document`` is a variable' - instantiation_error
		]
	]).

	:- public(compact/3).
	:- mode(compact(+term, +term, --term), one_or_error).
	:- info(compact/3, [
		comment is 'Compacts an expanded JSON-LD document using the given context. Compaction applies the context to shorten IRIs to terms or compact IRIs.',
		argnames is ['Document', 'Context', 'Compacted'],
		exceptions is [
			'``Document`` is a variable' - instantiation_error,
			'``Context`` is a variable' - instantiation_error
		]
	]).

	:- public(flatten/2).
	:- mode(flatten(+term, --term), one_or_error).
	:- info(flatten/2, [
		comment is 'Flattens an expanded JSON-LD document. Flattening collects all node objects into a flat ``@graph`` array, with nested nodes replaced by references. Blank node identifiers are generated for nodes without ``@id``.',
		argnames is ['Document', 'Flattened'],
		exceptions is [
			'``Document`` is a variable' - instantiation_error
		]
	]).

:- end_protocol.
