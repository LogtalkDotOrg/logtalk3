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


:- protocol(gpx_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-02,
		comment is 'GPX 1.1 parser, generator, and validator protocol.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --compound), one_or_error).
	:- info(parse/2, [
		comment is 'Parses a GPX 1.1 document from the given source (``file(Path)``, ``stream(Stream)``, ``codes(List)``, ``chars(List)``, or ``atom(Atom)``) into a native GPX term.',
		argnames is ['Source', 'GPX'],
		exceptions is [
			'``Source`` is not a ground term' - instantiation_error,
			'``Source`` is a ground term but not a valid source' - domain_error(gpx_source, 'Source'),
			'The source does not contain a valid GPX 1.1 document' - domain_error(gpx, 'Source')
		]
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++compound), one_or_error).
	:- info(generate/2, [
		comment is 'Generates a GPX 1.1 document to the given sink (``file(Path)``, ``stream(Stream)``, ``codes(List)``, ``chars(List)``, or ``atom(Atom)``) from a native GPX term.',
		argnames is ['Sink', 'GPX'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid sink' - domain_error(gpx_sink, 'Sink'),
			'``GPX`` is not ground' - instantiation_error,
			'``GPX`` is ground but not a valid native GPX term' - domain_error(gpx, 'GPX')
		]
	]).

	:- public(validate/1).
	:- mode(validate(@term), zero_or_one_or_error).
	:- info(validate/1, [
		comment is 'True iff the argument is a valid native GPX 1.1 term.',
		argnames is ['GPX'],
		exceptions is [
			'``GPX`` is a variable' - instantiation_error
		]
	]).

	:- public(validate/2).
	:- mode(validate(@term, -list(compound)), one_or_error).
	:- info(validate/2, [
		comment is 'Validates a native GPX 1.1 term, returning all detected errors as reason terms whose final argument is the failing path. List indexes in paths are zero-based.',
		argnames is ['GPX', 'Errors'],
		exceptions is [
			'``GPX`` is a variable' - instantiation_error
		],
		remarks is [
			'invalid_gpx_term(Path)' - 'The term is not a supported native GPX term.',
			'invalid_creator(Path)' - 'The required creator is not an atom.',
			'invalid_properties(Path)' - 'A properties argument is not a list.',
			'invalid_list(Path)' - 'A repeated GPX value is not represented by a list.',
			'invalid_value(Value, Path)' - 'A value does not satisfy the required GPX type or range.',
			'invalid_date_time(Value, Path)' - 'A value is not an atom in the XML Schema 1.0 ``dateTime`` lexical space.',
			'invalid_year(Value, Path)' - 'A value is not an atom in the XML Schema 1.0 ``gYear`` lexical space.',
			'invalid_bounds(Path)' - 'The minimum latitude is greater than the maximum latitude.',
			'invalid_extensions(Path)' - 'The extension nodes are not valid non-GPX namespaced XML elements.',
			'duplicate_property(Name, Path)' - 'A property occurs more than once in the enclosing GPX term.',
			'unknown_property(Property, Path)' - 'The property is not supported by the enclosing GPX term.'
		]
	]).

	:- public(xml_to_gpx/2).
	:- mode(xml_to_gpx(+compound, -compound), one_or_error).
	:- info(xml_to_gpx/2, [
		comment is 'Converts an XML parser document term into a native GPX 1.1 term.',
		argnames is ['XML', 'GPX'],
		exceptions is [
			'``XML`` is not a ground term' - instantiation_error,
			'``XML`` is a ground term but not a valid GPX 1.1 XML document term' - domain_error(gpx, 'XML')
		]
	]).

	:- public(gpx_to_xml/2).
	:- mode(gpx_to_xml(+compound, -compound), one_or_error).
	:- info(gpx_to_xml/2, [
		comment is 'Converts a native GPX 1.1 term into an XML parser document term.',
		argnames is ['GPX', 'XML'],
		exceptions is [
			'``GPX`` is not a ground term' - instantiation_error,
			'``GPX`` is a ground term but not a valid native GPX term' - domain_error(gpx, 'GPX')
		]
	]).

:- end_protocol.
