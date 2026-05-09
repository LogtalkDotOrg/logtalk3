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


:- protocol(wkt_wkb_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-09,
		comment is 'Well-Known Text (WKT) and Well-Known Binary (WKB) geometry parser, generator, and validator protocol.',
		see_also is [wkt_wkb, geo_json, geospatial]
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --term), one_or_error).
	:- info(parse/2, [
		comment is 'Parses a geometry from the given source. Supported sources are ``wkt(file(Path))``, ``wkt(stream(Stream))``, ``wkt(atom(Atom))``, ``wkt(chars(List))``, ``wkt(codes(List))``, ``wkb(file(Path))``, ``wkb(stream(Stream))``, ``wkb(bytes(List))``, ``wkb(hex(atom(Atom)))``, ``wkb(hex(chars(List)))``, and ``wkb(hex(codes(List)))``.',
		argnames is ['Source', 'Geometry'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(wkt_wkb_source, 'Source'),
			'``Source`` data does not represent a valid WKT or WKB geometry' - domain_error(wkt_wkb, 'Source')
		]
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++term), one_or_error).
	:- info(generate/2, [
		comment is 'Generates WKT or WKB content using the representation specified in the first argument from a native geometry term. Supported sinks are ``wkt(file(Path))``, ``wkt(stream(Stream))``, ``wkt(atom(Atom))``, ``wkt(chars(List))``, ``wkt(codes(List))``, ``wkb(file(Path))``, ``wkb(stream(Stream))``, ``wkb(bytes(List))``, ``wkb(hex(atom(Atom)))``, ``wkb(hex(chars(List)))``, ``wkb(hex(codes(List)))``, and the same WKB sink wrappers wrapped as ``wkb(Sink, Order)`` where ``Order`` is ``little`` or ``big``.',
		argnames is ['Sink', 'Geometry'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid sink' - domain_error(wkt_wkb_sink, 'Sink'),
			'``Geometry`` is not a valid native geometry term' - domain_error(wkt_wkb, 'Geometry'),
			'``Geometry`` is valid but cannot be represented in WKT due to special ordinates' - domain_error(wkt_wkb_representation, wkt('Geometry'))
		]
	]).

	:- public(validate/1).
	:- mode(validate(@term), zero_or_one).
	:- info(validate/1, [
		comment is 'True iff the argument is a valid native WKT/WKB geometry term.',
		argnames is ['Geometry']
	]).

	:- public(validate/2).
	:- mode(validate(@term, -list(compound)), one_or_error).
	:- info(validate/2, [
		comment is 'Validates a native geometry term returning a list of reason terms whose final argument is the failing path.',
		argnames is ['Geometry', 'Errors'],
		remarks is [
			'invalid_geometry_term(Path)' - 'The term is not any supported geometry representation.',
			'invalid_options(Path)' - 'The options argument is not a list.',
			'unknown_option(Option, Path)' - 'An option term is not recognized.',
			'duplicate_option(Name, Path)' - 'The same option name occurs more than once.',
			'invalid_dimensions(Path)' - 'A ``dimensions/1`` option does not use one of the supported values ``xy``, ``z``, ``m``, or ``zm``.',
			'invalid_position(Path)' - 'A position is not a list of numeric or supported special IEEE-754 coordinates with the expected dimensionality.',
			'invalid_position_array(Path)' - 'A coordinate array expected to contain positions is not a list.',
			'mixed_coordinate_dimension(Path)' - 'Coordinates in the same geometry do not all have the same dimensionality.',
			'coordinate_dimension_mismatch(Actual, Expected, Path)' - 'The coordinates dimensionality does not match the ``dimensions/1`` option.',
			'insufficient_positions(MinimumLength, Path)' - 'A coordinate array contains fewer positions than required for the enclosing geometry.',
			'ring_not_closed(Path)' - 'A polygon ring does not repeat its first position as its last position.',
			'invalid_polygon(Path)' - 'A polygon coordinate structure is invalid.',
			'invalid_multi_polygon(Path)' - 'A multi-polygon coordinate structure is invalid.',
			'invalid_geometry_collection(Path)' - 'A geometry collection member is not a list of valid geometry terms.'
		]
	]).

:- end_protocol.
