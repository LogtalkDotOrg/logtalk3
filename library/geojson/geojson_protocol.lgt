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


:- protocol(geojson_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-15,
		comment is 'GeoJSON (RFC 7946) parser, generator, and validator protocol.',
		see_also is [geojson, json, geospatial]
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --term), one_or_error).
	:- info(parse/2, [
		comment is 'Parses a GeoJSON document from the given source (``file(Path)``, ``stream(Stream)``, ``codes(List)``, ``chars(List)``, or ``atom(Atom)``) into a native GeoJSON term.',
		argnames is ['Source', 'Term'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid source' - domain_error(geojson_source, 'Source'),
			'Parsed JSON does not represent a valid GeoJSON document' - domain_error(geojson, 'Document')
		]
	]).

	:- public(generate/2).
	:- mode(generate(+compound, ++term), one_or_error).
	:- info(generate/2, [
		comment is 'Generates GeoJSON content using the representation specified in the first argument (``file(Path)``, ``stream(Stream)``, ``codes(List)``, ``chars(List)``, or ``atom(Atom)``) from a native GeoJSON term.',
		argnames is ['Sink', 'Term'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid sink' - domain_error(geojson_sink, 'Sink'),
			'``Term`` is not a valid GeoJSON term' - domain_error(geojson, 'Term')
		]
	]).

	:- public(validate/1).
	:- mode(validate(@term), zero_or_one).
	:- info(validate/1, [
		comment is 'True iff the argument is a valid native GeoJSON term according to RFC 7946.',
		argnames is ['Term']
	]).

	:- public(validate/2).
	:- mode(validate(@term, -list(compound)), one_or_error).
	:- info(validate/2, [
		comment is 'Validates a native GeoJSON term returning a list of reason terms whose final argument is the failing path.',
		argnames is ['Term', 'Errors'],
		exceptions is [
			'``Term`` is a variable' - instantiation_error
		],
		remarks is [
			'invalid_geojson_term(Path)' - 'The term is not any supported GeoJSON geometry, feature, or feature collection representation.',
			'invalid_geometry(Path)' - 'A geometry member is neither a valid geometry term nor ``@null`` where ``@null`` is allowed.',
			'invalid_properties(Path)' - 'A feature properties member is neither ``@null`` nor a valid JSON object term in the selected representation.',
			'invalid_options(Path)' - 'The options argument is not a list.',
			'option_not_allowed(Name, Path)' - 'An option with the given name is not allowed for the enclosing GeoJSON term.',
			'duplicate_option(Name, Path)' - 'The same option name occurs more than once in the options list.',
			'unknown_option(Option, Path)' - 'An option term is not recognized as one of the supported options.',
			'invalid_bbox(Path)' - 'A bounding box is not a list of four or six numbers.',
			'bbox_longitude_out_of_range(Path)' - 'A bounding box longitude value lies outside the RFC 7946 range ``[-180,180]``.',
			'bbox_latitude_out_of_range(Path)' - 'A bounding box latitude value lies outside the RFC 7946 range ``[-90,90]``.',
			'bbox_latitude_order(Path)' - 'The south latitude value is greater than the north latitude value in a bounding box.',
			'bbox_altitude_order(Path)' - 'The minimum altitude value is greater than the maximum altitude value in a three-dimensional bounding box.',
			'bbox_dimension_mismatch(Actual, Expected, Path)' - 'A bounding box length does not match the dimensionality required by the represented geometry, feature, or collection.',
			'invalid_id(Path)' - 'A feature identifier is neither a number nor a valid string term in the selected representation.',
			'invalid_foreign_members(Path)' - 'Foreign members are not represented as a valid pair list or embedded JSON object term, or contain an invalid pair term.',
			'prohibited_member(Key, Path)' - 'A prohibited foreign member name such as ``crs`` was used.',
			'reserved_foreign_member(Key, Path)' - 'A reserved GeoJSON member name was used as a foreign member key.',
			'invalid_position(Path)' - 'A position is not a list of at least two numeric coordinates.',
			'position_longitude_out_of_range(Path)' - 'A position longitude value lies outside the RFC 7946 range ``[-180,180]``.',
			'position_latitude_out_of_range(Path)' - 'A position latitude value lies outside the RFC 7946 range ``[-90,90]``.',
			'invalid_json_object_pair(Path)' - 'A JSON object member is not represented using a supported key-value pair term.',
			'invalid_json_key(Path)' - 'A JSON object key is not a valid string term in the selected representation.',
			'duplicate_json_object_key(Key, Path)' - 'The same JSON object key occurs more than once in the same object.',
			'invalid_json_value(Path)' - 'A JSON value is not a number, boolean, null, valid string term, array, or object term.',
			'insufficient_positions(MinimumLength, Path)' - 'A coordinate array contains fewer positions than required for the enclosing geometry.',
			'invalid_position_array(Path)' - 'A coordinate array expected to contain positions is not a list of valid positions.',
			'inconsistent_position_dimension(Path)' - 'Positions in the same coordinate array do not all have the same dimensionality.',
			'invalid_line_string_array(Path)' - 'A MultiLineString coordinate array is not a valid list of line string coordinate arrays.',
			'invalid_polygon(Path)' - 'A polygon coordinate structure is invalid, for example because it is not a non-empty list of linear rings.',
			'ring_not_closed(Path)' - 'A linear ring does not repeat its first position as its last position.',
			'invalid_multi_polygon(Path)' - 'A MultiPolygon coordinate array is not a valid list of polygon coordinate arrays.',
			'invalid_geometry_collection(Path)' - 'A GeometryCollection geometries member is not a list.',
			'invalid_feature_collection(Path)' - 'A FeatureCollection features member is not a list.',
			'invalid_feature(Path)' - 'A value inside a FeatureCollection is not a valid feature term.'
		]
	]).

	:- public(json_to_geojson/2).
	:- mode(json_to_geojson(+term, -term), one_or_error).
	:- info(json_to_geojson/2, [
		comment is 'Converts a JSON term, as returned by the ``json`` library, into a native GeoJSON term.',
		argnames is ['JSON', 'GeoJSON'],
		exceptions is [
			'``JSON`` is a variable' - instantiation_error,
			'``JSON`` is not a valid GeoJSON JSON term' - domain_error(geojson, 'JSON')
		]
	]).

	:- public(geojson_to_json/2).
	:- mode(geojson_to_json(+term, -term), one_or_error).
	:- info(geojson_to_json/2, [
		comment is 'Converts a native GeoJSON term into a JSON term suitable for the ``json`` library.',
		argnames is ['GeoJSON', 'JSON'],
		exceptions is [
			'``GeoJSON`` is a variable' - instantiation_error,
			'``GeoJSON`` is not a valid GeoJSON term' - domain_error(geojson, 'GeoJSON')
		]
	]).

:- end_protocol.
