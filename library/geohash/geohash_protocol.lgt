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


:- protocol(geohash_protocol).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-05-11,
		comment is 'Geohash predicates protocol.',
		see_also is [geohash, geospatial]
	]).

	:- public(valid_geohash/1).
	:- mode(valid_geohash(+atom), zero_or_one).
	:- info(valid_geohash/1, [
		comment is 'True when the argument is a non-empty geohash atom using the standard geohash base-32 alphabet ``0123456789bcdefghjkmnpqrstuvwxyz``.',
		argnames is ['Geohash']
	]).

	:- public(encode/3).
	:- mode(encode(+compound, +positive_integer, -atom), zero_or_one).
	:- info(encode/3, [
		comment is 'Encodes a geographic coordinate represented as ``geographic(Latitude,Longitude)`` into a geohash atom with the given precision in characters. Longitude is canonicalized to the ``[-180.0,180.0[`` range, so ``180.0`` is treated as ``-180.0``.',
		argnames is ['Coordinate', 'Precision', 'Geohash']
	]).

	:- public(decode/2).
	:- mode(decode(+atom, -compound), zero_or_one).
	:- info(decode/2, [
		comment is 'Decodes a geohash atom into the center geographic coordinate of the represented cell.',
		argnames is ['Geohash', 'Coordinate']
	]).

	:- public(bounding_box/2).
	:- mode(bounding_box(+atom, -compound), zero_or_one).
	:- info(bounding_box/2, [
		comment is 'Decodes a geohash atom into its geographic bounding box represented as ``bbox(geographic(MinLatitude,MinLongitude),geographic(MaxLatitude,MaxLongitude))``.',
		argnames is ['Geohash', 'BoundingBox']
	]).

	:- public(precision/3).
	:- mode(precision(+positive_integer, -float, -float), zero_or_one).
	:- info(precision/3, [
		comment is 'Returns the maximum latitude and longitude errors in degrees for geohashes of the given precision in characters.',
		argnames is ['Precision', 'LatitudeError', 'LongitudeError']
	]).

	:- public(cell_dimensions/3).
	:- mode(cell_dimensions(+positive_integer, -float, -float), zero_or_one).
	:- info(cell_dimensions/3, [
		comment is 'Returns the latitude and longitude spans in degrees of a geohash cell for the given precision in characters.',
		argnames is ['Precision', 'LatitudeSpan', 'LongitudeSpan']
	]).

	:- public(adjacent/3).
	:- mode(adjacent(+atom, +atom, -atom), zero_or_one).
	:- info(adjacent/3, [
		comment is 'Returns the adjacent geohash in the given direction. Supported directions are ``north``, ``south``, ``east``, ``west``, ``north_east``, ``north_west``, ``south_east``, and ``south_west``. Eastward and westward adjacency wraps across the antimeridian using the same longitude canonicalization as ``encode/3``. Northward and southward adjacency fails when there is no neighboring cell beyond the poles.',
		argnames is ['Geohash', 'Direction', 'AdjacentGeohash']
	]).

	:- public(neighbors/2).
	:- mode(neighbors(+atom, -list(compound)), zero_or_one).
	:- info(neighbors/2, [
		comment is 'Returns the available neighboring geohashes as ``Direction-Geohash`` pairs in clockwise order starting at north.',
		argnames is ['Geohash', 'Neighbors']
	]).

	:- public(covering/3).
	:- mode(covering(+compound, +positive_integer, -list(atom)), zero_or_one).
	:- info(covering/3, [
		comment is 'Returns the list of geohashes at the given precision whose cells cover a geographic bounding box represented as ``bbox(geographic(MinLatitude,MinLongitude),geographic(MaxLatitude,MaxLongitude))``. Degenerate point bounding boxes and exact cell-aligned boundaries are accepted. Antimeridian-crossing bounding boxes are accepted.',
		argnames is ['BoundingBox', 'Precision', 'Geohashes']
	]).

	:- public(covering/4).
	:- mode(covering(+compound, +compound, -list(atom), ++list(compound)), zero_or_one).
	:- info(covering/4, [
		comment is 'Returns a geohash cover for a geographic bounding box. ``CoverSpec`` is either ``precision(Precision)`` for a fixed-depth cover or ``max_precision(MaxPrecision)`` for an adaptive mixed-depth cover. ``Options`` may include ``compact(Boolean)`` and ``min_precision(PositiveInteger)``.',
		argnames is ['BoundingBox', 'CoverSpec', 'Geohashes', 'Options']
	]).

	:- public(compress/2).
	:- mode(compress(+list(atom), -list(atom)), zero_or_one).
	:- info(compress/2, [
		comment is 'Compresses a list of geohashes by replacing complete sibling sets with their parent prefix while preserving the covered area.',
		argnames is ['Geohashes', 'CompressedGeohashes']
	]).

	:- public(parent/2).
	:- mode(parent(+atom, -atom), zero_or_one).
	:- info(parent/2, [
		comment is 'Returns the immediate parent prefix of a geohash. Fails for precision-1 geohashes.',
		argnames is ['Geohash', 'Parent']
	]).

	:- public(children/2).
	:- mode(children(+atom, -list(atom)), zero_or_one).
	:- info(children/2, [
		comment is 'Returns the 32 child geohashes of a geohash in standard geohash base-32 order.',
		argnames is ['Geohash', 'Children']
	]).

	:- public(common_prefix/3).
	:- mode(common_prefix(+atom, +atom, -atom), zero_or_one).
	:- info(common_prefix/3, [
		comment is 'Returns the longest common geohash prefix shared by two geohashes. The prefix may be the empty atom when the geohashes share no leading characters.',
		argnames is ['Geohash1', 'Geohash2', 'Prefix']
	]).

	:- public(encode_int/3).
	:- mode(encode_int(+compound, +positive_integer, -integer), zero_or_one).
	:- info(encode_int/3, [
		comment is 'Encodes a geographic coordinate into an integer geohash representation at the given precision in characters.',
		argnames is ['Coordinate', 'Precision', 'HashInteger']
	]).

	:- public(decode_int/3).
	:- mode(decode_int(+integer, +positive_integer, -compound), zero_or_one).
	:- info(decode_int/3, [
		comment is 'Decodes an integer geohash representation at the given precision into the center geographic coordinate of the represented cell.',
		argnames is ['HashInteger', 'Precision', 'Coordinate']
	]).

	:- public(bounding_box_int/3).
	:- mode(bounding_box_int(+integer, +positive_integer, -compound), zero_or_one).
	:- info(bounding_box_int/3, [
		comment is 'Decodes an integer geohash representation at the given precision into its geographic bounding box.',
		argnames is ['HashInteger', 'Precision', 'BoundingBox']
	]).

	:- public(adjacent_int/4).
	:- mode(adjacent_int(+integer, +positive_integer, +atom, -integer), zero_or_one).
	:- info(adjacent_int/4, [
		comment is 'Returns the adjacent integer geohash at the same precision in the given direction.',
		argnames is ['HashInteger', 'Precision', 'Direction', 'AdjacentHashInteger']
	]).

	:- public(neighbors_int/3).
	:- mode(neighbors_int(+integer, +positive_integer, -list(compound)), zero_or_one).
	:- info(neighbors_int/3, [
		comment is 'Returns the available neighboring integer geohashes as ``Direction-HashInteger`` pairs in clockwise order starting at north.',
		argnames is ['HashInteger', 'Precision', 'Neighbors']
	]).

	:- public(geohash_to_int/2).
	:- mode(geohash_to_int(+atom, -integer), zero_or_one).
	:- info(geohash_to_int/2, [
		comment is 'Converts a geohash atom to its packed integer representation using five bits per character.',
		argnames is ['Geohash', 'HashInteger']
	]).

	:- public(int_to_geohash/3).
	:- mode(int_to_geohash(+integer, +positive_integer, -atom), zero_or_one).
	:- info(int_to_geohash/3, [
		comment is 'Converts a packed integer geohash representation and an explicit precision in characters to a geohash atom.',
		argnames is ['HashInteger', 'Precision', 'Geohash']
	]).

	:- public(encode_bits/3).
	:- mode(encode_bits(+compound, +positive_integer, -integer), zero_or_one).
	:- info(encode_bits/3, [
		comment is 'Encodes a geographic coordinate into an integer geohash representation using the given bit precision.',
		argnames is ['Coordinate', 'Bits', 'HashInteger']
	]).

	:- public(decode_bits/3).
	:- mode(decode_bits(+integer, +positive_integer, -compound), zero_or_one).
	:- info(decode_bits/3, [
		comment is 'Decodes an integer geohash representation at the given bit precision into the center geographic coordinate of the represented cell.',
		argnames is ['HashInteger', 'Bits', 'Coordinate']
	]).

	:- public(bounding_box_bits/3).
	:- mode(bounding_box_bits(+integer, +positive_integer, -compound), zero_or_one).
	:- info(bounding_box_bits/3, [
		comment is 'Decodes an integer geohash representation at the given bit precision into its geographic bounding box.',
		argnames is ['HashInteger', 'Bits', 'BoundingBox']
	]).

	:- public(adjacent_bits/4).
	:- mode(adjacent_bits(+integer, +positive_integer, +atom, -integer), zero_or_one).
	:- info(adjacent_bits/4, [
		comment is 'Returns the adjacent integer geohash at the same bit precision in the given direction.',
		argnames is ['HashInteger', 'Bits', 'Direction', 'AdjacentHashInteger']
	]).

	:- public(neighbors_bits/3).
	:- mode(neighbors_bits(+integer, +positive_integer, -list(compound)), zero_or_one).
	:- info(neighbors_bits/3, [
		comment is 'Returns the available neighboring integer geohashes as ``Direction-HashInteger`` pairs in clockwise order starting at north for the given bit precision.',
		argnames is ['HashInteger', 'Bits', 'Neighbors']
	]).

	:- public(expand/2).
	:- mode(expand(+atom, -list(atom)), zero_or_one).
	:- info(expand/2, [
		comment is 'Returns the geohash followed by its available neighbors in clockwise order starting at north.',
		argnames is ['Geohash', 'ExpandedGeohashes']
	]).

	:- public(expand_int/3).
	:- mode(expand_int(+integer, +positive_integer, -list(integer)), zero_or_one).
	:- info(expand_int/3, [
		comment is 'Returns the integer geohash followed by its available neighboring integer geohashes in clockwise order starting at north.',
		argnames is ['HashInteger', 'Precision', 'ExpandedHashes']
	]).

	:- public(polygon_covering/4).
	:- mode(polygon_covering(+list(compound), +compound, -list(atom), ++list(compound)), zero_or_one).
	:- info(polygon_covering/4, [
		comment is 'Returns a geohash cover for a polygon represented as a list of geographic coordinates. ``CoverSpec`` is either ``precision(Precision)`` for a fixed-depth cover or ``max_precision(MaxPrecision)`` for an adaptive mixed-depth cover. ``Options`` may include ``compact(Boolean)`` and ``min_precision(PositiveInteger)``. Antimeridian-crossing polygons are not supported.',
		argnames is ['Polygon', 'CoverSpec', 'Geohashes', 'Options']
	]).

	:- public(polyline_covering/4).
	:- mode(polyline_covering(+list(compound), +compound, -list(atom), ++list(compound)), zero_or_one).
	:- info(polyline_covering/4, [
		comment is 'Returns a geohash cover for a polyline represented as a list of two or more geographic coordinates. ``CoverSpec`` is either ``precision(Precision)`` for a fixed-depth cover or ``max_precision(MaxPrecision)`` for an adaptive mixed-depth cover. ``Options`` may include ``compact(Boolean)``, ``min_precision(PositiveInteger)``, and ``buffer(Distance)`` where the buffer distance is given in kilometers. Antimeridian-crossing polylines are not supported.',
		argnames is ['Polyline', 'CoverSpec', 'Geohashes', 'Options']
	]).

:- end_protocol.
