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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-11,
		comment is 'Unit tests for the "geohash" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(geohash, [
		valid_geohash/1, encode/3, decode/2, bounding_box/2, precision/3, cell_dimensions/3, adjacent/3,
		neighbors/2, covering/3, covering/4, compress/2, parent/2, children/2, common_prefix/3,
		encode_int/3, decode_int/3, bounding_box_int/3, adjacent_int/4, neighbors_int/3,
		geohash_to_int/2, int_to_geohash/3, encode_bits/3, decode_bits/3, bounding_box_bits/3,
		adjacent_bits/4, neighbors_bits/3, expand/2, expand_int/3, polygon_covering/4, polyline_covering/4
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(geohash).

	test(geohash_valid_geohash_1_01, deterministic) :-
		valid_geohash('ezs42').

	test(geohash_valid_geohash_1_02, deterministic) :-
		valid_geohash('u4pruydqqvj').

	test(geohash_valid_geohash_1_03, fail) :-
		valid_geohash('').

	test(geohash_valid_geohash_1_04, fail) :-
		valid_geohash('ezsa2').

	test(geohash_encode_3_01, deterministic(Geohash == 'ezs42')) :-
		encode(geographic(42.6, -5.6), 5, Geohash).

	test(geohash_encode_3_02, deterministic(Geohash == 'u4pruydqqvj')) :-
		encode(geographic(57.64911, 10.40744), 11, Geohash).

	test(geohash_encode_3_03, fail) :-
		encode(geographic(95.0, 0.0), 5, _).

	test(geohash_encode_3_04, fail) :-
		encode(geographic(42.6, -5.6), 0, _).

	test(geohash_encode_3_05, deterministic((Geohash1 == Geohash2, Geohash1 == '800'))) :-
		encode(geographic(0.0, 180.0), 3, Geohash1),
		encode(geographic(0.0, -180.0), 3, Geohash2).

	test(geohash_decode_2_01, deterministic((Latitude =~= 42.60498046875, Longitude =~= -5.60302734375))) :-
		decode('ezs42', geographic(Latitude, Longitude)).

	test(geohash_bounding_box_2_01, deterministic((MinLatitude =~= 42.5830078125, MinLongitude =~= -5.625, MaxLatitude =~= 42.626953125, MaxLongitude =~= -5.5810546875))) :-
		bounding_box('ezs42', bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude))).

	test(geohash_precision_3_01, deterministic((LatitudeError =~= 0.02197265625, LongitudeError =~= 0.02197265625))) :-
		precision(5, LatitudeError, LongitudeError).

	test(geohash_cell_dimensions_3_01, deterministic((LatitudeSpan =~= 0.0439453125, LongitudeSpan =~= 0.0439453125))) :-
		cell_dimensions(5, LatitudeSpan, LongitudeSpan).

	test(geohash_adjacent_3_01, deterministic) :-
		adjacent('ezs42', north, North),
		adjacent(North, south, 'ezs42').

	test(geohash_adjacent_3_02, deterministic) :-
		adjacent('ezs42', east, East),
		adjacent(East, west, 'ezs42').

	test(geohash_adjacent_3_03, deterministic(Longitude < -179.0)) :-
		encode(geographic(0.0, 179.95), 3, Geohash),
		adjacent(Geohash, east, East),
		decode(East, geographic(_, Longitude)).

	test(geohash_adjacent_3_04, fail) :-
		encode(geographic(89.9, 0.0), 2, Geohash),
		adjacent(Geohash, north, _).

	test(geohash_adjacent_3_05, fail) :-
		encode(geographic(-89.9, 0.0), 2, Geohash),
		adjacent(Geohash, south, _).

	test(geohash_neighbors_2_01, deterministic((Length == 8, memberchk(north-North, NeighborPairs), memberchk(east-East, NeighborPairs), memberchk(south-South, NeighborPairs), memberchk(west-West, NeighborPairs)))) :-
		neighbors('ezs42', NeighborPairs),
		length(NeighborPairs, Length),
		adjacent('ezs42', north, North),
		adjacent('ezs42', east, East),
		adjacent('ezs42', south, South),
		adjacent('ezs42', west, West).

	test(geohash_covering_3_01, deterministic(Geohashes == ['ezs42'])) :-
		bounding_box('ezs42', BoundingBox),
		covering(BoundingBox, 5, Geohashes).

	test(geohash_covering_3_02, deterministic((memberchk(WestHash, Geohashes), memberchk(EastHash, Geohashes), all_valid_hashes(Geohashes)))) :-
		encode(geographic(0.0, 179.95), 3, WestHash),
		encode(geographic(0.0, -179.95), 3, EastHash),
		covering(bbox(geographic(-0.1, 179.9), geographic(0.1, -179.9)), 3, Geohashes).

	test(geohash_covering_3_03, deterministic(Geohashes == ['s00000'])) :-
		covering(bbox(geographic(0.0, 0.0), geographic(0.0, 0.0)), 6, Geohashes).

	test(geohash_covering_3_04, deterministic((Length == 32, all_valid_hashes(Geohashes)))) :-
		covering(bbox(geographic(-90.0, -180.0), geographic(90.0, 180.0)), 1, Geohashes),
		length(Geohashes, Length).

	test(geohash_covering_4_01, deterministic(Geohashes4 == Geohashes3)) :-
		BoundingBox = bbox(geographic(42.58, -5.63), geographic(42.63, -5.58)),
		covering(BoundingBox, 5, Geohashes3),
		covering(BoundingBox, precision(5), Geohashes4, []).

	test(geohash_covering_4_02, deterministic(Geohashes == ['s0'])) :-
		bounding_box('s0', BoundingBox),
		covering(BoundingBox, max_precision(4), Geohashes, []).

	test(geohash_covering_4_03, deterministic((CompactLength < FixedLength, Geohashes == ['s000']))) :-
		bounding_box('s000', BoundingBox),
		covering(BoundingBox, precision(5), FixedGeohashes, []),
		covering(BoundingBox, precision(5), Geohashes, [compact(true), min_precision(4)]),
		length(FixedGeohashes, FixedLength),
		length(Geohashes, CompactLength).

	test(geohash_compress_2_01, deterministic(Compressed == ['s000'])) :-
		children('s000', Children),
		compress(Children, Compressed).

	test(geohash_parent_2_01, deterministic(Parent == 'ezs4')) :-
		parent('ezs42', Parent).

	test(geohash_children_2_01, deterministic((Length == 32, memberchk('ezs42', Children)))) :-
		children('ezs4', Children),
		length(Children, Length).

	test(geohash_common_prefix_3_01, deterministic(Prefix == 'ezs4')) :-
		common_prefix('ezs42', 'ezs48', Prefix).

	test(geohash_encode_int_3_01, deterministic(HashInteger == 14672002)) :-
		encode_int(geographic(42.6, -5.6), 5, HashInteger).

	test(geohash_decode_int_3_01, deterministic((Latitude =~= 42.60498046875, Longitude =~= -5.60302734375))) :-
		encode_int(geographic(42.6, -5.6), 5, HashInteger),
		decode_int(HashInteger, 5, geographic(Latitude, Longitude)).

	test(geohash_bounding_box_int_3_01, deterministic((MinLatitude =~= 42.5830078125, MinLongitude =~= -5.625, MaxLatitude =~= 42.626953125, MaxLongitude =~= -5.5810546875))) :-
		encode_int(geographic(42.6, -5.6), 5, HashInteger),
		bounding_box_int(HashInteger, 5, bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude))).

	test(geohash_adjacent_int_4_01, deterministic) :-
		encode_int(geographic(42.6, -5.6), 5, HashInteger),
		adjacent_int(HashInteger, 5, north, NorthInteger),
		adjacent_int(NorthInteger, 5, south, HashInteger).

	test(geohash_neighbors_int_3_01, deterministic((Length == 8, memberchk(north-NorthInteger, NeighborPairs)))) :-
		encode_int(geographic(42.6, -5.6), 5, HashInteger),
		neighbors_int(HashInteger, 5, NeighborPairs),
		length(NeighborPairs, Length),
		adjacent_int(HashInteger, 5, north, NorthInteger).

	test(geohash_geohash_to_int_2_01, deterministic(HashInteger == 14672002)) :-
		geohash_to_int('ezs42', HashInteger).

	test(geohash_int_to_geohash_3_01, deterministic(Geohash == 'ezs42')) :-
		int_to_geohash(14672002, 5, Geohash).

	test(geohash_encode_bits_3_01, deterministic(HashInteger == 14672002)) :-
		encode_bits(geographic(42.6, -5.6), 25, HashInteger).

	test(geohash_decode_bits_3_01, deterministic((Latitude =~= 42.60498046875, Longitude =~= -5.60302734375))) :-
		encode_bits(geographic(42.6, -5.6), 25, HashInteger),
		decode_bits(HashInteger, 25, geographic(Latitude, Longitude)).

	test(geohash_bounding_box_bits_3_01, deterministic((MinLatitude =~= 42.5830078125, MinLongitude =~= -5.625, MaxLatitude =~= 42.626953125, MaxLongitude =~= -5.5810546875))) :-
		encode_bits(geographic(42.6, -5.6), 25, HashInteger),
		bounding_box_bits(HashInteger, 25, bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude))).

	test(geohash_adjacent_bits_4_01, deterministic) :-
		encode_bits(geographic(42.6, -5.6), 25, HashInteger),
		adjacent_bits(HashInteger, 25, north, NorthInteger),
		adjacent_bits(NorthInteger, 25, south, HashInteger).

	test(geohash_neighbors_bits_3_01, deterministic((Length == 8, memberchk(north-NorthInteger, NeighborPairs)))) :-
		encode_bits(geographic(42.6, -5.6), 25, HashInteger),
		neighbors_bits(HashInteger, 25, NeighborPairs),
		length(NeighborPairs, Length),
		adjacent_bits(HashInteger, 25, north, NorthInteger).

	test(geohash_expand_2_01, deterministic((Length == 9, ExpandedGeohashes = ['ezs42'| _], memberchk(North, ExpandedGeohashes)))) :-
		expand('ezs42', ExpandedGeohashes),
		length(ExpandedGeohashes, Length),
		adjacent('ezs42', north, North).

	test(geohash_expand_int_3_01, deterministic((Length == 9, ExpandedHashes = [HashInteger| _], memberchk(NorthInteger, ExpandedHashes)))) :-
		encode_int(geographic(42.6, -5.6), 5, HashInteger),
		expand_int(HashInteger, 5, ExpandedHashes),
		length(ExpandedHashes, Length),
		adjacent_int(HashInteger, 5, north, NorthInteger).

	test(geohash_polygon_covering_4_01, deterministic(Geohashes == ['ezs42'])) :-
		bounding_box('ezs42', BoundingBox),
		bounding_box_polygon(BoundingBox, Polygon),
		polygon_covering(Polygon, precision(5), Geohashes, []).

	test(geohash_polygon_covering_4_02, deterministic(Geohashes == ['ezs42'])) :-
		bounding_box('ezs42', BoundingBox),
		bounding_box_polygon(BoundingBox, Polygon),
		polygon_covering(Polygon, max_precision(7), Geohashes, []).

	test(geohash_polyline_covering_4_01, deterministic(Geohashes == ['ezs42'])) :-
		polyline_covering([geographic(42.60, -5.61), geographic(42.61, -5.60)], precision(5), Geohashes, []).

	test(geohash_polyline_covering_4_02, deterministic((memberchk('ezs42', Geohashes), memberchk(East, Geohashes), all_valid_hashes(Geohashes)))) :-
		decode('ezs42', Coordinate1),
		adjacent('ezs42', east, East),
		decode(East, Coordinate2),
		polyline_covering([Coordinate1, Coordinate2], precision(5), Geohashes, []).

	all_valid_hashes([]).
	all_valid_hashes([Geohash| Geohashes]) :-
		valid_geohash(Geohash),
		all_valid_hashes(Geohashes).

	bounding_box_polygon(
		bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude)),
		[
			geographic(MinLatitude, MinLongitude),
			geographic(MaxLatitude, MinLongitude),
			geographic(MaxLatitude, MaxLongitude),
			geographic(MinLatitude, MaxLongitude)
		]
	).

:- end_object.
