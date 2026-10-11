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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-11,
		comment is 'Unit tests for the "geohash" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(geohash, [
		valid_geohash/1, encode/3, decode/2, bounding_box/2, precision/3, cell_dimensions/3, adjacent/3,
		neighbors/2, covering/3, covering/4, compress/2, parent/2, children/2, common_prefix/3,
		encode_int/3, decode_int/3, bounding_box_int/3, adjacent_int/4, neighbors_int/3, geohash_to_int/2,
		int_to_geohash/3, encode_bits/3, decode_bits/3, bounding_box_bits/3, adjacent_bits/4,
		neighbors_bits/3, expand/2, expand_int/3, polygon_covering/4, polyline_covering/4
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, reverse/2
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

	test(geohash_covering_antimeridian_adaptive, deterministic(Adaptive == Fixed)) :-
		BoundingBox = bbox(geographic(-0.1, 179.9), geographic(0.1, -179.9)),
		covering(BoundingBox, 3, Fixed0),
		covering(BoundingBox, max_precision(3), Adaptive0, []),
		sort(Fixed0, Fixed),
		sort(Adaptive0, Adaptive).

	test(geohash_covering_adaptive_degenerate, deterministic) :-
		forall(
			member(BoundingBox, [
				bbox(geographic(0.0, 0.0), geographic(0.0, 0.0)),
				bbox(geographic(1.0, 1.0), geographic(1.0, 1.0)),
				bbox(geographic(90.0, 0.0), geographic(90.0, 0.0)),
				bbox(geographic(-90.0, -180.0), geographic(-90.0, -180.0)),
				bbox(geographic(0.0, -1.0), geographic(0.0, 1.0)),
				bbox(geographic(-1.0, 0.0), geographic(1.0, 0.0))
			]),
			(	covering(BoundingBox, 3, Fixed),
				Fixed = [_| _],
				covering(BoundingBox, max_precision(3), Fixed, [])
			)
		).

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

	test(geohash_neighbors_bits_invalid, all(\+ geohash::neighbors_bits(Hash, Bits, _))) :-
		member(Hash-Bits, [-1-25, 33554432-25, 1.0-25, invalid-25, 0-0, 0-(-1), 0-1.5, 0-invalid]).

	test(geohash_neighbors_bits_one, deterministic(Neighbors == [east-1, west-1])) :-
		neighbors_bits(0, 1, Neighbors).

	test(geohash_polyline_zero_buffer, deterministic(Cover == ['ezs42'])) :-
		Polyline = [geographic(42.60, -5.61), geographic(42.61, -5.60)],
		polyline_covering(Polyline, precision(5), Cover, []),
		polyline_covering(Polyline, precision(5), Cover, [buffer(0)]),
		polyline_covering(Polyline, precision(5), Cover, [buffer(0.0)]).

	test(geohash_polyline_positive_buffer, deterministic) :-
		polyline_covering([geographic(42.60, -5.61), geographic(42.61, -5.60)], precision(5), Cover, [buffer(0.1)]),
		memberchk('ezs42', Cover).

	test(geohash_dateline_point_cover, deterministic) :-
		encode(geographic(0.0, 180.0), 3, Hash),
		covering(bbox(geographic(0.0, 180.0), geographic(0.0, 180.0)), 3, [Hash]),
		covering(bbox(geographic(0.0, 180.0), geographic(0.0, 180.0)), max_precision(3), [Hash], []).

	test(geohash_dateline_nearby, deterministic) :-
		encode(geographic(0.0, 179.9999999999995), 12, East),
		decode(East, geographic(_, EastLongitude)),
		EastLongitude > 179.0,
		encode(geographic(0.0, -179.9999999999995), 12, West),
		decode(West, geographic(_, WestLongitude)),
		WestLongitude < -179.0,
		encode(geographic(0.0, 179.9999999999995), 5, PackedHash),
		geohash_to_int(PackedHash, Integer),
		encode_int(geographic(0.0, 179.9999999999995), 5, Integer),
		encode_bits(geographic(0.0, 179.9999999999995), 25, Integer).

	test(geohash_dateline_aligned_interval, deterministic) :-
		covering(bbox(geographic(0.0, 179.0), geographic(0.0, 180.0)), 3, [Hash]),
		decode(Hash, geographic(_, Longitude)),
		Longitude > 179.0,
		adjacent(Hash, east, East),
		adjacent(East, west, Hash).

	test(geohash_spatial_precision_limit, deterministic) :-
		encode(geographic(89.999, 179.999), 12, Hash),
		bounding_box(Hash, bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude))),
		MinLatitude < MaxLatitude,
		MinLongitude < MaxLongitude,
		adjacent(Hash, east, East),
		adjacent(East, west, Hash),
		assertion(\+ geohash::encode(geographic(0.0, 0.0), 13, _)),
		assertion(\+ geohash::precision(13, _, _)),
		assertion(\+ geohash::cell_dimensions(13, _, _)),
		assertion(\+ geohash::covering(bbox(geographic(0.0, 0.0), geographic(0.0, 0.0)), 13, _)).

	test(geohash_precision_polar_regression, deterministic) :-
		valid_geohash('zzzzzzzzzzzzzzzzzzzzzzzz'),
		parent('zzzzzzzzzzzzzzzzzzzzzzzz', _),
		assertion(\+ geohash::bounding_box('zzzzzzzzzzzzzzzzzzzzzzzz', _)),
		assertion(\+ geohash::adjacent('zzzzzzzzzzzzzzzzzzzzzzzz', north, _)),
		assertion(\+ geohash::adjacent('zzzzzzzzzzzz', north, _)).

	test(geohash_bit_precision_limit, deterministic) :-
		(	geohash << valid_bit_precision(60) ->
			encode_bits(geographic(42.6, -5.6), 60, Hash),
			decode_bits(Hash, 60, _),
			Limit is 1 << 60,
			Maximum is Limit - 1,
			assertion(\+ geohash::decode_bits(Limit, 60, _)),
			int_to_geohash(Maximum, 12, 'zzzzzzzzzzzz')
		;	assertion(\+ geohash::encode_bits(geographic(42.6, -5.6), 60, _)),
			assertion(\+ geohash::decode_bits(0, 60, _)),
			assertion(\+ geohash::int_to_geohash(0, 12, _))
		),
		assertion(\+ geohash::encode_bits(geographic(0.0, 0.0), 61, _)),
		assertion(\+ geohash::decode_bits(0, 61, _)),
		assertion(\+ geohash::int_to_geohash(0, 13, _)).

	test(geohash_compress_normalization, deterministic) :-
		forall(
			member(Input-Expected, [
				[]-[],
				[s00, s0, s, u, s0, u]-[s, u],
				[u0, s00, s01, s00]-[s00, s01, u0]
			]),
			(	compress(Input, Expected),
				compress(Expected, Expected),
				reverse(Input, Reversed),
				compress(Reversed, Expected)
			)
		).

	test(geohash_compress_incomplete, deterministic(Compressed == Incomplete)) :-
		children(s00, [_| Incomplete]),
		compress(Incomplete, Compressed).

	test(geohash_compress_nested, deterministic(Compressed == [s00])) :-
		children(s00, Children),
		findall(
			Leaf,
			(	member(Child, Children),
				children(Child, Leaves),
				member(Leaf, Leaves)
			),
			Grandchildren
		),
		compress(Grandchildren, Compressed),
		compress(Compressed, Compressed),
		reverse(Grandchildren, Reversed),
		compress(Reversed, Compressed).

	test(geohash_integer_representation_matrix, deterministic) :-
		forall(
			member(Hash, ['0', '000', 'ezs42', 'u4pruydqqvj', 'zzzzzzzzzzzz']),
			(	atom_length(Hash, Precision),
				Bits is Precision * 5,
				(	geohash << valid_bit_precision(Bits) ->
					geohash_to_int(Hash, Integer),
					int_to_geohash(Integer, Precision, Hash),
					decode(Hash, Coordinate),
					decode_int(Integer, Precision, Coordinate),
					decode_bits(Integer, Bits, Coordinate),
					encode_int(Coordinate, Precision, Integer),
					encode_bits(Coordinate, Bits, Integer),
					bounding_box(Hash, Box),
					bounding_box_int(Integer, Precision, Box),
					bounding_box_bits(Integer, Bits, Box)
				;	assertion(\+ geohash::geohash_to_int(Hash, _)),
					assertion(\+ geohash::int_to_geohash(0, Precision, _)),
					assertion(\+ geohash::decode_int(0, Precision, _)),
					assertion(\+ geohash::encode_int(geographic(0.0, 0.0), Precision, _)),
					assertion(\+ geohash::bounding_box_int(0, Precision, _))
				)
			)
		).

	test(geohash_invalid_decode_and_conversion, deterministic) :-
		forall(
			member(Hash, ['', 'abc', 'EZ', invalid, '0000000000000']),
			(	assertion(\+ geohash::decode(Hash, _)),
				assertion(\+ geohash::geohash_to_int(Hash, _))
			)
		).

	test(geohash_grid_adjacency_matrix, deterministic) :-
		forall(
			(	member(Hash, ['0', 'z', 's', 'ezs42', '800', 'xbp']),
				member(Direction, [north, north_east, east, south_east, south, south_west, west, north_west])
			),
			(	bounding_box(Hash, Box),
				atom_length(Hash, Precision),
				(	reference_neighbor(Box, Direction, Coordinate) ->
					encode(Coordinate, Precision, Expected),
					adjacent(Hash, Direction, Expected),
					geohash_to_int(Hash, Integer),
					geohash_to_int(Expected, ExpectedInteger),
					adjacent_int(Integer, Precision, Direction, ExpectedInteger)
				;	assertion(\+ geohash::adjacent(Hash, Direction, _))
				)
			)
		).

	test(geohash_grid_bit_adjacency_matrix, deterministic) :-
		forall(
			(	member(Bits, [1, 2, 3, 7, 25, 60]),
				geohash << valid_bit_precision(Bits),
				Limit is 1 << Bits,
				Maximum is Limit - 1,
				Middle is Limit // 2,
				member(Hash, [0, Middle, Maximum]),
				member(Direction, [north, north_east, east, south_east, south, south_west, west, north_west])
			),
			(	bounding_box_bits(Hash, Bits, Box),
				(	reference_neighbor(Box, Direction, Coordinate) ->
					encode_bits(Coordinate, Bits, Expected),
					adjacent_bits(Hash, Bits, Direction, Expected)
				;	assertion(\+ geohash::adjacent_bits(Hash, Bits, Direction, _))
				)
			)
		).

	test(geohash_neighbors_invalid_spatial_precision, fail) :-
		neighbors('0000000000000', _).

	test(geohash_fixed_grid_order, deterministic(Cover == Expected)) :-
		findall(
			Hash,
			(	member(Latitude, [-67.5, -22.5, 22.5, 67.5]),
				member(Longitude, [-157.5, -112.5, -67.5, -22.5, 22.5, 67.5, 112.5, 157.5]),
				encode(geographic(Latitude, Longitude), 1, Hash)
			),
			Expected
		),
		covering(bbox(geographic(-90.0, -180.0), geographic(90.0, 180.0)), 1, Cover).

	test(geohash_fixed_grid_crossing_unique, deterministic(Length == 32)) :-
		covering(bbox(geographic(-90.0, 10.0), geographic(90.0, 9.0)), 1, Cover),
		sort(Cover, Unique),
		length(Cover, Length),
		length(Unique, Length),
		covering(bbox(geographic(-90.0, -180.0), geographic(90.0, 180.0)), 1, World),
		sort(World, Unique).

	test(geohash_fixed_grid_maximum_precision_point, deterministic) :-
		Coordinate = geographic(42.6, -5.6),
		encode(Coordinate, 12, Hash),
		covering(bbox(Coordinate, Coordinate), 12, [Hash]).

	test(geohash_geometry_cover_reference_matrix, deterministic) :-
		forall(
			member(Geometry, [
				polygon([geographic(42.58, -5.63), geographic(42.63, -5.63), geographic(42.63, -5.58)]),
				polygon([geographic(42.58, -5.63), geographic(42.63, -5.63), geographic(42.63, -5.61), geographic(42.60, -5.61), geographic(42.60, -5.58), geographic(42.58, -5.58)]),
				polyline([geographic(42.58, -5.63), geographic(42.63, -5.58)], 0),
				polyline([geographic(42.60, -5.61), geographic(42.61, -5.60)], 0.1),
				polyline([geographic(0.0, 0.0), geographic(0.0, 0.0)], 0)
			]),
			check_geometry_cover(Geometry, 5)
		).

	test(geohash_bounded_integer_capacity, deterministic) :-
		geohash << integer_capacity(4, 31, 1),
		geohash << integer_capacity(1, 2, 1),
		assertion(\+ (geohash << integer_capacity(5, 31, 1))),
		assertion(\+ (geohash << integer_capacity(1, 1, 1))).

	test(geohash_covering_reserved_buffer, error(domain_error(option, buffer(0)))) :-
		covering(bbox(geographic(0.0, 0.0), geographic(1.0, 1.0)), precision(3), _, [buffer(0)]).

	test(geohash_covering_variable_options, error(instantiation_error)) :-
		covering(bbox(geographic(0.0, 0.0), geographic(1.0, 1.0)), precision(3), _, _).

	test(geohash_covering_variable_option, error(instantiation_error)) :-
		covering(bbox(geographic(0.0, 0.0), geographic(1.0, 1.0)), precision(3), _, [_]).

	test(geohash_covering_nonlist_options, error(type_error(list, invalid))) :-
		covering(bbox(geographic(0.0, 0.0), geographic(1.0, 1.0)), precision(3), _, invalid).

	test(geohash_covering_noncompound_option, error(type_error(compound, invalid))) :-
		covering(bbox(geographic(0.0, 0.0), geographic(1.0, 1.0)), precision(3), _, [invalid]).

	test(geohash_covering_invalid_option, error(domain_error(option, compact(invalid)))) :-
		covering(bbox(geographic(0.0, 0.0), geographic(1.0, 1.0)), precision(3), _, [compact(invalid)]).

	test(geohash_covering_repeated_options, deterministic) :-
		bounding_box(s0, Box),
		covering(Box, 3, Expected),
		covering(Box, precision(3), Expected, [compact(false), compact(true)]),
		polyline_covering([geographic(42.60, -5.61), geographic(42.61, -5.60)], precision(5), ['ezs42'], [buffer(0), buffer(10)]).

	test(geohash_geometry_later_segment, deterministic) :-
		check_geometry_cover(polyline([geographic(42.58, -5.63), geographic(42.58, -5.61), geographic(42.63, -5.58)], 0), 5).

	test(geohash_geometry_concave_containment, deterministic) :-
		bounding_box(s00, bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude))),
		MiddleLatitude is (MinLatitude + MaxLatitude) / 2.0,
		LeftLongitude is MinLongitude + (MaxLongitude - MinLongitude) / 8.0,
		RightLongitude is MaxLongitude - (MaxLongitude - MinLongitude) / 8.0,
		Polygon = [geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MinLongitude), geographic(MaxLatitude, LeftLongitude), geographic(MiddleLatitude, LeftLongitude), geographic(MiddleLatitude, RightLongitude), geographic(MaxLatitude, RightLongitude), geographic(MaxLatitude, MaxLongitude), geographic(MinLatitude, MaxLongitude)],
		check_geometry_cover(polygon(Polygon), 4),
		polygon_covering(Polygon, max_precision(4), Cover, []),
		assertion(\+ list::member(s00, Cover)).

	test(geohash_polygon_reserved_buffer, error(domain_error(option, buffer(0)))) :-
		polygon_covering([geographic(0.0, 0.0), geographic(1.0, 0.0), geographic(1.0, 1.0)], precision(3), _, [buffer(0)]).

	test(geohash_polyline_negative_buffer, error(domain_error(option, buffer(-1)))) :-
		polyline_covering([geographic(0.0, 0.0), geographic(1.0, 1.0)], precision(3), _, [buffer(-1)]).

	test(geohash_geometry_minimum_precision, deterministic(Cover == Children)) :-
		bounding_box('ezs42', Box),
		bounding_box_polygon(Box, Polygon),
		children('ezs42', Children),
		polygon_covering(Polygon, max_precision(7), Cover, [min_precision(6), compact(false)]).

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

	reference_neighbor(bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude)), Direction, geographic(Latitude, Longitude)) :-
		reference_delta(Direction, LatitudeDelta, LongitudeDelta),
		Latitude is (MinLatitude + MaxLatitude) / 2.0 + LatitudeDelta * (MaxLatitude - MinLatitude),
		Latitude >= -90.0,
		Latitude =< 90.0,
		Longitude0 is (MinLongitude + MaxLongitude) / 2.0 + LongitudeDelta * (MaxLongitude - MinLongitude),
		(	Longitude0 >= 180.0 ->
			Longitude is Longitude0 - 360.0
		;	Longitude0 < -180.0 ->
			Longitude is Longitude0 + 360.0
		;	Longitude = Longitude0
		).

	reference_delta(north, 1, 0).
	reference_delta(north_east, 1, 1).
	reference_delta(east, 0, 1).
	reference_delta(south_east, -1, 1).
	reference_delta(south, -1, 0).
	reference_delta(south_west, -1, -1).
	reference_delta(west, 0, -1).
	reference_delta(north_west, 1, -1).

	check_geometry_cover(Geometry, Precision) :-
		reference_geometry_box(Geometry, Box),
		covering(Box, Precision, Candidates),
		findall(
			Hash,
			(	member(Hash, Candidates),
				bounding_box(Hash, Cell),
				reference_geometry_intersects(Geometry, Cell)
			),
			Expected
		),
		geometry_cover_call(Geometry, precision(Precision), [], Expected),
		geometry_cover_call(Geometry, max_precision(Precision), [], Adaptive),
		expand_cover_leaves(Adaptive, Precision, Leaves),
		sort(Expected, Sorted),
		sort(Leaves, Sorted),
		compress(Expected, Compressed),
		geometry_cover_call(Geometry, precision(Precision), [compact(true)], Compressed),
		!.

	reference_geometry_box(polygon(Polygon), Box) :-
		geospatial::polygon_bounding_box(Polygon, Box).
	reference_geometry_box(polyline(Polyline, Buffer), Box) :-
		geospatial::bbox_from_coordinates(Polyline, RawBox),
		(	Buffer =:= 0 ->
			Box = RawBox
		;	geospatial::bbox_expand(RawBox, Buffer, Box)
		).

	reference_geometry_intersects(polygon(Polygon), Box) :-
		geospatial::bbox_intersects_polygon(Box, Polygon).
	reference_geometry_intersects(polyline(Polyline, Buffer), Box) :-
		(	Buffer =:= 0 ->
			Expanded = Box
		;	geospatial::bbox_expand(Box, Buffer, Expanded)
		),
		geospatial::bbox_intersects_polyline(Expanded, Polyline).

	geometry_cover_call(polygon(Polygon), Spec, Options, Cover) :-
		polygon_covering(Polygon, Spec, Cover, Options).
	geometry_cover_call(polyline(Polyline, Buffer), Spec, Options, Cover) :-
		polyline_covering(Polyline, Spec, Cover, [buffer(Buffer)| Options]).

	expand_cover_leaves([], _Precision, []).
	expand_cover_leaves([Hash| Hashes], Precision, Leaves) :-
		atom_length(Hash, Length),
		(	Length < Precision ->
			children(Hash, Children),
			expand_cover_leaves(Children, Precision, Head)
		;	Head = [Hash]
		),
		expand_cover_leaves(Hashes, Precision, Tail),
		list::append(Head, Tail, Leaves).

:- end_object.
