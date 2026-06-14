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


:- object(geohash,
	implements(geohash_protocol),
	imports(options)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2026-05-21,
		comment is 'Geohash encoder, decoder, adjacency, covering, hierarchy, integer conversion, bit-precision, and geometry covering predicates for ``geographic(Latitude,Longitude)`` coordinates.',
		remarks is [
			'Alphabet' - 'Uses the standard geohash base-32 alphabet ``0123456789bcdefghjkmnpqrstuvwxyz``.',
			'Encoding canonicalization' - 'Encoding canonicalizes longitudes to the ``[-180.0,180.0[`` range, treating ``180.0`` as ``-180.0`` so equivalent dateline coordinates map to the same geohash.',
			'Bounding boxes' - 'Decoded cells are represented using ``bbox(geographic(MinLatitude,MinLongitude),geographic(MaxLatitude,MaxLongitude))``.',
			'Coverage' - 'Bounding-box coverage accepts antimeridian-crossing boxes where the minimum longitude is greater than the maximum longitude.',
			'Adaptive covering' - 'Adaptive covers use ``max_precision/1`` together with ``compact/1`` and ``min_precision/1`` options.',
			'Integer representation' - 'Integer geohashes are 5-bit packed base-32 values with an explicit precision argument preserving leading zero groups.',
			'Geometry covering' - 'Polygon and polyline covering predicates rely on generic ``geospatial`` geometry predicates and do not support antimeridian-crossing input geometries.'
		],
		see_also is [geohash_protocol, geospatial]
	]).

	:- uses(geospatial, [
		bbox_contains/2, bbox_expand/3, bbox_from_coordinates/2,
		bbox_intersects_polygon/2, bbox_intersects_polyline/2, bbox_overlaps/2,
		normalize_coordinate/2, polygon_bounding_box/2, valid_coordinate/1
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2, reverse/2
	]).

	:- uses(type, [
		valid/2
	]).

	valid_geohash(Geohash) :-
		atom(Geohash),
		atom_codes(Geohash, Codes),
		Codes \== [],
		valid_geohash_codes(Codes).

	encode(Coordinate, Precision, Geohash) :-
		valid_precision(Precision),
		valid_coordinate(Coordinate),
		canonical_encode_coordinate(Coordinate, geographic(Latitude, Longitude)),
		encode_hash(Precision, Latitude, Longitude, -180.0, 180.0, -90.0, 90.0, longitude, 0, 0, Codes),
		atom_codes(Geohash, Codes).

	decode(Geohash, geographic(Latitude, Longitude)) :-
		decode_intervals(Geohash, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax),
		Latitude is (LatitudeMin + LatitudeMax) / 2.0,
		Longitude is (LongitudeMin + LongitudeMax) / 2.0.

	bounding_box(Geohash, bbox(geographic(LatitudeMin, LongitudeMin), geographic(LatitudeMax, LongitudeMax))) :-
		decode_intervals(Geohash, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax).

	precision(Precision, LatitudeError, LongitudeError) :-
		valid_precision(Precision),
		bit_counts(Precision, LatitudeBits, LongitudeBits),
		power_of_two(LatitudeBits, LatitudeDivisor),
		power_of_two(LongitudeBits, LongitudeDivisor),
		LatitudeError is 90.0 / LatitudeDivisor,
		LongitudeError is 180.0 / LongitudeDivisor.

	cell_dimensions(Precision, LatitudeSpan, LongitudeSpan) :-
		valid_precision(Precision),
		bit_counts(Precision, LatitudeBits, LongitudeBits),
		power_of_two(LatitudeBits, LatitudeDivisor),
		power_of_two(LongitudeBits, LongitudeDivisor),
		LatitudeSpan is 180.0 / LatitudeDivisor,
		LongitudeSpan is 360.0 / LongitudeDivisor.

	adjacent(Geohash, Direction, AdjacentGeohash) :-
		direction_delta(Direction, LatitudeDelta, LongitudeDelta),
		decode_intervals(Geohash, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax),
		atom_length(Geohash, Precision),
		LatitudeSpan is LatitudeMax - LatitudeMin,
		LongitudeSpan is LongitudeMax - LongitudeMin,
		Latitude is (LatitudeMin + LatitudeMax) / 2.0 + LatitudeDelta * LatitudeSpan,
		Latitude >= -90.0,
		Latitude =< 90.0,
		Longitude0 is (LongitudeMin + LongitudeMax) / 2.0 + LongitudeDelta * LongitudeSpan,
		normalize_coordinate(geographic(Latitude, Longitude0), geographic(NormalizedLatitude, NormalizedLongitude)),
		encode(geographic(NormalizedLatitude, NormalizedLongitude), Precision, AdjacentGeohash).

	neighbors(Geohash, Neighbors) :-
		valid_geohash(Geohash),
		neighbors([north, north_east, east, south_east, south, south_west, west, north_west], Geohash, Neighbors).

	covering(BoundingBox, Precision, Geohashes) :-
		fixed_covering(BoundingBox, Precision, Geohashes).

	covering(BoundingBox, CoverSpec, Geohashes, UserOptions) :-
		reject_buffer_option(UserOptions),
		valid_bounding_box(BoundingBox),
		valid_cover_spec(CoverSpec, LimitPrecision),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(compact(Compact), Options),
		^^option(min_precision(MinPrecision), Options),
		MinPrecision =< LimitPrecision,
		covering_spec_geohashes(CoverSpec, BoundingBox, MinPrecision, Geohashes0),
		(	Compact == true ->
			compress_covering_geohashes(Geohashes0, MinPrecision, Geohashes)
		;	Geohashes = Geohashes0
		).

	compress(Geohashes, CompressedGeohashes) :-
		compress_covering_geohashes(Geohashes, 1, CompressedGeohashes).

	parent(Geohash, Parent) :-
		valid_geohash(Geohash),
		atom_length(Geohash, Length),
		Length > 1,
		ParentLength is Length - 1,
		sub_atom(Geohash, 0, ParentLength, _, Parent).

	children(Geohash, Children) :-
		valid_geohash(Geohash),
		findall(Child, geohash_child(Geohash, Child), Children).

	common_prefix(Geohash1, Geohash2, Prefix) :-
		valid_geohash(Geohash1),
		valid_geohash(Geohash2),
		atom_codes(Geohash1, Codes1),
		atom_codes(Geohash2, Codes2),
		common_prefix_codes(Codes1, Codes2, PrefixCodes),
		atom_codes(Prefix, PrefixCodes).

	encode_int(Coordinate, Precision, HashInteger) :-
		valid_precision(Precision),
		valid_coordinate(Coordinate),
		canonical_encode_coordinate(Coordinate, geographic(Latitude, Longitude)),
		encode_hash(Precision, Latitude, Longitude, -180.0, 180.0, -90.0, 90.0, longitude, 0, 0, Codes),
		codes_integer(Codes, HashInteger).

	decode_int(HashInteger, Precision, geographic(Latitude, Longitude)) :-
		integer_hash_codes(HashInteger, Precision, Codes),
		decode_codes(Codes, -180.0, 180.0, -90.0, 90.0, longitude, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, _),
		Latitude is (LatitudeMin + LatitudeMax) / 2.0,
		Longitude is (LongitudeMin + LongitudeMax) / 2.0.

	bounding_box_int(HashInteger, Precision, bbox(geographic(LatitudeMin, LongitudeMin), geographic(LatitudeMax, LongitudeMax))) :-
		integer_hash_codes(HashInteger, Precision, Codes),
		decode_codes(Codes, -180.0, 180.0, -90.0, 90.0, longitude, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, _).

	adjacent_int(HashInteger, Precision, Direction, AdjacentHashInteger) :-
		integer_hash_codes(HashInteger, Precision, Codes),
		atom_codes(Geohash, Codes),
		adjacent(Geohash, Direction, AdjacentGeohash),
		atom_codes(AdjacentGeohash, AdjacentCodes),
		codes_integer(AdjacentCodes, AdjacentHashInteger).

	neighbors_int(HashInteger, Precision, Neighbors) :-
		integer_hash_codes(HashInteger, Precision, Codes),
		atom_codes(Geohash, Codes),
		neighbors(Geohash, GeohashNeighbors),
		neighbor_hash_integers(GeohashNeighbors, Neighbors).

	geohash_to_int(Geohash, HashInteger) :-
		valid_geohash(Geohash),
		atom_codes(Geohash, Codes),
		codes_integer(Codes, HashInteger).

	int_to_geohash(HashInteger, Precision, Geohash) :-
		integer_hash_codes(HashInteger, Precision, Codes),
		atom_codes(Geohash, Codes).

	encode_bits(Coordinate, Bits, HashInteger) :-
		valid_bit_precision(Bits),
		valid_coordinate(Coordinate),
		canonical_encode_coordinate(Coordinate, geographic(Latitude, Longitude)),
		encode_bits_hash(Bits, Latitude, Longitude, -180.0, 180.0, -90.0, 90.0, longitude, 0, HashInteger).

	decode_bits(HashInteger, Bits, geographic(Latitude, Longitude)) :-
		decode_bit_intervals(HashInteger, Bits, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax),
		Latitude is (LatitudeMin + LatitudeMax) / 2.0,
		Longitude is (LongitudeMin + LongitudeMax) / 2.0.

	bounding_box_bits(HashInteger, Bits, bbox(geographic(LatitudeMin, LongitudeMin), geographic(LatitudeMax, LongitudeMax))) :-
		decode_bit_intervals(HashInteger, Bits, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax).

	adjacent_bits(HashInteger, Bits, Direction, AdjacentHashInteger) :-
		direction_delta(Direction, LatitudeDelta, LongitudeDelta),
		decode_bit_intervals(HashInteger, Bits, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax),
		LatitudeSpan is LatitudeMax - LatitudeMin,
		LongitudeSpan is LongitudeMax - LongitudeMin,
		Latitude is (LatitudeMin + LatitudeMax) / 2.0 + LatitudeDelta * LatitudeSpan,
		Latitude >= -90.0,
		Latitude =< 90.0,
		Longitude0 is (LongitudeMin + LongitudeMax) / 2.0 + LongitudeDelta * LongitudeSpan,
		normalize_coordinate(geographic(Latitude, Longitude0), geographic(NormalizedLatitude, NormalizedLongitude)),
		encode_bits(geographic(NormalizedLatitude, NormalizedLongitude), Bits, AdjacentHashInteger).

	neighbors_bits(HashInteger, Bits, Neighbors) :-
		neighbors_bits([north, north_east, east, south_east, south, south_west, west, north_west], HashInteger, Bits, Neighbors).

	expand(Geohash, ExpandedGeohashes) :-
		neighbors(Geohash, Neighbors),
		neighbor_geohashes(Neighbors, NeighborGeohashes),
		ExpandedGeohashes = [Geohash| NeighborGeohashes].

	expand_int(HashInteger, Precision, ExpandedHashes) :-
		neighbors_int(HashInteger, Precision, Neighbors),
		neighbor_hash_values(Neighbors, NeighborHashes),
		ExpandedHashes = [HashInteger| NeighborHashes].

	polygon_covering(Polygon, CoverSpec, Geohashes, UserOptions) :-
		reject_buffer_option(UserOptions),
		polygon_bounding_box(Polygon, BoundingBox),
		valid_cover_spec(CoverSpec, LimitPrecision),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(compact(Compact), Options),
		^^option(min_precision(MinPrecision), Options),
		MinPrecision =< LimitPrecision,
		geometry_cover_candidates(CoverSpec, BoundingBox, Candidates),
		polygon_cover_candidates(Candidates, Polygon, CoverSpec, Compact, MinPrecision, Geohashes).

	polyline_covering(Polyline, CoverSpec, Geohashes, UserOptions) :-
		valid_polyline(Polyline),
		valid_cover_spec(CoverSpec, LimitPrecision),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(compact(Compact), Options),
		^^option(min_precision(MinPrecision), Options),
		^^option(buffer(Buffer), Options),
		MinPrecision =< LimitPrecision,
		polyline_covering_bounding_box(Polyline, Buffer, BoundingBox),
		geometry_cover_candidates(CoverSpec, BoundingBox, Candidates),
		polyline_cover_candidates(Candidates, Polyline, Buffer, CoverSpec, Compact, MinPrecision, Geohashes).

	fixed_covering(BoundingBox, Precision, Geohashes) :-
		valid_precision(Precision),
		valid_bounding_box(BoundingBox),
		BoundingBox = bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude)),
		cell_dimensions(Precision, LatitudeSpan, LongitudeSpan),
		bit_counts(Precision, LatitudeBits, LongitudeBits),
		power_of_two(LatitudeBits, LatitudeCells),
		power_of_two(LongitudeBits, LongitudeCells),
		index_bounds(MinLatitude, MaxLatitude, -90.0, LatitudeSpan, LatitudeCells, StartLatitudeIndex, EndLatitudeIndex),
		longitude_segments(MinLongitude, MaxLongitude, LongitudeSpan, LongitudeCells, Segments),
		covering(StartLatitudeIndex, EndLatitudeIndex, Segments, LatitudeSpan, LongitudeSpan, Precision, [], ReversedGeohashes),
		reverse(ReversedGeohashes, Geohashes).

	covering_spec_geohashes(precision(Precision), BoundingBox, _MinPrecision, Geohashes) :-
		fixed_covering(BoundingBox, Precision, Geohashes).
	covering_spec_geohashes(max_precision(MaxPrecision), BoundingBox, MinPrecision, Geohashes) :-
		adaptive_covering(BoundingBox, MaxPrecision, MinPrecision, Geohashes).

	geometry_cover_candidates(precision(Precision), BoundingBox, Geohashes) :-
		fixed_covering(BoundingBox, Precision, Geohashes).
	geometry_cover_candidates(max_precision(MaxPrecision), BoundingBox, Geohashes) :-
		fixed_covering(BoundingBox, MaxPrecision, Geohashes).

	adaptive_covering(BoundingBox, MaxPrecision, MinPrecision, Geohashes) :-
		fixed_covering(BoundingBox, MinPrecision, Seeds),
		adaptive_covering(Seeds, BoundingBox, MaxPrecision, [], ReversedGeohashes),
		reverse(ReversedGeohashes, Geohashes).

	adaptive_covering([], _BoundingBox, _MaxPrecision, Accumulator, Accumulator).
	adaptive_covering([Geohash| Geohashes], BoundingBox, MaxPrecision, Accumulator0, Accumulator) :-
		adaptive_cover(Geohash, BoundingBox, MaxPrecision, Accumulator0, Accumulator1),
		adaptive_covering(Geohashes, BoundingBox, MaxPrecision, Accumulator1, Accumulator).

	adaptive_cover(Geohash, BoundingBox, MaxPrecision, Accumulator0, Accumulator) :-
		bounding_box(Geohash, GeohashBoundingBox),
		(	bbox_overlaps(BoundingBox, GeohashBoundingBox) ->
			atom_length(Geohash, Precision),
			(	Precision >= MaxPrecision ->
				Accumulator = [Geohash| Accumulator0]
			;	bbox_contains(BoundingBox, GeohashBoundingBox) ->
				Accumulator = [Geohash| Accumulator0]
			;	children(Geohash, Children),
				adaptive_covering(Children, BoundingBox, MaxPrecision, Accumulator0, Accumulator)
			)
		;	Accumulator = Accumulator0
		).

	compress_covering_geohashes(Geohashes, MinPrecision, CompressedGeohashes) :-
		valid_precision(MinPrecision),
		valid_geohashes(Geohashes),
		normalize_cover_geohashes(Geohashes, NormalizedGeohashes),
		compress_cover_geohashes(NormalizedGeohashes, MinPrecision, CompressedGeohashes).

	compress_cover_geohashes(Geohashes, MinPrecision, CompressedGeohashes) :-
		compress_cover_geohashes_pass(Geohashes, MinPrecision, PassGeohashes, Changed),
		(	Changed == true ->
			compress_cover_geohashes(PassGeohashes, MinPrecision, CompressedGeohashes)
		;	CompressedGeohashes = Geohashes
		).

	compress_cover_geohashes_pass(Geohashes, MinPrecision, CompressedGeohashes, Changed) :-
		findall(Parent,
			(	member(Geohash, Geohashes),
				atom_length(Geohash, Precision),
				Precision > MinPrecision,
				parent(Geohash, Parent),
				all_children_present(Parent, Geohashes)
			),
			Parents0
		),
		sort(Parents0, Parents),
		(	Parents == [] ->
			CompressedGeohashes = Geohashes,
			Changed = false
		;	collapse_parent_children(Geohashes, Parents, CompressedGeohashes),
			Changed = true
		).

	collapse_parent_children(Geohashes, Parents, CompressedGeohashes) :-
		remove_collapsed_children(Geohashes, Parents, RemainingGeohashes),
		append(Parents, RemainingGeohashes, CombinedGeohashes),
		normalize_cover_geohashes(CombinedGeohashes, CompressedGeohashes).

	remove_collapsed_children([], _Parents, []).
	remove_collapsed_children([Geohash| Geohashes], Parents, RemainingGeohashes) :-
		(	direct_child_of_any_parent(Geohash, Parents) ->
			remove_collapsed_children(Geohashes, Parents, RemainingGeohashes)
		;	RemainingGeohashes = [Geohash| Tail],
			remove_collapsed_children(Geohashes, Parents, Tail)
		).

	direct_child_of_any_parent(Geohash, Parents) :-
		atom_length(Geohash, Precision),
		Precision > 1,
		parent(Geohash, Parent),
		memberchk(Parent, Parents).

	all_children_present(Parent, Geohashes) :-
		children(Parent, Children),
		all_memberchk(Children, Geohashes).

	all_memberchk([], _Terms).
	all_memberchk([Term| Terms], List) :-
		memberchk(Term, List),
		all_memberchk(Terms, List).

	normalize_cover_geohashes(Geohashes, NormalizedGeohashes) :-
		sort(Geohashes, SortedGeohashes),
		remove_covered_geohashes(SortedGeohashes, [], ReversedNormalizedGeohashes),
		reverse(ReversedNormalizedGeohashes, NormalizedGeohashes).

	remove_covered_geohashes([], Accumulator, Accumulator).
	remove_covered_geohashes([Geohash| Geohashes], Accumulator0, Accumulator) :-
		(	covered_by_existing_prefix(Geohash, Accumulator0) ->
			Accumulator1 = Accumulator0
		;	Accumulator1 = [Geohash| Accumulator0]
		),
		remove_covered_geohashes(Geohashes, Accumulator1, Accumulator).

	covered_by_existing_prefix(Geohash, Prefixes) :-
		member(Prefix, Prefixes),
		proper_geohash_prefix(Prefix, Geohash),
		!.

	proper_geohash_prefix(Prefix, Geohash) :-
		atom_length(Prefix, PrefixLength),
		atom_length(Geohash, GeohashLength),
		PrefixLength < GeohashLength,
		sub_atom(Geohash, 0, PrefixLength, _, Prefix).

	geohash_child(Geohash, Child) :-
		code_index(Code, _),
		char_code(Suffix, Code),
		atom_concat(Geohash, Suffix, Child).

	valid_geohashes([]).
	valid_geohashes([Geohash| Geohashes]) :-
		valid_geohash(Geohash),
		valid_geohashes(Geohashes).

	common_prefix_codes([Code| Codes1], [Code| Codes2], [Code| PrefixCodes]) :-
		!,
		common_prefix_codes(Codes1, Codes2, PrefixCodes).
	common_prefix_codes(_, _, []).

	valid_cover_spec(precision(Precision), Precision) :-
		valid_precision(Precision).
	valid_cover_spec(max_precision(Precision), Precision) :-
		valid_precision(Precision).

	default_option(compact(false)).
	default_option(min_precision(1)).
	default_option(buffer(0.0)).

	valid_option(compact(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(min_precision(Precision)) :-
		valid_precision(Precision).
	valid_option(buffer(Buffer)) :-
		valid(non_negative_number, Buffer).

	reject_buffer_option(Options) :-
		var(Options),
		!.
	reject_buffer_option([buffer(Buffer)| _]) :-
		domain_error(option, buffer(Buffer)).
	reject_buffer_option([_| Options]) :-
		reject_buffer_option(Options).
	reject_buffer_option([]).

	neighbor_hash_integers([], []).
	neighbor_hash_integers([Direction-Geohash| GeohashNeighbors], [Direction-HashInteger| IntegerNeighbors]) :-
		atom_codes(Geohash, Codes),
		codes_integer(Codes, HashInteger),
		neighbor_hash_integers(GeohashNeighbors, IntegerNeighbors).

	neighbor_geohashes([], []).
	neighbor_geohashes([_-Geohash| Neighbors], [Geohash| Geohashes]) :-
		neighbor_geohashes(Neighbors, Geohashes).

	neighbor_hash_values([], []).
	neighbor_hash_values([_-HashInteger| Neighbors], [HashInteger| Hashes]) :-
		neighbor_hash_values(Neighbors, Hashes).

	neighbors_bits([], _HashInteger, _Bits, []).
	neighbors_bits([Direction| Directions], HashInteger, Bits, Neighbors) :-
		(	adjacent_bits(HashInteger, Bits, Direction, Neighbor) ->
			Neighbors = [Direction-Neighbor| Neighbors0]
		;	Neighbors = Neighbors0
		),
		neighbors_bits(Directions, HashInteger, Bits, Neighbors0).

	integer_hash_codes(HashInteger, Precision, Codes) :-
		valid_hash_integer(HashInteger, Precision),
		integer_hash_codes_from_integer(Precision, HashInteger, Codes).

	integer_hash_codes_from_integer(0, _HashInteger, []) :-
		!.
	integer_hash_codes_from_integer(Precision, HashInteger, [Code| Codes]) :-
		Precision > 0,
		Shift is (Precision - 1) * 5,
		Index is (HashInteger >> Shift) /\ 31,
		index_code(Index, Code),
		NextPrecision is Precision - 1,
		integer_hash_codes_from_integer(NextPrecision, HashInteger, Codes).

	decode_bit_intervals(HashInteger, Bits, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax) :-
		valid_hash_bits_integer(HashInteger, Bits),
		HighestMask is 1 << (Bits - 1),
		decode_bits_mask(HashInteger, HighestMask, -180.0, 180.0, -90.0, 90.0, longitude, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, _).

	decode_bits_mask(_HashInteger, 0, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis) :-
		!.
	decode_bits_mask(HashInteger, Mask, LongitudeMin0, LongitudeMax0, LatitudeMin0, LatitudeMax0, Axis0, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis) :-
		Bit is HashInteger /\ Mask,
		refine_decode_axis(Axis0, Bit, LongitudeMin0, LongitudeMax0, LatitudeMin0, LatitudeMax0, LongitudeMin1, LongitudeMax1, LatitudeMin1, LatitudeMax1, Axis1),
		NextMask is Mask >> 1,
		decode_bits_mask(HashInteger, NextMask, LongitudeMin1, LongitudeMax1, LatitudeMin1, LatitudeMax1, Axis1, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis).

	encode_bits_hash(0, _Latitude, _Longitude, _LongitudeMin, _LongitudeMax, _LatitudeMin, _LatitudeMax, _Axis, HashInteger, HashInteger) :-
		!.
	encode_bits_hash(Bits, Latitude, Longitude, LongitudeMin0, LongitudeMax0, LatitudeMin0, LatitudeMax0, Axis0, HashInteger0, HashInteger) :-
		refine_encode_axis(Axis0, Latitude, Longitude, LongitudeMin0, LongitudeMax0, LatitudeMin0, LatitudeMax0, Bit, LongitudeMin1, LongitudeMax1, LatitudeMin1, LatitudeMax1, Axis1),
		HashInteger1 is (HashInteger0 << 1) + Bit,
		NextBits is Bits - 1,
		encode_bits_hash(NextBits, Latitude, Longitude, LongitudeMin1, LongitudeMax1, LatitudeMin1, LatitudeMax1, Axis1, HashInteger1, HashInteger).

	valid_hash_integer(HashInteger, Precision) :-
		integer(HashInteger),
		HashInteger >= 0,
		valid_precision(Precision),
		TotalBits is Precision * 5,
		power_of_two(TotalBits, Limit),
		HashInteger < Limit.

	valid_hash_bits_integer(HashInteger, Bits) :-
		integer(HashInteger),
		HashInteger >= 0,
		valid_bit_precision(Bits),
		power_of_two(Bits, Limit),
		HashInteger < Limit.

	codes_integer(Codes, Integer) :-
		codes_integer(Codes, 0, Integer).

	codes_integer([], Integer, Integer).
	codes_integer([Code| Codes], Integer0, Integer) :-
		code_index(Code, Index),
		Integer1 is (Integer0 << 5) + Index,
		codes_integer(Codes, Integer1, Integer).

	polygon_cover_candidates(Candidates, Polygon, CoverSpec, Compact, MinPrecision, Geohashes) :-
		filter_polygon_candidates(Candidates, Polygon, FilteredGeohashes),
		compress_geometry_cover(CoverSpec, Compact, MinPrecision, FilteredGeohashes, Geohashes).

	filter_polygon_candidates([], _Polygon, []).
	filter_polygon_candidates([Geohash| Candidates], Polygon, FilteredGeohashes) :-
		(	polygon_intersects_geohash_cell(Polygon, Geohash) ->
			FilteredGeohashes = [Geohash| Tail]
		;	FilteredGeohashes = Tail
		),
		filter_polygon_candidates(Candidates, Polygon, Tail).

	polygon_intersects_geohash_cell(Polygon, Geohash) :-
		bounding_box(Geohash, BoundingBox),
		bbox_intersects_polygon(BoundingBox, Polygon).

	polyline_covering_bounding_box(Polyline, 0.0, BoundingBox) :-
		bbox_from_coordinates(Polyline, BoundingBox),
		!.
	polyline_covering_bounding_box(Polyline, Buffer, BoundingBox) :-
		bbox_from_coordinates(Polyline, PolylineBoundingBox),
		bbox_expand(PolylineBoundingBox, Buffer, BoundingBox).

	polyline_cover_candidates(Candidates, Polyline, Buffer, CoverSpec, Compact, MinPrecision, Geohashes) :-
		filter_polyline_candidates(Candidates, Polyline, Buffer, FilteredGeohashes),
		compress_geometry_cover(CoverSpec, Compact, MinPrecision, FilteredGeohashes, Geohashes).

	filter_polyline_candidates([], _Polyline, _Buffer, []).
	filter_polyline_candidates([Geohash| Candidates], Polyline, Buffer, FilteredGeohashes) :-
		(	polyline_intersects_geohash_cell(Polyline, Buffer, Geohash) ->
			FilteredGeohashes = [Geohash| Tail]
		;	FilteredGeohashes = Tail
		),
		filter_polyline_candidates(Candidates, Polyline, Buffer, Tail).

	polyline_intersects_geohash_cell(Polyline, 0.0, Geohash) :-
		bounding_box(Geohash, BoundingBox),
		bbox_intersects_polyline(BoundingBox, Polyline).
	polyline_intersects_geohash_cell(Polyline, Buffer, Geohash) :-
		Buffer > 0.0,
		bounding_box(Geohash, BoundingBox),
		bbox_expand(BoundingBox, Buffer, ExpandedBoundingBox),
		bbox_intersects_polyline(ExpandedBoundingBox, Polyline).

	compress_geometry_cover(max_precision(_), _Compact, MinPrecision, Geohashes, CompressedGeohashes) :-
		compress_covering_geohashes(Geohashes, MinPrecision, CompressedGeohashes).
	compress_geometry_cover(precision(_), true, MinPrecision, Geohashes, CompressedGeohashes) :-
		compress_covering_geohashes(Geohashes, MinPrecision, CompressedGeohashes).
	compress_geometry_cover(precision(_), false, _MinPrecision, Geohashes, Geohashes).

	valid_precision(Precision) :-
		integer(Precision),
		Precision > 0.

	valid_bit_precision(Bits) :-
		integer(Bits),
		Bits > 0.

	valid_polyline([Coordinate1, Coordinate2| Coordinates]) :-
		valid_coordinate(Coordinate1),
		valid_coordinate(Coordinate2),
		all_valid_coordinates(Coordinates).

	all_valid_coordinates([]).
	all_valid_coordinates([Coordinate| Coordinates]) :-
		valid_coordinate(Coordinate),
		all_valid_coordinates(Coordinates).

	canonical_encode_coordinate(Coordinate, geographic(Latitude, Longitude)) :-
		normalize_coordinate(Coordinate, geographic(Latitude, Longitude0)),
		canonical_longitude(Longitude0, Longitude).

	canonical_longitude(Longitude0, -180.0) :-
		Delta is abs(Longitude0 - 180.0),
		Delta =< 1.0e-12,
		!.
	canonical_longitude(Longitude, Longitude).

	valid_geohash_codes([]).
	valid_geohash_codes([Code| Codes]) :-
		code_index(Code, _),
		valid_geohash_codes(Codes).

	decode_intervals(Geohash, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax) :-
		valid_geohash(Geohash),
		atom_codes(Geohash, Codes),
		decode_codes(Codes, -180.0, 180.0, -90.0, 90.0, longitude, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, _).

	encode_hash(0, _Latitude, _Longitude, _LongitudeMin, _LongitudeMax, _LatitudeMin, _LatitudeMax, _Axis, 0, _CharValue, []) :-
		!.
	encode_hash(Precision, Latitude, Longitude, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis, 5, CharValue, [Code| Codes]) :-
		!,
		index_code(CharValue, Code),
		NextPrecision is Precision - 1,
		encode_hash(NextPrecision, Latitude, Longitude, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis, 0, 0, Codes).
	encode_hash(Precision, Latitude, Longitude, LongitudeMin0, LongitudeMax0, LatitudeMin0, LatitudeMax0, Axis0, BitCount0, CharValue0, Codes) :-
		refine_encode_axis(Axis0, Latitude, Longitude, LongitudeMin0, LongitudeMax0, LatitudeMin0, LatitudeMax0, Bit, LongitudeMin1, LongitudeMax1, LatitudeMin1, LatitudeMax1, Axis1),
		CharValue1 is (CharValue0 << 1) + Bit,
		BitCount1 is BitCount0 + 1,
		encode_hash(Precision, Latitude, Longitude, LongitudeMin1, LongitudeMax1, LatitudeMin1, LatitudeMax1, Axis1, BitCount1, CharValue1, Codes).

	refine_encode_axis(longitude, _Latitude, Longitude, LongitudeMin0, LongitudeMax0, LatitudeMin, LatitudeMax, Bit, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, latitude) :-
		LongitudeMid is (LongitudeMin0 + LongitudeMax0) / 2.0,
		(	Longitude >= LongitudeMid ->
			Bit = 1,
			LongitudeMin = LongitudeMid,
			LongitudeMax = LongitudeMax0
		;	Bit = 0,
			LongitudeMin = LongitudeMin0,
			LongitudeMax = LongitudeMid
		).
	refine_encode_axis(latitude, Latitude, _Longitude, LongitudeMin, LongitudeMax, LatitudeMin0, LatitudeMax0, Bit, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, longitude) :-
		LatitudeMid is (LatitudeMin0 + LatitudeMax0) / 2.0,
		(	Latitude >= LatitudeMid ->
			Bit = 1,
			LatitudeMin = LatitudeMid,
			LatitudeMax = LatitudeMax0
		;	Bit = 0,
			LatitudeMin = LatitudeMin0,
			LatitudeMax = LatitudeMid
		).

	decode_codes([], LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis).
	decode_codes([Code| Codes], LongitudeMin0, LongitudeMax0, LatitudeMin0, LatitudeMax0, Axis0, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis) :-
		code_index(Code, Index),
		decode_index_bits(Index, 16, LongitudeMin0, LongitudeMax0, LatitudeMin0, LatitudeMax0, Axis0, LongitudeMin1, LongitudeMax1, LatitudeMin1, LatitudeMax1, Axis1),
		decode_codes(Codes, LongitudeMin1, LongitudeMax1, LatitudeMin1, LatitudeMax1, Axis1, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis).

	decode_index_bits(_Index, 0, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis) :-
		!.
	decode_index_bits(Index, Mask, LongitudeMin0, LongitudeMax0, LatitudeMin0, LatitudeMax0, Axis0, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis) :-
		Bit is Index /\ Mask,
		refine_decode_axis(Axis0, Bit, LongitudeMin0, LongitudeMax0, LatitudeMin0, LatitudeMax0, LongitudeMin1, LongitudeMax1, LatitudeMin1, LatitudeMax1, Axis1),
		NextMask is Mask >> 1,
		decode_index_bits(Index, NextMask, LongitudeMin1, LongitudeMax1, LatitudeMin1, LatitudeMax1, Axis1, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, Axis).

	refine_decode_axis(longitude, Bit, LongitudeMin0, LongitudeMax0, LatitudeMin, LatitudeMax, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, latitude) :-
		LongitudeMid is (LongitudeMin0 + LongitudeMax0) / 2.0,
		(	Bit =:= 0 ->
			LongitudeMin = LongitudeMin0,
			LongitudeMax = LongitudeMid
		;	LongitudeMin = LongitudeMid,
			LongitudeMax = LongitudeMax0
		).
	refine_decode_axis(latitude, Bit, LongitudeMin, LongitudeMax, LatitudeMin0, LatitudeMax0, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, longitude) :-
		LatitudeMid is (LatitudeMin0 + LatitudeMax0) / 2.0,
		(	Bit =:= 0 ->
			LatitudeMin = LatitudeMin0,
			LatitudeMax = LatitudeMid
		;	LatitudeMin = LatitudeMid,
			LatitudeMax = LatitudeMax0
		).

	neighbors([], _Geohash, []).
	neighbors([Direction| Directions], Geohash, Neighbors) :-
		(	adjacent(Geohash, Direction, Neighbor) ->
			Neighbors = [Direction-Neighbor| Neighbors0]
		;	Neighbors = Neighbors0
		),
		neighbors(Directions, Geohash, Neighbors0).

	direction_delta(north, 1, 0).
	direction_delta(south, -1, 0).
	direction_delta(east, 0, 1).
	direction_delta(west, 0, -1).
	direction_delta(north_east, 1, 1).
	direction_delta(north_west, 1, -1).
	direction_delta(south_east, -1, 1).
	direction_delta(south_west, -1, -1).

	valid_bounding_box(bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude))) :-
		valid_coordinate(geographic(MinLatitude, MinLongitude)),
		valid_coordinate(geographic(MaxLatitude, MaxLongitude)),
		MinLatitude =< MaxLatitude.

	bit_counts(Precision, LatitudeBits, LongitudeBits) :-
		TotalBits is Precision * 5,
		LatitudeBits is TotalBits // 2,
		LongitudeBits is (TotalBits + 1) // 2.

	power_of_two(Exponent, Power) :-
		power_of_two(Exponent, 1, Power).

	power_of_two(0, Power, Power) :-
		!.
	power_of_two(Exponent, Power0, Power) :-
		NextExponent is Exponent - 1,
		Power1 is Power0 * 2,
		power_of_two(NextExponent, Power1, Power).

	index_bounds(MinValue, MaxValue, Offset, Span, Count, StartIndex, EndIndex) :-
		index_floor(MinValue, Offset, Span, Count, StartIndex),
		index_ceiling(MaxValue, Offset, Span, Count, EndIndex0),
		(	StartIndex =< EndIndex0 ->
			EndIndex = EndIndex0
		;	EndIndex = StartIndex
		).

	index_floor(Value, Offset, Span, Count, Index) :-
		Ratio is (Value - Offset) / Span,
		RawIndex is floor(Ratio),
		clamp_index(RawIndex, Count, Index).

	index_ceiling(Value, Offset, Span, Count, Index) :-
		Ratio is (Value - Offset) / Span,
		RawIndex is ceiling(Ratio) - 1,
		clamp_index(RawIndex, Count, Index).

	clamp_index(Index0, Count, Index) :-
		MaximumIndex is Count - 1,
		(	Index0 < 0 ->
			Index = 0
		;	Index0 > MaximumIndex ->
			Index = MaximumIndex
		;	Index = Index0
		).

	longitude_segments(MinLongitude, MaxLongitude, LongitudeSpan, LongitudeCells, [StartIndex-EndIndex]) :-
		MinLongitude =< MaxLongitude,
		!,
		index_bounds(MinLongitude, MaxLongitude, -180.0, LongitudeSpan, LongitudeCells, StartIndex, EndIndex).
	longitude_segments(MinLongitude, MaxLongitude, LongitudeSpan, LongitudeCells, [StartIndex1-EndIndex1, StartIndex2-EndIndex2]) :-
		index_bounds(MinLongitude, 180.0, -180.0, LongitudeSpan, LongitudeCells, StartIndex1, EndIndex1),
		index_bounds(-180.0, MaxLongitude, -180.0, LongitudeSpan, LongitudeCells, StartIndex2, EndIndex2).

	covering(LatitudeIndex, EndLatitudeIndex, _Segments, _LatitudeSpan, _LongitudeSpan, _Precision, Accumulator, Accumulator) :-
		LatitudeIndex > EndLatitudeIndex,
		!.
	covering(LatitudeIndex, EndLatitudeIndex, Segments, LatitudeSpan, LongitudeSpan, Precision, Accumulator0, Accumulator) :-
		Latitude is -90.0 + (LatitudeIndex + 0.5) * LatitudeSpan,
		covering_segments(Segments, Latitude, LongitudeSpan, Precision, Accumulator0, Accumulator1),
		NextLatitudeIndex is LatitudeIndex + 1,
		covering(NextLatitudeIndex, EndLatitudeIndex, Segments, LatitudeSpan, LongitudeSpan, Precision, Accumulator1, Accumulator).

	covering_segments([], _Latitude, _LongitudeSpan, _Precision, Accumulator, Accumulator).
	covering_segments([StartIndex-EndIndex| Segments], Latitude, LongitudeSpan, Precision, Accumulator0, Accumulator) :-
		covering_longitudes(StartIndex, EndIndex, Latitude, LongitudeSpan, Precision, Accumulator0, Accumulator1),
		covering_segments(Segments, Latitude, LongitudeSpan, Precision, Accumulator1, Accumulator).

	covering_longitudes(LongitudeIndex, EndLongitudeIndex, _Latitude, _LongitudeSpan, _Precision, Accumulator, Accumulator) :-
		LongitudeIndex > EndLongitudeIndex,
		!.
	covering_longitudes(LongitudeIndex, EndLongitudeIndex, Latitude, LongitudeSpan, Precision, Accumulator0, Accumulator) :-
		Longitude is -180.0 + (LongitudeIndex + 0.5) * LongitudeSpan,
		encode(geographic(Latitude, Longitude), Precision, Geohash),
		Accumulator1 = [Geohash| Accumulator0],
		NextLongitudeIndex is LongitudeIndex + 1,
		covering_longitudes(NextLongitudeIndex, EndLongitudeIndex, Latitude, LongitudeSpan, Precision, Accumulator1, Accumulator).

	code_index(0'0, 0).
	code_index(0'1, 1).
	code_index(0'2, 2).
	code_index(0'3, 3).
	code_index(0'4, 4).
	code_index(0'5, 5).
	code_index(0'6, 6).
	code_index(0'7, 7).
	code_index(0'8, 8).
	code_index(0'9, 9).
	code_index(0'b, 10).
	code_index(0'c, 11).
	code_index(0'd, 12).
	code_index(0'e, 13).
	code_index(0'f, 14).
	code_index(0'g, 15).
	code_index(0'h, 16).
	code_index(0'j, 17).
	code_index(0'k, 18).
	code_index(0'm, 19).
	code_index(0'n, 20).
	code_index(0'p, 21).
	code_index(0'q, 22).
	code_index(0'r, 23).
	code_index(0's, 24).
	code_index(0't, 25).
	code_index(0'u, 26).
	code_index(0'v, 27).
	code_index(0'w, 28).
	code_index(0'x, 29).
	code_index(0'y, 30).
	code_index(0'z, 31).

	index_code(0, 0'0).
	index_code(1, 0'1).
	index_code(2, 0'2).
	index_code(3, 0'3).
	index_code(4, 0'4).
	index_code(5, 0'5).
	index_code(6, 0'6).
	index_code(7, 0'7).
	index_code(8, 0'8).
	index_code(9, 0'9).
	index_code(10, 0'b).
	index_code(11, 0'c).
	index_code(12, 0'd).
	index_code(13, 0'e).
	index_code(14, 0'f).
	index_code(15, 0'g).
	index_code(16, 0'h).
	index_code(17, 0'j).
	index_code(18, 0'k).
	index_code(19, 0'm).
	index_code(20, 0'n).
	index_code(21, 0'p).
	index_code(22, 0'q).
	index_code(23, 0'r).
	index_code(24, 0's).
	index_code(25, 0't).
	index_code(26, 0'u).
	index_code(27, 0'v).
	index_code(28, 0'w).
	index_code(29, 0'x).
	index_code(30, 0'y).
	index_code(31, 0'z).

:- end_object.
