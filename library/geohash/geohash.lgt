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
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2026-10-11,
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
		bbox_contains/2, bbox_expand/3, bbox_from_coordinates/2, bbox_intersects_polygon/2,
		bbox_intersects_polyline/2, bbox_overlaps/2, point_in_polygon/2, polygon_bounding_box/2,
		valid_coordinate/1
	]).

	:- uses(list, [
		member/2, reverse/2
	]).

	valid_geohash(Geohash) :-
		atom(Geohash),
		atom_codes(Geohash, Codes),
		Codes \== [],
		valid_geohash_codes(Codes).

	encode(Coordinate, Precision, Geohash) :-
		valid_spatial_precision(Precision),
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
		valid_spatial_precision(Precision),
		bit_counts(Precision, LatitudeBits, LongitudeBits),
		power_of_two(LatitudeBits, LatitudeDivisor),
		power_of_two(LongitudeBits, LongitudeDivisor),
		LatitudeError is 90.0 / LatitudeDivisor,
		LongitudeError is 180.0 / LongitudeDivisor.

	cell_dimensions(Precision, LatitudeSpan, LongitudeSpan) :-
		valid_spatial_precision(Precision),
		bit_counts(Precision, LatitudeBits, LongitudeBits),
		power_of_two(LatitudeBits, LatitudeDivisor),
		power_of_two(LongitudeBits, LongitudeDivisor),
		LatitudeSpan is 180.0 / LatitudeDivisor,
		LongitudeSpan is 360.0 / LongitudeDivisor.

	adjacent(Geohash, Direction, AdjacentGeohash) :-
		direction_delta(Direction, LatitudeDelta, LongitudeDelta),
		geohash_grid(Geohash, Precision, LatitudeIndex, LongitudeIndex, LatitudeCells, LongitudeCells),
		adjacent_grid(LatitudeIndex, LongitudeIndex, LatitudeCells, LongitudeCells, LatitudeDelta, LongitudeDelta, NextLatitude, NextLongitude),
		grid_geohash(Precision, NextLatitude, NextLongitude, LatitudeCells, LongitudeCells, AdjacentGeohash).

	neighbors(Geohash, Neighbors) :-
		geohash_grid(Geohash, Precision, LatitudeIndex, LongitudeIndex, LatitudeCells, LongitudeCells),
		grid_neighbors([north, north_east, east, south_east, south, south_west, west, north_west], characters(Precision), LatitudeIndex, LongitudeIndex, LatitudeCells, LongitudeCells, Neighbors).

	covering(BoundingBox, Precision, Geohashes) :-
		fixed_covering(BoundingBox, Precision, Geohashes).

	covering(BoundingBox, CoverSpec, Geohashes, UserOptions) :-
		valid_bounding_box(BoundingBox),
		valid_cover_spec(CoverSpec, LimitPrecision),
		^^check_options(UserOptions),
		reject_buffer_option(UserOptions),
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
		valid_spatial_precision(Precision),
		Bits is Precision * 5,
		encode_bits(Coordinate, Bits, HashInteger).

	decode_int(HashInteger, Precision, Coordinate) :-
		valid_spatial_precision(Precision),
		Bits is Precision * 5,
		decode_bits(HashInteger, Bits, Coordinate).

	bounding_box_int(HashInteger, Precision, BoundingBox) :-
		valid_spatial_precision(Precision),
		Bits is Precision * 5,
		bounding_box_bits(HashInteger, Bits, BoundingBox).

	adjacent_int(HashInteger, Precision, Direction, AdjacentHashInteger) :-
		valid_spatial_precision(Precision),
		Bits is Precision * 5,
		adjacent_bits(HashInteger, Bits, Direction, AdjacentHashInteger).

	neighbors_int(HashInteger, Precision, Neighbors) :-
		valid_spatial_precision(Precision),
		Bits is Precision * 5,
		neighbors_bits(HashInteger, Bits, Neighbors).

	geohash_to_int(Geohash, HashInteger) :-
		atom(Geohash),
		atom_length(Geohash, Precision),
		valid_spatial_precision(Precision),
		Bits is Precision * 5,
		valid_bit_precision(Bits),
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
		integer_grid(HashInteger, Bits, LatitudeIndex, LongitudeIndex, LatitudeCells, LongitudeCells),
		adjacent_grid(LatitudeIndex, LongitudeIndex, LatitudeCells, LongitudeCells, LatitudeDelta, LongitudeDelta, NextLatitude, NextLongitude),
		grid_hash(bits(Bits), NextLatitude, NextLongitude, LatitudeCells, LongitudeCells, AdjacentHashInteger).

	neighbors_bits(HashInteger, Bits, Neighbors) :-
		integer_grid(HashInteger, Bits, LatitudeIndex, LongitudeIndex, LatitudeCells, LongitudeCells),
		grid_neighbors([north, north_east, east, south_east, south, south_west, west, north_west], bits(Bits), LatitudeIndex, LongitudeIndex, LatitudeCells, LongitudeCells, Neighbors).

	expand(Geohash, ExpandedGeohashes) :-
		neighbors(Geohash, Neighbors),
		neighbor_geohashes(Neighbors, NeighborGeohashes),
		ExpandedGeohashes = [Geohash| NeighborGeohashes].

	expand_int(HashInteger, Precision, ExpandedHashes) :-
		neighbors_int(HashInteger, Precision, Neighbors),
		neighbor_hash_values(Neighbors, NeighborHashes),
		ExpandedHashes = [HashInteger| NeighborHashes].

	polygon_covering(Polygon, CoverSpec, Geohashes, UserOptions) :-
		polygon_bounding_box(Polygon, BoundingBox),
		valid_cover_spec(CoverSpec, LimitPrecision),
		^^check_options(UserOptions),
		reject_buffer_option(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(compact(Compact), Options),
		^^option(min_precision(MinPrecision), Options),
		MinPrecision =< LimitPrecision,
		geometry_covering(CoverSpec, BoundingBox, MinPrecision, polygon(Polygon), Cover),
		compress_geometry_cover(CoverSpec, Compact, MinPrecision, Cover, Geohashes).

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
		geometry_covering(CoverSpec, BoundingBox, MinPrecision, polyline(Polyline, Buffer), Cover),
		compress_geometry_cover(CoverSpec, Compact, MinPrecision, Cover, Geohashes).

	fixed_covering(BoundingBox, Precision, Geohashes) :-
		fixed_covering_filtered(BoundingBox, Precision, none, Geohashes).

	fixed_covering_filtered(BoundingBox, Precision, Filter, Geohashes) :-
		valid_spatial_precision(Precision),
		valid_bounding_box(BoundingBox),
		BoundingBox = bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude)),
		bit_counts(Precision, LatitudeBits, LongitudeBits),
		power_of_two(LatitudeBits, LatitudeCells),
		power_of_two(LongitudeBits, LongitudeCells),
		LatitudeSpan is 180.0 / LatitudeCells,
		LongitudeSpan is 360.0 / LongitudeCells,
		index_bounds(MinLatitude, MaxLatitude, -90.0, LatitudeSpan, LatitudeCells, StartLatitudeIndex, EndLatitudeIndex),
		(	MinLongitude =:= 180, MaxLongitude =:= 180 ->
			longitude_segments(-180.0, -180.0, LongitudeSpan, LongitudeCells, Segments)
		;	longitude_segments(MinLongitude, MaxLongitude, LongitudeSpan, LongitudeCells, Segments)
		),
		Grid = grid(Precision, LatitudeCells, LongitudeCells, Filter),
		covering_grid(StartLatitudeIndex, EndLatitudeIndex, Segments, Grid, Geohashes, []).

	covering_spec_geohashes(precision(Precision), BoundingBox, _MinPrecision, Geohashes) :-
		fixed_covering(BoundingBox, Precision, Geohashes).
	covering_spec_geohashes(max_precision(MaxPrecision), BoundingBox, MinPrecision, Geohashes) :-
		adaptive_covering(BoundingBox, MaxPrecision, MinPrecision, Geohashes).

	geometry_covering(precision(Precision), BoundingBox, _MinPrecision, Filter, Geohashes) :-
		fixed_covering_filtered(BoundingBox, Precision, Filter, Geohashes).
	geometry_covering(max_precision(MaxPrecision), BoundingBox, MinPrecision, Filter, Geohashes) :-
		(	Filter = polyline(_, Buffer), Buffer > 0 ->
			fixed_covering_filtered(BoundingBox, MaxPrecision, Filter, Geohashes)
		;	fixed_covering(BoundingBox, MinPrecision, Seeds),
			prepare_geometry(Filter, Geometry),
			geometry_cover_seeds(Seeds, BoundingBox, MaxPrecision, Geometry, Geohashes, [])
		).

	prepare_geometry(polygon(Polygon), polygon(Polygon, Edges)) :-
		list::last(Polygon, Last),
		polygon_edges(Polygon, Last, Edges).
	prepare_geometry(polyline(Polyline, Buffer), polyline(Polyline, Buffer)).

	polygon_edges([], _Previous, []).
	polygon_edges([Coordinate| Coordinates], Previous, [Previous-Coordinate| Edges]) :-
		polygon_edges(Coordinates, Coordinate, Edges).

	geometry_cover_seeds([], _BoundingBox, _MaxPrecision, _Geometry, Tail, Tail).
	geometry_cover_seeds([Geohash| Geohashes], BoundingBox, MaxPrecision, Geometry, Cover, Tail) :-
		bounding_box(Geohash, Cell),
		atom_length(Geohash, Precision),
		(	Precision mod 2 =:= 0 ->
			Axis = longitude
		;	Axis = latitude
		),
		geometry_cover_node(Geohash, Cell, Axis, Precision, BoundingBox, MaxPrecision, Geometry, Cover, Rest),
		geometry_cover_seeds(Geohashes, BoundingBox, MaxPrecision, Geometry, Rest, Tail).

	geometry_cover_node(Geohash, Cell, Axis, Precision, BoundingBox, MaxPrecision, Geometry, Cover, Tail) :-
		(	candidate_cell_intersects(BoundingBox, Cell), prepared_geometry_intersects(Geometry, Cell) ->
			(	Precision >= MaxPrecision ->
				Cover = [Geohash| Tail]
			;	prepared_geometry_contains(Geometry, Cell) ->
				Cover = [Geohash| Tail]
			;	findall(Code-Index, code_index(Code, Index), Alphabet),
				NextPrecision is Precision + 1,
				geometry_cover_children(Alphabet, Geohash, Cell, Axis, NextPrecision, BoundingBox, MaxPrecision, Geometry, Cover, Tail)
			)
		;	Cover = Tail
		).

	geometry_cover_children([], _Prefix, _Cell, _Axis, _Precision, _BoundingBox, _MaxPrecision, _Geometry, Tail, Tail).
	geometry_cover_children([Code-Index| Codes], Prefix, bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude)), Axis, Precision, BoundingBox, MaxPrecision, Geometry, Cover, Tail) :-
		decode_index_bits(Index, 16, MinLongitude, MaxLongitude, MinLatitude, MaxLatitude, Axis, ChildMinLongitude, ChildMaxLongitude, ChildMinLatitude, ChildMaxLatitude, ChildAxis),
		ChildCell = bbox(geographic(ChildMinLatitude, ChildMinLongitude), geographic(ChildMaxLatitude, ChildMaxLongitude)),
		char_code(Suffix, Code),
		atom_concat(Prefix, Suffix, Child),
		geometry_cover_node(Child, ChildCell, ChildAxis, Precision, BoundingBox, MaxPrecision, Geometry, Cover, Rest),
		geometry_cover_children(Codes, Prefix, bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude)), Axis, Precision, BoundingBox, MaxPrecision, Geometry, Rest, Tail).

	candidate_cell_intersects(bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude)), bbox(geographic(CellMinLatitude, CellMinLongitude), geographic(CellMaxLatitude, CellMaxLongitude))) :-
		cover_interval_intersects(MinLatitude, MaxLatitude, CellMinLatitude, CellMaxLatitude, 90.0),
		(	MinLongitude =:= 180, MaxLongitude =:= 180 ->
			cover_interval_intersects(-180.0, -180.0, CellMinLongitude, CellMaxLongitude, 180.0)
		;	MinLongitude =< MaxLongitude ->
			cover_interval_intersects(MinLongitude, MaxLongitude, CellMinLongitude, CellMaxLongitude, 180.0)
		;	once((
				cover_interval_intersects(MinLongitude, 180.0, CellMinLongitude, CellMaxLongitude, 180.0);
				cover_interval_intersects(-180.0, MaxLongitude, CellMinLongitude, CellMaxLongitude, 180.0)
			))
		).

	cover_interval_intersects(Minimum, Maximum, CellMinimum, CellMaximum, UpperLimit) :-
		(	Minimum =:= Maximum ->
			Minimum >= CellMinimum,
			(	Minimum < CellMaximum ->
				true
			;	Minimum =:= UpperLimit,
				CellMaximum =:= UpperLimit
			)
		;	Minimum < CellMaximum,
			CellMinimum < Maximum
		).

	prepared_geometry_intersects(polygon(Polygon, _Edges), Cell) :-
		cell_geometry_intersects(polygon(Polygon), Cell).
	prepared_geometry_intersects(polyline(Polyline, Buffer), Cell) :-
		cell_geometry_intersects(polyline(Polyline, Buffer), Cell).

	prepared_geometry_contains(polygon(Polygon, Edges), bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude))) :-
		forall(
			member(Corner, [geographic(MinLatitude, MinLongitude), geographic(MinLatitude, MaxLongitude), geographic(MaxLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude)]),
			point_in_polygon(Corner, Polygon)
		),
		\+ (
			member(Start-End, Edges),
			segment_in_open_cell(Start, End, MinLatitude, MaxLatitude, MinLongitude, MaxLongitude)
		).

	segment_in_open_cell(geographic(StartLatitude, StartLongitude), geographic(EndLatitude, EndLongitude), MinLatitude, MaxLatitude, MinLongitude, MaxLongitude) :-
		LatitudeDelta is EndLatitude - StartLatitude,
		LongitudeDelta is EndLongitude - StartLongitude,
		open_axis_interval(StartLatitude, LatitudeDelta, MinLatitude, MaxLatitude, LatitudeStart, LatitudeEnd),
		open_axis_interval(StartLongitude, LongitudeDelta, MinLongitude, MaxLongitude, LongitudeStart, LongitudeEnd),
		max(LatitudeStart, LongitudeStart) < min(LatitudeEnd, LongitudeEnd).

	open_axis_interval(Origin, Delta, Minimum, Maximum, Start, End) :-
		(	Delta =:= 0 ->
			Origin > Minimum,
			Origin < Maximum,
			Start = 0.0,
			End = 1.0
		;	First is (Minimum - Origin) / Delta,
			Second is (Maximum - Origin) / Delta,
			Start is max(0.0, min(First, Second)),
			End is min(1.0, max(First, Second)),
			Start < End
		).

	adaptive_covering(BoundingBox, MaxPrecision, MinPrecision, Geohashes) :-
		BoundingBox = bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude)),
		(	(MinLatitude =:= MaxLatitude; MinLongitude =:= MaxLongitude) ->
			fixed_covering(BoundingBox, MaxPrecision, Geohashes)
		;	fixed_covering(BoundingBox, MinPrecision, Seeds),
			adaptive_covering(Seeds, BoundingBox, MaxPrecision, [], ReversedGeohashes),
			reverse(ReversedGeohashes, Geohashes)
		).

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
		findall(Code, code_index(Code, _), Alphabet),
		compress_prefix_node('', Geohashes, Alphabet, MinPrecision, CompressedGeohashes, [], _).

	compress_prefix_node(_Prefix, [], _Alphabet, _MinPrecision, Tail, Tail, false) :-
		!.
	compress_prefix_node(Prefix, [Prefix], _Alphabet, _MinPrecision, [Prefix| Tail], Tail, true) :-
		!.
	compress_prefix_node(Prefix, Geohashes, Alphabet, MinPrecision, Cover, Tail, Complete) :-
		compress_prefix_children(Alphabet, Prefix, Geohashes, Alphabet, MinPrecision, Children, Tail, Complete),
		atom_length(Prefix, Precision),
		(	Complete == true,
			Precision >= MinPrecision ->
			Cover = [Prefix| Tail]
		;	Cover = Children
		).

	compress_prefix_children([], _Prefix, [], _Alphabet, _MinPrecision, Tail, Tail, true).
	compress_prefix_children([Code| Codes], Prefix, Geohashes, Alphabet, MinPrecision, Cover, Tail, Complete) :-
		char_code(Suffix, Code),
		atom_concat(Prefix, Suffix, Child),
		take_prefix_group(Geohashes, Child, Group, Remaining),
		compress_prefix_node(Child, Group, Alphabet, MinPrecision, Cover, Rest, ChildComplete),
		compress_prefix_children(Codes, Prefix, Remaining, Alphabet, MinPrecision, Rest, Tail, RestComplete),
		(	ChildComplete == true,
			RestComplete == true ->
			Complete = true
		;	Complete = false
		).

	take_prefix_group([Geohash| Geohashes], Prefix, [Geohash| Group], Remaining) :-
		atom_length(Prefix, Length),
		sub_atom(Geohash, 0, Length, _, Prefix),
		!,
		take_prefix_group(Geohashes, Prefix, Group, Remaining).
	take_prefix_group(Geohashes, _Prefix, [], Geohashes).

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
		Prefixes = [Prefix| _],
		proper_geohash_prefix(Prefix, Geohash).

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
		valid_spatial_precision(Precision).
	valid_cover_spec(max_precision(Precision), Precision) :-
		valid_spatial_precision(Precision).

	default_option(compact(false)).
	default_option(min_precision(1)).
	default_option(buffer(0.0)).

	valid_option(compact(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(min_precision(Precision)) :-
		valid_precision(Precision).
	valid_option(buffer(Buffer)) :-
		number(Buffer),
		Buffer >= 0.

	reject_buffer_option([buffer(Buffer)| _]) :-
		domain_error(option, buffer(Buffer)).
	reject_buffer_option([_| Options]) :-
		reject_buffer_option(Options).
	reject_buffer_option([]).

	neighbor_geohashes([], []).
	neighbor_geohashes([_-Geohash| Neighbors], [Geohash| Geohashes]) :-
		neighbor_geohashes(Neighbors, Geohashes).

	neighbor_hash_values([], []).
	neighbor_hash_values([_-HashInteger| Neighbors], [HashInteger| Hashes]) :-
		neighbor_hash_values(Neighbors, Hashes).

	geohash_grid(Geohash, Precision, LatitudeIndex, LongitudeIndex, LatitudeCells, LongitudeCells) :-
		atom(Geohash),
		atom_length(Geohash, Precision),
		valid_spatial_precision(Precision),
		atom_codes(Geohash, Codes),
		decode_grid_codes(Codes, longitude, 0, 0, LatitudeIndex, LongitudeIndex),
		bit_counts(Precision, LatitudeBits, LongitudeBits),
		power_of_two(LatitudeBits, LatitudeCells),
		power_of_two(LongitudeBits, LongitudeCells).

	integer_grid(HashInteger, Bits, LatitudeIndex, LongitudeIndex, LatitudeCells, LongitudeCells) :-
		valid_hash_bits_integer(HashInteger, Bits),
		Mask is 1 << (Bits - 1),
		decode_grid_mask(HashInteger, Mask, longitude, 0, 0, LatitudeIndex, LongitudeIndex, _),
		LatitudeBits is Bits // 2,
		LongitudeBits is (Bits + 1) // 2,
		power_of_two(LatitudeBits, LatitudeCells),
		power_of_two(LongitudeBits, LongitudeCells).

	decode_grid_codes([], _Axis, Latitude, Longitude, Latitude, Longitude).
	decode_grid_codes([Code| Codes], Axis, Latitude0, Longitude0, Latitude, Longitude) :-
		code_index(Code, Index),
		decode_grid_mask(Index, 16, Axis, Latitude0, Longitude0, Latitude1, Longitude1, NextAxis),
		decode_grid_codes(Codes, NextAxis, Latitude1, Longitude1, Latitude, Longitude).

	decode_grid_mask(_Hash, 0, Axis, Latitude, Longitude, Latitude, Longitude, Axis) :-
		!.
	decode_grid_mask(Hash, Mask, Axis, Latitude0, Longitude0, Latitude, Longitude, FinalAxis) :-
		(	Hash /\ Mask =:= 0 ->
			Bit = 0
		;	Bit = 1
		),
		append_grid_bit(Axis, Bit, Latitude0, Longitude0, Latitude1, Longitude1, NextAxis),
		NextMask is Mask >> 1,
		decode_grid_mask(Hash, NextMask, NextAxis, Latitude1, Longitude1, Latitude, Longitude, FinalAxis).

	append_grid_bit(longitude, Bit, Latitude, Longitude0, Latitude, Longitude, latitude) :-
		Longitude is Longitude0 * 2 + Bit.
	append_grid_bit(latitude, Bit, Latitude0, Longitude, Latitude, Longitude, longitude) :-
		Latitude is Latitude0 * 2 + Bit.

	adjacent_grid(Latitude, Longitude, LatitudeCells, LongitudeCells, LatitudeDelta, LongitudeDelta, NextLatitude, NextLongitude) :-
		NextLatitude is Latitude + LatitudeDelta,
		NextLatitude >= 0,
		NextLatitude < LatitudeCells,
		Longitude0 is Longitude + LongitudeDelta,
		(	Longitude0 < 0 ->
			NextLongitude is Longitude0 + LongitudeCells
		;	Longitude0 >= LongitudeCells ->
			NextLongitude is Longitude0 - LongitudeCells
		;	NextLongitude = Longitude0
		).

	grid_neighbors([], _Representation, _Latitude, _Longitude, _LatitudeCells, _LongitudeCells, []).
	grid_neighbors([Direction| Directions], Representation, Latitude, Longitude, LatitudeCells, LongitudeCells, Neighbors) :-
		direction_delta(Direction, LatitudeDelta, LongitudeDelta),
		(	adjacent_grid(Latitude, Longitude, LatitudeCells, LongitudeCells, LatitudeDelta, LongitudeDelta, NextLatitude, NextLongitude) ->
			grid_hash(Representation, NextLatitude, NextLongitude, LatitudeCells, LongitudeCells, Neighbor),
			Neighbors = [Direction-Neighbor| Tail]
		;	Neighbors = Tail
		),
		grid_neighbors(Directions, Representation, Latitude, Longitude, LatitudeCells, LongitudeCells, Tail).

	grid_hash(characters(Precision), Latitude, Longitude, LatitudeCells, LongitudeCells, Geohash) :-
		grid_geohash(Precision, Latitude, Longitude, LatitudeCells, LongitudeCells, Geohash).
	grid_hash(bits(Bits), Latitude, Longitude, LatitudeCells, LongitudeCells, Hash) :-
		LatitudeMask is LatitudeCells // 2,
		LongitudeMask is LongitudeCells // 2,
		encode_grid_bits(Bits, Latitude, Longitude, LatitudeMask, LongitudeMask, longitude, 0, Hash, _, _, _).

	grid_geohash(Precision, Latitude, Longitude, LatitudeCells, LongitudeCells, Geohash) :-
		LatitudeMask is LatitudeCells // 2,
		LongitudeMask is LongitudeCells // 2,
		encode_grid_codes(Precision, Latitude, Longitude, LatitudeMask, LongitudeMask, longitude, Codes),
		atom_codes(Geohash, Codes).

	encode_grid_codes(0, _Latitude, _Longitude, _LatitudeMask, _LongitudeMask, _Axis, []) :-
		!.
	encode_grid_codes(Precision, Latitude, Longitude, LatitudeMask, LongitudeMask, Axis, [Code| Codes]) :-
		encode_grid_bits(5, Latitude, Longitude, LatitudeMask, LongitudeMask, Axis, 0, Index, NextLatitudeMask, NextLongitudeMask, NextAxis),
		index_code(Index, Code),
		NextPrecision is Precision - 1,
		encode_grid_codes(NextPrecision, Latitude, Longitude, NextLatitudeMask, NextLongitudeMask, NextAxis, Codes).

	encode_grid_bits(0, _Latitude, _Longitude, LatitudeMask, LongitudeMask, Axis, Hash, Hash, LatitudeMask, LongitudeMask, Axis) :-
		!.
	encode_grid_bits(Bits, Latitude, Longitude, LatitudeMask, LongitudeMask, Axis, Hash0, Hash, FinalLatitudeMask, FinalLongitudeMask, FinalAxis) :-
		grid_axis_bit(Axis, Latitude, Longitude, LatitudeMask, LongitudeMask, Bit, NextLatitudeMask, NextLongitudeMask, NextAxis),
		Hash1 is Hash0 * 2 + Bit,
		NextBits is Bits - 1,
		encode_grid_bits(NextBits, Latitude, Longitude, NextLatitudeMask, NextLongitudeMask, NextAxis, Hash1, Hash, FinalLatitudeMask, FinalLongitudeMask, FinalAxis).

	grid_axis_bit(longitude, _Latitude, Longitude, LatitudeMask, LongitudeMask, Bit, LatitudeMask, NextLongitudeMask, latitude) :-
		(	Longitude /\ LongitudeMask =:= 0 ->
			Bit = 0
		;	Bit = 1
		),
		NextLongitudeMask is LongitudeMask >> 1.
	grid_axis_bit(latitude, Latitude, _Longitude, LatitudeMask, LongitudeMask, Bit, NextLatitudeMask, LongitudeMask, longitude) :-
		(	Latitude /\ LatitudeMask =:= 0 ->
			Bit = 0
		;	Bit = 1
		),
		NextLatitudeMask is LatitudeMask >> 1.

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
		valid_spatial_precision(Precision),
		TotalBits is Precision * 5,
		valid_bit_precision(TotalBits),
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

	polyline_covering_bounding_box(Polyline, Buffer, BoundingBox) :-
		Buffer =:= 0,
		bbox_from_coordinates(Polyline, BoundingBox),
		!.
	polyline_covering_bounding_box(Polyline, Buffer, BoundingBox) :-
		bbox_from_coordinates(Polyline, PolylineBoundingBox),
		bbox_expand(PolylineBoundingBox, Buffer, BoundingBox).

	cell_geometry_intersects(polygon(Polygon), BoundingBox) :-
		bbox_intersects_polygon(BoundingBox, Polygon).
	cell_geometry_intersects(polyline(Polyline, Buffer), BoundingBox) :-
		Buffer =:= 0,
		!,
		bbox_intersects_polyline(BoundingBox, Polyline).
	cell_geometry_intersects(polyline(Polyline, Buffer), BoundingBox) :-
		Buffer > 0.0,
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

	valid_spatial_precision(Precision) :-
		valid_precision(Precision),
		Precision =< 12.

	valid_bit_precision(Bits) :-
		integer(Bits),
		Bits > 0,
		Bits =< 60,
		(	current_prolog_flag(bounded, true) ->
			current_prolog_flag(max_integer, Maximum),
			integer_capacity(Bits, Maximum, 1)
		;	true
		).

	integer_capacity(0, _Maximum, _Power) :-
		!.
	integer_capacity(Bits, Maximum, Power) :-
		Power =< Maximum // 2,
		NextPower is Power * 2,
		NextBits is Bits - 1,
		integer_capacity(NextBits, Maximum, NextPower).

	valid_polyline([Coordinate1, Coordinate2| Coordinates]) :-
		valid_coordinate(Coordinate1),
		valid_coordinate(Coordinate2),
		all_valid_coordinates(Coordinates).

	all_valid_coordinates([]).
	all_valid_coordinates([Coordinate| Coordinates]) :-
		valid_coordinate(Coordinate),
		all_valid_coordinates(Coordinates).

	canonical_encode_coordinate(geographic(Latitude, Longitude0), geographic(Latitude, Longitude)) :-
		canonical_longitude(Longitude0, Longitude).

	canonical_longitude(Longitude0, -180.0) :-
		Longitude0 =:= 180,
		!.
	canonical_longitude(Longitude, Longitude).

	valid_geohash_codes([]).
	valid_geohash_codes([Code| Codes]) :-
		code_index(Code, _),
		valid_geohash_codes(Codes).

	decode_intervals(Geohash, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax) :-
		atom(Geohash),
		atom_length(Geohash, Precision),
		valid_spatial_precision(Precision),
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
		Power is 1 << Exponent.

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
	longitude_segments(MinLongitude, MaxLongitude, LongitudeSpan, LongitudeCells, Segments) :-
		index_bounds(MinLongitude, 180.0, -180.0, LongitudeSpan, LongitudeCells, StartIndex1, EndIndex1),
		index_bounds(-180.0, MaxLongitude, -180.0, LongitudeSpan, LongitudeCells, StartIndex2, EndIndex2),
		(	StartIndex1 =:= 0 ->
			Segments = [StartIndex1-EndIndex1]
		;	StartIndex1 =< EndIndex2 ->
			EndIndex is StartIndex1 - 1,
			Segments = [StartIndex1-EndIndex1, StartIndex2-EndIndex]
		;	Segments = [StartIndex1-EndIndex1, StartIndex2-EndIndex2]
		).

	covering_grid(LatitudeIndex, EndLatitudeIndex, _Segments, _Grid, Tail, Tail) :-
		LatitudeIndex > EndLatitudeIndex,
		!.
	covering_grid(LatitudeIndex, EndLatitudeIndex, Segments, Grid, Cover, Tail) :-
		covering_grid_segments(Segments, LatitudeIndex, Grid, Cover, Rest),
		NextLatitudeIndex is LatitudeIndex + 1,
		covering_grid(NextLatitudeIndex, EndLatitudeIndex, Segments, Grid, Rest, Tail).

	covering_grid_segments([], _Latitude, _Grid, Tail, Tail).
	covering_grid_segments([StartIndex-EndIndex| Segments], Latitude, Grid, Cover, Tail) :-
		covering_grid_longitudes(StartIndex, EndIndex, Latitude, Grid, Cover, Rest),
		covering_grid_segments(Segments, Latitude, Grid, Rest, Tail).

	covering_grid_longitudes(LongitudeIndex, EndLongitudeIndex, _Latitude, _Grid, Tail, Tail) :-
		LongitudeIndex > EndLongitudeIndex,
		!.
	covering_grid_longitudes(LongitudeIndex, EndLongitudeIndex, Latitude, grid(Precision, LatitudeCells, LongitudeCells, Filter), Cover, Tail) :-
		(	selected_grid_cell(Filter, Latitude, LongitudeIndex, LatitudeCells, LongitudeCells) ->
			grid_geohash(Precision, Latitude, LongitudeIndex, LatitudeCells, LongitudeCells, Geohash),
			Cover = [Geohash| Rest]
		;	Cover = Rest
		),
		NextLongitudeIndex is LongitudeIndex + 1,
		covering_grid_longitudes(NextLongitudeIndex, EndLongitudeIndex, Latitude, grid(Precision, LatitudeCells, LongitudeCells, Filter), Rest, Tail).

	selected_grid_cell(none, _Latitude, _Longitude, _LatitudeCells, _LongitudeCells) :-
		!.
	selected_grid_cell(Filter, Latitude, Longitude, LatitudeCells, LongitudeCells) :-
		LatitudeSpan is 180.0 / LatitudeCells,
		LongitudeSpan is 360.0 / LongitudeCells,
		MinLatitude is -90.0 + Latitude * LatitudeSpan,
		MaxLatitude is MinLatitude + LatitudeSpan,
		MinLongitude is -180.0 + Longitude * LongitudeSpan,
		MaxLongitude is MinLongitude + LongitudeSpan,
		cell_geometry_intersects(Filter, bbox(geographic(MinLatitude, MinLongitude), geographic(MaxLatitude, MaxLongitude))).

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
