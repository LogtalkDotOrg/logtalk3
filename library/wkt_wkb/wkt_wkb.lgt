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


:- object(wkt_wkb,
	implements(wkt_wkb_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-09,
		comment is 'Well-Known Text (WKT) and Well-Known Binary (WKB) geometry parser, generator, and validator.',
		see_also is [wkt_wkb_protocol, geo_json, geospatial, cbor, message_pack(_)]
	]).

	:- uses(reader, [
		file_to_bytes/2, file_to_codes/2, stream_to_bytes/2, stream_to_codes/2
	]).

	:- uses(list, [
		append/3, last/2, length/2, memberchk/2, reverse/2, take/4, valid/1 as is_list/1
	]).

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(Source, Geometry) :-
		parse_source_geometry(Source, Geometry),
		!.
	parse(Source, _) :-
		valid_source(Source),
		!,
		domain_error(wkt_wkb, Source).
	parse(Source, _) :-
		domain_error(wkt_wkb_source, Source).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(Sink, Geometry) :-
		generate_sink_geometry(Sink, Geometry),
		!.
	generate(Sink, Geometry) :-
		valid_sink(Sink),
		!,
		domain_error(wkt_wkb, Geometry).
	generate(Sink, _) :-
		domain_error(wkt_wkb_sink, Sink).

	validate(Geometry) :-
		validate(Geometry, Errors),
		Errors == [].

	validate(Geometry, _) :-
		var(Geometry),
		instantiation_error.
	validate(Geometry, Errors) :-
		validate_geometry(Geometry, [], Errors).

	parse_source_geometry(wkt(TextSource), Geometry) :-
		read_wkt_source(TextSource, Codes),
		parse_wkt_codes(Codes, Geometry).
	parse_source_geometry(wkb(BinarySource), Geometry) :-
		read_wkb_source(BinarySource, Bytes),
		parse_wkb_bytes(Bytes, Geometry).

	generate_sink_geometry(wkt(TextSink), Geometry) :-
		validate(Geometry),
		(	geometry_has_special_coordinates(Geometry) ->
			domain_error(wkt_wkb_representation, wkt(Geometry))
		;	true
		),
		generate_wkt_codes(Geometry, Codes),
		write_wkt_sink(TextSink, Codes).
	generate_sink_geometry(wkb(BinarySink), Geometry) :-
		generate_sink_geometry(wkb(BinarySink, little), Geometry).
	generate_sink_geometry(wkb(BinarySink, Order), Geometry) :-
		valid_order(Order),
		validate(Geometry),
		generate_wkb_bytes(Order, Geometry, Bytes),
		write_wkb_sink(BinarySink, Bytes).

	valid_source(wkt(TextSource)) :-
		valid_wkt_source(TextSource).
	valid_source(wkb(BinarySource)) :-
		valid_wkb_source(BinarySource).

	valid_sink(wkt(TextSink)) :-
		valid_wkt_sink(TextSink).
	valid_sink(wkb(BinarySink)) :-
		valid_wkb_sink(BinarySink).
	valid_sink(wkb(BinarySink, Order)) :-
		valid_wkb_sink(BinarySink),
		valid_order(Order).

	valid_wkt_source(file(_)).
	valid_wkt_source(stream(_)).
	valid_wkt_source(atom(_)).
	valid_wkt_source(chars(_)).
	valid_wkt_source(codes(_)).

	valid_wkb_source(file(_)).
	valid_wkb_source(stream(_)).
	valid_wkb_source(bytes(_)).
	valid_wkb_source(hex(atom(_))).
	valid_wkb_source(hex(chars(_))).
	valid_wkb_source(hex(codes(_))).

	valid_wkt_sink(file(_)).
	valid_wkt_sink(stream(_)).
	valid_wkt_sink(atom(_)).
	valid_wkt_sink(chars(_)).
	valid_wkt_sink(codes(_)).

	valid_wkb_sink(file(_)).
	valid_wkb_sink(stream(_)).
	valid_wkb_sink(bytes(_)).
	valid_wkb_sink(hex(atom(_))).
	valid_wkb_sink(hex(chars(_))).
	valid_wkb_sink(hex(codes(_))).

	valid_order(little).
	valid_order(big).

	read_wkt_source(file(File), Codes) :-
		file_to_codes(File, Codes),
		!.
	read_wkt_source(stream(Stream), Codes) :-
		stream_to_codes(Stream, Codes),
		!.
	read_wkt_source(atom(Atom), Codes) :-
		atom_codes(Atom, Codes),
		!.
	read_wkt_source(chars(Chars), Codes) :-
		chars_to_codes(Chars, Codes),
		!.
	read_wkt_source(codes(Codes), Codes) :-
		!.

	read_wkb_source(file(File), Bytes) :-
		file_to_bytes(File, Bytes),
		!.
	read_wkb_source(stream(Stream), Bytes) :-
		stream_to_bytes(Stream, Bytes),
		!.
	read_wkb_source(bytes(Bytes), Bytes) :-
		!.
	read_wkb_source(hex(atom(Atom)), Bytes) :-
		atom_codes(Atom, Codes),
		hex_codes_bytes(Codes, Bytes),
		!.
	read_wkb_source(hex(chars(Chars)), Bytes) :-
		chars_to_codes(Chars, Codes),
		hex_codes_bytes(Codes, Bytes),
		!.
	read_wkb_source(hex(codes(Codes)), Bytes) :-
		hex_codes_bytes(Codes, Bytes),
		!.

	write_wkt_sink(file(File), Codes) :-
		open(File, write, Stream),
		catch(
			write_codes(Codes, Stream),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream),
		!.
	write_wkt_sink(stream(Stream), Codes) :-
		write_codes(Codes, Stream),
		!.
	write_wkt_sink(atom(Atom), Codes) :-
		atom_codes(Atom, Codes),
		!.
	write_wkt_sink(chars(Chars), Codes) :-
		codes_to_chars(Codes, Chars),
		!.
	write_wkt_sink(codes(Codes), Codes) :-
		!.

	write_wkb_sink(file(File), Bytes) :-
		open(File, write, Stream, [type(binary)]),
		catch(
			write_bytes(Bytes, Stream),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream),
		!.
	write_wkb_sink(stream(Stream), Bytes) :-
		write_bytes(Bytes, Stream),
		!.
	write_wkb_sink(bytes(Bytes), Bytes) :-
		!.
	write_wkb_sink(hex(atom(Atom)), Bytes) :-
		bytes_hex(Bytes, Atom),
		!.
	write_wkb_sink(hex(chars(Chars)), Bytes) :-
		bytes_hex(Bytes, Atom),
		atom_chars(Atom, Chars),
		!.
	write_wkb_sink(hex(codes(Codes)), Bytes) :-
		bytes_hex(Bytes, Atom),
		atom_codes(Atom, Codes),
		!.

	parse_wkt_codes(Codes, Geometry) :-
		uppercase_ascii_codes(Codes, UppercaseCodes),
		tokenize_wkt(UppercaseCodes, Tokens),
		wkt_geometry_tokens(xy, Geometry, Tokens, []),
		validate(Geometry).

	generate_wkt_codes(Geometry, Codes) :-
		phrase(wkt_geometry_codes(Geometry), Codes).

	wkt_geometry_tokens(DefaultDimensions, Geometry, [id(TypeToken)| Tokens0], Tokens) :-
		wkt_type_token(TypeToken, Type),
		wkt_dimensions_tokens(DefaultDimensions, Dimensions, Tokens0, Tokens1),
		wkt_geometry_body_tokens(Type, Dimensions, Geometry, Tokens1, Tokens).

	wkt_dimensions_tokens(_DefaultDimensions, Dimensions, [id(DimensionToken)| Tokens], Tokens) :-
		dimension_token(DimensionToken, Dimensions),
		!.
	wkt_dimensions_tokens(DefaultDimensions, DefaultDimensions, Tokens, Tokens).

	wkt_geometry_body_tokens(point, Dimensions, Geometry, [id('EMPTY')| Tokens], Tokens) :-
		!,
		build_geometry(point, [], Dimensions, Geometry).
	wkt_geometry_body_tokens(point, Dimensions, Geometry, [lparen| Tokens0], Tokens) :-
		dimension_arity(Dimensions, Arity),
		wkt_position_tokens(Position, Arity, Tokens0, [rparen| Tokens]),
		build_geometry(point, Position, Dimensions, Geometry).

	wkt_geometry_body_tokens(line_string, Dimensions, Geometry, [id('EMPTY')| Tokens], Tokens) :-
		!,
		build_geometry(line_string, [], Dimensions, Geometry).
	wkt_geometry_body_tokens(line_string, Dimensions, Geometry, [lparen| Tokens0], Tokens) :-
		dimension_arity(Dimensions, Arity),
		wkt_position_list_tokens(Positions, Arity, Tokens0, [rparen| Tokens]),
		build_geometry(line_string, Positions, Dimensions, Geometry).

	wkt_geometry_body_tokens(polygon, Dimensions, Geometry, [id('EMPTY')| Tokens], Tokens) :-
		!,
		build_geometry(polygon, [], Dimensions, Geometry).
	wkt_geometry_body_tokens(polygon, Dimensions, Geometry, [lparen| Tokens0], Tokens) :-
		dimension_arity(Dimensions, Arity),
		wkt_ring_list_tokens(Rings, Arity, Tokens0, [rparen| Tokens]),
		build_geometry(polygon, Rings, Dimensions, Geometry).

	wkt_geometry_body_tokens(multi_point, Dimensions, Geometry, [id('EMPTY')| Tokens], Tokens) :-
		!,
		build_geometry(multi_point, [], Dimensions, Geometry).
	wkt_geometry_body_tokens(multi_point, Dimensions, Geometry, [lparen| Tokens0], Tokens) :-
		dimension_arity(Dimensions, Arity),
		wkt_multi_point_list_tokens(Points, Arity, Tokens0, [rparen| Tokens]),
		build_geometry(multi_point, Points, Dimensions, Geometry).

	wkt_geometry_body_tokens(multi_line_string, Dimensions, Geometry, [id('EMPTY')| Tokens], Tokens) :-
		!,
		build_geometry(multi_line_string, [], Dimensions, Geometry).
	wkt_geometry_body_tokens(multi_line_string, Dimensions, Geometry, [lparen| Tokens0], Tokens) :-
		dimension_arity(Dimensions, Arity),
		wkt_multi_line_string_list_tokens(LineStrings, Arity, Tokens0, [rparen| Tokens]),
		build_geometry(multi_line_string, LineStrings, Dimensions, Geometry).

	wkt_geometry_body_tokens(multi_polygon, Dimensions, Geometry, [id('EMPTY')| Tokens], Tokens) :-
		!,
		build_geometry(multi_polygon, [], Dimensions, Geometry).
	wkt_geometry_body_tokens(multi_polygon, Dimensions, Geometry, [lparen| Tokens0], Tokens) :-
		dimension_arity(Dimensions, Arity),
		wkt_multi_polygon_list_tokens(Polygons, Arity, Tokens0, [rparen| Tokens]),
		build_geometry(multi_polygon, Polygons, Dimensions, Geometry).

	wkt_geometry_body_tokens(geometry_collection, Dimensions, Geometry, [id('EMPTY')| Tokens], Tokens) :-
		!,
		build_geometry(geometry_collection, [], Dimensions, Geometry).
	wkt_geometry_body_tokens(geometry_collection, Dimensions, Geometry, [lparen| Tokens0], Tokens) :-
		wkt_geometry_list_tokens(Geometries, Dimensions, Tokens0, [rparen| Tokens]),
		build_geometry(geometry_collection, Geometries, Dimensions, Geometry).

	wkt_position_tokens([], 0, Tokens, Tokens) :-
		!.
	wkt_position_tokens([Number| Numbers], Arity, [number(Number)| Tokens0], Tokens) :-
		Arity > 0,
		NextArity is Arity - 1,
		wkt_position_tokens(Numbers, NextArity, Tokens0, Tokens).

	wkt_position_list_tokens([Position| Positions], Arity, Tokens0, Tokens) :-
		wkt_position_tokens(Position, Arity, Tokens0, Tokens1),
		wkt_position_list_tail_tokens(Positions, Arity, Tokens1, Tokens).

	wkt_position_list_tail_tokens([Position| Positions], Arity, [comma| Tokens0], Tokens) :-
		wkt_position_tokens(Position, Arity, Tokens0, Tokens1),
		wkt_position_list_tail_tokens(Positions, Arity, Tokens1, Tokens).
	wkt_position_list_tail_tokens([], _Arity, Tokens, Tokens).

	wkt_ring_list_tokens([Ring| Rings], Arity, [lparen| Tokens0], Tokens) :-
		wkt_position_list_tokens(Ring, Arity, Tokens0, [rparen| Tokens1]),
		wkt_ring_list_tail_tokens(Rings, Arity, Tokens1, Tokens).

	wkt_ring_list_tail_tokens([Ring| Rings], Arity, [comma| Tokens0], Tokens) :-
		Tokens0 = [lparen| Tokens1],
		wkt_position_list_tokens(Ring, Arity, Tokens1, [rparen| Tokens2]),
		wkt_ring_list_tail_tokens(Rings, Arity, Tokens2, Tokens).
	wkt_ring_list_tail_tokens([], _Arity, Tokens, Tokens).

	wkt_multi_point_list_tokens([Point| Points], Arity, Tokens0, Tokens) :-
		wkt_multi_point_item_tokens(Point, Arity, Tokens0, Tokens1),
		wkt_multi_point_list_tail_tokens(Points, Arity, Tokens1, Tokens).

	wkt_multi_point_list_tail_tokens([Point| Points], Arity, [comma| Tokens0], Tokens) :-
		wkt_multi_point_item_tokens(Point, Arity, Tokens0, Tokens1),
		wkt_multi_point_list_tail_tokens(Points, Arity, Tokens1, Tokens).
	wkt_multi_point_list_tail_tokens([], _Arity, Tokens, Tokens).

	wkt_multi_point_item_tokens([], _Arity, [id('EMPTY')| Tokens], Tokens) :-
		!.
	wkt_multi_point_item_tokens(Point, Arity, [lparen| Tokens0], Tokens) :-
		!,
		wkt_position_tokens(Point, Arity, Tokens0, [rparen| Tokens]).
	wkt_multi_point_item_tokens(Point, Arity, Tokens0, Tokens) :-
		wkt_position_tokens(Point, Arity, Tokens0, Tokens).

	wkt_multi_line_string_list_tokens([LineString| LineStrings], Arity, Tokens0, Tokens) :-
		wkt_multi_line_string_item_tokens(Arity, LineString, Tokens0, Tokens1),
		wkt_multi_line_string_tail_tokens(LineStrings, Arity, Tokens1, Tokens).

	wkt_multi_line_string_tail_tokens([LineString| LineStrings], Arity, [comma| Tokens0], Tokens) :-
		wkt_multi_line_string_item_tokens(Arity, LineString, Tokens0, Tokens1),
		wkt_multi_line_string_tail_tokens(LineStrings, Arity, Tokens1, Tokens).
	wkt_multi_line_string_tail_tokens([], _Arity, Tokens, Tokens).

	wkt_multi_line_string_item_tokens(_Arity, [], [id('EMPTY')| Tokens], Tokens) :-
		!.
	wkt_multi_line_string_item_tokens(Arity, LineString, [lparen| Tokens0], Tokens) :-
		wkt_position_list_tokens(LineString, Arity, Tokens0, [rparen| Tokens]).

	wkt_multi_polygon_list_tokens([Polygon| Polygons], Arity, Tokens0, Tokens) :-
		wkt_multi_polygon_item_tokens(Polygon, Arity, Tokens0, Tokens1),
		wkt_multi_polygon_tail_tokens(Polygons, Arity, Tokens1, Tokens).

	wkt_multi_polygon_tail_tokens([Polygon| Polygons], Arity, [comma| Tokens0], Tokens) :-
		wkt_multi_polygon_item_tokens(Polygon, Arity, Tokens0, Tokens1),
		wkt_multi_polygon_tail_tokens(Polygons, Arity, Tokens1, Tokens).
	wkt_multi_polygon_tail_tokens([], _Arity, Tokens, Tokens).

	wkt_multi_polygon_item_tokens([], _Arity, [id('EMPTY')| Tokens], Tokens) :-
		!.
	wkt_multi_polygon_item_tokens(Polygon, Arity, [lparen| Tokens0], Tokens) :-
		wkt_ring_list_tokens(Polygon, Arity, Tokens0, [rparen| Tokens]).

	wkt_geometry_list_tokens([Geometry| Geometries], DefaultDimensions, Tokens0, Tokens) :-
		wkt_geometry_tokens(DefaultDimensions, Geometry, Tokens0, Tokens1),
		wkt_geometry_list_tail_tokens(Geometries, DefaultDimensions, Tokens1, Tokens).

	wkt_geometry_list_tail_tokens([Geometry| Geometries], DefaultDimensions, [comma| Tokens0], Tokens) :-
		wkt_geometry_tokens(DefaultDimensions, Geometry, Tokens0, Tokens1),
		wkt_geometry_list_tail_tokens(Geometries, DefaultDimensions, Tokens1, Tokens).
	wkt_geometry_list_tail_tokens([], _DefaultDimensions, Tokens, Tokens).

	wkt_geometry_codes(Geometry) -->
		{
			geometry_term(Geometry, Type, Data, _Options),
			geometry_dimensions(Geometry, Dimensions),
			wkt_type_atom(Type, TypeAtom)
		},
		wkt_header_codes(TypeAtom, Dimensions),
		wkt_geometry_body_codes(Type, Dimensions, Data).

	wkt_header_codes(TypeAtom, xy) -->
		{atom_codes(TypeAtom, Codes)},
		codes(Codes),
		wkt_space_codes.
	wkt_header_codes(TypeAtom, Dimensions) -->
		{
			atom_codes(TypeAtom, TypeCodes),
			dimensions_token_atom(Dimensions, DimensionAtom),
			atom_codes(DimensionAtom, DimensionCodes)
		},
		codes(TypeCodes),
		wkt_space_codes,
		codes(DimensionCodes),
		wkt_space_codes.

	wkt_geometry_body_codes(point, _Dimensions, []) -->
		wkt_empty_codes.
	wkt_geometry_body_codes(point, _Dimensions, Position) -->
		wkt_lparen_codes,
		wkt_position_codes(Position),
		wkt_rparen_codes.

	wkt_geometry_body_codes(line_string, _Dimensions, []) -->
		wkt_empty_codes.
	wkt_geometry_body_codes(line_string, _Dimensions, Positions) -->
		wkt_lparen_codes,
		wkt_position_list_codes(Positions),
		wkt_rparen_codes.

	wkt_geometry_body_codes(polygon, _Dimensions, []) -->
		wkt_empty_codes.
	wkt_geometry_body_codes(polygon, _Dimensions, Rings) -->
		wkt_lparen_codes,
		wkt_ring_list_codes(Rings),
		wkt_rparen_codes.

	wkt_geometry_body_codes(multi_point, _Dimensions, []) -->
		wkt_empty_codes.
	wkt_geometry_body_codes(multi_point, _Dimensions, Points) -->
		wkt_lparen_codes,
		wkt_multi_point_list_codes(Points),
		wkt_rparen_codes.

	wkt_geometry_body_codes(multi_line_string, _Dimensions, []) -->
		wkt_empty_codes.
	wkt_geometry_body_codes(multi_line_string, _Dimensions, LineStrings) -->
		wkt_lparen_codes,
		wkt_multi_line_string_list_codes(LineStrings),
		wkt_rparen_codes.

	wkt_geometry_body_codes(multi_polygon, _Dimensions, []) -->
		wkt_empty_codes.
	wkt_geometry_body_codes(multi_polygon, _Dimensions, Polygons) -->
		wkt_lparen_codes,
		wkt_multi_polygon_list_codes(Polygons),
		wkt_rparen_codes.

	wkt_geometry_body_codes(geometry_collection, _Dimensions, []) -->
		wkt_empty_codes.
	wkt_geometry_body_codes(geometry_collection, _Dimensions, Geometries) -->
		wkt_lparen_codes,
		wkt_geometry_list_codes(Geometries),
		wkt_rparen_codes.

	wkt_position_codes([Coordinate]) -->
		!,
		wkt_number_codes(Coordinate).
	wkt_position_codes([Coordinate| Coordinates]) -->
		wkt_number_codes(Coordinate),
		wkt_space_codes,
		wkt_position_codes(Coordinates).

	wkt_position_list_codes([Position]) -->
		!,
		wkt_position_codes(Position).
	wkt_position_list_codes([Position| Positions]) -->
		wkt_position_codes(Position),
		wkt_comma_space_codes,
		wkt_position_list_codes(Positions).

	wkt_ring_list_codes([Ring]) -->
		!,
		wkt_lparen_codes,
		wkt_position_list_codes(Ring),
		wkt_rparen_codes.
	wkt_ring_list_codes([Ring| Rings]) -->
		wkt_lparen_codes,
		wkt_position_list_codes(Ring),
		wkt_rparen_codes,
		wkt_comma_space_codes,
		wkt_ring_list_codes(Rings).

	wkt_multi_point_list_codes([Point]) -->
		!,
		wkt_multi_point_item_codes(Point).
	wkt_multi_point_list_codes([Point| Points]) -->
		wkt_multi_point_item_codes(Point),
		wkt_comma_space_codes,
		wkt_multi_point_list_codes(Points).

	wkt_multi_point_item_codes([]) -->
		!,
		wkt_empty_codes.
	wkt_multi_point_item_codes(Point) -->
		wkt_lparen_codes,
		wkt_position_codes(Point),
		wkt_rparen_codes.

	wkt_multi_line_string_list_codes([LineString]) -->
		!,
		wkt_multi_line_string_item_codes(LineString).
	wkt_multi_line_string_list_codes([LineString| LineStrings]) -->
		wkt_multi_line_string_item_codes(LineString),
		wkt_comma_space_codes,
		wkt_multi_line_string_list_codes(LineStrings).

	wkt_multi_line_string_item_codes([]) -->
		!,
		wkt_empty_codes.
	wkt_multi_line_string_item_codes(LineString) -->
		wkt_lparen_codes,
		wkt_position_list_codes(LineString),
		wkt_rparen_codes.

	wkt_multi_polygon_list_codes([Polygon]) -->
		!,
		wkt_multi_polygon_item_codes(Polygon).
	wkt_multi_polygon_list_codes([Polygon| Polygons]) -->
		wkt_multi_polygon_item_codes(Polygon),
		wkt_comma_space_codes,
		wkt_multi_polygon_list_codes(Polygons).

	wkt_multi_polygon_item_codes([]) -->
		!,
		wkt_empty_codes.
	wkt_multi_polygon_item_codes(Polygon) -->
		wkt_lparen_codes,
		wkt_ring_list_codes(Polygon),
		wkt_rparen_codes.

	wkt_geometry_list_codes([Geometry]) -->
		!,
		wkt_geometry_codes(Geometry).
	wkt_geometry_list_codes([Geometry| Geometries]) -->
		wkt_geometry_codes(Geometry),
		wkt_comma_space_codes,
		wkt_geometry_list_codes(Geometries).

	wkt_number_codes(Number) -->
		{number_codes(Number, Codes)},
		codes(Codes).

	wkt_space_codes -->
		[32].

	wkt_comma_space_codes -->
		[0',, 32].

	wkt_lparen_codes -->
		[0'(].

	wkt_rparen_codes -->
		[0')].

	wkt_empty_codes -->
		[0'E, 0'M, 0'P, 0'T, 0'Y].

	parse_wkb_bytes(Bytes, Geometry) :-
		wkb_geometry(Bytes, Geometry, []),
		validate(Geometry).

	generate_wkb_bytes(Order, Geometry, Bytes) :-
		phrase(wkb_geometry_codes(Order, Geometry), Bytes).

	wkb_geometry([ByteOrderByte| Bytes0], Geometry, Bytes) :-
		decode_byte_order(ByteOrderByte, Order),
		read_uint32(Order, TypeCode, Bytes0, Bytes1),
		decode_wkb_type(TypeCode, Type, Dimensions),
		wkb_geometry_body(Order, Type, Dimensions, Data, Bytes1, Bytes),
		build_geometry(Type, Data, Dimensions, Geometry).

	wkb_geometry_body(Order, point, Dimensions, Data, Bytes0, Bytes) :-
		dimension_arity(Dimensions, Arity),
		read_coordinates(Coordinates, Order, Arity, Bytes0, Bytes),
		decode_point_coordinates(Coordinates, Data).
	wkb_geometry_body(Order, line_string, Dimensions, Data, Bytes0, Bytes) :-
		read_uint32(Order, Count, Bytes0, Bytes1),
		dimension_arity(Dimensions, Arity),
		read_positions(Data, Order, Count, Arity, Bytes1, Bytes).
	wkb_geometry_body(Order, polygon, Dimensions, Data, Bytes0, Bytes) :-
		read_uint32(Order, Count, Bytes0, Bytes1),
		dimension_arity(Dimensions, Arity),
		read_rings(Data, Order, Count, Arity, Bytes1, Bytes).
	wkb_geometry_body(Order, multi_point, Dimensions, Data, Bytes0, Bytes) :-
		read_uint32(Order, Count, Bytes0, Bytes1),
		read_component_geometries(Data, point, Dimensions, Count, Bytes1, Bytes).
	wkb_geometry_body(Order, multi_line_string, Dimensions, Data, Bytes0, Bytes) :-
		read_uint32(Order, Count, Bytes0, Bytes1),
		read_component_geometries(Data, line_string, Dimensions, Count, Bytes1, Bytes).
	wkb_geometry_body(Order, multi_polygon, Dimensions, Data, Bytes0, Bytes) :-
		read_uint32(Order, Count, Bytes0, Bytes1),
		read_component_geometries(Data, polygon, Dimensions, Count, Bytes1, Bytes).
	wkb_geometry_body(Order, geometry_collection, _Dimensions, Geometries, Bytes0, Bytes) :-
		read_uint32(Order, Count, Bytes0, Bytes1),
		read_geometries(Geometries, Count, Bytes1, Bytes).

	decode_point_coordinates(Coordinates, []) :-
		all_nan(Coordinates),
		!.
	decode_point_coordinates(Coordinates, Coordinates).

	read_uint32(Order, Value, Bytes0, Bytes) :-
		take(4, Bytes0, RawBytes, Bytes),
		canonical_order_bytes(Order, RawBytes, Bytes1),
		bytes_to_unsigned_integer(Bytes1, Value).

	read_coordinates([], _Order, 0, Bytes, Bytes) :-
		!.
	read_coordinates([Coordinate| Coordinates], Order, Count, Bytes0, Bytes) :-
		Count > 0,
		read_double(Order, Coordinate, Bytes0, Bytes1),
		NextCount is Count - 1,
		read_coordinates(Coordinates, Order, NextCount, Bytes1, Bytes).

	read_positions([], _Order, 0, _Arity, Bytes, Bytes) :-
		!.
	read_positions([Position| Positions], Order, Count, Arity, Bytes0, Bytes) :-
		Count > 0,
		read_coordinates(Position, Order, Arity, Bytes0, Bytes1),
		NextCount is Count - 1,
		read_positions(Positions, Order, NextCount, Arity, Bytes1, Bytes).

	read_rings([], _Order, 0, _Arity, Bytes, Bytes) :-
		!.
	read_rings([Ring| Rings], Order, Count, Arity, Bytes0, Bytes) :-
		Count > 0,
		read_uint32(Order, RingCount, Bytes0, Bytes1),
		read_positions(Ring, Order, RingCount, Arity, Bytes1, Bytes2),
		NextCount is Count - 1,
		read_rings(Rings, Order, NextCount, Arity, Bytes2, Bytes).

	read_component_geometries([], _Type, _Dimensions, 0, Bytes, Bytes) :-
		!.
	read_component_geometries([Data| DataList], Type, Dimensions, Count, Bytes0, Bytes) :-
		Count > 0,
		wkb_geometry(Bytes0, Geometry, Bytes1),
		geometry_term(Geometry, Type, Data, _Options),
		geometry_dimensions(Geometry, GeometryDimensions),
		GeometryDimensions == Dimensions,
		NextCount is Count - 1,
		read_component_geometries(DataList, Type, Dimensions, NextCount, Bytes1, Bytes).

	read_geometries([], 0, Bytes, Bytes).
	read_geometries([Geometry| Geometries], Count, Bytes0, Bytes) :-
		Count > 0,
		wkb_geometry(Bytes0, Geometry, Bytes1),
		NextCount is Count - 1,
		read_geometries(Geometries, NextCount, Bytes1, Bytes).

	read_double(Order, Value, Bytes0, Bytes) :-
		take(8, Bytes0, RawBytes, Bytes),
		canonical_order_bytes(Order, RawBytes, Bytes1),
		decode_ieee754_double(Bytes1, Value).

	wkb_geometry_codes(Order, Geometry) -->
		{
			geometry_term(Geometry, Type, Data, _Options),
			geometry_dimensions(Geometry, Dimensions),
			encode_wkb_type(Type, Dimensions, TypeCode),
			encode_byte_order(Order, ByteOrderByte),
			encode_uint32_bytes(Order, TypeCode, TypeBytes)
		},
		[ByteOrderByte],
		bytes(TypeBytes),
		wkb_geometry_body_codes(Order, Type, Dimensions, Data).

	wkb_geometry_body_codes(Order, point, Dimensions, []) -->
		{dimension_arity(Dimensions, Arity)},
		empty_point_codes(Order, Arity).
	wkb_geometry_body_codes(Order, point, _Dimensions, Position) -->
		coordinate_list_codes(Position, Order).

	wkb_geometry_body_codes(Order, line_string, Dimensions, Positions) -->
		{length(Positions, Count), encode_uint32_bytes(Order, Count, CountBytes), dimension_arity(Dimensions, Arity)},
		bytes(CountBytes),
		position_list_codes(Positions, Order, Arity).

	wkb_geometry_body_codes(Order, polygon, Dimensions, Rings) -->
		{length(Rings, Count), encode_uint32_bytes(Order, Count, CountBytes), dimension_arity(Dimensions, Arity)},
		bytes(CountBytes),
		ring_list_codes(Rings, Order, Arity).

	wkb_geometry_body_codes(Order, multi_point, Dimensions, Points) -->
		{length(Points, Count), encode_uint32_bytes(Order, Count, CountBytes)},
		bytes(CountBytes),
		component_geometry_list_codes(Points, Order, point, Dimensions).

	wkb_geometry_body_codes(Order, multi_line_string, Dimensions, LineStrings) -->
		{length(LineStrings, Count), encode_uint32_bytes(Order, Count, CountBytes)},
		bytes(CountBytes),
		component_geometry_list_codes(LineStrings, Order, line_string, Dimensions).

	wkb_geometry_body_codes(Order, multi_polygon, Dimensions, Polygons) -->
		{length(Polygons, Count), encode_uint32_bytes(Order, Count, CountBytes)},
		bytes(CountBytes),
		component_geometry_list_codes(Polygons, Order, polygon, Dimensions).

	wkb_geometry_body_codes(Order, geometry_collection, _Dimensions, Geometries) -->
		{length(Geometries, Count), encode_uint32_bytes(Order, Count, CountBytes)},
		bytes(CountBytes),
		geometry_list_codes(Geometries, Order).

	empty_point_codes(_Order, 0) -->
		[].
	empty_point_codes(Order, Arity) -->
		{Arity > 0, nan_bytes(Order, Bytes)},
		bytes(Bytes),
		{NextArity is Arity - 1},
		empty_point_codes(Order, NextArity).

	coordinate_list_codes([], _Order) -->
		[].
	coordinate_list_codes([Coordinate| Coordinates], Order) -->
		{coordinate_bytes(Order, Coordinate, Bytes)},
		bytes(Bytes),
		coordinate_list_codes(Coordinates, Order).

	position_list_codes([], _Order, _Arity) -->
		[].
	position_list_codes([Position| Positions], Order, Arity) -->
		coordinate_list_codes(Position, Order),
		position_list_codes(Positions, Order, Arity).

	ring_list_codes([], _Order, _Arity) -->
		[].
	ring_list_codes([Ring| Rings], Order, Arity) -->
		{length(Ring, Count), encode_uint32_bytes(Order, Count, CountBytes)},
		bytes(CountBytes),
		position_list_codes(Ring, Order, Arity),
		ring_list_codes(Rings, Order, Arity).

	component_geometry_list_codes([], _Order, _Type, _Dimensions) -->
		[].
	component_geometry_list_codes([Data| DataList], Order, Type, Dimensions) -->
		{build_geometry(Type, Data, Dimensions, Geometry)},
		wkb_geometry_codes(Order, Geometry),
		component_geometry_list_codes(DataList, Order, Type, Dimensions).

	geometry_list_codes([], _Order) -->
		[].
	geometry_list_codes([Geometry| Geometries], Order) -->
		wkb_geometry_codes(Order, Geometry),
		geometry_list_codes(Geometries, Order).

	validate_geometry(Geometry, Path, Errors) :-
		reverse(Path, RevPath),
		validate_geometry_rev(Geometry, RevPath, Errors, []).

	same_arities([], _Arity).
	same_arities([Arity| Arities], Arity0) :-
		Arity =:= Arity0,
		same_arities(Arities, Arity0).

	validate_geometry_rev(Geometry, RevPath, Errors, Tail) :-
		(	geometry_term(Geometry, Type, Data, Options) ->
			validate_options_rev(Options, RevPath, Dimensions, Errors, Errors1),
			validate_geometry_data_rev(Type, Data, Dimensions, RevPath, Errors1, Tail)
		;	path_from_rev(RevPath, Path),
			Errors = [invalid_geometry_term(Path)| Tail]
		).

	validate_geometry_data_rev(point, Data, Dimensions, RevPath, Errors, Tail) :-
		validate_point_data_rev(Data, RevPath, Arity, Errors, Errors1),
		coordinates_rev_path(RevPath, CoordinateRevPath),
		validate_dimensions_match_rev(Dimensions, Arity, CoordinateRevPath, Errors1, Tail).
	validate_geometry_data_rev(line_string, Data, Dimensions, RevPath, Errors, Tail) :-
		validate_line_string_data_rev(Data, 2, RevPath, Arity, Errors, Errors1),
		coordinates_rev_path(RevPath, CoordinateRevPath),
		validate_dimensions_match_rev(Dimensions, Arity, CoordinateRevPath, Errors1, Tail).
	validate_geometry_data_rev(polygon, Data, Dimensions, RevPath, Errors, Tail) :-
		validate_polygon_data_rev(Data, RevPath, Arity, Errors, Errors1),
		coordinates_rev_path(RevPath, CoordinateRevPath),
		validate_dimensions_match_rev(Dimensions, Arity, CoordinateRevPath, Errors1, Tail).
	validate_geometry_data_rev(multi_point, Data, Dimensions, RevPath, Errors, Tail) :-
		validate_multi_point_data_rev(Data, RevPath, Arity, Errors, Errors1),
		coordinates_rev_path(RevPath, CoordinateRevPath),
		validate_dimensions_match_rev(Dimensions, Arity, CoordinateRevPath, Errors1, Tail).
	validate_geometry_data_rev(multi_line_string, Data, Dimensions, RevPath, Errors, Tail) :-
		validate_multi_line_string_data_rev(Data, RevPath, Arity, Errors, Errors1),
		coordinates_rev_path(RevPath, CoordinateRevPath),
		validate_dimensions_match_rev(Dimensions, Arity, CoordinateRevPath, Errors1, Tail).
	validate_geometry_data_rev(multi_polygon, Data, Dimensions, RevPath, Errors, Tail) :-
		validate_multi_polygon_data_rev(Data, RevPath, Arity, Errors, Errors1),
		coordinates_rev_path(RevPath, CoordinateRevPath),
		validate_dimensions_match_rev(Dimensions, Arity, CoordinateRevPath, Errors1, Tail).
	validate_geometry_data_rev(geometry_collection, Data, Dimensions, RevPath, Errors, Tail) :-
		validate_geometry_collection_data_rev(Data, RevPath, Arity, Errors, Errors1),
		geometries_rev_path(RevPath, GeometriesRevPath),
		validate_dimensions_match_rev(Dimensions, Arity, GeometriesRevPath, Errors1, Tail).

	validate_options_rev(Options, RevPath, none, Errors, Tail) :-
		(	var(Options) ; \+ is_list(Options) ),
		!,
		options_rev_path(RevPath, OptionsRevPath),
		path_from_rev(OptionsRevPath, OptionsPath),
		Errors = [invalid_options(OptionsPath)| Tail].
	validate_options_rev(Options, RevPath, Dimensions, Errors, Tail) :-
		options_rev_path(RevPath, OptionsRevPath),
		scan_options_rev(Options, OptionsRevPath, false, none, Dimensions, Errors, Tail).

	scan_options_rev([], _OptionsRevPath, _SeenDimensions, Dimensions, Dimensions, Errors, Errors).
	scan_options_rev([Option| Options], OptionsRevPath, SeenDimensions, Dimensions0, Dimensions, Errors, Tail) :-
		scan_option_rev(Option, OptionsRevPath, SeenDimensions, NextSeenDimensions, Dimensions0, Dimensions1, Errors, Errors1),
		scan_options_rev(Options, OptionsRevPath, NextSeenDimensions, Dimensions1, Dimensions, Errors1, Tail).

	scan_option_rev(dimensions(Dimension), OptionsRevPath, SeenDimensions, true, Dimensions0, Dimensions, Errors, Tail) :-
		!,
		dimensions_option_rev_path(OptionsRevPath, DimensionRevPath),
		(	valid_dimensions(Dimension) ->
			(	Dimensions0 == none ->
				Dimensions = Dimension
			;	Dimensions = Dimensions0
			),
			(	SeenDimensions == true ->
				path_from_rev(OptionsRevPath, OptionsPath),
				Errors = [duplicate_option(dimensions, OptionsPath)| Tail]
			;	Errors = Tail
			)
		;	Dimensions = Dimensions0,
			(	SeenDimensions == true ->
				path_from_rev(OptionsRevPath, OptionsPath),
				path_from_rev(DimensionRevPath, DimensionPath),
				Errors = [duplicate_option(dimensions, OptionsPath), invalid_dimensions(DimensionPath)| Tail]
			;	path_from_rev(DimensionRevPath, DimensionPath),
				Errors = [invalid_dimensions(DimensionPath)| Tail]
			)
		).
	scan_option_rev(Option, OptionsRevPath, SeenDimensions, SeenDimensions, Dimensions, Dimensions, Errors, Tail) :-
		path_from_rev(OptionsRevPath, OptionsPath),
		Errors = [unknown_option(Option, OptionsPath)| Tail].

	validate_point_data_rev([], _RevPath, none, Errors, Errors) :-
		!.
	validate_point_data_rev(Position, RevPath, Arity, Errors, Tail) :-
		coordinates_rev_path(RevPath, CoordinateRevPath),
		validate_position_rev(Position, CoordinateRevPath, Arity, Errors, Tail).

	validate_line_string_data_rev([], _Minimum, _RevPath, none, Errors, Errors) :-
		!.
	validate_line_string_data_rev(Data, Minimum, RevPath, Arity, Errors, Tail) :-
		coordinates_rev_path(RevPath, CoordinateRevPath),
		validate_line_string_coordinates_rev(Data, Minimum, CoordinateRevPath, Arity, Errors, Tail).

	validate_polygon_data_rev([], _RevPath, none, Errors, Errors) :-
		!.
	validate_polygon_data_rev(Data, RevPath, Arity, Errors, Tail) :-
		coordinates_rev_path(RevPath, CoordinateRevPath),
		validate_polygon_coordinates_rev(Data, CoordinateRevPath, Arity, Errors, Tail).

	validate_multi_point_data_rev([], _RevPath, none, Errors, Errors) :-
		!.
	validate_multi_point_data_rev(Data, RevPath, Arity, Errors, Tail) :-
		coordinates_rev_path(RevPath, CoordinateRevPath),
		(	is_list(Data) ->
			validate_multi_points_rev(Data, CoordinateRevPath, PointArities, Errors, Errors1),
			consistent_arities_rev(PointArities, CoordinateRevPath, Arity, Errors1, Tail)
		;	path_from_rev(CoordinateRevPath, CoordinatePath),
			Errors = [invalid_position_array(CoordinatePath)| Tail],
			Arity = none
		).

	validate_multi_line_string_data_rev([], _RevPath, none, Errors, Errors) :-
		!.
	validate_multi_line_string_data_rev(Data, RevPath, Arity, Errors, Tail) :-
		coordinates_rev_path(RevPath, CoordinateRevPath),
		(	is_list(Data) ->
			validate_multi_line_strings_rev(Data, CoordinateRevPath, LineArities, Errors, Errors1),
			consistent_arities_rev(LineArities, CoordinateRevPath, Arity, Errors1, Tail)
		;	path_from_rev(CoordinateRevPath, CoordinatePath),
			Errors = [invalid_position_array(CoordinatePath)| Tail],
			Arity = none
		).

	validate_multi_polygon_data_rev([], _RevPath, none, Errors, Errors) :-
		!.
	validate_multi_polygon_data_rev(Data, RevPath, Arity, Errors, Tail) :-
		coordinates_rev_path(RevPath, CoordinateRevPath),
		(	is_list(Data) ->
			validate_multi_polygons_rev(Data, CoordinateRevPath, PolygonArities, Errors, Errors1),
			consistent_arities_rev(PolygonArities, CoordinateRevPath, Arity, Errors1, Tail)
		;	path_from_rev(CoordinateRevPath, CoordinatePath),
			Errors = [invalid_multi_polygon(CoordinatePath)| Tail],
			Arity = none
		).

	validate_geometry_collection_data_rev([], _RevPath, none, Errors, Errors) :-
		!.
	validate_geometry_collection_data_rev(Data, RevPath, Arity, Errors, Tail) :-
		geometries_rev_path(RevPath, GeometriesRevPath),
		(	is_list(Data) ->
			validate_geometries_rev(Data, GeometriesRevPath, GeometryArities, Errors, Errors1),
			consistent_arities_rev(GeometryArities, GeometriesRevPath, Arity, Errors1, Tail)
		;	path_from_rev(GeometriesRevPath, GeometriesPath),
			Errors = [invalid_geometry_collection(GeometriesPath)| Tail],
			Arity = none
		).

	validate_rings_rev(Rings, CoordinateRevPath, Arities, Errors, Tail) :-
		validate_rings_rev(Rings, CoordinateRevPath, 0, Arities, Errors, Tail).

	validate_rings_rev([], _CoordinateRevPath, _Index, [], Errors, Errors).
	validate_rings_rev([Ring| Rings], CoordinateRevPath, Index, Arities, Errors, Tail) :-
		indexed_rev_path(CoordinateRevPath, Index, RingRevPath),
		validate_ring_rev(Ring, RingRevPath, RingArity, Errors, Errors1),
		NextIndex is Index + 1,
		validate_rings_rev(Rings, CoordinateRevPath, NextIndex, RestArities, Errors1, Tail),
		(	RingArity == none -> Arities = RestArities ; Arities = [RingArity| RestArities] ).

	validate_ring_rev(Ring, RingRevPath, Arity, Errors, Tail) :-
		(	is_list(Ring) ->
			validate_positions_rev(Ring, RingRevPath, Count, Arity, Errors, Errors1),
			(	Count < 4 ->
				path_from_rev(RingRevPath, RingPath),
				Errors1 = [insufficient_positions(4, RingPath)| Errors2]
			;	Errors1 = Errors2
			),
			(	Errors == Errors1, Count >= 4, Ring = [First| _], last(Ring, Last), \+ coordinates_equal(First, Last) ->
				path_from_rev(RingRevPath, RingPath),
				Errors2 = [ring_not_closed(RingPath)| Tail]
			;	Errors2 = Tail
			)
		;	path_from_rev(RingRevPath, RingPath),
			Errors = [invalid_polygon(RingPath)| Tail],
			Arity = none
		).

	validate_multi_points_rev(Points, CoordinateRevPath, Arities, Errors, Tail) :-
		validate_multi_points_rev(Points, CoordinateRevPath, 0, Arities, Errors, Tail).

	validate_multi_points_rev([], _CoordinateRevPath, _Index, [], Errors, Errors).
	validate_multi_points_rev([Point| Points], CoordinateRevPath, Index, Arities, Errors, Tail) :-
		indexed_rev_path(CoordinateRevPath, Index, PointRevPath),
		validate_multi_point_rev(Point, PointRevPath, PointArity, Errors, Errors1),
		NextIndex is Index + 1,
		validate_multi_points_rev(Points, CoordinateRevPath, NextIndex, RestArities, Errors1, Tail),
		(	PointArity == none ->
			Arities = RestArities
		;	Arities = [PointArity| RestArities]
		).

	validate_multi_point_rev([], _PointRevPath, none, Errors, Errors) :-
		!.
	validate_multi_point_rev(Point, PointRevPath, Arity, Errors, Tail) :-
		validate_position_rev(Point, PointRevPath, Arity, Errors, Tail).

	validate_multi_line_strings_rev(LineStrings, CoordinateRevPath, Arities, Errors, Tail) :-
		validate_multi_line_strings_rev(LineStrings, CoordinateRevPath, 0, Arities, Errors, Tail).

	validate_multi_line_strings_rev([], _CoordinateRevPath, _Index, [], Errors, Errors).
	validate_multi_line_strings_rev([[]| LineStrings], CoordinateRevPath, Index, Arities, Errors, Tail) :-
		!,
		NextIndex is Index + 1,
		validate_multi_line_strings_rev(LineStrings, CoordinateRevPath, NextIndex, Arities, Errors, Tail).
	validate_multi_line_strings_rev([LineString| LineStrings], CoordinateRevPath, Index, Arities, Errors, Tail) :-
		indexed_rev_path(CoordinateRevPath, Index, LineStringRevPath),
		validate_line_string_coordinates_rev(LineString, 2, LineStringRevPath, LineArity, Errors, Errors1),
		NextIndex is Index + 1,
		validate_multi_line_strings_rev(LineStrings, CoordinateRevPath, NextIndex, RestArities, Errors1, Tail),
		(	LineArity == none -> Arities = RestArities ; Arities = [LineArity| RestArities] ).

	validate_multi_polygons_rev(Polygons, CoordinateRevPath, Arities, Errors, Tail) :-
		validate_multi_polygons_rev(Polygons, CoordinateRevPath, 0, Arities, Errors, Tail).

	validate_multi_polygons_rev([], _CoordinateRevPath, _Index, [], Errors, Errors).
	validate_multi_polygons_rev([Polygon| Polygons], CoordinateRevPath, Index, Arities, Errors, Tail) :-
		indexed_rev_path(CoordinateRevPath, Index, PolygonRevPath),
		validate_polygon_coordinates_rev(Polygon, PolygonRevPath, PolygonArity, Errors, Errors1),
		NextIndex is Index + 1,
		validate_multi_polygons_rev(Polygons, CoordinateRevPath, NextIndex, RestArities, Errors1, Tail),
		(	PolygonArity == none -> Arities = RestArities ; Arities = [PolygonArity| RestArities] ).

	validate_line_string_coordinates_rev(Data, Minimum, CoordinateRevPath, Arity, Errors, Tail) :-
		(	is_list(Data) ->
			validate_positions_rev(Data, CoordinateRevPath, Count, Arity, Errors, Errors1),
			(	Count < Minimum ->
				path_from_rev(CoordinateRevPath, CoordinatePath),
				Errors1 = [insufficient_positions(Minimum, CoordinatePath)| Tail]
			;	Errors1 = Tail
			)
		;	path_from_rev(CoordinateRevPath, CoordinatePath),
			Errors = [invalid_position_array(CoordinatePath)| Tail],
			Arity = none
		).

	validate_polygon_coordinates_rev(Data, CoordinateRevPath, Arity, Errors, Tail) :-
		(	is_list(Data) ->
			validate_rings_rev(Data, CoordinateRevPath, RingArities, Errors, Errors1),
			consistent_arities_rev(RingArities, CoordinateRevPath, Arity, Errors1, Tail)
		;	path_from_rev(CoordinateRevPath, CoordinatePath),
			Errors = [invalid_polygon(CoordinatePath)| Tail],
			Arity = none
		).

	validate_geometries_rev(Geometries, GeometriesRevPath, Arities, Errors, Tail) :-
		validate_geometries_rev(Geometries, GeometriesRevPath, 0, Arities, Errors, Tail).

	validate_geometries_rev([], _GeometriesRevPath, _Index, [], Errors, Errors).
	validate_geometries_rev([Geometry| Geometries], GeometriesRevPath, Index, Arities, Errors, Tail) :-
		indexed_rev_path(GeometriesRevPath, Index, GeometryRevPath),
		validate_geometry_rev(Geometry, GeometryRevPath, Errors, Errors1),
		geometry_term_arity(Geometry, GeometryArity),
		NextIndex is Index + 1,
		validate_geometries_rev(Geometries, GeometriesRevPath, NextIndex, RestArities, Errors1, Tail),
		(	GeometryArity == none ->
			Arities = RestArities
		;	Arities = [GeometryArity| RestArities]
		).

	validate_positions_rev(Data, CoordinateRevPath, Count, Arity, Errors, Tail) :-
		validate_positions_rev(Data, CoordinateRevPath, 0, Count, Arities, Errors, Errors1),
		consistent_arities_rev(Arities, CoordinateRevPath, Arity, Errors1, Tail).

	validate_positions_rev([], _CoordinateRevPath, _Index, 0, [], Errors, Errors).
	validate_positions_rev([Position| Positions], CoordinateRevPath, Index, Count, Arities, Errors, Tail) :-
		indexed_rev_path(CoordinateRevPath, Index, PositionRevPath),
		validate_position_rev(Position, PositionRevPath, PositionArity, Errors, Errors1),
		NextIndex is Index + 1,
		validate_positions_rev(Positions, CoordinateRevPath, NextIndex, RestCount, RestArities, Errors1, Tail),
		Count is RestCount + 1,
		(	PositionArity == none ->
			Arities = RestArities
		;	Arities = [PositionArity| RestArities]
		).

	validate_position_rev(Position, _PositionRevPath, Arity, Errors, Tail) :-
		valid_position_arity(Position, Arity),
		!,
		Errors = Tail.
	validate_position_rev(_Position, PositionRevPath, none, Errors, Tail) :-
		path_from_rev(PositionRevPath, PositionPath),
		Errors = [invalid_position(PositionPath)| Tail].

	consistent_arities_rev([], _RevPath, none, Errors, Errors).
	consistent_arities_rev([Arity| Arities], RevPath, Arity, Errors, Tail) :-
		(	same_arities(Arities, Arity) ->
			Errors = Tail
		;	path_from_rev(RevPath, Path),
			Errors = [mixed_coordinate_dimension(Path)| Tail]
		).

	validate_dimensions_match_rev(none, _Arity, _RevPath, Errors, Errors) :-
		!.
	validate_dimensions_match_rev(_Dimensions, none, _RevPath, Errors, Errors) :-
		!.
	validate_dimensions_match_rev(Dimensions, Arity, RevPath, Errors, Tail) :-
		dimension_arity(Dimensions, Expected),
		(	Arity =\= Expected ->
			path_from_rev(RevPath, Path),
			Errors = [coordinate_dimension_mismatch(Arity, Expected, Path)| Tail]
		;	Errors = Tail
		).

	options_rev_path(RevPath, [options| RevPath]).

	coordinates_rev_path(RevPath, [coordinates| RevPath]).

	geometries_rev_path(RevPath, [geometries| RevPath]).

	dimensions_option_rev_path(OptionsRevPath, [dimensions| OptionsRevPath]).

	indexed_rev_path(RevPath, Index, [Index| RevPath]).

	path_from_rev(RevPath, Path) :-
		reverse(RevPath, Path).

	valid_position_arity(Position, Arity) :-
		is_list(Position),
		coordinate_list_arity(Position, 0, Arity),
		Arity >= 2,
		Arity =< 4.

	coordinate_list_arity([], Arity, Arity).
	coordinate_list_arity([Coordinate| Coordinates], Arity0, Arity) :-
		valid_coordinate(Coordinate),
		Arity1 is Arity0 + 1,
		coordinate_list_arity(Coordinates, Arity1, Arity).

	valid_coordinate(Coordinate) :-
		number(Coordinate),
		!.
	valid_coordinate(Coordinate) :-
		special_coordinate(Coordinate),
		!.

	special_coordinate(@infinity).
	special_coordinate(@negative_infinity).
	special_coordinate(Coordinate) :-
		nan_coordinate(Coordinate).

	geometry_has_special_coordinates(Geometry) :-
		geometry_term(Geometry, Type, Data, _Options),
		geometry_data_has_special_coordinates(Type, Data).

	geometry_data_has_special_coordinates(point, Position) :-
		position_has_special_coordinates(Position).
	geometry_data_has_special_coordinates(multi_point, Positions) :-
		positions_have_special_coordinates(Positions).
	geometry_data_has_special_coordinates(line_string, Positions) :-
		positions_have_special_coordinates(Positions).
	geometry_data_has_special_coordinates(multi_line_string, LineStrings) :-
		line_strings_have_special_coordinates(LineStrings).
	geometry_data_has_special_coordinates(polygon, Rings) :-
		line_strings_have_special_coordinates(Rings).
	geometry_data_has_special_coordinates(multi_polygon, Polygons) :-
		polygons_have_special_coordinates(Polygons).
	geometry_data_has_special_coordinates(geometry_collection, Geometries) :-
		geometries_have_special_coordinates(Geometries).

	positions_have_special_coordinates([Position| _]) :-
		position_has_special_coordinates(Position),
		!.
	positions_have_special_coordinates([_| Positions]) :-
		positions_have_special_coordinates(Positions).

	line_strings_have_special_coordinates([LineString| _]) :-
		positions_have_special_coordinates(LineString),
		!.
	line_strings_have_special_coordinates([_| LineStrings]) :-
		line_strings_have_special_coordinates(LineStrings).

	polygons_have_special_coordinates([Polygon| _]) :-
		line_strings_have_special_coordinates(Polygon),
		!.
	polygons_have_special_coordinates([_| Polygons]) :-
		polygons_have_special_coordinates(Polygons).

	geometries_have_special_coordinates([Geometry| _]) :-
		geometry_has_special_coordinates(Geometry),
		!.
	geometries_have_special_coordinates([_| Geometries]) :-
		geometries_have_special_coordinates(Geometries).

	position_has_special_coordinates([Coordinate| _]) :-
		special_coordinate(Coordinate),
		!.
	position_has_special_coordinates([_| Coordinates]) :-
		position_has_special_coordinates(Coordinates).

	coordinates_equal(Coordinates1, Coordinates2) :-
		coordinates_equal(Coordinates1, Coordinates2, true).

	coordinates_equal([], [], true).
	coordinates_equal([Coordinate1| Coordinates1], [Coordinate2| Coordinates2], true) :-
		coordinate_equal(Coordinate1, Coordinate2),
		coordinates_equal(Coordinates1, Coordinates2, true).

	coordinate_equal(Coordinate1, Coordinate2) :-
		nan_coordinate(Coordinate1),
		nan_coordinate(Coordinate2),
		canonical_nan_coordinate_bytes(Coordinate1, CanonicalBytes),
		canonical_nan_coordinate_bytes(Coordinate2, CanonicalBytes).
	coordinate_equal(Coordinate1, Coordinate2) :-
		\+ nan_coordinate(Coordinate1),
		\+ nan_coordinate(Coordinate2),
		Coordinate1 =:= Coordinate2.

	geometry_term(point(Position), point, Position, []).
	geometry_term(point(Position, Options), point, Position, Options).
	geometry_term(multi_point(Positions), multi_point, Positions, []).
	geometry_term(multi_point(Positions, Options), multi_point, Positions, Options).
	geometry_term(line_string(Positions), line_string, Positions, []).
	geometry_term(line_string(Positions, Options), line_string, Positions, Options).
	geometry_term(multi_line_string(LineStrings), multi_line_string, LineStrings, []).
	geometry_term(multi_line_string(LineStrings, Options), multi_line_string, LineStrings, Options).
	geometry_term(polygon(Rings), polygon, Rings, []).
	geometry_term(polygon(Rings, Options), polygon, Rings, Options).
	geometry_term(multi_polygon(Polygons), multi_polygon, Polygons, []).
	geometry_term(multi_polygon(Polygons, Options), multi_polygon, Polygons, Options).
	geometry_term(geometry_collection(Geometries), geometry_collection, Geometries, []).
	geometry_term(geometry_collection(Geometries, Options), geometry_collection, Geometries, Options).

	build_geometry(Type, Data, xy, Geometry) :-
		geometry_term(Geometry, Type, Data, []),
		!.
	build_geometry(Type, Data, Dimensions, Geometry) :-
		geometry_term(Geometry, Type, Data, [dimensions(Dimensions)]).

	geometry_option_dimensions(Options, Dimensions) :-
		memberchk(dimensions(Dimensions), Options),
		valid_dimensions(Dimensions).

	geometry_dimensions(Geometry, Dimensions) :-
		geometry_term(Geometry, Type, Data, Options),
		( geometry_option_dimensions(Options, Dimensions) ->
			true
		; geometry_data_arity(Type, Data, Arity),
			( Arity == none -> Dimensions = xy ; arity_default_dimensions(Arity, Dimensions) )
		).

	geometry_term_arity(Geometry, Arity) :-
		( geometry_term(Geometry, Type, Data, _Options) -> geometry_data_arity(Type, Data, Arity) ; Arity = none ).

	geometry_data_arity(point, [], none) :-
		!.
	geometry_data_arity(point, Position, Arity) :-
		position_arity(Position, Arity).
	geometry_data_arity(multi_point, Positions, Arity) :-
		first_position_arity(Positions, Arity).
	geometry_data_arity(line_string, Positions, Arity) :-
		first_position_arity(Positions, Arity).
	geometry_data_arity(multi_line_string, LineStrings, Arity) :-
		first_line_string_arity(LineStrings, Arity).
	geometry_data_arity(polygon, Rings, Arity) :-
		first_line_string_arity(Rings, Arity).
	geometry_data_arity(multi_polygon, Polygons, Arity) :-
		first_polygon_arity(Polygons, Arity).
	geometry_data_arity(geometry_collection, Geometries, Arity) :-
		first_geometry_arity(Geometries, Arity).

	position_arity(Position, Arity) :-
		valid_position_arity(Position, Arity).

	first_position_arity([], none).
	first_position_arity([[]| Positions], Arity) :-
		!,
		first_position_arity(Positions, Arity).
	first_position_arity([Position| _], Arity) :-
		position_arity(Position, Arity),
		!.

	first_line_string_arity([], none).
	first_line_string_arity([[]| LineStrings], Arity) :-
		!,
		first_line_string_arity(LineStrings, Arity).
	first_line_string_arity([LineString| _], Arity) :-
		first_position_arity(LineString, Arity),
		Arity \== none,
		!.
	first_line_string_arity([_| LineStrings], Arity) :-
		first_line_string_arity(LineStrings, Arity).

	first_polygon_arity([], none).
	first_polygon_arity([[]| Polygons], Arity) :-
		!,
		first_polygon_arity(Polygons, Arity).
	first_polygon_arity([Polygon| _], Arity) :-
		first_line_string_arity(Polygon, Arity),
		Arity \== none,
		!.
	first_polygon_arity([_| Polygons], Arity) :-
		first_polygon_arity(Polygons, Arity).

	first_geometry_arity([], none).
	first_geometry_arity([Geometry| Geometries], Arity) :-
		geometry_term_arity(Geometry, GeometryArity),
		( GeometryArity == none -> first_geometry_arity(Geometries, Arity) ; Arity = GeometryArity ).

	valid_dimensions(xy).
	valid_dimensions(z).
	valid_dimensions(m).
	valid_dimensions(zm).

	dimension_arity(xy, 2).
	dimension_arity(z, 3).
	dimension_arity(m, 3).
	dimension_arity(zm, 4).

	arity_default_dimensions(2, xy).
	arity_default_dimensions(3, z).
	arity_default_dimensions(4, zm).

	dimension_token('Z', z).
	dimension_token('M', m).
	dimension_token('ZM', zm).

	dimensions_token_atom(z, 'Z').
	dimensions_token_atom(m, 'M').
	dimensions_token_atom(zm, 'ZM').

	wkt_type_token('POINT', point).
	wkt_type_token('LINESTRING', line_string).
	wkt_type_token('POLYGON', polygon).
	wkt_type_token('MULTIPOINT', multi_point).
	wkt_type_token('MULTILINESTRING', multi_line_string).
	wkt_type_token('MULTIPOLYGON', multi_polygon).
	wkt_type_token('GEOMETRYCOLLECTION', geometry_collection).

	wkt_type_atom(point, 'POINT').
	wkt_type_atom(line_string, 'LINESTRING').
	wkt_type_atom(polygon, 'POLYGON').
	wkt_type_atom(multi_point, 'MULTIPOINT').
	wkt_type_atom(multi_line_string, 'MULTILINESTRING').
	wkt_type_atom(multi_polygon, 'MULTIPOLYGON').
	wkt_type_atom(geometry_collection, 'GEOMETRYCOLLECTION').

	wkb_type_code(point, 1).
	wkb_type_code(line_string, 2).
	wkb_type_code(polygon, 3).
	wkb_type_code(multi_point, 4).
	wkb_type_code(multi_line_string, 5).
	wkb_type_code(multi_polygon, 6).
	wkb_type_code(geometry_collection, 7).

	decode_wkb_type(TypeCode, Type, Dimensions) :-
		( TypeCode >= 3000 ->
			BaseTypeCode is TypeCode - 3000,
			Dimensions = zm
		; TypeCode >= 2000 ->
			BaseTypeCode is TypeCode - 2000,
			Dimensions = m
		; TypeCode >= 1000 ->
			BaseTypeCode is TypeCode - 1000,
			Dimensions = z
		; BaseTypeCode = TypeCode,
			Dimensions = xy
		),
		wkb_type_code(Type, BaseTypeCode).

	encode_wkb_type(Type, Dimensions, TypeCode) :-
		wkb_type_code(Type, BaseTypeCode),
		( Dimensions == zm -> TypeCode is BaseTypeCode + 3000
		; Dimensions == m -> TypeCode is BaseTypeCode + 2000
		; Dimensions == z -> TypeCode is BaseTypeCode + 1000
		; TypeCode = BaseTypeCode
		).

	decode_byte_order(0, big).
	decode_byte_order(1, little).

	encode_byte_order(big, 0).
	encode_byte_order(little, 1).

	encode_uint32_bytes(big, Integer, Bytes) :-
		integer_to_bytes(4, Integer, Bytes).
	encode_uint32_bytes(little, Integer, Bytes) :-
		integer_to_bytes(4, Integer, BigEndianBytes),
		reverse(BigEndianBytes, Bytes).

	coordinate_bytes(Order, @infinity, Bytes) :-
		!,
		positive_infinity_bytes(Order, Bytes).
	coordinate_bytes(Order, @negative_infinity, Bytes) :-
		!,
		negative_infinity_bytes(Order, Bytes).
	coordinate_bytes(Order, @not_a_number, Bytes) :-
		!,
		nan_bytes(Order, Bytes).
	coordinate_bytes(Order, not_a_number(CanonicalBytes), Bytes) :-
		!,
		nan_coordinate_bytes(CanonicalBytes),
		canonical_order_bytes(Order, Bytes, CanonicalBytes).
	coordinate_bytes(big, Coordinate, Bytes) :-
		Float is Coordinate + 0.0,
		encode_ieee754_double(Float, Bytes).
	coordinate_bytes(little, Coordinate, Bytes) :-
		Float is Coordinate + 0.0,
		encode_ieee754_double(Float, BigEndianBytes),
		reverse(BigEndianBytes, Bytes).

	canonical_order_bytes(big, Bytes, Bytes).
	canonical_order_bytes(little, Bytes, CanonicalBytes) :-
		reverse(Bytes, CanonicalBytes).

	positive_infinity_bytes(big, [0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]).
	positive_infinity_bytes(little, Bytes) :-
		positive_infinity_bytes(big, BigEndianBytes),
		reverse(BigEndianBytes, Bytes).

	negative_infinity_bytes(big, [0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]).
	negative_infinity_bytes(little, Bytes) :-
		negative_infinity_bytes(big, BigEndianBytes),
		reverse(BigEndianBytes, Bytes).

	nan_bytes(big, [0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]).
	nan_bytes(little, Bytes) :-
		nan_bytes(big, BigEndianBytes),
		reverse(BigEndianBytes, Bytes).

	all_nan([]).
	all_nan([Coordinate| Values]) :-
		nan_coordinate(Coordinate),
		all_nan(Values).

	canonical_nan_coordinate_bytes(@not_a_number, CanonicalBytes) :-
		nan_bytes(big, CanonicalBytes).
	canonical_nan_coordinate_bytes(not_a_number(CanonicalBytes), CanonicalBytes) :-
		nan_coordinate_bytes(CanonicalBytes).

	nan_coordinate(@not_a_number).
	nan_coordinate(not_a_number(CanonicalBytes)) :-
		nan_coordinate_bytes(CanonicalBytes).

	nan_coordinate_bytes(Bytes) :-
		Bytes = [_, _, _, _, _, _, _, _],
		bytes_to_unsigned_integer(Bytes, Bits),
		ExponentBits is (Bits >> 52) /\ 0x7ff,
		MantissaBits is Bits /\ 0xfffffffffffff,
		all_one_bits(ExponentBits),
		MantissaBits =\= 0.

	tokenize_wkt(Codes, Tokens) :-
		tokenize_wkt(Codes, Tokens, []).

	tokenize_wkt([], Tokens, Tokens).
	tokenize_wkt([Code| Codes], Tokens0, Tokens) :-
		( whitespace_code(Code) ->
			tokenize_wkt(Codes, Tokens0, Tokens)
		; Code =:= 0'( ->
			Tokens0 = [lparen| RestTokens],
			tokenize_wkt(Codes, RestTokens, Tokens)
		; Code =:= 0') ->
			Tokens0 = [rparen| RestTokens],
			tokenize_wkt(Codes, RestTokens, Tokens)
		; Code =:= 0', ->
			Tokens0 = [comma| RestTokens],
			tokenize_wkt(Codes, RestTokens, Tokens)
		; identifier_start_code(Code) ->
			identifier_codes([Code| Codes], IdentifierCodes, RestCodes),
			atom_codes(Identifier, IdentifierCodes),
			Tokens0 = [id(Identifier)| RestTokens],
			tokenize_wkt(RestCodes, RestTokens, Tokens)
		; number_start_code(Code) ->
			number_codes_token([Code| Codes], NumberCodes, RestCodes),
			number_codes(Number, NumberCodes),
			Tokens0 = [number(Number)| RestTokens],
			tokenize_wkt(RestCodes, RestTokens, Tokens)
		; fail
		).

	identifier_codes(Codes, IdentifierCodes, RestCodes) :-
		identifier_codes(Codes, [], RevIdentifierCodes, RestCodes),
		reverse(RevIdentifierCodes, IdentifierCodes).

	identifier_codes([], IdentifierCodes, IdentifierCodes, []).
	identifier_codes([Code| Codes], Codes0, IdentifierCodes, RestCodes) :-
		identifier_code(Code),
		!,
		identifier_codes(Codes, [Code| Codes0], IdentifierCodes, RestCodes).
	identifier_codes(Codes, IdentifierCodes, IdentifierCodes, Codes).

	number_codes_token(Codes, NumberCodes, RestCodes) :-
		number_codes_token(Codes, [], RevNumberCodes, RestCodes),
		reverse(RevNumberCodes, NumberCodes).

	number_codes_token([], NumberCodes, NumberCodes, []).
	number_codes_token([Code| Codes], Codes0, NumberCodes, RestCodes) :-
		\+ token_delimiter_code(Code),
		!,
		number_codes_token(Codes, [Code| Codes0], NumberCodes, RestCodes).
	number_codes_token(Codes, NumberCodes, NumberCodes, Codes).

	whitespace_code(32).
	whitespace_code(0'\t).
	whitespace_code(0'\n).
	whitespace_code(0'\r).

	identifier_start_code(Code) :-
		0'A =< Code,
		Code =< 0'Z.

	identifier_code(Code) :-
		identifier_start_code(Code).

	number_start_code(Code) :-
		0'0 =< Code,
		Code =< 0'9.
	number_start_code(0'+).
	number_start_code(0'-).
	number_start_code(0'.).

	token_delimiter_code(Code) :-
		whitespace_code(Code).
	token_delimiter_code(0'().
	token_delimiter_code(0')).
	token_delimiter_code(0',).

	uppercase_ascii_codes([], []).
	uppercase_ascii_codes([Code| Codes], [UppercaseCode| UppercaseCodes]) :-
		( 0'a =< Code, Code =< 0'z -> UppercaseCode is Code - 32 ; UppercaseCode = Code ),
		uppercase_ascii_codes(Codes, UppercaseCodes).

	write_codes([], _Stream).
	write_codes([Code| Codes], Stream) :-
		put_code(Stream, Code),
		write_codes(Codes, Stream).

	write_bytes([], _Stream).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

	chars_to_codes(Chars, Codes) :-
		chars_to_codes(Chars, Codes, []).

	chars_to_codes([], Codes, Codes).
	chars_to_codes([Char| Chars], [Code| Codes0], Codes) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes0, Codes).

	codes_to_chars(Codes, Chars) :-
		codes_to_chars(Codes, Chars, []).

	codes_to_chars([], Chars, Chars).
	codes_to_chars([Code| Codes], [Char| Chars0], Chars) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars0, Chars).

	hex_codes_bytes(Codes, Bytes) :-
		hex_codes_bytes(Codes, Bytes, []).

	hex_codes_bytes([], Bytes, Bytes).
	hex_codes_bytes([Code| Codes], Bytes0, Bytes) :-
		whitespace_code(Code),
		!,
		hex_codes_bytes(Codes, Bytes0, Bytes).
	hex_codes_bytes([HighCode, LowCode| Codes], [Byte| Bytes0], Bytes) :-
		hex_digit_value(HighCode, High),
		hex_digit_value(LowCode, Low),
		Byte is (High << 4) \/ Low,
		hex_codes_bytes(Codes, Bytes0, Bytes).

	hex_digit_value(Code, Value) :-
		0'0 =< Code,
		Code =< 0'9,
		!,
		Value is Code - 0'0.
	hex_digit_value(Code, Value) :-
		0'a =< Code,
		Code =< 0'f,
		!,
		Value is Code - 0'a + 10.
	hex_digit_value(Code, Value) :-
		0'A =< Code,
		Code =< 0'F,
		Value is Code - 0'A + 10.

	bytes_hex(Bytes, Hex) :-
		bytes_hex_codes(Bytes, Codes),
		atom_codes(Hex, Codes).

	bytes_hex_codes([], []).
	bytes_hex_codes([Byte| Bytes], [HighCode, LowCode| Codes]) :-
		High is (Byte >> 4) /\ 0x0f,
		Low is Byte /\ 0x0f,
		hex_digit_code(High, HighCode),
		hex_digit_code(Low, LowCode),
		bytes_hex_codes(Bytes, Codes).

	hex_digit_code(Value, Code) :-
		Value < 10,
		!,
		Code is Value + 0'0.
	hex_digit_code(Value, Code) :-
		Code is Value - 10 + 0'a.

	bytes_to_unsigned_integer(Bytes, Integer) :-
		bytes_to_unsigned_integer(Bytes, 0, Integer).

	bytes_to_unsigned_integer([], Integer, Integer).
	bytes_to_unsigned_integer([Byte| Bytes], Integer0, Integer) :-
		Integer1 is (Integer0 << 8) \/ Byte,
		bytes_to_unsigned_integer(Bytes, Integer1, Integer).

	integer_to_bytes(0, _Integer, []) :-
		!.
	integer_to_bytes(Count, Integer, [Byte| Bytes]) :-
		Count > 0,
		Shift is (Count - 1) * 8,
		Byte is (Integer >> Shift) /\ 0xff,
		NextCount is Count - 1,
		integer_to_bytes(NextCount, Integer, Bytes).

	encode_ieee754_double(0.0, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) :-
		!.
	encode_ieee754_double(Value, [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) :-
		Value = -0.0,
		!.
	encode_ieee754_double(Value, Bytes) :-
		float_to_ieee754_double(Value, Bits),
		integer_to_bytes(8, Bits, Bytes).

	float_to_ieee754_double(Value, Bits) :-
		float_sign_and_abs(Value, Sign, AbsValue),
		normalize_binary_float(AbsValue, Significand, Exponent),
		encode_ieee754_double_finite(Significand, Exponent, ExponentBits, MantissaBits),
		Bits is (Sign << 63) \/ (ExponentBits << 52) \/ MantissaBits.

	float_sign_and_abs(Value, 1, AbsValue) :-
		Value < 0.0,
		!,
		AbsValue is -Value.
	float_sign_and_abs(Value, 0, Value).

	normalize_binary_float(Value, Significand, Exponent) :-
		(	Value >= 1.0 ->
			normalize_binary_float_down(Value, 0, Significand, Exponent)
		;	normalize_binary_float_up(Value, 0, Significand, Exponent)
		).

	normalize_binary_float_down(Value, Exponent0, Significand, Exponent) :-
		(	Value < 2.0 ->
			Significand = Value,
			Exponent = Exponent0
		;	NextValue is Value / 2.0,
			NextExponent is Exponent0 + 1,
			normalize_binary_float_down(NextValue, NextExponent, Significand, Exponent)
		).

	normalize_binary_float_up(Value, Exponent0, Significand, Exponent) :-
		(	Value >= 1.0 ->
			Significand = Value,
			Exponent = Exponent0
		;	NextValue is Value * 2.0,
			NextExponent is Exponent0 - 1,
			normalize_binary_float_up(NextValue, NextExponent, Significand, Exponent)
		).

	encode_ieee754_double_finite(Significand, Exponent, ExponentBits, MantissaBits) :-
		(	Exponent > 1023 ->
			fail
		;	Exponent >= -1022 ->
			significand_fraction_bits(Significand, 52, MantissaBits, Remainder),
			Remainder = 0.0,
			ExponentBits is Exponent + 1023
		;	Exponent >= -1074 ->
			Shift is Exponent + 1074,
			scaled_significand_bits(Significand, Shift, MantissaBits, Remainder),
			Remainder = 0.0,
			ExponentBits = 0
		;	fail
		).

	significand_fraction_bits(Significand, Precision, Bits, Remainder) :-
		Fraction is Significand - 1.0,
		fraction_bits(Precision, Fraction, 0, Bits, Remainder).

	fraction_bits(0, Fraction, Bits, Bits, Fraction) :-
		!.
	fraction_bits(Precision, Fraction0, Bits0, Bits, Fraction) :-
		Precision > 0,
		Twice is Fraction0 * 2.0,
		(	Twice >= 1.0 ->
			Bit = 1,
			Fraction1 is Twice - 1.0
		;	Bit = 0,
			Fraction1 = Twice
		),
		Bits1 is (Bits0 << 1) \/ Bit,
		NextPrecision is Precision - 1,
		fraction_bits(NextPrecision, Fraction1, Bits1, Bits, Fraction).

	scaled_significand_bits(Significand, Shift, Bits, Remainder) :-
		Fraction is Significand - 1.0,
		scaled_bits(Shift, Fraction, 1, Bits, Remainder).

	scaled_bits(0, Fraction, Bits, Bits, Fraction) :-
		!.
	scaled_bits(Shift, Fraction0, Bits0, Bits, Fraction) :-
		Shift > 0,
		Twice is Fraction0 * 2.0,
		(	Twice >= 1.0 ->
			Bit = 1,
			Fraction1 is Twice - 1.0
		;	Bit = 0,
			Fraction1 = Twice
		),
		Bits1 is (Bits0 << 1) \/ Bit,
		NextShift is Shift - 1,
		scaled_bits(NextShift, Fraction1, Bits1, Bits, Fraction).

	decode_ieee754_double(Bytes, Term) :-
		bytes_to_unsigned_integer(Bytes, Bits),
		ieee754_double_to_term(Bits, Bytes, Term).

	ieee754_double_to_term(Bits, Bytes, Term) :-
		Sign is (Bits >> 63) /\ 0x01,
		ExponentBits is (Bits >> 52) /\ 0x7ff,
		MantissaBits is Bits /\ 0xfffffffffffff,
		decode_ieee754_term(Sign, ExponentBits, MantissaBits, Bytes, 52, 1023, -1074, Term).

	decode_ieee754_term(Sign, 0, 0, _Bytes, _Precision, _Bias, _SubnormalExponent, Term) :-
		!,
		zero_from_sign(Sign, Term).
	decode_ieee754_term(Sign, 0, MantissaBits, _Bytes, _Precision, _Bias, SubnormalExponent, Term) :-
		!,
		Magnitude is MantissaBits * (2.0 ** SubnormalExponent),
		apply_float_sign(Sign, Magnitude, Term).
	decode_ieee754_term(0, ExponentBits, 0, _Bytes, _Precision, _Bias, _SubnormalExponent, @infinity) :-
		all_one_bits(ExponentBits),
		!.
	decode_ieee754_term(1, ExponentBits, 0, _Bytes, _Precision, _Bias, _SubnormalExponent, @negative_infinity) :-
		all_one_bits(ExponentBits),
		!.
	decode_ieee754_term(_Sign, ExponentBits, MantissaBits, Bytes, _Precision, _Bias, _SubnormalExponent, Term) :-
		all_one_bits(ExponentBits),
		MantissaBits =\= 0,
		( nan_bytes(big, Bytes) -> Term = @not_a_number ; Term = not_a_number(Bytes) ),
		!.
	decode_ieee754_term(Sign, ExponentBits, MantissaBits, _Bytes, Precision, Bias, _SubnormalExponent, Term) :-
		Scale is 2.0 ** Precision,
		Exponent is ExponentBits - Bias,
		Magnitude is (1.0 + MantissaBits / Scale) * (2.0 ** Exponent),
		apply_float_sign(Sign, Magnitude, Term).

	all_one_bits(0x7ff).

	zero_from_sign(0, 0.0).
	zero_from_sign(1, NegativeZero) :-
		NegativeZero is -0.0.

	apply_float_sign(0, Magnitude, Magnitude).
	apply_float_sign(1, Magnitude, Term) :-
		Term is -Magnitude.

	codes([]) -->
		[].
	codes([Code| Codes]) -->
		[Code],
		codes(Codes).

	bytes([]) -->
		[].
	bytes([Byte| Bytes]) -->
		[Byte],
		bytes(Bytes).

:- end_object.
