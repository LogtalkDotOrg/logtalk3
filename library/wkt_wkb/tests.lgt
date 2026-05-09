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
		date is 2026-05-09,
		comment is 'Unit tests for the "wkt_wkb" library.'
	]).

	:- uses(wkt_wkb, [
		parse/2, generate/2, validate/1, validate/2
	]).

	cover(wkt_wkb).

	cleanup :-
		^^clean_file('test_files/output.wkt'),
		^^clean_file('test_files/output_stream.wkt'),
		^^clean_file('test_files/output.wkb'),
		^^clean_file('test_files/output_stream.wkb').

	test(wkt_wkb_parse_error_var_source_01, error(instantiation_error)) :-
		parse(_, _).

	test(wkt_wkb_parse_error_invalid_source_01, error(domain_error(wkt_wkb_source, foo))) :-
		parse(foo, _).

	test(wkt_wkb_parse_wkt_file_01, deterministic(Geometry == point([100, 0]))) :-
		^^file_path('test_files/point.wkt', Path),
		parse(wkt(file(Path)), Geometry).

	test(wkt_wkb_parse_wkt_stream_01, deterministic(Geometry == point([100, 0]))) :-
		^^file_path('test_files/point.wkt', Path),
		open(Path, read, Stream),
		parse(wkt(stream(Stream)), Geometry),
		close(Stream).

	test(wkt_wkb_parse_wkt_point_m_01, deterministic(Geometry == point([1, 2, 3], [dimensions(m)]))) :-
		parse(wkt(atom('POINT M (1 2 3)')), Geometry).

	test(wkt_wkb_parse_wkt_chars_multi_line_string_01, deterministic(Geometry == multi_line_string([[], [[0, 0, 0], [1, 1, 1]]], [dimensions(z)]))) :-
		atom_chars('MULTILINESTRING Z (EMPTY, (0 0 0, 1 1 1))', Chars),
		parse(wkt(chars(Chars)), Geometry).

	test(wkt_wkb_parse_wkt_codes_multi_polygon_01, deterministic(Geometry == multi_polygon([[], [[[0, 0, 0, 1], [1, 0, 0, 1], [1, 1, 0, 1], [0, 0, 0, 1]]]], [dimensions(zm)]))) :-
		atom_codes('MULTIPOLYGON ZM (EMPTY, ((0 0 0 1, 1 0 0 1, 1 1 0 1, 0 0 0 1)))', Codes),
		parse(wkt(codes(Codes)), Geometry).

	test(wkt_wkb_parse_wkt_polygon_hole_01, deterministic(Geometry == polygon([[[0, 0], [4, 0], [4, 4], [0, 0]], [[1, 1], [2, 1], [2, 2], [1, 1]]]))) :-
		parse(wkt(atom('POLYGON ((0 0, 4 0, 4 4, 0 0), (1 1, 2 1, 2 2, 1 1))')), Geometry).

	test(wkt_wkb_parse_wkt_multi_point_empty_01, deterministic(Geometry == multi_point([[], [1, 2]]))) :-
		parse(wkt(atom('MULTIPOINT (EMPTY, (1 2))')), Geometry).

	test(wkt_wkb_parse_wkt_collection_inherited_dimensions_01, deterministic(Geometry == geometry_collection([
		point([1, 2, 3], [dimensions(z)]),
		line_string([[0, 0, 0], [1, 1, 1]], [dimensions(z)])
	], [dimensions(z)]))) :-
		parse(wkt(atom('GEOMETRYCOLLECTION Z (POINT (1 2 3), LINESTRING (0 0 0, 1 1 1))')), Geometry).

	test(wkt_wkb_parse_wkt_invalid_content_01, error(domain_error(wkt_wkb, wkt(atom('POINT (1)'))))) :-
		parse(wkt(atom('POINT (1)')), _).

	test(wkt_wkb_generate_error_var_sink_01, error(instantiation_error)) :-
		generate(_, point([1, 2])).

	test(wkt_wkb_generate_error_invalid_sink_01, error(domain_error(wkt_wkb_sink, foo))) :-
		generate(foo, point([1, 2])).

	test(wkt_wkb_generate_wkt_atom_01, deterministic(WKT == 'POINT (1 2)')) :-
		generate(wkt(atom(WKT)), point([1, 2])).

	test(wkt_wkb_generate_wkt_atom_m_01, deterministic(WKT == 'POINT M (1 2 3)')) :-
		generate(wkt(atom(WKT)), point([1, 2, 3], [dimensions(m)])).

	test(wkt_wkb_generate_wkt_chars_multi_line_string_01, deterministic(Chars == ExpectedChars)) :-
		generate(wkt(chars(Chars)), multi_line_string([[], [[0, 0, 0], [1, 1, 1]]])),
		atom_chars('MULTILINESTRING Z (EMPTY, (0 0 0, 1 1 1))', ExpectedChars).

	test(wkt_wkb_generate_wkt_codes_multi_polygon_01, deterministic(Codes == ExpectedCodes)) :-
		generate(wkt(codes(Codes)), multi_polygon([[], [[[0, 0, 0, 1], [1, 0, 0, 1], [1, 1, 0, 1], [0, 0, 0, 1]]]])),
		atom_codes('MULTIPOLYGON ZM (EMPTY, ((0 0 0 1, 1 0 0 1, 1 1 0 1, 0 0 0 1)))', ExpectedCodes).

	test(wkt_wkb_generate_wkt_atom_polygon_hole_01, deterministic(WKT == 'POLYGON ((0 0, 4 0, 4 4, 0 0), (1 1, 2 1, 2 2, 1 1))')) :-
		generate(wkt(atom(WKT)), polygon([[[0, 0], [4, 0], [4, 4], [0, 0]], [[1, 1], [2, 1], [2, 2], [1, 1]]])).

	test(wkt_wkb_generate_wkt_atom_geometry_collection_01, deterministic(WKT == 'GEOMETRYCOLLECTION Z (POINT Z (1 2 3), POINT Z (4 5 6))')) :-
		generate(wkt(atom(WKT)), geometry_collection([point([1, 2, 3]), point([4, 5, 6])])).

	test(wkt_wkb_generate_wkt_atom_multi_point_empty_01, deterministic(WKT == 'MULTIPOINT (EMPTY, (1 2))')) :-
		generate(wkt(atom(WKT)), multi_point([[], [1, 2]])).

	test(wkt_wkb_generate_wkt_error_invalid_geometry_01, error(domain_error(wkt_wkb, point([1])))) :-
		generate(wkt(atom(_)), point([1])).

	test(wkt_wkb_generate_wkt_error_special_coordinates_01, error(domain_error(wkt_wkb_representation, wkt(point([@infinity, 0]))))) :-
		generate(wkt(atom(_)), point([@infinity, 0])).

	test(wkt_wkb_generate_wkt_file_01, deterministic(Geometry == line_string([[1, 2], [3, 4]]))) :-
		^^file_path('test_files/output.wkt', Path),
		generate(wkt(file(Path)), line_string([[1, 2], [3, 4]])),
		parse(wkt(file(Path)), Geometry).

	test(wkt_wkb_generate_wkt_stream_01, deterministic(Geometry == polygon([[[0, 0], [1, 0], [1, 1], [0, 0]]]))) :-
		^^file_path('test_files/output_stream.wkt', Path),
		open(Path, write, Stream),
		generate(wkt(stream(Stream)), polygon([[[0, 0], [1, 0], [1, 1], [0, 0]]])),
		close(Stream),
		parse(wkt(file(Path)), Geometry).

	test(wkt_wkb_parse_wkb_bytes_01, deterministic(Geometry == point([1.0, 2.0]))) :-
		parse(wkb(bytes([1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 0, 64])), Geometry).

	test(wkt_wkb_parse_wkb_hex_atom_01, deterministic(Geometry == point([1.0, 2.0]))) :-
		parse(wkb(hex(atom('0101000000000000000000f03f0000000000000040'))), Geometry).

	test(wkt_wkb_parse_wkb_hex_chars_01, deterministic(Geometry == point([1.0, 2.0]))) :-
		atom_chars('0101000000000000000000f03f0000000000000040', Chars),
		parse(wkb(hex(chars(Chars))), Geometry).

	test(wkt_wkb_parse_wkb_hex_codes_01, deterministic(Geometry == point([1.0, 2.0]))) :-
		atom_codes('0101000000000000000000f03f0000000000000040', Codes),
		parse(wkb(hex(codes(Codes))), Geometry).

	test(wkt_wkb_parse_wkb_bytes_big_01, deterministic(Geometry == point([1.0, 2.0]))) :-
		parse(wkb(bytes([0, 0, 0, 0, 1, 63, 240, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0])), Geometry).

	test(wkt_wkb_parse_wkb_line_string_infinities_01, deterministic(Geometry == line_string([[@infinity, @negative_infinity], [0.0, 1.0]]))) :-
		parse(wkb(bytes([1, 2, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 127, 0, 0, 0, 0, 0, 0, 240, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 63])), Geometry).

	test(wkt_wkb_parse_wkb_line_string_nan_01, deterministic(Geometry == line_string([[@not_a_number, 0.0], [1.0, 2.0]]))) :-
		parse(wkb(bytes([1, 2, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 248, 127, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 0, 64])), Geometry).

	test(wkt_wkb_parse_wkb_point_nan_01, deterministic(Geometry == point([@not_a_number, 0.0]))) :-
		parse(wkb(bytes([1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 248, 127, 0, 0, 0, 0, 0, 0, 0, 0])), Geometry).

	test(wkt_wkb_parse_wkb_point_nan_payload_01, deterministic(Geometry == point([not_a_number([0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]), 0.0]))) :-
		parse(wkb(bytes([1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 248, 127, 0, 0, 0, 0, 0, 0, 0, 0])), Geometry).

	test(wkt_wkb_parse_generate_wkb_line_string_infinities_01, deterministic(OutputBytes == InputBytes)) :-
		InputBytes = [1, 2, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 127, 0, 0, 0, 0, 0, 0, 240, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 63],
		parse(wkb(bytes(InputBytes)), Geometry),
		generate(wkb(bytes(OutputBytes)), Geometry).

	test(wkt_wkb_parse_generate_wkb_line_string_nan_01, deterministic(OutputBytes == InputBytes)) :-
		InputBytes = [1, 2, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 248, 127, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 0, 64],
		parse(wkb(bytes(InputBytes)), Geometry),
		generate(wkb(bytes(OutputBytes)), Geometry).

	test(wkt_wkb_parse_generate_wkb_point_nan_01, deterministic(OutputBytes == InputBytes)) :-
		InputBytes = [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 248, 127, 0, 0, 0, 0, 0, 0, 0, 0],
		parse(wkb(bytes(InputBytes)), Geometry),
		generate(wkb(bytes(OutputBytes)), Geometry).

	test(wkt_wkb_parse_generate_wkb_point_nan_payload_01, deterministic(OutputBytes == InputBytes)) :-
		InputBytes = [1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 248, 127, 0, 0, 0, 0, 0, 0, 0, 0],
		parse(wkb(bytes(InputBytes)), Geometry),
		generate(wkb(bytes(OutputBytes)), Geometry).

	test(wkt_wkb_parse_generate_wkb_line_string_nan_payload_01, deterministic(OutputBytes == InputBytes)) :-
		InputBytes = [1, 2, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 248, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 0, 64],
		parse(wkb(bytes(InputBytes)), Geometry),
		generate(wkb(bytes(OutputBytes)), Geometry).

	test(wkt_wkb_parse_generate_wkb_subnormal_point_01, deterministic(OutputBytes == InputBytes)) :-
		InputBytes = [1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
		parse(wkb(bytes(InputBytes)), Geometry),
		generate(wkb(bytes(OutputBytes)), Geometry).

	test(wkt_wkb_parse_wkb_invalid_line_string_01, error(domain_error(wkt_wkb, wkb(bytes([1, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 0, 64]))))) :-
		parse(wkb(bytes([1, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 0, 64])), _).

	test(wkt_wkb_parse_wkb_invalid_content_01, error(domain_error(wkt_wkb, wkb(bytes([1, 1, 0, 0]))))) :-
		parse(wkb(bytes([1, 1, 0, 0])), _).

	test(wkt_wkb_generate_wkb_bytes_little_01, deterministic(Bytes == [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 0, 64])) :-
		generate(wkb(bytes(Bytes)), point([1, 2])).

	test(wkt_wkb_generate_wkb_bytes_big_01, deterministic(Bytes == [0, 0, 0, 0, 1, 63, 240, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0])) :-
		generate(wkb(bytes(Bytes), big), point([1, 2])).

	test(wkt_wkb_generate_wkb_hex_atom_01, deterministic(Hex == '0101000000000000000000f03f0000000000000040')) :-
		generate(wkb(hex(atom(Hex))), point([1, 2])).

	test(wkt_wkb_generate_wkb_hex_chars_01, deterministic(Chars == ExpectedChars)) :-
		generate(wkb(hex(chars(Chars))), point([1, 2])),
		atom_chars('0101000000000000000000f03f0000000000000040', ExpectedChars).

	test(wkt_wkb_generate_wkb_hex_codes_01, deterministic(Codes == ExpectedCodes)) :-
		generate(wkb(hex(codes(Codes))), point([1, 2])),
		atom_codes('0101000000000000000000f03f0000000000000040', ExpectedCodes).

	test(wkt_wkb_generate_wkb_file_01, deterministic(Geometry == multi_point([[], [1.0, 2.0]]))) :-
		^^file_path('test_files/output.wkb', Path),
		generate(wkb(file(Path)), multi_point([[], [1, 2]])),
		parse(wkb(file(Path)), Geometry).

	test(wkt_wkb_generate_wkb_polygon_hole_01, deterministic(Geometry == polygon([[[0.0, 0.0], [4.0, 0.0], [4.0, 4.0], [0.0, 0.0]], [[1.0, 1.0], [2.0, 1.0], [2.0, 2.0], [1.0, 1.0]]]))) :-
		generate(wkb(bytes(Bytes)), polygon([[[0, 0], [4, 0], [4, 4], [0, 0]], [[1, 1], [2, 1], [2, 2], [1, 1]]])),
		parse(wkb(bytes(Bytes)), Geometry).

	test(wkt_wkb_generate_wkb_multi_line_string_01, deterministic(Geometry == multi_line_string([[], [[1.0, 2.0], [3.0, 4.0]]]))) :-
		generate(wkb(bytes(Bytes)), multi_line_string([[], [[1, 2], [3, 4]]])),
		parse(wkb(bytes(Bytes)), Geometry).

	test(wkt_wkb_generate_wkb_multi_polygon_01, deterministic(Geometry == multi_polygon([[], [[[0.0, 0.0], [1.0, 0.0], [1.0, 1.0], [0.0, 0.0]]]]))) :-
		generate(wkb(bytes(Bytes)), multi_polygon([[], [[[0, 0], [1, 0], [1, 1], [0, 0]]]])),
		parse(wkb(bytes(Bytes)), Geometry).

	test(wkt_wkb_generate_wkb_geometry_collection_01, deterministic(Geometry == geometry_collection([point([1.0, 2.0]), line_string([[3.0, 4.0], [5.0, 6.0]])]))) :-
		generate(wkb(bytes(Bytes)), geometry_collection([point([1, 2]), line_string([[3, 4], [5, 6]])])),
		parse(wkb(bytes(Bytes)), Geometry).

	test(wkt_wkb_generate_wkb_stream_01, deterministic(Geometry == line_string([[1.0, 2.0], [3.0, 4.0]]))) :-
		^^file_path('test_files/output_stream.wkb', Path),
		open(Path, write, Stream, [type(binary)]),
		generate(wkb(stream(Stream)), line_string([[1, 2], [3, 4]])),
		close(Stream),
		parse(wkb(file(Path)), Geometry).

	test(wkt_wkb_generate_wkb_empty_point_01, deterministic(Bytes == [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 248, 127, 0, 0, 0, 0, 0, 0, 248, 127])) :-
		generate(wkb(bytes(Bytes)), point([])).

	test(wkt_wkb_parse_wkb_empty_point_01, deterministic(Geometry == point([]))) :-
		parse(wkb(bytes([1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 248, 127, 0, 0, 0, 0, 0, 0, 248, 127])), Geometry).

	test(wkt_wkb_validate_valid_polygon_01, deterministic) :-
		validate(polygon([[[0, 0], [1, 0], [1, 1], [0, 0]]])).

	test(wkt_wkb_validate_valid_point_m_01, deterministic) :-
		validate(point([1, 2, 3], [dimensions(m)])).

	test(wkt_wkb_validate_invalid_point_01, deterministic(Errors == [invalid_position([coordinates])])) :-
		validate(point([1]), Errors).

	test(wkt_wkb_validate_invalid_line_string_01, deterministic(Errors == [insufficient_positions(2, [coordinates])])) :-
		validate(line_string([[1, 2]]), Errors).

	test(wkt_wkb_validate_invalid_polygon_ring_01, deterministic(Errors == [ring_not_closed([coordinates,0])])) :-
		validate(polygon([[[0, 0], [1, 0], [1, 1], [0, 1]]]), Errors).

	test(wkt_wkb_validate_dimension_mismatch_01, deterministic(Errors == [coordinate_dimension_mismatch(3, 4, [coordinates])])) :-
		validate(point([1, 2, 3], [dimensions(zm)]), Errors).

	test(wkt_wkb_validate_unknown_option_01, deterministic(Errors == [unknown_option(foo, [options])])) :-
		validate(point([1, 2], [foo]), Errors).

	test(wkt_wkb_validate_duplicate_option_01, deterministic(Errors == [duplicate_option(dimensions, [options])])) :-
		validate(point([1, 2], [dimensions(xy), dimensions(xy)]), Errors).

	test(wkt_wkb_validate_invalid_dimension_option_01, deterministic(Errors == [invalid_dimensions([options,dimensions])])) :-
		validate(point([1, 2], [dimensions(q)]), Errors).

	test(wkt_wkb_validate_mixed_coordinate_dimension_01, deterministic(Errors == [mixed_coordinate_dimension([coordinates])])) :-
		validate(line_string([[1, 2], [3, 4, 5]]), Errors).

	test(wkt_wkb_validate_invalid_geometry_collection_01, deterministic(Errors == [invalid_geometry_term([geometries,0])])) :-
		validate(geometry_collection([foo]), Errors).

	test(wkt_wkb_validate_invalid_multi_line_string_01, deterministic(Errors == [invalid_position_array([coordinates,0])])) :-
		validate(multi_line_string([foo]), Errors).

	test(wkt_wkb_validate_invalid_multi_polygon_01, deterministic(Errors == [invalid_polygon([coordinates,0])])) :-
		validate(multi_polygon([foo]), Errors).

	test(wkt_wkb_validate_invalid_geometry_collection_data_01, deterministic(Errors == [invalid_geometry_collection([geometries])])) :-
		validate(geometry_collection(foo), Errors).

:- end_object.
