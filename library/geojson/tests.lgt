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
		date is 2026-05-08,
		comment is 'Unit tests for the "geojson" library.'
	]).

	:- uses(geojson, [
		parse/2, generate/2, validate/1, validate/2, json_to_geojson/2, geojson_to_json/2
	]).

	cover(geojson(_, _, _)).
	cover(geojson).

	cleanup :-
		^^clean_file('test_files/output.geojson'),
		^^clean_file('test_files/output_stream.geojson').

	test(geojson_parse_error_var_source_01, error(instantiation_error)) :-
		parse(_, _).

	test(geojson_parse_error_invalid_source_01, error(domain_error(geojson_source, foo))) :-
		parse(foo, _).

	test(geojson_parse_point_file_01, deterministic(Term == point([100.0, 0.0]))) :-
		^^file_path('test_files/point.geojson', Path),
		parse(file(Path), Term).

	test(geojson_parse_point_stream_source_01, deterministic(Term == point([100.0, 0.0]))) :-
		^^file_path('test_files/point.geojson', Path),
		open(Path, read, Stream),
		parse(stream(Stream), Term),
		close(Stream).

	test(geojson_parse_point_atom_01, deterministic(Term == point([100.0, 0.0]))) :-
		parse(atom('{"type":"Point","coordinates":[100.0,0.0]}'), Term).

	test(geojson_parse_point_codes_source_01, deterministic(Term == point([100.0, 0.0]))) :-
		atom_codes('{"type":"Point","coordinates":[100.0,0.0]}', Codes),
		parse(codes(Codes), Term).

	test(geojson_parse_point_chars_source_01, deterministic(Term == point([100.0, 0.0]))) :-
		atom_chars('{"type":"Point","coordinates":[100.0,0.0]}', Chars),
		parse(chars(Chars), Term).

	test(geojson_parse_point_chars_representation_01, deterministic(Term == point([100.0, 0.0]))) :-
		geojson(curly, dash, chars)::parse(atom('{"type":"Point","coordinates":[100.0,0.0]}'), Term).

	test(geojson_parse_point_list_equal_representation_01, deterministic(Term == point([100.0, 0.0]))) :-
		geojson(list, equal, atom)::parse(atom('{"type":"Point","coordinates":[100.0,0.0]}'), Term).

	test(geojson_parse_geometry_collection_atom_01, deterministic(Term == geometry_collection([
		point([100.0, 0.0]),
		line_string([[101.0, 0.0], [102.0, 1.0]])
	], [bbox([100.0, 0.0, 102.0, 1.0])]))) :-
		parse(atom('{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[100.0,0.0]},{"type":"LineString","coordinates":[[101.0,0.0],[102.0,1.0]]}],"bbox":[100.0,0.0,102.0,1.0]}'), Term).

	test(geojson_parse_multi_point_atom_01, deterministic(Term == multi_point([[100.0, 0.0], [101.0, 1.0]], [bbox([100.0, 0.0, 101.0, 1.0])]))) :-
		parse(atom('{"type":"MultiPoint","coordinates":[[100.0,0.0],[101.0,1.0]],"bbox":[100.0,0.0,101.0,1.0]}'), Term).

	test(geojson_parse_multi_line_string_atom_01, deterministic(Term == multi_line_string([[[100.0, 0.0], [101.0, 1.0]], [[102.0, 2.0], [103.0, 3.0]]], [bbox([100.0, 0.0, 103.0, 3.0])]))) :-
		parse(atom('{"type":"MultiLineString","coordinates":[[[100.0,0.0],[101.0,1.0]],[[102.0,2.0],[103.0,3.0]]],"bbox":[100.0,0.0,103.0,3.0]}'), Term).

	test(geojson_parse_multi_polygon_atom_01, deterministic(Term == multi_polygon([
		[[[0.0, 0.0], [1.0, 0.0], [1.0, 1.0], [0.0, 1.0], [0.0, 0.0]]],
		[[[2.0, 2.0], [3.0, 2.0], [3.0, 3.0], [2.0, 3.0], [2.0, 2.0]]]
	], [bbox([0.0, 0.0, 3.0, 3.0])]))) :-
		parse(atom('{"type":"MultiPolygon","coordinates":[[[[0.0,0.0],[1.0,0.0],[1.0,1.0],[0.0,1.0],[0.0,0.0]]],[[[2.0,2.0],[3.0,2.0],[3.0,3.0],[2.0,3.0],[2.0,2.0]]]],"bbox":[0.0,0.0,3.0,3.0]}'), Term).

	test(geojson_parse_feature_null_geometry_atom_01, deterministic(Term == feature(@null, {}, [id(1)]))) :-
		parse(atom('{"type":"Feature","geometry":null,"properties":{},"id":1}'), Term).

	test(geojson_generate_error_var_sink_01, error(instantiation_error)) :-
		generate(_, point([1, 2])).

	test(geojson_generate_error_invalid_sink_01, error(domain_error(geojson_sink, foo))) :-
		generate(foo, point([1, 2])).

	test(geojson_generate_point_atom_01, deterministic(Atom == '{"type":"Point","coordinates":[100.0,0.0]}')) :-
		generate(atom(Atom), point([100.0, 0.0])).

	test(geojson_generate_point_codes_01, deterministic(Codes == Expected)) :-
		atom_codes('{"type":"Point","coordinates":[100.0,0.0]}', Expected),
		generate(codes(Codes), point([100.0, 0.0])).

	test(geojson_generate_point_chars_01, deterministic(Chars == Expected)) :-
		atom_chars('{"type":"Point","coordinates":[100.0,0.0]}', Expected),
		generate(chars(Chars), point([100.0, 0.0])).

	test(geojson_generate_point_file_01, deterministic(Term == point([1, 2]))) :-
		^^file_path('test_files/output.geojson', Path),
		generate(file(Path), point([1, 2])),
		parse(file(Path), Term).

	test(geojson_generate_point_stream_01, deterministic(Term == point([1, 2]))) :-
		^^file_path('test_files/output_stream.geojson', Path),
		open(Path, write, Stream),
		generate(stream(Stream), point([1, 2])),
		close(Stream),
		parse(file(Path), Term).

	test(geojson_parse_feature_atom_01, deterministic(Term == feature(point([100.0, 0.0]), {name-sample}, [id(feature_1), bbox([99.0, -1.0, 101.0, 1.0]), foreign_members([title-demo])]))) :-
		parse(atom('{"type":"Feature","id":"feature_1","bbox":[99.0,-1.0,101.0,1.0],"geometry":{"type":"Point","coordinates":[100.0,0.0]},"properties":{"name":"sample"},"title":"demo"}'), Term).

	test(geojson_parse_feature_collection_file_01, deterministic(Term == feature_collection([
		feature(point([102.0, 0.5]), {prop0-value0}),
		feature(line_string([[102.0, 0.0], [103.0, 1.0], [104.0, 0.0], [105.0, 1.0]]), {prop0-value0, prop1-0.0}),
		feature(polygon([[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0]]]), {prop0-value0, prop1-{'this'-that}})
	], [foreign_members([name-sample])]))) :-
		^^file_path('test_files/feature_collection.geojson', Path),
		parse(file(Path), Term).

	test(geojson_generate_feature_collection_atom_01, deterministic(JSON == '{"type":"FeatureCollection","features":[{"type":"Feature","geometry":{"type":"Point","coordinates":[1,2]},"properties":{}},{"type":"Feature","geometry":null,"properties":null}],"bbox":[0,0,2,2],"title":"demo"}')) :-
		generate(atom(JSON), feature_collection([
			feature(point([1, 2]), {}),
			feature(@null, @null)
		], [bbox([0, 0, 2, 2]), foreign_members([title-demo])])).

	test(geojson_generate_geometry_collection_atom_01, deterministic(JSON == '{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[1,2]},{"type":"LineString","coordinates":[[3,4],[5,6]]}],"bbox":[1,2,5,6]}')) :-
		generate(atom(JSON), geometry_collection([
			point([1, 2]),
			line_string([[3, 4], [5, 6]])
		], [bbox([1, 2, 5, 6])])).

	test(geojson_generate_multi_point_atom_01, deterministic(JSON == '{"type":"MultiPoint","coordinates":[[1,2],[3,4]]}')) :-
		generate(atom(JSON), multi_point([[1, 2], [3, 4]])).

	test(geojson_generate_multi_line_string_atom_01, deterministic(JSON == '{"type":"MultiLineString","coordinates":[[[1,2],[3,4]],[[5,6],[7,8]]]}')) :-
		generate(atom(JSON), multi_line_string([[[1, 2], [3, 4]], [[5, 6], [7, 8]]])).

	test(geojson_generate_feature_atom_01, deterministic(JSON == '{"type":"Feature","geometry":{"type":"Point","coordinates":[1,2]},"properties":{}}')) :-
		generate(atom(JSON), feature(point([1, 2]), {})).

	test(geojson_generate_multi_polygon_atom_01, deterministic(JSON == '{"type":"MultiPolygon","coordinates":[[[[0,0],[1,0],[1,1],[0,1],[0,0]]],[[[2,2],[3,2],[3,3],[2,3],[2,2]]]],"bbox":[0,0,3,3]}')) :-
		generate(atom(JSON), multi_polygon([
			[[[0, 0], [1, 0], [1, 1], [0, 1], [0, 0]]],
			[[[2, 2], [3, 2], [3, 3], [2, 3], [2, 2]]]
		], [bbox([0, 0, 3, 3])])).

	test(geojson_validate_valid_polygon_01, deterministic) :-
		validate(polygon([[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0]]])).

	test(geojson_validate_valid_multi_point_01, deterministic) :-
		validate(multi_point([[100.0, 0.0], [101.0, 1.0]])).

	test(geojson_validate_valid_multi_point_bbox_01, deterministic) :-
		validate(multi_point([[100.0, 0.0], [101.0, 1.0]], [bbox([100.0, 0.0, 101.0, 1.0])])).

	test(geojson_validate_valid_line_string_01, deterministic) :-
		validate(line_string([[100.0, 0.0], [101.0, 1.0]])).

	test(geojson_validate_valid_line_string_bbox_01, deterministic) :-
		validate(line_string([[100.0, 0.0], [101.0, 1.0]], [bbox([100.0, 0.0, 101.0, 1.0])])).

	test(geojson_validate_valid_multi_line_string_01, deterministic) :-
		validate(multi_line_string([
			[[0.0, 0.0], [1.0, 1.0]],
			[[2.0, 2.0], [3.0, 3.0]]
		])).

	test(geojson_validate_valid_polygon_bbox_01, deterministic) :-
		validate(polygon([[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0]]], [bbox([100.0, 0.0, 101.0, 1.0])])).

	test(geojson_validate_valid_multi_polygon_01, deterministic) :-
		validate(multi_polygon([
			[[[0.0, 0.0], [1.0, 0.0], [1.0, 1.0], [0.0, 1.0], [0.0, 0.0]]],
			[[[2.0, 2.0], [3.0, 2.0], [3.0, 3.0], [2.0, 3.0], [2.0, 2.0]]]
		])).

	test(geojson_validate_valid_geometry_collection_01, deterministic) :-
		validate(geometry_collection([
			point([1.0, 2.0]),
			line_string([[3.0, 4.0], [5.0, 6.0]])
		])).

	test(geojson_validate_valid_feature_01, deterministic) :-
		validate(feature(point([100.0, 0.0]), {})).

	test(geojson_validate_valid_feature_options_01, deterministic) :-
		validate(feature(@null, @null, [id(1), foreign_members([title-demo])])).

	test(geojson_validate_valid_null_geometry_bbox_01, deterministic) :-
		validate(feature(@null, @null, [bbox([0.0, 0.0, 1.0, 1.0])])).

	test(geojson_validate_valid_feature_collection_01, deterministic) :-
		validate(feature_collection([feature(point([100.0, 0.0]), {})])).

	test(geojson_validate_valid_json_scalar_values_01, deterministic) :-
		validate(feature(point([100.0, 0.0]), {
			number-1,
			string-demo,
			boolean_true - @true,
			boolean_false - @false,
			null - @null,
			object - {nested-value},
			array-[1, demo, @true, @null]
		})).

	test(geojson_validate_invalid_position_01, deterministic(Errors == [invalid_position([coordinates])])) :-
		validate(point([100.0]), Errors).

	test(geojson_validate_invalid_geojson_term_01, deterministic(Errors == [invalid_geojson_term([])])) :-
		validate(foo, Errors).

	test(geojson_validate_invalid_position_longitude_01, deterministic(Errors == [position_longitude_out_of_range([coordinates,0])])) :-
		validate(point([181.0, 0.0]), Errors).

	test(geojson_validate_invalid_position_latitude_01, deterministic(Errors == [position_latitude_out_of_range([coordinates,1])])) :-
		validate(point([100.0, 91.0]), Errors).

	test(geojson_validate_nested_feature_collection_path_01, deterministic(Errors == [invalid_position([features,0,geometry,coordinates])])) :-
		validate(feature_collection([feature(point([100.0]), {})]), Errors).

	test(geojson_validate_nested_position_longitude_01, deterministic(Errors == [position_longitude_out_of_range([coordinates,1,0])])) :-
		validate(line_string([[0.0, 0.0], [200.0, 0.0]]), Errors).

	test(geojson_validate_invalid_ring_01, deterministic(Errors == [ring_not_closed([coordinates,0])])) :-
		validate(polygon([[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0]]]), Errors).

	test(geojson_validate_invalid_properties_01, deterministic(Errors == [invalid_properties([properties])])) :-
		validate(feature(point([100.0, 0.0]), [a, b]), Errors).

	test(geojson_validate_invalid_property_value_01, deterministic(Errors == [invalid_json_value([properties,bad])])) :-
		validate(feature(point([100.0, 0.0]), {bad-f(foo)}), Errors).

	test(geojson_validate_invalid_property_array_value_01, deterministic(Errors == [invalid_json_value([properties,values,1])])) :-
		validate(feature(point([100.0, 0.0]), {values-[1, f(foo)]}), Errors).

	test(geojson_validate_invalid_property_key_01, deterministic(Errors == [invalid_json_key([properties])])) :-
		validate(feature(point([100.0, 0.0]), {1-demo}), Errors).

	test(geojson_validate_duplicate_property_key_01, deterministic(Errors == [duplicate_json_object_key(a, [properties,a])])) :-
		validate(feature(point([100.0, 0.0]), {a-1, a-2}), Errors).

	test(geojson_validate_invalid_feature_id_01, deterministic(Errors == [invalid_id([id])])) :-
		validate(feature(point([100.0, 0.0]), {}, [id([not, valid])]), Errors).

	test(geojson_validate_invalid_chars_feature_id_01, deterministic(Errors == [invalid_id([id])])) :-
		geojson(curly, dash, chars)::validate(feature(point([100.0, 0.0]), {}, [id(chars([1]))]), Errors).

	test(geojson_validate_invalid_codes_feature_id_01, deterministic(Errors == [invalid_id([id])])) :-
		geojson(curly, dash, codes)::validate(feature(point([100.0, 0.0]), {}, [id(codes([a]))]), Errors).

	test(geojson_validate_reserved_foreign_member_01, deterministic(Errors == [reserved_foreign_member(type, [foreign_members,type])])) :-
		validate(point([100.0, 0.0], [foreign_members([type-'Point'])]), Errors).

	test(geojson_validate_prohibited_crs_01, deterministic(Errors == [prohibited_member(crs, [foreign_members,crs])])) :-
		validate(point([100.0, 0.0], [foreign_members([crs-{}])]), Errors).

	test(geojson_validate_invalid_foreign_member_value_01, deterministic(Errors == [invalid_json_value([foreign_members,title])])) :-
		validate(point([100.0, 0.0], [foreign_members([title-f(foo)])]), Errors).

	test(geojson_validate_invalid_foreign_members_01, deterministic(Errors == [invalid_foreign_members([foreign_members])])) :-
		validate(point([100.0, 0.0], [foreign_members(foo)]), Errors).

	test(geojson_validate_option_not_allowed_01, deterministic(Errors == [option_not_allowed(id, [])])) :-
		validate(point([100.0, 0.0], [id(1)]), Errors).

	test(geojson_validate_unknown_option_01, deterministic(Errors == [unknown_option(foo, [])])) :-
		validate(point([100.0, 0.0], [foo]), Errors).

	test(geojson_validate_antimeridian_bbox_01, deterministic) :-
		validate(point([179.0, -18.0], [bbox([177.0, -20.0, -178.0, -16.0])])).

	test(geojson_validate_invalid_bbox_longitude_01, deterministic(Errors == [bbox_longitude_out_of_range([bbox,0])])) :-
		validate(point([100.0, 0.0], [bbox([-181.0, -1.0, 101.0, 1.0])]), Errors).

	test(geojson_validate_invalid_bbox_latitude_01, deterministic(Errors == [bbox_latitude_out_of_range([bbox,1])])) :-
		validate(point([100.0, 0.0], [bbox([99.0, -91.0, 101.0, 1.0])]), Errors).

	test(geojson_validate_invalid_bbox_latitude_order_01, deterministic(Errors == [bbox_latitude_order([bbox])])) :-
		validate(point([100.0, 0.0], [bbox([99.0, 2.0, 101.0, 1.0])]), Errors).

	test(geojson_validate_invalid_bbox_altitude_order_01, deterministic(Errors == [bbox_altitude_order([bbox])])) :-
		validate(point([100.0, 0.0, 7.0], [bbox([99.0, -1.0, 10.0, 101.0, 1.0, 5.0])]), Errors).

	test(geojson_validate_invalid_bbox_dimension_01, deterministic(Errors == [bbox_dimension_mismatch(6, 4, [bbox])])) :-
		validate(point([100.0, 0.0], [bbox([99.0, -1.0, 0.0, 101.0, 1.0, 5.0])]), Errors).

	test(geojson_validate_invalid_bbox_dimension_02, deterministic(Errors == [bbox_dimension_mismatch(4, 6, [bbox])])) :-
		validate(point([100.0, 0.0, 7.0], [bbox([99.0, -1.0, 101.0, 1.0])]), Errors).

	test(geojson_validate_mixed_dimension_feature_collection_bbox_01, deterministic) :-
		validate(feature_collection([
			feature(point([1.0, 2.0]), {}),
			feature(point([3.0, 4.0, 5.0]), {})
		], [bbox([0.0, 0.0, 0.0, 4.0, 5.0, 6.0])])).

	test(geojson_validate_mixed_dimension_feature_collection_bbox_02, deterministic(Errors == [bbox_dimension_mismatch(4, 6, [bbox])])) :-
		validate(feature_collection([
			feature(point([1.0, 2.0]), {}),
			feature(point([3.0, 4.0, 5.0]), {})
		], [bbox([0.0, 0.0, 4.0, 5.0])]), Errors).

	test(geojson_validate_valid_multi_line_string_bbox_01, deterministic) :-
		validate(multi_line_string([
			[[0.0, 0.0], [1.0, 1.0]],
			[[2.0, 2.0], [3.0, 3.0]]
		], [bbox([0.0, 0.0, 3.0, 3.0])])).

	test(geojson_validate_valid_multi_polygon_bbox_01, deterministic) :-
		validate(multi_polygon([
			[[[0.0, 0.0], [1.0, 0.0], [1.0, 1.0], [0.0, 1.0], [0.0, 0.0]]],
			[[[2.0, 2.0], [3.0, 2.0], [3.0, 3.0], [2.0, 3.0], [2.0, 2.0]]]
		], [bbox([0.0, 0.0, 3.0, 3.0])])).

	test(geojson_validate_valid_geometry_collection_bbox_01, deterministic) :-
		validate(geometry_collection([
			point([1.0, 2.0]),
			line_string([[3.0, 4.0], [5.0, 6.0]])
		], [bbox([1.0, 2.0, 5.0, 6.0])])).

	test(geojson_validate_invalid_geometry_collection_member_01, deterministic(Errors == [invalid_geometry([geometries,0])])) :-
		validate(geometry_collection([@null]), Errors).

	test(geojson_validate_duplicate_option_01, deterministic(Errors == [duplicate_option(bbox, [])])) :-
		validate(point([100.0, 0.0], [bbox([0,0,1,1]), bbox([0,0,1,1])]), Errors).

	test(geojson_json_to_geojson_01, deterministic(Term == point([1, 2]))) :-
		json_to_geojson({type-'Point', coordinates-[1, 2]}, Term).

	test(geojson_json_to_geojson_equal_representation_01, deterministic(Term == point([1, 2]))) :-
		geojson(list, equal, atom)::json_to_geojson(json([type='Point', coordinates=[1, 2]]), Term).

	test(geojson_json_to_geojson_list_representation_01, deterministic(Term == point([1, 2]))) :-
		geojson(list, colon, atom)::json_to_geojson(json([':'(type, 'Point'), ':'(coordinates, [1, 2])]), Term).

	test(geojson_geojson_to_json_list_representation_01, deterministic(JSON == json([type-'Point', coordinates-[1, 2]]))) :-
		geojson(list, dash, atom)::geojson_to_json(point([1, 2]), JSON).

	test(geojson_geojson_to_json_01, deterministic(JSON == {type-'Feature', geometry - @null, properties - @null, id-1})) :-
		geojson_to_json(feature(@null, @null, [id(1)]), JSON).

	test(geojson_geojson_to_json_multi_point_01, deterministic(JSON == {type-'MultiPoint', coordinates-[[1, 2], [3, 4]], bbox-[1, 2, 3, 4]})) :-
		geojson_to_json(multi_point([[1, 2], [3, 4]], [bbox([1, 2, 3, 4])]), JSON).

	test(geojson_geojson_to_json_line_string_01, deterministic(JSON == {type-'LineString', coordinates-[[1, 2], [3, 4]], bbox-[1, 2, 3, 4]})) :-
		geojson_to_json(line_string([[1, 2], [3, 4]], [bbox([1, 2, 3, 4])]), JSON).

	test(geojson_geojson_to_json_multi_line_string_01, deterministic(JSON == {type-'MultiLineString', coordinates-[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]})) :-
		geojson_to_json(multi_line_string([[[1, 2], [3, 4]], [[5, 6], [7, 8]]]), JSON).

	test(geojson_geojson_to_json_multi_line_string_options_01, deterministic(JSON == {type-'MultiLineString', coordinates-[[[1, 2], [3, 4]], [[5, 6], [7, 8]]], bbox-[1, 2, 7, 8]})) :-
		geojson_to_json(multi_line_string([[[1, 2], [3, 4]], [[5, 6], [7, 8]]], [bbox([1, 2, 7, 8])]), JSON).

	test(geojson_geojson_to_json_polygon_01, deterministic(JSON == {type-'Polygon', coordinates-[[[0, 0], [1, 0], [1, 1], [0, 1], [0, 0]]]})) :-
		geojson_to_json(polygon([[[0, 0], [1, 0], [1, 1], [0, 1], [0, 0]]]), JSON).

	test(geojson_geojson_to_json_polygon_options_01, deterministic(JSON == {type-'Polygon', coordinates-[[[0, 0], [1, 0], [1, 1], [0, 1], [0, 0]]], bbox-[0, 0, 1, 1]})) :-
		geojson_to_json(polygon([[[0, 0], [1, 0], [1, 1], [0, 1], [0, 0]]], [bbox([0, 0, 1, 1])]), JSON).

	test(geojson_geojson_to_json_multi_polygon_no_options_01, deterministic(JSON == {type-'MultiPolygon', coordinates-[[[[0, 0], [1, 0], [1, 1], [0, 1], [0, 0]]], [[[2, 2], [3, 2], [3, 3], [2, 3], [2, 2]]]]})) :-
		geojson_to_json(multi_polygon([
			[[[0, 0], [1, 0], [1, 1], [0, 1], [0, 0]]],
			[[[2, 2], [3, 2], [3, 3], [2, 3], [2, 2]]]
		]), JSON).

	test(geojson_geojson_to_json_geometry_collection_01, deterministic(JSON == {type-'GeometryCollection', geometries-[{type-'Point', coordinates-[1, 2]}, {type-'LineString', coordinates-[[3, 4], [5, 6]]}]})) :-
		geojson_to_json(geometry_collection([point([1, 2]), line_string([[3, 4], [5, 6]])]), JSON).

	test(geojson_geojson_to_json_feature_collection_01, deterministic(JSON == {type-'FeatureCollection', features-[{type-'Feature', geometry - {type-'Point', coordinates-[1, 2]}, properties-{}}, {type-'Feature', geometry - @null, properties - @null}]})) :-
		geojson_to_json(feature_collection([feature(point([1, 2]), {}), feature(@null, @null)]), JSON).

	test(geojson_generate_foreign_member_object_01, deterministic(JSON == '{"type":"Point","coordinates":[1,2],"title":"demo"}')) :-
		generate(atom(JSON), point([1, 2], [foreign_members({title-demo})])).

	test(geojson_parse_invalid_polygon_file_01, error(domain_error(geojson, _))) :-
		^^file_path('test_files/invalid_polygon.geojson', Path),
		parse(file(Path), _).

	test(geojson_parse_invalid_root_01, error(domain_error(geojson, _))) :-
		parse(atom('[1,2,3]'), _).

	test(geojson_parse_invalid_json_text_01, error(domain_error(geojson, _))) :-
		parse(atom('{"type":"Point","coordinates":[100.0,0.0]'), _).

	test(geojson_parse_duplicate_reserved_member_01, error(domain_error(geojson, _))) :-
		parse(atom('{"type":"Point","coordinates":[100.0,0.0],"bbox":[0,0,1,1],"bbox":[1,1,2,2]}'), _).

	test(geojson_generate_invalid_term_01, error(domain_error(geojson, _))) :-
		generate(atom(_), feature_collection([point([1, 2])])).

	test(geojson_generate_invalid_term_codes_sink_01, error(domain_error(geojson, _))) :-
		generate(codes(_), feature_collection([point([1, 2])])).

	test(geojson_generate_invalid_term_chars_sink_01, error(domain_error(geojson, _))) :-
		generate(chars(_), feature_collection([point([1, 2])])).

	test(geojson_generate_invalid_term_file_sink_01, error(domain_error(geojson, _))) :-
		^^file_path('test_files/output.geojson', Path),
		generate(file(Path), feature_collection([point([1, 2])])).

	test(geojson_generate_invalid_term_stream_sink_01, error(domain_error(geojson, _))) :-
		^^file_path('test_files/output_stream.geojson', Path),
		open(Path, write, Stream),
		catch(generate(stream(Stream), feature_collection([point([1, 2])])), Error, (close(Stream), throw(Error))).

	test(geojson_generate_invalid_property_value_01, error(domain_error(geojson, _))) :-
		generate(atom(_), feature(point([1, 2]), {bad-f(foo)})).

:- end_object.
