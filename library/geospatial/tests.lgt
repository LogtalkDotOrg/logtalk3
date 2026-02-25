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
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2026-02-25,
		comment is 'Unit tests for the "geospatial" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(geospatial, [
		valid_coordinate/1,
		normalize_coordinate/2,
		equirectangular_projection/4,
		equirectangular_inverse/4,
		haversine_distance/3,
		vincenty_distance/3,
		rhumb_distance/3,
		rhumb_bearing/3,
		rhumb_destination_point/4,
		interpolate_rhumb/4,
		rhumb_midpoint/3,
		distance/4,
		distance/5,
		initial_bearing/3,
		final_bearing/3,
		midpoint/3,
		destination_point/4,
		interpolate_great_circle/4,
		cross_track_distance/4,
		along_track_distance/4,
		within_distance/4,
		nearest_coordinate/5,
		mean_center/2,
		minimum_enclosing_circle/3,
		coordinates_bounding_box/2,
		bbox_from_coordinates/2,
		point_in_polygon/2,
		polygon_area/2,
		polygon_centroid/2,
		polygon_bounding_box/2,
		close_polygon/2,
		polygon_orientation/2,
		is_clockwise_polygon/1,
		normalize_polygon_orientation/3,
		clockwise_polygon/2,
		counterclockwise_polygon/2,
		is_valid_polygon/1,
		bbox_contains/2,
		bbox_intersects/2,
		bbox_union/3,
		bbox_expand/3,
		point_to_polyline_distance/3,
		nearest_point_on_segment/4,
		nearest_point_on_polyline/4,
		polyline_length/2,
		polyline_length/3,
		polyline_simplify/3,
		polyline_split_at_distance/4,
		polyline_resample/3,
		polygon_perimeter/2,
		polygon_perimeter/3,
		polygons_intersect/2,
		bounding_box/3,
		route_distance/2,
		route_distance/3,
		route_distance/4
	]).

	cover(geospatial).

	test(geospatial_valid_coordinate_1_01, deterministic) :-
		valid_coordinate((0.0, 0.0)).

	test(geospatial_valid_coordinate_1_02, fail) :-
		valid_coordinate((91.0, 0.0)).

	test(geospatial_valid_coordinate_1_03, fail) :-
		valid_coordinate((0.0, -181.0)).

	test(geospatial_normalize_coordinate_2_01, deterministic(Coordinate == (10.0, -170.0))) :-
		normalize_coordinate((10.0, 190.0), Coordinate).

	test(geospatial_normalize_coordinate_2_02, deterministic(Coordinate == (85.0, -170.0))) :-
		normalize_coordinate((95.0, 10.0), Coordinate).

	test(geospatial_equirectangular_projection_4_01, deterministic((X > 111.19, X < 111.20, abs(Y) < 1.0e-9))) :-
		equirectangular_projection((0.0, 1.0), 0.0, X, Y).

	test(geospatial_equirectangular_inverse_4_01, deterministic((abs(Latitude) < 1.0e-9, Longitude > 0.99, Longitude < 1.01))) :-
		equirectangular_inverse(111.195, 0.0, 0.0, (Latitude, Longitude)).

	test(geospatial_haversine_distance_3_01, deterministic((Distance > 111.19, Distance < 111.20))) :-
		haversine_distance((0.0, 0.0), (0.0, 1.0), Distance).

	test(geospatial_haversine_distance_3_02, fail) :-
		haversine_distance((0.0, 0.0), (95.0, 1.0), _).

	test(geospatial_vincenty_distance_3_01, deterministic((Distance > 111.31, Distance < 111.33))) :-
		vincenty_distance((0.0, 0.0), (0.0, 1.0), Distance).

	test(geospatial_vincenty_distance_3_02, deterministic((Distance > 272.0, Distance < 278.0))) :-
		vincenty_distance((38.7223, -9.1393), (41.1579, -8.6291), Distance).

	test(geospatial_rhumb_distance_3_01, deterministic((Distance > 111.19, Distance < 111.20))) :-
		rhumb_distance((0.0, 0.0), (0.0, 1.0), Distance).

	test(geospatial_rhumb_bearing_3_01, deterministic((Bearing > 44.99, Bearing < 45.01))) :-
		rhumb_bearing((0.0, 0.0), (1.0, 1.0), Bearing).

	test(geospatial_rhumb_destination_point_4_01, deterministic((abs(Latitude) < 0.01, Longitude > 0.99, Longitude < 1.01))) :-
		rhumb_destination_point((0.0, 0.0), 90.0, 111.195, (Latitude, Longitude)).

	test(geospatial_interpolate_rhumb_4_01, deterministic((abs(Latitude) < 0.01, Longitude > 0.99, Longitude < 1.01))) :-
		interpolate_rhumb((0.0, 0.0), (0.0, 2.0), 0.5, (Latitude, Longitude)).

	test(geospatial_interpolate_rhumb_4_02, fail) :-
		interpolate_rhumb((0.0, 0.0), (0.0, 2.0), -0.1, _).

	test(geospatial_rhumb_midpoint_3_01, deterministic((abs(Latitude) < 0.01, Longitude > 0.99, Longitude < 1.01))) :-
		rhumb_midpoint((0.0, 0.0), (0.0, 2.0), (Latitude, Longitude)).

	test(geospatial_distance_4_01, deterministic((Distance > 111.19, Distance < 111.20))) :-
		distance((0.0, 0.0), (0.0, 1.0), haversine, Distance).

	test(geospatial_distance_4_03, deterministic((Distance > 111.19, Distance < 111.20))) :-
		distance((0.0, 0.0), (0.0, 1.0), rhumb, Distance).

	test(geospatial_distance_4_02, fail) :-
		distance((0.0, 0.0), (0.0, 1.0), unsupported, _).

	test(geospatial_distance_5_01, deterministic((Distance > 111194.9, Distance < 111195.1))) :-
		distance((0.0, 0.0), (0.0, 1.0), haversine, meters, Distance).

	test(geospatial_distance_5_02, deterministic((Distance > 69.09, Distance < 69.10))) :-
		distance((0.0, 0.0), (0.0, 1.0), haversine, miles, Distance).

	test(geospatial_distance_5_04, fail) :-
		distance((0.0, 0.0), (0.0, 1.0), haversine, mi, _).

	test(geospatial_distance_5_05, fail) :-
		distance((0.0, 0.0), (0.0, 1.0), haversine, mile, _).

	test(geospatial_distance_5_03, fail) :-
		distance((0.0, 0.0), (0.0, 1.0), haversine, unsupported_unit, _).

	test(geospatial_initial_bearing_3_01, deterministic((Bearing > 89.99, Bearing < 90.01))) :-
		initial_bearing((0.0, 0.0), (0.0, 1.0), Bearing).

	test(geospatial_final_bearing_3_01, deterministic((Bearing > 89.99, Bearing < 90.01))) :-
		final_bearing((0.0, 0.0), (0.0, 1.0), Bearing).

	test(geospatial_midpoint_3_01, deterministic((abs(Latitude) < 0.01, Longitude > 0.99, Longitude < 1.01))) :-
		midpoint((0.0, 0.0), (0.0, 2.0), (Latitude, Longitude)).

	test(geospatial_destination_point_4_01, deterministic((abs(Latitude) < 0.01, Longitude > 0.99, Longitude < 1.01))) :-
		destination_point((0.0, 0.0), 90.0, 111.195, (Latitude, Longitude)).

	test(geospatial_interpolate_great_circle_4_01, deterministic((abs(Latitude) < 0.01, Longitude > 0.99, Longitude < 1.01))) :-
		interpolate_great_circle((0.0, 0.0), (0.0, 2.0), 0.5, (Latitude, Longitude)).

	test(geospatial_interpolate_great_circle_4_02, fail) :-
		interpolate_great_circle((0.0, 0.0), (0.0, 2.0), 1.5, _).

	test(geospatial_cross_track_distance_4_01, deterministic((abs(Distance) > 111.1, abs(Distance) < 111.3))) :-
		cross_track_distance((1.0, 1.0), (0.0, 0.0), (0.0, 2.0), Distance).

	test(geospatial_along_track_distance_4_01, deterministic((Distance > 111.1, Distance < 111.3))) :-
		along_track_distance((1.0, 1.0), (0.0, 0.0), (0.0, 2.0), Distance).

	test(geospatial_within_distance_4_01, deterministic) :-
		within_distance((0.0, 0.0), (0.0, 1.0), 112.0, haversine).

	test(geospatial_within_distance_4_02, fail) :-
		within_distance((0.0, 0.0), (0.0, 1.0), 100.0, haversine).

	test(geospatial_nearest_coordinate_5_01, deterministic((Nearest == (0.0, 1.0), Distance > 111.19, Distance < 111.20))) :-
		nearest_coordinate(
			(0.0, 0.0),
			[(0.0, 2.0), (0.0, 1.0), (0.0, 3.0)],
			haversine,
			Nearest,
			Distance
		).

	test(geospatial_nearest_coordinate_5_02, fail) :-
		nearest_coordinate((0.0, 0.0), [], haversine, _, _).

	test(geospatial_mean_center_2_01, deterministic((Latitude =~= 2.0, Longitude =~= 2.0))) :-
		mean_center([(0.0, 0.0), (2.0, 2.0), (4.0, 4.0)], (Latitude, Longitude)).

	test(geospatial_mean_center_2_02, fail) :-
		mean_center([], _).

	test(geospatial_minimum_enclosing_circle_3_01, deterministic((Center = (10.0, -20.0), Radius =~= 0.0))) :-
		minimum_enclosing_circle([(10.0, -20.0)], Center, Radius).

	test(geospatial_minimum_enclosing_circle_3_02, deterministic((abs(Latitude) < 0.01, Longitude > 0.99, Longitude < 1.01, Radius > 111.19, Radius < 111.20))) :-
		minimum_enclosing_circle([(0.0, 0.0), (0.0, 2.0)], (Latitude, Longitude), Radius).

	test(geospatial_minimum_enclosing_circle_3_03, deterministic((Latitude > 0.49, Latitude < 0.51, Longitude > 0.49, Longitude < 0.51, Radius > 78.62, Radius < 78.64))) :-
		minimum_enclosing_circle([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], (Latitude, Longitude), Radius).

	test(geospatial_minimum_enclosing_circle_3_04, fail) :-
		minimum_enclosing_circle([], _, _).

	test(geospatial_coordinates_bounding_box_2_01, deterministic((MinLatitude =~= -1.0, MinLongitude =~= -2.0, MaxLatitude =~= 1.0, MaxLongitude =~= 1.0))) :-
		coordinates_bounding_box([(1.0, -2.0), (0.0, 1.0), (-1.0, -1.0)], bbox((MinLatitude, MinLongitude), (MaxLatitude, MaxLongitude))).

	test(geospatial_coordinates_bounding_box_2_02, fail) :-
		coordinates_bounding_box([], _).

	test(geospatial_bbox_from_coordinates_2_01, deterministic((MinLatitude =~= -1.0, MinLongitude =~= -2.0, MaxLatitude =~= 1.0, MaxLongitude =~= 1.0))) :-
		bbox_from_coordinates([(1.0, -2.0), (0.0, 1.0), (-1.0, -1.0)], bbox((MinLatitude, MinLongitude), (MaxLatitude, MaxLongitude))).

	test(geospatial_bbox_from_coordinates_2_02, fail) :-
		bbox_from_coordinates([], _).

	test(geospatial_point_in_polygon_2_01, deterministic) :-
		point_in_polygon(
			(0.5, 0.5),
			[(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)]
		).

	test(geospatial_point_in_polygon_2_02, fail) :-
		point_in_polygon(
			(1.5, 0.5),
			[(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)]
		).

	test(geospatial_point_in_polygon_2_03, deterministic) :-
		point_in_polygon(
			(0.0, 0.5),
			[(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0), (0.0, 0.0)]
		).

	test(geospatial_polygon_area_2_01, deterministic((Area > 12363.0, Area < 12365.0))) :-
		polygon_area(
			[(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)],
			Area
		).

	test(geospatial_polygon_area_2_02, fail) :-
		polygon_area([(0.0, 0.0), (0.0, 1.0)], _).

	test(geospatial_polygon_centroid_2_01, deterministic((Latitude > 0.49, Latitude < 0.51, Longitude > 0.49, Longitude < 0.51))) :-
		polygon_centroid([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], (Latitude, Longitude)).

	test(geospatial_polygon_centroid_2_02, fail) :-
		polygon_centroid([(0.0, 0.0), (0.0, 1.0)], _).

	test(geospatial_polygon_bounding_box_2_01, deterministic((MinLatitude =~= -1.0, MinLongitude =~= -2.0, MaxLatitude =~= 1.0, MaxLongitude =~= 1.0))) :-
		polygon_bounding_box(
			[(1.0, -2.0), (0.0, 1.0), (-1.0, -1.0)],
			bbox((MinLatitude, MinLongitude), (MaxLatitude, MaxLongitude))
		).

	test(geospatial_close_polygon_2_01, deterministic(ClosedPolygon == [(0.0, 0.0), (0.0, 1.0), (1.0, 0.0), (0.0, 0.0)])) :-
		close_polygon([(0.0, 0.0), (0.0, 1.0), (1.0, 0.0)], ClosedPolygon).

	test(geospatial_polygon_orientation_2_01, deterministic(Orientation == clockwise)) :-
		polygon_orientation([(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)], Orientation).

	test(geospatial_polygon_orientation_2_02, deterministic(Orientation == counterclockwise)) :-
		polygon_orientation([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], Orientation).

	test(geospatial_is_clockwise_polygon_1_01, deterministic) :-
		is_clockwise_polygon([(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)]).

	test(geospatial_is_clockwise_polygon_1_02, fail) :-
		is_clockwise_polygon([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)]).

	test(geospatial_normalize_polygon_orientation_3_01, deterministic(is_clockwise_polygon(Oriented))) :-
		normalize_polygon_orientation([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], clockwise, Oriented).

	test(geospatial_normalize_polygon_orientation_3_02, fail) :-
		normalize_polygon_orientation([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], unsupported, _).

	test(geospatial_clockwise_polygon_2_01, deterministic(is_clockwise_polygon(Clockwise))) :-
		clockwise_polygon([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], Clockwise).

	test(geospatial_counterclockwise_polygon_2_01, deterministic(Orientation == counterclockwise)) :-
		counterclockwise_polygon([(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)], Counterclockwise),
		polygon_orientation(Counterclockwise, Orientation).

	test(geospatial_is_valid_polygon_1_01, deterministic) :-
		is_valid_polygon([(0.0, 0.0), (0.0, 1.0), (1.0, 0.0)]).

	test(geospatial_is_valid_polygon_1_02, fail) :-
		is_valid_polygon([(0.0, 0.0), (0.0, 1.0)]).

	test(geospatial_bbox_contains_2_01, deterministic) :-
		bbox_contains(bbox((-1.0, -1.0), (1.0, 1.0)), (0.5, 0.5)).

	test(geospatial_bbox_contains_2_02, fail) :-
		bbox_contains(bbox((-1.0, -1.0), (1.0, 1.0)), (2.0, 0.5)).

	test(geospatial_bbox_intersects_2_01, deterministic) :-
		bbox_intersects(bbox((0.0, 0.0), (2.0, 2.0)), bbox((1.0, 1.0), (3.0, 3.0))).

	test(geospatial_bbox_intersects_2_02, fail) :-
		bbox_intersects(bbox((0.0, 0.0), (1.0, 1.0)), bbox((2.0, 2.0), (3.0, 3.0))).

	test(geospatial_bbox_union_3_01, deterministic) :-
		bbox_union(bbox((0.0, 0.0), (1.0, 1.0)), bbox((-1.0, 0.5), (0.5, 2.0)), bbox((MinLatitude, MinLongitude), (MaxLatitude, MaxLongitude))),
		^^assertion(MinLatitude =~= -1.0),
		^^assertion(MinLongitude =~= 0.0),
		^^assertion(MaxLatitude =~= 1.0),
		^^assertion(MaxLongitude =~= 2.0).

	test(geospatial_bbox_expand_3_01, deterministic) :-
		bbox_expand(bbox((0.0, 0.0), (0.0, 0.0)), 111.195, bbox((MinLatitude, MinLongitude), (MaxLatitude, MaxLongitude))),
		^^assertion(MinLatitude > -1.01),
		^^assertion(MinLatitude < -0.99),
		^^assertion(MaxLatitude > 0.99),
		^^assertion(MaxLatitude < 1.01),
		^^assertion(MinLongitude > -1.01),
		^^assertion(MinLongitude < -0.99),
		^^assertion(MaxLongitude > 0.99),
		^^assertion(MaxLongitude < 1.01).

	test(geospatial_point_to_polyline_distance_3_01, deterministic((Distance > 111.1, Distance < 111.3))) :-
		point_to_polyline_distance((1.0, 1.0), [(0.0, 0.0), (0.0, 2.0)], Distance).

	test(geospatial_nearest_point_on_segment_4_01, deterministic) :-
		nearest_point_on_segment((1.0, 1.0), (0.0, 0.0), (0.0, 2.0), (Latitude, Longitude)),
		^^assertion(abs(Latitude) < 0.01),
		^^assertion(Longitude > 0.99),
		^^assertion(Longitude < 1.01).

	test(geospatial_nearest_point_on_polyline_4_01, deterministic) :-
		nearest_point_on_polyline((1.0, 1.0), [(0.0, 0.0), (0.0, 2.0), (2.0, 2.0)], Nearest, Distance),
		^^assertion(geospatial::valid_coordinate(Nearest)),
		^^assertion(Distance > 111.1),
		^^assertion(Distance < 111.3).

	test(geospatial_polyline_length_2_01, deterministic((Length > 222.39, Length < 222.40))) :-
		polyline_length([(0.0, 0.0), (0.0, 1.0), (0.0, 2.0)], Length).

	test(geospatial_polyline_length_3_01, deterministic((Length > 111.31, Length < 111.33))) :-
		polyline_length([(0.0, 0.0), (0.0, 1.0)], vincenty, Length).

	test(geospatial_polyline_simplify_3_01, deterministic(Simplified == [(0.0, 0.0), (1.0, 0.0)])) :-
		polyline_simplify(
			[(0.0, 0.0), (0.5, 0.1), (1.0, 0.0)],
			20.0,
			Simplified
		).

	test(geospatial_polyline_simplify_3_02, deterministic(Simplified == [(0.0, 0.0), (0.5, 0.1), (1.0, 0.0)])) :-
		polyline_simplify(
			[(0.0, 0.0), (0.5, 0.1), (1.0, 0.0)],
			5.0,
			Simplified
		).

	test(geospatial_polyline_simplify_3_03, fail) :-
		polyline_simplify([(0.0, 0.0)], 5.0, _).

	test(geospatial_polyline_split_at_distance_4_01, deterministic((coordinates_close(Left, [(0.0, 0.0), (0.0, 1.0)]), coordinates_close(Right, [(0.0, 1.0), (0.0, 2.0)])))) :-
		haversine_distance((0.0, 0.0), (0.0, 1.0), Step),
		polyline_split_at_distance([(0.0, 0.0), (0.0, 1.0), (0.0, 2.0)], Step, Left, Right).

	test(geospatial_polyline_split_at_distance_4_02, deterministic((Left == [(0.0, 0.0)], Right == [(0.0, 0.0), (0.0, 1.0)]))) :-
		polyline_split_at_distance([(0.0, 0.0), (0.0, 1.0)], 0.0, Left, Right).

	test(geospatial_polyline_split_at_distance_4_03, deterministic((Left == [(0.0, 0.0), (0.0, 1.0)], Right == [(0.0, 1.0)]))) :-
		polyline_split_at_distance([(0.0, 0.0), (0.0, 1.0)], 300.0, Left, Right).

	test(geospatial_polyline_resample_3_01, deterministic(coordinates_close(Resampled, [(0.0, 0.0), (0.0, 1.0), (0.0, 2.0)]))) :-
		haversine_distance((0.0, 0.0), (0.0, 1.0), Step),
		polyline_resample([(0.0, 0.0), (0.0, 1.0), (0.0, 2.0)], Step, Resampled).

	test(geospatial_polyline_resample_3_02, deterministic(coordinates_close(Resampled, [(0.0, 0.0), (0.0, 1.0)]))) :-
		polyline_resample([(0.0, 0.0), (0.0, 1.0)], 500.0, Resampled).

	test(geospatial_polygon_perimeter_2_01, deterministic((Perimeter > 444.75, Perimeter < 444.80))) :-
		polygon_perimeter([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], Perimeter).

	test(geospatial_polygon_perimeter_3_01, deterministic((Perimeter > 443.7, Perimeter < 444.1))) :-
		polygon_perimeter([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], vincenty, Perimeter).

	test(geospatial_polygons_intersect_2_01, deterministic) :-
		polygons_intersect(
			[(0.0, 0.0), (0.0, 2.0), (2.0, 2.0), (2.0, 0.0)],
			[(1.0, 1.0), (1.0, 3.0), (3.0, 3.0), (3.0, 1.0)]
		).

	test(geospatial_polygons_intersect_2_02, deterministic) :-
		polygons_intersect(
			[(0.0, 0.0), (0.0, 4.0), (4.0, 4.0), (4.0, 0.0)],
			[(1.0, 1.0), (1.0, 2.0), (2.0, 2.0), (2.0, 1.0)]
		).

	test(geospatial_polygons_intersect_2_03, fail) :-
		polygons_intersect(
			[(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)],
			[(2.0, 2.0), (2.0, 3.0), (3.0, 3.0), (3.0, 2.0)]
		).

	test(geospatial_bounding_box_3_01, deterministic) :-
		bounding_box((0.0, 0.0), 111.195, bbox((MinLatitude, MinLongitude), (MaxLatitude, MaxLongitude))),
		^^assertion(MinLatitude > -1.01),
		^^assertion(MinLatitude < -0.99),
		^^assertion(MaxLatitude > 0.99),
		^^assertion(MaxLatitude < 1.01),
		^^assertion(MinLongitude > -1.01),
		^^assertion(MinLongitude < -0.99),
		^^assertion(MaxLongitude > 0.99),
		^^assertion(MaxLongitude < 1.01).

	test(geospatial_route_distance_2_01, deterministic((Distance > 222.39, Distance < 222.40))) :-
		route_distance([(0.0, 0.0), (0.0, 1.0), (0.0, 2.0)], Distance).

	test(geospatial_route_distance_3_01, deterministic((Distance > 111.31, Distance < 111.33))) :-
		route_distance([(0.0, 0.0), (0.0, 1.0)], vincenty, Distance).

	test(geospatial_route_distance_3_02, fail) :-
		route_distance([(0.0, 0.0)], haversine, _).

	test(geospatial_route_distance_3_03, fail) :-
		route_distance([(0.0, 0.0), (100.0, 0.0)], haversine, _).

	test(geospatial_route_distance_4_01, deterministic((Distance > 60.03, Distance < 60.05))) :-
		route_distance([(0.0, 0.0), (0.0, 1.0)], haversine, nautical_miles, Distance).

	test(geospatial_route_distance_4_02, fail) :-
		route_distance([(0.0, 0.0), (0.0, 1.0)], haversine, unsupported_unit, _).

	test(geospatial_route_distance_4_03, fail) :-
		route_distance([(0.0, 0.0), (0.0, 1.0)], haversine, nm, _).

	% auxiliary predicates

	coordinate_close((Latitude1, Longitude1), (Latitude2, Longitude2)) :-
		Latitude1 =~= Latitude2,
		Longitude1 =~= Longitude2.

	coordinates_close([], []).
	coordinates_close([Coordinate1| Coordinates1], [Coordinate2| Coordinates2]) :-
		coordinate_close(Coordinate1, Coordinate2),
		coordinates_close(Coordinates1, Coordinates2).

:- end_object.
