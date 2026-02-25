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


:- protocol(geospatial_protocol).

	:- info([
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2026-02-25,
		comment is 'Geospatial predicates protocol.',
		see_also is [geospatial, numberlistp, listp]
	]).

	:- public(valid_coordinate/1).
	:- mode(valid_coordinate(@compound), zero_or_one).
	:- info(valid_coordinate/1, [
		comment is 'True if the argument is a valid geographic coordinate represented as ``(Latitude,Longitude)`` with latitude in the ``[-90.0,90.0]`` range and longitude in the ``[-180.0,180.0]`` range.',
		argnames is ['Coordinate']
	]).

	:- public(normalize_coordinate/2).
	:- mode(normalize_coordinate(+compound, -compound), one).
	:- info(normalize_coordinate/2, [
		comment is 'Normalizes a coordinate by wrapping longitude to the ``[-180.0,180.0]`` range and reflecting latitude over the poles when necessary.',
		argnames is ['Coordinate', 'NormalizedCoordinate']
	]).

	:- public(equirectangular_projection/4).
	:- mode(equirectangular_projection(+compound, +float, -float, -float), zero_or_one).
	:- info(equirectangular_projection/4, [
		comment is 'Projects a coordinate to local equirectangular planar coordinates in kilometers using a reference latitude in degrees.',
		argnames is ['Coordinate', 'ReferenceLatitude', 'X', 'Y']
	]).

	:- public(equirectangular_inverse/4).
	:- mode(equirectangular_inverse(+float, +float, +float, -compound), zero_or_one).
	:- info(equirectangular_inverse/4, [
		comment is 'Converts local equirectangular planar coordinates in kilometers to a geographic coordinate using a reference latitude in degrees.',
		argnames is ['X', 'Y', 'ReferenceLatitude', 'Coordinate']
	]).

	:- public(haversine_distance/3).
	:- mode(haversine_distance(+compound, +compound, -float), zero_or_one).
	:- info(haversine_distance/3, [
		comment is 'Computes the great-circle distance in kilometers between two coordinates using the Haversine formula.',
		argnames is ['Coordinate1', 'Coordinate2', 'Distance']
	]).

	:- public(vincenty_distance/3).
	:- mode(vincenty_distance(+compound, +compound, -float), zero_or_one).
	:- info(vincenty_distance/3, [
		comment is 'Computes the geodesic distance in kilometers between two coordinates using the Vincenty inverse formula over the WGS84 ellipsoid. Fails if the iterative method does not converge.',
		argnames is ['Coordinate1', 'Coordinate2', 'Distance']
	]).

	:- public(rhumb_distance/3).
	:- mode(rhumb_distance(+compound, +compound, -float), zero_or_one).
	:- info(rhumb_distance/3, [
		comment is 'Computes the rhumb-line (loxodrome) distance in kilometers between two coordinates on a spherical Earth model.',
		argnames is ['Coordinate1', 'Coordinate2', 'Distance']
	]).

	:- public(rhumb_bearing/3).
	:- mode(rhumb_bearing(+compound, +compound, -float), zero_or_one).
	:- info(rhumb_bearing/3, [
		comment is 'Computes the rhumb-line initial bearing in degrees in the ``[0.0,360.0[`` range from the first coordinate to the second coordinate.',
		argnames is ['Coordinate1', 'Coordinate2', 'Bearing']
	]).

	:- public(rhumb_destination_point/4).
	:- mode(rhumb_destination_point(+compound, +float, +float, -compound), zero_or_one).
	:- info(rhumb_destination_point/4, [
		comment is 'Computes the destination coordinate from a start coordinate, a rhumb-line bearing in degrees, and a distance in kilometers on a spherical Earth model.',
		argnames is ['Start', 'Bearing', 'Distance', 'Destination']
	]).

	:- public(interpolate_rhumb/4).
	:- mode(interpolate_rhumb(+compound, +compound, +float, -compound), zero_or_one).
	:- info(interpolate_rhumb/4, [
		comment is 'Computes an intermediate coordinate along the rhumb-line path between two coordinates using a fraction in the ``[0.0,1.0]`` range.',
		argnames is ['Coordinate1', 'Coordinate2', 'Fraction', 'Coordinate']
	]).

	:- public(rhumb_midpoint/3).
	:- mode(rhumb_midpoint(+compound, +compound, -compound), zero_or_one).
	:- info(rhumb_midpoint/3, [
		comment is 'Computes the midpoint along the rhumb-line path between two coordinates.',
		argnames is ['Coordinate1', 'Coordinate2', 'Midpoint']
	]).

	:- public(distance/4).
	:- mode(distance(+compound, +compound, +atom, -float), zero_or_one).
	:- info(distance/4, [
		comment is 'Computes the distance in kilometers between two coordinates using a selected metric. Supported metrics are ``haversine``, ``vincenty``, and ``rhumb``.',
		argnames is ['Coordinate1', 'Coordinate2', 'Metric', 'Distance']
	]).

	:- public(distance/5).
	:- mode(distance(+compound, +compound, +atom, +atom, -float), zero_or_one).
	:- info(distance/5, [
		comment is 'Computes the distance between two coordinates using a selected metric and output unit. Supported metrics are ``haversine``, ``vincenty``, and ``rhumb``. Valid ``Unit`` argument values are ``kilometers``, ``meters``, ``miles``, and ``nautical_miles``.',
		argnames is ['Coordinate1', 'Coordinate2', 'Metric', 'Unit', 'Distance']
	]).

	:- public(initial_bearing/3).
	:- mode(initial_bearing(+compound, +compound, -float), zero_or_one).
	:- info(initial_bearing/3, [
		comment is 'Computes the initial bearing in degrees in the ``[0.0,360.0[`` range from the first coordinate to the second coordinate.',
		argnames is ['Coordinate1', 'Coordinate2', 'Bearing']
	]).

	:- public(final_bearing/3).
	:- mode(final_bearing(+compound, +compound, -float), zero_or_one).
	:- info(final_bearing/3, [
		comment is 'Computes the final bearing in degrees in the ``[0.0,360.0[`` range when arriving at the second coordinate from the first coordinate.',
		argnames is ['Coordinate1', 'Coordinate2', 'Bearing']
	]).

	:- public(midpoint/3).
	:- mode(midpoint(+compound, +compound, -compound), zero_or_one).
	:- info(midpoint/3, [
		comment is 'Computes the geographic midpoint between two coordinates using a spherical Earth model.',
		argnames is ['Coordinate1', 'Coordinate2', 'Midpoint']
	]).

	:- public(destination_point/4).
	:- mode(destination_point(+compound, +float, +float, -compound), zero_or_one).
	:- info(destination_point/4, [
		comment is 'Computes the destination coordinate from a start coordinate, an initial bearing in degrees, and a distance in kilometers using a spherical Earth model.',
		argnames is ['Start', 'Bearing', 'Distance', 'Destination']
	]).

	:- public(interpolate_great_circle/4).
	:- mode(interpolate_great_circle(+compound, +compound, +float, -compound), zero_or_one).
	:- info(interpolate_great_circle/4, [
		comment is 'Computes an intermediate coordinate along the great-circle path between two coordinates using a fraction in the ``[0.0,1.0]`` range.',
		argnames is ['Coordinate1', 'Coordinate2', 'Fraction', 'Coordinate']
	]).

	:- public(cross_track_distance/4).
	:- mode(cross_track_distance(+compound, +compound, +compound, -float), zero_or_one).
	:- info(cross_track_distance/4, [
		comment is 'Computes the signed cross-track distance in kilometers from a coordinate to the great-circle path defined by a start and end coordinate.',
		argnames is ['Coordinate', 'Start', 'End', 'Distance']
	]).

	:- public(along_track_distance/4).
	:- mode(along_track_distance(+compound, +compound, +compound, -float), zero_or_one).
	:- info(along_track_distance/4, [
		comment is 'Computes the along-track distance in kilometers from the start coordinate to the closest point to a coordinate on the great-circle path defined by a start and end coordinate.',
		argnames is ['Coordinate', 'Start', 'End', 'Distance']
	]).

	:- public(within_distance/4).
	:- mode(within_distance(+compound, +compound, +float, +atom), zero_or_one).
	:- info(within_distance/4, [
		comment is 'True when the distance between two coordinates is less than or equal to the given radius in kilometers using the selected metric.',
		argnames is ['Coordinate1', 'Coordinate2', 'Radius', 'Metric']
	]).

	:- public(nearest_coordinate/5).
	:- mode(nearest_coordinate(+compound, +list(compound), +atom, -compound, -float), zero_or_one).
	:- info(nearest_coordinate/5, [
		comment is 'Finds the nearest coordinate to the origin coordinate in a list using the selected metric, returning the nearest coordinate and distance in kilometers.',
		argnames is ['Origin', 'Coordinates', 'Metric', 'Nearest', 'Distance']
	]).

	:- public(mean_center/2).
	:- mode(mean_center(+list(compound), -compound), zero_or_one).
	:- info(mean_center/2, [
		comment is 'Computes the arithmetic mean center of a list of one or more coordinates.',
		argnames is ['Coordinates', 'Center']
	]).

	:- public(minimum_enclosing_circle/3).
	:- mode(minimum_enclosing_circle(+list(compound), -compound, -float), zero_or_one).
	:- info(minimum_enclosing_circle/3, [
		comment is 'Computes an approximate minimum enclosing circle for a list of one or more coordinates, returning the circle center coordinate and radius in kilometers.',
		argnames is ['Coordinates', 'Center', 'Radius']
	]).

	:- public(coordinates_bounding_box/2).
	:- mode(coordinates_bounding_box(+list(compound), -compound), zero_or_one).
	:- info(coordinates_bounding_box/2, [
		comment is 'Computes the axis-aligned latitude/longitude bounding box for a list of one or more coordinates.',
		argnames is ['Coordinates', 'BoundingBox']
	]).

	:- public(point_in_polygon/2).
	:- mode(point_in_polygon(+compound, +list(compound)), zero_or_one).
	:- info(point_in_polygon/2, [
		comment is 'True when a coordinate is inside (or on the boundary of) a polygon represented as a list of coordinates. Uses a planar ray-casting algorithm over latitude/longitude coordinates.',
		argnames is ['Point', 'Polygon']
	]).

	:- public(polygon_area/2).
	:- mode(polygon_area(+list(compound), -float), zero_or_one).
	:- info(polygon_area/2, [
		comment is 'Computes an approximate polygon area in square kilometers by projecting coordinates to a local equirectangular plane and applying the shoelace formula.',
		argnames is ['Polygon', 'Area']
	]).

	:- public(polygon_centroid/2).
	:- mode(polygon_centroid(+list(compound), -compound), zero_or_one).
	:- info(polygon_centroid/2, [
		comment is 'Computes an approximate polygon centroid by using a local equirectangular projection and planar centroid formula.',
		argnames is ['Polygon', 'Centroid']
	]).

	:- public(polygon_bounding_box/2).
	:- mode(polygon_bounding_box(+list(compound), -compound), zero_or_one).
	:- info(polygon_bounding_box/2, [
		comment is 'Computes the axis-aligned latitude/longitude bounding box for a polygon represented as a list of coordinates.',
		argnames is ['Polygon', 'BoundingBox']
	]).

	:- public(close_polygon/2).
	:- mode(close_polygon(+list(compound), -list(compound)), zero_or_one).
	:- info(close_polygon/2, [
		comment is 'Returns a closed polygon ring by ensuring the first coordinate is repeated at the end of the list.',
		argnames is ['Polygon', 'ClosedPolygon']
	]).

	:- public(polygon_orientation/2).
	:- mode(polygon_orientation(+list(compound), -atom), zero_or_one).
	:- info(polygon_orientation/2, [
		comment is 'Computes polygon ring orientation as ``clockwise`` or ``counterclockwise`` using a local projected signed area approximation.',
		argnames is ['Polygon', 'Orientation']
	]).

	:- public(is_clockwise_polygon/1).
	:- mode(is_clockwise_polygon(+list(compound)), zero_or_one).
	:- info(is_clockwise_polygon/1, [
		comment is 'True when a polygon ring orientation is clockwise.',
		argnames is ['Polygon']
	]).

	:- public(normalize_polygon_orientation/3).
	:- mode(normalize_polygon_orientation(+list(compound), +atom, -list(compound)), zero_or_one).
	:- info(normalize_polygon_orientation/3, [
		comment is 'Normalizes a polygon ring orientation to ``clockwise`` or ``counterclockwise``.',
		argnames is ['Polygon', 'Orientation', 'OrientedPolygon']
	]).

	:- public(clockwise_polygon/2).
	:- mode(clockwise_polygon(+list(compound), -list(compound)), zero_or_one).
	:- info(clockwise_polygon/2, [
		comment is 'Returns a polygon ring with clockwise orientation.',
		argnames is ['Polygon', 'ClockwisePolygon']
	]).

	:- public(counterclockwise_polygon/2).
	:- mode(counterclockwise_polygon(+list(compound), -list(compound)), zero_or_one).
	:- info(counterclockwise_polygon/2, [
		comment is 'Returns a polygon ring with counterclockwise orientation.',
		argnames is ['Polygon', 'CounterclockwisePolygon']
	]).

	:- public(is_valid_polygon/1).
	:- mode(is_valid_polygon(+list(compound)), zero_or_one).
	:- info(is_valid_polygon/1, [
		comment is 'True when a polygon has at least three valid coordinates after normalizing optional closure.',
		argnames is ['Polygon']
	]).

	:- public(bbox_contains/2).
	:- mode(bbox_contains(+compound, +compound), zero_or_one).
	:- info(bbox_contains/2, [
		comment is 'True when a coordinate is inside or on the boundary of a bounding box term ``bbox((MinLatitude,MinLongitude),(MaxLatitude,MaxLongitude))``.',
		argnames is ['BoundingBox', 'Coordinate']
	]).

	:- public(bbox_intersects/2).
	:- mode(bbox_intersects(+compound, +compound), zero_or_one).
	:- info(bbox_intersects/2, [
		comment is 'True when two bounding boxes intersect or touch.',
		argnames is ['BoundingBox1', 'BoundingBox2']
	]).

	:- public(bbox_union/3).
	:- mode(bbox_union(+compound, +compound, -compound), zero_or_one).
	:- info(bbox_union/3, [
		comment is 'Computes the minimal bounding box containing two bounding boxes.',
		argnames is ['BoundingBox1', 'BoundingBox2', 'BoundingBox']
	]).

	:- public(bbox_expand/3).
	:- mode(bbox_expand(+compound, +number, -compound), zero_or_one).
	:- info(bbox_expand/3, [
		comment is 'Expands a bounding box by a distance in kilometers on all sides using a local spherical approximation.',
		argnames is ['BoundingBox', 'Distance', 'ExpandedBoundingBox']
	]).

	:- public(bbox_from_coordinates/2).
	:- mode(bbox_from_coordinates(+list(compound), -compound), zero_or_one).
	:- info(bbox_from_coordinates/2, [
		comment is 'Computes a bounding box from a list of one or more coordinates.',
		argnames is ['Coordinates', 'BoundingBox']
	]).

	:- public(point_to_polyline_distance/3).
	:- mode(point_to_polyline_distance(+compound, +list(compound), -float), zero_or_one).
	:- info(point_to_polyline_distance/3, [
		comment is 'Computes the minimum distance in kilometers from a coordinate to a polyline with two or more coordinates.',
		argnames is ['Point', 'Polyline', 'Distance']
	]).

	:- public(nearest_point_on_segment/4).
	:- mode(nearest_point_on_segment(+compound, +compound, +compound, -compound), zero_or_one).
	:- info(nearest_point_on_segment/4, [
		comment is 'Computes the nearest coordinate on a segment to a coordinate using a local equirectangular approximation.',
		argnames is ['Point', 'SegmentStart', 'SegmentEnd', 'NearestPoint']
	]).

	:- public(nearest_point_on_polyline/4).
	:- mode(nearest_point_on_polyline(+compound, +list(compound), -compound, -float), zero_or_one).
	:- info(nearest_point_on_polyline/4, [
		comment is 'Computes the nearest coordinate on a polyline to a coordinate and the corresponding distance in kilometers.',
		argnames is ['Point', 'Polyline', 'NearestPoint', 'Distance']
	]).

	:- public(polyline_length/2).
	:- mode(polyline_length(+list(compound), -float), zero_or_one).
	:- info(polyline_length/2, [
		comment is 'Computes the polyline length in kilometers for a list of two or more coordinates using the default ``haversine`` metric.',
		argnames is ['Coordinates', 'Length']
	]).

	:- public(polyline_length/3).
	:- mode(polyline_length(+list(compound), +atom, -float), zero_or_one).
	:- info(polyline_length/3, [
		comment is 'Computes the polyline length in kilometers for a list of two or more coordinates using the selected metric.',
		argnames is ['Coordinates', 'Metric', 'Length']
	]).

	:- public(polyline_simplify/3).
	:- mode(polyline_simplify(+list(compound), +number, -list(compound)), zero_or_one).
	:- info(polyline_simplify/3, [
		comment is 'Simplifies a polyline using the Douglas-Peucker algorithm with a tolerance in kilometers.',
		argnames is ['Coordinates', 'Tolerance', 'SimplifiedCoordinates']
	]).

	:- public(polyline_split_at_distance/4).
	:- mode(polyline_split_at_distance(+list(compound), +number, -list(compound), -list(compound)), zero_or_one).
	:- info(polyline_split_at_distance/4, [
		comment is 'Splits a polyline at a distance in kilometers from its first coordinate, returning left and right polylines that share the split coordinate.',
		argnames is ['Coordinates', 'Distance', 'LeftCoordinates', 'RightCoordinates']
	]).

	:- public(polyline_resample/3).
	:- mode(polyline_resample(+list(compound), +number, -list(compound)), zero_or_one).
	:- info(polyline_resample/3, [
		comment is 'Resamples a polyline using a fixed step in kilometers, preserving first and last coordinates.',
		argnames is ['Coordinates', 'Step', 'ResampledCoordinates']
	]).

	:- public(polygon_perimeter/2).
	:- mode(polygon_perimeter(+list(compound), -float), zero_or_one).
	:- info(polygon_perimeter/2, [
		comment is 'Computes the polygon perimeter in kilometers using the default ``haversine`` metric.',
		argnames is ['Polygon', 'Perimeter']
	]).

	:- public(polygon_perimeter/3).
	:- mode(polygon_perimeter(+list(compound), +atom, -float), zero_or_one).
	:- info(polygon_perimeter/3, [
		comment is 'Computes the polygon perimeter in kilometers using the selected metric.',
		argnames is ['Polygon', 'Metric', 'Perimeter']
	]).

	:- public(polygons_intersect/2).
	:- mode(polygons_intersect(+list(compound), +list(compound)), zero_or_one).
	:- info(polygons_intersect/2, [
		comment is 'True when two polygons intersect or one polygon is contained in the other.',
		argnames is ['Polygon1', 'Polygon2']
	]).

	:- public(bounding_box/3).
	:- mode(bounding_box(+compound, +positive_number, -compound), zero_or_one).
	:- info(bounding_box/3, [
		comment is 'Computes a spherical bounding box around a center coordinate for a given radius in kilometers. The returned bounding box term is ``bbox((MinLatitude,MinLongitude),(MaxLatitude,MaxLongitude))``.',
		argnames is ['Center', 'Radius', 'BoundingBox']
	]).

	:- public(route_distance/2).
	:- mode(route_distance(+list(compound), -float), zero_or_one).
	:- info(route_distance/2, [
		comment is 'Computes the route distance in kilometers for a list of two or more coordinates using the default ``haversine`` metric.',
		argnames is ['Coordinates', 'Distance']
	]).

	:- public(route_distance/3).
	:- mode(route_distance(+list(compound), +atom, -float), zero_or_one).
	:- info(route_distance/3, [
		comment is 'Computes the route distance in kilometers for a list of two or more coordinates using the selected metric. Supported metrics are ``haversine``, ``vincenty``, and ``rhumb``.',
		argnames is ['Coordinates', 'Metric', 'Distance']
	]).

	:- public(route_distance/4).
	:- mode(route_distance(+list(compound), +atom, +atom, -float), zero_or_one).
	:- info(route_distance/4, [
		comment is 'Computes the route distance for a list of two or more coordinates using the selected metric and output unit. Supported metrics are ``haversine``, ``vincenty``, and ``rhumb``. Valid ``Unit`` argument values are ``kilometers``, ``meters``, ``miles``, and ``nautical_miles``.',
		argnames is ['Coordinates', 'Metric', 'Unit', 'Distance']
	]).

:- end_protocol.
