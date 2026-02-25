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


:- object(geospatial,
	implements(geospatial_protocol)).

	:- info([
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2026-02-25,
		comment is 'Geospatial predicates over geographic coordinates represented as ``(Latitude,Longitude)``.',
		remarks is [
			'Distance unit' - 'Kilometers.',
			'Coordinate ranges' - 'Latitude values must be in the ``[-90.0,90.0]`` range and longitude values in the ``[-180.0,180.0]`` range.'
		],
		see_also is [geospatial_protocol, numberlist]
	]).

	:- uses(list, [
		append/3, length/2
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

	valid_coordinate((Latitude, Longitude)) :-
		number(Latitude),
		number(Longitude),
		Latitude >= -90.0,
		Latitude =< 90.0,
		Longitude >= -180.0,
		Longitude =< 180.0.

	normalize_coordinate((Latitude0, Longitude0), (Latitude, Longitude)) :-
		number(Latitude0),
		number(Longitude0),
		normalize_coordinate_angles(Latitude0, Longitude0, Latitude1, Longitude1),
		LongitudeRadians is Longitude1 * pi / 180.0,
		normalize_longitude(LongitudeRadians, LongitudeNormalizedRadians),
		Longitude is LongitudeNormalizedRadians * 180.0 / pi,
		Latitude = Latitude1.

	equirectangular_projection(Coordinate, ReferenceLatitude, X, Y) :-
		mean_earth_radius_km(MeanEarthRadiusKm),
		coordinate_radians(Coordinate, LatitudeRadians, LongitudeRadians),
		number(ReferenceLatitude),
		ReferenceLatitudeRadians is ReferenceLatitude * pi / 180.0,
		X is MeanEarthRadiusKm * LongitudeRadians * cos(ReferenceLatitudeRadians),
		Y is MeanEarthRadiusKm * LatitudeRadians.

	equirectangular_inverse(X, Y, ReferenceLatitude, Coordinate) :-
		mean_earth_radius_km(MeanEarthRadiusKm),
		number(X),
		number(Y),
		number(ReferenceLatitude),
		ReferenceLatitudeRadians is ReferenceLatitude * pi / 180.0,
		LatitudeRadians is Y / MeanEarthRadiusKm,
		LongitudeRadians0 is X / (MeanEarthRadiusKm * cos(ReferenceLatitudeRadians)),
		normalize_longitude(LongitudeRadians0, LongitudeRadians),
		Latitude is LatitudeRadians * 180.0 / pi,
		Longitude is LongitudeRadians * 180.0 / pi,
		normalize_coordinate((Latitude, Longitude), Coordinate).

	haversine_distance(Coordinate1, Coordinate2, Distance) :-
		mean_earth_radius_km(MeanEarthRadiusKm),
		coordinate_radians(Coordinate1, Latitude1, Longitude1),
		coordinate_radians(Coordinate2, Latitude2, Longitude2),
		DeltaLatitude is Latitude2 - Latitude1,
		DeltaLongitude is Longitude2 - Longitude1,
		HalfDeltaLatitude is DeltaLatitude / 2.0,
		HalfDeltaLongitude is DeltaLongitude / 2.0,
		SinHalfDeltaLatitude is sin(HalfDeltaLatitude),
		SinHalfDeltaLongitude is sin(HalfDeltaLongitude),
		A0 is SinHalfDeltaLatitude * SinHalfDeltaLatitude + cos(Latitude1) * cos(Latitude2) * SinHalfDeltaLongitude * SinHalfDeltaLongitude,
		A is min(1.0, max(0.0, A0)),
		C is 2.0 * atan2(sqrt(A), sqrt(1.0 - A)),
		Distance is MeanEarthRadiusKm * C.

	vincenty_distance(Coordinate1, Coordinate2, Distance) :-
		wgs84_a(WGS84A),
		wgs84_b(WGS84B),
		wgs84_f(WGS84F),
		coordinate_radians(Coordinate1, Latitude1, Longitude1),
		coordinate_radians(Coordinate2, Latitude2, Longitude2),
		L is Longitude2 - Longitude1,
		U1 is atan((1.0 - WGS84F) * tan(Latitude1)),
		U2 is atan((1.0 - WGS84F) * tan(Latitude2)),
		SinU1 is sin(U1),
		CosU1 is cos(U1),
		SinU2 is sin(U2),
		CosU2 is cos(U2),
		vincenty_lambda(
			WGS84F,
			L,
			L,
			SinU1, CosU1,
			SinU2, CosU2,
			100,
			Sigma,
			SinSigma,
			CosSigma,
			CosSquaredAlpha,
			Cos2SigmaM
		),
		BSquared is WGS84B * WGS84B,
		USquared is CosSquaredAlpha * ((WGS84A * WGS84A - BSquared) / BSquared),
		CoefficientA is 1.0 + USquared / 16384.0 * (4096.0 + USquared * (-768.0 + USquared * (320.0 - 175.0 * USquared))),
		CoefficientB is USquared / 1024.0 * (256.0 + USquared * (-128.0 + USquared * (74.0 - 47.0 * USquared))),
		SinSigmaSquared is SinSigma * SinSigma,
		Cos2SigmaMSquared is Cos2SigmaM * Cos2SigmaM,
		DeltaSigma is CoefficientB * SinSigma * (Cos2SigmaM + CoefficientB / 4.0 * (CosSigma * (-1.0 + 2.0 * Cos2SigmaMSquared) - CoefficientB / 6.0 * Cos2SigmaM * (-3.0 + 4.0 * SinSigmaSquared) * (-3.0 + 4.0 * Cos2SigmaMSquared))),
		Distance is WGS84B * CoefficientA * (Sigma - DeltaSigma).

	rhumb_distance(Coordinate1, Coordinate2, Distance) :-
		mean_earth_radius_km(MeanEarthRadiusKm),
		coordinate_radians(Coordinate1, Latitude1, Longitude1),
		coordinate_radians(Coordinate2, Latitude2, Longitude2),
		DeltaLatitude is Latitude2 - Latitude1,
		DeltaLongitude0 is Longitude2 - Longitude1,
		normalize_longitude(DeltaLongitude0, DeltaLongitude),
		mercator_latitude(Latitude1, Psi1),
		mercator_latitude(Latitude2, Psi2),
		DeltaPsi is Psi2 - Psi1,
		(	abs(DeltaPsi) =< 1.0e-12 ->
			Q is cos(Latitude1)
		;	Q is DeltaLatitude / DeltaPsi
		),
		AngularDistance is sqrt(DeltaLatitude * DeltaLatitude + Q * Q * DeltaLongitude * DeltaLongitude),
		Distance is MeanEarthRadiusKm * AngularDistance.

	rhumb_bearing(Coordinate1, Coordinate2, Bearing) :-
		coordinate_radians(Coordinate1, Latitude1, Longitude1),
		coordinate_radians(Coordinate2, Latitude2, Longitude2),
		DeltaLongitude0 is Longitude2 - Longitude1,
		normalize_longitude(DeltaLongitude0, DeltaLongitude),
		mercator_latitude(Latitude1, Psi1),
		mercator_latitude(Latitude2, Psi2),
		DeltaPsi is Psi2 - Psi1,
		BearingRadians is atan2(DeltaLongitude, DeltaPsi),
		BearingDegrees is BearingRadians * 180.0 / pi,
		normalize_bearing(BearingDegrees, Bearing).

	rhumb_destination_point(Start, Bearing, Distance, Destination) :-
		mean_earth_radius_km(MeanEarthRadiusKm),
		coordinate_radians(Start, Latitude1, Longitude1),
		number(Bearing),
		number(Distance),
		Distance >= 0.0,
		BearingRadians is Bearing * pi / 180.0,
		Delta is Distance / MeanEarthRadiusKm,
		DeltaLatitude is Delta * cos(BearingRadians),
		Latitude2 is Latitude1 + DeltaLatitude,
		mercator_latitude(Latitude1, Psi1),
		mercator_latitude(Latitude2, Psi2),
		DeltaPsi is Psi2 - Psi1,
		(	abs(DeltaPsi) =< 1.0e-12 ->
			Q is cos(Latitude1)
		;	Q is DeltaLatitude / DeltaPsi
		),
		abs(Q) > 1.0e-15,
		DeltaLongitude is Delta * sin(BearingRadians) / Q,
		Longitude2_0 is Longitude1 + DeltaLongitude,
		normalize_longitude(Longitude2_0, Longitude2),
		LatitudeDegrees is Latitude2 * 180.0 / pi,
		LongitudeDegrees is Longitude2 * 180.0 / pi,
		normalize_coordinate((LatitudeDegrees, LongitudeDegrees), Destination).

	interpolate_rhumb(Coordinate1, Coordinate2, Fraction, Coordinate) :-
		number(Fraction),
		Fraction >= 0.0,
		Fraction =< 1.0,
		rhumb_distance(Coordinate1, Coordinate2, Distance),
		rhumb_bearing(Coordinate1, Coordinate2, Bearing),
		PartialDistance is Fraction * Distance,
		rhumb_destination_point(Coordinate1, Bearing, PartialDistance, Coordinate).

	rhumb_midpoint(Coordinate1, Coordinate2, Midpoint) :-
		interpolate_rhumb(Coordinate1, Coordinate2, 0.5, Midpoint).

	distance(Coordinate1, Coordinate2, haversine, Distance) :-
		!,
		haversine_distance(Coordinate1, Coordinate2, Distance).
	distance(Coordinate1, Coordinate2, great_circle, Distance) :-
		!,
		haversine_distance(Coordinate1, Coordinate2, Distance).
	distance(Coordinate1, Coordinate2, vincenty, Distance) :-
		!,
		vincenty_distance(Coordinate1, Coordinate2, Distance).
	distance(Coordinate1, Coordinate2, geodesic, Distance) :-
		!,
		vincenty_distance(Coordinate1, Coordinate2, Distance).
	distance(Coordinate1, Coordinate2, rhumb, Distance) :-
		!,
		rhumb_distance(Coordinate1, Coordinate2, Distance).
	distance(Coordinate1, Coordinate2, loxodrome, Distance) :-
		!,
		rhumb_distance(Coordinate1, Coordinate2, Distance).

	distance(Coordinate1, Coordinate2, Metric, Unit, Distance) :-
		distance(Coordinate1, Coordinate2, Metric, DistanceKm),
		convert_km(DistanceKm, Unit, Distance).

	initial_bearing(Coordinate1, Coordinate2, Bearing) :-
		coordinate_radians(Coordinate1, Latitude1, Longitude1),
		coordinate_radians(Coordinate2, Latitude2, Longitude2),
		DeltaLongitude is Longitude2 - Longitude1,
		Y is sin(DeltaLongitude) * cos(Latitude2),
		X is cos(Latitude1) * sin(Latitude2) - sin(Latitude1) * cos(Latitude2) * cos(DeltaLongitude),
		InitialBearing is atan2(Y, X),
		InitialBearingDegrees is InitialBearing * 180.0 / pi,
		normalize_bearing(InitialBearingDegrees, Bearing).

	final_bearing(Coordinate1, Coordinate2, Bearing) :-
		initial_bearing(Coordinate2, Coordinate1, ReverseBearing),
		Bearing0 is ReverseBearing + 180.0,
		normalize_bearing(Bearing0, Bearing).

	midpoint(Coordinate1, Coordinate2, Midpoint) :-
		coordinate_radians(Coordinate1, Latitude1, Longitude1),
		coordinate_radians(Coordinate2, Latitude2, Longitude2),
		DeltaLongitude is Longitude2 - Longitude1,
		BX is cos(Latitude2) * cos(DeltaLongitude),
		BY is cos(Latitude2) * sin(DeltaLongitude),
		Latitude3 is atan2(
			sin(Latitude1) + sin(Latitude2),
			sqrt((cos(Latitude1) + BX) * (cos(Latitude1) + BX) + BY * BY)
		),
		Longitude3_0 is Longitude1 + atan2(BY, cos(Latitude1) + BX),
		normalize_longitude(Longitude3_0, Longitude3),
		Latitude3Degrees is Latitude3 * 180.0 / pi,
		Longitude3Degrees is Longitude3 * 180.0 / pi,
		Midpoint = (Latitude3Degrees, Longitude3Degrees).

	destination_point(Start, Bearing, Distance, Destination) :-
		mean_earth_radius_km(MeanEarthRadiusKm),
		coordinate_radians(Start, Latitude1, Longitude1),
		number(Bearing),
		number(Distance),
		Distance >= 0.0,
		BearingRadians is Bearing * pi / 180.0,
		AngularDistance is Distance / MeanEarthRadiusKm,
		Latitude2 is asin(sin(Latitude1) * cos(AngularDistance) + cos(Latitude1) * sin(AngularDistance) * cos(BearingRadians)),
		Longitude2_0 is Longitude1 + atan2(
			sin(BearingRadians) * sin(AngularDistance) * cos(Latitude1),
			cos(AngularDistance) - sin(Latitude1) * sin(Latitude2)
		),
		normalize_longitude(Longitude2_0, Longitude2),
		Latitude2Degrees is Latitude2 * 180.0 / pi,
		Longitude2Degrees is Longitude2 * 180.0 / pi,
		Destination = (Latitude2Degrees, Longitude2Degrees).

	interpolate_great_circle(Coordinate1, Coordinate2, Fraction, Coordinate) :-
		number(Fraction),
		Fraction >= 0.0,
		Fraction =< 1.0,
		haversine_distance(Coordinate1, Coordinate2, DistanceKm),
		mean_earth_radius_km(MeanEarthRadiusKm),
		Delta is DistanceKm / MeanEarthRadiusKm,
		(	abs(Delta) =< 1.0e-12 ->
			Coordinate = Coordinate1
		;	coordinate_radians(Coordinate1, Latitude1, Longitude1),
			coordinate_radians(Coordinate2, Latitude2, Longitude2),
			SinDelta is sin(Delta),
			Weight1 is sin((1.0 - Fraction) * Delta) / SinDelta,
			Weight2 is sin(Fraction * Delta) / SinDelta,
			X is Weight1 * cos(Latitude1) * cos(Longitude1) + Weight2 * cos(Latitude2) * cos(Longitude2),
			Y is Weight1 * cos(Latitude1) * sin(Longitude1) + Weight2 * cos(Latitude2) * sin(Longitude2),
			Z is Weight1 * sin(Latitude1) + Weight2 * sin(Latitude2),
			Latitude is atan2(Z, sqrt(X * X + Y * Y)),
			Longitude0 is atan2(Y, X),
			normalize_longitude(Longitude0, Longitude),
			LatitudeDegrees is Latitude * 180.0 / pi,
			LongitudeDegrees is Longitude * 180.0 / pi,
			Coordinate = (LatitudeDegrees, LongitudeDegrees)
		).

	cross_track_distance(Coordinate, Start, End, Distance) :-
		mean_earth_radius_km(MeanEarthRadiusKm),
		haversine_distance(Start, Coordinate, Distance13Km),
		Delta13 is Distance13Km / MeanEarthRadiusKm,
		initial_bearing(Start, Coordinate, Bearing13Degrees),
		initial_bearing(Start, End, Bearing12Degrees),
		Bearing13 is Bearing13Degrees * pi / 180.0,
		Bearing12 is Bearing12Degrees * pi / 180.0,
		DeltaCrossTrack is asin(sin(Delta13) * sin(Bearing13 - Bearing12)),
		Distance is MeanEarthRadiusKm * DeltaCrossTrack.

	along_track_distance(Coordinate, Start, End, Distance) :-
		mean_earth_radius_km(MeanEarthRadiusKm),
		haversine_distance(Start, Coordinate, Distance13Km),
		Delta13 is Distance13Km / MeanEarthRadiusKm,
		initial_bearing(Start, Coordinate, Bearing13Degrees),
		initial_bearing(Start, End, Bearing12Degrees),
		Bearing13 is Bearing13Degrees * pi / 180.0,
		Bearing12 is Bearing12Degrees * pi / 180.0,
		DeltaCrossTrack is asin(sin(Delta13) * sin(Bearing13 - Bearing12)),
		CosDeltaCrossTrack is cos(DeltaCrossTrack),
		abs(CosDeltaCrossTrack) > 1.0e-15,
		CosAlongTrack0 is cos(Delta13) / CosDeltaCrossTrack,
		CosAlongTrack is min(1.0, max(-1.0, CosAlongTrack0)),
		DeltaAlongTrack0 is acos(CosAlongTrack),
		(	cos(Bearing13 - Bearing12) < 0.0 ->
			DeltaAlongTrack is -DeltaAlongTrack0
		;	DeltaAlongTrack = DeltaAlongTrack0
		),
		Distance is MeanEarthRadiusKm * DeltaAlongTrack.

	within_distance(Coordinate1, Coordinate2, Radius, Metric) :-
		number(Radius),
		Radius >= 0.0,
		distance(Coordinate1, Coordinate2, Metric, Distance),
		Distance =< Radius.

	nearest_coordinate(Origin, [Coordinate| Coordinates], Metric, Nearest, Distance) :-
		valid_coordinate(Origin),
		valid_coordinate(Coordinate),
		distance(Origin, Coordinate, Metric, Distance0),
		nearest_coordinate(Coordinates, Origin, Metric, Coordinate, Distance0, Nearest, Distance).

	normalize_coordinate_angles(Latitude0, Longitude0, Latitude, Longitude) :-
		(	Latitude0 > 90.0 ->
			Latitude1 is 180.0 - Latitude0,
			Longitude1 is Longitude0 + 180.0,
			normalize_coordinate_angles(Latitude1, Longitude1, Latitude, Longitude)
		;	Latitude0 < -90.0 ->
			Latitude1 is -180.0 - Latitude0,
			Longitude1 is Longitude0 + 180.0,
			normalize_coordinate_angles(Latitude1, Longitude1, Latitude, Longitude)
		;	Latitude = Latitude0,
			Longitude = Longitude0
		).

	mean_center([Coordinate| Coordinates], Center) :-
		valid_coordinate(Coordinate),
		Coordinate = (Latitude0, Longitude0),
		mean_center(Coordinates, Latitude0, Longitude0, 1, Latitude, Longitude),
		Center = (Latitude, Longitude).

	minimum_enclosing_circle([Coordinate| Coordinates], Center, Radius) :-
		all_valid_coordinates([Coordinate| Coordinates]),
		mean_latitude([Coordinate| Coordinates], MeanLatitude),
		project_coordinates([Coordinate| Coordinates], MeanLatitude, ProjectedCoordinates),
		minimum_enclosing_circle_projected(ProjectedCoordinates, CenterX, CenterY, Radius),
		equirectangular_inverse(CenterX, CenterY, MeanLatitude, Center),
		!.

	coordinates_bounding_box([Coordinate| Coordinates], BoundingBox) :-
		valid_coordinate(Coordinate),
		Coordinate = (MinLatitude0, MinLongitude0),
		coordinates_min_max(Coordinates, MinLatitude0, MinLongitude0, MinLatitude0, MinLongitude0, MinLatitude, MinLongitude, MaxLatitude, MaxLongitude),
		BoundingBox = bbox((MinLatitude, MinLongitude), (MaxLatitude, MaxLongitude)).

	point_in_polygon(Point, Polygon) :-
		valid_coordinate(Point),
		normalize_polygon(Polygon, NormalizedPolygon),
		(	point_on_polygon_boundary(Point, NormalizedPolygon) ->
			true
		;	count_polygon_ray_crossings(Point, NormalizedPolygon, Crossings),
			1 is Crossings mod 2
		),
		!.

	polygon_area(Polygon, Area) :-
		normalize_polygon(Polygon, NormalizedPolygon),
		mean_latitude(NormalizedPolygon, MeanLatitude),
		project_coordinates(NormalizedPolygon, MeanLatitude, Projected),
		polygon_ring_sum(Projected, RingSum),
		Area is abs(RingSum) / 2.0.

	polygon_centroid(Polygon, Centroid) :-
		normalize_polygon(Polygon, NormalizedPolygon),
		mean_latitude(NormalizedPolygon, MeanLatitude),
		project_coordinates(NormalizedPolygon, MeanLatitude, Projected),
		polygon_centroid_projected(Projected, CentroidX, CentroidY),
		mean_earth_radius_km(MeanEarthRadiusKm),
		MeanLatitudeRadians is MeanLatitude * pi / 180.0,
		LatitudeRadians is CentroidY / MeanEarthRadiusKm,
		LongitudeRadians is CentroidX / (MeanEarthRadiusKm * cos(MeanLatitudeRadians)),
		normalize_longitude(LongitudeRadians, NormalizedLongitude),
		Latitude is LatitudeRadians * 180.0 / pi,
		Longitude is NormalizedLongitude * 180.0 / pi,
		Centroid = (Latitude, Longitude).

	polygon_bounding_box(Polygon, BoundingBox) :-
		normalize_polygon(Polygon, [(MinLatitude0, MinLongitude0)| Coordinates]),
		polygon_min_max(Coordinates, MinLatitude0, MinLongitude0, MinLatitude0, MinLongitude0, MinLatitude, MinLongitude, MaxLatitude, MaxLongitude),
		BoundingBox = bbox((MinLatitude, MinLongitude), (MaxLatitude, MaxLongitude)).

	close_polygon(Polygon, ClosedPolygon) :-
		normalize_polygon(Polygon, NormalizedPolygon),
		NormalizedPolygon = [First| _],
		append(NormalizedPolygon, [First], ClosedPolygon).

	polygon_orientation(Polygon, Orientation) :-
		normalize_polygon(Polygon, NormalizedPolygon),
		mean_latitude(NormalizedPolygon, MeanLatitude),
		project_coordinates(NormalizedPolygon, MeanLatitude, Projected),
		polygon_ring_sum(Projected, RingSum),
		(	RingSum < 0.0 ->
			Orientation = clockwise
		;	Orientation = counterclockwise
		).

	is_clockwise_polygon(Polygon) :-
		polygon_orientation(Polygon, clockwise).

	normalize_polygon_orientation(Polygon, clockwise, OrientedPolygon) :-
		!,
		normalize_polygon(Polygon, NormalizedPolygon),
		polygon_orientation(NormalizedPolygon, CurrentOrientation),
		(	CurrentOrientation == clockwise ->
			OrientedPolygon = NormalizedPolygon
		;	reverse_list(NormalizedPolygon, OrientedPolygon)
		).
	normalize_polygon_orientation(Polygon, counterclockwise, OrientedPolygon) :-
		!,
		normalize_polygon(Polygon, NormalizedPolygon),
		polygon_orientation(NormalizedPolygon, CurrentOrientation),
		(	CurrentOrientation == counterclockwise ->
			OrientedPolygon = NormalizedPolygon
		;	reverse_list(NormalizedPolygon, OrientedPolygon)
		).

	clockwise_polygon(Polygon, ClockwisePolygon) :-
		normalize_polygon_orientation(Polygon, clockwise, ClockwisePolygon).

	counterclockwise_polygon(Polygon, CounterclockwisePolygon) :-
		normalize_polygon_orientation(Polygon, counterclockwise, CounterclockwisePolygon).

	is_valid_polygon(Polygon) :-
		normalize_polygon(Polygon, _).

	bbox_contains(BoundingBox, Coordinate) :-
		valid_coordinate(Coordinate),
		BoundingBox = bbox((MinLatitude, MinLongitude), (MaxLatitude, MaxLongitude)),
		number(MinLatitude), number(MinLongitude),
		number(MaxLatitude), number(MaxLongitude),
		Coordinate = (Latitude, Longitude),
		Latitude >= MinLatitude,
		Latitude =< MaxLatitude,
		Longitude >= MinLongitude,
		Longitude =< MaxLongitude.

	bbox_intersects(BoundingBox1, BoundingBox2) :-
		BoundingBox1 = bbox((MinLatitude1, MinLongitude1), (MaxLatitude1, MaxLongitude1)),
		BoundingBox2 = bbox((MinLatitude2, MinLongitude2), (MaxLatitude2, MaxLongitude2)),
		number(MinLatitude1), number(MinLongitude1), number(MaxLatitude1), number(MaxLongitude1),
		number(MinLatitude2), number(MinLongitude2), number(MaxLatitude2), number(MaxLongitude2),
		MaxLatitude1 >= MinLatitude2,
		MaxLatitude2 >= MinLatitude1,
		MaxLongitude1 >= MinLongitude2,
		MaxLongitude2 >= MinLongitude1.

	bbox_union(BoundingBox1, BoundingBox2, BoundingBox) :-
		BoundingBox1 = bbox((MinLatitude1, MinLongitude1), (MaxLatitude1, MaxLongitude1)),
		BoundingBox2 = bbox((MinLatitude2, MinLongitude2), (MaxLatitude2, MaxLongitude2)),
		number(MinLatitude1), number(MinLongitude1), number(MaxLatitude1), number(MaxLongitude1),
		number(MinLatitude2), number(MinLongitude2), number(MaxLatitude2), number(MaxLongitude2),
		MinLatitude is min(MinLatitude1, MinLatitude2),
		MinLongitude is min(MinLongitude1, MinLongitude2),
		MaxLatitude is max(MaxLatitude1, MaxLatitude2),
		MaxLongitude is max(MaxLongitude1, MaxLongitude2),
		BoundingBox = bbox((MinLatitude, MinLongitude), (MaxLatitude, MaxLongitude)).

	bbox_expand(BoundingBox, Distance, ExpandedBoundingBox) :-
		BoundingBox = bbox((MinLatitude0, MinLongitude0), (MaxLatitude0, MaxLongitude0)),
		number(MinLatitude0), number(MinLongitude0), number(MaxLatitude0), number(MaxLongitude0),
		number(Distance),
		Distance >= 0.0,
		mean_earth_radius_km(MeanEarthRadiusKm),
		LatitudeDelta is Distance * 180.0 / (pi * MeanEarthRadiusKm),
		MeanLatitude is (MinLatitude0 + MaxLatitude0) / 2.0,
		MeanLatitudeRadians is MeanLatitude * pi / 180.0,
		CosLatitude is cos(MeanLatitudeRadians),
		abs(CosLatitude) > 1.0e-12,
		LongitudeDelta is Distance * 180.0 / (pi * MeanEarthRadiusKm * CosLatitude),
		MinLatitude is max(-90.0, MinLatitude0 - LatitudeDelta),
		MaxLatitude is min(90.0, MaxLatitude0 + LatitudeDelta),
		MinLongitudePre is MinLongitude0 - LongitudeDelta,
		MaxLongitudePre is MaxLongitude0 + LongitudeDelta,
		normalize_coordinate((MeanLatitude, MinLongitudePre), (_, MinLongitude)),
		normalize_coordinate((MeanLatitude, MaxLongitudePre), (_, MaxLongitude)),
		ExpandedBoundingBox = bbox((MinLatitude, MinLongitude), (MaxLatitude, MaxLongitude)).

	bbox_from_coordinates(Coordinates, BoundingBox) :-
		coordinates_bounding_box(Coordinates, BoundingBox).

	point_to_polyline_distance(Point, [Coordinate1, Coordinate2| Coordinates], Distance) :-
		valid_coordinate(Point),
		valid_coordinate(Coordinate1),
		nearest_point_on_polyline(Point, [Coordinate1, Coordinate2| Coordinates], _, Distance).

	nearest_point_on_segment(Point, SegmentStart, SegmentEnd, NearestPoint) :-
		point_segment_projection(Point, SegmentStart, SegmentEnd, NearestPoint, _).

	nearest_point_on_polyline(Point, [Coordinate1, Coordinate2| Coordinates], NearestPoint, Distance) :-
		valid_coordinate(Point),
		valid_coordinate(Coordinate1),
		valid_coordinate(Coordinate2),
		point_segment_projection(Point, Coordinate1, Coordinate2, ProjectionPoint0, Distance0),
		nearest_point_on_polyline(Coordinates, Point, Coordinate2, ProjectionPoint0, Distance0, NearestPoint, Distance).

	polyline_length(Coordinates, Length) :-
		polyline_length(Coordinates, haversine, Length).

	polyline_length(Coordinates, Metric, Length) :-
		route_distance(Coordinates, Metric, Length).

	polyline_simplify([Coordinate1, Coordinate2| Coordinates], Tolerance, SimplifiedCoordinates) :-
		number(Tolerance),
		Tolerance >= 0.0,
		all_valid_coordinates([Coordinate1, Coordinate2| Coordinates]),
		douglas_peucker([Coordinate1, Coordinate2| Coordinates], Tolerance, SimplifiedCoordinates),
		!.

	polyline_split_at_distance([Coordinate1, Coordinate2| Coordinates], Distance, LeftCoordinates, RightCoordinates) :-
		number(Distance),
		Distance >= 0.0,
		valid_coordinate(Coordinate1),
		valid_coordinate(Coordinate2),
		(	Distance =< 0.0 ->
			LeftCoordinates = [Coordinate1],
			RightCoordinates = [Coordinate1, Coordinate2| Coordinates]
		;	polyline_split_at_distance([Coordinate1, Coordinate2| Coordinates], Distance, [Coordinate1], LeftCoordinates, RightCoordinates)
		),
		!.

	polyline_resample([Coordinate1, Coordinate2| Coordinates], Step, ResampledCoordinates) :-
		number(Step),
		Step > 0.0,
		all_valid_coordinates([Coordinate1, Coordinate2| Coordinates]),
		polyline_resample([Coordinate1, Coordinate2| Coordinates], Step, [Coordinate1], ResampledCoordinates),
		!.

	polygon_perimeter(Polygon, Perimeter) :-
		polygon_perimeter(Polygon, haversine, Perimeter).

	polygon_perimeter(Polygon, Metric, Perimeter) :-
		normalize_polygon(Polygon, NormalizedPolygon),
		last_polygon_coordinate(NormalizedPolygon, Last),
		polygon_perimeter(NormalizedPolygon, Metric, Last, 0.0, Perimeter).

	polygons_intersect(Polygon1, Polygon2) :-
		normalize_polygon(Polygon1, NormalizedPolygon1),
		normalize_polygon(Polygon2, NormalizedPolygon2),
		(	polygon_edges_intersect(NormalizedPolygon1, NormalizedPolygon2) ->
			true
		;	NormalizedPolygon1 = [Point1| _],
			point_in_polygon(Point1, NormalizedPolygon2)
		;	NormalizedPolygon2 = [Point2| _],
			point_in_polygon(Point2, NormalizedPolygon1)
		).

	bounding_box(Center, Radius, BoundingBox) :-
		mean_earth_radius_km(MeanEarthRadiusKm),
		coordinate_radians(Center, Latitude, Longitude),
		number(Radius),
		Radius > 0.0,
		HalfEarthCircumference is pi * MeanEarthRadiusKm,
		(	Radius >= HalfEarthCircumference ->
			BoundingBox = bbox((-90.0, -180.0), (90.0, 180.0))
		;	AngularRadius is Radius / MeanEarthRadiusKm,
			MinLatitude0 is Latitude - AngularRadius,
			MaxLatitude0 is Latitude + AngularRadius,
			MinLatitude is max(MinLatitude0, -pi / 2.0),
			MaxLatitude is min(MaxLatitude0, pi / 2.0),
			(	MinLatitude =< -pi / 2.0 ; MaxLatitude >= pi / 2.0 ->
				MinLongitude = -pi,
				MaxLongitude = pi
			;	DeltaLongitude is asin(sin(AngularRadius) / cos(Latitude)),
				MinLongitude0 is Longitude - DeltaLongitude,
				MaxLongitude0 is Longitude + DeltaLongitude,
				normalize_longitude(MinLongitude0, MinLongitude),
				normalize_longitude(MaxLongitude0, MaxLongitude)
			),
			MinLatitudeDegrees is MinLatitude * 180.0 / pi,
			MaxLatitudeDegrees is MaxLatitude * 180.0 / pi,
			MinLongitudeDegrees is MinLongitude * 180.0 / pi,
			MaxLongitudeDegrees is MaxLongitude * 180.0 / pi,
			BoundingBox = bbox(
				(MinLatitudeDegrees, MinLongitudeDegrees),
				(MaxLatitudeDegrees, MaxLongitudeDegrees)
			)
		).

	route_distance(Coordinates, Distance) :-
		route_distance(Coordinates, haversine, Distance).

	route_distance([Coordinate1, Coordinate2| Coordinates], Metric, Distance) :-
		valid_coordinate(Coordinate1),
		route_distance([Coordinate2| Coordinates], Metric, Coordinate1, 0.0, Distance).

	route_distance(Coordinates, Metric, Unit, Distance) :-
		route_distance(Coordinates, Metric, DistanceKm),
		convert_km(DistanceKm, Unit, Distance).

	route_distance([Coordinate| Coordinates], Metric, Previous, Accumulator, Distance) :-
		valid_coordinate(Coordinate),
		distance(Previous, Coordinate, Metric, SegmentDistance),
		Accumulator2 is Accumulator + SegmentDistance,
		route_distance(Coordinates, Metric, Coordinate, Accumulator2, Distance).
	route_distance([], _, _, Distance, Distance).

	nearest_point_on_polyline([], _, _, BestPoint, BestDistance, BestPoint, BestDistance).
	nearest_point_on_polyline([Coordinate| Coordinates], Point, Previous, BestPoint0, BestDistance0, BestPoint, BestDistance) :-
		valid_coordinate(Coordinate),
		point_segment_projection(Point, Previous, Coordinate, ProjectionPoint, Distance),
		(	Distance < BestDistance0 ->
			BestPoint1 = ProjectionPoint,
			BestDistance1 = Distance
		;	BestPoint1 = BestPoint0,
			BestDistance1 = BestDistance0
		),
		nearest_point_on_polyline(Coordinates, Point, Coordinate, BestPoint1, BestDistance1, BestPoint, BestDistance).

	polyline_split_at_distance([Coordinate1, Coordinate2| Coordinates], RemainingDistance, LeftAccumulator, LeftCoordinates, RightCoordinates) :-
		haversine_distance(Coordinate1, Coordinate2, SegmentDistance),
		(	SegmentDistance =< 1.0e-12 ->
			NextRemainingDistance is RemainingDistance,
			append(LeftAccumulator, [Coordinate2], NextLeftAccumulator),
			polyline_split_at_distance([Coordinate2| Coordinates], NextRemainingDistance, NextLeftAccumulator, LeftCoordinates, RightCoordinates)
		;	RemainingDistance < SegmentDistance ->
			Fraction is RemainingDistance / SegmentDistance,
			interpolate_great_circle(Coordinate1, Coordinate2, Fraction, SplitCoordinate),
			append(LeftAccumulator, [SplitCoordinate], LeftCoordinates),
			RightCoordinates = [SplitCoordinate, Coordinate2| Coordinates]
		;	RemainingDistance =:= SegmentDistance ->
			append(LeftAccumulator, [Coordinate2], LeftCoordinates),
			( Coordinates == [] ->
				RightCoordinates = [Coordinate2]
			;	RightCoordinates = [Coordinate2| Coordinates]
			)
		;	RemainingDistance > SegmentDistance,
			Coordinates \== [] ->
			NextRemainingDistance is RemainingDistance - SegmentDistance,
			append(LeftAccumulator, [Coordinate2], NextLeftAccumulator),
			polyline_split_at_distance([Coordinate2| Coordinates], NextRemainingDistance, NextLeftAccumulator, LeftCoordinates, RightCoordinates)
		; append(LeftAccumulator, [Coordinate2], LeftCoordinates),
			RightCoordinates = [Coordinate2]
		).

	polyline_resample(Coordinates, Step, Accumulator, ResampledCoordinates) :-
		route_distance(Coordinates, haversine, RemainingDistance),
		(	RemainingDistance =< Step ->
			last_polygon_coordinate(Coordinates, LastCoordinate),
			append(Accumulator, [LastCoordinate], ResampledCoordinates)
		;	polyline_split_at_distance(Coordinates, Step, _, [NextCoordinate| RemainingCoordinates]),
			append(Accumulator, [NextCoordinate], NextAccumulator),
			polyline_resample([NextCoordinate| RemainingCoordinates], Step, NextAccumulator, ResampledCoordinates)
		).

	mean_center([], SumLatitude, SumLongitude, Count, Latitude, Longitude) :-
		Latitude is SumLatitude / Count,
		Longitude is SumLongitude / Count.
	mean_center([Coordinate| Coordinates], SumLatitude0, SumLongitude0, Count0, Latitude, Longitude) :-
		valid_coordinate(Coordinate),
		Coordinate = (CoordinateLatitude, CoordinateLongitude),
		SumLatitude1 is SumLatitude0 + CoordinateLatitude,
		SumLongitude1 is SumLongitude0 + CoordinateLongitude,
		Count1 is Count0 + 1,
		mean_center(Coordinates, SumLatitude1, SumLongitude1, Count1, Latitude, Longitude).

	coordinates_min_max([], MinLatitude, MinLongitude, MaxLatitude, MaxLongitude, MinLatitude, MinLongitude, MaxLatitude, MaxLongitude).
	coordinates_min_max([Coordinate| Coordinates], MinLatitude0, MinLongitude0, MaxLatitude0, MaxLongitude0, MinLatitude, MinLongitude, MaxLatitude, MaxLongitude) :-
		valid_coordinate(Coordinate),
		Coordinate = (CoordinateLatitude, CoordinateLongitude),
		MinLatitude1 is min(MinLatitude0, CoordinateLatitude),
		MinLongitude1 is min(MinLongitude0, CoordinateLongitude),
		MaxLatitude1 is max(MaxLatitude0, CoordinateLatitude),
		MaxLongitude1 is max(MaxLongitude0, CoordinateLongitude),
		coordinates_min_max(Coordinates, MinLatitude1, MinLongitude1, MaxLatitude1, MaxLongitude1, MinLatitude, MinLongitude, MaxLatitude, MaxLongitude).

	minimum_enclosing_circle_projected([X-Y| Points], CenterX, CenterY, Radius) :-
		LargeRadius is 1.0e300,
		update_best_circle([X-Y| Points], X, Y, 0.0, X, Y, LargeRadius, CenterX0, CenterY0, Radius0),
		minimum_enclosing_circle_pairs([X-Y| Points], [X-Y| Points], CenterX0, CenterY0, Radius0, CenterX1, CenterY1, Radius1),
		minimum_enclosing_circle_triples([X-Y| Points], [X-Y| Points], CenterX1, CenterY1, Radius1, CenterX, CenterY, Radius),
		Radius < LargeRadius.

	minimum_enclosing_circle_pairs([], _, CenterX, CenterY, Radius, CenterX, CenterY, Radius).
	minimum_enclosing_circle_pairs([Point1| Points], AllPoints, CenterX0, CenterY0, Radius0, CenterX, CenterY, Radius) :-
		minimum_enclosing_circle_pairs_with(Point1, Points, AllPoints, CenterX0, CenterY0, Radius0, CenterX1, CenterY1, Radius1),
		minimum_enclosing_circle_pairs(Points, AllPoints, CenterX1, CenterY1, Radius1, CenterX, CenterY, Radius).

	minimum_enclosing_circle_pairs_with(_, [], _, CenterX, CenterY, Radius, CenterX, CenterY, Radius).
	minimum_enclosing_circle_pairs_with(Point1, [Point2| Points], AllPoints, CenterX0, CenterY0, Radius0, CenterX, CenterY, Radius) :-
		circle_from_pair(Point1, Point2, CandidateCenterX, CandidateCenterY, CandidateRadius),
		update_best_circle(AllPoints, CandidateCenterX, CandidateCenterY, CandidateRadius, CenterX0, CenterY0, Radius0, CenterX1, CenterY1, Radius1),
		minimum_enclosing_circle_pairs_with(Point1, Points, AllPoints, CenterX1, CenterY1, Radius1, CenterX, CenterY, Radius).

	minimum_enclosing_circle_triples([], _, CenterX, CenterY, Radius, CenterX, CenterY, Radius).
	minimum_enclosing_circle_triples([Point1| Points], AllPoints, CenterX0, CenterY0, Radius0, CenterX, CenterY, Radius) :-
		minimum_enclosing_circle_triples_with_first(Point1, Points, AllPoints, CenterX0, CenterY0, Radius0, CenterX1, CenterY1, Radius1),
		minimum_enclosing_circle_triples(Points, AllPoints, CenterX1, CenterY1, Radius1, CenterX, CenterY, Radius).

	minimum_enclosing_circle_triples_with_first(_, [], _, CenterX, CenterY, Radius, CenterX, CenterY, Radius).
	minimum_enclosing_circle_triples_with_first(Point1, [Point2| Points], AllPoints, CenterX0, CenterY0, Radius0, CenterX, CenterY, Radius) :-
		minimum_enclosing_circle_triples_with_pair(Point1, Point2, Points, AllPoints, CenterX0, CenterY0, Radius0, CenterX1, CenterY1, Radius1),
		minimum_enclosing_circle_triples_with_first(Point1, Points, AllPoints, CenterX1, CenterY1, Radius1, CenterX, CenterY, Radius).

	minimum_enclosing_circle_triples_with_pair(_, _, [], _, CenterX, CenterY, Radius, CenterX, CenterY, Radius).
	minimum_enclosing_circle_triples_with_pair(Point1, Point2, [Point3| Points], AllPoints, CenterX0, CenterY0, Radius0, CenterX, CenterY, Radius) :-
		( circle_from_triple(Point1, Point2, Point3, CandidateCenterX, CandidateCenterY, CandidateRadius) ->
			update_best_circle(AllPoints, CandidateCenterX, CandidateCenterY, CandidateRadius, CenterX0, CenterY0, Radius0, CenterX1, CenterY1, Radius1)
		;	CenterX1 = CenterX0,
			CenterY1 = CenterY0,
			Radius1 = Radius0
		),
		minimum_enclosing_circle_triples_with_pair(Point1, Point2, Points, AllPoints, CenterX1, CenterY1, Radius1, CenterX, CenterY, Radius).

	update_best_circle(Points, CandidateCenterX, CandidateCenterY, CandidateRadius, CenterX0, CenterY0, Radius0, CenterX, CenterY, Radius) :-
		( CandidateRadius < Radius0,
			circle_contains_all_points(CandidateCenterX, CandidateCenterY, CandidateRadius, Points) ->
			CenterX = CandidateCenterX,
			CenterY = CandidateCenterY,
			Radius = CandidateRadius
		;	CenterX = CenterX0,
			CenterY = CenterY0,
			Radius = Radius0
		).

	circle_contains_all_points(_, _, _, []).
	circle_contains_all_points(CenterX, CenterY, Radius, [X-Y| Points]) :-
		point_inside_circle(X, Y, CenterX, CenterY, Radius),
		circle_contains_all_points(CenterX, CenterY, Radius, Points).

	point_inside_circle(X, Y, CenterX, CenterY, Radius) :-
		DeltaX is X - CenterX,
		DeltaY is Y - CenterY,
		DistanceSquared is DeltaX * DeltaX + DeltaY * DeltaY,
		Limit is Radius * Radius + 1.0e-9,
		DistanceSquared =< Limit.

	circle_from_pair(X1-Y1, X2-Y2, CenterX, CenterY, Radius) :-
		CenterX is (X1 + X2) / 2.0,
		CenterY is (Y1 + Y2) / 2.0,
		DeltaX is X1 - X2,
		DeltaY is Y1 - Y2,
		Radius is sqrt(DeltaX * DeltaX + DeltaY * DeltaY) / 2.0.

	circle_from_triple(X1-Y1, X2-Y2, X3-Y3, CenterX, CenterY, Radius) :-
		Determinant is 2.0 * (X1 * (Y2 - Y3) + X2 * (Y3 - Y1) + X3 * (Y1 - Y2)),
		abs(Determinant) > 1.0e-12,
		S1 is X1 * X1 + Y1 * Y1,
		S2 is X2 * X2 + Y2 * Y2,
		S3 is X3 * X3 + Y3 * Y3,
		CenterX is (S1 * (Y2 - Y3) + S2 * (Y3 - Y1) + S3 * (Y1 - Y2)) / Determinant,
		CenterY is (S1 * (X3 - X2) + S2 * (X1 - X3) + S3 * (X2 - X1)) / Determinant,
		DeltaX is X1 - CenterX,
		DeltaY is Y1 - CenterY,
		Radius is sqrt(DeltaX * DeltaX + DeltaY * DeltaY).

	douglas_peucker([Coordinate1, Coordinate2], _, [Coordinate1, Coordinate2]) :-
		!.
	douglas_peucker(Coordinates, Tolerance, SimplifiedCoordinates) :-
		Coordinates = [Start| _],
		last_polygon_coordinate(Coordinates, End),
		maximum_perpendicular_distance(Coordinates, Start, End, Index, MaximumDistance),
		(	number(MaximumDistance), MaximumDistance > Tolerance ->
			split_polyline_at(Coordinates, Index, LeftCoordinates, RightCoordinates),
			douglas_peucker(LeftCoordinates, Tolerance, LeftSimplified),
			douglas_peucker(RightCoordinates, Tolerance, RightSimplified),
			append_without_duplicate_last(LeftSimplified, RightSimplified, SimplifiedCoordinates)
		;	SimplifiedCoordinates = [Start, End]
		).

	maximum_perpendicular_distance(Coordinates, Start, End, Index, Distance) :-
		length(Coordinates, Length),
		LastIndex is Length,
		maximum_perpendicular_distance(Coordinates, Start, End, 1, LastIndex, 2, -1.0, Index, Distance).

	maximum_perpendicular_distance([], _, _, _, _, BestIndex, BestDistance, BestIndex, BestDistance).
	maximum_perpendicular_distance([Coordinate| Coordinates], Start, End, Position, LastPosition, BestIndex0, BestDistance0, BestIndex, BestDistance) :-
		(	Position =< 1 ->
			BestIndex1 = BestIndex0,
			BestDistance1 = BestDistance0
		;	Position >= LastPosition ->
			BestIndex1 = BestIndex0,
			BestDistance1 = BestDistance0
		;	point_segment_distance(Coordinate, Start, End, Distance),
			(	number(Distance), Distance > BestDistance0 ->
				BestIndex1 = Position,
				BestDistance1 = Distance
			;	BestIndex1 = BestIndex0,
				BestDistance1 = BestDistance0
			)
		),
		Position1 is Position + 1,
		maximum_perpendicular_distance(Coordinates, Start, End, Position1, LastPosition, BestIndex1, BestDistance1, BestIndex, BestDistance).

	split_polyline_at([Coordinate| Coordinates], 1, [Coordinate], [Coordinate| Coordinates]) :-
		!.
	split_polyline_at([Coordinate| Coordinates], Index, [Coordinate| LeftCoordinates], RightCoordinates) :-
		Index > 1,
		Index1 is Index - 1,
		split_polyline_at(Coordinates, Index1, LeftCoordinates, RightCoordinates).

	append_without_duplicate_last(Left, Right, Combined) :-
		remove_last_polygon_coordinate(Left, LeftWithoutLast),
		append(LeftWithoutLast, Right, Combined).

	reverse_list(List, Reversed) :-
		reverse_list(List, [], Reversed).

	reverse_list([], Reversed, Reversed).
	reverse_list([Item| Items], Accumulator, Reversed) :-
		reverse_list(Items, [Item| Accumulator], Reversed).

	point_segment_distance(Point, SegmentStart, SegmentEnd, Distance) :-
		point_segment_projection(Point, SegmentStart, SegmentEnd, _, Distance).

	point_segment_projection(Point, SegmentStart, SegmentEnd, ProjectionPoint, Distance) :-
		segment_reference_latitude(SegmentStart, SegmentEnd, ReferenceLatitude),
		equirectangular_projection(SegmentStart, ReferenceLatitude, X1, Y1),
		equirectangular_projection(SegmentEnd, ReferenceLatitude, X2, Y2),
		equirectangular_projection(Point, ReferenceLatitude, X0, Y0),
		DeltaX is X2 - X1,
		DeltaY is Y2 - Y1,
		SegmentSquaredLength is DeltaX * DeltaX + DeltaY * DeltaY,
		(	SegmentSquaredLength =< 1.0e-12 ->
			Distance is sqrt((X0 - X1) * (X0 - X1) + (Y0 - Y1) * (Y0 - Y1)),
			equirectangular_inverse(X1, Y1, ReferenceLatitude, ProjectionPoint)
		;	Projection0 is ((X0 - X1) * DeltaX + (Y0 - Y1) * DeltaY) / SegmentSquaredLength,
			Projection is min(1.0, max(0.0, Projection0)),
			ClosestX is X1 + Projection * DeltaX,
			ClosestY is Y1 + Projection * DeltaY,
			Distance is sqrt((X0 - ClosestX) * (X0 - ClosestX) + (Y0 - ClosestY) * (Y0 - ClosestY)),
			equirectangular_inverse(ClosestX, ClosestY, ReferenceLatitude, ProjectionPoint)
		).

	segment_reference_latitude((Latitude1, _), (Latitude2, _), ReferenceLatitude) :-
		ReferenceLatitude is (Latitude1 + Latitude2) / 2.0.

	polygon_perimeter([], _, _, Perimeter, Perimeter).
	polygon_perimeter([Coordinate| Coordinates], Metric, Previous, Accumulator, Perimeter) :-
		distance(Previous, Coordinate, Metric, SegmentDistance),
		Accumulator2 is Accumulator + SegmentDistance,
		polygon_perimeter(Coordinates, Metric, Coordinate, Accumulator2, Perimeter).

	polygon_edges_intersect(Polygon1, Polygon2) :-
		last_polygon_coordinate(Polygon1, Last1),
		last_polygon_coordinate(Polygon2, Last2),
		polygon_edges_intersect(Polygon1, Last1, Polygon2, Last2).

	polygon_edges_intersect([], _, _, _) :-
		fail.
	polygon_edges_intersect([Coordinate1| Coordinates1], Previous1, Polygon2, Last2) :-
		(	edge_intersects_polygon(Previous1, Coordinate1, Polygon2, Last2) ->
			true
		;	polygon_edges_intersect(Coordinates1, Coordinate1, Polygon2, Last2)
		).

	edge_intersects_polygon(A1, A2, [B2| Bs], B1) :-
		(	segments_intersect(A1, A2, B1, B2) ->
			true
		;	edge_intersects_polygon(A1, A2, Bs, B2)
		).

	segments_intersect(P1, Q1, P2, Q2) :-
		orientation(P1, Q1, P2, O1),
		orientation(P1, Q1, Q2, O2),
		orientation(P2, Q2, P1, O3),
		orientation(P2, Q2, Q1, O4),
		(	O1 =\= O2, O3 =\= O4
		;	O1 =:= 0, point_on_segment(P2, P1, Q1)
		;	O2 =:= 0, point_on_segment(Q2, P1, Q1)
		;	O3 =:= 0, point_on_segment(P1, P2, Q2)
		;	O4 =:= 0, point_on_segment(Q1, P2, Q2)
		).

	orientation((Y1, X1), (Y2, X2), (Y3, X3), Orientation) :-
		Value is (X2 - X1) * (Y3 - Y1) - (Y2 - Y1) * (X3 - X1),
		(	abs(Value) =< 1.0e-12 ->
			Orientation = 0
		;	Value > 0.0 ->
			Orientation = 1
		;	Orientation = -1
		).

	nearest_coordinate([], _, _, Nearest, Distance, Nearest, Distance).
	nearest_coordinate([Coordinate| Coordinates], Origin, Metric, CurrentNearest, CurrentDistance, Nearest, Distance) :-
		valid_coordinate(Coordinate),
		distance(Origin, Coordinate, Metric, CandidateDistance),
		( CandidateDistance < CurrentDistance ->
			nearest_coordinate(Coordinates, Origin, Metric, Coordinate, CandidateDistance, Nearest, Distance)
		;	nearest_coordinate(Coordinates, Origin, Metric, CurrentNearest, CurrentDistance, Nearest, Distance)
		).

	normalize_polygon(Polygon, NormalizedPolygon) :-
		remove_polygon_closure(Polygon, OpenPolygon),
		OpenPolygon = [_, _, _| _],
		all_valid_coordinates(OpenPolygon),
		NormalizedPolygon = OpenPolygon.

	remove_polygon_closure([], []).
	remove_polygon_closure([Coordinate], [Coordinate]).
	remove_polygon_closure([Coordinate1, Coordinate2| Coordinates], OpenPolygon) :-
		Polygon = [Coordinate1, Coordinate2| Coordinates],
		last_polygon_coordinate(Polygon, Last),
		( Coordinate1 == Last ->
			remove_last_polygon_coordinate(Polygon, OpenPolygon)
		;	OpenPolygon = Polygon
		).

	last_polygon_coordinate([Item], Item) :-
		!.
	last_polygon_coordinate([_| Items], Last) :-
		last_polygon_coordinate(Items, Last).

	remove_last_polygon_coordinate([_], []).
	remove_last_polygon_coordinate([Coordinate| Coordinates], [Coordinate| OpenCoordinates]) :-
		Coordinates \= [],
		remove_last_polygon_coordinate(Coordinates, OpenCoordinates).

	all_valid_coordinates([]).
	all_valid_coordinates([Coordinate| Coordinates]) :-
		valid_coordinate(Coordinate),
		all_valid_coordinates(Coordinates).

	point_on_polygon_boundary(Point, Polygon) :-
		last_polygon_coordinate(Polygon, Last),
		point_on_polygon_boundary(Polygon, Last, Point).

	point_on_polygon_boundary([], _, _) :-
		fail.
	point_on_polygon_boundary([Coordinate| Coordinates], Previous, Point) :-
		( point_on_segment(Point, Previous, Coordinate) ->
			true
		;	point_on_polygon_boundary(Coordinates, Coordinate, Point)
		).

	point_on_segment((Latitude, Longitude), (Latitude1, Longitude1), (Latitude2, Longitude2)) :-
		Cross is (Longitude - Longitude1) * (Latitude2 - Latitude1) - (Latitude - Latitude1) * (Longitude2 - Longitude1),
		abs(Cross) =< 1.0e-12,
		MinLatitude is min(Latitude1, Latitude2),
		MaxLatitude is max(Latitude1, Latitude2),
		MinLongitude is min(Longitude1, Longitude2),
		MaxLongitude is max(Longitude1, Longitude2),
		Latitude >= MinLatitude,
		Latitude =< MaxLatitude,
		Longitude >= MinLongitude,
		Longitude =< MaxLongitude.

	count_polygon_ray_crossings(Point, Polygon, Crossings) :-
		last_polygon_coordinate(Polygon, Last),
		count_polygon_ray_crossings(Polygon, Last, Point, 0, Crossings).

	count_polygon_ray_crossings([], _, _, Crossings, Crossings).
	count_polygon_ray_crossings([Coordinate| Coordinates], Previous, Point, Crossings0, Crossings) :-
		( edge_crosses_ray(Point, Previous, Coordinate) ->
			Crossings1 is Crossings0 + 1
		;	Crossings1 = Crossings0
		),
		count_polygon_ray_crossings(Coordinates, Coordinate, Point, Crossings1, Crossings).

	edge_crosses_ray((Latitude, Longitude), (Latitude1, Longitude1), (Latitude2, Longitude2)) :-
		(	Latitude1 > Latitude, Latitude2 =< Latitude
		;	Latitude2 > Latitude, Latitude1 =< Latitude
		),
		LongitudeIntersection is (Longitude2 - Longitude1) * (Latitude - Latitude1) / (Latitude2 - Latitude1) + Longitude1,
		Longitude < LongitudeIntersection.

	mean_latitude(Polygon, MeanLatitude) :-
		sum_latitudes(Polygon, 0.0, 0, SumLatitudes, Count),
		MeanLatitude is SumLatitudes / Count.

	sum_latitudes([], SumLatitudes, Count, SumLatitudes, Count).
	sum_latitudes([(Latitude, _)| Coordinates], SumLatitudes0, Count0, SumLatitudes, Count) :-
		SumLatitudes1 is SumLatitudes0 + Latitude,
		Count1 is Count0 + 1,
		sum_latitudes(Coordinates, SumLatitudes1, Count1, SumLatitudes, Count).

	project_coordinates([], _, []).
	project_coordinates([(Latitude, Longitude)| Coordinates], MeanLatitude, [X-Y| ProjectedCoordinates]) :-
		mean_earth_radius_km(MeanEarthRadiusKm),
		LatitudeRadians is Latitude * pi / 180.0,
		LongitudeRadians is Longitude * pi / 180.0,
		MeanLatitudeRadians is MeanLatitude * pi / 180.0,
		X is MeanEarthRadiusKm * LongitudeRadians * cos(MeanLatitudeRadians),
		Y is MeanEarthRadiusKm * LatitudeRadians,
		project_coordinates(Coordinates, MeanLatitude, ProjectedCoordinates).

	polygon_ring_sum(Projected, RingSum) :-
		last_polygon_coordinate(Projected, LastX-LastY),
		polygon_ring_sum(Projected, LastX-LastY, 0.0, RingSum).

	polygon_ring_sum([], _, RingSum, RingSum).
	polygon_ring_sum([X-Y| Coordinates], PreviousX-PreviousY, RingSum0, RingSum) :-
		RingSum1 is RingSum0 + PreviousX * Y - X * PreviousY,
		polygon_ring_sum(Coordinates, X-Y, RingSum1, RingSum).

	polygon_centroid_projected(Projected, CentroidX, CentroidY) :-
		last_polygon_coordinate(Projected, LastX-LastY),
		polygon_centroid_projected(Projected, LastX-LastY, 0.0, 0.0, 0.0, AreaFactor, SumX, SumY),
		abs(AreaFactor) > 1.0e-12,
		CentroidX is SumX / (3.0 * AreaFactor),
		CentroidY is SumY / (3.0 * AreaFactor).

	polygon_centroid_projected([], _, Area0, SumX0, SumY0, Area0, SumX0, SumY0).
	polygon_centroid_projected([X-Y| Coordinates], PreviousX-PreviousY, Area0, SumX0, SumY0, Area, SumX, SumY) :-
		Cross is PreviousX * Y - X * PreviousY,
		Area1 is Area0 + Cross,
		SumX1 is SumX0 + (PreviousX + X) * Cross,
		SumY1 is SumY0 + (PreviousY + Y) * Cross,
		polygon_centroid_projected(Coordinates, X-Y, Area1, SumX1, SumY1, Area, SumX, SumY).

	polygon_min_max([], MinLatitude, MinLongitude, MaxLatitude, MaxLongitude, MinLatitude, MinLongitude, MaxLatitude, MaxLongitude).
	polygon_min_max([(Latitude, Longitude)| Coordinates], MinLatitude0, MinLongitude0, MaxLatitude0, MaxLongitude0, MinLatitude, MinLongitude, MaxLatitude, MaxLongitude) :-
		MinLatitude1 is min(MinLatitude0, Latitude),
		MinLongitude1 is min(MinLongitude0, Longitude),
		MaxLatitude1 is max(MaxLatitude0, Latitude),
		MaxLongitude1 is max(MaxLongitude0, Longitude),
		polygon_min_max(Coordinates, MinLatitude1, MinLongitude1, MaxLatitude1, MaxLongitude1, MinLatitude, MinLongitude, MaxLatitude, MaxLongitude).

	coordinate_radians(Coordinate, Latitude, Longitude) :-
		valid_coordinate(Coordinate),
		Coordinate = (LatitudeDegrees, LongitudeDegrees),
		Latitude is LatitudeDegrees * pi / 180.0,
		Longitude is LongitudeDegrees * pi / 180.0.

	vincenty_lambda(_, _, _, _, _, _, _, 0, _, _, _, _, _) :-
		!,
		fail.
	vincenty_lambda(WGS84F, LongitudeDifference, Lambda, SinU1, CosU1, SinU2, CosU2, Iterations, Sigma, SinSigma, CosSigma, CosSquaredAlpha, Cos2SigmaM) :-
		SinLambda is sin(Lambda),
		CosLambda is cos(Lambda),
		Term1 is CosU2 * SinLambda,
		Term2 is CosU1 * SinU2 - SinU1 * CosU2 * CosLambda,
		SinSigma0 is sqrt(Term1 * Term1 + Term2 * Term2),
		(	abs(SinSigma0) =< 1.0e-15 ->
			Sigma = 0.0,
			SinSigma = 0.0,
			CosSigma = 1.0,
			CosSquaredAlpha = 0.0,
			Cos2SigmaM = 0.0
		;	CosSigma0 is SinU1 * SinU2 + CosU1 * CosU2 * CosLambda,
			Sigma0 is atan2(SinSigma0, CosSigma0),
			SinAlpha is CosU1 * CosU2 * SinLambda / SinSigma0,
			CosSquaredAlpha0 is 1.0 - SinAlpha * SinAlpha,
			(	abs(CosSquaredAlpha0) =< 1.0e-15 ->
				Cos2SigmaM0 = 0.0
			;	Cos2SigmaM0 is CosSigma0 - 2.0 * SinU1 * SinU2 / CosSquaredAlpha0
			),
			C is WGS84F / 16.0 * CosSquaredAlpha0 * (4.0 + WGS84F * (4.0 - 3.0 * CosSquaredAlpha0)),
			Lambda2 is LongitudeDifference + (1.0 - C) * WGS84F * SinAlpha * (Sigma0 + C * SinSigma0 * (Cos2SigmaM0 + C * CosSigma0 * (-1.0 + 2.0 * Cos2SigmaM0 * Cos2SigmaM0))),
			(	abs(Lambda2 - Lambda) =< 1.0e-12 ->
				Sigma = Sigma0,
				SinSigma = SinSigma0,
				CosSigma = CosSigma0,
				CosSquaredAlpha = CosSquaredAlpha0,
				Cos2SigmaM = Cos2SigmaM0
			;	Iterations2 is Iterations - 1,
				vincenty_lambda(WGS84F, LongitudeDifference, Lambda2, SinU1, CosU1, SinU2, CosU2, Iterations2, Sigma, SinSigma, CosSigma, CosSquaredAlpha, Cos2SigmaM)
			)
		).

	normalize_bearing(Bearing0, Bearing) :-
		Bearing1 is Bearing0 + 360.0,
		Bearing2 is Bearing1 - 360.0 * floor(Bearing1 / 360.0),
		(	abs(Bearing2 - 360.0) =< 1.0e-12 ->
			Bearing = 0.0
		;	Bearing = Bearing2
		).

	mercator_latitude(Latitude, MercatorLatitude) :-
		HalfPi is pi / 2.0,
		Epsilon is 1.0e-12,
		MaxLatitude is HalfPi - Epsilon,
		MinLatitude is -HalfPi + Epsilon,
		ClampedLatitude is min(MaxLatitude, max(MinLatitude, Latitude)),
		MercatorLatitude is log(tan(pi / 4.0 + ClampedLatitude / 2.0)).

	normalize_longitude(Longitude, NormalizedLongitude) :-
		(	Longitude > pi ->
			Longitude2 is Longitude - 2.0 * pi,
			normalize_longitude(Longitude2, NormalizedLongitude)
		;	Longitude < -pi ->
			Longitude2 is Longitude + 2.0 * pi,
			normalize_longitude(Longitude2, NormalizedLongitude)
		;	NormalizedLongitude = Longitude
		).

	convert_km(DistanceKm, Unit, Distance) :-
		unit_factor(Unit, Factor),
		Distance is DistanceKm * Factor.

	unit_factor(kilometers, 1.0).
	unit_factor(meters, 1000.0).
	unit_factor(miles, 0.621371192237334).
	unit_factor(nautical_miles, 0.539956803455724).

	mean_earth_radius_km(6371.0088).

	wgs84_a(6378.1370).

	wgs84_f(1.0 / 298.257223563).

	wgs84_b(6356.752314245).

:- end_object.
