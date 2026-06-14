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


:- object(crs_projections,
	implements(crs_projections_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-10,
		comment is 'Coordinate reference systems and common transformations for 2D and 3D WGS84 geodetic coordinates, Web Mercator, World Mercator, ECEF, local ENU tangent-plane, Lambert azimuthal equal-area, and UTM coordinates.',
		remarks is [
			'Geographic coordinates' - 'WGS84 coordinates are represented as ``geographic(Latitude,Longitude)`` in degrees.',
			'3D geodetic coordinates' - 'WGS84 3D coordinates are represented as ``geographic(Latitude,Longitude,EllipsoidalHeight)`` using degrees for angular axes and meters for height.',
			'Projected coordinates' - 'Web Mercator, World Mercator, and Lambert azimuthal equal-area coordinates are represented as meter-based ``projected(X,Y)`` pairs. UTM coordinates are represented as ``grid(Easting,Northing)`` pairs.',
			'Geocentric coordinates' - 'ECEF coordinates are represented as ``ecef(X,Y,Z)`` triples in meters.',
			'Local tangent-plane coordinates' - 'ENU coordinates are represented as ``enu(East,North,Up)`` triples in meters relative to an origin coordinate.',
			'UTM coverage' - 'UTM helpers and transformations only succeed for latitudes in the ``[-80.0,84.0[`` range.'
		],
		see_also is [crs_projections_protocol, geospatial]
	]).

	:- uses(geospatial, [
		valid_coordinate/1 as valid_geographic_coordinate/1,
		normalize_coordinate/2 as normalize_geographic_coordinate/2
	]).

	valid_crs(wgs84).
	valid_crs(wgs84_3d).
	valid_crs(web_mercator).
	valid_crs(world_mercator).
	valid_crs(ecef).
	valid_crs(enu(Origin)) :-
		valid_geodetic_coordinate(Origin).
	valid_crs(lambert_azimuthal_equal_area(Origin)) :-
		valid_geodetic_coordinate(Origin).
	valid_crs(utm(Zone, Hemisphere)) :-
		valid_utm_zone(Zone),
		valid_utm_hemisphere(Hemisphere).

	crs_name(wgs84, 'WGS84').
	crs_name(wgs84_3d, 'WGS84 3D').
	crs_name(web_mercator, 'Web Mercator').
	crs_name(world_mercator, 'World Mercator').
	crs_name(ecef, 'Earth-Centered, Earth-Fixed').
	crs_name(enu(Origin), 'Local tangent plane (ENU)') :-
		valid_geodetic_coordinate(Origin).
	crs_name(lambert_azimuthal_equal_area(Origin), 'Lambert azimuthal equal-area') :-
		valid_geodetic_coordinate(Origin).
	crs_name(utm(Zone, Hemisphere), Name) :-
		valid_crs(utm(Zone, Hemisphere)),
		utm_hemisphere_suffix(Hemisphere, Suffix),
		number_codes(Zone, ZoneCodes),
		atom_codes(ZoneAtom, ZoneCodes),
		atom_concat('UTM zone ', ZoneAtom, Prefix),
		atom_concat(Prefix, Suffix, Name).

	crs_kind(wgs84, geographic).
	crs_kind(wgs84_3d, geographic).
	crs_kind(web_mercator, projected).
	crs_kind(world_mercator, projected).
	crs_kind(ecef, geocentric).
	crs_kind(enu(Origin), local_tangent_plane) :-
		valid_geodetic_coordinate(Origin).
	crs_kind(lambert_azimuthal_equal_area(Origin), projected) :-
		valid_geodetic_coordinate(Origin).
	crs_kind(utm(Zone, Hemisphere), projected) :-
		valid_crs(utm(Zone, Hemisphere)).

	crs_units(wgs84, degrees).
	crs_units(wgs84_3d, [degrees, degrees, meters]).
	crs_units(web_mercator, meters).
	crs_units(world_mercator, meters).
	crs_units(ecef, meters).
	crs_units(enu(Origin), meters) :-
		valid_geodetic_coordinate(Origin).
	crs_units(lambert_azimuthal_equal_area(Origin), meters) :-
		valid_geodetic_coordinate(Origin).
	crs_units(utm(Zone, Hemisphere), meters) :-
		valid_crs(utm(Zone, Hemisphere)).

	crs_dimensions(wgs84, 2).
	crs_dimensions(wgs84_3d, 3).
	crs_dimensions(web_mercator, 2).
	crs_dimensions(world_mercator, 2).
	crs_dimensions(ecef, 3).
	crs_dimensions(enu(Origin), 3) :-
		valid_geodetic_coordinate(Origin).
	crs_dimensions(lambert_azimuthal_equal_area(Origin), 2) :-
		valid_geodetic_coordinate(Origin).
	crs_dimensions(utm(Zone, Hemisphere), 2) :-
		valid_crs(utm(Zone, Hemisphere)).

	crs_epsg(wgs84, 4326).
	crs_epsg(wgs84_3d, 4979).
	crs_epsg(web_mercator, 3857).
	crs_epsg(world_mercator, 3395).
	crs_epsg(ecef, 4978).
	crs_epsg(utm(Zone, north), EPSG) :-
		!,
		valid_utm_zone(Zone),
		EPSG is 32600 + Zone.
	crs_epsg(utm(Zone, south), EPSG) :-
		valid_utm_zone(Zone),
		EPSG is 32700 + Zone.

	epsg_crs(4326, wgs84) :-
		!.
	epsg_crs(4979, wgs84_3d) :-
		!.
	epsg_crs(3857, web_mercator) :-
		!.
	epsg_crs(3395, world_mercator) :-
		!.
	epsg_crs(4978, ecef) :-
		!.
	epsg_crs(EPSG, utm(Zone, north)) :-
		integer(EPSG),
		EPSG >= 32601,
		EPSG =< 32660,
		Zone is EPSG - 32600,
		!.
	epsg_crs(EPSG, utm(Zone, south)) :-
		integer(EPSG),
		EPSG >= 32701,
		EPSG =< 32760,
		Zone is EPSG - 32700.

	valid_coordinate(wgs84, Coordinate) :-
		valid_geographic_coordinate(Coordinate).
	valid_coordinate(wgs84_3d, geographic(Latitude, Longitude, Height)) :-
		valid_geographic_coordinate(geographic(Latitude, Longitude)),
		number(Height).
	valid_coordinate(web_mercator, projected(X, Y)) :-
		number(X),
		number(Y),
		web_mercator_max_extent(MaxExtent),
		X >= -MaxExtent,
		X =< MaxExtent,
		Y >= -MaxExtent,
		Y =< MaxExtent.
	valid_coordinate(world_mercator, projected(X, Y)) :-
		number(X),
		number(Y).
	valid_coordinate(ecef, ecef(X, Y, Z)) :-
		number(X),
		number(Y),
		number(Z).
	valid_coordinate(enu(Origin), enu(East, North, Up)) :-
		valid_geodetic_coordinate(Origin),
		number(East),
		number(North),
		number(Up).
	valid_coordinate(lambert_azimuthal_equal_area(Origin), projected(X, Y)) :-
		valid_geodetic_coordinate(Origin),
		number(X),
		number(Y).
	valid_coordinate(utm(Zone, Hemisphere), grid(Easting, Northing)) :-
		valid_crs(utm(Zone, Hemisphere)),
		number(Easting),
		number(Northing),
		Easting >= 100000.0,
		Easting =< 900000.0,
		Northing >= 0.0,
		Northing =< 10000000.0.

	utm_zone(Coordinate, Zone) :-
		valid_utm_geographic_coordinate(Coordinate),
		utm_zone_from_coordinate(Coordinate, Zone).

	utm_crs(Coordinate, utm(Zone, Hemisphere)) :-
		valid_utm_geographic_coordinate(Coordinate),
		utm_zone_from_coordinate(Coordinate, Zone),
		hemisphere_from_coordinate(Coordinate, Hemisphere).

	local_tangent_plane(Origin, Coordinate, LocalCoordinate) :-
		transform(wgs84, enu(Origin), Coordinate, LocalCoordinate).

	local_tangent_plane_inverse(Origin, LocalCoordinate, Coordinate) :-
		transform(enu(Origin), wgs84, LocalCoordinate, Coordinate).

	lambert_azimuthal_equal_area(Origin, Coordinate, ProjectedCoordinate) :-
		transform(wgs84, lambert_azimuthal_equal_area(Origin), Coordinate, ProjectedCoordinate).

	lambert_azimuthal_equal_area_inverse(Origin, ProjectedCoordinate, Coordinate) :-
		transform(lambert_azimuthal_equal_area(Origin), wgs84, ProjectedCoordinate, Coordinate).

	transform(CRS, CRS, Coordinate, Coordinate) :-
		!,
		valid_coordinate(CRS, Coordinate).
	transform(wgs84, wgs84_3d, geographic(Latitude, Longitude), geographic(Latitude, Longitude, 0.0)) :-
		!,
		valid_coordinate(wgs84, geographic(Latitude, Longitude)).
	transform(wgs84_3d, wgs84, geographic(Latitude, Longitude, Height), geographic(Latitude, Longitude)) :-
		!,
		valid_coordinate(wgs84_3d, geographic(Latitude, Longitude, Height)).
	transform(wgs84, web_mercator, Coordinate, ProjectedCoordinate) :-
		!,
		wgs84_to_web_mercator(Coordinate, ProjectedCoordinate).
	transform(web_mercator, wgs84, Coordinate, GeographicCoordinate) :-
		!,
		web_mercator_to_wgs84(Coordinate, GeographicCoordinate).
	transform(wgs84, world_mercator, Coordinate, ProjectedCoordinate) :-
		!,
		wgs84_to_world_mercator(Coordinate, ProjectedCoordinate).
	transform(world_mercator, wgs84, Coordinate, GeographicCoordinate) :-
		!,
		world_mercator_to_wgs84(Coordinate, GeographicCoordinate).
	transform(wgs84, ecef, Coordinate, GeocentricCoordinate) :-
		!,
		wgs84_to_ecef(Coordinate, GeocentricCoordinate).
	transform(wgs84_3d, ecef, Coordinate, GeocentricCoordinate) :-
		!,
		wgs84_to_ecef(Coordinate, GeocentricCoordinate).
	transform(ecef, wgs84, Coordinate, GeographicCoordinate) :-
		!,
		ecef_to_wgs84(Coordinate, GeographicCoordinate).
	transform(ecef, wgs84_3d, Coordinate, GeographicCoordinate) :-
		!,
		ecef_to_wgs84_3d(Coordinate, GeographicCoordinate).
	transform(wgs84, enu(Origin), Coordinate, LocalCoordinate) :-
		!,
		wgs84_to_enu(Origin, Coordinate, LocalCoordinate).
	transform(wgs84_3d, enu(Origin), Coordinate, LocalCoordinate) :-
		!,
		wgs84_to_enu(Origin, Coordinate, LocalCoordinate).
	transform(enu(Origin), wgs84, Coordinate, GeographicCoordinate) :-
		!,
		enu_to_wgs84(Origin, Coordinate, GeographicCoordinate).
	transform(enu(Origin), wgs84_3d, Coordinate, GeographicCoordinate) :-
		!,
		enu_to_wgs84_3d(Origin, Coordinate, GeographicCoordinate).
	transform(ecef, enu(Origin), Coordinate, LocalCoordinate) :-
		!,
		ecef_to_enu(Origin, Coordinate, LocalCoordinate).
	transform(enu(Origin), ecef, Coordinate, GeocentricCoordinate) :-
		!,
		enu_to_ecef(Origin, Coordinate, GeocentricCoordinate).
	transform(wgs84, lambert_azimuthal_equal_area(Origin), Coordinate, ProjectedCoordinate) :-
		!,
		wgs84_to_lambert_azimuthal_equal_area(Origin, Coordinate, ProjectedCoordinate).
	transform(lambert_azimuthal_equal_area(Origin), wgs84, Coordinate, GeographicCoordinate) :-
		!,
		lambert_azimuthal_equal_area_to_wgs84(Origin, Coordinate, GeographicCoordinate).
	transform(wgs84, utm(Zone, Hemisphere), Coordinate, ProjectedCoordinate) :-
		!,
		wgs84_to_utm(Coordinate, Zone, Hemisphere, ProjectedCoordinate).
	transform(utm(Zone, Hemisphere), wgs84, Coordinate, GeographicCoordinate) :-
		!,
		utm_to_wgs84(Coordinate, Zone, Hemisphere, GeographicCoordinate).
	transform(SourceCRS, TargetCRS, Coordinate, TransformedCoordinate) :-
		SourceCRS \== wgs84,
		TargetCRS \== wgs84,
		transform(SourceCRS, wgs84, Coordinate, GeographicCoordinate),
		transform(wgs84, TargetCRS, GeographicCoordinate, TransformedCoordinate).

	valid_utm_zone(Zone) :-
		integer(Zone),
		Zone >= 1,
		Zone =< 60.

	valid_utm_hemisphere(north).
	valid_utm_hemisphere(south).

	valid_geodetic_coordinate(geographic(Latitude, Longitude)) :-
		valid_geographic_coordinate(geographic(Latitude, Longitude)).
	valid_geodetic_coordinate(geographic(Latitude, Longitude, Height)) :-
		valid_geographic_coordinate(geographic(Latitude, Longitude)),
		number(Height).

	horizontal_geographic_coordinate(geographic(Latitude, Longitude), geographic(Latitude, Longitude)).
	horizontal_geographic_coordinate(geographic(Latitude, Longitude, _), geographic(Latitude, Longitude)).

	coordinate_height(geographic(_, _, Height), Height).
	coordinate_height(geographic(_, _), 0.0).

	valid_utm_geographic_coordinate(geographic(Latitude, Longitude)) :-
		valid_geographic_coordinate(geographic(Latitude, Longitude)),
		Latitude >= -80.0,
		Latitude < 84.0.

	utm_zone_from_coordinate(geographic(Latitude, Longitude), Zone) :-
		Latitude >= 56.0,
		Latitude < 64.0,
		Longitude >= 3.0,
		Longitude < 12.0,
		!,
		Zone = 32.
	utm_zone_from_coordinate(geographic(Latitude, Longitude), Zone) :-
		Latitude >= 72.0,
		Latitude < 84.0,
		utm_zone_from_svalbard_longitude(Longitude, Zone),
		!.
	utm_zone_from_coordinate(geographic(_, Longitude), Zone) :-
		RawZone is floor((Longitude + 180.0) / 6.0) + 1,
		(	RawZone > 60 ->
			Zone = 60
		;	Zone = RawZone
		).

	utm_zone_from_svalbard_longitude(Longitude, 31) :-
		Longitude >= 0.0,
		Longitude < 9.0,
		!.
	utm_zone_from_svalbard_longitude(Longitude, 33) :-
		Longitude >= 9.0,
		Longitude < 21.0,
		!.
	utm_zone_from_svalbard_longitude(Longitude, 35) :-
		Longitude >= 21.0,
		Longitude < 33.0,
		!.
	utm_zone_from_svalbard_longitude(Longitude, 37) :-
		Longitude >= 33.0,
		Longitude < 42.0.

	hemisphere_from_coordinate(geographic(Latitude, _), south) :-
		Latitude < 0.0,
		!.
	hemisphere_from_coordinate(_, north).

	hemisphere_matches_latitude(Latitude, north) :-
		Latitude >= 0.0,
		!.
	hemisphere_matches_latitude(Latitude, south) :-
		Latitude =< 0.0.

	utm_hemisphere_suffix(north, 'N').
	utm_hemisphere_suffix(south, 'S').

	wgs84_to_web_mercator(geographic(Latitude, Longitude), projected(X, Y)) :-
		valid_geographic_coordinate(geographic(Latitude, Longitude)),
		web_mercator_max_latitude(MaxLatitude),
		Latitude >= -MaxLatitude,
		Latitude =< MaxLatitude,
		wgs84_semi_major_axis_meters(SemiMajorAxis),
		LongitudeRadians is Longitude * pi / 180.0,
		LatitudeRadians is Latitude * pi / 180.0,
		X is SemiMajorAxis * LongitudeRadians,
		Y is SemiMajorAxis * log(tan(pi / 4.0 + LatitudeRadians / 2.0)).

	web_mercator_to_wgs84(projected(X, Y), Coordinate) :-
		valid_coordinate(web_mercator, projected(X, Y)),
		wgs84_semi_major_axis_meters(SemiMajorAxis),
		LongitudeRadians is X / SemiMajorAxis,
		LatitudeRadians is 2.0 * atan(exp(Y / SemiMajorAxis)) - pi / 2.0,
		Latitude is LatitudeRadians * 180.0 / pi,
		Longitude is LongitudeRadians * 180.0 / pi,
		normalize_geographic_coordinate(geographic(Latitude, Longitude), Coordinate).

	valid_world_mercator_geographic_coordinate(geographic(Latitude, Longitude)) :-
		valid_geographic_coordinate(geographic(Latitude, Longitude)),
		Latitude > -90.0,
		Latitude < 90.0.

	wgs84_to_world_mercator(geographic(Latitude, Longitude), projected(X, Y)) :-
		valid_world_mercator_geographic_coordinate(geographic(Latitude, Longitude)),
		wgs84_semi_major_axis_meters(SemiMajorAxis),
		wgs84_eccentricity(Eccentricity),
		LatitudeRadians is Latitude * pi / 180.0,
		LongitudeRadians is Longitude * pi / 180.0,
		SinLatitude is sin(LatitudeRadians),
		RatioBase is (1.0 - Eccentricity * SinLatitude) / (1.0 + Eccentricity * SinLatitude),
		Ratio is exp(Eccentricity * log(RatioBase) / 2.0),
		X is SemiMajorAxis * LongitudeRadians,
		Y is SemiMajorAxis * log(tan(pi / 4.0 + LatitudeRadians / 2.0) * Ratio).

	world_mercator_to_wgs84(projected(X, Y), Coordinate) :-
		valid_coordinate(world_mercator, projected(X, Y)),
		wgs84_semi_major_axis_meters(SemiMajorAxis),
		wgs84_eccentricity(Eccentricity),
		LongitudeRadians is X / SemiMajorAxis,
		T is exp(-Y / SemiMajorAxis),
		Latitude0 is pi / 2.0 - 2.0 * atan(T),
		world_mercator_latitude(Latitude0, T, Eccentricity, 12, LatitudeRadians),
		Latitude is LatitudeRadians * 180.0 / pi,
		Longitude is LongitudeRadians * 180.0 / pi,
		normalize_geographic_coordinate(geographic(Latitude, Longitude), Coordinate).

	world_mercator_latitude(Latitude0, T, Eccentricity, Iterations, Latitude) :-
		SinLatitude0 is sin(Latitude0),
		RatioBase is (1.0 - Eccentricity * SinLatitude0) / (1.0 + Eccentricity * SinLatitude0),
		Ratio is exp(Eccentricity * log(RatioBase) / 2.0),
		Latitude1 is pi / 2.0 - 2.0 * atan(T * Ratio),
		(	abs(Latitude1 - Latitude0) =< 1.0e-12 ->
			Latitude = Latitude1
		;	Iterations =< 0 ->
			Latitude = Latitude1
		;	NextIterations is Iterations - 1,
			world_mercator_latitude(Latitude1, T, Eccentricity, NextIterations, Latitude)
		).

	wgs84_to_ecef(Coordinate, ecef(X, Y, Z)) :-
		valid_geodetic_coordinate(Coordinate),
		horizontal_geographic_coordinate(Coordinate, geographic(Latitude, Longitude)),
		coordinate_height(Coordinate, Height),
		wgs84_semi_major_axis_meters(SemiMajorAxis),
		wgs84_eccentricity_squared(EccentricitySquared),
		LatitudeRadians is Latitude * pi / 180.0,
		LongitudeRadians is Longitude * pi / 180.0,
		SinLatitude is sin(LatitudeRadians),
		CosLatitude is cos(LatitudeRadians),
		PrimeVerticalRadius is SemiMajorAxis / sqrt(1.0 - EccentricitySquared * SinLatitude * SinLatitude),
		X is (PrimeVerticalRadius + Height) * CosLatitude * cos(LongitudeRadians),
		Y is (PrimeVerticalRadius + Height) * CosLatitude * sin(LongitudeRadians),
		Z is (PrimeVerticalRadius * (1.0 - EccentricitySquared) + Height) * SinLatitude.

	ecef_to_wgs84(ecef(X, Y, Z), Coordinate) :-
		ecef_to_wgs84_3d(ecef(X, Y, Z), geographic(Latitude, Longitude, _)),
		Coordinate = geographic(Latitude, Longitude).

	ecef_to_wgs84_3d(ecef(X, Y, Z), Coordinate) :-
		valid_coordinate(ecef, ecef(X, Y, Z)),
		(	abs(X) < 1.0e-12, abs(Y) < 1.0e-12, abs(Z) < 1.0e-12 ->
			fail
		;	wgs84_semi_major_axis_meters(SemiMajorAxis),
			wgs84_semi_minor_axis_meters(SemiMinorAxis),
			wgs84_eccentricity_squared(EccentricitySquared),
			wgs84_second_eccentricity_squared(SecondEccentricitySquared),
			P is sqrt(X * X + Y * Y),
			Theta is atan2(Z * SemiMajorAxis, P * SemiMinorAxis),
			SinTheta is sin(Theta),
			CosTheta is cos(Theta),
			LatitudeRadians is atan2(
				Z + SecondEccentricitySquared * SemiMinorAxis * SinTheta * SinTheta * SinTheta,
				P - EccentricitySquared * SemiMajorAxis * CosTheta * CosTheta * CosTheta
			),
			LongitudeRadians is atan2(Y, X),
			SinLatitude is sin(LatitudeRadians),
			CosLatitude is cos(LatitudeRadians),
			PrimeVerticalRadius is SemiMajorAxis / sqrt(1.0 - EccentricitySquared * SinLatitude * SinLatitude),
			(	abs(CosLatitude) > 1.0e-12 ->
				Height is P / CosLatitude - PrimeVerticalRadius
			;	Height is Z / SinLatitude - PrimeVerticalRadius * (1.0 - EccentricitySquared)
			),
			Latitude is LatitudeRadians * 180.0 / pi,
			Longitude is LongitudeRadians * 180.0 / pi,
			normalize_geographic_coordinate(geographic(Latitude, Longitude), geographic(NormalizedLatitude, NormalizedLongitude)),
			Coordinate = geographic(NormalizedLatitude, NormalizedLongitude, Height)
		).

	wgs84_to_enu(Origin, Coordinate, enu(East, North, Up)) :-
		valid_geodetic_coordinate(Origin),
		valid_geodetic_coordinate(Coordinate),
		wgs84_to_ecef(Origin, ecef(OriginX, OriginY, OriginZ)),
		wgs84_to_ecef(Coordinate, ecef(X, Y, Z)),
		ecef_delta_to_enu(Origin, ecef(X - OriginX, Y - OriginY, Z - OriginZ), enu(East, North, Up)).

	enu_to_wgs84(Origin, LocalCoordinate, Coordinate) :-
		enu_to_wgs84_3d(Origin, LocalCoordinate, geographic(Latitude, Longitude, _)),
		Coordinate = geographic(Latitude, Longitude).

	enu_to_wgs84_3d(Origin, LocalCoordinate, Coordinate) :-
		enu_to_ecef(Origin, LocalCoordinate, ECEFCoordinate),
		ecef_to_wgs84_3d(ECEFCoordinate, Coordinate).

	ecef_to_enu(Origin, ecef(X, Y, Z), LocalCoordinate) :-
		valid_geodetic_coordinate(Origin),
		valid_coordinate(ecef, ecef(X, Y, Z)),
		wgs84_to_ecef(Origin, ecef(OriginX, OriginY, OriginZ)),
		ecef_delta_to_enu(Origin, ecef(X - OriginX, Y - OriginY, Z - OriginZ), LocalCoordinate).

	enu_to_ecef(Origin, enu(East, North, Up), ecef(X, Y, Z)) :-
		valid_coordinate(enu(Origin), enu(East, North, Up)),
		wgs84_to_ecef(Origin, ecef(OriginX, OriginY, OriginZ)),
		enu_to_ecef_delta(Origin, enu(East, North, Up), ecef(DeltaX, DeltaY, DeltaZ)),
		X is OriginX + DeltaX,
		Y is OriginY + DeltaY,
		Z is OriginZ + DeltaZ.

	ecef_delta_to_enu(Origin, ecef(DeltaX, DeltaY, DeltaZ), enu(East, North, Up)) :-
		horizontal_geographic_coordinate(Origin, geographic(Latitude0, Longitude0)),
		Latitude0Radians is Latitude0 * pi / 180.0,
		Longitude0Radians is Longitude0 * pi / 180.0,
		SinLatitude0 is sin(Latitude0Radians),
		CosLatitude0 is cos(Latitude0Radians),
		SinLongitude0 is sin(Longitude0Radians),
		CosLongitude0 is cos(Longitude0Radians),
		East is -SinLongitude0 * DeltaX + CosLongitude0 * DeltaY,
		North is -SinLatitude0 * CosLongitude0 * DeltaX - SinLatitude0 * SinLongitude0 * DeltaY + CosLatitude0 * DeltaZ,
		Up is CosLatitude0 * CosLongitude0 * DeltaX + CosLatitude0 * SinLongitude0 * DeltaY + SinLatitude0 * DeltaZ.

	enu_to_ecef_delta(Origin, enu(East, North, Up), ecef(DeltaX, DeltaY, DeltaZ)) :-
		horizontal_geographic_coordinate(Origin, geographic(Latitude0, Longitude0)),
		Latitude0Radians is Latitude0 * pi / 180.0,
		Longitude0Radians is Longitude0 * pi / 180.0,
		SinLatitude0 is sin(Latitude0Radians),
		CosLatitude0 is cos(Latitude0Radians),
		SinLongitude0 is sin(Longitude0Radians),
		CosLongitude0 is cos(Longitude0Radians),
		DeltaX is -SinLongitude0 * East - SinLatitude0 * CosLongitude0 * North + CosLatitude0 * CosLongitude0 * Up,
		DeltaY is CosLongitude0 * East - SinLatitude0 * SinLongitude0 * North + CosLatitude0 * SinLongitude0 * Up,
		DeltaZ is CosLatitude0 * North + SinLatitude0 * Up.

	wgs84_to_lambert_azimuthal_equal_area(Origin, Coordinate, projected(X, Y)) :-
		valid_geodetic_coordinate(Origin),
		valid_geographic_coordinate(Coordinate),
		wgs84_authalic_radius_meters(Radius),
		horizontal_geographic_coordinate(Origin, geographic(OriginLatitude, OriginLongitude)),
		Coordinate = geographic(Latitude, Longitude),
		OriginLatitudeRadians is OriginLatitude * pi / 180.0,
		OriginLongitudeRadians is OriginLongitude * pi / 180.0,
		LatitudeRadians is Latitude * pi / 180.0,
		LongitudeRadians is Longitude * pi / 180.0,
		SinOriginLatitude is sin(OriginLatitudeRadians),
		CosOriginLatitude is cos(OriginLatitudeRadians),
		SinLatitude is sin(LatitudeRadians),
		CosLatitude is cos(LatitudeRadians),
		DeltaLongitude is LongitudeRadians - OriginLongitudeRadians,
		Denominator is 1.0 + SinOriginLatitude * SinLatitude + CosOriginLatitude * CosLatitude * cos(DeltaLongitude),
		Denominator > 0.0,
		Scale is sqrt(2.0 / Denominator),
		X is Radius * Scale * CosLatitude * sin(DeltaLongitude),
		Y is Radius * Scale * (CosOriginLatitude * SinLatitude - SinOriginLatitude * CosLatitude * cos(DeltaLongitude)).

	lambert_azimuthal_equal_area_to_wgs84(Origin, projected(X, Y), Coordinate) :-
		valid_geodetic_coordinate(Origin),
		valid_coordinate(lambert_azimuthal_equal_area(Origin), projected(X, Y)),
		wgs84_authalic_radius_meters(Radius),
		horizontal_geographic_coordinate(Origin, geographic(OriginLatitude, OriginLongitude)),
		OriginLatitudeRadians is OriginLatitude * pi / 180.0,
		OriginLongitudeRadians is OriginLongitude * pi / 180.0,
		Rho is sqrt(X * X + Y * Y),
		Rho =< 2.0 * Radius,
		(	Rho =< 1.0e-12 ->
			horizontal_geographic_coordinate(Origin, Coordinate)
		;	CentralAngle is 2.0 * asin(Rho / (2.0 * Radius)),
			SinCentralAngle is sin(CentralAngle),
			CosCentralAngle is cos(CentralAngle),
			SinOriginLatitude is sin(OriginLatitudeRadians),
			CosOriginLatitude is cos(OriginLatitudeRadians),
			LatitudeRadians is asin(CosCentralAngle * SinOriginLatitude + Y * SinCentralAngle * CosOriginLatitude / Rho),
			LongitudeRadians is OriginLongitudeRadians + atan2(X * SinCentralAngle, Rho * CosOriginLatitude * CosCentralAngle - Y * SinOriginLatitude * SinCentralAngle),
			Latitude is LatitudeRadians * 180.0 / pi,
			Longitude is LongitudeRadians * 180.0 / pi,
			normalize_geographic_coordinate(geographic(Latitude, Longitude), Coordinate)
		).

	wgs84_to_utm(geographic(Latitude, Longitude), Zone, Hemisphere, grid(Easting, Northing)) :-
		valid_crs(utm(Zone, Hemisphere)),
		valid_utm_geographic_coordinate(geographic(Latitude, Longitude)),
		hemisphere_matches_latitude(Latitude, Hemisphere),
		wgs84_second_eccentricity_squared(SecondEccentricitySquared),
		utm_scale_factor(ScaleFactor),
		central_meridian_radians(Zone, CentralMeridianRadians),
		LatitudeRadians is Latitude * pi / 180.0,
		LongitudeRadians is Longitude * pi / 180.0,
		SinLatitude is sin(LatitudeRadians),
		CosLatitude is cos(LatitudeRadians),
		TangentLatitude is tan(LatitudeRadians),
		SinLatitudeSquared is SinLatitude * SinLatitude,
		TangentLatitudeSquared is TangentLatitude * TangentLatitude,
		EtaSquared is SecondEccentricitySquared * CosLatitude * CosLatitude,
		LongitudeOffset is LongitudeRadians - CentralMeridianRadians,
		ReducedLongitude is CosLatitude * LongitudeOffset,
		ReducedLongitudeSquared is ReducedLongitude * ReducedLongitude,
		ReducedLongitudeFourth is ReducedLongitudeSquared * ReducedLongitudeSquared,
		ReducedLongitudeSixth is ReducedLongitudeFourth * ReducedLongitudeSquared,
		prime_vertical_radius(SinLatitudeSquared, PrimeVerticalRadius),
		meridional_arc(LatitudeRadians, MeridionalArc),
		Easting is ScaleFactor * PrimeVerticalRadius * (
			ReducedLongitude +
			(1.0 - TangentLatitudeSquared + EtaSquared) * ReducedLongitude * ReducedLongitudeSquared / 6.0 +
			(5.0 - 18.0 * TangentLatitudeSquared + TangentLatitudeSquared * TangentLatitudeSquared + 72.0 * EtaSquared - 58.0 * SecondEccentricitySquared) * ReducedLongitude * ReducedLongitudeFourth / 120.0
		) + 500000.0,
		Northing0 is ScaleFactor * (
			MeridionalArc +
			PrimeVerticalRadius * TangentLatitude * (
				ReducedLongitudeSquared / 2.0 +
				(5.0 - TangentLatitudeSquared + 9.0 * EtaSquared + 4.0 * EtaSquared * EtaSquared) * ReducedLongitudeFourth / 24.0 +
				(61.0 - 58.0 * TangentLatitudeSquared + TangentLatitudeSquared * TangentLatitudeSquared + 600.0 * EtaSquared - 330.0 * SecondEccentricitySquared) * ReducedLongitudeSixth / 720.0
			)
		),
		(	Hemisphere == south ->
			Northing is Northing0 + 10000000.0
		;	Northing = Northing0
		),
		valid_coordinate(utm(Zone, Hemisphere), grid(Easting, Northing)).

	utm_to_wgs84(grid(Easting, Northing), Zone, Hemisphere, Coordinate) :-
		valid_coordinate(utm(Zone, Hemisphere), grid(Easting, Northing)),
		wgs84_second_eccentricity_squared(SecondEccentricitySquared),
		utm_scale_factor(ScaleFactor),
		central_meridian_radians(Zone, CentralMeridianRadians),
		easting_offset(Easting, OffsetEasting),
		northing_offset(Hemisphere, Northing, OffsetNorthing),
		MeridionalArc is OffsetNorthing / ScaleFactor,
		footpoint_latitude(MeridionalArc, FootpointLatitude),
		SinFootpointLatitude is sin(FootpointLatitude),
		CosFootpointLatitude is cos(FootpointLatitude),
		TangentFootpointLatitude is tan(FootpointLatitude),
		SinFootpointLatitudeSquared is SinFootpointLatitude * SinFootpointLatitude,
		TangentFootpointLatitudeSquared is TangentFootpointLatitude * TangentFootpointLatitude,
		EtaSquared is SecondEccentricitySquared * CosFootpointLatitude * CosFootpointLatitude,
		prime_vertical_radius(SinFootpointLatitudeSquared, PrimeVerticalRadius),
		meridional_radius(SinFootpointLatitudeSquared, MeridionalRadius),
		NormalizedEasting is OffsetEasting / (PrimeVerticalRadius * ScaleFactor),
		NormalizedEastingSquared is NormalizedEasting * NormalizedEasting,
		NormalizedEastingFourth is NormalizedEastingSquared * NormalizedEastingSquared,
		NormalizedEastingSixth is NormalizedEastingFourth * NormalizedEastingSquared,
		EtaFourth is EtaSquared * EtaSquared,
		LatitudeRadians is FootpointLatitude - (
			PrimeVerticalRadius * TangentFootpointLatitude / MeridionalRadius
		) * (
			NormalizedEastingSquared / 2.0 -
			(5.0 + 3.0 * TangentFootpointLatitudeSquared + 10.0 * EtaSquared - 4.0 * EtaFourth - 9.0 * SecondEccentricitySquared) * NormalizedEastingFourth / 24.0 +
			(61.0 + 90.0 * TangentFootpointLatitudeSquared + 298.0 * EtaSquared + 45.0 * TangentFootpointLatitudeSquared * TangentFootpointLatitudeSquared - 252.0 * SecondEccentricitySquared - 3.0 * EtaFourth) * NormalizedEastingSixth / 720.0
		),
		LongitudeRadians is CentralMeridianRadians + (
			NormalizedEasting -
			(1.0 + 2.0 * TangentFootpointLatitudeSquared + EtaSquared) * NormalizedEasting * NormalizedEastingSquared / 6.0 +
			(5.0 - 2.0 * EtaSquared + 28.0 * TangentFootpointLatitudeSquared - 3.0 * EtaFourth + 8.0 * SecondEccentricitySquared + 24.0 * TangentFootpointLatitudeSquared * TangentFootpointLatitudeSquared) * NormalizedEasting * NormalizedEastingFourth / 120.0
		) / CosFootpointLatitude,
		Latitude is LatitudeRadians * 180.0 / pi,
		Longitude is LongitudeRadians * 180.0 / pi,
		normalize_geographic_coordinate(geographic(Latitude, Longitude), Coordinate),
		valid_geographic_coordinate(Coordinate).

	wgs84_semi_major_axis_meters(6378137.0).

	wgs84_inverse_flattening(298.257223563).

	wgs84_authalic_radius_meters(6371007.180918474).

	wgs84_semi_minor_axis_meters(SemiMinorAxis) :-
		wgs84_semi_major_axis_meters(SemiMajorAxis),
		wgs84_flattening(Flattening),
		SemiMinorAxis is SemiMajorAxis * (1.0 - Flattening).

	utm_scale_factor(0.9996).

	wgs84_flattening(Flattening) :-
		wgs84_inverse_flattening(InverseFlattening),
		Flattening is 1.0 / InverseFlattening.

	wgs84_eccentricity_squared(EccentricitySquared) :-
		wgs84_flattening(Flattening),
		EccentricitySquared is Flattening * (2.0 - Flattening).

	wgs84_eccentricity(Eccentricity) :-
		wgs84_eccentricity_squared(EccentricitySquared),
		Eccentricity is sqrt(EccentricitySquared).

	wgs84_second_eccentricity_squared(SecondEccentricitySquared) :-
		wgs84_eccentricity_squared(EccentricitySquared),
		SecondEccentricitySquared is EccentricitySquared / (1.0 - EccentricitySquared).

	web_mercator_max_latitude(85.0511287798066).
	web_mercator_max_extent(MaxExtent) :-
		wgs84_semi_major_axis_meters(SemiMajorAxis),
		MaxExtent is SemiMajorAxis * pi.

	central_meridian_radians(Zone, CentralMeridianRadians) :-
		CentralMeridianDegrees is Zone * 6 - 183,
		CentralMeridianRadians is CentralMeridianDegrees * pi / 180.0.

	prime_vertical_radius(SinLatitudeSquared, PrimeVerticalRadius) :-
		wgs84_semi_major_axis_meters(SemiMajorAxis),
		wgs84_eccentricity_squared(EccentricitySquared),
		PrimeVerticalRadius is SemiMajorAxis / sqrt(1.0 - EccentricitySquared * SinLatitudeSquared).

	meridional_radius(SinLatitudeSquared, MeridionalRadius) :-
		wgs84_semi_major_axis_meters(SemiMajorAxis),
		wgs84_eccentricity_squared(EccentricitySquared),
		Denominator is 1.0 - EccentricitySquared * SinLatitudeSquared,
		MeridionalRadius is SemiMajorAxis * (1.0 - EccentricitySquared) / (Denominator * sqrt(Denominator)).

	meridional_arc(LatitudeRadians, MeridionalArc) :-
		wgs84_semi_major_axis_meters(SemiMajorAxis),
		wgs84_eccentricity_squared(EccentricitySquared),
		EccentricityFourth is EccentricitySquared * EccentricitySquared,
		EccentricitySixth is EccentricityFourth * EccentricitySquared,
		MeridionalArc is SemiMajorAxis * (
			(1.0 - EccentricitySquared / 4.0 - 3.0 * EccentricityFourth / 64.0 - 5.0 * EccentricitySixth / 256.0) * LatitudeRadians -
			(3.0 * EccentricitySquared / 8.0 + 3.0 * EccentricityFourth / 32.0 + 45.0 * EccentricitySixth / 1024.0) * sin(2.0 * LatitudeRadians) +
			(15.0 * EccentricityFourth / 256.0 + 45.0 * EccentricitySixth / 1024.0) * sin(4.0 * LatitudeRadians) -
			(35.0 * EccentricitySixth / 3072.0) * sin(6.0 * LatitudeRadians)
		).

	footpoint_latitude(MeridionalArc, FootpointLatitude) :-
		wgs84_semi_major_axis_meters(SemiMajorAxis),
		wgs84_eccentricity_squared(EccentricitySquared),
		EccentricityFourth is EccentricitySquared * EccentricitySquared,
		EccentricitySixth is EccentricityFourth * EccentricitySquared,
		Denominator is SemiMajorAxis * (1.0 - EccentricitySquared / 4.0 - 3.0 * EccentricityFourth / 64.0 - 5.0 * EccentricitySixth / 256.0),
		Mu is MeridionalArc / Denominator,
		first_eccentricity_term(EccentricitySquared, FirstEccentricityTerm),
		FirstEccentricityTermSquared is FirstEccentricityTerm * FirstEccentricityTerm,
		FirstEccentricityTermCubed is FirstEccentricityTermSquared * FirstEccentricityTerm,
		FirstEccentricityTermFourth is FirstEccentricityTermSquared * FirstEccentricityTermSquared,
		FootpointLatitude is Mu +
			(3.0 * FirstEccentricityTerm / 2.0 - 27.0 * FirstEccentricityTermCubed / 32.0) * sin(2.0 * Mu) +
			(21.0 * FirstEccentricityTermSquared / 16.0 - 55.0 * FirstEccentricityTermFourth / 32.0) * sin(4.0 * Mu) +
			(151.0 * FirstEccentricityTermCubed / 96.0) * sin(6.0 * Mu) +
			(1097.0 * FirstEccentricityTermFourth / 512.0) * sin(8.0 * Mu).

	first_eccentricity_term(EccentricitySquared, FirstEccentricityTerm) :-
		FirstDenominator is 1.0 + sqrt(1.0 - EccentricitySquared),
		FirstNumerator is 1.0 - sqrt(1.0 - EccentricitySquared),
		FirstEccentricityTerm is FirstNumerator / FirstDenominator.

	easting_offset(Easting, OffsetEasting) :-
		OffsetEasting is Easting - 500000.0.

	northing_offset(north, Northing, Northing).
	northing_offset(south, Northing, OffsetNorthing) :-
		OffsetNorthing is Northing - 10000000.0.

:- end_object.
