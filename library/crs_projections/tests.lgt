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
		date is 2026-05-10,
		comment is 'Unit tests for the "crs_projections" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(crs_projections, [
		valid_crs/1, crs_name/2, crs_kind/2, crs_units/2, crs_dimensions/2, crs_epsg/2, epsg_crs/2,
		valid_coordinate/2, local_tangent_plane/3, local_tangent_plane_inverse/3,
		lambert_azimuthal_equal_area/3, lambert_azimuthal_equal_area_inverse/3, utm_zone/2, utm_crs/2,
		transform/4
	]).

	cover(crs_projections).

	test(crs_projections_valid_crs_1_01, deterministic) :-
		valid_crs(wgs84).

	test(crs_projections_valid_crs_1_02, deterministic) :-
		valid_crs(wgs84_3d).

	test(crs_projections_valid_crs_1_03, deterministic) :-
		valid_crs(web_mercator).

	test(crs_projections_valid_crs_1_04, deterministic) :-
		valid_crs(world_mercator).

	test(crs_projections_valid_crs_1_05, deterministic) :-
		valid_crs(ecef).

	test(crs_projections_valid_crs_1_06, deterministic) :-
		valid_crs(enu(geographic(38.7223, -9.1393))).

	test(crs_projections_valid_crs_1_07, deterministic) :-
		valid_crs(enu(geographic(38.7223, -9.1393, 100.0))).

	test(crs_projections_valid_crs_1_08, deterministic) :-
		valid_crs(lambert_azimuthal_equal_area(geographic(38.7223, -9.1393))).

	test(crs_projections_valid_crs_1_09, deterministic) :-
		valid_crs(utm(29, north)).

	test(crs_projections_valid_crs_1_10, fail) :-
		valid_crs(utm(0, north)).

	test(crs_projections_crs_name_2_01, deterministic(Name == 'Web Mercator')) :-
		crs_name(web_mercator, Name).

	test(crs_projections_crs_name_2_02, deterministic(Name == 'UTM zone 29N')) :-
		crs_name(utm(29, north), Name).

	test(crs_projections_crs_name_2_03, deterministic(Name == 'WGS84 3D')) :-
		crs_name(wgs84_3d, Name).

	test(crs_projections_crs_kind_2_01, deterministic(Kind == geographic)) :-
		crs_kind(wgs84, Kind).

	test(crs_projections_crs_kind_2_02, deterministic(Kind == projected)) :-
		crs_kind(world_mercator, Kind).

	test(crs_projections_crs_kind_2_03, deterministic(Kind == geocentric)) :-
		crs_kind(ecef, Kind).

	test(crs_projections_crs_kind_2_04, deterministic(Kind == local_tangent_plane)) :-
		crs_kind(enu(geographic(38.7223, -9.1393)), Kind).

	test(crs_projections_crs_units_2_01, deterministic(Units == degrees)) :-
		crs_units(wgs84, Units).

	test(crs_projections_crs_units_2_02, deterministic(Units == meters)) :-
		crs_units(ecef, Units).

	test(crs_projections_crs_units_2_03, deterministic(Units == [degrees, degrees, meters])) :-
		crs_units(wgs84_3d, Units).

	test(crs_projections_crs_dimensions_2_01, deterministic(Dimensions == 2)) :-
		crs_dimensions(world_mercator, Dimensions).

	test(crs_projections_crs_dimensions_2_02, deterministic(Dimensions == 3)) :-
		crs_dimensions(ecef, Dimensions).

	test(crs_projections_crs_dimensions_2_03, deterministic(Dimensions == 2)) :-
		crs_dimensions(lambert_azimuthal_equal_area(geographic(38.7223, -9.1393)), Dimensions).

	test(crs_projections_crs_dimensions_2_04, deterministic(Dimensions == 3)) :-
		crs_dimensions(wgs84_3d, Dimensions).

	test(crs_projections_crs_epsg_2_01, deterministic(EPSG == 4326)) :-
		crs_epsg(wgs84, EPSG).

	test(crs_projections_crs_epsg_2_02, deterministic(EPSG == 3857)) :-
		crs_epsg(web_mercator, EPSG).

	test(crs_projections_crs_epsg_2_03, deterministic(EPSG == 4979)) :-
		crs_epsg(wgs84_3d, EPSG).

	test(crs_projections_crs_epsg_2_04, deterministic(EPSG == 3395)) :-
		crs_epsg(world_mercator, EPSG).

	test(crs_projections_crs_epsg_2_05, deterministic(EPSG == 4978)) :-
		crs_epsg(ecef, EPSG).

	test(crs_projections_crs_epsg_2_06, deterministic(EPSG == 32629)) :-
		crs_epsg(utm(29, north), EPSG).

	test(crs_projections_epsg_crs_2_01, deterministic(CRS == wgs84_3d)) :-
		epsg_crs(4979, CRS).

	test(crs_projections_epsg_crs_2_02, deterministic(CRS == world_mercator)) :-
		epsg_crs(3395, CRS).

	test(crs_projections_epsg_crs_2_03, deterministic(CRS == ecef)) :-
		epsg_crs(4978, CRS).

	test(crs_projections_epsg_crs_2_04, deterministic(CRS == utm(56, south))) :-
		epsg_crs(32756, CRS).

	test(crs_projections_valid_coordinate_2_01, deterministic) :-
		valid_coordinate(wgs84, geographic(38.7223, -9.1393)).

	test(crs_projections_valid_coordinate_2_02, deterministic) :-
		valid_coordinate(wgs84_3d, geographic(38.7223, -9.1393, 123.45)).

	test(crs_projections_valid_coordinate_2_03, deterministic) :-
		valid_coordinate(web_mercator, projected(0.0, 0.0)).

	test(crs_projections_valid_coordinate_2_04, deterministic) :-
		valid_coordinate(world_mercator, projected(0.0, 0.0)).

	test(crs_projections_valid_coordinate_2_05, deterministic) :-
		valid_coordinate(ecef, ecef(6378137.0, 0.0, 0.0)).

	test(crs_projections_valid_coordinate_2_06, deterministic) :-
		valid_coordinate(enu(geographic(38.7223, -9.1393)), enu(0.0, 0.0, 0.0)).

	test(crs_projections_valid_coordinate_2_07, deterministic) :-
		valid_coordinate(enu(geographic(38.7223, -9.1393, 100.0)), enu(0.0, 0.0, 0.0)).

	test(crs_projections_valid_coordinate_2_08, deterministic) :-
		valid_coordinate(lambert_azimuthal_equal_area(geographic(38.7223, -9.1393)), projected(0.0, 0.0)).

	test(crs_projections_valid_coordinate_2_09, deterministic) :-
		valid_coordinate(utm(31, north), grid(500000.0, 0.0)).

	test(crs_projections_valid_coordinate_2_10, fail) :-
		valid_coordinate(web_mercator, projected(30000000.0, 0.0)).

	test(crs_projections_utm_zone_2_01, deterministic(Zone == 29)) :-
		utm_zone(geographic(38.7223, -9.1393), Zone).

	test(crs_projections_utm_zone_2_02, deterministic(Zone == 32)) :-
		utm_zone(geographic(60.0, 3.1), Zone).

	test(crs_projections_utm_zone_2_03, deterministic(Zone == 33)) :-
		utm_zone(geographic(75.0, 20.0), Zone).

	test(crs_projections_utm_zone_2_04, fail) :-
		utm_zone(geographic(85.0, 0.0), _).

	test(crs_projections_utm_crs_2_01, deterministic(CRS == utm(29, north))) :-
		utm_crs(geographic(38.7223, -9.1393), CRS).

	test(crs_projections_transform_4_01, deterministic((abs(X) < 1.0e-9, abs(Y) < 1.0e-9))) :-
		transform(wgs84, web_mercator, geographic(0.0, 0.0), projected(X, Y)).

	test(crs_projections_transform_4_02, deterministic((abs(Latitude) < 1.0e-9, Longitude > 0.999999, Longitude < 1.000001))) :-
		transform(web_mercator, wgs84, projected(111319.49079327357, 0.0), geographic(Latitude, Longitude)).

	test(crs_projections_transform_4_03, deterministic((Easting > 499999.99, Easting < 500000.01, abs(Northing) < 1.0e-6))) :-
		transform(wgs84, utm(31, north), geographic(0.0, 3.0), grid(Easting, Northing)).

	test(crs_projections_transform_4_04, deterministic((abs(Latitude) < 1.0e-8, Longitude > 2.999999, Longitude < 3.000001))) :-
		transform(utm(31, north), wgs84, grid(500000.0, 0.0), geographic(Latitude, Longitude)).

	test(crs_projections_transform_4_05, deterministic((Latitude > 38.722299, Latitude < 38.722301, Longitude > -9.139301, Longitude < -9.139299))) :-
		transform(wgs84, web_mercator, geographic(38.7223, -9.1393), ProjectedCoordinate),
		transform(web_mercator, wgs84, ProjectedCoordinate, geographic(Latitude, Longitude)).

	test(crs_projections_transform_4_06, deterministic((Latitude > -45.000001, Latitude < -44.999999, Longitude > 169.999999, Longitude < 170.000001))) :-
		transform(wgs84, utm(59, south), geographic(-45.0, 170.0), ProjectedCoordinate),
		transform(utm(59, south), wgs84, ProjectedCoordinate, geographic(Latitude, Longitude)).

	test(crs_projections_transform_4_07, deterministic((Easting > 166021.0, Easting < 166022.0, abs(Northing) < 1.0e-6))) :-
		transform(web_mercator, utm(31, north), projected(0.0, 0.0), grid(Easting, Northing)).

	test(crs_projections_transform_4_08, deterministic(Coordinate == projected(1000.0, 2000.0))) :-
		transform(web_mercator, web_mercator, projected(1000.0, 2000.0), Coordinate).

	test(crs_projections_transform_4_09, fail) :-
		transform(wgs84, web_mercator, geographic(86.0, 0.0), _).

	test(crs_projections_transform_4_10, fail) :-
		transform(wgs84, utm(29, south), geographic(38.7223, -9.1393), _).

	test(crs_projections_transform_4_11, deterministic((abs(X) < 1.0e-9, Y > 5591295.91, Y < 5591295.93))) :-
		transform(wgs84, world_mercator, geographic(45.0, 0.0), projected(X, Y)).

	test(crs_projections_transform_4_12, deterministic((Latitude > 44.999999, Latitude < 45.000001, abs(Longitude) < 1.0e-9))) :-
		transform(world_mercator, wgs84, projected(0.0, 5591295.9185533915), geographic(Latitude, Longitude)).

	test(crs_projections_transform_4_13, deterministic((X > 6378136.99, X < 6378137.01, abs(Y) < 1.0e-9, abs(Z) < 1.0e-9))) :-
		transform(wgs84, ecef, geographic(0.0, 0.0), ecef(X, Y, Z)).

	test(crs_projections_transform_4_14, deterministic((abs(Latitude) < 1.0e-9, abs(Longitude) < 1.0e-9))) :-
		transform(ecef, wgs84, ecef(6378137.0, 0.0, 0.0), geographic(Latitude, Longitude)).

	test(crs_projections_transform_4_15, deterministic((X > 6378136.99, X < 6378137.01, abs(Y) < 1.0e-9, abs(Z) < 1.0e-9))) :-
		transform(web_mercator, ecef, projected(0.0, 0.0), ecef(X, Y, Z)).

	test(crs_projections_transform_4_16, deterministic((Latitude > 38.722299, Latitude < 38.722301, Longitude > -9.139301, Longitude < -9.139299))) :-
		transform(wgs84, ecef, geographic(38.7223, -9.1393), GeocentricCoordinate),
		transform(ecef, wgs84, GeocentricCoordinate, geographic(Latitude, Longitude)).

	test(crs_projections_transform_4_17, fail) :-
		transform(wgs84, world_mercator, geographic(90.0, 0.0), _).

	test(crs_projections_transform_4_18, deterministic((Latitude == 38.7223, Longitude == -9.1393, Height == 0.0))) :-
		transform(wgs84, wgs84_3d, geographic(38.7223, -9.1393), geographic(Latitude, Longitude, Height)).

	test(crs_projections_transform_4_19, deterministic((Latitude == 38.7223, Longitude == -9.1393))) :-
		transform(wgs84_3d, wgs84, geographic(38.7223, -9.1393, 123.45), geographic(Latitude, Longitude)).

	test(crs_projections_transform_4_20, deterministic((X > 6379136.99, X < 6379137.01, abs(Y) < 1.0e-9, abs(Z) < 1.0e-9))) :-
		transform(wgs84_3d, ecef, geographic(0.0, 0.0, 1000.0), ecef(X, Y, Z)).

	test(crs_projections_transform_4_21, deterministic((abs(Latitude) < 1.0e-9, abs(Longitude) < 1.0e-9, Height > 999.999999, Height < 1000.000001))) :-
		transform(ecef, wgs84_3d, ecef(6379137.0, 0.0, 0.0), geographic(Latitude, Longitude, Height)).

	test(crs_projections_transform_4_22, deterministic((Latitude > 38.722299, Latitude < 38.722301, Longitude > -9.139301, Longitude < -9.139299, Height > 123.449999, Height < 123.450001))) :-
		transform(wgs84_3d, ecef, geographic(38.7223, -9.1393, 123.45), GeocentricCoordinate),
		transform(ecef, wgs84_3d, GeocentricCoordinate, geographic(Latitude, Longitude, Height)).

	test(crs_projections_local_tangent_plane_3_01, deterministic((abs(East) < 1.0e-9, abs(North) < 1.0e-9, abs(Up) < 1.0e-9))) :-
		local_tangent_plane(geographic(38.7223, -9.1393), geographic(38.7223, -9.1393), enu(East, North, Up)).

	test(crs_projections_local_tangent_plane_3_02, deterministic((East > 86.95, East < 86.98, North > 111.00, North < 111.02, Up > -0.01, Up < 0.01))) :-
		local_tangent_plane(geographic(38.7223, -9.1393), geographic(38.7233, -9.1383), enu(East, North, Up)).

	test(crs_projections_local_tangent_plane_inverse_3_01, deterministic((Latitude > 38.723299, Latitude < 38.723301, Longitude > -9.138301, Longitude < -9.138299))) :-
		local_tangent_plane_inverse(geographic(38.7223, -9.1393), enu(86.96278605308834, 111.01067205998118, -0.0015608255733425835), geographic(Latitude, Longitude)).

	test(crs_projections_transform_4_23, deterministic((abs(East) < 1.0e-9, abs(North) < 1.0e-9, abs(Up) < 1.0e-9))) :-
		transform(wgs84, enu(geographic(38.7223, -9.1393)), geographic(38.7223, -9.1393), enu(East, North, Up)).

	test(crs_projections_transform_4_24, deterministic((Latitude > 38.723299, Latitude < 38.723301, Longitude > -9.138301, Longitude < -9.138299))) :-
		transform(enu(geographic(38.7223, -9.1393)), wgs84, enu(86.96278605308834, 111.01067205998118, -0.0015608255733425835), geographic(Latitude, Longitude)).

	test(crs_projections_transform_4_25, deterministic((abs(East) < 1.0e-9, abs(North) < 1.0e-9, Up > 24.999999, Up < 25.000001))) :-
		transform(wgs84_3d, enu(geographic(38.7223, -9.1393, 100.0)), geographic(38.7223, -9.1393, 125.0), enu(East, North, Up)).

	test(crs_projections_transform_4_26, deterministic((Latitude > 38.722299, Latitude < 38.722301, Longitude > -9.139301, Longitude < -9.139299, Height > 124.999999, Height < 125.000001))) :-
		transform(enu(geographic(38.7223, -9.1393, 100.0)), wgs84_3d, enu(0.0, 0.0, 25.0), geographic(Latitude, Longitude, Height)).

	test(crs_projections_lambert_azimuthal_equal_area_3_01, deterministic((abs(X) < 1.0e-9, abs(Y) < 1.0e-9))) :-
		lambert_azimuthal_equal_area(geographic(0.0, 0.0), geographic(0.0, 0.0), projected(X, Y)).

	test(crs_projections_lambert_azimuthal_equal_area_3_02, deterministic((X > 111193.63, X < 111193.65, abs(Y) < 1.0e-9))) :-
		lambert_azimuthal_equal_area(geographic(0.0, 0.0), geographic(0.0, 1.0), projected(X, Y)).

	test(crs_projections_lambert_azimuthal_equal_area_inverse_3_01, deterministic((abs(Latitude) < 1.0e-9, Longitude > 0.999999, Longitude < 1.000001))) :-
		lambert_azimuthal_equal_area_inverse(geographic(0.0, 0.0), projected(111193.64064936062, 0.0), geographic(Latitude, Longitude)).

	test(crs_projections_transform_4_27, deterministic((abs(X) < 1.0e-9, abs(Y) < 1.0e-9))) :-
		transform(wgs84, lambert_azimuthal_equal_area(geographic(0.0, 0.0)), geographic(0.0, 0.0), projected(X, Y)).

	test(crs_projections_transform_4_28, deterministic((abs(Latitude) < 1.0e-9, Longitude > 0.999999, Longitude < 1.000001))) :-
		transform(lambert_azimuthal_equal_area(geographic(0.0, 0.0)), wgs84, projected(111193.64064936062, 0.0), geographic(Latitude, Longitude)).

:- end_object.
