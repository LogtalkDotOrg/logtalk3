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


:- protocol(crs_projections_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-10,
		comment is 'Coordinate reference systems and coordinate transformation predicates protocol.',
		see_also is [crs_projections, geospatial_protocol]
	]).

	:- public(valid_crs/1).
	:- mode(valid_crs(@term), zero_or_one).
	:- info(valid_crs/1, [
		comment is 'True when the argument is a supported coordinate reference system. Supported values are ``wgs84``, ``wgs84_3d``, ``web_mercator``, ``world_mercator``, ``ecef``, ``enu(OriginCoordinate)``, ``lambert_azimuthal_equal_area(OriginCoordinate)``, and ``utm(Zone,Hemisphere)`` terms.',
		argnames is ['CRS']
	]).

	:- public(crs_name/2).
	:- mode(crs_name(+term, -atom), zero_or_one).
	:- info(crs_name/2, [
		comment is 'Maps a supported coordinate reference system to a human-readable name.',
		argnames is ['CRS', 'Name']
	]).

	:- public(crs_kind/2).
	:- mode(crs_kind(+term, -atom), zero_or_one).
	:- info(crs_kind/2, [
		comment is 'Classifies a supported coordinate reference system as ``geographic``, ``projected``, ``geocentric``, or ``local_tangent_plane``.',
		argnames is ['CRS', 'Kind']
	]).

	:- public(crs_units/2).
	:- mode(crs_units(+term, -term), zero_or_one).
	:- info(crs_units/2, [
		comment is 'Returns the coordinate axis units used by a supported coordinate reference system. Supported values are ``degrees``, ``meters``, and ``[degrees,degrees,meters]``.',
		argnames is ['CRS', 'Units']
	]).

	:- public(crs_dimensions/2).
	:- mode(crs_dimensions(+term, -positive_integer), zero_or_one).
	:- info(crs_dimensions/2, [
		comment is 'Returns the coordinate dimensionality for a supported coordinate reference system.',
		argnames is ['CRS', 'Dimensions']
	]).

	:- public(crs_epsg/2).
	:- mode(crs_epsg(+term, -positive_integer), zero_or_one).
	:- info(crs_epsg/2, [
		comment is 'Maps a supported coordinate reference system to the corresponding EPSG code.',
		argnames is ['CRS', 'EPSG']
	]).

	:- public(epsg_crs/2).
	:- mode(epsg_crs(+positive_integer, -term), zero_or_one).
	:- info(epsg_crs/2, [
		comment is 'Maps a supported EPSG code to the corresponding coordinate reference system term.',
		argnames is ['EPSG', 'CRS']
	]).

	:- public(valid_coordinate/2).
	:- mode(valid_coordinate(+term, @compound), zero_or_one).
	:- info(valid_coordinate/2, [
		comment is 'True when the coordinate is valid for the given coordinate reference system. ``wgs84`` coordinates are represented as ``geographic(Latitude,Longitude)`` in degrees, ``wgs84_3d`` coordinates as ``geographic(Latitude,Longitude,EllipsoidalHeight)`` in degrees and meters, ``web_mercator``, ``world_mercator``, and ``lambert_azimuthal_equal_area(OriginCoordinate)`` coordinates as ``projected(X,Y)`` pairs in meters, ``ecef`` coordinates as ``ecef(X,Y,Z)`` triples in meters, ``enu(OriginCoordinate)`` coordinates as ``enu(East,North,Up)`` triples in meters, and ``utm(Zone,Hemisphere)`` coordinates as ``grid(Easting,Northing)`` pairs in meters.',
		argnames is ['CRS', 'Coordinate']
	]).

	:- public(local_tangent_plane/3).
	:- mode(local_tangent_plane(+compound, +compound, -compound), zero_or_one).
	:- info(local_tangent_plane/3, [
		comment is 'Projects a WGS84 geographic coordinate to local ``enu(East,North,Up)`` coordinates in meters using an origin coordinate. For height-preserving 3D geodetic workflows, use ``transform/4`` with ``wgs84_3d``.',
		argnames is ['Origin', 'Coordinate', 'LocalCoordinate']
	]).

	:- public(local_tangent_plane_inverse/3).
	:- mode(local_tangent_plane_inverse(+compound, +compound, -compound), zero_or_one).
	:- info(local_tangent_plane_inverse/3, [
		comment is 'Converts local ``enu(East,North,Up)`` coordinates in meters back to a WGS84 geographic coordinate using an origin coordinate. For height-preserving 3D geodetic workflows, use ``transform/4`` with ``wgs84_3d``.',
		argnames is ['Origin', 'LocalCoordinate', 'Coordinate']
	]).

	:- public(lambert_azimuthal_equal_area/3).
	:- mode(lambert_azimuthal_equal_area(+compound, +compound, -compound), zero_or_one).
	:- info(lambert_azimuthal_equal_area/3, [
		comment is 'Projects a WGS84 geographic coordinate to a Lambert azimuthal equal-area plane centered on an origin coordinate, returning ``projected(X,Y)`` coordinates in meters.',
		argnames is ['Origin', 'Coordinate', 'ProjectedCoordinate']
	]).

	:- public(lambert_azimuthal_equal_area_inverse/3).
	:- mode(lambert_azimuthal_equal_area_inverse(+compound, +compound, -compound), zero_or_one).
	:- info(lambert_azimuthal_equal_area_inverse/3, [
		comment is 'Converts Lambert azimuthal equal-area ``projected(X,Y)`` coordinates back to a WGS84 geographic coordinate using an origin coordinate.',
		argnames is ['Origin', 'ProjectedCoordinate', 'Coordinate']
	]).

	:- public(utm_zone/2).
	:- mode(utm_zone(+compound, -integer), zero_or_one).
	:- info(utm_zone/2, [
		comment is 'Infers the UTM longitudinal zone for a WGS84 ``geographic(Latitude,Longitude)`` coordinate. Uses the standard Norway and Svalbard special-zone rules and only succeeds for coordinates within the UTM latitude coverage.',
		argnames is ['Coordinate', 'Zone']
	]).

	:- public(utm_crs/2).
	:- mode(utm_crs(+compound, -compound), zero_or_one).
	:- info(utm_crs/2, [
		comment is 'Infers the native UTM coordinate reference system term ``utm(Zone,Hemisphere)`` for a WGS84 ``geographic(Latitude,Longitude)`` coordinate.',
		argnames is ['Coordinate', 'CRS']
	]).

	:- public(transform/4).
	:- mode(transform(+term, +term, +compound, -compound), zero_or_one).
	:- info(transform/4, [
		comment is 'Transforms a coordinate between supported coordinate reference systems. Transformations between projected systems are computed by converting through ``wgs84``.',
		argnames is ['SourceCRS', 'TargetCRS', 'Coordinate', 'TransformedCoordinate']
	]).

:- end_protocol.
