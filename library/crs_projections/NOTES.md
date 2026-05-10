________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`crs_projections`
=================

This library provides a `crs_projections_protocol` protocol and a
`crs_projections` object for working with a small set of common coordinate
reference systems and transforming coordinates between them.

Both 2D and explicit 3D WGS84 geodetic coordinates are supported.


API documentation
-----------------

Open the [../../apis/library_index.html#crs_projections](../../apis/library_index.html#crs_projections)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(crs_projections(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(crs_projections(tester)).


Supported coordinate reference systems
--------------------------------------

The library currently supports:

- `wgs84` geographic coordinates represented as `geographic(Latitude,Longitude)` in degrees
- `wgs84_3d` geographic coordinates represented as `geographic(Latitude,Longitude,EllipsoidalHeight)` in degrees and meters
- `web_mercator` projected coordinates represented as `projected(X,Y)` in meters
- `world_mercator` projected coordinates represented as `projected(X,Y)` in meters
- `ecef` geocentric coordinates represented as `ecef(X,Y,Z)` in meters
- `enu(OriginCoordinate)` local tangent-plane coordinates represented as `enu(East,North,Up)` in meters
- `lambert_azimuthal_equal_area(OriginCoordinate)` equal-area projected coordinates represented as `projected(X,Y)` in meters
- `utm(Zone,Hemisphere)` projected coordinates represented as `grid(Easting,Northing)` in meters

EPSG helpers are available for these reference systems:

- `4326` for `wgs84`
- `4979` for `wgs84_3d`
- `3857` for `web_mercator`
- `3395` for `world_mercator`
- `4978` for `ecef`
- `32601`-`32660` for northern-hemisphere UTM zones
- `32701`-`32760` for southern-hemisphere UTM zones

Additional metadata helpers are provided for querying CRS names, kinds,
coordinate units, and dimensions.

Additional projection helpers are provided for working directly with local ENU
tangent-plane coordinates and Lambert azimuthal equal-area coordinates.


Usage
-----

Load the library:

	| ?- logtalk_load(crs_projections(loader)).

Validate CRS terms and look up EPSG codes:

	| ?- crs_projections::valid_crs(utm(29, north)).
	| ?- crs_projections::crs_name(wgs84_3d, Name).
	| ?- crs_projections::crs_name(ecef, Name).
	| ?- crs_projections::crs_kind(world_mercator, Kind).
	| ?- crs_projections::crs_units(wgs84, Units).
	| ?- crs_projections::crs_units(wgs84_3d, Units).
	| ?- crs_projections::crs_dimensions(ecef, Dimensions).
	| ?- crs_projections::crs_dimensions(wgs84_3d, Dimensions).
	| ?- crs_projections::crs_epsg(web_mercator, EPSG).
	| ?- crs_projections::crs_epsg(wgs84_3d, EPSG).
	| ?- crs_projections::epsg_crs(32629, CRS).
	| ?- crs_projections::epsg_crs(4979, CRS).
	| ?- crs_projections::valid_crs(enu(geographic(38.7223, -9.1393))).
	| ?- crs_projections::valid_crs(lambert_azimuthal_equal_area(geographic(38.7223, -9.1393))).

Infer the native UTM CRS for a WGS84 coordinate:

	| ?- crs_projections::utm_zone(geographic(38.7223, -9.1393), Zone).
	| ?- crs_projections::utm_crs(geographic(38.7223, -9.1393), CRS).

Transform WGS84 coordinates to projected systems and back:

	| ?- crs_projections::transform(wgs84, web_mercator, geographic(38.7223, -9.1393), Projected).
	| ?- crs_projections::transform(wgs84, wgs84_3d, geographic(38.7223, -9.1393), Geodetic3D).
	| ?- crs_projections::transform(wgs84_3d, ecef, geographic(38.7223, -9.1393, 123.45), Geocentric3D).
	| ?- crs_projections::transform(ecef, wgs84_3d, Geocentric3D, Geodetic3D).
	| ?- crs_projections::transform(web_mercator, wgs84, Projected, Coordinate).
	| ?- crs_projections::transform(wgs84, world_mercator, geographic(38.7223, -9.1393), Mercator).
	| ?- crs_projections::transform(wgs84, ecef, geographic(38.7223, -9.1393), Geocentric).
	| ?- crs_projections::transform(wgs84_3d, enu(geographic(38.7223, -9.1393, 100.0)), geographic(38.7223, -9.1393, 125.0), Local3D).
	| ?- crs_projections::transform(enu(geographic(38.7223, -9.1393, 100.0)), wgs84_3d, Local3D, Geodetic3D).
	| ?- crs_projections::transform(wgs84, enu(geographic(38.7223, -9.1393)), geographic(38.7233, -9.1383), Local).
	| ?- crs_projections::transform(wgs84, lambert_azimuthal_equal_area(geographic(38.7223, -9.1393)), geographic(38.7233, -9.1383), EqualArea).
	| ?- crs_projections::transform(wgs84, utm(29, north), geographic(38.7223, -9.1393), UTM).
	| ?- crs_projections::transform(utm(29, north), wgs84, UTM, Coordinate).

Use direct helper predicates for local tangent-plane and equal-area workflows:

	| ?- crs_projections::local_tangent_plane(geographic(38.7223, -9.1393), geographic(38.7233, -9.1383), Local).
	| ?- crs_projections::local_tangent_plane_inverse(geographic(38.7223, -9.1393), Local, Coordinate).
	| ?- crs_projections::lambert_azimuthal_equal_area(geographic(38.7223, -9.1393), geographic(38.7233, -9.1383), EqualArea).
	| ?- crs_projections::lambert_azimuthal_equal_area_inverse(geographic(38.7223, -9.1393), EqualArea, Coordinate).


Notes
-----

The `utm_zone/2`, `utm_crs/2`, and UTM transformation predicates only succeed
for latitudes in the `[-80.0,84.0[` range, matching the standard UTM coverage.

Transformations between projected coordinate reference systems are implemented
by converting through `wgs84`.

When converting from `wgs84` to `wgs84_3d`, the ellipsoidal height defaults to
`0.0`. When converting from 2D projected or geographic coordinate reference
systems to `wgs84_3d`, the resulting ellipsoidal height is also `0.0` because
the source coordinate reference system does not carry height information.

Transforms between `ecef` and projected coordinate reference systems are also
implemented through `wgs84`.

Transforms involving `enu(OriginCoordinate)` preserve the local `Up` component
when converting directly to or from `ecef` and `wgs84_3d`.
