.. _library_geospatial:

``geospatial``
==============

This library provides a ``geospatial_protocol`` protocol and a
``geospatial`` object for common geographic computations over
coordinates represented as ``(Latitude,Longitude)``. By default,
distances are returned in kilometers.

API documentation
-----------------

Open the
`../../apis/library_index.html#geospatial <../../apis/library_index.html#geospatial>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(geospatial(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(geospatial(tester)).

Available predicates
--------------------

The library currently includes predicates for:

- Coordinate validation
- Coordinate normalization helpers
- Point-to-point distances (``haversine`` and ``vincenty``)
- Rhumb-line distance, bearing, and destination predicates
- Rhumb-line interpolation and midpoint helpers
- Generic distance dispatch (``distance/4`` and ``distance/5``)
- Initial and final bearings
- Midpoint and destination point computations
- Great-circle interpolation and track-distance helpers
- Proximity checks and nearest coordinate search
- Mean center and coordinates bounding boxes
- Minimum enclosing circle helper
- Coordinates-to-bounding-box conversion helper
- Equirectangular projection and inverse helpers
- Point-in-polygon checks
- Polygon area, polygon centroid, and polygon bounding boxes
- Polygon closure and orientation helpers
- Polygon orientation normalization helpers
- Polygon validity checks
- Bounding-box utilities
- Nearest-point and point-to-polyline distance helpers
- Polyline length and polygon perimeter
- Polyline simplification by tolerance
- Polyline split and resampling helpers
- Polygon intersection checks
- Spherical bounding boxes
- Route distance accumulation (``route_distance/2``,
  ``route_distance/3``, and ``route_distance/4``)

For explicit units, predicates ``distance/5`` and ``route_distance/4``
support ``kilometers``, ``meters``, ``miles``, and ``nautical_miles``.

Generic metric dispatch accepts ``rhumb`` (and alias ``loxodrome``) in
addition to existing spherical and ellipsoidal metrics.

Notes
-----

The generic vector distance predicates already available in the
``types`` library (notably in the ``listp`` and ``numberlistp``
protocols) remain the recommended choice for non-geographic
n-dimensional data.

Usage
-----

Load the library:

::

   | ?- logtalk_load(geospatial(loader)).

Validate a coordinate:

::

   | ?- geospatial::valid_coordinate((38.7223, -9.1393)).

Normalize coordinates and convert to/from local planar coordinates:

::

   | ?- geospatial::normalize_coordinate((95.0, 10.0), Normalized).
   | ?- geospatial::equirectangular_projection((38.7223, -9.1393), 38.0, X, Y).
   | ?- geospatial::equirectangular_inverse(X, Y, 38.0, Coordinate).

Compute default distance in kilometers (Haversine):

::

   | ?- geospatial::distance((38.7223, -9.1393), (41.1579, -8.6291), haversine, Distance).
   | ?- geospatial::rhumb_distance((38.7223, -9.1393), (41.1579, -8.6291), Distance).
   | ?- geospatial::rhumb_bearing((38.7223, -9.1393), (41.1579, -8.6291), Bearing).
   | ?- geospatial::rhumb_destination_point((38.7223, -9.1393), 45.0, 50.0, Destination).
   | ?- geospatial::interpolate_rhumb((38.7223, -9.1393), (41.1579, -8.6291), 0.5, Intermediate).
   | ?- geospatial::rhumb_midpoint((38.7223, -9.1393), (41.1579, -8.6291), Midpoint).

Compute distance with explicit unit (``meters``, ``miles``, or
``nautical_miles``):

::

   | ?- geospatial::distance((38.7223, -9.1393), (41.1579, -8.6291), vincenty, miles, Distance).

Compute route distance using the default metric (``haversine``) in
kilometers:

::

   | ?- geospatial::route_distance([(38.7223, -9.1393), (39.7440, -8.8070), (41.1579, -8.6291)], Distance).

Compute route distance with explicit metric and unit:

::

   | ?- geospatial::route_distance([(38.7223, -9.1393), (39.7440, -8.8070), (41.1579, -8.6291)], vincenty, nautical_miles, Distance).

Compute midpoint and destination point:

::

   | ?- geospatial::midpoint((38.7223, -9.1393), (41.1579, -8.6291), Midpoint).
   | ?- geospatial::destination_point((38.7223, -9.1393), 45.0, 50.0, Destination).
   | ?- geospatial::interpolate_great_circle((38.7223, -9.1393), (41.1579, -8.6291), 0.5, Intermediate).
   | ?- geospatial::cross_track_distance((40.0, -9.0), (38.7223, -9.1393), (41.1579, -8.6291), CrossTrackKm).
   | ?- geospatial::along_track_distance((40.0, -9.0), (38.7223, -9.1393), (41.1579, -8.6291), AlongTrackKm).

Compute final bearing and proximity checks:

::

   | ?- geospatial::final_bearing((38.7223, -9.1393), (41.1579, -8.6291), Bearing).
   | ?- geospatial::within_distance((38.7223, -9.1393), (41.1579, -8.6291), 300.0, haversine).

Find the nearest coordinate from a list:

::

   | ?- geospatial::nearest_coordinate((38.7223, -9.1393), [(37.7749, -122.4194), (41.1579, -8.6291), (40.4168, -3.7038)], vincenty, Nearest, Distance).

Compute center and coordinate-list bounding boxes:

::

   | ?- geospatial::mean_center([(38.7223, -9.1393), (41.1579, -8.6291), (40.4168, -3.7038)], Center).
   | ?- geospatial::minimum_enclosing_circle([(38.7223, -9.1393), (41.1579, -8.6291), (40.4168, -3.7038)], Center, Radius).
   | ?- geospatial::coordinates_bounding_box([(38.7223, -9.1393), (41.1579, -8.6291), (40.4168, -3.7038)], BoundingBox).
   | ?- geospatial::bbox_from_coordinates([(38.7223, -9.1393), (41.1579, -8.6291), (40.4168, -3.7038)], BoundingBox).

Work with polygons:

::

   | ?- geospatial::point_in_polygon((0.5, 0.5), [(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)]).
   | ?- geospatial::polygon_area([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], Area).
   | ?- geospatial::polygon_centroid([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], Centroid).
   | ?- geospatial::polygon_bounding_box([(1.0, -2.0), (0.0, 1.0), (-1.0, -1.0)], BoundingBox).
   | ?- geospatial::close_polygon([(0.0, 0.0), (0.0, 1.0), (1.0, 0.0)], ClosedPolygon).
   | ?- geospatial::polygon_orientation([(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)], Orientation).
   | ?- geospatial::is_clockwise_polygon([(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)]).
   | ?- geospatial::normalize_polygon_orientation([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], clockwise, Oriented).
   | ?- geospatial::clockwise_polygon([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], Clockwise).
   | ?- geospatial::counterclockwise_polygon([(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)], Counterclockwise).
   | ?- geospatial::is_valid_polygon([(0.0, 0.0), (0.0, 1.0), (1.0, 0.0)]).

Work with bounding boxes:

::

   | ?- geospatial::bbox_contains(bbox((-1.0, -1.0), (1.0, 1.0)), (0.5, 0.5)).
   | ?- geospatial::bbox_intersects(bbox((0.0, 0.0), (1.0, 1.0)), bbox((0.5, 0.5), (2.0, 2.0))).
   | ?- geospatial::bbox_union(bbox((0.0, 0.0), (1.0, 1.0)), bbox((-1.0, 0.5), (0.5, 2.0)), BoundingBox).
   | ?- geospatial::bbox_expand(bbox((0.0, 0.0), (0.0, 0.0)), 111.195, ExpandedBoundingBox).

Compute nearest points and distances to paths:

::

   | ?- geospatial::nearest_point_on_segment((1.0, 1.0), (0.0, 0.0), (0.0, 2.0), Nearest).
   | ?- geospatial::nearest_point_on_polyline((1.0, 1.0), [(0.0, 0.0), (0.0, 2.0), (2.0, 2.0)], Nearest, Distance).
   | ?- geospatial::point_to_polyline_distance((1.0, 1.0), [(0.0, 0.0), (0.0, 2.0)], Distance).

Compute polyline and polygon lengths:

::

   | ?- geospatial::polyline_length([(0.0, 0.0), (0.0, 1.0), (0.0, 2.0)], Length).
   | ?- geospatial::polyline_length([(0.0, 0.0), (0.0, 1.0)], vincenty, Length).
   | ?- geospatial::polyline_simplify([(0.0, 0.0), (0.5, 0.1), (1.0, 0.0)], 20.0, Simplified).
   | ?- geospatial::polyline_split_at_distance([(0.0, 0.0), (0.0, 1.0), (0.0, 2.0)], 111.195, Left, Right).
   | ?- geospatial::polyline_resample([(0.0, 0.0), (0.0, 1.0), (0.0, 2.0)], 50.0, Resampled).
   | ?- geospatial::polygon_perimeter([(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)], Perimeter).

Check polygon intersections:

::

   | ?- geospatial::polygons_intersect([(0.0, 0.0), (0.0, 2.0), (2.0, 2.0), (2.0, 0.0)], [(1.0, 1.0), (1.0, 3.0), (3.0, 3.0), (3.0, 1.0)]).
