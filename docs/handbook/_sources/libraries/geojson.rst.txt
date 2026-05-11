.. _library_geojson:

``geojson``
===========

The ``geojson`` library provides predicates for parsing, generating, and
validating GeoJSON documents as specified by RFC 7946:

- https://www.rfc-editor.org/rfc/rfc7946

It builds on top of the ``json`` library for JSON parsing and generation
and complements the ``geospatial`` library by providing a standard
interchange format for geometry, features, and feature collections.

API documentation
-----------------

Open the
`../../apis/library_index.html#geojson <../../apis/library_index.html#geojson>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(geojson(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(geojson(tester)).

Representation
--------------

The library parses GeoJSON documents into native terms using the
following representation:

- ``point(Position)`` or ``point(Position, Options)``
- ``multi_point(Positions)`` or ``multi_point(Positions, Options)``
- ``line_string(Positions)`` or ``line_string(Positions, Options)``
- ``multi_line_string(LineStrings)`` or
  ``multi_line_string(LineStrings, Options)``
- ``polygon(Rings)`` or ``polygon(Rings, Options)``
- ``multi_polygon(Polygons)`` or ``multi_polygon(Polygons, Options)``
- ``geometry_collection(Geometries)`` or
  ``geometry_collection(Geometries, Options)``
- ``feature(Geometry, Properties)`` or
  ``feature(Geometry, Properties, Options)``
- ``feature_collection(Features)`` or
  ``feature_collection(Features, Options)``

Positions follow the GeoJSON order and are represented as lists of
numbers, e.g. ``[Longitude, Latitude]`` or
``[Longitude, Latitude, Altitude]``. Validation also checks longitude
and latitude values against the RFC 7946 geographic ranges.

The ``Options`` lists may contain:

- ``bbox(BBox)`` for the optional bounding box member
- ``id(Id)`` for feature identifiers
- ``foreign_members(ForeignMembers)`` for additional non-reserved
  members, represented either as a list of pairs or as an embedded JSON
  object term using the selected object representation

Feature geometries and properties may be ``@null`` to represent GeoJSON
``null``.

Examples
--------

Parse a point document:

::

   | ?- geojson::parse(atom('{"type":"Point","coordinates":[100.0,0.0]}'), GeoJSON).
   GeoJSON = point([100.0, 0.0])
   yes

Generate a GeoJSON feature collection:

::

   | ?- geojson::generate(atom(JSON), feature_collection([
       feature(point([102.0, 0.5]), {prop0-value0}),
       feature(@null, @null)
   ], [bbox([100.0, 0.0, 105.0, 1.0])])).

Validate a native GeoJSON term:

::

   | ?- geojson::validate(polygon([[[100.0,0.0],[101.0,0.0],[101.0,1.0],[100.0,1.0],[100.0,0.0]]])).

Inspect validation errors:

::

   | ?- geojson::validate(polygon([[[100.0,0.0],[101.0,0.0],[101.0,1.0],[100.0,1.0]]]), Errors).
   Errors = [ring_not_closed([coordinates,0])]
   yes

Convert between JSON terms and native GeoJSON terms:

::

   | ?- geojson::json_to_geojson({type-'Point', coordinates-[1,2]}, GeoJSON).
   GeoJSON = point([1,2])
   yes

   | ?- geojson::geojson_to_json(feature(@null, @null, [id(1)]), JSON).
   JSON = {type-'Feature', geometry-@null, properties-@null, id-1}
   yes

Notes
-----

- The library validates the RFC 7946 structural rules for geometry,
  features, feature collections, linear rings, bounding boxes, and
  prohibited ``crs`` members.
- Bounding boxes are represented as plain lists of numbers with either
  four or six elements. Longitude values must stay within
  ``[-180,180]``, latitude values within ``[-90,90]``, south must not
  exceed north, and three-dimensional boxes also require minimum
  altitude not to exceed maximum altitude. Bounding box dimensionality
  must match the represented geometry dimensionality, so two-dimensional
  geometries use four-element boxes while geometries with altitude use
  six-element boxes. Mixed two-dimensional and three-dimensional
  geometry or feature collections are treated as three-dimensional for
  bounding-box validation. Antimeridian-crossing boxes are accepted, so
  west may still be greater than east.
- Parsing rejects duplicate reserved GeoJSON members instead of silently
  normalizing them.
- Properties and foreign member values are recursively validated as JSON
  values compatible with the selected embedded object representation.
- When using the ``chars`` or ``codes`` string representations, string
  terms are validated as proper character or character-code lists.
- Foreign members are preserved but reserved GeoJSON member names are
  rejected when used as foreign member keys, regardless of whether the
  foreign members are supplied as pair lists or embedded object terms.
- The ``geojson/3`` parametric object mirrors the ``json/3``
  customization parameters for embedded object, pair, and string
  representations.
