.. _library_wkt_wkb:

``wkt_wkb``
===========

The ``wkt_wkb`` library provides predicates for parsing, generating, and
validating geometries represented using the Well-Known Text (WKT) and
Well-Known Binary (WKB) interchange formats.

It complements the ``geojson`` and ``geospatial`` libraries by reusing
the same geometry constructors while adding support for the standard
text and binary geometry encodings widely used by spatial databases and
GIS tooling.

API documentation
-----------------

Open the
`../../apis/library_index.html#wkt_wkb <../../apis/library_index.html#wkt_wkb>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(wkt_wkb(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(wkt_wkb(tester)).

Representation
--------------

The library uses the same native geometry constructors as the
``geojson`` library for geometry values:

- ``point(Position)`` or ``point(Position, Options)``
- ``multi_point(Positions)`` or ``multi_point(Positions, Options)``
- ``line_string(Positions)`` or ``line_string(Positions, Options)``
- ``multi_line_string(LineStrings)`` or
  ``multi_line_string(LineStrings, Options)``
- ``polygon(Rings)`` or ``polygon(Rings, Options)``
- ``multi_polygon(Polygons)`` or ``multi_polygon(Polygons, Options)``
- ``geometry_collection(Geometries)`` or
  ``geometry_collection(Geometries, Options)``

Positions are represented as lists of numeric coordinates. Empty
geometries use empty lists, e.g. ``point([])`` or ``polygon([])``.

The optional ``Options`` lists currently support:

- ``dimensions(xy)``
- ``dimensions(z)``
- ``dimensions(m)``
- ``dimensions(zm)``

The ``dimensions/1`` option is mainly required to distinguish ``Z`` and
``M`` three-coordinate geometries and to preserve dimensionality for
empty geometries. When the option is omitted, dimensionality is inferred
from the coordinates and defaults to ``xy`` for empty geometries.

Sources and sinks
-----------------

WKT sources and sinks are wrapped as ``wkt(...)`` terms:

- ``wkt(file(Path))``
- ``wkt(stream(Stream))``
- ``wkt(atom(Atom))``
- ``wkt(chars(List))``
- ``wkt(codes(List))``

WKB sources and sinks are wrapped as ``wkb(...)`` terms:

- ``wkb(file(Path))``
- ``wkb(stream(Stream))``
- ``wkb(bytes(List))``
- ``wkb(hex(atom(Atom)))``
- ``wkb(hex(chars(List)))``
- ``wkb(hex(codes(List)))``

When generating WKB, byte order defaults to little-endian. A specific
byte order can be selected using ``wkb(Sink, little)`` or
``wkb(Sink, big)``.

Examples
--------

Parse a WKT point:

::

   | ?- wkt_wkb::parse(wkt(atom('POINT Z (1 2 3)')), Geometry).
   Geometry = point([1, 2, 3], [dimensions(z)])
   yes

Generate canonical WKT for a polygon:

::

   | ?- wkt_wkb::generate(wkt(atom(WKT)), polygon([[[0,0],[1,0],[1,1],[0,1],[0,0]]])).
   WKT = 'POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))'
   yes

Generate WKB hex for a point:

::

   | ?- wkt_wkb::generate(wkb(hex(atom(Hex))), point([1, 2])).
   Hex = '0101000000000000000000f03f0000000000000040'
   yes

Round-trip through a big-endian WKB byte sequence:

::

   | ?- wkt_wkb::generate(wkb(bytes(Bytes), big), line_string([[0,0],[1,1]])),
        wkt_wkb::parse(wkb(bytes(Bytes)), Geometry).

Validate a geometry term:

::

   | ?- wkt_wkb::validate(multi_point([[], [1, 2], [3, 4]])).
   yes

   | ?- wkt_wkb::validate(polygon([[[0,0],[1,0],[1,1],[0,1]]]), Errors).
   Errors = [ring_not_closed([coordinates,0])]
   yes

Notes
-----

- WKT parsing accepts the standard geometry keywords ``POINT``,
  ``LINESTRING``, ``POLYGON``, ``MULTIPOINT``, ``MULTILINESTRING``,
  ``MULTIPOLYGON``, and ``GEOMETRYCOLLECTION``, together with the ``Z``,
  ``M``, and ``ZM`` dimension tags.
- Canonical WKT generation always uses uppercase geometry keywords.
- WKB generation uses the ISO/OGC type-code offsets ``1000``, ``2000``,
  and ``3000`` for ``Z``, ``M``, and ``ZM`` dimensionality.
- Empty WKB points are encoded using quiet NaN ordinates as required by
  the format, while empty non-point geometries are encoded using zero
  counts.
- Geometry validation checks coordinate dimensionality consistency,
  minimum cardinalities for line strings and rings, and polygon ring
  closure.
