.. _library_geohash:

``geohash``
===========

The ``geohash`` library provides a ``geohash_protocol`` protocol and a
``geohash`` object for encoding and decoding geographic coordinates
using the standard geohash algorithm.

It uses the standard geohash base-32 alphabet:

- ``0123456789bcdefghjkmnpqrstuvwxyz``

The library complements the ``geospatial`` library and uses the same
``geographic(Latitude,Longitude)`` coordinate representation and
``bbox(geographic(MinLatitude,MinLongitude),geographic(MaxLatitude,MaxLongitude))``
bounding-box representation.

For encoding, longitudes are canonicalized to the ``[-180.0,180.0[``
range so that ``geographic(Latitude,180.0)`` and
``geographic(Latitude,-180.0)`` encode to the same geohash.

API documentation
-----------------

Open the
`../../apis/library_index.html#geohash <../../apis/library_index.html#geohash>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(geohash(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(geohash(tester)).

Supported operations
--------------------

The library provides predicates for:

- validating geohash atoms
- encoding coordinates at a requested precision
- decoding a geohash to its cell center
- decoding a geohash to its cell bounding box
- querying the nominal latitude and longitude precision of a geohash
  length
- querying cell dimensions for a geohash length
- computing adjacent and neighboring cells
- covering a geographic bounding box with geohashes of a fixed precision
- covering a geographic bounding box using fixed-depth or adaptive
  mixed-depth covers
- compressing complete sibling sets into parent prefixes
- navigating geohash parent-child relationships and shared prefixes
- converting between atom geohashes and integer-backed 5-bit packed
  representations
- converting between atom and integer geohash representations explicitly
- encoding and decoding integer geohashes using bit precision
- expanding a geohash to the center cell plus neighbors
- covering polygons and polylines with geohashes

Examples
--------

Encode a coordinate at a selected precision:

::

   | ?- geohash::encode(geographic(42.6, -5.6), 5, Geohash).
   Geohash = 'ezs42'
   yes

   | ?- geohash::encode(geographic(57.64911, 10.40744), 11, Geohash).
   Geohash = 'u4pruydqqvj'
   yes

Decode a geohash to its center coordinate and bounding box:

::

   | ?- geohash::decode('ezs42', Coordinate).
   Coordinate = geographic(42.60498046875, -5.60302734375)
   yes

   | ?- geohash::bounding_box('ezs42', BoundingBox).
   BoundingBox = bbox(geographic(42.5830078125, -5.625), geographic(42.626953125, -5.5810546875))
   yes

Inspect cell precision metadata:

::

   | ?- geohash::precision(5, LatitudeError, LongitudeError).
   | ?- geohash::cell_dimensions(5, LatitudeSpan, LongitudeSpan).

Compute adjacent cells and neighbors:

::

   | ?- geohash::adjacent('ezs42', north, North).
   | ?- geohash::neighbors('ezs42', Neighbors).

Cover a bounding box with geohashes of a fixed precision:

::

   | ?- geohash::covering(bbox(geographic(42.58, -5.63), geographic(42.63, -5.58)), 5, Geohashes).

Compute adaptive and compact covers:

::

   | ?- geohash::covering(bbox(geographic(42.58, -5.63), geographic(42.63, -5.58)), max_precision(6), Geohashes, []).

   | ?- geohash::covering(bbox(geographic(42.58, -5.63), geographic(42.63, -5.58)), precision(6), Geohashes, [compact(true), min_precision(4)]).

Inspect hierarchy and integer representations:

::

   | ?- geohash::parent('ezs42', Parent).

   | ?- geohash::children('ezs4', Children).

   | ?- geohash::encode_int(geographic(42.6, -5.6), 5, HashInteger).

   | ?- geohash::bounding_box_int(HashInteger, 5, BoundingBox).

   | ?- geohash::geohash_to_int('ezs42', HashInteger).

   | ?- geohash::int_to_geohash(HashInteger, 5, Geohash).

   | ?- geohash::encode_bits(geographic(42.6, -5.6), 25, HashInteger).

   | ?- geohash::expand('ezs42', ExpandedGeohashes).

Cover polygons and polylines:

::

   | ?- geohash::polygon_covering(
   |       [geographic(42.5830078125, -5.625), geographic(42.626953125, -5.625), geographic(42.626953125, -5.5810546875), geographic(42.5830078125, -5.5810546875)],
   |       precision(5),
   |       Geohashes,
   |       []
   |   ).

   | ?- geohash::polyline_covering(
   |       [geographic(42.60498046875, -5.60302734375), geographic(42.60498046875, -5.55)],
   |       precision(5),
   |       Geohashes,
   |       [buffer(0.1)]
   |   ).

Notes
-----

- Geohash cell width and height alternate between equal and unequal
  angular resolution depending on whether the geohash length is even or
  odd.
- Encoding canonicalizes longitude ``180.0`` to ``-180.0`` so
  dateline-equivalent coordinates map to the same hash.
- Neighbor and covering computations use decoded cell dimensions and
  therefore handle antimeridian wrapping for eastward and westward
  movement.
- Northward adjacency from the northernmost cells and southward
  adjacency from the southernmost cells fails because there is no cell
  beyond the poles.
- Bounding-box covering accepts antimeridian-crossing boxes by allowing
  the minimum longitude to be greater than the maximum longitude.
- Degenerate point bounding boxes are accepted by ``covering/3`` and
  return the single geohash cell containing the point.
- ``covering/4`` keeps ``covering/3`` as the fixed-precision shorthand
  while adding a ``CoverSpec`` argument with ``precision(Precision)``
  and ``max_precision(MaxPrecision)`` forms.
- ``geohash`` now imports the shared ``options`` library and uses it to
  validate and merge covering options.
- ``covering/4`` and ``polygon_covering/4`` accept ``compact(Boolean)``
  and ``min_precision(PositiveInteger)`` options. Compact covers never
  compress below the selected minimum precision.
- ``polyline_covering/4`` also accepts a ``buffer(Distance)`` option
  where the distance is expressed in kilometers.
- ``compress/2`` operates on complete sibling sets only, replacing them
  with parent prefixes while preserving the covered area.
- Integer-backed predicates mirror the string predicates and keep the
  precision argument explicit, avoiding any hidden dependence on integer
  bit width.
- Bit-precision predicates provide the same packed integer
  representation while measuring precision directly in bits instead of
  characters.
- ``polygon_covering/4`` and ``polyline_covering/4`` rely on generic
  ``geospatial`` geometry predicates and currently do not support
  antimeridian-crossing input geometries.

References
----------

- https://en.wikipedia.org/wiki/Geohash
- https://geohash.org/
