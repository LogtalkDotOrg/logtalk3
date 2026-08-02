.. _library_gpx:

``gpx``
=======

The ``gpx`` library provides predicates for parsing, generating, and
validating GPX 1.1 documents. GPX is an XML interchange format for GPS
waypoints, routes, and tracks. Coordinates use the WGS84 datum and
measurements use metric units.

- https://www.topografix.com/gpx.asp
- https://www.topografix.com/GPX/1/1/

The library uses the ``dates`` library for Gregorian calendar validation
and the contributed ``xml_parser`` library for portable XML parsing and
generation. GPX structure and scalar constraints are checked directly;
there is no dependency on an XSD processor.

API documentation
-----------------

Open the
`../../apis/library_index.html#gpx <../../apis/library_index.html#gpx>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(gpx(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(gpx(tester)).

Representation
--------------

A document is represented by the term ``gpx(Creator, Properties)``. The
GPX version is always 1.1 and is therefore not repeated in the native
term. Root properties are:

- ``metadata(metadata(Properties))``
- ``waypoints(Waypoints)``
- ``routes(Routes)``
- ``tracks(Tracks)``
- ``extensions(XMLNodes)``

Points are represented by
``waypoint(geographic(Latitude, Longitude), Properties)``. The same
point term is used for standalone waypoints, route points, and track
points. Point properties include ``elevation/1``, ``time/1``,
``magnetic_variation/1``, ``geoid_height/1``, ``name/1``, ``comment/1``,
``description/1``, ``source/1``, ``links/1``, ``symbol/1``, ``type/1``,
``fix/1``, ``satellites/1``, ``hdop/1``, ``vdop/1``, ``pdop/1``,
``age_of_dgps_data/1``, ``dgps_station/1``, and ``extensions/1``.

Routes and tracks use these terms:

- ``route(Points, Properties)``
- ``track(Segments, Properties)``
- ``track_segment(Points, Properties)``

Nested metadata values use ``person(Properties)``,
``email(Id, Domain)``, ``copyright(Author, Properties)``,
``link(Href, Properties)``, and
``bounds(MinLatitude, MinLongitude, MaxLatitude, MaxLongitude)`` terms.

Optional values are omitted from property lists. Property order is
ignored when validating or generating a document. Generated XML always
follows the element order required by the GPX 1.1 schema; parsing also
rejects elements in an invalid order.

The ``time/1`` values are atoms using the XML Schema 1.0 ``dateTime``
lexical space. Calendar dates, leap years and seconds, fractional
seconds, the special ``24:00:00`` representation, expanded and negative
years, and time zones up to ``14:00`` are validated without changing the
original atom. Copyright ``year/1`` values similarly use the XML Schema
1.0 ``gYear`` lexical space. In both types, year zero is invalid and
expanded years cannot have leading zeroes.

Validation errors
-----------------

The ``validate/2`` predicate returns all detected errors. Every reason
term ends in a path from the GPX root to the failing value. List indexes
are zero-based; for example, ``[tracks,0,segments,1,points,3,time]``
identifies the time property of the fourth point in the second segment
of the first track.

Reasons distinguish malformed native terms, creators, property and value
lists, scalar values and ranges, date/time and year lexical values,
bounds, extensions, duplicate properties, and unknown properties. See
the ``gpx_protocol`` API documentation for the complete reason-term
vocabulary.

Extensions
----------

Children of GPX ``extensions`` elements are preserved using the
``xml_parser`` native XML terms. Their top-level elements must use a
namespace other than the GPX namespace. Namespace URIs, prefixes,
attributes, text, and nested content are preserved across parse and
generate operations.

Extension schemas are not interpreted or validated. This corresponds to
the portable part of the GPX schema extension contract; its XSD
``processContents="lax"`` behavior would require an XSD processor.
