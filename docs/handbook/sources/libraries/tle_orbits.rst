.. _library_tle_orbits:

``tle_orbits``
==============

The ``tle_orbits`` library parses Two-Line Element (TLE) records and
provides a small set of helpers for portable approximate orbit
propagation and ground-track generation.

The current version defaults to an approximate automatic propagation
mode that dispatches between dedicated near-earth and deep-space
variants. The near-earth variant applies low-order B\* drag damping
together with J2 secular and short-period position corrections. The
deep-space variant uses its own long-period and resonance-aware
corrections so high-period orbits no longer share the near-earth path.
The two-body Keplerian approximation is also available as an explicit
baseline model for comparison and simple fallback use. Propagated states
can also be queried together with direct velocity outputs in inertial,
Earth-fixed, or local ENU frames.

API documentation
-----------------

Open the
`../../apis/library_index.html#tle_orbits <../../apis/library_index.html#tle_orbits>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(tle_orbits(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(tle_orbits(tester)).

Representation
--------------

Parsed TLE records are represented as:

::

   ```
       tle(
           Name,
           satellite(CatalogNumber, Classification, designator(LaunchYear, LaunchNumber, Piece)),
           epoch_julian_date(EpochJulianDate),
           drag(MeanMotionDot, MeanMotionDdot, BStar),
           orbit(InclinationDegrees, RightAscensionDegrees, Eccentricity,
               ArgumentOfPerigeeDegrees, MeanAnomalyDegrees, MeanMotionRevolutionsPerDay),
           ephemeris_type(EphemerisType),
           element_set(ElementSetNumber),
           revolution(RevolutionNumber)
       )
   ```

If the optional title line is absent, ``Name`` is the atom ``none``.

Public API
----------

- ``parse/2`` parses one or more TLE records from ``atom(Atom)``,
  ``chars(List)``, ``codes(List)``, ``stream(Stream)``, or
  ``file(Path)`` sources.
- ``parse_lines/4`` parses a single TLE record from an optional name and
  the two fixed-width element lines.
- ``propagate/3`` propagates to a default
  ``geographic(Latitude,Longitude,Height)`` coordinate in WGS84 using
  the default ``approximate`` model.
- ``propagate/4`` supports the frames ``eci``, ``ecef``, and
  ``wgs84_3d`` using the default ``approximate`` model.
- ``propagate/5`` additionally allows choosing one of the
  ``approximate``, ``approximate_near_earth``,
  ``approximate_deep_space``, or ``two_body`` models.
- ``propagate_state/4`` returns ``state(Position,Velocity)`` in a
  requested frame using the default ``approximate`` model.
- ``propagate_state/5`` additionally allows choosing one of the
  ``approximate``, ``approximate_near_earth``,
  ``approximate_deep_space``, or ``two_body`` models. Velocity is
  derived directly from the propagated orbital elements in ECI and then
  analytically transformed to ECEF or local ENU when requested.
- ``ground_track/5`` samples
  ``sample(DateTime, geographic(Latitude,Longitude,Height))`` points
  over a time range using a fixed step in seconds and the default
  ``approximate`` model.
- ``ground_track/6`` additionally allows choosing one of the
  ``approximate``, ``approximate_near_earth``,
  ``approximate_deep_space``, or ``two_body`` models.

Supported propagation time specifications are:

- ``date_time(Year, Month, Day, Hours, Minutes, Seconds)``
- ``julian_date(JulianDate)``
- ``offset_seconds(SecondsSinceEpoch)``

Examples
--------

Parse one named TLE record:

::

   | ?- tle_orbits::parse(atom('ISS (ZARYA)\n1 25544U 98067A   24120.51782528  .00012051  00000-0  21940-3 0  9995\n2 25544  51.6393 184.4452 0003580  32.9443 327.1663 15.50957687452123\n'), TLEs).

Propagate to a WGS84 3D geodetic coordinate using the TLE epoch:

::

   | ?- TLEs = [TLE], tle_orbits::propagate(TLE, offset_seconds(0.0), Coordinate).

Propagate to ECI or ECEF coordinates:

::

   | ?- tle_orbits::propagate(TLE, offset_seconds(600.0), eci, ECI).
   | ?- tle_orbits::propagate(TLE, offset_seconds(600.0), ecef, ECEF).

Select the propagation model explicitly:

::

   | ?- tle_orbits::propagate(TLE, offset_seconds(600.0), eci, two_body, ECI).
   | ?- tle_orbits::propagate(TLE, offset_seconds(600.0), eci, approximate_near_earth, ECI).
   | ?- tle_orbits::propagate(TLE, offset_seconds(600.0), eci, approximate, ECI).
   | ?- tle_orbits::propagate(TLE, offset_seconds(43200.0), eci, approximate_deep_space, ECI).

Request propagated state plus velocity:

::

   | ?- tle_orbits::propagate_state(TLE, offset_seconds(600.0), eci, State).
   | ?- tle_orbits::propagate_state(TLE, offset_seconds(600.0), wgs84_3d, approximate, State).

Generate a ground track sampled every 10 minutes:

::

   | ?- tle_orbits::ground_track(
       TLE,
       date_time(2024, 4, 29, 12, 25, 40.0),
       date_time(2024, 4, 29, 13, 25, 40.0),
       600.0,
       Samples
   ).

Notes
-----

- Parsing validates the TLE checksums and rejects malformed records.
- The ``approximate`` family includes low-order B\* drag handling,
  near-earth short-period corrections, and a separate deep-space
  approximation with independent solar, lunar, and resonance-aware
  perturbation terms. It is a portable approximation, not an
  implementation of standards-grade SGP4/SDP4.
- State velocity output is no longer estimated using centered finite
  differences. The library now computes ECI velocity directly from the
  propagated orbital elements and transforms that velocity analytically
  to ECEF and local ENU.
- Position and velocity frame conversion now share the same
  Earth-rotation basis to keep propagated state components internally
  consistent across frames.
- The geodetic output path reuses the ``crs_projections`` library for
  converting propagated ``ecef(X,Y,Z)`` coordinates to ``geographic/3``
  coordinates.
