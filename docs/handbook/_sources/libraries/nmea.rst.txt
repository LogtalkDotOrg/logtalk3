.. _library_nmea:

the raw parsed term.

- ``data/2`` maps supported sentence types into typed semantic terms.

Raw Sentence Representation
---------------------------

Parsed sentences are represented as:

::

   nmea_sentence(Talker, Type, Fields, checksum(Provided, Computed))

Where:

- ``Talker`` is a normalized lowercase talker atom such as ``gp`` or
  ``gn``
- proprietary ``$P...`` sentences use the atom ``proprietary``
- ``Type`` is the normalized lowercase sentence type atom
- ``Fields`` is the ordered list of field atoms after the sentence
  identifier
- ``Provided`` is either a normalized uppercase hexadecimal checksum
  atom or ``missing``
- ``Computed`` is the normalized uppercase hexadecimal checksum atom
  computed from the sentence payload

Typed ``data/2`` Results
------------------------

The library currently provides semantic decoding for these sentence
types:

- ``gga(Time, Coordinate, fix(FixQuality, SatellitesUsed, HDOP), altitude(AntennaAltitudeMeters, GeoidSeparationMeters), dgps(DifferentialAgeSeconds, StationId))``
- ``rmc(Time, Status, Coordinate, movement(SpeedKnots, CourseDegrees), Date, MagneticVariation, Mode)``
- ``gsa(SelectionMode, FixType, SatelliteIds, dop(PDOP, HDOP, VDOP), SystemId)``
- ``gsv(MessageCount, MessageNumber, SatellitesInView, Satellites)``
- ``vtg(track(TrueCourseDegrees, MagneticCourseDegrees), speed(SpeedKnots, SpeedKph), Mode)``
- ``gll(Coordinate, Time, Status, Mode)``

Shared semantic subterms include:

- ``geographic(Latitude, Longitude)``
- ``utc_time(Hour, Minute, Second, fraction(Numerator, Denominator))``
- ``date(Year, Month, Day)``
- ``magnetic_variation(Degrees, east|west)``
- ``satellite(PRN, ElevationDegrees, AzimuthDegrees, SnrDbHz)``
- ``missing``

Date Rule
---------

Two-digit ``RMC`` years are expanded using a fixed GPS-era rule:

- ``80..99`` map to ``1980..1999``
- ``00..79`` map to ``2000..2079``

Current Scope
-------------

Implemented in this first version:

- raw parsing from text sources
- configurable checksum handling
- proprietary ``$P...`` sentence passthrough
- semantic decoding for ``GGA``, ``RMC``, ``GSA``, ``GSV``, ``VTG``, and
  ``GLL``

Deliberately out of scope for this first version:

- sentence generation
- AIS ``!`` sentences
- serial or socket transport helpers
- corrupted-stream resynchronization
- ``GSV`` multi-fragment aggregation
- typed proprietary sentence decoding
