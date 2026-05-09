.. _library_ccsds_time_codes:

``ccsds_time_codes``
====================

The ``ccsds_time_codes`` library provides support for CCSDS time codes.
The current implementation includes CCSDS Unsegmented Time Code (CUC)
and CCSDS Day Segmented Time Code (CDS), and CCSDS Calendar Segmented
Time Code (CCS), keeping the package small and reusable by the existing
``ccsds_packets`` packet library and by future frame-level libraries.

Available entities
------------------

- ``ccsds_time_code_protocol`` Common protocol for time code format
  objects.

- ``ccsds_time_codes`` Facade object for introspecting time code terms.

- ``ccsds_cuc(CoarseOctets, FineOctets, Epoch)`` Parametric object
  implementing parsing, generation, and conversion for CUC time codes.

- ``ccsds_cds(DaySegmentOctets, SubmillisecondOctets, Epoch)``
  Parametric object implementing parsing, generation, and conversion for
  CDS time codes.

- ``ccsds_ccs(CalendarVariant, FractionOctets)`` Parametric object
  implementing parsing, generation, and conversion for CCS time codes.

- ``ccsds_time_codes_types`` Type definitions and arbitrary generators
  for CUC terms and byte encodings.

Representation
--------------

CUC values are represented using the compound term:

::

   cuc_time(Coarse, Fine)

Where:

- ``Coarse`` is the integer value stored in the coarse time octets
- ``Fine`` is the integer value stored in the fine time octets

The number of octets used for each field is selected by the parametric
object.

CDS values are represented using the compound terms:

::

   cds_time(Days, Milliseconds)
   cds_time(Days, Milliseconds, Submilliseconds)

Where:

- ``Days`` is the integer value stored in the day segment
- ``Milliseconds`` is the millisecond-of-day value
- ``Submilliseconds`` is the optional submillisecond value when the
  object uses a submillisecond segment; 2-octet objects store
  microseconds and 4-octet objects store picoseconds within the
  millisecond

CCS values are represented using the compound terms:

::

       ccs_calendar_time(Year, Month, Day, Hour, Minute, Second, Fraction)
       ccs_ordinal_time(Year, DayOfYear, Hour, Minute, Second, Fraction)

Where:

- ``Year`` is the four-digit BCD year
- ``Month`` and ``Day`` are used by the ``calendar`` variant
- ``DayOfYear`` is used by the ``day_of_year`` variant
- ``Fraction`` stores the optional BCD fractional digits encoded by the
  object

Parsing and generating
----------------------

To parse a 4-octet coarse and 2-octet fine CUC value using the CCSDS
epoch:

::

   | ?- ccsds_cuc(4, 2, ccsds_epoch)::parse(bytes([0x00, 0x00, 0x01, 0x00, 0x00, 0x80]), TimeCode).
   TimeCode = cuc_time(256, 128)
   yes

To generate the corresponding byte sequence:

::

   | ?- ccsds_cuc(4, 2, ccsds_epoch)::generate(bytes(Bytes), cuc_time(256, 128)).
   Bytes = [0, 0, 1, 0, 0, 128]
   yes

To parse a 16-bit day-segment CDS value without the optional
submillisecond segment:

::

   | ?- ccsds_cds(2, 0, ccsds_epoch)::parse(bytes([0x00, 0x01, 0x00, 0x00, 0x07, 0xD0]), TimeCode).
   TimeCode = cds_time(1, 2000)
   yes

To parse a CDS value with a 16-bit submillisecond segment:

::

   | ?- ccsds_cds(2, 2, unix_epoch)::parse(bytes([0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0x01, 0xF4]), TimeCode).
   TimeCode = cds_time(1, 2000, 500)
   yes

To parse a CDS value with a 32-bit submillisecond segment:

::

   | ?- ccsds_cds(3, 4, unix_epoch)::parse(bytes([0x00, 0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0x1D, 0xCD, 0x65, 0x00]), TimeCode).
   TimeCode = cds_time(1, 2000, 500000000)
   yes

To parse a CCS calendar-segmented value without fractional digits:

::

   | ?- ccsds_ccs(calendar, 0)::parse(bytes([0x20, 0x26, 0x05, 0x08, 0x14, 0x30, 0x45]), TimeCode).
   TimeCode = ccs_calendar_time(2026, 5, 8, 14, 30, 45, 0)
   yes

To parse an ordinal CCS value with one fractional BCD octet:

::

   | ?- ccsds_ccs(day_of_year, 1)::parse(bytes([0x20, 0x26, 0x01, 0x28, 0x14, 0x30, 0x45, 0x67]), TimeCode).
   TimeCode = ccs_ordinal_time(2026, 128, 14, 30, 45, 67)
   yes

Unix time conversion
--------------------

To convert a CUC value that uses the Unix epoch into Unix seconds:

::

   | ?- ccsds_cuc(4, 2, unix_epoch)::unix_seconds(cuc_time(1, 32768), Seconds).
   Seconds = 1.5
   yes

To convert Unix seconds back into a CUC value:

::

   | ?- ccsds_cuc(4, 2, unix_epoch)::from_unix_seconds(1.5, TimeCode).
   TimeCode = cuc_time(1, 32768)
   yes

To convert a CDS value that uses the Unix epoch into Unix seconds:

::

   | ?- ccsds_cds(2, 2, unix_epoch)::unix_seconds(cds_time(1, 2000, 500), Seconds).
   Seconds = 86402.0005
   yes

To convert Unix seconds back into a CDS value:

::

   | ?- ccsds_cds(2, 2, unix_epoch)::from_unix_seconds(86402.0005, TimeCode).
   TimeCode = cds_time(1, 2000, 500)
   yes

To convert a CCS calendar value into Unix seconds:

::

   | ?- ccsds_ccs(calendar, 0)::unix_seconds(ccs_calendar_time(1970, 1, 1, 0, 0, 0, 0), Seconds).
   Seconds = 0
   yes

To convert Unix seconds back into an ordinal CCS value:

::

   | ?- ccsds_ccs(day_of_year, 1)::from_unix_seconds(1.5, TimeCode).
   TimeCode = ccs_ordinal_time(1970, 1, 0, 0, 1, 50)
   yes

Types and arbitrary generators
------------------------------

The ``ccsds_time_codes_types`` category defines the following types:

- ``ccsds_cuc(CoarseOctets, FineOctets, Epoch)`` for byte encodings
- ``ccsds_cuc_time(CoarseOctets, FineOctets)`` for ``cuc_time/2`` terms
- ``ccsds_cds(DaySegmentOctets, SubmillisecondOctets, Epoch)`` for byte
  encodings
- ``ccsds_cds_time(DaySegmentOctets, SubmillisecondOctets)`` for CDS
  terms
- ``ccsds_ccs(CalendarVariant, FractionOctets)`` for byte encodings
- ``ccsds_ccs_time(CalendarVariant, FractionOctets)`` for CCS terms

For example:

::

   | ?- type::check(ccsds_cuc(4, 2, ccsds_epoch), [0, 0, 1, 0, 0, 128]).

   | ?- type::check(ccsds_cuc_time(4, 2), cuc_time(256, 128)).

   | ?- type::check(ccsds_cds(2, 0, ccsds_epoch), [0, 1, 0, 0, 7, 208]).

   | ?- type::check(ccsds_cds_time(2, 2), cds_time(1, 2000, 500)).

   | ?- type::check(ccsds_ccs(calendar, 0), [0x20, 0x26, 0x05, 0x08, 0x14, 0x30, 0x45]).

   | ?- type::check(ccsds_ccs_time(day_of_year, 1), ccs_ordinal_time(2026, 128, 14, 30, 45, 67)).

API documentation
-----------------

Open the
`../../apis/library_index.html#ccsds_time_codes <../../apis/library_index.html#ccsds_time_codes>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(ccsds_time_codes(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(ccsds_time_codes(tester)).
