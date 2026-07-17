.. _library_ccsds_time_fields:

``ccsds_time_fields``
=====================

The ``ccsds_time_fields`` library adds the self-describing time-field
layer that is missing between raw embedded time bytes and the
format-specific objects in ``ccsds_time_codes``.

This library covers CCSDS binary P-fields for:

- CUC time fields using 1 to 7 coarse octets and 0 to 10 fine octets,
  including the two-octet extended P-field encoding
- CDS time fields using 16-bit or 24-bit day segments and either no
  submillisecond segment, a 16-bit microsecond segment, or a 32-bit
  picosecond segment
- CCS time fields using either calendar or day-of-year encoding and up
  to 6 BCD fraction octets

API documentation
-----------------

Open the
`../../apis/library_index.html#ccsds_time_fields <../../apis/library_index.html#ccsds_time_fields>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(ccsds_time_fields(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(ccsds_time_fields(tester)).

Representation
--------------

Descriptors are represented using the compound terms:

::

   cuc_descriptor(CoarseOctets, FineOctets, Epoch)
   cds_descriptor(DaySegmentOctets, SubmillisecondOctets, Epoch)
   ccs_descriptor(CalendarVariant, FractionOctets)

Where:

- ``Epoch`` is either ``ccsds_epoch`` or ``unix_epoch``
- ``CalendarVariant`` is either ``calendar`` or ``day_of_year``

This library uses the existing ``ccsds_time_codes`` objects to parse and
generate the T-field bytes once the descriptor has been decoded.

For the current scope, the agency-defined epoch bit in CUC and CDS
descriptors is mapped to the existing ``unix_epoch`` atom so callers can
keep using the same time-code objects and terms already provided by
``ccsds_time_codes``.

Parsing and generating
----------------------

To parse a self-describing CUC time field:

::

   | ?- ccsds_time_fields::parse(bytes([0x1E, 0x00, 0x00, 0x01, 0x00, 0x00, 0x80]), Descriptor, TimeCode).

To parse a self-describing extended CUC time field:

::

   | ?- ccsds_time_fields::parse(bytes([0x9F, 0x28, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80]), Descriptor, TimeCode).

To generate the same time field from a descriptor and a time-code term:

::

   | ?- ccsds_time_fields::generate(bytes(Bytes), cuc_descriptor(4, 2, ccsds_epoch), cuc_time(256, 128)).

To parse a self-describing CDS time field:

::

   | ?- ccsds_time_fields::parse(bytes([0x40, 0x00, 0x01, 0x00, 0x00, 0x07, 0xD0]), Descriptor, TimeCode).

To parse a self-describing CDS time field with a 32-bit submillisecond
segment:

::

   | ?- ccsds_time_fields::parse(bytes([0x4E, 0x00, 0x00, 0x01, 0x00, 0x00, 0x07, 0xD0, 0x1D, 0xCD, 0x65, 0x00]), Descriptor, TimeCode).

To generate a self-describing CCS day-of-year time field:

::

   | ?- ccsds_time_fields::generate(bytes(Bytes), ccs_descriptor(day_of_year, 1), ccs_ordinal_time(2026, 128, 14, 30, 45, 67)).

Descriptor helpers
------------------

The library also provides helper predicates for descriptor
introspection:

- ``valid_descriptor/1``
- ``format/2``
- ``epoch/2``

For example:

::

   | ?- ccsds_time_fields::format(cds_descriptor(3, 4, unix_epoch), Format).

   | ?- ccsds_time_fields::epoch(ccs_descriptor(calendar, 1), Epoch).
