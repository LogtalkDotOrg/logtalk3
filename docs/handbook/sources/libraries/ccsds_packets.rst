.. _library_ccsds_packets:

``ccsds_packets``
=================

The ``ccsds_packets`` library implements predicates for parsing and
generating CCSDS (Consultative Committee for Space Data Systems) Space
Packets following the CCSDS 133.0-B-2 standard (Space Packet Protocol):

- https://public.ccsds.org/Pubs/133x0b2e1.pdf
- https://jastoolbox.sandia.gov/topic/communication-specification/jas-packets/ccsds-telecommand-and-telemetry-format-packet-standard/

API documentation
-----------------

Open the
`../../apis/library_index.html#ccsds_packets <../../apis/library_index.html#ccsds_packets>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(ccsds_packets(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(ccsds_packets(tester)).

To test the performance of the library parsing predicates, load the
``tester_performance.lgt`` file:

::

   | ?- logtalk_load(ccsds_packets(tester_performance)).

Packet Structure
----------------

A CCSDS Space Packet consists of:

1. **Primary Header (6 bytes)**:

   - Version Number (3 bits) - Always 000 for Space Packets
   - Packet Type (1 bit) - 0=Telemetry, 1=Telecommand
   - Secondary Header Flag (1 bit) - 0=absent, 1=present
   - Application Process ID (APID) (11 bits) - 0-2047
   - Sequence Flags (2 bits) - 00=continuation, 01=first, 10=last,
     11=standalone
   - Packet Sequence Count (14 bits) - 0-16383
   - Packet Data Length (16 bits) - Number of octets in data field minus
     1

2. **User Data Field** - Variable length payload

Representation
--------------

Packets are represented using the compound term:

::

   ccsds_packet(Version, Type, SecHeaderFlag, APID, SeqFlags, SeqCount, SecHeader, UserData)

Where:

- ``Version`` is an integer (0-7, typically 0)
- ``Type`` is an integer (0=telemetry, 1=telecommand)
- ``SecHeaderFlag`` is an integer (0 or 1)
- ``APID`` is an integer (0-2047)
- ``SeqFlags`` is an integer (0-3)
- ``SeqCount`` is an integer (0-16383)
- ``SecHeader`` is either ``none`` or ``secondary_header(Bytes)`` where
  ``Bytes`` is a list of bytes
- ``UserData`` is a list of bytes

Note that the ``DataLength`` field from the wire format is not stored in
the term representation as it can be computed from ``SecHeader`` and
``UserData``. The ``data_length/2`` accessor predicate computes and
returns this value when needed.

Parsing
-------

The ``parse/2`` predicate accepts a source term as its first argument.
The source can be ``file(File)``, ``stream(Stream)``, or
``bytes(Bytes)``. All source types return a list of packets for
uniformity.

To parse packets from a list of bytes:

::

   | ?- ccsds_packets::parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF]), Packets).
   Packets = [ccsds_packet(0, 0, 1, 1, 3, 0, none, [222, 173, 190, 239])]
   yes

To parse packets from a binary file:

::

   | ?- ccsds_packets::parse(file('telemetry.bin'), Packets).

To parse packets from a binary stream:

::

   | ?- ccsds_packets::parse(stream(Stream), Packets).

When the packets include a secondary header, the secondary header length
must be known. In this case, use the
``ccsds_packets(SecondaryHeaderLength)`` object instead of the
``ccsds_packets`` object. For example, to parse packets with a secondary
header of 6 bytes:

::

   | ?- ccsds_packets(6)::parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x07, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xAA, 0xBB]), Packets).

Generating
----------

The ``generate/2`` predicate accepts a sink term as its first argument
and a list of packet terms as the second argument. The sink can be
``file(File)``, ``stream(Stream)``, or ``bytes(Bytes)``.

To generate bytes from a list of packet terms:

::

   | ?- ccsds_packets::generate(bytes(Bytes), [ccsds_packet(0, 0, 1, 1, 3, 0, none, [0xDE, 0xAD, 0xBE, 0xEF])]).
   Bytes = [8, 1, 192, 0, 0, 3, 222, 173, 190, 239]
   yes

To write the bytes generated from a list of packet terms to a binary
file:

::

   | ?- ccsds_packets::generate(file('output.bin'), Packets).

To write the bytes generated from a list of packet terms to a binary
stream:

::

   | ?- ccsds_packets::generate(stream(Stream), Packets).

Accessor Predicates
-------------------

The library provides convenient accessor predicates for extracting
packet fields:

::

   | ?- ccsds_packets::apid(Packet, APID).

   % Returns telemetry or telecommand
   | ?- ccsds_packets::type(Packet, Type).

   % Returns continuation, first, last, or standalone
   | ?- ccsds_packets::sequence_flags(Packet, Flags).

   % Returns the secondary header as a list of bytes or none
   | ?- ccsds_packets::secondary_header(Packet, SecHeader).

   % Parses a self-describing secondary-header time field when Descriptor is a variable.
   | ?- ccsds_packets(7)::secondary_header_time(Packet, Descriptor, Time).

   % Parses raw secondary-header T-field bytes using an explicit descriptor term.
   | ?- ccsds_packets(10)::secondary_header_time(Packet, cuc_descriptor(5, 5, ccsds_epoch), Time).

   | ?- ccsds_packets::user_data(Packet, Data).

   % Returns the packet data length (computed from secondary header and user data)
   | ?- ccsds_packets::data_length(Packet, Length).

Types and arbitrary generators
------------------------------

The library includes a ``ccsds_packets_types`` category that provides
``ccsds_packet`` and ``ccsds_packet(SecondaryHeaderLength)`` types and
arbitrary generators for CCSDS packets. For example:

::

   | ?- type::check(ccsds_packet, Bytes).

   | ?- type::arbitrary(ccsds_packet(42), Bytes).

It also provides a ``ccsds_packets(N)`` and
``ccsds_packets(SecondaryHeaderLength, N)`` types for generating a list
with ``N`` packets. For example:

::

   | ?- type::arbitrary(ccsds_packets(10), Bytes).

   | ?- type::check(ccsds_packets(42, 10), Bytes).

For a term representation of a packet, use the ``ccsds_packet_term`` and
``ccsds_packet_term(SecondaryHeaderLength)`` types and arbitrary
generators. For example:

::

   | ?- type::check(ccsds_packet_term, Packet).

   | ?- type::arbitrary(ccsds_packet_term(42), Packet).
