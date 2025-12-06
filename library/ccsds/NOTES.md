________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 2025 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`ccsds`
=======

The `ccsds` library implements predicates for parsing and generating CCSDS
(Consultative Committee for Space Data Systems) Space Packets following the
CCSDS 133.0-B-2 standard (Space Packet Protocol).

Reference documentation:

- https://public.ccsds.org/Pubs/133x0b2e1.pdf
- https://jastoolbox.sandia.gov/topic/communication-specification/jas-packets/ccsds-telecommand-and-telemetry-format-packet-standard/


Packet Structure
----------------

A CCSDS Space Packet consists of:

1. **Primary Header (6 bytes)**:
   - Version Number (3 bits) - Always 000 for Space Packets
   - Packet Type (1 bit) - 0=Telemetry, 1=Telecommand
   - Secondary Header Flag (1 bit) - 0=absent, 1=present
   - Application Process ID (APID) (11 bits) - 0-2047
   - Sequence Flags (2 bits) - 00=continuation, 01=first, 10=last, 11=standalone
   - Packet Sequence Count (14 bits) - 0-16383
   - Packet Data Length (16 bits) - Number of octets in data field minus 1

2. **User Data Field** - Variable length payload


Representation
--------------

Packets are represented using the compound term:

    ccsds_packet(Version, Type, SecHeaderFlag, APID, SeqFlags, SeqCount, DataLength, UserData)

Where:

- `Version` is an integer (0-7, typically 0)
- `Type` is an integer (0=telemetry, 1=telecommand)
- `SecHeaderFlag` is an integer (0 or 1)
- `APID` is an integer (0-2047)
- `SeqFlags` is an integer (0-3)
- `SeqCount` is an integer (0-16383)
- `DataLength` is an integer (0-65535)
- `UserData` is a list of bytes


Parsing
-------

To parse a single packet from a list of bytes:

    | ?- ccsds::parse([0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF], Packet).
    Packet = ccsds_packet(0, 0, 1, 1, 3, 0, 3, [222, 173, 190, 239])
    yes

To parse multiple packets from a byte stream:

    | ?- ccsds::parse_all(Bytes, Packets).

To parse packets from a binary file:

    | ?- ccsds::parse_file('telemetry.bin', Packets).


Generating
----------

To generate bytes from a packet term:

    | ?- ccsds::generate(ccsds_packet(0, 0, 1, 1, 3, 0, 3, [0xDE, 0xAD, 0xBE, 0xEF]), Bytes).
    Bytes = [8, 1, 192, 0, 0, 3, 222, 173, 190, 239]
    yes


Accessor Predicates
-------------------

The library provides accessor predicates for extracting packet fields:

    | ?- ccsds::parse(Bytes, Packet), ccsds::apid(Packet, APID).
    | ?- ccsds::type(Packet, Type).           % Returns telemetry or telecommand
    | ?- ccsds::sequence_flags(Packet, Flags). % Returns continuation, first, last, or standalone
    | ?- ccsds::user_data(Packet, Data).


Types and arbitrary generators
------------------------------

The library includes a `ccsds_types` category that provides `ccsds_packet` and
`ccsds_packet(SecondaryHeaderLength)` types and arbitrary generators for CCSDS
packets. For example:

    | ?- type::check(ccsds_packet, Bytes).
    | ?- type::arbitrary(ccsds_packet(42), Bytes).

It also provides a `ccsds_packets(N)` and `ccsds_packets(SecondaryHeaderLength, N)`
types for generating a list with `N` packets. For example:

    | ?- type::arbitrary(ccsds_packets(10), Bytes).
    | ?- type::arbitrary(ccsds_packets(42, 10), Bytes).


API documentation
-----------------

Open the [../../apis/library_index.html#ccsds](../../apis/library_index.html#ccsds)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

    | ?- logtalk_load(ccsds(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(ccsds(tester)).

To test the performance of the library parsing predicates, load the
`tester_performance.lgt` file:

    | ?- logtalk_load(ccsds(tester_performance)).
