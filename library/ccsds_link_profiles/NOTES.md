________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


`ccsds_link_profiles`
=====================

The `ccsds_link_profiles` library provides an ergonomic wrapper layer over the
existing parametric CCSDS frame objects.

Instead of selecting one of the raw `ccsds_tm_frames(...)`,
`ccsds_tc_frames(...)`, or `ccsds_aos_frames(...)` objects directly at each call
site, callers can work with explicit profile terms and generic wrapper
predicates.

Representation
--------------

Link profiles are represented using the compound terms:

    tm_profile(FrameLength, SecondaryHeaderLength, HasFECF)
    tc_profile(FrameLength, SegmentHeaderLength, HasFECF)
    aos_profile(FrameLength, InsertZoneLength, HasOCF, HasFECF)

Where:

- `FrameLength` is the fixed frame length in octets
- `SecondaryHeaderLength`, `SegmentHeaderLength`, and `InsertZoneLength` are
  the mission-profile field lengths in octets
- `HasOCF` and `HasFECF` are the atoms `true` or `false`

Public API
----------

The current implementation provides the predicates:

- `valid_profile/1`
- `parse_frame/3`
- `parse_frames/3`
- `generate_frame/3`
- `generate_frames/3`
- `valid_reassembly_state/1`
- `initial_reassembly_state/1`
- `pending_fragments/2`
- `valid_discontinuity_policy/1`
- `extract_packets/4`
- `insert_packets/5`
- `reassemble_packets/6-8`
- `reassemble_frames/6-8`

Parsing and generating
----------------------

To parse exactly one telemetry transfer frame using a profile term:

    | ?- ccsds_link_profiles::parse_frame(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 1, 2, 3, 4, 5, 6, 0xDE, 0xAD, 0xBE, 0xEF]), tm_profile(16, 0, false), Frame).

To generate a telecommand transfer frame using a profile term:

    | ?- ccsds_link_profiles::generate_frame(bytes(Bytes), tc_profile(10, 0, true), tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D]))).

`parse_frame/3` expects the source to contain exactly one frame. If the source
contains zero frames or more than one frame, the predicate throws a
`domain_error(ccsds_single_frame_source, Source)` exception.

Parsing and generation inherit FECF verification and regeneration from the
underlying TM, TC, and AOS frame objects selected by the profile.

To parse or generate multi-frame sources and sinks, use `parse_frames/3` and
`generate_frames/3`.

Packet helpers
--------------

Packet extraction and insertion are provided for TM and AOS profiles using the
high-level packet-zone semantics from `ccsds_packet_services`:

    | ?- ccsds_link_profiles::extract_packets(tm_profile(18, 0, false), 0, tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 2, none, [0xAA,0xBB,0x08,0x01,0xC0,0x00,0x00,0x03,0xDE,0xAD,0xBE,0xEF], none, none), PacketZone).

    | ?- ccsds_link_profiles::insert_packets(aos_profile(20, 0, false, false), 0, packet_zone([0xAA,0xBB], [ccsds_packet(0, 0, 1, 1, 3, 0, none, [0xDE,0xAD,0xBE,0xEF])], []), aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [0,0,0,0,0,0,0,0,0,0,0,0,0,0], none, none), UpdatedFrame).

For cross-frame TM and AOS packet reconstruction, initialize a reassembly state
and call the generic profile wrapper predicates:

    | ?- ccsds_link_profiles::initial_reassembly_state(State), ccsds_link_profiles::reassemble_frames(tm_profile(16, 0, false), 0, Frames, resynchronize, State, Packets, UpdatedState).

Telecommand profiles are intentionally rejected by `extract_packets/4` and
`insert_packets/5`, `reassemble_packets/6-8`, and `reassemble_frames/6-8`,
which throw a
`domain_error(ccsds_packet_link_profile, tc_profile(...))` exception.

API documentation
-----------------

Open the [../../apis/library_index.html#ccsds_link_profiles](../../apis/library_index.html#ccsds_link_profiles)
link in a web browser.

Loading
-------

To load all entities in this library, load the `loader.lgt` file:

    | ?- logtalk_load(ccsds_link_profiles(loader)).

Testing
-------

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(ccsds_link_profiles(tester)).
