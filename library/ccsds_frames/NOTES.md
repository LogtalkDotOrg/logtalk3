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


`ccsds_frames`
===============

The `ccsds_frames` library provides support for CCSDS transfer frames.
The current implementation includes CCSDS telemetry transfer frames and CCSDS
telecommand transfer frames, plus CCSDS advanced orbiting systems transfer
frames, all using fixed frame lengths and optional mission-profile fields
configured at the object level.


Available entities
------------------

- `ccsds_frame_protocol`
  Common protocol for transfer frame format objects.

- `ccsds_frames`
  Facade object for introspecting frame terms and bridging raw frame data fields with CCSDS space packets.

- `ccsds_tm_frames(FrameLength, SecondaryHeaderLength, HasFECF)`
  Parametric object implementing parsing, generation, and accessors for CCSDS
  telemetry transfer frames.

- `ccsds_tc_frames(FrameLength, SegmentHeaderLength, HasFECF)`
  Parametric object implementing parsing, generation, and accessors for CCSDS
  telecommand transfer frames.

- `ccsds_aos_frames(FrameLength, InsertZoneLength, HasOCF, HasFECF)`
  Parametric object implementing parsing, generation, and accessors for CCSDS
  advanced orbiting systems transfer frames.

- `ccsds_frames_types`
  Type definitions for TM, TC, and AOS transfer frame byte encodings and terms.


Representation
--------------

Telemetry transfer frames are represented using the compound term:

    tm_transfer_frame(
        Version,
        SpacecraftId,
        VirtualChannelId,
        OCFFlag,
        MasterChannelFrameCount,
        VirtualChannelFrameCount,
        SecondaryHeaderFlag,
        SynchronizationFlag,
        PacketOrderFlag,
        SegmentLengthIdentifier,
        FirstHeaderPointer,
        SecondaryHeader,
        DataField,
        OCF,
        FECF
    )

Where:

- `SecondaryHeader` is either `none` or `secondary_header(Bytes)`
- `OCF` is either `none` or `ocf(Bytes)`
- `FECF` is either `none` or `fecf(Bytes)`

Telecommand transfer frames are represented using the compound term:

  tc_transfer_frame(
    Version,
    BypassFlag,
    ControlCommandFlag,
    SpacecraftId,
    VirtualChannelId,
    SequenceNumber,
    SegmentHeader,
    DataField,
    FECF
  )

Where:

- `SegmentHeader` is either `none` or `segment_header(Bytes)`
- `FECF` is either `none` or `fecf(Bytes)`

Advanced orbiting systems transfer frames are represented using the compound
term:

  aos_transfer_frame(
    Version,
    SpacecraftId,
    VirtualChannelId,
    VirtualChannelFrameCount,
    SignalingField,
    InsertZone,
    DataField,
    OCF,
    FECF
  )

Where:

- `SignalingField` is `signaling_field(ReplayFlag, FrameCountUsageFlag, SpacecraftIdExtension, FrameCountCycle)`
- `InsertZone` is either `none` or `insert_zone(Bytes)`
- `OCF` is either `none` or `ocf(Bytes)`
- `FECF` is either `none` or `fecf(Bytes)`


Parsing and generating
----------------------

To parse a fixed 16-octet telemetry transfer frame without a telemetry transfer
frame secondary header and without a frame error control field:

    | ?- ccsds_tm_frames(16, 0, false)::parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 1, 2, 3, 4, 5, 6, 0xDE, 0xAD, 0xBE, 0xEF]), Frames).

To generate the original byte sequence from the parsed term:

    | ?- ccsds_tm_frames(16, 0, false)::generate(bytes(Bytes), Frames).

To parse a fixed 10-octet telecommand transfer frame without a segment header:

  | ?- ccsds_tc_frames(10, 0, true)::parse(bytes([0x20, 0x2A, 0x0C, 0x09, 0x07, 1, 2, 3, 0x44, 0x6D]), Frames).

To generate the original byte sequence from the parsed term:

  | ?- ccsds_tc_frames(10, 0, true)::generate(bytes(Bytes), Frames).

When FECF support is enabled for a frame object, parsing verifies the incoming
FECF, `valid/1` checks that it matches the remaining frame fields, and
`generate/2-3` recomputes it from the remaining frame content. The parametric
frame objects also provide `update_fecf/2` and `verify_fecf/1` for explicit
integrity handling:

  | ?- ccsds_tc_frames(10, 0, true)::update_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], none), Frame).

  | ?- ccsds_tc_frames(10, 0, true)::verify_fecf(Frame).

To parse a fixed 12-octet AOS transfer frame without an insert zone, OCF, or
FECF:

  | ?- ccsds_aos_frames(12, 0, false, false)::parse(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 1, 2, 3, 4, 5, 6]), Frames).

To generate the original byte sequence from the parsed term:

  | ?- ccsds_aos_frames(12, 0, false, false)::generate(bytes(Bytes), Frames).


Types
-----

The `ccsds_frames_types` category defines the following types:

- `ccsds_tm_frame(FrameLength, SecondaryHeaderLength, HasFECF)` for byte encodings
- `ccsds_tm_frame_term(FrameLength, SecondaryHeaderLength, HasFECF)` for frame terms
- `ccsds_tc_frame(FrameLength, SegmentHeaderLength, HasFECF)` for byte encodings
- `ccsds_tc_frame_term(FrameLength, SegmentHeaderLength, HasFECF)` for frame terms
- `ccsds_aos_frame(FrameLength, InsertZoneLength, HasOCF, HasFECF)` for byte encodings
- `ccsds_aos_frame_term(FrameLength, InsertZoneLength, HasOCF, HasFECF)` for frame terms

For example:

    | ?- type::check(ccsds_tm_frame(16, 0, false), [0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 1, 2, 3, 4, 5, 6, 0xDE, 0xAD, 0xBE, 0xEF]).

    | ?- type::check(ccsds_tm_frame_term(16, 0, false), tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none)).

    | ?- type::check(ccsds_tc_frame(10, 0, true), [0x20, 0x2A, 0x0C, 0x09, 0x07, 1, 2, 3, 0x44, 0x6D]).

    | ?- type::check(ccsds_tc_frame_term(10, 0, true), tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D]))).

    | ?- type::check(ccsds_aos_frame(12, 0, false, false), [0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 1, 2, 3, 4, 5, 6]).

    | ?- type::check(ccsds_aos_frame_term(12, 0, false, false), aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [1,2,3,4,5,6], none, none)).


Facade helpers
--------------

The `ccsds_frames` facade object provides generic accessors that work across TM,
TC, and AOS frame terms:

- `frame_type/2`
- `version/2`
- `spacecraft_id/2`
- `virtual_channel_id/2`
- `data_field/2`
- `ocf/2`
- `fecf/2`
- `update_fecf/2`
- `verify_fecf/1`

It also provides raw payload bridge helpers for CCSDS space packets:

- `extract_packets(Frame, SecondaryHeaderLength, Packets)` parses the frame data
  field bytes using `ccsds_packets(SecondaryHeaderLength)`.
- `insert_packets(Packets, SecondaryHeaderLength, Frame, UpdatedFrame)`
  generates packet bytes using `ccsds_packets(SecondaryHeaderLength)` and replaces the
  frame data field while leaving the remaining frame fields unchanged.

For example:

    | ?- ccsds_frames::extract_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x08,0x01,0xC0,0x00,0x00,0x03,0xDE,0xAD,0xBE,0xEF], none, none), 0, Packets).

    | ?- ccsds_frames::insert_packets([ccsds_packet(0, 0, 1, 1, 3, 0, none, [0xDE,0xAD,0xBE,0xEF])], 0, tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [], none, none), UpdatedFrame).

  | ?- ccsds_frames::update_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x00,0x00])), UpdatedFrame).

  | ?- ccsds_frames::verify_fecf(UpdatedFrame).


API documentation
-----------------

Open the [../../apis/library_index.html#ccsds_frames](../../apis/library_index.html#ccsds_frames)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

    | ?- logtalk_load(ccsds_frames(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(ccsds_frames(tester)).
