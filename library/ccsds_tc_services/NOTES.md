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


`ccsds_tc_services`
===================

The `ccsds_tc_services` library provides service-aware helpers that sit above
raw `ccsds_frames` telecommand transfer frame terms.

This first implementation slice covers:

- extracting telecommand segments from CCSDS telecommand transfer frames
- encoding telecommand segments back into telecommand transfer frames
- tracking segmented telecommand reassembly state per spacecraft identifier and
  virtual channel identifier
- keeping pending segmented data separately per MAP identifier inside each
  virtual channel
- detecting telecommand frame-sequence discontinuities during reassembly
- applying configurable discontinuity recovery policies during reassembly
- returning explicit recovery event streams from the richer frame reassembly
  predicates
- returning provenance-aware reassembled service-unit terms that preserve the
    ordered per-frame mission-specific segment-header suffixes contributing to a
    complete telecommand service unit
- grouping complete telecommand service units into MAP-aware dispatch buckets so
    callers can route them by MAP identifier without rebuilding that bookkeeping

Representation
--------------

Telecommand segments extracted by this library are represented using the
compound term:

        tc_segment(SequenceFlags, MapId, HeaderSuffix, Data)

Where:

- `SequenceFlags` is one of `continuation`, `first`, `last`, or `unsegmented`
- `MapId` is the multiplexer access point identifier in the range 0-63
- `HeaderSuffix` is the list of mission-specific segment-header octets that
    follow the standard first octet
- `Data` is the segment data as a list of bytes

The compact term `tc_segment(SequenceFlags, MapId, Data)` is also accepted by
predicates such as `valid_segment/1` and `insert_tc_segment/3` as shorthand for
an empty header suffix.

Reassembled service units that are reconstructed from multiple transfer frames
use the empty list as `HeaderSuffix`, as there is no single mission-specific
suffix value that unambiguously represents the merged result.

When callers need that provenance instead of the compact compatibility view,
the companion reassembly predicates return terms of the form:

    tc_reassembled_segment(MapId, HeaderSuffixes, Data)

Where `HeaderSuffixes` is the ordered list of the per-frame mission-specific
segment-header suffixes that contributed to the complete service unit.

MAP-aware dispatch helpers group complete telecommand service units into terms
of the form:

    map_dispatch(MapId, ServiceUnits)

Where `ServiceUnits` is a list of complete telecommand service-unit terms for a
single `MapId`, preserving the original service-unit order inside each bucket
and the first-seen order of the MAP buckets.

Telecommand reassembly state is represented using the compound term:

    tc_reassembly_state(Channels)

Where `Channels` is a list of terms of the form:

    tc_reassembly_channel(SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, PendingSegments)

The `PendingSegments` list stores the currently buffered segmented data by MAP
identifier using terms of the form:

    tc_pending_segment(MapId, PendingData)

Pending buffered fragments can be queried using `pending_fragments/2`, which
returns flattened terms of the form:

    pending_fragment(SpacecraftId, VirtualChannelId, MapId, PendingData)

Discontinuity recovery policies are represented by the atoms:

        throw
        drop
        resynchronize

The policies have the following meaning when a frame sequence discontinuity is
found for a telecommand virtual channel:

- `throw`: raise a `domain_error/2` exception
- `drop`: discard all pending MAP fragments for that virtual channel and skip
  the discontinuous frame payload
- `resynchronize`: discard all pending MAP fragments for that virtual channel
  and still process the current frame payload

Recovery event streams are returned by `reassemble_tc_frame/6` and
`reassemble_tc_frames/6` as lists of terms of the form:

    dropped_fragment(SpacecraftId, VirtualChannelId, MapId, Reason, PendingData)
    skipped_frame(SpacecraftId, VirtualChannelId, Discontinuity)
    resynchronized(SpacecraftId, VirtualChannelId, Discontinuity)

Where `Discontinuity` is represented by:

    discontinuity(ExpectedSequenceNumber, SequenceNumber)

Provenance-aware reassembly is provided by the companion predicates
`reassemble_tc_frame_with_provenance/5-6` and
`reassemble_tc_frames_with_provenance/5-6`.

MAP-aware dispatch is provided by `dispatch_service_units_by_map/2-3`, which
accept both compact telecommand segment terms and provenance-aware reassembled
telecommand service-unit terms.

Scope
-----

This first milestone interprets the standard telecommand MAP and sequence
semantics from the first segment-header octet, where the high two bits encode
the sequence flags and the low six bits encode the MAP identifier. Transfer
frames with longer mission-specific segment headers are also supported:
additional segment-header octets are exposed in extracted segment terms as the
`HeaderSuffix` component and are preserved when updating frames.

Examples
--------

To extract a telecommand segment from a telecommand transfer frame term:

    | ?- ccsds_tc_services::extract_tc_segment(tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0xC5]), [1,2,3], none), Segment).

To update a telecommand transfer frame from a telecommand segment term:

    | ?- ccsds_tc_services::insert_tc_segment(tc_segment(first, 42, [1,2,3]), tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [], none), UpdatedFrame).

To inspect the mission-specific segment-header suffix from an extracted
telecommand segment term:

    | ?- ccsds_tc_services::segment_header_suffix(tc_segment(first, 5, [0xAA,0xBB], [1,2,3]), HeaderSuffix).

Mission-specific segment-header suffix octets are preserved when present in the
input frame term:

    | ?- ccsds_tc_services::insert_tc_segment(tc_segment(first, 42, [1,2,3]), tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x00,0xAA,0xBB]), [], none), UpdatedFrame).

To reassemble a segmented telecommand service unit across a sequence of frames:

    | ?- ccsds_tc_services::reassemble_tc_frames([tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x45]), [1,2], none), tc_transfer_frame(0, 1, 0, 42, 3, 8, segment_header([0x85]), [3,4], none)], tc_reassembly_state([]), Segments, UpdatedState).

To reassemble a segmented telecommand service unit across a sequence of frames
and preserve the ordered per-frame mission-specific header-suffix provenance:

    | ?- ccsds_tc_services::reassemble_tc_frames_with_provenance([tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x45,0xAA]), [1,2], none), tc_transfer_frame(0, 1, 0, 42, 3, 8, segment_header([0x85,0xBB]), [3,4], none)], tc_reassembly_state([]), ReassembledSegments, UpdatedState, Events).

To group complete telecommand service units into MAP-aware dispatch buckets:

    | ?- ccsds_tc_services::dispatch_service_units_by_map([tc_segment(unsegmented, 7, [], [0x01]), tc_reassembled_segment(6, [[0xBB]], [0x03]), tc_reassembled_segment(7, [[0xAA]], [0x02])], Dispatches).

To resynchronize on a discontinuous telecommand frame while receiving explicit
recovery events:

    | ?- ccsds_tc_services::reassemble_tc_frame(tc_transfer_frame(0, 1, 0, 42, 3, 9, segment_header([0xC6]), [9], none), resynchronize, tc_reassembly_state([tc_reassembly_channel(42, 3, 8, [tc_pending_segment(5, [1])])]), Segments, UpdatedState, Events).

API documentation
-----------------

Open the [../../apis/library_index.html#ccsds_tc_services](../../apis/library_index.html#ccsds_tc_services)
link in a web browser.

Loading
-------

To load all entities in this library, load the `loader.lgt` file:

    | ?- logtalk_load(ccsds_tc_services(loader)).

Testing
-------

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(ccsds_tc_services(tester)).
