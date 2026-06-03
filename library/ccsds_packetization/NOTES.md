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


`ccsds_packetization`
=====================

The `ccsds_packetization` library provides the inverse service-layer helpers to
the existing `ccsds_packet_services` library.

The current implementation covers:

- fitting packet streams into TM and AOS packet-service regions
- preserving partially emitted packet bytes across frame boundaries
- tracking queued packets per frame type, spacecraft identifier, and virtual
  channel identifier
- generating idle packets deliberately when there is remaining frame capacity
- reporting explicit packetization events for buffered fragments and generated
  idle packets

Representation
--------------

Packetizer state is represented using the compound term:

    packetizer_state(Channels)

Where `Channels` is a list of terms of the form:

    packetizer_channel(FrameType, SpacecraftId, VirtualChannelId, PendingBytes, QueuedPackets)

Where:

- `FrameType` is either `tm` or `aos`
- `SpacecraftId` is the keyed spacecraft identifier
- `VirtualChannelId` is the keyed virtual channel identifier
- `PendingBytes` is a list of packet bytes already started in an earlier frame
  but not yet fully emitted
- `QueuedPackets` is a list of complete `ccsds_packet(...)` terms waiting to be
  packetized for that keyed channel

Idle packets are generated as telemetry packets using APID `2047`, standalone
sequence flags, the requested sequence count, a zero-filled secondary header of
the requested length, and zero-filled user data.

Packetization
-------------

To get the initial packetizer state:

    | ?- ccsds_packetization::initial_state(State).

To inspect the currently buffered pending bytes and queued packets:

    | ?- ccsds_packetization::pending_packets(packetizer_state([packetizer_channel(tm, 42, 3, [0x22,0x33], [ccsds_packet(0,0,0,1,3,7,none,[0x44])])]), PendingPackets).

To packetize packets into a TM transfer frame:

    | ?- ccsds_packetization::packetize_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 2047, none, [0,0,0,0,0,0,0], none, none), 0, packetizer_state([]), [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], UpdatedFrame, UpdatedState).

When the input TM frame already carries a `fecf/1` term,
`packetize_tm_packets/6-7` refresh it from the updated frame bytes. Frames with
`none` keep `none`.

To packetize packets into an AOS transfer frame:

    | ?- ccsds_packetization::packetize_aos_packets(aos_transfer_frame(1, 42, 3, 16, signaling_field(0,0,0,0), none, [0,0,0,0,0,0,0,0,0], none, none), 0, packetizer_state([]), [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], UpdatedFrame, UpdatedState).

When the input AOS frame already carries a `fecf/1` term,
`packetize_aos_packets/6-7` refresh it from the updated frame bytes. Frames with
`none` keep `none`.

To packetize packets across a sequence of TM transfer frames:

    | ?- ccsds_packetization::packetize_tm_frames([Frame1, Frame2], 0, packetizer_state([]), Packets, UpdatedFrames, RemainingPackets, UpdatedState).

Events
------

The richer single-frame and multi-frame packetization predicates return event
lists using the terms:

    buffered_packet_fragment(FrameType, SpacecraftId, VirtualChannelId, PendingBytes)
    generated_idle_packet(FrameType, SpacecraftId, VirtualChannelId, Packet)

Where:

- `buffered_packet_fragment/4` reports trailing packet bytes buffered for the
  next frame
- `generated_idle_packet/4` reports an idle packet synthesized to consume
  remaining frame capacity

Idle packets
------------

To generate a telemetry idle packet with APID `2047`:

    | ?- ccsds_packetization::generate_idle_packet(0, 7, 2, Packet).

API documentation
-----------------

Open the [../../apis/library_index.html#ccsds_packetization](../../apis/library_index.html#ccsds_packetization)
link in a web browser.

Loading
-------

To load all entities in this library, load the `loader.lgt` file:

    | ?- logtalk_load(ccsds_packetization(loader)).

Testing
-------

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(ccsds_packetization(tester)).
