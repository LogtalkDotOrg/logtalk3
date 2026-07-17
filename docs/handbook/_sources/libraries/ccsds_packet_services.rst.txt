.. _library_ccsds_packet_services:

``ccsds_packet_services``
=========================

The ``ccsds_packet_services`` library provides service-aware helpers
that sit above the raw ``ccsds_packets`` and ``ccsds_frames`` codec
libraries.

The current implementation supports telemetry and advanced orbiting
systems packet-zone handling above the raw ``ccsds_frames`` payload
bridge:

- splitting a packet-zone byte sequence using a TM transfer frame first
  header pointer
- parsing the complete packets that start in that zone
- preserving any leading continuation bytes and trailing incomplete
  bytes
- rebuilding a packet zone and reinserting it into a TM transfer frame
- splitting an AOS M_PDU packet-service data field into its
  first-header-pointer header and packet zone
- representing AOS idle-only packet zones distinctly from AOS no-start
  packet zones using the same ``packet_zone/3`` term
- rebuilding an AOS M_PDU packet-service data field and reinserting it
  into an AOS transfer frame
- carrying trailing packet fragments across multiple TM or AOS frames
  using an explicit packet reassembly state term
- reassembling complete packets across frame sequences while preserving
  any still-incomplete trailing fragment for the next batch
- tracking TM and AOS reassembly state per frame type, spacecraft
  identifier, and virtual channel identifier
- detecting dropped or out-of-order TM and AOS frames by checking the
  expected virtual-channel frame counts during reassembly
- applying configurable discontinuity recovery policies at the frame
  level, choosing between throwing an error, dropping the discontinuous
  frame, or resynchronizing on the current frame
- returning explicit recovery event streams from the richer frame
  reassembly predicates so callers can observe dropped fragments,
  skipped frames, and resynchronization points

API documentation
-----------------

Open the
`../../apis/library_index.html#ccsds_packet_services <../../apis/library_index.html#ccsds_packet_services>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(ccsds_packet_services(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(ccsds_packet_services(tester)).

Representation
--------------

Packet zones are represented using the compound term:

::

   packet_zone(PrefixData, Packets, SuffixData)

Where:

- ``PrefixData`` is a list of bytes preceding the first complete packet
- ``Packets`` is a list of complete ``ccsds_packet(...)`` terms
- ``SuffixData`` is a list of bytes trailing the last complete packet

When a telemetry transfer frame first header pointer indicates that no
packet starts in the frame data field, the full data field is
represented as:

::

   packet_zone(DataField, [], [])

For AOS packet service data fields, the same term is used after decoding
the two-octet M_PDU first header pointer:

- ``packet_zone(PrefixData, [], [])`` corresponds to first header
  pointer ``2047`` and means that no packet starts in this frame packet
  zone
- ``packet_zone([], [], SuffixData)`` corresponds to first header
  pointer ``2046`` and means that the packet zone contains idle data
  only

Cross-frame packet reassembly state is represented using the compound
term:

::

   packet_reassembly_state(PendingData)

Where ``PendingData`` is a list of bytes belonging to an incomplete
packet whose header started in an earlier frame and whose remaining
bytes are expected in a later frame.

Frame-channel reassembly state is represented using the compound term:

::

   channel_reassembly_state(Channels)

Where ``Channels`` is a list of terms of the form:

::

   reassembly_channel(FrameType, SpacecraftId, VirtualChannelId, CounterModulus, ExpectedFrameCount, PendingData)

The ``FrameType`` argument is either ``tm`` or ``aos``. The keyed state
is used by the frame-level reassembly predicates so that interleaved
virtual channels and TM versus AOS streams do not collide.

Discontinuity recovery policies are represented by the atoms:

::

       throw
       drop
       resynchronize

The policies have the following meaning when a frame-count discontinuity
is detected for a keyed channel:

- ``throw``: raise a ``domain_error/2`` exception and leave recovery to
  the caller
- ``drop``: discard any buffered fragment for that keyed channel and
  skip the discontinuous frame payload entirely
- ``resynchronize``: discard any buffered fragment for that keyed
  channel but still process the current frame, ignoring any pre-pointer
  continuation bytes

Recovery event streams are returned by the seven-argument TM and AOS
frame reassembly predicates as lists of terms of the form:

::

   dropped_fragment(FrameType, SpacecraftId, VirtualChannelId, Reason, PendingData)
   skipped_frame(FrameType, SpacecraftId, VirtualChannelId, Discontinuity)
   resynchronized(FrameType, SpacecraftId, VirtualChannelId, Discontinuity)

Where ``Reason`` is either:

- ``discontinuity(StoredCounterModulus, CounterModulus, ExpectedFrameCount, VirtualChannelFrameCount)``
- ``idle_only(VirtualChannelFrameCount)`` for AOS idle-only M_PDUs that
  clear a buffered fragment

Packet-zone helpers
-------------------

To split a packet zone using a first header pointer and parse the
complete packets that start there:

::

   | ?- ccsds_packet_services::split_packet_zone([0xAA,0xBB,0x00,0x00,0xC0,0x00,0x00,0x00,0x42,0xCC], 2, 0, PacketZone).

To rebuild a packet zone and recover the corresponding first header
pointer:

::

   | ?- ccsds_packet_services::join_packet_zone(packet_zone([0xAA,0xBB], [ccsds_packet(0,0,0,0,3,0,none,[0x42])], [0xCC]), 0, Bytes, FirstHeaderPointer).

Telemetry transfer frame helpers
--------------------------------

To extract a packet zone from a telemetry transfer frame term:

::

   | ?- ccsds_packet_services::extract_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 2, none, [0xAA,0xBB,0x00,0x00,0xC0,0x00,0x00,0x00,0x42,0xCC], none, none), 0, PacketZone).

To update a telemetry transfer frame from a packet zone term:

::

   | ?- ccsds_packet_services::insert_tm_packets(packet_zone([0xAA,0xBB], [ccsds_packet(0,0,0,0,3,0,none,[0x42])], [0xCC]), 0, tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [], none, none), UpdatedFrame).

If the input TM frame already carries a ``fecf/1`` term,
``insert_tm_packets/4`` refreshes it from the updated frame bytes.
Frames with ``none`` keep ``none``.

Advanced orbiting systems helpers
---------------------------------

To split an AOS M_PDU packet-service data field:

::

   | ?- ccsds_packet_services::split_aos_packet_zone([0x00,0x02,0xAA,0xBB,0x00,0x00,0xC0,0x00,0x00,0x00,0x42,0xCC], 0, PacketZone).

To rebuild an AOS M_PDU packet-service data field from a packet zone
term:

::

   | ?- ccsds_packet_services::join_aos_packet_zone(packet_zone([0xAA,0xBB], [ccsds_packet(0,0,0,0,3,0,none,[0x42])], [0xCC]), 0, Bytes).

To extract a packet zone from an AOS transfer frame term:

::

   | ?- ccsds_packet_services::extract_aos_packets(aos_transfer_frame(1, 42, 3, 16, signaling_field(0,0,0,0), none, [0x00,0x02,0xAA,0xBB,0x00,0x00,0xC0,0x00,0x00,0x00,0x42,0xCC], none, none), 0, PacketZone).

To update an AOS transfer frame from a packet zone term:

::

   | ?- ccsds_packet_services::insert_aos_packets(packet_zone([0xAA,0xBB], [ccsds_packet(0,0,0,0,3,0,none,[0x42])], [0xCC]), 0, aos_transfer_frame(1, 42, 3, 16, signaling_field(0,0,0,0), none, [], none, none), UpdatedFrame).

If the input AOS frame already carries a ``fecf/1`` term,
``insert_aos_packets/4`` refreshes it from the updated frame bytes.
Frames with ``none`` keep ``none``.

Cross-frame packet reassembly
-----------------------------

To get the initial low-level packet reassembly state:

::

   | ?- ccsds_packet_services::initial_reassembly_state(State).

To get the initial keyed frame-channel reassembly state:

::

   | ?- ccsds_packet_services::initial_channel_reassembly_state(State).

To reassemble a packet zone against a prior trailing fragment:

::

   | ?- ccsds_packet_services::reassemble_packet_zone(packet_zone([0x00,0x00,0x42], [ccsds_packet(0,0,0,0,3,1,none,[0x43])], []), 0, packet_reassembly_state([0x00,0x00,0xC0,0x00]), Packets, UpdatedState).

To reassemble complete packets across a sequence of telemetry transfer
frames:

::

   | ?- ccsds_packet_services::reassemble_tm_frames([tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00,0x00,0xC0,0x00], none, none), tm_transfer_frame(0, 42, 3, 0, 16, 33, 0, 0, 0, 3, 3, none, [0x00,0x00,0x42,0x00,0x00,0xC0,0x01,0x00,0x00,0x43], none, none)], 0, channel_reassembly_state([]), Packets, UpdatedState).

To reassemble telemetry transfer frames while resynchronizing
automatically on a detected discontinuity:

::

   | ?- ccsds_packet_services::reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 34, 0, 0, 0, 3, 3, none, [0x00,0x00,0x42,0x00,0x00,0xC0,0x02,0x00,0x00,0x44], none, none), 0, resynchronize, channel_reassembly_state([reassembly_channel(tm,42,3,256,33,[0x00,0x00,0xC0,0x00])]), Packets, UpdatedState).

To reassemble a telemetry transfer frame and also receive explicit
recovery events:

::

   | ?- ccsds_packet_services::reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 34, 0, 0, 0, 3, 3, none, [0x00,0x00,0x42,0x00,0x00,0xC0,0x02,0x00,0x00,0x44], none, none), 0, resynchronize, channel_reassembly_state([reassembly_channel(tm,42,3,256,33,[0x00,0x00,0xC0,0x00])]), Packets, UpdatedState, Events).

To reassemble complete packets across a sequence of AOS transfer frames:

::

   | ?- ccsds_packet_services::reassemble_aos_frames([aos_transfer_frame(1, 42, 3, 16, signaling_field(0,0,0,0), none, [0x00,0x00,0x00,0x00,0xC0,0x00], none, none), aos_transfer_frame(1, 42, 3, 17, signaling_field(0,0,0,0), none, [0x00,0x03,0x00,0x00,0x42,0x00,0x00,0xC0,0x01,0x00,0x00,0x43], none, none)], 0, channel_reassembly_state([]), Packets, UpdatedState).

To reassemble AOS transfer frames while dropping a discontinuous frame
and resetting the keyed buffered fragment:

::

   | ?- ccsds_packet_services::reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 18, signaling_field(0,0,0,0), none, [0x00,0x03,0x00,0x00,0x42,0x00,0x00,0xC0,0x02,0x00,0x00,0x44], none, none), 0, drop, channel_reassembly_state([reassembly_channel(aos,42,3,16777216,17,[0x00,0x00,0xC0,0x00])]), Packets, UpdatedState).

To reassemble AOS transfer frames and receive explicit recovery events
across a frame sequence:

::

   | ?- ccsds_packet_services::reassemble_aos_frames([aos_transfer_frame(1, 42, 3, 16, signaling_field(0,0,0,0), none, [0x00,0x00,0x00,0x00,0xC0,0x00], none, none), aos_transfer_frame(1, 42, 3, 18, signaling_field(0,0,0,0), none, [0x00,0x03,0x00,0x00,0x42,0x00,0x00,0xC0,0x02,0x00,0x00,0x44], none, none)], 0, drop, channel_reassembly_state([]), Packets, UpdatedState, Events).

To inspect the currently buffered keyed pending fragments:

::

   | ?- ccsds_packet_services::pending_fragments(channel_reassembly_state([reassembly_channel(tm,42,3,256,33,[0x00,0x00,0xC0,0x00])]), PendingFragments).

For AOS packet service, idle-only M_PDUs clear any pending fragment
state. This matches the packet-zone interpretation where first header
pointer ``2046`` denotes idle data rather than continuation bytes.

If a TM or AOS frame arrives with a virtual-channel frame count
different from the keyed state expected count, the frame-level
reassembly predicates apply the selected discontinuity recovery policy.
The five-argument variants default to the ``throw`` policy. The
seven-argument variants additionally report the recovery decisions as an
explicit event stream.

Scope
-----

This milestone covers TM transfer frame packet zones, AOS packet-service
M_PDUs, low-level packet-fragment stitching, and keyed TM/AOS
cross-frame packet reassembly with continuity checks and configurable
discontinuity recovery. Future milestones can extend the same library
with segmentation policies, idle packet generation, richer per-channel
recovery controls, and higher level packetization policies.
