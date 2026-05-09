%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(ccsds_packet_services).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-05-08,
		comment is 'Helpers for CCSDS packet service packet-zone splitting and cross-frame TM/AOS packet reassembly.'
	]).

	:- public(valid_reassembly_state/1).
	:- mode(valid_reassembly_state(@compound), zero_or_one).
	:- info(valid_reassembly_state/1, [
		comment is 'True if the argument is a valid packet reassembly state term.',
		argnames is ['State']
	]).

	:- public(initial_reassembly_state/1).
	:- mode(initial_reassembly_state(-compound), one).
	:- info(initial_reassembly_state/1, [
		comment is 'Returns the initial cross-frame packet reassembly state.',
		argnames is ['State']
	]).

	:- public(pending_data/2).
	:- mode(pending_data(+compound, -list(byte)), one).
	:- info(pending_data/2, [
		comment is 'Extracts the currently buffered trailing packet fragment bytes from a packet reassembly state.',
		argnames is ['State', 'Bytes']
	]).

	:- public(valid_channel_reassembly_state/1).
	:- mode(valid_channel_reassembly_state(@compound), zero_or_one).
	:- info(valid_channel_reassembly_state/1, [
		comment is 'True if the argument is a valid frame-channel packet reassembly state term keyed by frame type, spacecraft identifier, and virtual channel identifier.',
		argnames is ['State']
	]).

	:- public(initial_channel_reassembly_state/1).
	:- mode(initial_channel_reassembly_state(-compound), one).
	:- info(initial_channel_reassembly_state/1, [
		comment is 'Returns the initial frame-channel packet reassembly state.',
		argnames is ['State']
	]).

	:- public(pending_fragments/2).
	:- mode(pending_fragments(+compound, -list(compound)), one).
	:- info(pending_fragments/2, [
		comment is 'Extracts the non-empty pending packet fragments buffered per frame type, spacecraft identifier, and virtual channel identifier.',
		argnames is ['State', 'PendingFragments']
	]).

	:- public(valid_discontinuity_policy/1).
	:- mode(valid_discontinuity_policy(@atom), zero_or_one).
	:- info(valid_discontinuity_policy/1, [
		comment is 'True if the argument is a valid discontinuity recovery policy atom. Valid values are ``throw``, ``drop``, and ``resynchronize``.',
		argnames is ['Policy']
	]).

	:- public(valid/1).
	:- mode(valid(@compound), zero_or_one).
	:- info(valid/1, [
		comment is 'True if the argument is a valid packet-zone term.',
		argnames is ['PacketZone']
	]).

	:- public(prefix_data/2).
	:- mode(prefix_data(+compound, -list(byte)), one).
	:- info(prefix_data/2, [
		comment is 'Extracts the bytes that precede the first complete packet in a packet zone.',
		argnames is ['PacketZone', 'PrefixData']
	]).

	:- public(packets/2).
	:- mode(packets(+compound, -list(compound)), one).
	:- info(packets/2, [
		comment is 'Extracts the list of complete packets in a packet zone.',
		argnames is ['PacketZone', 'Packets']
	]).

	:- public(suffix_data/2).
	:- mode(suffix_data(+compound, -list(byte)), one).
	:- info(suffix_data/2, [
		comment is 'Extracts the bytes that trail the last complete packet in a packet zone.',
		argnames is ['PacketZone', 'SuffixData']
	]).

	:- public(split_packet_zone/4).
	:- mode(split_packet_zone(+list(byte), +integer, +integer, -compound), one_or_error).
	:- info(split_packet_zone/4, [
		comment is 'Splits a packet-zone byte sequence using the first header pointer and parses any complete packets starting at that offset. A pointer value of 2047 denotes that no packet starts in the zone.',
		argnames is ['Bytes', 'FirstHeaderPointer', 'SecondaryHeaderLength', 'PacketZone']
	]).

	:- public(join_packet_zone/4).
	:- mode(join_packet_zone(+compound, +integer, -list(byte), -integer), one_or_error).
	:- info(join_packet_zone/4, [
		comment is 'Encodes a packet-zone term back into bytes and returns the corresponding first header pointer. When the packet list is empty, the pointer is set to 2047.',
		argnames is ['PacketZone', 'SecondaryHeaderLength', 'Bytes', 'FirstHeaderPointer']
	]).

	:- public(extract_tm_packets/3).
	:- mode(extract_tm_packets(+compound, +integer, -compound), one_or_error).
	:- info(extract_tm_packets/3, [
		comment is 'Extracts a TM transfer frame packet zone as a packet-zone term using the frame first header pointer and the given packet secondary header length.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'PacketZone']
	]).

	:- public(insert_tm_packets/4).
	:- mode(insert_tm_packets(+compound, +integer, +compound, -compound), one_or_error).
	:- info(insert_tm_packets/4, [
		comment is 'Updates a TM transfer frame data field and first header pointer from a packet-zone term while preserving the remaining frame fields unchanged.',
		argnames is ['PacketZone', 'SecondaryHeaderLength', 'Frame', 'UpdatedFrame']
	]).

	:- public(split_aos_packet_zone/3).
	:- mode(split_aos_packet_zone(+list(byte), +integer, -compound), one_or_error).
	:- info(split_aos_packet_zone/3, [
		comment is 'Splits an AOS M_PDU packet-service data field into a packet-zone term. The first two octets are interpreted as the M_PDU first header pointer field, where 2046 denotes idle data only and 2047 denotes that no packet starts in the packet zone.',
		argnames is ['Bytes', 'SecondaryHeaderLength', 'PacketZone']
	]).

	:- public(join_aos_packet_zone/3).
	:- mode(join_aos_packet_zone(+compound, +integer, -list(byte)), one_or_error).
	:- info(join_aos_packet_zone/3, [
		comment is 'Encodes a packet-zone term as an AOS M_PDU packet-service data field, including the two-octet first header pointer field.',
		argnames is ['PacketZone', 'SecondaryHeaderLength', 'Bytes']
	]).

	:- public(reassemble_packet_zone/5).
	:- mode(reassemble_packet_zone(+compound, +integer, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_packet_zone/5, [
		comment is 'Reassembles complete packets from a packet-zone term and a prior packet reassembly state, returning any complete packets plus the updated trailing fragment state.',
		argnames is ['PacketZone', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedState']
	]).

	:- public(extract_aos_packets/3).
	:- mode(extract_aos_packets(+compound, +integer, -compound), one_or_error).
	:- info(extract_aos_packets/3, [
		comment is 'Extracts an AOS transfer frame packet zone as a packet-zone term using the two-octet M_PDU header at the beginning of the frame data field.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'PacketZone']
	]).

	:- public(insert_aos_packets/4).
	:- mode(insert_aos_packets(+compound, +integer, +compound, -compound), one_or_error).
	:- info(insert_aos_packets/4, [
		comment is 'Updates an AOS transfer frame data field from a packet-zone term by regenerating the M_PDU first header pointer and packet-zone bytes while preserving the remaining frame fields unchanged.',
		argnames is ['PacketZone', 'SecondaryHeaderLength', 'Frame', 'UpdatedFrame']
	]).

	:- public(reassemble_tm_packets/5).
	:- mode(reassemble_tm_packets(+compound, +integer, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_tm_packets/5, [
		comment is 'Extracts the packet zone from a TM transfer frame, using the default ``throw`` discontinuity recovery policy, and returns the complete packets plus the updated channel reassembly state.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedState']
	]).

	:- public(reassemble_tm_packets/6).
	:- mode(reassemble_tm_packets(+compound, +integer, +atom, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_tm_packets/6, [
		comment is 'Extracts the packet zone from a TM transfer frame, applies the selected discontinuity recovery policy when frame-count continuity is broken, and returns the complete packets plus the updated channel reassembly state.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'Policy', 'State', 'Packets', 'UpdatedState']
	]).

	:- public(reassemble_tm_packets/7).
	:- mode(reassemble_tm_packets(+compound, +integer, +atom, +compound, -list(compound), -compound, -list(compound)), one_or_error).
	:- info(reassemble_tm_packets/7, [
		comment is 'Extracts the packet zone from a TM transfer frame, applies the selected discontinuity recovery policy when frame-count continuity is broken, and returns the complete packets, updated channel reassembly state, and explicit recovery events.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'Policy', 'State', 'Packets', 'UpdatedState', 'Events']
	]).

	:- public(reassemble_tm_frames/5).
	:- mode(reassemble_tm_frames(+list(compound), +integer, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_tm_frames/5, [
		comment is 'Reassembles complete packets across a sequence of TM transfer frames using the default ``throw`` discontinuity recovery policy.',
		argnames is ['Frames', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedState']
	]).

	:- public(reassemble_tm_frames/6).
	:- mode(reassemble_tm_frames(+list(compound), +integer, +atom, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_tm_frames/6, [
		comment is 'Reassembles complete packets across a sequence of TM transfer frames using the selected discontinuity recovery policy.',
		argnames is ['Frames', 'SecondaryHeaderLength', 'Policy', 'State', 'Packets', 'UpdatedState']
	]).

	:- public(reassemble_tm_frames/7).
	:- mode(reassemble_tm_frames(+list(compound), +integer, +atom, +compound, -list(compound), -compound, -list(compound)), one_or_error).
	:- info(reassemble_tm_frames/7, [
		comment is 'Reassembles complete packets across a sequence of TM transfer frames using the selected discontinuity recovery policy and returns any recovery events in frame order.',
		argnames is ['Frames', 'SecondaryHeaderLength', 'Policy', 'State', 'Packets', 'UpdatedState', 'Events']
	]).

	:- public(reassemble_aos_packets/5).
	:- mode(reassemble_aos_packets(+compound, +integer, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_aos_packets/5, [
		comment is 'Extracts the packet zone from an AOS transfer frame, using the default ``throw`` discontinuity recovery policy, and returns the complete packets plus the updated channel reassembly state.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedState']
	]).

	:- public(reassemble_aos_packets/6).
	:- mode(reassemble_aos_packets(+compound, +integer, +atom, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_aos_packets/6, [
		comment is 'Extracts the packet zone from an AOS transfer frame, applies the selected discontinuity recovery policy when frame-count continuity is broken, and returns the complete packets plus the updated channel reassembly state.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'Policy', 'State', 'Packets', 'UpdatedState']
	]).

	:- public(reassemble_aos_packets/7).
	:- mode(reassemble_aos_packets(+compound, +integer, +atom, +compound, -list(compound), -compound, -list(compound)), one_or_error).
	:- info(reassemble_aos_packets/7, [
		comment is 'Extracts the packet zone from an AOS transfer frame, applies the selected discontinuity recovery policy when frame-count continuity is broken, and returns the complete packets, updated channel reassembly state, and explicit recovery events.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'Policy', 'State', 'Packets', 'UpdatedState', 'Events']
	]).

	:- public(reassemble_aos_frames/5).
	:- mode(reassemble_aos_frames(+list(compound), +integer, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_aos_frames/5, [
		comment is 'Reassembles complete packets across a sequence of AOS transfer frames using the default ``throw`` discontinuity recovery policy.',
		argnames is ['Frames', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedState']
	]).

	:- public(reassemble_aos_frames/6).
	:- mode(reassemble_aos_frames(+list(compound), +integer, +atom, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_aos_frames/6, [
		comment is 'Reassembles complete packets across a sequence of AOS transfer frames using the selected discontinuity recovery policy.',
		argnames is ['Frames', 'SecondaryHeaderLength', 'Policy', 'State', 'Packets', 'UpdatedState']
	]).

	:- public(reassemble_aos_frames/7).
	:- mode(reassemble_aos_frames(+list(compound), +integer, +atom, +compound, -list(compound), -compound, -list(compound)), one_or_error).
	:- info(reassemble_aos_frames/7, [
		comment is 'Reassembles complete packets across a sequence of AOS transfer frames using the selected discontinuity recovery policy and returns any recovery events in frame order.',
		argnames is ['Frames', 'SecondaryHeaderLength', 'Policy', 'State', 'Packets', 'UpdatedState', 'Events']
	]).

	:- uses(list, [
		append/3, length/2, take/4
	]).

	:- uses(type, [
		valid/2
	]).

	valid(packet_zone(PrefixData, Packets, SuffixData)) :-
		valid(list(byte), PrefixData),
		valid_packet_terms(Packets),
		valid(list(byte), SuffixData).

	valid_reassembly_state(packet_reassembly_state(PendingData)) :-
		valid(list(byte), PendingData).

	initial_reassembly_state(packet_reassembly_state([])).

	pending_data(packet_reassembly_state(PendingData), PendingData).

	valid_channel_reassembly_state(channel_reassembly_state(Channels)) :-
		valid_reassembly_channels(Channels).

	initial_channel_reassembly_state(channel_reassembly_state([])).

	pending_fragments(channel_reassembly_state(Channels), PendingFragments) :-
		pending_fragments_(Channels, PendingFragments).

	valid_discontinuity_policy(throw).
	valid_discontinuity_policy(drop).
	valid_discontinuity_policy(resynchronize).

	prefix_data(packet_zone(PrefixData, _, _), PrefixData).

	packets(packet_zone(_, Packets, _), Packets).

	suffix_data(packet_zone(_, _, SuffixData), SuffixData).

	split_packet_zone(Bytes, _, _, _) :-
		var(Bytes),
		instantiation_error.
	split_packet_zone(_, FirstHeaderPointer, _, _) :-
		var(FirstHeaderPointer),
		instantiation_error.
	split_packet_zone(_, _, SecondaryHeaderLength, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	split_packet_zone(Bytes, FirstHeaderPointer, SecondaryHeaderLength, PacketZone) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		valid_packet_zone_bytes(Bytes),
		valid_first_header_pointer(FirstHeaderPointer, Bytes),
		split_packet_zone_(Bytes, FirstHeaderPointer, SecondaryHeaderLength, PacketZone).

	join_packet_zone(PacketZone, _, _, _) :-
		var(PacketZone),
		instantiation_error.
	join_packet_zone(_, SecondaryHeaderLength, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	join_packet_zone(PacketZone, SecondaryHeaderLength, Bytes, FirstHeaderPointer) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		(   valid(PacketZone) ->
			true
		;   domain_error(ccsds_packet_zone, PacketZone)
		),
		PacketZone = packet_zone(PrefixData, Packets, SuffixData),
		(   Packets == [] ->
			append(PrefixData, SuffixData, Bytes),
			FirstHeaderPointer = 2047
		;   ccsds_packets(SecondaryHeaderLength)::generate(bytes(PacketBytes), Packets),
			append(PrefixData, PacketBytes, Bytes0),
			append(Bytes0, SuffixData, Bytes),
			length(PrefixData, FirstHeaderPointer)
		).

	extract_tm_packets(Frame, _, _) :-
		var(Frame),
		instantiation_error.
	extract_tm_packets(_, SecondaryHeaderLength, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	extract_tm_packets(Frame, SecondaryHeaderLength, PacketZone) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		tm_packet_zone(Frame, FirstHeaderPointer, Bytes),
		split_packet_zone(Bytes, FirstHeaderPointer, SecondaryHeaderLength, PacketZone).

	insert_tm_packets(PacketZone, _, _, _) :-
		var(PacketZone),
		instantiation_error.
	insert_tm_packets(_, SecondaryHeaderLength, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	insert_tm_packets(_, _, Frame, _) :-
		var(Frame),
		instantiation_error.
	insert_tm_packets(PacketZone, SecondaryHeaderLength, Frame, UpdatedFrame) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		tm_packet_zone(Frame, _, _),
		join_packet_zone(PacketZone, SecondaryHeaderLength, DataField, FirstHeaderPointer),
		replace_tm_packet_zone(Frame, FirstHeaderPointer, DataField, UpdatedFrame).

	split_aos_packet_zone(Bytes, _, _) :-
		var(Bytes),
		instantiation_error.
	split_aos_packet_zone(_, SecondaryHeaderLength, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	split_aos_packet_zone(Bytes, SecondaryHeaderLength, PacketZone) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		valid_aos_packet_zone_bytes(Bytes),
		decode_aos_packet_zone(Bytes, FirstHeaderPointer, PacketZoneBytes),
		split_aos_packet_zone_(FirstHeaderPointer, PacketZoneBytes, SecondaryHeaderLength, PacketZone).

	join_aos_packet_zone(PacketZone, _, _) :-
		var(PacketZone),
		instantiation_error.
	join_aos_packet_zone(_, SecondaryHeaderLength, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	join_aos_packet_zone(PacketZone, SecondaryHeaderLength, Bytes) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		join_aos_packet_zone_(PacketZone, SecondaryHeaderLength, PacketZoneBytes, FirstHeaderPointer),
		encode_aos_packet_zone(FirstHeaderPointer, PacketZoneBytes, Bytes).

	reassemble_packet_zone(PacketZone, _, _, _, _) :-
		var(PacketZone),
		instantiation_error.
	reassemble_packet_zone(_, SecondaryHeaderLength, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_packet_zone(_, _, State, _, _) :-
		var(State),
		instantiation_error.
	reassemble_packet_zone(PacketZone, SecondaryHeaderLength, State, Packets, UpdatedState) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		(   valid(PacketZone) ->
			true
		;   domain_error(ccsds_packet_zone, PacketZone)
		),
		valid_packet_reassembly_state(State),
		reassemble_packet_zone_(PacketZone, SecondaryHeaderLength, State, Packets, UpdatedState).

	extract_aos_packets(Frame, _, _) :-
		var(Frame),
		instantiation_error.
	extract_aos_packets(_, SecondaryHeaderLength, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	extract_aos_packets(Frame, SecondaryHeaderLength, PacketZone) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		aos_packet_zone(Frame, Bytes),
		split_aos_packet_zone(Bytes, SecondaryHeaderLength, PacketZone).

	insert_aos_packets(PacketZone, _, _, _) :-
		var(PacketZone),
		instantiation_error.
	insert_aos_packets(_, SecondaryHeaderLength, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	insert_aos_packets(_, _, Frame, _) :-
		var(Frame),
		instantiation_error.
	insert_aos_packets(PacketZone, SecondaryHeaderLength, Frame, UpdatedFrame) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		aos_packet_zone(Frame, _),
		join_aos_packet_zone(PacketZone, SecondaryHeaderLength, DataField),
		replace_aos_packet_zone(Frame, DataField, UpdatedFrame).

	reassemble_tm_packets(Frame, _, _, _, _) :-
		var(Frame),
		instantiation_error.
	reassemble_tm_packets(_, SecondaryHeaderLength, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_tm_packets(_, _, State, _, _) :-
		var(State),
		instantiation_error.
	reassemble_tm_packets(Frame, SecondaryHeaderLength, State, Packets, UpdatedState) :-
		reassemble_tm_packets(Frame, SecondaryHeaderLength, throw, State, Packets, UpdatedState).

	reassemble_tm_packets(Frame, _, _, _, _, _) :-
		var(Frame),
		instantiation_error.
	reassemble_tm_packets(_, SecondaryHeaderLength, _, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_tm_packets(_, _, Policy, _, _, _) :-
		var(Policy),
		instantiation_error.
	reassemble_tm_packets(_, _, _, State, _, _) :-
		var(State),
		instantiation_error.
	reassemble_tm_packets(Frame, SecondaryHeaderLength, Policy, State, Packets, UpdatedState) :-
		reassemble_tm_packets(Frame, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, _).

	reassemble_tm_packets(Frame, _, _, _, _, _, _) :-
		var(Frame),
		instantiation_error.
	reassemble_tm_packets(_, SecondaryHeaderLength, _, _, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_tm_packets(_, _, Policy, _, _, _, _) :-
		var(Policy),
		instantiation_error.
	reassemble_tm_packets(_, _, _, State, _, _, _) :-
		var(State),
		instantiation_error.
	reassemble_tm_packets(Frame, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		valid_discontinuity_policy_(Policy),
		valid_channel_reassembly_state_(State),
		tm_reassembly_context(Frame, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, PacketZone),
		reassemble_channel_packet_zone(tm, SpacecraftId, VirtualChannelId, 256, VirtualChannelFrameCount, PacketZone, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events).

	reassemble_tm_frames(Frames, _, _, _, _) :-
		var(Frames),
		instantiation_error.
	reassemble_tm_frames(_, SecondaryHeaderLength, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_tm_frames(_, _, State, _, _) :-
		var(State),
		instantiation_error.
	reassemble_tm_frames(Frames, SecondaryHeaderLength, State, Packets, UpdatedState) :-
		reassemble_tm_frames(Frames, SecondaryHeaderLength, throw, State, Packets, UpdatedState).

	reassemble_tm_frames(Frames, _, _, _, _, _) :-
		var(Frames),
		instantiation_error.
	reassemble_tm_frames(_, SecondaryHeaderLength, _, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_tm_frames(_, _, Policy, _, _, _) :-
		var(Policy),
		instantiation_error.
	reassemble_tm_frames(_, _, _, State, _, _) :-
		var(State),
		instantiation_error.
	reassemble_tm_frames(Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState) :-
		reassemble_tm_frames(Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, _).

	reassemble_tm_frames(Frames, _, _, _, _, _, _) :-
		var(Frames),
		instantiation_error.
	reassemble_tm_frames(_, SecondaryHeaderLength, _, _, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_tm_frames(_, _, Policy, _, _, _, _) :-
		var(Policy),
		instantiation_error.
	reassemble_tm_frames(_, _, _, State, _, _, _) :-
		var(State),
		instantiation_error.
	reassemble_tm_frames(Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		valid_discontinuity_policy_(Policy),
		valid_channel_reassembly_state_(State),
		reassemble_tm_frames_(Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events).

	reassemble_aos_packets(Frame, _, _, _, _) :-
		var(Frame),
		instantiation_error.
	reassemble_aos_packets(_, SecondaryHeaderLength, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_aos_packets(_, _, State, _, _) :-
		var(State),
		instantiation_error.
	reassemble_aos_packets(Frame, SecondaryHeaderLength, State, Packets, UpdatedState) :-
		reassemble_aos_packets(Frame, SecondaryHeaderLength, throw, State, Packets, UpdatedState).

	reassemble_aos_packets(Frame, _, _, _, _, _) :-
		var(Frame),
		instantiation_error.
	reassemble_aos_packets(_, SecondaryHeaderLength, _, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_aos_packets(_, _, Policy, _, _, _) :-
		var(Policy),
		instantiation_error.
	reassemble_aos_packets(_, _, _, State, _, _) :-
		var(State),
		instantiation_error.
	reassemble_aos_packets(Frame, SecondaryHeaderLength, Policy, State, Packets, UpdatedState) :-
		reassemble_aos_packets(Frame, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, _).

	reassemble_aos_packets(Frame, _, _, _, _, _, _) :-
		var(Frame),
		instantiation_error.
	reassemble_aos_packets(_, SecondaryHeaderLength, _, _, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_aos_packets(_, _, Policy, _, _, _, _) :-
		var(Policy),
		instantiation_error.
	reassemble_aos_packets(_, _, _, State, _, _, _) :-
		var(State),
		instantiation_error.
	reassemble_aos_packets(Frame, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		valid_discontinuity_policy_(Policy),
		valid_channel_reassembly_state_(State),
		aos_reassembly_context(Frame, SpacecraftId, VirtualChannelId, CounterModulus, VirtualChannelFrameCount, ReassemblyMode),
		reassemble_aos_channel(SpacecraftId, VirtualChannelId, CounterModulus, VirtualChannelFrameCount, ReassemblyMode, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events).

	reassemble_aos_frames(Frames, _, _, _, _) :-
		var(Frames),
		instantiation_error.
	reassemble_aos_frames(_, SecondaryHeaderLength, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_aos_frames(_, _, State, _, _) :-
		var(State),
		instantiation_error.
	reassemble_aos_frames(Frames, SecondaryHeaderLength, State, Packets, UpdatedState) :-
		reassemble_aos_frames(Frames, SecondaryHeaderLength, throw, State, Packets, UpdatedState).

	reassemble_aos_frames(Frames, _, _, _, _, _) :-
		var(Frames),
		instantiation_error.
	reassemble_aos_frames(_, SecondaryHeaderLength, _, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_aos_frames(_, _, Policy, _, _, _) :-
		var(Policy),
		instantiation_error.
	reassemble_aos_frames(_, _, _, State, _, _) :-
		var(State),
		instantiation_error.
	reassemble_aos_frames(Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState) :-
		reassemble_aos_frames(Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, _).

	reassemble_aos_frames(Frames, _, _, _, _, _, _) :-
		var(Frames),
		instantiation_error.
	reassemble_aos_frames(_, SecondaryHeaderLength, _, _, _, _, _) :-
		var(SecondaryHeaderLength),
		instantiation_error.
	reassemble_aos_frames(_, _, Policy, _, _, _, _) :-
		var(Policy),
		instantiation_error.
	reassemble_aos_frames(_, _, _, State, _, _, _) :-
		var(State),
		instantiation_error.
	reassemble_aos_frames(Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events) :-
		valid_secondary_header_length(SecondaryHeaderLength),
		valid_discontinuity_policy_(Policy),
		valid_channel_reassembly_state_(State),
		reassemble_aos_frames_(Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events).

	split_packet_zone_(Bytes, 2047, _, packet_zone(Bytes, [], [])) :-
		!.
	split_packet_zone_(Bytes, FirstHeaderPointer, SecondaryHeaderLength, packet_zone(PrefixData, Packets, SuffixData)) :-
		take(FirstHeaderPointer, Bytes, PrefixData, PacketBytes),
		take_complete_packets(PacketBytes, SecondaryHeaderLength, Packets, SuffixData).

	take_complete_packets(Bytes, SecondaryHeaderLength, [Packet| Packets], SuffixData) :-
		packet_total_length(Bytes, TotalLength),
		take(TotalLength, Bytes, PacketBytes, Rest),
		ccsds_packets(SecondaryHeaderLength)::parse(bytes(PacketBytes), [Packet]),
		!,
		take_complete_packets(Rest, SecondaryHeaderLength, Packets, SuffixData).
	take_complete_packets(Bytes, _, [], Bytes).

	split_aos_packet_zone_(2046, Bytes, _, packet_zone([], [], Bytes)) :-
		!.
	split_aos_packet_zone_(2047, Bytes, _, packet_zone(Bytes, [], [])) :-
		!.
	split_aos_packet_zone_(FirstHeaderPointer, Bytes, SecondaryHeaderLength, PacketZone) :-
		split_packet_zone(Bytes, FirstHeaderPointer, SecondaryHeaderLength, PacketZone).

	reassemble_packet_zone_(packet_zone([], Packets, SuffixData), _, packet_reassembly_state([]), Packets, packet_reassembly_state(SuffixData)) :-
		!.
	reassemble_packet_zone_(packet_zone(PrefixData, [], []), SecondaryHeaderLength, packet_reassembly_state([]), [], packet_reassembly_state(PrefixData)) :-
		PrefixData \== [],
		valid_secondary_header_length(SecondaryHeaderLength),
		!.
	reassemble_packet_zone_(packet_zone(PrefixData, Packets, SuffixData), SecondaryHeaderLength, packet_reassembly_state([]), Packets, packet_reassembly_state(SuffixData)) :-
		PrefixData \== [],
		valid_secondary_header_length(SecondaryHeaderLength),
		!.
	reassemble_packet_zone_(packet_zone(PrefixData, Packets, SuffixData), SecondaryHeaderLength, packet_reassembly_state(PendingData), ReassembledPackets, packet_reassembly_state(UpdatedPendingData)) :-
		PendingData \== [],
		PrefixData \== [],
		!,
		append(PendingData, PrefixData, CombinedBytes),
		reassemble_pending_fragment(CombinedBytes, SecondaryHeaderLength, Packets, PendingPackets, PendingContinuation),
		append(PendingPackets, Packets, ReassembledPackets),
		updated_pending_fragment(PendingData, PrefixData, Packets, SuffixData, PendingContinuation, UpdatedPendingData).
	reassemble_packet_zone_(packet_zone([], _, _), _, packet_reassembly_state(PendingData), _, _) :-
		PendingData \== [],
		domain_error(ccsds_packet_reassembly_continuation, packet_reassembly_state(PendingData)).

	reassemble_pending_fragment(CombinedBytes, SecondaryHeaderLength, _, [Packet], []) :-
		recover_complete_packet(CombinedBytes, SecondaryHeaderLength, Packet),
		!.
	reassemble_pending_fragment(CombinedBytes, _, [], [], CombinedBytes) :-
		!.
	reassemble_pending_fragment(_, _, [_| _], _, _) :-
		domain_error(ccsds_packet_reassembly_continuation, incomplete_continuation_before_new_packet).

	updated_pending_fragment(_, PrefixData, [], [], PendingContinuation, PendingContinuation) :-
		PrefixData \== [],
		!.
	updated_pending_fragment(_, _, _, SuffixData, _, SuffixData).

	reassemble_tm_frames_([Frame| Frames], SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events) :-
		reassemble_tm_packets(Frame, SecondaryHeaderLength, Policy, State, FramePackets, IntermediateState, FrameEvents),
		append(FramePackets, RemainingPackets, Packets),
		append(FrameEvents, RemainingEvents, Events),
		reassemble_tm_frames_(Frames, SecondaryHeaderLength, Policy, IntermediateState, RemainingPackets, UpdatedState, RemainingEvents).
	reassemble_tm_frames_([], _, _, State, [], State, []).

	reassemble_aos_frames_([Frame| Frames], SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events) :-
		reassemble_aos_packets(Frame, SecondaryHeaderLength, Policy, State, FramePackets, IntermediateState, FrameEvents),
		append(FramePackets, RemainingPackets, Packets),
		append(FrameEvents, RemainingEvents, Events),
		reassemble_aos_frames_(Frames, SecondaryHeaderLength, Policy, IntermediateState, RemainingPackets, UpdatedState, RemainingEvents).
	reassemble_aos_frames_([], _, _, State, [], State, []).

	tm_reassembly_context(tm_transfer_frame(_, SpacecraftId, VirtualChannelId, _, _, VirtualChannelFrameCount, _, _, _, _, FirstHeaderPointer, _, DataField, _, _), SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, PacketZone) :-
		!,
		PacketZone = packet_zone_context(FirstHeaderPointer, DataField).
	tm_reassembly_context(Frame, _, _, _, _) :-
		domain_error(ccsds_tm_transfer_frame_term, Frame).

	aos_reassembly_context(aos_transfer_frame(_, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, _, DataField, _, _), SpacecraftId, VirtualChannelId, CounterModulus, VirtualChannelFrameCount, ReassemblyMode) :-
		!,
		valid_aos_packet_zone_bytes(DataField),
		decode_aos_packet_zone(DataField, FirstHeaderPointer, PacketZoneBytes),
		aos_counter_modulus(SignalingField, CounterModulus),
		(   FirstHeaderPointer =:= 2046 ->
			ReassemblyMode = idle_only
		;   ReassemblyMode = packet_zone_context(FirstHeaderPointer, PacketZoneBytes)
		).
	aos_reassembly_context(Frame, _, _, _, _, _) :-
		domain_error(ccsds_aos_transfer_frame_term, Frame).

	reassemble_aos_channel(SpacecraftId, VirtualChannelId, CounterModulus, VirtualChannelFrameCount, idle_only, _, Policy, State, [], UpdatedState, Events) :-
		!,
		channel_state(State, aos, SpacecraftId, VirtualChannelId, CounterModulus, VirtualChannelFrameCount, Policy, _, PendingData, RecoveryEvents),
		next_frame_count(VirtualChannelFrameCount, CounterModulus, ExpectedFrameCount),
		idle_only_recovery_events(aos, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, PendingData, IdleEvents),
		append(RecoveryEvents, IdleEvents, Events),
		update_channel_state(State, aos, SpacecraftId, VirtualChannelId, CounterModulus, ExpectedFrameCount, [], UpdatedState).
	reassemble_aos_channel(SpacecraftId, VirtualChannelId, CounterModulus, VirtualChannelFrameCount, packet_zone_context(FirstHeaderPointer, PacketZoneBytes), SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events) :-
		channel_state(State, aos, SpacecraftId, VirtualChannelId, CounterModulus, VirtualChannelFrameCount, Policy, RecoveryMode, PendingData, Events),
		split_aos_packet_zone_(FirstHeaderPointer, PacketZoneBytes, SecondaryHeaderLength, PacketZone),
		reassemble_discontinuity_packet_zone(RecoveryMode, PacketZone, SecondaryHeaderLength, PendingData, Packets, UpdatedPendingData),
		next_frame_count(VirtualChannelFrameCount, CounterModulus, ExpectedFrameCount),
		update_channel_state(State, aos, SpacecraftId, VirtualChannelId, CounterModulus, ExpectedFrameCount, UpdatedPendingData, UpdatedState).

	reassemble_channel_packet_zone(FrameType, SpacecraftId, VirtualChannelId, CounterModulus, VirtualChannelFrameCount, packet_zone_context(FirstHeaderPointer, PacketZoneBytes), SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events) :-
		channel_state(State, FrameType, SpacecraftId, VirtualChannelId, CounterModulus, VirtualChannelFrameCount, Policy, RecoveryMode, PendingData, Events),
		split_packet_zone(PacketZoneBytes, FirstHeaderPointer, SecondaryHeaderLength, PacketZone),
		reassemble_discontinuity_packet_zone(RecoveryMode, PacketZone, SecondaryHeaderLength, PendingData, Packets, UpdatedPendingData),
		next_frame_count(VirtualChannelFrameCount, CounterModulus, ExpectedFrameCount),
		update_channel_state(State, FrameType, SpacecraftId, VirtualChannelId, CounterModulus, ExpectedFrameCount, UpdatedPendingData, UpdatedState).

	channel_state(channel_reassembly_state(Channels), FrameType, SpacecraftId, VirtualChannelId, CounterModulus, VirtualChannelFrameCount, Policy, RecoveryMode, PendingData, Events) :-
		(   select_channel_entry(FrameType, SpacecraftId, VirtualChannelId, Channels, ExistingEntry, _) ->
			ExistingEntry = reassembly_channel(FrameType, SpacecraftId, VirtualChannelId, StoredCounterModulus, ExpectedFrameCount, StoredPendingData),
			channel_recovery_mode(Policy, FrameType, SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus, ExpectedFrameCount, VirtualChannelFrameCount, StoredPendingData, RecoveryMode, PendingData, Events)
		;   PendingData = [],
			RecoveryMode = normal,
			Events = []
		).

	channel_recovery_mode(_, _, _, _, CounterModulus, CounterModulus, ExpectedFrameCount, VirtualChannelFrameCount, PendingData, normal, PendingData, []) :-
		ExpectedFrameCount =:= VirtualChannelFrameCount,
		!.
	channel_recovery_mode(throw, FrameType, SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus, ExpectedFrameCount, VirtualChannelFrameCount, _, _, _, _) :-
		throw_sequence_error(FrameType, SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus, ExpectedFrameCount, VirtualChannelFrameCount).
	channel_recovery_mode(drop, FrameType, SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus, ExpectedFrameCount, VirtualChannelFrameCount, StoredPendingData, drop, [], Events) :-
		discontinuity_events(drop, FrameType, SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus, ExpectedFrameCount, VirtualChannelFrameCount, StoredPendingData, Events).
	channel_recovery_mode(resynchronize, FrameType, SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus, ExpectedFrameCount, VirtualChannelFrameCount, StoredPendingData, resynchronize, [], Events) :-
		discontinuity_events(resynchronize, FrameType, SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus, ExpectedFrameCount, VirtualChannelFrameCount, StoredPendingData, Events).

	discontinuity_events(Policy, FrameType, SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus, ExpectedFrameCount, VirtualChannelFrameCount, StoredPendingData, Events) :-
		Discontinuity = discontinuity(StoredCounterModulus, CounterModulus, ExpectedFrameCount, VirtualChannelFrameCount),
		dropped_fragment_event(FrameType, SpacecraftId, VirtualChannelId, Discontinuity, StoredPendingData, DroppedEvents),
		recovery_action_event(Policy, FrameType, SpacecraftId, VirtualChannelId, Discontinuity, ActionEvents),
		append(DroppedEvents, ActionEvents, Events).

	dropped_fragment_event(_, _, _, _, [], []) :-
		!.
	dropped_fragment_event(FrameType, SpacecraftId, VirtualChannelId, Reason, PendingData, [dropped_fragment(FrameType, SpacecraftId, VirtualChannelId, Reason, PendingData)]).

	recovery_action_event(drop, FrameType, SpacecraftId, VirtualChannelId, Discontinuity, [skipped_frame(FrameType, SpacecraftId, VirtualChannelId, Discontinuity)]).
	recovery_action_event(resynchronize, FrameType, SpacecraftId, VirtualChannelId, Discontinuity, [resynchronized(FrameType, SpacecraftId, VirtualChannelId, Discontinuity)]).

	idle_only_recovery_events(_, _, _, _, [], []) :-
		!.
	idle_only_recovery_events(FrameType, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, PendingData, [dropped_fragment(FrameType, SpacecraftId, VirtualChannelId, idle_only(VirtualChannelFrameCount), PendingData)]).

	reassemble_discontinuity_packet_zone(normal, PacketZone, SecondaryHeaderLength, PendingData, Packets, UpdatedPendingData) :-
		reassemble_packet_zone(PacketZone, SecondaryHeaderLength, packet_reassembly_state(PendingData), Packets, packet_reassembly_state(UpdatedPendingData)).
	reassemble_discontinuity_packet_zone(drop, _, _, _, [], []).
	reassemble_discontinuity_packet_zone(resynchronize, PacketZone, SecondaryHeaderLength, _, Packets, UpdatedPendingData) :-
		sanitize_packet_zone_after_discontinuity(PacketZone, SanitizedPacketZone),
		reassemble_packet_zone(SanitizedPacketZone, SecondaryHeaderLength, packet_reassembly_state([]), Packets, packet_reassembly_state(UpdatedPendingData)).

	sanitize_packet_zone_after_discontinuity(packet_zone(_, [], []), packet_zone([], [], [])) :-
		!.
	sanitize_packet_zone_after_discontinuity(packet_zone(_, Packets, SuffixData), packet_zone([], Packets, SuffixData)).

	update_channel_state(channel_reassembly_state(Channels), FrameType, SpacecraftId, VirtualChannelId, CounterModulus, ExpectedFrameCount, PendingData, channel_reassembly_state(UpdatedChannels)) :-
		(   select_channel_entry(FrameType, SpacecraftId, VirtualChannelId, Channels, _, OtherChannels) ->
			true
		;   OtherChannels = Channels
		),
		UpdatedChannels = [reassembly_channel(FrameType, SpacecraftId, VirtualChannelId, CounterModulus, ExpectedFrameCount, PendingData)| OtherChannels].

	select_channel_entry(FrameType, SpacecraftId, VirtualChannelId, [Entry| Entries], Entry, Entries) :-
		Entry = reassembly_channel(FrameType, SpacecraftId, VirtualChannelId, _, _, _),
		!.
	select_channel_entry(FrameType, SpacecraftId, VirtualChannelId, [Entry| Entries], SelectedEntry, [Entry| Rest]) :-
		select_channel_entry(FrameType, SpacecraftId, VirtualChannelId, Entries, SelectedEntry, Rest).

	throw_sequence_error(FrameType, SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus, _, _) :-
		StoredCounterModulus =\= CounterModulus,
		!,
		throw_counter_modulus_error(FrameType, SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus).
	throw_sequence_error(tm, SpacecraftId, VirtualChannelId, _, _, ExpectedFrameCount, VirtualChannelFrameCount) :-
		domain_error(ccsds_tm_transfer_frame_sequence, tm_transfer_frame_sequence(SpacecraftId, VirtualChannelId, ExpectedFrameCount, VirtualChannelFrameCount)).
	throw_sequence_error(aos, SpacecraftId, VirtualChannelId, _, _, ExpectedFrameCount, VirtualChannelFrameCount) :-
		domain_error(ccsds_aos_transfer_frame_sequence, aos_transfer_frame_sequence(SpacecraftId, VirtualChannelId, ExpectedFrameCount, VirtualChannelFrameCount)).

	throw_counter_modulus_error(aos, SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus) :-
		domain_error(ccsds_aos_transfer_frame_counter_modulus, aos_transfer_frame_counter_modulus(SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus)).
	throw_counter_modulus_error(tm, SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus) :-
		domain_error(ccsds_tm_transfer_frame_counter_modulus, tm_transfer_frame_counter_modulus(SpacecraftId, VirtualChannelId, StoredCounterModulus, CounterModulus)).

	next_frame_count(VirtualChannelFrameCount, CounterModulus, ExpectedFrameCount) :-
		ExpectedFrameCount is (VirtualChannelFrameCount + 1) mod CounterModulus.

	aos_counter_modulus(signaling_field(_, FrameCountUsageFlag, _, _), CounterModulus) :-
		(   FrameCountUsageFlag =:= 1 ->
			CounterModulus = 268435456
		;   CounterModulus = 16777216
		).

	pending_fragments_([reassembly_channel(FrameType, SpacecraftId, VirtualChannelId, _, _, PendingData)| Channels], [pending_fragment(FrameType, SpacecraftId, VirtualChannelId, PendingData)| PendingFragments]) :-
		PendingData \== [],
		!,
		pending_fragments_(Channels, PendingFragments).
	pending_fragments_([_| Channels], PendingFragments) :-
		pending_fragments_(Channels, PendingFragments).
	pending_fragments_([], []).

	join_aos_packet_zone_(PacketZone, SecondaryHeaderLength, PacketZoneBytes, FirstHeaderPointer) :-
		(   valid(PacketZone) ->
			true
		;   domain_error(ccsds_packet_zone, PacketZone)
		),
		PacketZone = packet_zone(PrefixData, Packets, SuffixData),
		(   Packets == [] ->
			join_empty_aos_packet_zone(PrefixData, SuffixData, PacketZone, PacketZoneBytes, FirstHeaderPointer)
		;   join_packet_zone(PacketZone, SecondaryHeaderLength, PacketZoneBytes, FirstHeaderPointer)
		).

	join_empty_aos_packet_zone([], SuffixData, _, SuffixData, 2046) :-
		!.
	join_empty_aos_packet_zone(PrefixData, [], _, PrefixData, 2047) :-
		!.
	join_empty_aos_packet_zone(_, _, PacketZone, _, _) :-
		domain_error(ccsds_aos_packet_zone, PacketZone).

	recover_complete_packet(Bytes, SecondaryHeaderLength, Packet) :-
		packet_total_length(Bytes, TotalLength),
		take(TotalLength, Bytes, PacketBytes, _),
		ccsds_packets(SecondaryHeaderLength)::parse(bytes(PacketBytes), [Packet]).

	packet_total_length(Bytes, TotalLength) :-
		take(6, Bytes, [_, _, _, _, Byte4, Byte5], _),
		DataLength is (Byte4 << 8) \/ Byte5,
		TotalLength is DataLength + 7,
		length(Bytes, Length),
		Length >= TotalLength.

	tm_packet_zone(tm_transfer_frame(_, _, _, _, _, _, _, _, _, _, FirstHeaderPointer, _, DataField, _, _), FirstHeaderPointer, DataField) :-
		!.
	tm_packet_zone(Frame, _, _) :-
		domain_error(ccsds_tm_transfer_frame_term, Frame).

	aos_packet_zone(aos_transfer_frame(_, _, _, _, _, _, DataField, _, _), DataField) :-
		!.
	aos_packet_zone(Frame, _) :-
		domain_error(ccsds_aos_transfer_frame_term, Frame).

	replace_tm_packet_zone(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, _, SecondaryHeader, _, OCF, FECF), FirstHeaderPointer, DataField, UpdatedFrame) :-
		refresh_tm_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, FECF), UpdatedFrame).

	replace_aos_packet_zone(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, _, OCF, FECF), DataField, UpdatedFrame) :-
		refresh_aos_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, FECF), UpdatedFrame).

	refresh_tm_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, none), tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, none)) :-
		!.
	refresh_tm_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, _), tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, FECF)) :-
		compute_tm_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, none), FECF).

	refresh_aos_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, none), aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, none)) :-
		!.
	refresh_aos_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, _), aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, FECF)) :-
		compute_aos_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, none), FECF).

	compute_tm_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, _), fecf(Bytes)) :-
		secondary_header_bytes(SecondaryHeader, SecondaryHeaderBytes),
		ocf_bytes(OCF, OCFBytes),
		Byte0 is ((Version /\ 0x03) << 6) \/ ((SpacecraftId >> 4) /\ 0x3F),
		Byte1 is ((SpacecraftId /\ 0x0F) << 4) \/ ((VirtualChannelId /\ 0x07) << 1) \/ (OCFFlag /\ 0x01),
		Byte2 is MasterChannelFrameCount,
		Byte3 is VirtualChannelFrameCount,
		Byte4 is ((SecondaryHeaderFlag /\ 0x01) << 7) \/ ((SynchronizationFlag /\ 0x01) << 6) \/ ((PacketOrderFlag /\ 0x01) << 5) \/ ((SegmentLengthIdentifier /\ 0x03) << 3) \/ ((FirstHeaderPointer >> 8) /\ 0x07),
		Byte5 is FirstHeaderPointer /\ 0xFF,
		FrameBytes = [Byte0, Byte1, Byte2, Byte3, Byte4, Byte5| Tail0],
		append(SecondaryHeaderBytes, Tail1, Tail0),
		append(DataField, Tail2, Tail1),
		append(OCFBytes, [], Tail2),
		compute_fecf_bytes(FrameBytes, Bytes).

	compute_aos_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, signaling_field(ReplayFlag, FrameCountUsageFlag, SpacecraftIdExtension, FrameCountCycle), InsertZone, DataField, OCF, _), fecf(Bytes)) :-
		insert_zone_bytes(InsertZone, InsertZoneBytes),
		ocf_bytes(OCF, OCFBytes),
		BaseFrameCount is VirtualChannelFrameCount /\ 0xFFFFFF,
		Byte0 is ((Version /\ 0x03) << 6) \/ ((SpacecraftId /\ 0x0FF) >> 2),
		Byte1 is ((SpacecraftId /\ 0x03) << 6) \/ (VirtualChannelId /\ 0x3F),
		Byte2 is (BaseFrameCount >> 16) /\ 0xFF,
		Byte3 is (BaseFrameCount >> 8) /\ 0xFF,
		Byte4 is BaseFrameCount /\ 0xFF,
		Byte5 is ((ReplayFlag /\ 0x01) << 7) \/ ((FrameCountUsageFlag /\ 0x01) << 6) \/ ((SpacecraftIdExtension /\ 0x03) << 4) \/ (FrameCountCycle /\ 0x0F),
		FrameBytes = [Byte0, Byte1, Byte2, Byte3, Byte4, Byte5| Tail0],
		append(InsertZoneBytes, Tail1, Tail0),
		append(DataField, Tail2, Tail1),
		append(OCFBytes, [], Tail2),
		compute_fecf_bytes(FrameBytes, Bytes).

	secondary_header_bytes(none, []).
	secondary_header_bytes(secondary_header(Bytes), Bytes).

	insert_zone_bytes(none, []).
	insert_zone_bytes(insert_zone(Bytes), Bytes).

	ocf_bytes(none, []).
	ocf_bytes(ocf(Bytes), Bytes).

	compute_fecf_bytes(FrameBytes, [HighByte, LowByte]) :-
		compute_fecf_crc(FrameBytes, 0xFFFF, CRC),
		HighByte is (CRC >> 8) /\ 0xFF,
		LowByte is CRC /\ 0xFF.

	compute_fecf_crc([Byte| Bytes], CRC0, CRC) :-
		CRC1 is xor(CRC0, Byte << 8) /\ 0xFFFF,
		compute_fecf_crc_bits(8, CRC1, CRC2),
		compute_fecf_crc(Bytes, CRC2, CRC).
	compute_fecf_crc([], CRC, CRC).

	compute_fecf_crc_bits(0, CRC, CRC).
	compute_fecf_crc_bits(Bit, CRC0, CRC) :-
		(   CRC0 /\ 0x8000 =:= 0 ->
			CRC1 is (CRC0 << 1) /\ 0xFFFF
		;   CRC1 is xor((CRC0 << 1) /\ 0xFFFF, 0x1021)
		),
		NextBit is Bit - 1,
		compute_fecf_crc_bits(NextBit, CRC1, CRC).

	decode_aos_packet_zone([Byte0, Byte1| PacketZoneBytes], FirstHeaderPointer, PacketZoneBytes) :-
		Reserved is (Byte0 >> 3) /\ 0x1F,
		(   Reserved =:= 0 ->
			FirstHeaderPointer is ((Byte0 /\ 0x07) << 8) \/ Byte1,
			valid_aos_first_header_pointer(FirstHeaderPointer, PacketZoneBytes)
		;   domain_error(ccsds_aos_packet_zone_bytes, [Byte0, Byte1| PacketZoneBytes])
		).

	encode_aos_packet_zone(FirstHeaderPointer, PacketZoneBytes, [Byte0, Byte1| PacketZoneBytes]) :-
		valid_aos_first_header_pointer(FirstHeaderPointer, PacketZoneBytes),
		Byte0 is (FirstHeaderPointer >> 8) /\ 0x07,
		Byte1 is FirstHeaderPointer /\ 0xFF.

	valid_packet_terms([]).
	valid_packet_terms([Packet| Packets]) :-
		valid_packet_term(Packet),
		valid_packet_terms(Packets).

	valid_packet_term(ccsds_packet(Version, Type, SecHeaderFlag, APID, SeqFlags, SeqCount, SecondaryHeader, UserData)) :-
		integer(Version), Version >= 0, Version =< 7,
		integer(Type), Type >= 0, Type =< 1,
		integer(SecHeaderFlag), SecHeaderFlag >= 0, SecHeaderFlag =< 1,
		integer(APID), APID >= 0, APID =< 2047,
		integer(SeqFlags), SeqFlags >= 0, SeqFlags =< 3,
		integer(SeqCount), SeqCount >= 0, SeqCount =< 16383,
		valid_secondary_header_term(SecondaryHeader),
		valid(list(byte), UserData).

	valid_secondary_header_term(none).
	valid_secondary_header_term(secondary_header(Bytes)) :-
		valid(list(byte), Bytes).

	valid_secondary_header_length(SecondaryHeaderLength) :-
		integer(SecondaryHeaderLength),
		SecondaryHeaderLength >= 0,
		!.
	valid_secondary_header_length(SecondaryHeaderLength) :-
		domain_error(ccsds_secondary_header_length, SecondaryHeaderLength).

	valid_packet_zone_bytes(Bytes) :-
		valid(list(byte), Bytes),
		!.
	valid_packet_zone_bytes(Bytes) :-
		domain_error(ccsds_packet_zone_bytes, Bytes).

	valid_aos_packet_zone_bytes([Byte0, Byte1| PacketZoneBytes]) :-
		valid(list(byte), [Byte0, Byte1| PacketZoneBytes]),
		!.
	valid_aos_packet_zone_bytes(Bytes) :-
		domain_error(ccsds_aos_packet_zone_bytes, Bytes).

	valid_packet_reassembly_state(packet_reassembly_state(PendingData)) :-
		valid(list(byte), PendingData),
		!.
	valid_packet_reassembly_state(State) :-
		domain_error(ccsds_packet_reassembly_state, State).

	valid_channel_reassembly_state_(channel_reassembly_state(Channels)) :-
		valid_reassembly_channels(Channels),
		!.
	valid_channel_reassembly_state_(State) :-
		domain_error(ccsds_channel_reassembly_state, State).

	valid_discontinuity_policy_(Policy) :-
		valid_discontinuity_policy(Policy),
		!.
	valid_discontinuity_policy_(Policy) :-
		domain_error(ccsds_discontinuity_policy, Policy).

	valid_reassembly_channels([Channel| Channels]) :-
		valid_reassembly_channel(Channel),
		no_duplicate_channel(Channel, Channels),
		valid_reassembly_channels(Channels).
	valid_reassembly_channels([]).

	valid_reassembly_channel(reassembly_channel(FrameType, SpacecraftId, VirtualChannelId, CounterModulus, ExpectedFrameCount, PendingData)) :-
		valid_frame_type(FrameType),
		integer(SpacecraftId),
		SpacecraftId >= 0,
		SpacecraftId =< 1023,
		integer(VirtualChannelId),
		VirtualChannelId >= 0,
		valid_virtual_channel_id(FrameType, VirtualChannelId),
		integer(CounterModulus),
		CounterModulus > 0,
		integer(ExpectedFrameCount),
		ExpectedFrameCount >= 0,
		ExpectedFrameCount < CounterModulus,
		valid(list(byte), PendingData).

	no_duplicate_channel(reassembly_channel(FrameType, SpacecraftId, VirtualChannelId, _, _, _), [reassembly_channel(FrameType, SpacecraftId, VirtualChannelId, _, _, _)| _]) :-
		!,
		fail.
	no_duplicate_channel(Channel, [_| Channels]) :-
		!,
		no_duplicate_channel(Channel, Channels).
	no_duplicate_channel(_, []).

	valid_frame_type(tm).
	valid_frame_type(aos).

	valid_virtual_channel_id(tm, VirtualChannelId) :-
		VirtualChannelId =< 7.
	valid_virtual_channel_id(aos, VirtualChannelId) :-
		VirtualChannelId =< 63.

	valid_first_header_pointer(2047, _) :-
		!.
	valid_first_header_pointer(FirstHeaderPointer, Bytes) :-
		integer(FirstHeaderPointer),
		FirstHeaderPointer >= 0,
		FirstHeaderPointer =< 2047,
		length(Bytes, Length),
		FirstHeaderPointer =< Length,
		!.
	valid_first_header_pointer(FirstHeaderPointer, _) :-
		domain_error(ccsds_first_header_pointer, FirstHeaderPointer).

	valid_aos_first_header_pointer(2046, _) :-
		!.
	valid_aos_first_header_pointer(2047, _) :-
		!.
	valid_aos_first_header_pointer(FirstHeaderPointer, Bytes) :-
		integer(FirstHeaderPointer),
		FirstHeaderPointer >= 0,
		FirstHeaderPointer =< 2045,
		length(Bytes, Length),
		FirstHeaderPointer =< Length,
		!.
	valid_aos_first_header_pointer(FirstHeaderPointer, _) :-
		domain_error(ccsds_aos_first_header_pointer, FirstHeaderPointer).

:- end_object.
