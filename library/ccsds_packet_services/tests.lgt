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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-05-08,
		comment is 'Unit tests for the "ccsds_packet_services" library.'
	]).

	:- uses(ccsds_packet_services, [
		valid/1, prefix_data/2, packets/2, suffix_data/2, split_packet_zone/4, join_packet_zone/4,
		valid_reassembly_state/1, initial_reassembly_state/1, pending_data/2, reassemble_packet_zone/5,
		valid_channel_reassembly_state/1, initial_channel_reassembly_state/1, pending_fragments/2,
		valid_discontinuity_policy/1,
		extract_tm_packets/3, insert_tm_packets/4,
		reassemble_tm_packets/5, reassemble_tm_packets/6, reassemble_tm_packets/7, reassemble_tm_frames/5, reassemble_tm_frames/6, reassemble_tm_frames/7,
		split_aos_packet_zone/3, join_aos_packet_zone/3, extract_aos_packets/3, insert_aos_packets/4,
		reassemble_aos_packets/5, reassemble_aos_packets/6, reassemble_aos_packets/7, reassemble_aos_frames/5, reassemble_aos_frames/6, reassemble_aos_frames/7
	]).

	cover(ccsds_packet_services).

	test(ccsds_packet_services_valid_1_01, deterministic) :-
		valid(packet_zone([0xAA], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], [0xBB])).

	test(ccsds_packet_services_prefix_data_2_01, deterministic(PrefixData == [0xAA])) :-
		prefix_data(packet_zone([0xAA], [], [0xBB]), PrefixData).

	test(ccsds_packet_services_packets_2_01, deterministic(Packets == [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])])) :-
		packets(packet_zone([], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], []), Packets).

	test(ccsds_packet_services_suffix_data_2_01, deterministic(SuffixData == [0xBB])) :-
		suffix_data(packet_zone([0xAA], [], [0xBB]), SuffixData).

	test(ccsds_packet_services_split_packet_zone_4_01, deterministic(PacketZone == packet_zone([], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], []))) :-
		split_packet_zone([0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42], 0, 0, PacketZone).

	test(ccsds_packet_services_split_packet_zone_4_02, deterministic(PacketZone == packet_zone([0xAA, 0xBB], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], [0xCC]))) :-
		split_packet_zone([0xAA, 0xBB, 0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42, 0xCC], 2, 0, PacketZone).

	test(ccsds_packet_services_split_packet_zone_4_03, deterministic(PacketZone == packet_zone([], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42]), ccsds_packet(0, 0, 0, 0, 3, 1, none, [0x43])], []))) :-
		split_packet_zone([0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42, 0x00, 0x00, 0xC0, 0x01, 0x00, 0x00, 0x43], 0, 0, PacketZone).

	test(ccsds_packet_services_split_packet_zone_4_04, deterministic(PacketZone == packet_zone([0xAA, 0xBB, 0xCC], [], []))) :-
		split_packet_zone([0xAA, 0xBB, 0xCC], 2047, 0, PacketZone).

	test(ccsds_packet_services_split_packet_zone_4_05, error(domain_error(ccsds_first_header_pointer, _))) :-
		split_packet_zone([0xAA, 0xBB, 0xCC], 4, 0, _).

	test(ccsds_packet_services_join_packet_zone_4_01, deterministic((Bytes == [0xAA, 0xBB, 0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42, 0xCC], FirstHeaderPointer == 2))) :-
		join_packet_zone(packet_zone([0xAA, 0xBB], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], [0xCC]), 0, Bytes, FirstHeaderPointer).

	test(ccsds_packet_services_join_packet_zone_4_02, deterministic((Bytes == [0xAA, 0xBB, 0xCC], FirstHeaderPointer == 2047))) :-
		join_packet_zone(packet_zone([0xAA, 0xBB], [], [0xCC]), 0, Bytes, FirstHeaderPointer).

	test(ccsds_packet_services_valid_reassembly_state_1_01, deterministic) :-
		valid_reassembly_state(packet_reassembly_state([0xAA, 0xBB])).

	test(ccsds_packet_services_initial_reassembly_state_1_01, deterministic(State == packet_reassembly_state([]))) :-
		initial_reassembly_state(State).

	test(ccsds_packet_services_pending_data_2_01, deterministic(Bytes == [0xAA, 0xBB])) :-
		pending_data(packet_reassembly_state([0xAA, 0xBB]), Bytes).

	test(ccsds_packet_services_reassemble_packet_zone_5_01, deterministic((Packets == [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42]), ccsds_packet(0, 0, 0, 0, 3, 1, none, [0x43])], UpdatedState == packet_reassembly_state([])))) :-
		reassemble_packet_zone(packet_zone([0x00, 0x00, 0x42], [ccsds_packet(0, 0, 0, 0, 3, 1, none, [0x43])], []), 0, packet_reassembly_state([0x00, 0x00, 0xC0, 0x00]), Packets, UpdatedState).

	test(ccsds_packet_services_reassemble_packet_zone_5_02, deterministic((Packets == [], UpdatedState == packet_reassembly_state([0xAA, 0xBB, 0xCC])))) :-
		reassemble_packet_zone(packet_zone([0xAA, 0xBB, 0xCC], [], []), 0, packet_reassembly_state([]), Packets, UpdatedState).

	test(ccsds_packet_services_reassemble_packet_zone_5_03, deterministic((Packets == [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], UpdatedState == packet_reassembly_state([])))) :-
		reassemble_packet_zone(packet_zone([0xAA, 0xBB], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], []), 0, packet_reassembly_state([]), Packets, UpdatedState).

	test(ccsds_packet_services_reassemble_packet_zone_5_04, error(domain_error(ccsds_packet_reassembly_continuation, _))) :-
		reassemble_packet_zone(packet_zone([], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], []), 0, packet_reassembly_state([0x00, 0x00, 0xC0, 0x00]), _, _).

	test(ccsds_packet_services_valid_channel_reassembly_state_1_01, deterministic) :-
		valid_channel_reassembly_state(channel_reassembly_state([reassembly_channel(tm, 42, 3, 256, 33, [0x00, 0x00, 0xC0, 0x00])])).

	test(ccsds_packet_services_initial_channel_reassembly_state_1_01, deterministic(State == channel_reassembly_state([]))) :-
		initial_channel_reassembly_state(State).

	test(ccsds_packet_services_pending_fragments_2_01, deterministic(Pending == [pending_fragment(tm, 42, 3, [0xAA, 0xBB])])) :-
		pending_fragments(channel_reassembly_state([reassembly_channel(tm, 42, 3, 256, 9, [0xAA, 0xBB]), reassembly_channel(aos, 42, 4, 16777216, 10, [])]), Pending).

	test(ccsds_packet_services_valid_discontinuity_policy_1_01, deterministic) :-
		valid_discontinuity_policy(throw),
		valid_discontinuity_policy(drop),
		valid_discontinuity_policy(resynchronize).

	test(ccsds_packet_services_extract_tm_packets_3_01, deterministic(PacketZone == packet_zone([], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], []))) :-
		extract_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42], none, none), 0, PacketZone).

	test(ccsds_packet_services_extract_tm_packets_3_02, deterministic(PacketZone == packet_zone([0xAA, 0xBB], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], [0xCC]))) :-
		extract_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 2, none, [0xAA, 0xBB, 0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42, 0xCC], none, none), 0, PacketZone).

	test(ccsds_packet_services_insert_tm_packets_4_01, deterministic(UpdatedFrame == tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 2, none, [0xAA, 0xBB, 0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42, 0xCC], none, none))) :-
		insert_tm_packets(packet_zone([0xAA, 0xBB], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], [0xCC]), 0, tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [], none, none), UpdatedFrame).

	test(ccsds_packet_services_insert_tm_packets_4_02, deterministic(UpdatedFrame == tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 2047, none, [0xAA, 0xBB, 0xCC], none, none))) :-
		insert_tm_packets(packet_zone([0xAA, 0xBB], [], [0xCC]), 0, tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [], none, none), UpdatedFrame).

	test(ccsds_packet_services_reassemble_tm_packets_5_01, deterministic((Packets == [], UpdatedState == channel_reassembly_state([reassembly_channel(tm, 42, 3, 256, 33, [0x00, 0x00, 0xC0, 0x00])])))) :-
		initial_channel_reassembly_state(State),
		reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00, 0x00, 0xC0, 0x00], none, none), 0, State, Packets, UpdatedState).

	test(ccsds_packet_services_reassemble_tm_packets_5_02, deterministic((Packets == [ccsds_packet(0, 0, 0, 0, 3, 2, none, [0x44])], Pending == [pending_fragment(tm, 42, 3, [0x00, 0x00, 0xC0, 0x00])]))) :-
		initial_channel_reassembly_state(State0),
		reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00, 0x00, 0xC0, 0x00], none, none), 0, State0, [], State1),
		reassemble_tm_packets(tm_transfer_frame(0, 42, 4, 0, 16, 10, 0, 0, 0, 3, 0, none, [0x00, 0x00, 0xC0, 0x02, 0x00, 0x00, 0x44], none, none), 0, State1, Packets, State2),
		pending_fragments(State2, Pending).

	test(ccsds_packet_services_reassemble_tm_frames_5_01, deterministic((Packets == [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42]), ccsds_packet(0, 0, 0, 0, 3, 1, none, [0x43])], Pending == []))) :-
		initial_channel_reassembly_state(State),
		reassemble_tm_frames([
			tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00, 0x00, 0xC0, 0x00], none, none),
			tm_transfer_frame(0, 42, 3, 0, 16, 33, 0, 0, 0, 3, 3, none, [0x00, 0x00, 0x42, 0x00, 0x00, 0xC0, 0x01, 0x00, 0x00, 0x43], none, none)
		], 0, State, Packets, UpdatedState),
		pending_fragments(UpdatedState, Pending).

	test(ccsds_packet_services_reassemble_tm_packets_5_03, error(domain_error(ccsds_tm_transfer_frame_sequence, _))) :-
		initial_channel_reassembly_state(State0),
		reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00, 0x00, 0xC0, 0x00], none, none), 0, State0, [], State1),
		reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 34, 0, 0, 0, 3, 3, none, [0x00, 0x00, 0x42], none, none), 0, State1, _, _).

	test(ccsds_packet_services_reassemble_tm_packets_6_01, deterministic((Packets == [], UpdatedState == channel_reassembly_state([reassembly_channel(tm, 42, 3, 256, 35, [])])))) :-
		initial_channel_reassembly_state(State0),
		reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00, 0x00, 0xC0, 0x00], none, none), 0, State0, [], State1),
		reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 34, 0, 0, 0, 3, 3, none, [0x00, 0x00, 0x42, 0x00, 0x00, 0xC0, 0x02, 0x00, 0x00, 0x44], none, none), 0, drop, State1, Packets, UpdatedState).

	test(ccsds_packet_services_reassemble_tm_packets_6_02, deterministic((Packets == [ccsds_packet(0, 0, 0, 0, 3, 2, none, [0x44])], UpdatedState == channel_reassembly_state([reassembly_channel(tm, 42, 3, 256, 35, [])])))) :-
		initial_channel_reassembly_state(State0),
		reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00, 0x00, 0xC0, 0x00], none, none), 0, State0, [], State1),
		reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 34, 0, 0, 0, 3, 3, none, [0x00, 0x00, 0x42, 0x00, 0x00, 0xC0, 0x02, 0x00, 0x00, 0x44], none, none), 0, resynchronize, State1, Packets, UpdatedState).

	test(ccsds_packet_services_reassemble_tm_packets_7_01, deterministic((Packets == [], UpdatedState == channel_reassembly_state([reassembly_channel(tm, 42, 3, 256, 35, [])]), Events == [dropped_fragment(tm, 42, 3, discontinuity(256, 256, 33, 34), [0x00, 0x00, 0xC0, 0x00]), skipped_frame(tm, 42, 3, discontinuity(256, 256, 33, 34))]))) :-
		initial_channel_reassembly_state(State0),
		reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00, 0x00, 0xC0, 0x00], none, none), 0, State0, [], State1),
		reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 34, 0, 0, 0, 3, 3, none, [0x00, 0x00, 0x42, 0x00, 0x00, 0xC0, 0x02, 0x00, 0x00, 0x44], none, none), 0, drop, State1, Packets, UpdatedState, Events).

	test(ccsds_packet_services_reassemble_tm_frames_7_01, deterministic((Packets == [ccsds_packet(0, 0, 0, 0, 3, 2, none, [0x44])], UpdatedState == channel_reassembly_state([reassembly_channel(tm, 42, 3, 256, 35, [])]), Events == [dropped_fragment(tm, 42, 3, discontinuity(256, 256, 33, 34), [0x00, 0x00, 0xC0, 0x00]), resynchronized(tm, 42, 3, discontinuity(256, 256, 33, 34))]))) :-
		initial_channel_reassembly_state(State),
		reassemble_tm_frames([
			tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00, 0x00, 0xC0, 0x00], none, none),
			tm_transfer_frame(0, 42, 3, 0, 16, 34, 0, 0, 0, 3, 3, none, [0x00, 0x00, 0x42, 0x00, 0x00, 0xC0, 0x02, 0x00, 0x00, 0x44], none, none)
		], 0, resynchronize, State, Packets, UpdatedState, Events).

	test(ccsds_packet_services_split_aos_packet_zone_3_01, deterministic(PacketZone == packet_zone([], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], []))) :-
		split_aos_packet_zone([0x00, 0x00, 0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42], 0, PacketZone).

	test(ccsds_packet_services_split_aos_packet_zone_3_02, deterministic(PacketZone == packet_zone([0xAA, 0xBB], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], [0xCC]))) :-
		split_aos_packet_zone([0x00, 0x02, 0xAA, 0xBB, 0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42, 0xCC], 0, PacketZone).

	test(ccsds_packet_services_split_aos_packet_zone_3_03, deterministic(PacketZone == packet_zone([], [], [0xAA, 0xBB, 0xCC]))) :-
		split_aos_packet_zone([0x07, 0xFE, 0xAA, 0xBB, 0xCC], 0, PacketZone).

	test(ccsds_packet_services_split_aos_packet_zone_3_04, deterministic(PacketZone == packet_zone([0xAA, 0xBB, 0xCC], [], []))) :-
		split_aos_packet_zone([0x07, 0xFF, 0xAA, 0xBB, 0xCC], 0, PacketZone).

	test(ccsds_packet_services_split_aos_packet_zone_3_05, error(domain_error(ccsds_aos_packet_zone_bytes, _))) :-
		split_aos_packet_zone([0xF8, 0x00, 0xAA], 0, _).

	test(ccsds_packet_services_join_aos_packet_zone_3_01, deterministic(Bytes == [0x00, 0x02, 0xAA, 0xBB, 0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42, 0xCC])) :-
		join_aos_packet_zone(packet_zone([0xAA, 0xBB], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], [0xCC]), 0, Bytes).

	test(ccsds_packet_services_join_aos_packet_zone_3_02, deterministic(Bytes == [0x07, 0xFE, 0xAA, 0xBB, 0xCC])) :-
		join_aos_packet_zone(packet_zone([], [], [0xAA, 0xBB, 0xCC]), 0, Bytes).

	test(ccsds_packet_services_join_aos_packet_zone_3_03, deterministic(Bytes == [0x07, 0xFF, 0xAA, 0xBB, 0xCC])) :-
		join_aos_packet_zone(packet_zone([0xAA, 0xBB, 0xCC], [], []), 0, Bytes).

	test(ccsds_packet_services_join_aos_packet_zone_3_04, error(domain_error(ccsds_aos_packet_zone, _))) :-
		join_aos_packet_zone(packet_zone([0xAA], [], [0xBB]), 0, _).

	test(ccsds_packet_services_extract_aos_packets_3_01, deterministic(PacketZone == packet_zone([], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], []))) :-
		extract_aos_packets(aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00, 0x00, 0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42], none, none), 0, PacketZone).

	test(ccsds_packet_services_extract_aos_packets_3_02, deterministic(PacketZone == packet_zone([], [], [0xAA, 0xBB, 0xCC]))) :-
		extract_aos_packets(aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x07, 0xFE, 0xAA, 0xBB, 0xCC], none, none), 0, PacketZone).

	test(ccsds_packet_services_insert_aos_packets_4_01, deterministic(UpdatedFrame == aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00, 0x02, 0xAA, 0xBB, 0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42, 0xCC], none, none))) :-
		insert_aos_packets(packet_zone([0xAA, 0xBB], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], [0xCC]), 0, aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [], none, none), UpdatedFrame).

	test(ccsds_packet_services_insert_aos_packets_4_02, deterministic(UpdatedFrame == aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x07, 0xFE, 0xAA, 0xBB, 0xCC], none, none))) :-
		insert_aos_packets(packet_zone([], [], [0xAA, 0xBB, 0xCC]), 0, aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [], none, none), UpdatedFrame).

	test(ccsds_packet_services_reassemble_aos_packets_5_01, deterministic((Packets == [], Pending == [pending_fragment(aos, 42, 3, [0x00, 0x00, 0xC0, 0x00])]))) :-
		initial_channel_reassembly_state(State),
		reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00, 0x00, 0x00, 0x00, 0xC0, 0x00], none, none), 0, State, Packets, UpdatedState),
		pending_fragments(UpdatedState, Pending).

	test(ccsds_packet_services_reassemble_aos_packets_5_02, deterministic((Packets == [ccsds_packet(0, 0, 0, 0, 3, 2, none, [0x44])], Pending == [pending_fragment(tm, 42, 3, [0x00, 0x00, 0xC0, 0x00])]))) :-
		initial_channel_reassembly_state(State0),
		reassemble_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00, 0x00, 0xC0, 0x00], none, none), 0, State0, [], State1),
		reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00, 0x00, 0x00, 0x00, 0xC0, 0x02, 0x00, 0x00, 0x44], none, none), 0, State1, Packets, State2),
		pending_fragments(State2, Pending).

	test(ccsds_packet_services_reassemble_aos_packets_5_03, deterministic(Pending == [])) :-
		initial_channel_reassembly_state(State0),
		reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00, 0x00, 0x00, 0x00, 0xC0, 0x00], none, none), 0, State0, [], State1),
		reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 17, signaling_field(0, 0, 0, 0), none, [0x07, 0xFE, 0xAA, 0xBB], none, none), 0, State1, [], State2),
		pending_fragments(State2, Pending).

	test(ccsds_packet_services_reassemble_aos_frames_5_01, deterministic((Packets == [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42]), ccsds_packet(0, 0, 0, 0, 3, 1, none, [0x43])], Pending == []))) :-
		initial_channel_reassembly_state(State),
		reassemble_aos_frames([
			aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00, 0x00, 0x00, 0x00, 0xC0, 0x00], none, none),
			aos_transfer_frame(1, 42, 3, 17, signaling_field(0, 0, 0, 0), none, [0x00, 0x03, 0x00, 0x00, 0x42, 0x00, 0x00, 0xC0, 0x01, 0x00, 0x00, 0x43], none, none)
		], 0, State, Packets, UpdatedState),
		pending_fragments(UpdatedState, Pending).

	test(ccsds_packet_services_reassemble_aos_packets_5_04, error(domain_error(ccsds_aos_transfer_frame_sequence, _))) :-
		initial_channel_reassembly_state(State0),
		reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00, 0x00, 0x00, 0x00, 0xC0, 0x00], none, none), 0, State0, [], State1),
		reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 18, signaling_field(0, 0, 0, 0), none, [0x00, 0x03, 0x00, 0x00, 0x42], none, none), 0, State1, _, _).

	test(ccsds_packet_services_reassemble_aos_packets_6_01, deterministic((Packets == [], UpdatedState == channel_reassembly_state([reassembly_channel(aos, 42, 3, 16777216, 19, [])])))) :-
		initial_channel_reassembly_state(State0),
		reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00, 0x00, 0x00, 0x00, 0xC0, 0x00], none, none), 0, State0, [], State1),
		reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 18, signaling_field(0, 0, 0, 0), none, [0x00, 0x03, 0x00, 0x00, 0x42, 0x00, 0x00, 0xC0, 0x02, 0x00, 0x00, 0x44], none, none), 0, drop, State1, Packets, UpdatedState).

	test(ccsds_packet_services_reassemble_aos_packets_6_02, deterministic((Packets == [ccsds_packet(0, 0, 0, 0, 3, 2, none, [0x44])], UpdatedState == channel_reassembly_state([reassembly_channel(aos, 42, 3, 16777216, 19, [])])))) :-
		initial_channel_reassembly_state(State0),
		reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00, 0x00, 0x00, 0x00, 0xC0, 0x00], none, none), 0, State0, [], State1),
		reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 18, signaling_field(0, 0, 0, 0), none, [0x00, 0x03, 0x00, 0x00, 0x42, 0x00, 0x00, 0xC0, 0x02, 0x00, 0x00, 0x44], none, none), 0, resynchronize, State1, Packets, UpdatedState).

	test(ccsds_packet_services_reassemble_aos_packets_7_01, deterministic((Packets == [], UpdatedState == channel_reassembly_state([reassembly_channel(aos, 42, 3, 16777216, 18, [])]), Events == [dropped_fragment(aos, 42, 3, idle_only(17), [0x00, 0x00, 0xC0, 0x00])]))) :-
		initial_channel_reassembly_state(State0),
		reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00, 0x00, 0x00, 0x00, 0xC0, 0x00], none, none), 0, State0, [], State1),
		reassemble_aos_packets(aos_transfer_frame(1, 42, 3, 17, signaling_field(0, 0, 0, 0), none, [0x07, 0xFE, 0xAA, 0xBB], none, none), 0, throw, State1, Packets, UpdatedState, Events).

	test(ccsds_packet_services_reassemble_aos_frames_7_01, deterministic((Packets == [], UpdatedState == channel_reassembly_state([reassembly_channel(aos, 42, 3, 16777216, 19, [])]), Events == [dropped_fragment(aos, 42, 3, discontinuity(16777216, 16777216, 17, 18), [0x00, 0x00, 0xC0, 0x00]), skipped_frame(aos, 42, 3, discontinuity(16777216, 16777216, 17, 18))]))) :-
		initial_channel_reassembly_state(State),
		reassemble_aos_frames([
			aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00, 0x00, 0x00, 0x00, 0xC0, 0x00], none, none),
			aos_transfer_frame(1, 42, 3, 18, signaling_field(0, 0, 0, 0), none, [0x00, 0x03, 0x00, 0x00, 0x42, 0x00, 0x00, 0xC0, 0x02, 0x00, 0x00, 0x44], none, none)
		], 0, drop, State, Packets, UpdatedState, Events).

:- end_object.
