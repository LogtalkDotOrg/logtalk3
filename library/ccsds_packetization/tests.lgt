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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-09,
		comment is 'Unit tests for the "ccsds_packetization" library.'
	]).

	:- uses(ccsds_packetization, [
		initial_state/1, pending_packets/2,
		packetize_tm_packets/6, packetize_tm_packets/7,
		packetize_aos_packets/6,
		packetize_tm_frames/7,
		generate_idle_packet/4
	]).

	:- uses(ccsds_packet_services, [
		extract_tm_packets/3,
		extract_aos_packets/3
	]).

	cover(ccsds_packetization).

	test(ccsds_packetization_initial_state_1_01, deterministic(State == packetizer_state([]))) :-
		initial_state(State).

	test(ccsds_packetization_pending_packets_2_01, deterministic(PendingPackets == [])) :-
		initial_state(State),
		pending_packets(State, PendingPackets).

	test(ccsds_packetization_generate_idle_packet_4_01, deterministic(Packet == ccsds_packet(0, 0, 0, 2047, 3, 7, none, [0x00, 0x00]))) :-
		generate_idle_packet(0, 7, 2, Packet).

	test(ccsds_packetization_generate_idle_packet_4_02, deterministic(Packet == ccsds_packet(0, 0, 0, 2047, 3, 9, secondary_header([0x00, 0x00]), [0x00]))) :-
		generate_idle_packet(2, 9, 1, Packet).

	test(ccsds_packetization_packetize_tm_packets_6_01, deterministic((PacketZone == packet_zone([], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], []), UpdatedState == packetizer_state([])))) :-
		initial_state(State),
		packetize_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 2047, none, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], none, none), 0, State, [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], UpdatedFrame, UpdatedState),
		extract_tm_packets(UpdatedFrame, 0, PacketZone).

	test(ccsds_packetization_packetize_tm_packets_6_02, deterministic((PacketZone == packet_zone([], [], [0x00, 0x00, 0xC0, 0x00, 0x00, 0x03, 0x11]), PendingPackets == [packetizer_channel(tm, 42, 3, [0x22, 0x33, 0x44], [])]))) :-
		initial_state(State),
		packetize_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 2047, none, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], none, none), 0, State, [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x11, 0x22, 0x33, 0x44])], UpdatedFrame, UpdatedState),
		extract_tm_packets(UpdatedFrame, 0, PacketZone),
		pending_packets(UpdatedState, PendingPackets).

	test(ccsds_packetization_packetize_tm_packets_6_03, deterministic((PacketZone == packet_zone([0x22, 0x33, 0x44], [], []), UpdatedState == packetizer_state([])))) :-
		packetize_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 33, 0, 0, 0, 3, 2047, none, [0x00, 0x00, 0x00], none, none), 0, packetizer_state([packetizer_channel(tm, 42, 3, [0x22, 0x33, 0x44], [])]), [], UpdatedFrame, UpdatedState),
		extract_tm_packets(UpdatedFrame, 0, PacketZone).

	test(ccsds_packetization_packetize_tm_packets_7_01, deterministic((PacketZone == packet_zone([], [ccsds_packet(0, 0, 0, 2047, 3, 0, none, [0x00])], []), Events == [generated_idle_packet(tm, 42, 3, ccsds_packet(0, 0, 0, 2047, 3, 0, none, [0x00]))]))) :-
		initial_state(State),
		packetize_tm_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 2047, none, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], none, none), 0, State, [], UpdatedFrame, _, Events),
		extract_tm_packets(UpdatedFrame, 0, PacketZone).

	test(ccsds_packetization_packetize_aos_packets_6_01, deterministic((PacketZone == packet_zone([], [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], []), UpdatedState == packetizer_state([])))) :-
		initial_state(State),
		packetize_aos_packets(aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], none, none), 0, State, [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])], UpdatedFrame, UpdatedState),
		extract_aos_packets(UpdatedFrame, 0, PacketZone).

	test(ccsds_packetization_packetize_tm_frames_7_01, deterministic((UpdatedFrames == [tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00, 0x00, 0xC0, 0x00, 0x00, 0x03, 0x11], none, none), tm_transfer_frame(0, 42, 3, 0, 16, 33, 0, 0, 0, 3, 2047, none, [0x22, 0x33, 0x44], none, none)], RemainingPackets == [], UpdatedState == packetizer_state([])))) :-
		initial_state(State),
		packetize_tm_frames([
			tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 2047, none, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], none, none),
			tm_transfer_frame(0, 42, 3, 0, 16, 33, 0, 0, 0, 3, 2047, none, [0x00, 0x00, 0x00], none, none)
		], 0, State, [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x11, 0x22, 0x33, 0x44])], UpdatedFrames, RemainingPackets, UpdatedState).

:- end_object.
