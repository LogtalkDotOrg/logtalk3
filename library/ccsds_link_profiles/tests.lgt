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
		date is 2026-07-20,
		comment is 'Unit tests for the "ccsds_link_profiles" library.'
	]).

	:- uses(ccsds_link_profiles, [
		valid_profile/1,
		parse_frame/3,
		parse_frames/3,
		generate_frame/3,
		generate_frames/3,
		valid_reassembly_state/1,
		initial_reassembly_state/1,
		pending_fragments/2,
		valid_discontinuity_policy/1,
		extract_packets/4,
		insert_packets/5,
		reassemble_packets/6,
		reassemble_frames/8
	]).

	cover(ccsds_link_profiles).

	test(ccsds_link_profiles_valid_profile_1_01, deterministic) :-
		valid_profile(tm_profile(16, 0, false)).

	test(ccsds_link_profiles_valid_profile_1_02, deterministic) :-
		valid_profile(tc_profile(10, 0, true)).

	test(ccsds_link_profiles_valid_profile_1_03, deterministic) :-
		valid_profile(aos_profile(12, 0, false, false)).

	test(ccsds_link_profiles_valid_profile_1_04, fail) :-
		valid_profile(aos_profile(12, -1, false, false)).

	test(ccsds_link_profiles_parse_frame_3_01, deterministic(Frame == tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none))) :-
		parse_frame(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 1, 2, 3, 4, 5, 6, 0xDE, 0xAD, 0xBE, 0xEF]), tm_profile(16, 0, false), Frame).

	test(ccsds_link_profiles_parse_frame_3_02, deterministic(Frame == tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D])))) :-
		parse_frame(bytes([0x20, 0x2A, 0x0C, 0x09, 0x07, 1, 2, 3, 0x44, 0x6D]), tc_profile(10, 0, true), Frame).

	test(ccsds_link_profiles_parse_frame_3_03, deterministic(Frame == aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [1,2,3,4,5,6], none, none))) :-
		parse_frame(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 1, 2, 3, 4, 5, 6]), aos_profile(12, 0, false, false), Frame).

	test(ccsds_link_profiles_parse_frame_3_04, error(domain_error(ccsds_single_frame_source, _))) :-
		parse_frame(bytes([
			0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 1, 2, 3, 4, 5, 6, 0xDE, 0xAD, 0xBE, 0xEF,
			0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 1, 2, 3, 4, 5, 6, 0xDE, 0xAD, 0xBE, 0xEF
		]), tm_profile(16, 0, false), _).

	test(ccsds_link_profiles_parse_frames_3_01, deterministic(Frames == [
		tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none),
		tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none)
	])) :-
		parse_frames(bytes([
			0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 1, 2, 3, 4, 5, 6, 0xDE, 0xAD, 0xBE, 0xEF,
			0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 1, 2, 3, 4, 5, 6, 0xDE, 0xAD, 0xBE, 0xEF
		]), tm_profile(16, 0, false), Frames).

	test(ccsds_link_profiles_generate_frame_3_01, deterministic(Bytes == [0x20, 0x2A, 0x0C, 0x09, 0x07, 1, 2, 3, 0x44, 0x6D])) :-
		generate_frame(bytes(Bytes), tc_profile(10, 0, true), tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D]))).

	test(ccsds_link_profiles_generate_frames_3_01, deterministic(Bytes == [0x20, 0x2A, 0x0C, 0x09, 0x07, 1, 2, 3, 0x44, 0x6D, 0x20, 0x2A, 0x0C, 0x09, 0x07, 1, 2, 3, 0x44, 0x6D])) :-
		generate_frames(bytes(Bytes), tc_profile(10, 0, true), [
			tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D])),
			tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D]))
		]).

	test(ccsds_link_profiles_valid_reassembly_state_1_01, deterministic) :-
		valid_reassembly_state(channel_reassembly_state([])).

	test(ccsds_link_profiles_initial_reassembly_state_1_01, deterministic(State == channel_reassembly_state([]))) :-
		initial_reassembly_state(State).

	test(ccsds_link_profiles_valid_discontinuity_policy_1_01, deterministic) :-
		valid_discontinuity_policy(throw),
		valid_discontinuity_policy(drop),
		valid_discontinuity_policy(resynchronize).

	test(ccsds_link_profiles_extract_packets_4_01, deterministic(PacketZone == packet_zone([], [ccsds_packet(0, 0, 1, 1, 3, 0, none, [0xDE,0xAD,0xBE,0xEF])], []))) :-
		extract_packets(tm_profile(16, 0, false), 0, tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x08,0x01,0xC0,0x00,0x00,0x03,0xDE,0xAD,0xBE,0xEF], none, none), PacketZone).

	test(ccsds_link_profiles_extract_packets_4_02, deterministic(PacketZone == packet_zone([], [ccsds_packet(0, 0, 1, 1, 3, 0, none, [0xDE,0xAD,0xBE,0xEF])], []))) :-
		extract_packets(aos_profile(18, 0, false, false), 0, aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [0x00,0x00,0x08,0x01,0xC0,0x00,0x00,0x03,0xDE,0xAD,0xBE,0xEF], none, none), PacketZone).

	test(ccsds_link_profiles_extract_packets_4_03, error(domain_error(ccsds_packet_link_profile, _))) :-
		extract_packets(tc_profile(10, 0, true), 0, tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D])), _).

	test(ccsds_link_profiles_insert_packets_5_01, deterministic(UpdatedFrame == tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 2, none, [0xAA,0xBB,0x08,0x01,0xC0,0x00,0x00,0x03,0xDE,0xAD,0xBE,0xEF], none, none))) :-
		insert_packets(tm_profile(18, 0, false), 0, packet_zone([0xAA,0xBB], [ccsds_packet(0, 0, 1, 1, 3, 0, none, [0xDE,0xAD,0xBE,0xEF])], []), tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0,0,0,0,0,0,0,0,0,0,0,0], none, none), UpdatedFrame).

	test(ccsds_link_profiles_insert_packets_5_02, deterministic(UpdatedFrame == aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [0x00,0x02,0xAA,0xBB,0x08,0x01,0xC0,0x00,0x00,0x03,0xDE,0xAD,0xBE,0xEF], none, none))) :-
		insert_packets(aos_profile(20, 0, false, false), 0, packet_zone([0xAA,0xBB], [ccsds_packet(0, 0, 1, 1, 3, 0, none, [0xDE,0xAD,0xBE,0xEF])], []), aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [0,0,0,0,0,0,0,0,0,0,0,0,0,0], none, none), UpdatedFrame).

	test(ccsds_link_profiles_reassemble_packets_6_01, deterministic((Packets == [], Pending == [pending_fragment(aos, 42, 3, [0x00,0x00,0xC0,0x00])]))) :-
		initial_reassembly_state(State),
		reassemble_packets(aos_profile(12, 0, false, false), 0, aos_transfer_frame(1, 42, 3, 16, signaling_field(0, 0, 0, 0), none, [0x00,0x00,0x00,0x00,0xC0,0x00], none, none), State, Packets, UpdatedState),
		pending_fragments(UpdatedState, Pending).

	test(ccsds_link_profiles_reassemble_frames_7_01, deterministic((Packets == [ccsds_packet(0, 0, 0, 0, 3, 2, none, [0x44])], Pending == [], Events == [dropped_fragment(tm, 42, 3, discontinuity(256, 256, 33, 34), [0x00, 0x00, 0xC0, 0x00]), resynchronized(tm, 42, 3, discontinuity(256, 256, 33, 34))]))) :-
		initial_reassembly_state(State),
		reassemble_frames(tm_profile(16, 0, false), 0, [
			tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x00,0x00,0xC0,0x00], none, none),
			tm_transfer_frame(0, 42, 3, 0, 16, 34, 0, 0, 0, 3, 3, none, [0x00,0x00,0x42,0x00,0x00,0xC0,0x02,0x00,0x00,0x44], none, none)
		], resynchronize, State, Packets, UpdatedState, Events),
		pending_fragments(UpdatedState, Pending).

:- end_object.
