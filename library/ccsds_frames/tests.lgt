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
		date is 2026-06-30,
		comment is 'Unit tests for the "ccsds_frames" library.'
	]).

	:- uses(ccsds_frames, [
		valid/1 as valid_frame/1, frame_type/2,
		version/2 as frame_version/2,
		spacecraft_id/2 as frame_spacecraft_id/2,
		virtual_channel_id/2 as frame_virtual_channel_id/2,
		data_field/2 as frame_data_field/2,
		ocf/2 as frame_ocf/2,
		fecf/2 as frame_fecf/2,
		update_fecf/2 as refresh_frame_fecf/2,
		verify_fecf/1 as verify_frame_fecf/1,
		extract_packets/3, insert_packets/4
	]).

	:- uses(ccsds_tm_frames(16, 0, false), [
		parse/2, generate/2, generate/3, valid/1 as valid_tm_frame/1,
		version/2, spacecraft_id/2, virtual_channel_id/2,
		ocf_flag/2, master_channel_frame_count/2, virtual_channel_frame_count/2,
		secondary_header_flag/2, synchronization_flag/2, packet_order_flag/2,
		segment_length_identifier/2, first_header_pointer/2,
		secondary_header/2, data_field/2, ocf/2, fecf/2
	]).

	:- uses(ccsds_tc_frames(10, 0, true), [
		parse/2 as parse_tc_fecf/2,
		generate/2 as generate_tc_fecf/2,
		generate/3 as generate_tc_fecf/3,
		valid/1 as valid_tc_fecf/1,
		update_fecf/2 as update_tc_fecf/2,
		verify_fecf/1 as verify_tc_fecf/1,
		bypass_flag/2,
		control_command_flag/2,
		sequence_number/2
	]).

	:- uses(ccsds_tc_frames(12, 2, false), [
		parse/2 as parse_tc_segment/2,
		segment_header/2 as tc_segment_header/2
	]).

	:- uses(ccsds_aos_frames(12, 0, false, false), [
		parse/2 as parse_aos_basic/2,
		generate/2 as generate_aos_basic/2,
		generate/3 as generate_aos_basic/3,
		valid/1 as valid_aos_basic/1,
		version/2 as aos_version/2,
		spacecraft_id/2 as aos_spacecraft_id/2,
		virtual_channel_id/2 as aos_virtual_channel_id/2,
		virtual_channel_frame_count/2 as aos_virtual_channel_frame_count/2,
		signaling_field/2 as aos_signaling_field/2,
		insert_zone/2 as aos_insert_zone/2,
		data_field/2 as aos_data_field/2,
		ocf/2 as aos_ocf/2,
		fecf/2 as aos_fecf/2
	]).

	:- uses(ccsds_aos_frames(18, 2, true, true), [
		parse/2 as parse_aos_mission/2,
		generate/2 as generate_aos_mission/2,
		valid/1 as valid_aos_mission/1,
		update_fecf/2 as update_aos_mission_fecf/2,
		verify_fecf/1 as verify_aos_mission_fecf/1
	]).

	cover(ccsds_frames).
	cover(ccsds_tm_frames(_, _, _)).
	cover(ccsds_tc_frames(_, _, _)).
	cover(ccsds_aos_frames(_, _, _, _)).
	cover(ccsds_frames_types).

	test(ccsds_frames_valid_1_01, deterministic) :-
		valid_frame(tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none)).

	test(ccsds_frames_valid_1_02, deterministic) :-
		valid_frame(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D]))).

	test(ccsds_frames_valid_1_03, deterministic) :-
		valid_frame(aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [1,2,3,4,5,6], none, none)).

	test(ccsds_frames_frame_type_2_01, deterministic(Type == tm)) :-
		frame_type(tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none), Type).

	test(ccsds_frames_frame_type_2_02, deterministic(Type == tc)) :-
		frame_type(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D])), Type).

	test(ccsds_frames_frame_type_2_03, deterministic(Type == aos)) :-
		frame_type(aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [1,2,3,4,5,6], none, none), Type).

	test(ccsds_frames_version_2_01, deterministic(Value == 0)) :-
		frame_version(tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none), Value).

	test(ccsds_frames_spacecraft_id_2_01, deterministic(Value == 42)) :-
		frame_spacecraft_id(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D])), Value).

	test(ccsds_frames_virtual_channel_id_2_01, deterministic(Value == 3)) :-
		frame_virtual_channel_id(aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [1,2,3,4,5,6], none, none), Value).

	test(ccsds_frames_data_field_2_01, deterministic(Value == [1,2,3])) :-
		frame_data_field(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D])), Value).

	test(ccsds_frames_ocf_2_01, deterministic(Value == ocf([0xDE,0xAD,0xBE,0xEF]))) :-
		frame_ocf(tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none), Value).

	test(ccsds_frames_fecf_2_01, deterministic(Value == fecf([0x44,0x6D]))) :-
		frame_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D])), Value).

	test(ccsds_frames_update_fecf_2_01, deterministic(UpdatedFrame == tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D])))) :-
		refresh_frame_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x00,0x00])), UpdatedFrame).

	test(ccsds_frames_verify_fecf_1_01, deterministic) :-
		verify_frame_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D]))).

	test(ccsds_frames_verify_fecf_1_02, fail) :-
		verify_frame_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x00,0x00]))).

	test(ccsds_frames_extract_packets_3_01, deterministic(Packets == [ccsds_packet(0, 0, 1, 1, 3, 0, none, [0xDE,0xAD,0xBE,0xEF])])) :-
		extract_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x08,0x01,0xC0,0x00,0x00,0x03,0xDE,0xAD,0xBE,0xEF], none, none), 0, Packets).

	test(ccsds_frames_insert_packets_4_01, deterministic(UpdatedFrame == tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x08,0x01,0xC0,0x00,0x00,0x03,0xDE,0xAD,0xBE,0xEF], none, none))) :-
		insert_packets([ccsds_packet(0, 0, 1, 1, 3, 0, none, [0xDE,0xAD,0xBE,0xEF])], 0, tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [], none, none), UpdatedFrame).

	test(ccsds_frames_extract_packets_3_02, error(domain_error(ccsds_secondary_header_length, _))) :-
		extract_packets(tm_transfer_frame(0, 42, 3, 0, 16, 32, 0, 0, 0, 3, 0, none, [0x08,0x01,0xC0,0x00,0x00,0x03,0xDE,0xAD,0xBE,0xEF], none, none), -1, _).

	test(ccsds_tm_frames_parse_2_01, true(Frames == [tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none)])) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), Frames).

	test(ccsds_tm_frames_parse_2_02, error(domain_error(ccsds_frame_byte_sequence, _))) :-
		parse(bytes([0x00, 0x01, 0x02]), _).

	test(ccsds_tm_frames_generate_2_01, true(Bytes == [0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF])) :-
		generate(bytes(Bytes), [tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none)]).

	test(ccsds_tm_frames_generate_3_01, true(Bytes == [0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF, 0xFF])) :-
		generate(tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none), Bytes, [0xFF]).

	test(ccsds_tm_frames_valid_1_01, deterministic) :-
		valid_tm_frame(tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none)).

	test(ccsds_tm_frames_valid_1_02, fail) :-
		valid_tm_frame(tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3], ocf([0xDE,0xAD,0xBE,0xEF]), none)).

	test(ccsds_tm_frames_version_2_01, deterministic(Value == 0)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		version(Frame, Value).

	test(ccsds_tm_frames_spacecraft_id_2_01, deterministic(Value == 42)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		spacecraft_id(Frame, Value).

	test(ccsds_tm_frames_virtual_channel_id_2_01, deterministic(Value == 3)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		virtual_channel_id(Frame, Value).

	test(ccsds_tm_frames_ocf_flag_2_01, deterministic(Value == present)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		ocf_flag(Frame, Value).

	test(ccsds_tm_frames_master_count_2_01, deterministic(Value == 16)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		master_channel_frame_count(Frame, Value).

	test(ccsds_tm_frames_virtual_count_2_01, deterministic(Value == 32)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		virtual_channel_frame_count(Frame, Value).

	test(ccsds_tm_frames_secondary_header_flag_2_01, deterministic(Value == absent)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		secondary_header_flag(Frame, Value).

	test(ccsds_tm_frames_synchronization_flag_2_01, deterministic(Value == 0)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		synchronization_flag(Frame, Value).

	test(ccsds_tm_frames_packet_order_flag_2_01, deterministic(Value == 0)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		packet_order_flag(Frame, Value).

	test(ccsds_tm_frames_segment_length_identifier_2_01, deterministic(Value == 3)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		segment_length_identifier(Frame, Value).

	test(ccsds_tm_frames_first_header_pointer_2_01, deterministic(Value == 0)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		first_header_pointer(Frame, Value).

	test(ccsds_tm_frames_secondary_header_2_01, deterministic(Value == none)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		secondary_header(Frame, Value).

	test(ccsds_tm_frames_data_field_2_01, deterministic(Value == [1,2,3,4,5,6])) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		data_field(Frame, Value).

	test(ccsds_tm_frames_ocf_2_01, deterministic(Value == ocf([0xDE,0xAD,0xBE,0xEF]))) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		ocf(Frame, Value).

	test(ccsds_tm_frames_fecf_2_01, deterministic(Value == none)) :-
		parse(bytes([0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]), [Frame]),
		fecf(Frame, Value).

	test(ccsds_tm_frames_parse_fecf_01, true(Frames == [tm_transfer_frame(0, 42, 3, 0, 16, 32, 1, 0, 0, 3, 0, secondary_header([0xAA,0xBB]), [1,2,3,4,5,6,7,8], none, fecf([0x28,0x97]))])) :-
		ccsds_tm_frames(18, 2, true)::parse(bytes([0x02, 0xA6, 0x10, 0x20, 0x98, 0x00, 0xAA, 0xBB, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x28, 0x97]), Frames).

	test(ccsds_tm_frames_parse_fecf_02, error(domain_error(ccsds_frame_byte_sequence, _))) :-
		ccsds_tm_frames(18, 2, true)::parse(bytes([0x02, 0xA6, 0x10, 0x20, 0x98, 0x00, 0xAA, 0xBB, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x12, 0x34]), _).

	test(ccsds_tm_frames_update_fecf_2_01, deterministic(UpdatedFrame == tm_transfer_frame(0, 42, 3, 0, 16, 32, 1, 0, 0, 3, 0, secondary_header([0xAA,0xBB]), [1,2,3,4,5,6,7,8], none, fecf([0x28,0x97])))) :-
		ccsds_tm_frames(18, 2, true)::update_fecf(tm_transfer_frame(0, 42, 3, 0, 16, 32, 1, 0, 0, 3, 0, secondary_header([0xAA,0xBB]), [1,2,3,4,5,6,7,8], none, none), UpdatedFrame).

	test(ccsds_tm_frames_verify_fecf_1_01, deterministic) :-
		ccsds_tm_frames(18, 2, true)::verify_fecf(tm_transfer_frame(0, 42, 3, 0, 16, 32, 1, 0, 0, 3, 0, secondary_header([0xAA,0xBB]), [1,2,3,4,5,6,7,8], none, fecf([0x28,0x97]))).

	test(ccsds_tm_frames_verify_fecf_1_02, fail) :-
		ccsds_tm_frames(18, 2, true)::verify_fecf(tm_transfer_frame(0, 42, 3, 0, 16, 32, 1, 0, 0, 3, 0, secondary_header([0xAA,0xBB]), [1,2,3,4,5,6,7,8], none, fecf([0x12,0x34]))).

	test(ccsds_tc_frames_parse_2_01, true(Frames == [tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D]))])) :-
		parse_tc_fecf(bytes([0x20, 0x2A, 0x0C, 0x09, 0x07, 0x01, 0x02, 0x03, 0x44, 0x6D]), Frames).

	test(ccsds_tc_frames_parse_2_03, error(domain_error(ccsds_frame_byte_sequence, _))) :-
		parse_tc_fecf(bytes([0x20, 0x2A, 0x0C, 0x09, 0x07, 0x01, 0x02, 0x03, 0x12, 0x34]), _).

	test(ccsds_tc_frames_parse_2_02, true(Frames == [tc_transfer_frame(0, 0, 1, 42, 3, 7, segment_header([0xAA,0xBB]), [1,2,3,4,5], none)])) :-
		parse_tc_segment(bytes([0x10, 0x2A, 0x0C, 0x0B, 0x07, 0xAA, 0xBB, 0x01, 0x02, 0x03, 0x04, 0x05]), Frames).

	test(ccsds_tc_frames_generate_2_01, true(Bytes == [0x20, 0x2A, 0x0C, 0x09, 0x07, 0x01, 0x02, 0x03, 0x44, 0x6D])) :-
		generate_tc_fecf(bytes(Bytes), [tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], none)]).

	test(ccsds_tc_frames_generate_3_01, true(Bytes == [0x20, 0x2A, 0x0C, 0x09, 0x07, 0x01, 0x02, 0x03, 0x44, 0x6D, 0xFF])) :-
		generate_tc_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], none), Bytes, [0xFF]).

	test(ccsds_tc_frames_valid_1_01, deterministic) :-
		valid_tc_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D]))).

	test(ccsds_tc_frames_valid_1_02, fail) :-
		valid_tc_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2], fecf([0x44,0x6D]))).

	test(ccsds_tc_frames_valid_1_03, fail) :-
		valid_tc_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x12,0x34]))).

	test(ccsds_tc_frames_update_fecf_2_01, deterministic(UpdatedFrame == tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D])))) :-
		update_tc_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], none), UpdatedFrame).

	test(ccsds_tc_frames_verify_fecf_1_01, deterministic) :-
		verify_tc_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D]))).

	test(ccsds_tc_frames_verify_fecf_1_02, fail) :-
		verify_tc_fecf(tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x12,0x34]))).

	test(ccsds_tc_frames_bypass_flag_2_01, deterministic(Value == 1)) :-
		parse_tc_fecf(bytes([0x20, 0x2A, 0x0C, 0x09, 0x07, 0x01, 0x02, 0x03, 0x44, 0x6D]), [Frame]),
		bypass_flag(Frame, Value).

	test(ccsds_tc_frames_control_command_flag_2_01, deterministic(Value == 0)) :-
		parse_tc_fecf(bytes([0x20, 0x2A, 0x0C, 0x09, 0x07, 0x01, 0x02, 0x03, 0x44, 0x6D]), [Frame]),
		control_command_flag(Frame, Value).

	test(ccsds_tc_frames_sequence_number_2_01, deterministic(Value == 7)) :-
		parse_tc_fecf(bytes([0x20, 0x2A, 0x0C, 0x09, 0x07, 0x01, 0x02, 0x03, 0x44, 0x6D]), [Frame]),
		sequence_number(Frame, Value).

	test(ccsds_tc_frames_segment_header_2_01, deterministic(Value == segment_header([0xAA,0xBB]))) :-
		parse_tc_segment(bytes([0x10, 0x2A, 0x0C, 0x0B, 0x07, 0xAA, 0xBB, 0x01, 0x02, 0x03, 0x04, 0x05]), [Frame]),
		tc_segment_header(Frame, Value).

	test(ccsds_aos_frames_parse_2_01, true(Frames == [aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [1,2,3,4,5,6], none, none)])) :-
		parse_aos_basic(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), Frames).

	test(ccsds_aos_frames_parse_2_02, true(Frames == [aos_transfer_frame(1, 42, 3, 0x1123456, signaling_field(0, 1, 0, 1), insert_zone([0xAA,0xBB]), [1,2,3,4], ocf([0xDE,0xAD,0xBE,0xEF]), fecf([0xD1,0x7D]))])) :-
		parse_aos_mission(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x41, 0xAA, 0xBB, 0x01, 0x02, 0x03, 0x04, 0xDE, 0xAD, 0xBE, 0xEF, 0xD1, 0x7D]), Frames).

	test(ccsds_aos_frames_parse_2_03, error(domain_error(ccsds_frame_byte_sequence, _))) :-
		parse_aos_basic(bytes([0x4A, 0x83]), _).

	test(ccsds_aos_frames_parse_2_04, true(Frames == [aos_transfer_frame(1, 513, 3, 0x123456, signaling_field(0, 0, 2, 0), none, [1,2,3,4,5,6], none, none)])) :-
		parse_aos_basic(bytes([0x40, 0x43, 0x12, 0x34, 0x56, 0x20, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), Frames).

	test(ccsds_aos_frames_generate_2_01, true(Bytes == [0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06])) :-
		generate_aos_basic(bytes(Bytes), [aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [1,2,3,4,5,6], none, none)]).

	test(ccsds_aos_frames_generate_2_02, true(Bytes == [0x4A, 0x83, 0x12, 0x34, 0x56, 0x41, 0xAA, 0xBB, 0x01, 0x02, 0x03, 0x04, 0xDE, 0xAD, 0xBE, 0xEF, 0xD1, 0x7D])) :-
		generate_aos_mission(bytes(Bytes), [aos_transfer_frame(1, 42, 3, 0x1123456, signaling_field(0, 1, 0, 1), insert_zone([0xAA,0xBB]), [1,2,3,4], ocf([0xDE,0xAD,0xBE,0xEF]), none)]).

	test(ccsds_aos_frames_generate_3_01, true(Bytes == [0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xFF])) :-
		generate_aos_basic(aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [1,2,3,4,5,6], none, none), Bytes, [0xFF]).

	test(ccsds_aos_frames_valid_1_01, deterministic) :-
		valid_aos_basic(aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [1,2,3,4,5,6], none, none)).

	test(ccsds_aos_frames_valid_1_02, fail) :-
		valid_aos_basic(aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 1, 0), none, [1,2,3,4,5,6], none, none)).

	test(ccsds_aos_frames_valid_1_03, deterministic) :-
		valid_aos_mission(aos_transfer_frame(1, 42, 3, 0x1123456, signaling_field(0, 1, 0, 1), insert_zone([0xAA,0xBB]), [1,2,3,4], ocf([0xDE,0xAD,0xBE,0xEF]), fecf([0xD1,0x7D]))).

	test(ccsds_aos_frames_valid_1_04, fail) :-
		valid_aos_mission(aos_transfer_frame(1, 42, 3, 0x1123456, signaling_field(0, 1, 0, 1), insert_zone([0xAA,0xBB]), [1,2,3,4], ocf([0xDE,0xAD,0xBE,0xEF]), fecf([0x12,0x34]))).

	test(ccsds_aos_frames_version_2_01, deterministic(Value == 1)) :-
		parse_aos_basic(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), [Frame]),
		aos_version(Frame, Value).

	test(ccsds_aos_frames_spacecraft_id_2_01, deterministic(Value == 42)) :-
		parse_aos_basic(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), [Frame]),
		aos_spacecraft_id(Frame, Value).

	test(ccsds_aos_frames_spacecraft_id_2_02, deterministic(Value == 513)) :-
		parse_aos_basic(bytes([0x40, 0x43, 0x12, 0x34, 0x56, 0x20, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), [Frame]),
		aos_spacecraft_id(Frame, Value).

	test(ccsds_aos_frames_virtual_channel_id_2_01, deterministic(Value == 3)) :-
		parse_aos_basic(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), [Frame]),
		aos_virtual_channel_id(Frame, Value).

	test(ccsds_aos_frames_virtual_channel_frame_count_2_01, deterministic(Value == 0x123456)) :-
		parse_aos_basic(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), [Frame]),
		aos_virtual_channel_frame_count(Frame, Value).

	test(ccsds_aos_frames_signaling_field_2_01, deterministic(Value == signaling_field(1, 0, 0, 0))) :-
		parse_aos_basic(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), [Frame]),
		aos_signaling_field(Frame, Value).

	test(ccsds_aos_frames_insert_zone_2_01, deterministic(Value == none)) :-
		parse_aos_basic(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), [Frame]),
		aos_insert_zone(Frame, Value).

	test(ccsds_aos_frames_data_field_2_01, deterministic(Value == [1,2,3,4,5,6])) :-
		parse_aos_basic(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), [Frame]),
		aos_data_field(Frame, Value).

	test(ccsds_aos_frames_ocf_2_01, deterministic(Value == none)) :-
		parse_aos_basic(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), [Frame]),
		aos_ocf(Frame, Value).

	test(ccsds_aos_frames_ocf_2_02, deterministic(Value == ocf([0xDE,0xAD,0xBE,0xEF]))) :-
		parse_aos_mission(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x41, 0xAA, 0xBB, 0x01, 0x02, 0x03, 0x04, 0xDE, 0xAD, 0xBE, 0xEF, 0xD1, 0x7D]), [Frame]),
		aos_ocf(Frame, Value).

	test(ccsds_aos_frames_fecf_2_01, deterministic(Value == none)) :-
		parse_aos_basic(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]), [Frame]),
		aos_fecf(Frame, Value).

	test(ccsds_aos_frames_fecf_2_02, deterministic(Value == fecf([0xD1,0x7D]))) :-
		parse_aos_mission(bytes([0x4A, 0x83, 0x12, 0x34, 0x56, 0x41, 0xAA, 0xBB, 0x01, 0x02, 0x03, 0x04, 0xDE, 0xAD, 0xBE, 0xEF, 0xD1, 0x7D]), [Frame]),
		aos_fecf(Frame, Value).

	test(ccsds_aos_frames_update_fecf_2_01, deterministic(UpdatedFrame == aos_transfer_frame(1, 42, 3, 0x1123456, signaling_field(0, 1, 0, 1), insert_zone([0xAA,0xBB]), [1,2,3,4], ocf([0xDE,0xAD,0xBE,0xEF]), fecf([0xD1,0x7D])))) :-
		update_aos_mission_fecf(aos_transfer_frame(1, 42, 3, 0x1123456, signaling_field(0, 1, 0, 1), insert_zone([0xAA,0xBB]), [1,2,3,4], ocf([0xDE,0xAD,0xBE,0xEF]), none), UpdatedFrame).

	test(ccsds_aos_frames_verify_fecf_1_01, deterministic) :-
		verify_aos_mission_fecf(aos_transfer_frame(1, 42, 3, 0x1123456, signaling_field(0, 1, 0, 1), insert_zone([0xAA,0xBB]), [1,2,3,4], ocf([0xDE,0xAD,0xBE,0xEF]), fecf([0xD1,0x7D]))).

	test(ccsds_aos_frames_verify_fecf_1_02, fail) :-
		verify_aos_mission_fecf(aos_transfer_frame(1, 42, 3, 0x1123456, signaling_field(0, 1, 0, 1), insert_zone([0xAA,0xBB]), [1,2,3,4], ocf([0xDE,0xAD,0xBE,0xEF]), fecf([0x12,0x34]))).

	test(ccsds_frames_types_check_01, deterministic) :-
		type::check(ccsds_tm_frame(16, 0, false), [0x02, 0xA7, 0x10, 0x20, 0x18, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0xDE, 0xAD, 0xBE, 0xEF]).

	test(ccsds_frames_types_check_02, deterministic) :-
		type::check(ccsds_tm_frame_term(16, 0, false), tm_transfer_frame(0, 42, 3, 1, 16, 32, 0, 0, 0, 3, 0, none, [1,2,3,4,5,6], ocf([0xDE,0xAD,0xBE,0xEF]), none)).

	test(ccsds_frames_types_check_03, deterministic) :-
		type::check(ccsds_tc_frame(10, 0, true), [0x20, 0x2A, 0x0C, 0x09, 0x07, 0x01, 0x02, 0x03, 0x44, 0x6D]).

	test(ccsds_frames_types_check_04, deterministic) :-
		type::check(ccsds_tc_frame_term(10, 0, true), tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [1,2,3], fecf([0x44,0x6D]))).

	test(ccsds_frames_types_check_05, deterministic) :-
		type::check(ccsds_aos_frame(12, 0, false, false), [0x4A, 0x83, 0x12, 0x34, 0x56, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06]).

	test(ccsds_frames_types_check_06, deterministic) :-
		type::check(ccsds_aos_frame_term(12, 0, false, false), aos_transfer_frame(1, 42, 3, 0x123456, signaling_field(1, 0, 0, 0), none, [1,2,3,4,5,6], none, none)).

	test(ccsds_frames_types_check_07, deterministic) :-
		type::check(ccsds_aos_frame(18, 2, true, true), [0x4A, 0x83, 0x12, 0x34, 0x56, 0x41, 0xAA, 0xBB, 0x01, 0x02, 0x03, 0x04, 0xDE, 0xAD, 0xBE, 0xEF, 0xD1, 0x7D]).

	test(ccsds_frames_types_check_08, deterministic) :-
		type::check(ccsds_aos_frame_term(18, 2, true, true), aos_transfer_frame(1, 42, 3, 0x1123456, signaling_field(0, 1, 0, 1), insert_zone([0xAA,0xBB]), [1,2,3,4], ocf([0xDE,0xAD,0xBE,0xEF]), fecf([0xD1,0x7D]))).

:- end_object.
