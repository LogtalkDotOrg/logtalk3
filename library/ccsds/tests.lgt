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
		version is 0:5:1,
		author is 'Paulo Moura',
		date is 2025-12-17,
		comment is 'Unit tests for the "ccsds" library.'
	]).

	:- uses(ccsds, [
		parse/2, generate/2,
		version/2, type/2, secondary_header_flag/2, apid/2,
		sequence_flags/2, sequence_count/2, data_length/2, user_data/2,
		secondary_header/2, secondary_header_time/2
	]).

	:- uses(list, [
		member/2, flatten/2, length/2
	]).

	cover(ccsds).
	cover(ccsds(_)).
	cover(ccsds_types).

	% Test packet structure (without secondary header parsing):
	% - Version: 0 (3 bits)
	% - Type: 0 (telemetry) (1 bit)
	% - Secondary Header Flag: 1 (1 bit)
	% - APID: 1 (11 bits) -> 0x0801 in first two bytes
	% - Sequence Flags: 3 (standalone) (2 bits)
	% - Sequence Count: 0 (14 bits) -> 0xC000 in bytes 2-3
	% - Data Length: 3 (means 4 bytes of data) (16 bits) -> 0x0003
	% - Secondary Header: none (when using ccsds object with no sec header parsing)
	% - User Data: [0xDE, 0xAD, 0xBE, 0xEF]

	% parse/2 tests

	test(ccsds_parse_2_01, true(Packets == [])) :-
		% Empty byte stream
		parse(bytes([]), Packets).

	test(ccsds_parse_2_02, true(Packets == [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])])) :-
		% Single minimal packet
		parse(bytes([0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42]), Packets).

	test(ccsds_parse_2_03, true(Packets == [ccsds_packet(0, 0, 1, 1, 3, 0, none, [0xDE, 0xAD, 0xBE, 0xEF])])) :-
		parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF]), Packets).

	test(ccsds_parse_2_04, true(length(Packets, 2))) :-
		% Two consecutive packets
		parse(bytes([
			0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42,  % First packet (1 byte data)
			0x00, 0x00, 0xC0, 0x01, 0x00, 0x00, 0x43   % Second packet (1 byte data)
		]), Packets).

	test(ccsds_parse_2_05, true(Packets == [ccsds_packet(0, 1, 0, 2047, 0, 16383, none, [0xFF])])) :-
		% Telecommand packet with max APID (2047) and max sequence count (16383)
		% Byte0: 0001 0111 = 0x17 (version=0, type=1, secheader=0, apid_high=7)
		% Byte1: 1111 1111 = 0xFF (apid_low=255) -> APID = 7*256 + 255 = 2047
		% Byte2: 0011 1111 = 0x3F (seqflags=0, seqcount_high=63)
		% Byte3: 1111 1111 = 0xFF (seqcount_low=255) -> SeqCount = 63*256 + 255 = 16383
		% Byte4-5: 0x0000 (data length = 0, means 1 byte)
		parse(bytes([0x17, 0xFF, 0x3F, 0xFF, 0x00, 0x00, 0xFF]), Packets).

	test(ccsds_parse_2_06, true(Packets == [ccsds_packet(0, 0, 0, 0, 1, 1, none, [0xAA, 0xBB])])) :-
		% Telemetry, no secondary header, APID=0, first segment, seqcount=1, 2 bytes data
		% Byte0: 0000 0000 = 0x00
		% Byte1: 0000 0000 = 0x00
		% Byte2: 0100 0000 = 0x40 (seqflags=1, seqcount_high=0)
		% Byte3: 0000 0001 = 0x01 (seqcount_low=1)
		% Byte4-5: 0x0001 (data length = 1, means 2 bytes)
		parse(bytes([0x00, 0x00, 0x40, 0x01, 0x00, 0x01, 0xAA, 0xBB]), Packets).

	test(ccsds_parse_2_07, error(domain_error(ccsds_byte_sequence, _))) :-
		% Too few bytes for a valid packet
		parse(bytes([0x00, 0x00, 0x00]), _).

	test(ccsds_parse_2_08, true(Packets == [])) :-
		% Empty byte list returns empty list
		parse(bytes([]), Packets).

	% generate/2 tests

	test(ccsds_generate_2_01, true(Bytes == [0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF])) :-
		generate(bytes(Bytes), [ccsds_packet(0, 0, 1, 1, 3, 0, none, [0xDE, 0xAD, 0xBE, 0xEF])]).

	test(ccsds_generate_2_02, true(Bytes == [0x17, 0xFF, 0x3F, 0xFF, 0x00, 0x00, 0xFF])) :-
		generate(bytes(Bytes), [ccsds_packet(0, 1, 0, 2047, 0, 16383, none, [0xFF])]).

	test(ccsds_generate_2_03, true(Bytes == [0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42])) :-
		generate(bytes(Bytes), [ccsds_packet(0, 0, 0, 0, 3, 0, none, [0x42])]).

	% Roundtrip tests (parse then generate should give original bytes)

	test(ccsds_roundtrip_01, true(Bytes == OriginalBytes)) :-
		OriginalBytes = [0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF],
		parse(bytes(OriginalBytes), Packets),
		generate(bytes(Bytes), Packets).

	test(ccsds_roundtrip_02, true(Bytes == OriginalBytes)) :-
		OriginalBytes = [0x17, 0xFF, 0x3F, 0xFF, 0x00, 0x00, 0xFF],
		parse(bytes(OriginalBytes), Packets),
		generate(bytes(Bytes), Packets).

	% Accessor predicate tests

	test(ccsds_version_2_01, true(Version == 0)) :-
		parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF]), [Packet]),
		version(Packet, Version).

	test(ccsds_type_2_01, true(Type == telemetry)) :-
		parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF]), [Packet]),
		type(Packet, Type).

	test(ccsds_type_2_02, true(Type == telecommand)) :-
		parse(bytes([0x17, 0xFF, 0x3F, 0xFF, 0x00, 0x00, 0xFF]), [Packet]),
		type(Packet, Type).

	test(ccsds_secondary_header_flag_2_01, true(Flag == present)) :-
		parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF]), [Packet]),
		secondary_header_flag(Packet, Flag).

	test(ccsds_secondary_header_flag_2_02, true(Flag == absent)) :-
		parse(bytes([0x17, 0xFF, 0x3F, 0xFF, 0x00, 0x00, 0xFF]), [Packet]),
		secondary_header_flag(Packet, Flag).

	test(ccsds_apid_2_01, true(APID == 1)) :-
		parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF]), [Packet]),
		apid(Packet, APID).

	test(ccsds_apid_2_02, true(APID == 2047)) :-
		parse(bytes([0x17, 0xFF, 0x3F, 0xFF, 0x00, 0x00, 0xFF]), [Packet]),
		apid(Packet, APID).

	test(ccsds_sequence_flags_2_01, true(Flags == standalone)) :-
		parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF]), [Packet]),
		sequence_flags(Packet, Flags).

	test(ccsds_sequence_flags_2_02, true(Flags == continuation)) :-
		parse(bytes([0x17, 0xFF, 0x3F, 0xFF, 0x00, 0x00, 0xFF]), [Packet]),
		sequence_flags(Packet, Flags).

	test(ccsds_sequence_count_2_01, true(Count == 0)) :-
		parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF]), [Packet]),
		sequence_count(Packet, Count).

	test(ccsds_sequence_count_2_02, true(Count == 16383)) :-
		parse(bytes([0x17, 0xFF, 0x3F, 0xFF, 0x00, 0x00, 0xFF]), [Packet]),
		sequence_count(Packet, Count).

	test(ccsds_data_length_2_01, true(Length == 3)) :-
		parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF]), [Packet]),
		data_length(Packet, Length).

	test(ccsds_user_data_2_01, true(Data == [0xDE, 0xAD, 0xBE, 0xEF])) :-
		parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF]), [Packet]),
		user_data(Packet, Data).

	% secondary_header/2 tests

	test(ccsds_secondary_header_2_01, true(SecHeader == none)) :-
		% No secondary header when flag is 0
		parse(bytes([0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42]), [Packet]),
		secondary_header(Packet, SecHeader).

	test(ccsds_secondary_header_2_02, true(SecHeader == none)) :-
		% Secondary header flag is 1 but using ccsds (no parsing)
		parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x03, 0xDE, 0xAD, 0xBE, 0xEF]), [Packet]),
		secondary_header(Packet, SecHeader).

	% Secondary header parsing tests using ccsds(N) parameterized object

	test(ccsds_secondary_header_parse_01, true(SecHeader == secondary_header([0x01, 0x02, 0x03, 0x04, 0x05, 0x06]))) :-
		% Parse 6-byte secondary header
		% Packet: sec_header_flag=1, APID=1, standalone, seqcount=0, data_length=7 (8 bytes: 6 sec header + 2 user data)
		ccsds(6)::parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x07,
		                       0x01, 0x02, 0x03, 0x04, 0x05, 0x06,  % 6-byte secondary header
		                       0xAA, 0xBB]), [Packet]),            % 2-byte user data
		secondary_header(Packet, SecHeader).

	test(ccsds_secondary_header_parse_02, true(UserData == [0xAA, 0xBB])) :-
		% Verify user data is correctly extracted after secondary header
		ccsds(6)::parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x07,
		                       0x01, 0x02, 0x03, 0x04, 0x05, 0x06,
		                       0xAA, 0xBB]), [Packet]),
		user_data(Packet, UserData).

	test(ccsds_secondary_header_parse_03, true(SecHeader == none)) :-
		% No secondary header parsed when flag is 0 (even with ccsds(6))
		ccsds(6)::parse(bytes([0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42]), [Packet]),
		secondary_header(Packet, SecHeader).

	test(ccsds_secondary_header_parse_04, true(SecHeader == secondary_header([0x00, 0x00, 0x01, 0x00, 0x00, 0x80]))) :-
		% Parse secondary header with CUC time code (coarse=256, fine=128)
		ccsds(6)::parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x06,
		                       0x00, 0x00, 0x01, 0x00, 0x00, 0x80,  % CUC time: coarse=256, fine=128
		                       0xFF]), [Packet]),
		secondary_header(Packet, SecHeader).

	% secondary_header_time/2 tests

	test(ccsds_secondary_header_time_01, true(Time == cuc_time(256, 128))) :-
		% Extract CUC time from secondary header
		ccsds(6)::parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x06,
		                       0x00, 0x00, 0x01, 0x00, 0x00, 0x80,
		                       0xFF]), [Packet]),
		secondary_header_time(Packet, Time).

	test(ccsds_secondary_header_time_02, true(Time == cuc_time(16909060, 1286))) :-
		% CUC time with larger values: coarse=0x01020304, fine=0x0506
		ccsds(6)::parse(bytes([0x08, 0x01, 0xC0, 0x00, 0x00, 0x06,
		                       0x01, 0x02, 0x03, 0x04, 0x05, 0x06,
		                       0xFF]), [Packet]),
		secondary_header_time(Packet, Time).

	test(ccsds_secondary_header_time_03, fail) :-
		% No time available when secondary header is none
		parse(bytes([0x00, 0x00, 0xC0, 0x00, 0x00, 0x00, 0x42]), [Packet]),
		secondary_header_time(Packet, _).

	% Roundtrip with secondary header

	test(ccsds_secondary_header_roundtrip_01, true(Bytes == OriginalBytes)) :-
		OriginalBytes = [0x08, 0x01, 0xC0, 0x00, 0x00, 0x07,
		                 0x01, 0x02, 0x03, 0x04, 0x05, 0x06,
		                 0xAA, 0xBB],
		ccsds(6)::parse(bytes(OriginalBytes), Packets),
		ccsds(6)::generate(bytes(Bytes), Packets).

	% Generate with secondary header

	test(ccsds_generate_secondary_header_01, true(Bytes == Expected)) :-
		Expected = [0x08, 0x01, 0xC0, 0x00, 0x00, 0x07,
		            0x01, 0x02, 0x03, 0x04, 0x05, 0x06,
		            0xAA, 0xBB],
		ccsds(6)::generate(bytes(Bytes), [ccsds_packet(0, 0, 1, 1, 3, 0,
		                   secondary_header([0x01, 0x02, 0x03, 0x04, 0x05, 0x06]),
		                   [0xAA, 0xBB])]).

	% ============================================================
	% Tests using sample data files from CCSDSPy project
	% ============================================================

	% --- var_length_packets.bin tests ---
	% Contains 10 variable-length packets with APIDs 0x20E2 (8418)

	test(ccsds_file_var_length_01, true(N == 10)) :-
		^^file_path('test_files/var_length_packets.bin', Path),
		ccsds::parse(file(Path), Packets),
		length(Packets, N).

	test(ccsds_file_var_length_02, true(APID == 226)) :-
		% First packet APID (0x00E2 from first two bytes 0x20E2 with version/type bits)
		^^file_path('test_files/var_length_packets.bin', Path),
		ccsds::parse(file(Path), [Packet| _]),
		apid(Packet, APID).

	test(ccsds_file_var_length_03, true) :-
		% All packets have the same APID
		^^file_path('test_files/var_length_packets.bin', Path),
		ccsds::parse(file(Path), Packets),
		forall(member(Packet, Packets), apid(Packet, 226)).

	test(ccsds_file_var_length_04, true(Counts == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])) :-
		% Sequence counts increment from 0 to 9
		^^file_path('test_files/var_length_packets.bin', Path),
		ccsds::parse(file(Path), Packets),
		findall(Count, (member(Packet, Packets), sequence_count(Packet, Count)), Counts).

	% --- apid01217.tlm tests (Europa Clipper) ---
	% Small file with 4 packets, APID 1217 (0x04C1)

	test(ccsds_file_europa_clipper_01, true(N > 0)) :-
		^^file_path('test_files/apid01217.tlm', Path),
		ccsds::parse(file(Path), Packets),
		length(Packets, N).

	test(ccsds_file_europa_clipper_02, true(APID == 1217)) :-
		% First packet should have APID 1217
		^^file_path('test_files/apid01217.tlm', Path),
		ccsds::parse(file(Path), [Packet| _]),
		apid(Packet, APID).

	test(ccsds_file_europa_clipper_03, true(Type == telemetry)) :-
		% Packets should be telemetry
		^^file_path('test_files/apid01217.tlm', Path),
		ccsds::parse(file(Path), [Packet| _]),
		type(Packet, Type).

	% --- SSAT1 telemetry tests ---
	% Larger file with many packets, APID 1

	test(ccsds_file_ssat1_01, true(N > 100)) :-
		% File should contain many packets
		^^file_path('test_files/SSAT1_2015-180-00-00-00_2015-180-01-59-58_1_1_sim.tlm', Path),
		ccsds::parse(file(Path), Packets),
		length(Packets, N).

	test(ccsds_file_ssat1_02, true(APID == 1)) :-
		% First packet should have APID 1
		^^file_path('test_files/SSAT1_2015-180-00-00-00_2015-180-01-59-58_1_1_sim.tlm', Path),
		ccsds::parse(file(Path), [Packet| _]),
		apid(Packet, APID).

	test(ccsds_file_ssat1_03, true(Flag == present)) :-
		% Packets should have secondary header
		^^file_path('test_files/SSAT1_2015-180-00-00-00_2015-180-01-59-58_1_1_sim.tlm', Path),
		ccsds::parse(file(Path), [Packet| _]),
		secondary_header_flag(Packet, Flag).

	% --- Roundtrip tests with real data ---
	% Parse and regenerate should produce identical bytes

	test(ccsds_file_roundtrip_01, true(Bytes == OriginalBytes)) :-
		% Roundtrip first packet from var_length_packets.bin
		^^file_path('test_files/var_length_packets.bin', Path),
		reader::file_to_bytes(Path, AllBytes),
		% First packet: 6 header + 4 data bytes = 10 bytes
		AllBytes = [B0, B1, B2, B3, B4, B5, B6, B7, B8, B9| _],
		OriginalBytes = [B0, B1, B2, B3, B4, B5, B6, B7, B8, B9],
		ccsds::parse(bytes(OriginalBytes), Packets),
		ccsds::generate(bytes(Bytes), Packets).

	test(ccsds_file_roundtrip_02, true(Packets == ParsedAgain)) :-
		% Parse all packets, regenerate all bytes, parse again
		^^file_path('test_files/var_length_packets.bin', Path),
		ccsds::parse(file(Path), Packets),
		ccsds::generate(bytes(AllBytes), Packets),
		ccsds::parse(bytes(AllBytes), ParsedAgain).

	% QuickCheck tests for arbitrary generators and type checking

	% Test that generated ccsds_packet bytes pass type checking
	quick_check(
		ccsds_quickcheck_type_01,
		type::check({ccsds_packet}, +ccsds_packet),
		[n(100)]
	).

	% Test that generated ccsds_packet(0) bytes pass type checking
	quick_check(
		ccsds_quickcheck_type_02,
		type::check({ccsds_packet(0)}, +ccsds_packet(0)),
		[n(100)]
	).

	% Test that generated ccsds_packet(6) bytes pass type checking
	quick_check(
		ccsds_quickcheck_type_03,
		type::check({ccsds_packet(6)}, +ccsds_packet(6)),
		[n(100)]
	).

	% Test roundtrip: generate bytes, parse them, regenerate bytes
	quick_check(
		ccsds_quickcheck_roundtrip_01,
		roundtrip_property(+ccsds_packet),
		[n(100)]
	).

	% Test roundtrip with secondary header
	quick_check(
		ccsds_quickcheck_roundtrip_02,
		roundtrip_property_with_secondary_header(+ccsds_packet(6)),
		[n(100)]
	).

	% Test that parsed packets can be regenerated
	quick_check(
		ccsds_quickcheck_parse_generate_01,
		parse_generate_property(+ccsds_packet),
		[n(100)]
	).

	% Helper predicates for QuickCheck tests

	roundtrip_property(Bytes) :-
		ccsds::parse(bytes(Bytes), Packets),
		ccsds::generate(bytes(GeneratedBytes), Packets),
		Bytes == GeneratedBytes.

	roundtrip_property_with_secondary_header(Bytes) :-
		ccsds(6)::parse(bytes(Bytes), Packets),
		ccsds(6)::generate(bytes(GeneratedBytes), Packets),
		Bytes == GeneratedBytes.

	parse_generate_property(Bytes) :-
		ccsds::parse(bytes(Bytes), Packets),
		ccsds::generate(bytes(_GeneratedBytes), Packets).

:- end_object.
