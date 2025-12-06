%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(ccsds(_SecondaryHeaderLength_)).

	:- info([
		version is 0:3:0,
		author is 'Paulo Moura',
		date is 2025-12-06,
		comment is 'CCSDS Space Packet parser following the CCSDS 133.0-B-2 standard. Parses binary packet data including optional secondary headers.',
		parameters is [
			'SecondaryHeaderLength' - 'Length in bytes of the secondary header when present (0 for no secondary header parsing, or a positive integer).'
		]
	]).

	:- public(parse/2).
	:- mode(parse(+list(byte), -compound), one_or_error).
	:- info(parse/2, [
		comment is 'Parses a list of bytes into a CCSDS packet term. The packet term includes the primary header fields and user data.',
		argnames is ['Bytes', 'Packet']
	]).

	:- public(parse_all/2).
	:- mode(parse_all(+list(byte), -list(compound)), one_or_error).
	:- info(parse_all/2, [
		comment is 'Parses a list of bytes containing multiple CCSDS packets into a list of packet terms.',
		argnames is ['Bytes', 'Packets']
	]).

	:- public(parse_file/2).
	:- mode(parse_file(+atom, -list(compound)), one_or_error).
	:- info(parse_file/2, [
		comment is 'Reads a binary file and parses all CCSDS packets, returning a list of packet terms.',
		argnames is ['File', 'Packets']
	]).

	:- public(parse_stream/2).
	:- mode(parse_stream(+stream_or_alias, -list(compound)), one_or_error).
	:- info(parse_stream/2, [
		comment is 'Reads a binary stream and parses all CCSDS packets, returning a list of packet terms.',
		argnames is ['Stream', 'Packets']
	]).

	:- public(generate/2).
	:- mode(generate(+compound, -list(byte)), one_or_error).
	:- info(generate/2, [
		comment is 'Generates a list of bytes from a CCSDS packet term.',
		argnames is ['Packet', 'Bytes']
	]).

	:- public(generate/3).
	:- mode(generate(+compound, -list(byte), --variable), one_or_error).
	:- info(generate/3, [
		comment is 'Generates a list of bytes from a CCSDS packet term with an open tail.',
		argnames is ['Packet', 'Bytes', 'Tail']
	]).

	:- public(version/2).
	:- mode(version(+compound, -integer), one).
	:- info(version/2, [
		comment is 'Extracts the version number from a packet (always 0 for CCSDS Space Packets).',
		argnames is ['Packet', 'Version']
	]).

	:- public(type/2).
	:- mode(type(+compound, -atom), one).
	:- info(type/2, [
		comment is 'Extracts the packet type from a packet. Returns ``telemetry`` or ``telecommand``.',
		argnames is ['Packet', 'Type']
	]).

	:- public(secondary_header_flag/2).
	:- mode(secondary_header_flag(+compound, -atom), one).
	:- info(secondary_header_flag/2, [
		comment is 'Extracts the secondary header flag. Returns ``absent`` or ``present``.',
		argnames is ['Packet', 'Flag']
	]).

	:- public(apid/2).
	:- mode(apid(+compound, -integer), one).
	:- info(apid/2, [
		comment is 'Extracts the Application Process Identifier (APID) from a packet.',
		argnames is ['Packet', 'APID']
	]).

	:- public(sequence_flags/2).
	:- mode(sequence_flags(+compound, -atom), one).
	:- info(sequence_flags/2, [
		comment is 'Extracts the sequence flags. Returns ``continuation``, ``first``, ``last``, or ``standalone``.',
		argnames is ['Packet', 'Flags']
	]).

	:- public(sequence_count/2).
	:- mode(sequence_count(+compound, -integer), one).
	:- info(sequence_count/2, [
		comment is 'Extracts the packet sequence count (0-16383).',
		argnames is ['Packet', 'Count']
	]).

	:- public(data_length/2).
	:- mode(data_length(+compound, -integer), one).
	:- info(data_length/2, [
		comment is 'Extracts the packet data length field value.',
		argnames is ['Packet', 'Length']
	]).

	:- public(user_data/2).
	:- mode(user_data(+compound, -list(byte)), one).
	:- info(user_data/2, [
		comment is 'Extracts the user data field as a list of bytes.',
		argnames is ['Packet', 'Data']
	]).

	:- public(secondary_header/2).
	:- mode(secondary_header(+compound, -compound), one).
	:- info(secondary_header/2, [
		comment is 'Extracts the secondary header. Returns none if not present, or secondary_header(Bytes) with the raw bytes.',
		argnames is ['Packet', 'SecondaryHeader']
	]).

	:- public(secondary_header_time/2).
	:- mode(secondary_header_time(+compound, -compound), zero_or_one).
	:- info(secondary_header_time/2, [
		comment is 'Extracts time from a secondary header as cuc_time(Coarse, Fine) for CCSDS Unsegmented Time Code. Fails if no secondary header or time cannot be parsed.',
		argnames is ['Packet', 'Time']
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(reader, [
		file_to_bytes/2, stream_to_bytes/2
	]).

	% Primary header is 6 bytes (48 bits):
	% - Packet Version Number: 3 bits (always 000)
	% - Packet Type: 1 bit (0=telemetry, 1=telecommand)
	% - Secondary Header Flag: 1 bit
	% - Application Process ID (APID): 11 bits
	% - Sequence Flags: 2 bits
	% - Packet Sequence Count: 14 bits
	% - Packet Data Length: 16 bits (number of octets in data field - 1)

	parse(Bytes, Packet) :-
		phrase(decode_packet(Packet), Bytes),
		!.
	parse(Bytes, _) :-
		domain_error(ccsds_byte_sequence, Bytes).

	parse_all(Bytes, Packets) :-
		phrase(decode_packets(Packets), Bytes),
		!.
	parse_all(Bytes, _) :-
		domain_error(ccsds_byte_sequence, Bytes).

	parse_file(File, Packets) :-
		file_to_bytes(File, Bytes),
		parse_all(Bytes, Packets).

	parse_stream(Stream, Packets) :-
		stream_to_bytes(Stream, Bytes),
		parse_all(Bytes, Packets).

	generate(Packet, Bytes) :-
		phrase(encode_packet(Packet), Bytes),
		!.
	generate(Packet, _) :-
		domain_error(ccsds_packet, Packet).

	generate(Packet, Bytes, Tail) :-
		phrase(encode_packet(Packet), Bytes, Tail),
		!.
	generate(Packet, _, _) :-
		domain_error(ccsds_packet, Packet).

	% Accessor predicates
	version(ccsds_packet(Version, _, _, _, _, _, _, _, _), Version).

	type(ccsds_packet(_, TypeCode, _, _, _, _, _, _, _), Type) :-
		(	TypeCode =:= 0 ->
			Type = telemetry
		;	Type = telecommand
		).

	secondary_header_flag(ccsds_packet(_, _, SecHeaderFlag, _, _, _, _, _, _), SecHeader) :-
		secondary_header_flag_atom(SecHeaderFlag, SecHeader).

	apid(ccsds_packet(_, _, _, APID, _, _, _, _, _), APID).

	sequence_flags(ccsds_packet(_, _, _, _, SeqFlagsCode, _, _, _, _), Flags) :-
		sequence_flags_atom(SeqFlagsCode, Flags).

	sequence_count(ccsds_packet(_, _, _, _, _, SeqCount, _, _, _), SeqCount).

	data_length(ccsds_packet(_, _, _, _, _, _, DataLength, _, _), DataLength).

	user_data(ccsds_packet(_, _, _, _, _, _, _, _, UserData), UserData).

	secondary_header(ccsds_packet(_, _, _, _, _, _, _, SecHeader, _), SecHeader).

	secondary_header_time(Packet, cuc_time(Coarse, Fine)) :-
		secondary_header(Packet, secondary_header(Bytes)),
		Bytes = [B0, B1, B2, B3| Rest],
		Coarse is (B0 << 24) \/ (B1 << 16) \/ (B2 << 8) \/ B3,
		(	Rest = [F0, F1| _] ->
			Fine is (F0 << 8) \/ F1
		;	Rest = [F0| _] ->
			Fine is F0
		;	Fine = 0
		).

	% Sequence flags mapping
	sequence_flags_atom(0, continuation).
	sequence_flags_atom(1, first).
	sequence_flags_atom(2, last).
	sequence_flags_atom(3, standalone).

	% Secondary header flag mapping
	secondary_header_flag_atom(0, absent).
	secondary_header_flag_atom(1, present).

	% DCG rules for decoding

	decode_packets([Packet| Packets]) -->
		decode_packet(Packet),
		!,
		decode_packets(Packets).
	decode_packets([]) -->
		[].

	decode_packet(ccsds_packet(Version, Type, SecHeaderFlag, APID, SeqFlags, SeqCount, DataLength, SecHeader, UserData)) -->
		decode_primary_header(Version, Type, SecHeaderFlag, APID, SeqFlags, SeqCount, DataLength),
		decode_secondary_header(SecHeaderFlag, SecHeader, SecHeaderLen),
		{	ActualLength is DataLength + 1 - SecHeaderLen,
			length(UserData, ActualLength)
		},
		bytes(UserData).

	decode_primary_header(Version, Type, SecHeaderFlag, APID, SeqFlags, SeqCount, DataLength) -->
		[Byte0, Byte1, Byte2, Byte3, Byte4, Byte5],
		{	% First two bytes: Version(3) + Type(1) + SecHeader(1) + APID(11)
			Version is (Byte0 >> 5) /\ 0x07,
			Type is (Byte0 >> 4) /\ 0x01,
			SecHeaderFlag is (Byte0 >> 3) /\ 0x01,
			APID is ((Byte0 /\ 0x07) << 8) \/ Byte1,
			% Bytes 2-3: SeqFlags(2) + SeqCount(14)
			SeqFlags is (Byte2 >> 6) /\ 0x03,
			SeqCount is ((Byte2 /\ 0x3F) << 8) \/ Byte3,
			% Bytes 4-5: DataLength(16)
			DataLength is (Byte4 << 8) \/ Byte5
		}.

	% Secondary header decoding - uses parameter to determine length
	decode_secondary_header(0, none, 0) -->
		% No secondary header when flag is 0
		!,
		[].
	decode_secondary_header(1, SecHeader, SecHeaderLen) -->
		% Secondary header present - use parameterized length
		{	_SecondaryHeaderLength_ > 0,
			!,
			SecHeaderLen = _SecondaryHeaderLength_,
			length(SecHeaderBytes, SecHeaderLen)
		},
		bytes(SecHeaderBytes),
		{	SecHeader = secondary_header(SecHeaderBytes)
		}.
	decode_secondary_header(1, none, 0) -->
		% Secondary header flag is 1 but length parameter is 0 - skip parsing
		[].

	% DCG rules for encoding

	encode_packet(ccsds_packet(Version, Type, SecHeaderFlag, APID, SeqFlags, SeqCount, DataLength, SecHeader, UserData)) -->
		encode_primary_header(Version, Type, SecHeaderFlag, APID, SeqFlags, SeqCount, DataLength),
		encode_secondary_header(SecHeader),
		bytes(UserData).

	encode_secondary_header(none) -->
		!,
		[].
	encode_secondary_header(secondary_header(Bytes)) -->
		bytes(Bytes).

	encode_primary_header(Version, Type, SecHeaderFlag, APID, SeqFlags, SeqCount, DataLength) -->
		{	% Build Byte0: Version(3) + Type(1) + SecHeader(1) + APID_high(3)
			Byte0 is ((Version /\ 0x07) << 5) \/
			         ((Type /\ 0x01) << 4) \/
			         ((SecHeaderFlag /\ 0x01) << 3) \/
			         ((APID >> 8) /\ 0x07),
			% Build Byte1: APID_low(8)
			Byte1 is APID /\ 0xFF,
			% Build Byte2: SeqFlags(2) + SeqCount_high(6)
			Byte2 is ((SeqFlags /\ 0x03) << 6) \/
			         ((SeqCount >> 8) /\ 0x3F),
			% Build Byte3: SeqCount_low(8)
			Byte3 is SeqCount /\ 0xFF,
			% Build Bytes 4-5: DataLength(16)
			Byte4 is (DataLength >> 8) /\ 0xFF,
			Byte5 is DataLength /\ 0xFF
		},
		[Byte0, Byte1, Byte2, Byte3, Byte4, Byte5].

	% Helper DCG for reading/writing byte lists
	bytes([]) -->
		[].
	bytes([Byte| Bytes]) -->
		[Byte],
		bytes(Bytes).

:- end_object.


:- object(ccsds,
	extends(ccsds(0))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-12-05,
		comment is 'CCSDS Space Packet parser with no secondary header parsing. For secondary header support, use ccsds(Length) where Length is the secondary header size in bytes.'
	]).

:- end_object.
