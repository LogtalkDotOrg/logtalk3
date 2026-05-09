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


:- object(ccsds_tm_frames(_FrameLength_, _SecondaryHeaderLength_, _HasFECF_),
	implements(ccsds_frame_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-09,
		comment is 'CCSDS telemetry transfer frame parser and generator.',
		parameters is [
			'FrameLength' - 'Fixed telemetry transfer frame length in octets.',
			'SecondaryHeaderLength' - 'Length in octets of the telemetry transfer frame secondary header when present.',
			'HasFECF' - 'Boolean indicating if frames include the frame error control field.'
		]
	]).

	:- public(ocf_flag/2).
	:- mode(ocf_flag(+compound, -atom), one).
	:- info(ocf_flag/2, [
		comment is 'Extracts the operational control field flag. Returns ``absent`` or ``present``.',
		argnames is ['Frame', 'Flag']
	]).

	:- public(master_channel_frame_count/2).
	:- mode(master_channel_frame_count(+compound, -integer), one).
	:- info(master_channel_frame_count/2, [
		comment is 'Extracts the master channel frame count.',
		argnames is ['Frame', 'Count']
	]).

	:- public(virtual_channel_frame_count/2).
	:- mode(virtual_channel_frame_count(+compound, -integer), one).
	:- info(virtual_channel_frame_count/2, [
		comment is 'Extracts the virtual channel frame count.',
		argnames is ['Frame', 'Count']
	]).

	:- public(secondary_header_flag/2).
	:- mode(secondary_header_flag(+compound, -atom), one).
	:- info(secondary_header_flag/2, [
		comment is 'Extracts the telemetry transfer frame secondary header flag. Returns ``absent`` or ``present``.',
		argnames is ['Frame', 'Flag']
	]).

	:- public(synchronization_flag/2).
	:- mode(synchronization_flag(+compound, -integer), one).
	:- info(synchronization_flag/2, [
		comment is 'Extracts the synchronization flag value.',
		argnames is ['Frame', 'Flag']
	]).

	:- public(packet_order_flag/2).
	:- mode(packet_order_flag(+compound, -integer), one).
	:- info(packet_order_flag/2, [
		comment is 'Extracts the packet order flag value.',
		argnames is ['Frame', 'Flag']
	]).

	:- public(segment_length_identifier/2).
	:- mode(segment_length_identifier(+compound, -integer), one).
	:- info(segment_length_identifier/2, [
		comment is 'Extracts the segment length identifier value.',
		argnames is ['Frame', 'Identifier']
	]).

	:- public(first_header_pointer/2).
	:- mode(first_header_pointer(+compound, -integer), one).
	:- info(first_header_pointer/2, [
		comment is 'Extracts the first header pointer value.',
		argnames is ['Frame', 'Pointer']
	]).

	:- public(secondary_header/2).
	:- mode(secondary_header(+compound, -compound), one).
	:- info(secondary_header/2, [
		comment is 'Extracts the telemetry transfer frame secondary header. Returns ``none`` when absent.',
		argnames is ['Frame', 'SecondaryHeader']
	]).

	:- public(update_fecf/2).
	:- mode(update_fecf(+compound, -compound), one_or_error).
	:- info(update_fecf/2, [
		comment is 'Computes the correct telemetry transfer frame FECF for the selected object configuration and returns the corresponding updated frame term.',
		argnames is ['Frame', 'UpdatedFrame']
	]).

	:- public(verify_fecf/1).
	:- mode(verify_fecf(@compound), zero_or_one).
	:- info(verify_fecf/1, [
		comment is 'True if the telemetry transfer frame term contains the correct FECF for the selected object configuration.',
		argnames is ['Frame']
	]).

	:- uses(list, [
		append/3, take/4
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(reader, [
		file_to_bytes/2, stream_to_bytes/2
	]).

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(file(File), Frames) :-
		!,
		file_to_bytes(File, Bytes),
		parse_bytes(Bytes, Frames).
	parse(stream(Stream), Frames) :-
		!,
		stream_to_bytes(Stream, Bytes),
		parse_bytes(Bytes, Frames).
	parse(bytes(Bytes), Frames) :-
		!,
		parse_bytes(Bytes, Frames).
	parse(Source, _) :-
		domain_error(ccsds_frame_source, Source).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(file(File), Frames) :-
		!,
		open(File, write, Stream, [type(binary)]),
		generate_all_stream(Frames, Stream),
		close(Stream).
	generate(stream(Stream), Frames) :-
		!,
		generate_all_stream(Frames, Stream).
	generate(bytes(Bytes), Frames) :-
		!,
		generate_all_bytes(Frames, Bytes).
	generate(Sink, _) :-
		domain_error(ccsds_frame_sink, Sink).

	generate(Frame, _, _) :-
		var(Frame),
		instantiation_error.
	generate(Frame, Bytes, Tail) :-
		refresh_fecf(Frame, UpdatedFrame),
		!,
		encode_frame(UpdatedFrame, Bytes, Tail).
	generate(Frame, _, _) :-
		domain_error(ccsds_frame_term, Frame).

	valid(Frame) :-
		refresh_fecf(Frame, UpdatedFrame),
		Frame == UpdatedFrame.

	update_fecf(Frame, UpdatedFrame) :-
		(   var(Frame) ->
			instantiation_error
		;   refresh_fecf(Frame, UpdatedFrame) ->
			true
		;   domain_error(ccsds_frame_term, Frame)
		).

	verify_fecf(Frame) :-
		refresh_fecf(Frame, UpdatedFrame),
		Frame == UpdatedFrame.

	frame_template(Frame) :-
		valid_parameters,
		Frame = tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, FECF),
		valid(between(integer, 0, 3), Version),
		valid(between(integer, 0, 1023), SpacecraftId),
		valid(between(integer, 0, 7), VirtualChannelId),
		valid(between(integer, 0, 1), OCFFlag),
		valid(between(integer, 0, 255), MasterChannelFrameCount),
		valid(between(integer, 0, 255), VirtualChannelFrameCount),
		valid(between(integer, 0, 1), SecondaryHeaderFlag),
		valid(between(integer, 0, 1), SynchronizationFlag),
		valid(between(integer, 0, 1), PacketOrderFlag),
		valid(between(integer, 0, 3), SegmentLengthIdentifier),
		valid(between(integer, 0, 2047), FirstHeaderPointer),
		valid_secondary_header(SecondaryHeaderFlag, SecondaryHeader, SecondaryHeaderLength),
		valid_ocf(OCFFlag, OCF, OCFLen),
		valid_template_fecf(_HasFECF_, FECF, FECFLen),
		DataFieldLength is _FrameLength_ - 6 - SecondaryHeaderLength - OCFLen - FECFLen,
		DataFieldLength >= 0,
		valid(list(byte, DataFieldLength), DataField).

	refresh_fecf(Frame, UpdatedFrame) :-
		frame_template(Frame),
		(   _HasFECF_ == true ->
			compute_fecf(Frame, FECF),
			replace_fecf(Frame, FECF, UpdatedFrame)
		;   UpdatedFrame = Frame
		).

	frame_type(tm_transfer_frame(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _), tm).

	version(tm_transfer_frame(Version, _, _, _, _, _, _, _, _, _, _, _, _, _, _), Version).

	spacecraft_id(tm_transfer_frame(_, SpacecraftId, _, _, _, _, _, _, _, _, _, _, _, _, _), SpacecraftId).

	virtual_channel_id(tm_transfer_frame(_, _, VirtualChannelId, _, _, _, _, _, _, _, _, _, _, _, _), VirtualChannelId).

	data_field(tm_transfer_frame(_, _, _, _, _, _, _, _, _, _, _, _, DataField, _, _), DataField).

	ocf(tm_transfer_frame(_, _, _, _, _, _, _, _, _, _, _, _, _, OCF, _), OCF).

	fecf(tm_transfer_frame(_, _, _, _, _, _, _, _, _, _, _, _, _, _, FECF), FECF).

	ocf_flag(tm_transfer_frame(_, _, _, OCFFlag, _, _, _, _, _, _, _, _, _, _, _), Flag) :-
		flag_atom(OCFFlag, Flag).

	master_channel_frame_count(tm_transfer_frame(_, _, _, _, Count, _, _, _, _, _, _, _, _, _, _), Count).

	virtual_channel_frame_count(tm_transfer_frame(_, _, _, _, _, Count, _, _, _, _, _, _, _, _, _), Count).

	secondary_header_flag(tm_transfer_frame(_, _, _, _, _, _, SecondaryHeaderFlag, _, _, _, _, _, _, _, _), Flag) :-
		flag_atom(SecondaryHeaderFlag, Flag).

	synchronization_flag(tm_transfer_frame(_, _, _, _, _, _, _, Flag, _, _, _, _, _, _, _), Flag).

	packet_order_flag(tm_transfer_frame(_, _, _, _, _, _, _, _, Flag, _, _, _, _, _, _), Flag).

	segment_length_identifier(tm_transfer_frame(_, _, _, _, _, _, _, _, _, Identifier, _, _, _, _, _), Identifier).

	first_header_pointer(tm_transfer_frame(_, _, _, _, _, _, _, _, _, _, Pointer, _, _, _, _), Pointer).

	secondary_header(tm_transfer_frame(_, _, _, _, _, _, _, _, _, _, _, SecondaryHeader, _, _, _), SecondaryHeader).

	parse_bytes(Bytes, Frames) :-
		(   valid_parameters,
			valid(list(byte), Bytes),
			parse_all(Bytes, Frames) ->
			true
		;	domain_error(ccsds_frame_byte_sequence, Bytes)
		).

	parse_all([], []).
	parse_all(Bytes, [Frame| Frames]) :-
		take(_FrameLength_, Bytes, FrameBytes, Rest),
		decode_frame(FrameBytes, Frame),
		verify_fecf(Frame),
		parse_all(Rest, Frames).

	decode_frame([Byte0, Byte1, Byte2, Byte3, Byte4, Byte5| Tail], tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, FECF)) :-
		Version is (Byte0 >> 6) /\ 0x03,
		SpacecraftId is ((Byte0 /\ 0x3F) << 4) \/ ((Byte1 >> 4) /\ 0x0F),
		VirtualChannelId is (Byte1 >> 1) /\ 0x07,
		OCFFlag is Byte1 /\ 0x01,
		MasterChannelFrameCount = Byte2,
		VirtualChannelFrameCount = Byte3,
		SecondaryHeaderFlag is (Byte4 >> 7) /\ 0x01,
		SynchronizationFlag is (Byte4 >> 6) /\ 0x01,
		PacketOrderFlag is (Byte4 >> 5) /\ 0x01,
		SegmentLengthIdentifier is (Byte4 >> 3) /\ 0x03,
		FirstHeaderPointer is ((Byte4 /\ 0x07) << 8) \/ Byte5,
		secondary_header_length(SecondaryHeaderFlag, SecondaryHeaderLength),
		ocf_length(OCFFlag, OCFLen),
		fecf_length(FECFLen),
		DataFieldLength is _FrameLength_ - 6 - SecondaryHeaderLength - OCFLen - FECFLen,
		DataFieldLength >= 0,
		take_optional_secondary_header(SecondaryHeaderFlag, SecondaryHeaderLength, Tail, Tail1, SecondaryHeader),
		take(DataFieldLength, Tail1, DataField, Tail2),
		take_optional_ocf(OCFFlag, Tail2, Tail3, OCF),
		take_optional_fecf(Tail3, [], FECF).

	encode_frame(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, FECF), Bytes, Tail) :-
		encode_frame_without_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, FECF), Bytes, Tail0),
		append_optional_fecf(FECF, Tail0, Tail).

	encode_frame_without_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, _), Bytes, Tail) :-
		Byte0 is ((Version /\ 0x03) << 6) \/ ((SpacecraftId >> 4) /\ 0x3F),
		Byte1 is ((SpacecraftId /\ 0x0F) << 4) \/ ((VirtualChannelId /\ 0x07) << 1) \/ (OCFFlag /\ 0x01),
		Byte2 is MasterChannelFrameCount,
		Byte3 is VirtualChannelFrameCount,
		Byte4 is ((SecondaryHeaderFlag /\ 0x01) << 7) \/ ((SynchronizationFlag /\ 0x01) << 6) \/ ((PacketOrderFlag /\ 0x01) << 5) \/ ((SegmentLengthIdentifier /\ 0x03) << 3) \/ ((FirstHeaderPointer >> 8) /\ 0x07),
		Byte5 is FirstHeaderPointer /\ 0xFF,
		Bytes = [Byte0, Byte1, Byte2, Byte3, Byte4, Byte5| Rest0],
		append_optional_secondary_header(SecondaryHeaderFlag, SecondaryHeader, Rest0, Rest1),
		append(DataField, Rest2, Rest1),
		append_optional_ocf(OCFFlag, OCF, Rest2, Tail).

	generate_all_stream([Frame| Frames], Stream) :-
		(   var(Frame) ->
			instantiation_error
		;   generate(Frame, Bytes, []) ->
			write_bytes(Bytes, Stream),
			generate_all_stream(Frames, Stream)
		;   domain_error(ccsds_frame_term, Frame)
		).
	generate_all_stream([], _).

	generate_all_bytes([Frame| Frames], Bytes) :-
		(   var(Frame) ->
			instantiation_error
		;   generate(Frame, Bytes, Tail) ->
			generate_all_bytes(Frames, Tail)
		;   domain_error(ccsds_frame_term, Frame)
		).
	generate_all_bytes([], []).

	take_optional_secondary_header(0, 0, Bytes, Bytes, none).
	take_optional_secondary_header(1, Length, Bytes, Rest, secondary_header(SecondaryHeaderBytes)) :-
		Length > 0,
		take(Length, Bytes, SecondaryHeaderBytes, Rest).

	take_optional_ocf(0, Bytes, Bytes, none).
	take_optional_ocf(1, Bytes, Rest, ocf(OCFBytes)) :-
		take(4, Bytes, OCFBytes, Rest).

	take_optional_fecf(Bytes, Rest, FECF) :-
		(   _HasFECF_ == true ->
			take(2, Bytes, FECFBytes, Rest),
			FECF = fecf(FECFBytes)
		;   Rest = Bytes,
			FECF = none
		).

	append_optional_secondary_header(0, none, Bytes, Bytes).
	append_optional_secondary_header(1, secondary_header(SecondaryHeaderBytes), Bytes, Rest) :-
		append(SecondaryHeaderBytes, Rest, Bytes).

	append_optional_ocf(0, none, Bytes, Bytes).
	append_optional_ocf(1, ocf(OCFBytes), Bytes, Rest) :-
		append(OCFBytes, Rest, Bytes).

	append_optional_fecf(FECF, Bytes, Rest) :-
		(   _HasFECF_ == true ->
			FECF = fecf(FECFBytes),
			append(FECFBytes, Rest, Bytes)
		;   FECF = none,
			Rest = Bytes
		).

	secondary_header_length(0, 0).
	secondary_header_length(1, Length) :-
		_SecondaryHeaderLength_ > 0,
		Length = _SecondaryHeaderLength_.

	ocf_length(0, 0).
	ocf_length(1, 4).

	fecf_length(Length) :-
		(   _HasFECF_ == true ->
			Length = 2
		;   Length = 0
		).

	valid_secondary_header(0, none, 0).
	valid_secondary_header(1, secondary_header(Bytes), Length) :-
		_SecondaryHeaderLength_ > 0,
		Length = _SecondaryHeaderLength_,
		valid(list(byte, Length), Bytes).

	valid_ocf(0, none, 0).
	valid_ocf(1, ocf(Bytes), 4) :-
		valid(list(byte, 4), Bytes).

	valid_template_fecf(false, none, 0).
	valid_template_fecf(true, none, 2).
	valid_template_fecf(true, fecf(Bytes), 2) :-
		valid(list(byte, 2), Bytes).

	compute_fecf(Frame, fecf(Bytes)) :-
		encode_frame_without_fecf(Frame, FrameBytes, []),
		compute_fecf_bytes(FrameBytes, Bytes).

	replace_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, _), FECF, tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, FECF)).

	compute_fecf_bytes(FrameBytes, [HighByte, LowByte]) :-
		compute_fecf_crc(FrameBytes, 0xFFFF, CRC),
		HighByte is (CRC >> 8) /\ 0xFF,
		LowByte is CRC /\ 0xFF.

	compute_fecf_crc([Byte| Bytes], CRC0, CRC) :-
		CRC1 is xor(CRC0, Byte << 8) /\ 0xFFFF,
		compute_fecf_crc_bits(8, CRC1, CRC2),
		compute_fecf_crc(Bytes, CRC2, CRC).
	compute_fecf_crc([], CRC, CRC).

	compute_fecf_crc_bits(0, CRC, CRC) :-
		!.
	compute_fecf_crc_bits(Bit, CRC0, CRC) :-
		Bit > 0,
		(   CRC0 /\ 0x8000 =:= 0 ->
			CRC1 is (CRC0 << 1) /\ 0xFFFF
		;   CRC1 is xor((CRC0 << 1) /\ 0xFFFF, 0x1021)
		),
		NextBit is Bit - 1,
		compute_fecf_crc_bits(NextBit, CRC1, CRC).

	write_bytes([], _).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

	valid_parameters :-
		integer(_FrameLength_),
		_FrameLength_ >= 6,
		integer(_SecondaryHeaderLength_),
		_SecondaryHeaderLength_ >= 0,
		(   _HasFECF_ == true ->
			FECFLen = 2
		;   (   _HasFECF_ == false ->
				FECFLen = 0
			;   fail
			)
		),
		_FrameLength_ >= 6 + FECFLen.

	flag_atom(0, absent).
	flag_atom(1, present).

:- end_object.
