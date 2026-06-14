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


:- object(ccsds_aos_frames(_FrameLength_, _InsertZoneLength_, _HasOCF_, _HasFECF_),
	implements(ccsds_frame_protocol)).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-06-14,
		comment is 'CCSDS advanced orbiting systems transfer frame parser and generator.',
		parameters is [
			'FrameLength' - 'Fixed AOS transfer frame length in octets.',
			'InsertZoneLength' - 'Length in octets of the optional insert zone when present.',
			'HasOCF' - 'Boolean indicating if frames include the operational control field.',
			'HasFECF' - 'Boolean indicating if frames include the frame error control field.'
		]
	]).

	:- public(virtual_channel_frame_count/2).
	:- mode(virtual_channel_frame_count(+compound, -integer), one).
	:- info(virtual_channel_frame_count/2, [
		comment is 'Extracts the virtual channel frame count.',
		argnames is ['Frame', 'Count']
	]).

	:- public(signaling_field/2).
	:- mode(signaling_field(+compound, -compound), one).
	:- info(signaling_field/2, [
		comment is 'Extracts the signaling field term.',
		argnames is ['Frame', 'SignalingField']
	]).

	:- public(insert_zone/2).
	:- mode(insert_zone(+compound, -compound), one).
	:- info(insert_zone/2, [
		comment is 'Extracts the optional insert zone. Returns ``none`` when absent.',
		argnames is ['Frame', 'InsertZone']
	]).

	:- public(update_fecf/2).
	:- mode(update_fecf(+compound, -compound), one_or_error).
	:- info(update_fecf/2, [
		comment is 'Computes the correct AOS transfer frame FECF for the selected object configuration and returns the corresponding updated frame term.',
		argnames is ['Frame', 'UpdatedFrame'],
		exceptions is [
			'``Frame`` is a variable' - instantiation_error,
			'``Frame`` is neither a variable nor a valid AOS transfer frame term' - domain_error(ccsds_frame_term, 'Frame')
		]
	]).

	:- public(verify_fecf/1).
	:- mode(verify_fecf(@compound), zero_or_one).
	:- info(verify_fecf/1, [
		comment is 'True if the AOS transfer frame term contains the correct FECF for the selected object configuration.',
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
		(	var(Frame) ->
			instantiation_error
		;	refresh_fecf(Frame, UpdatedFrame) ->
			true
		;	domain_error(ccsds_frame_term, Frame)
		).

	verify_fecf(Frame) :-
		refresh_fecf(Frame, UpdatedFrame),
		Frame == UpdatedFrame.

	frame_template(Frame) :-
		valid_parameters,
		Frame = aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, FECF),
		valid(between(integer, 0, 3), Version),
		valid(between(integer, 0, 1023), SpacecraftId),
		valid(between(integer, 0, 63), VirtualChannelId),
		valid_virtual_channel_frame_count(VirtualChannelFrameCount),
		valid_signaling_field(SignalingField, SpacecraftId, VirtualChannelFrameCount),
		valid_insert_zone(InsertZone),
		valid_ocf(OCF, OCFLen),
		valid_template_fecf(FECF, FECFLen),
		DataFieldLength is _FrameLength_ - 6 - _InsertZoneLength_ - OCFLen - FECFLen,
		DataFieldLength >= 0,
		valid(list(byte, DataFieldLength), DataField).

	refresh_fecf(Frame, UpdatedFrame) :-
		frame_template(Frame),
		(	_HasFECF_ == true ->
			compute_fecf(Frame, FECF),
			replace_fecf(Frame, FECF, UpdatedFrame)
		;	UpdatedFrame = Frame
		).

	frame_type(aos_transfer_frame(_, _, _, _, _, _, _, _, _), aos).

	version(aos_transfer_frame(Version, _, _, _, _, _, _, _, _), Version).

	spacecraft_id(aos_transfer_frame(_, SpacecraftId, _, _, _, _, _, _, _), SpacecraftId).

	virtual_channel_id(aos_transfer_frame(_, _, VirtualChannelId, _, _, _, _, _, _), VirtualChannelId).

	virtual_channel_frame_count(aos_transfer_frame(_, _, _, Count, _, _, _, _, _), Count).

	signaling_field(aos_transfer_frame(_, _, _, _, SignalingField, _, _, _, _), SignalingField).

	insert_zone(aos_transfer_frame(_, _, _, _, _, InsertZone, _, _, _), InsertZone).

	data_field(aos_transfer_frame(_, _, _, _, _, _, DataField, _, _), DataField).

	ocf(aos_transfer_frame(_, _, _, _, _, _, _, OCF, _), OCF).

	fecf(aos_transfer_frame(_, _, _, _, _, _, _, _, FECF), FECF).

	parse_bytes(Bytes, Frames) :-
		(	valid_parameters,
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

	decode_frame([Byte0, Byte1, Byte2, Byte3, Byte4, Byte5| Tail], aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, FECF)) :-
		Version is (Byte0 >> 6) /\ 0x03,
		SpacecraftIdExtension is (Byte5 >> 4) /\ 0x03,
		SpacecraftIdLow is ((Byte0 /\ 0x3F) << 2) \/ ((Byte1 >> 6) /\ 0x03),
		SpacecraftId is (SpacecraftIdExtension << 8) \/ SpacecraftIdLow,
		VirtualChannelId is Byte1 /\ 0x3F,
		BaseFrameCount is (Byte2 << 16) \/ (Byte3 << 8) \/ Byte4,
		ReplayFlag is (Byte5 >> 7) /\ 0x01,
		FrameCountUsageFlag is (Byte5 >> 6) /\ 0x01,
		FrameCountCycle is Byte5 /\ 0x0F,
		(	FrameCountUsageFlag =:= 1 ->
			VirtualChannelFrameCount is (FrameCountCycle << 24) \/ BaseFrameCount
		;	VirtualChannelFrameCount = BaseFrameCount
		),
		SignalingField = signaling_field(ReplayFlag, FrameCountUsageFlag, SpacecraftIdExtension, FrameCountCycle),
		ocf_length(OCFLen),
		fecf_length(FECFLen),
		DataFieldLength is _FrameLength_ - 6 - _InsertZoneLength_ - OCFLen - FECFLen,
		DataFieldLength >= 0,
		take_optional_insert_zone(Tail, Tail1, InsertZone),
		take(DataFieldLength, Tail1, DataField, Tail2),
		take_optional_ocf(Tail2, Tail3, OCF),
		take_optional_fecf(Tail3, [], FECF).

	encode_frame(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, signaling_field(ReplayFlag, FrameCountUsageFlag, SpacecraftIdExtension, FrameCountCycle), InsertZone, DataField, OCF, FECF), Bytes, Tail) :-
		encode_frame_without_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, signaling_field(ReplayFlag, FrameCountUsageFlag, SpacecraftIdExtension, FrameCountCycle), InsertZone, DataField, OCF, FECF), Bytes, Tail0),
		append_optional_fecf(FECF, Tail0, Tail).

	encode_frame_without_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, signaling_field(ReplayFlag, FrameCountUsageFlag, SpacecraftIdExtension, FrameCountCycle), InsertZone, DataField, OCF, _), Bytes, Tail) :-
		BaseFrameCount is VirtualChannelFrameCount /\ 0xFFFFFF,
		Byte0 is ((Version /\ 0x03) << 6) \/ ((SpacecraftId /\ 0x0FF) >> 2),
		Byte1 is ((SpacecraftId /\ 0x03) << 6) \/ (VirtualChannelId /\ 0x3F),
		Byte2 is (BaseFrameCount >> 16) /\ 0xFF,
		Byte3 is (BaseFrameCount >> 8) /\ 0xFF,
		Byte4 is BaseFrameCount /\ 0xFF,
		Byte5 is ((ReplayFlag /\ 0x01) << 7) \/ ((FrameCountUsageFlag /\ 0x01) << 6) \/ ((SpacecraftIdExtension /\ 0x03) << 4) \/ (FrameCountCycle /\ 0x0F),
		Bytes = [Byte0, Byte1, Byte2, Byte3, Byte4, Byte5| Rest0],
		append_optional_insert_zone(InsertZone, Rest0, Rest1),
		append(DataField, Rest2, Rest1),
		append_optional_ocf(OCF, Rest2, Tail).

	generate_all_stream([Frame| Frames], Stream) :-
		(	var(Frame) ->
			instantiation_error
		;	generate(Frame, Bytes, []) ->
			write_bytes(Bytes, Stream),
			generate_all_stream(Frames, Stream)
		;	domain_error(ccsds_frame_term, Frame)
		).
	generate_all_stream([], _).

	generate_all_bytes([Frame| Frames], Bytes) :-
		(	var(Frame) ->
			instantiation_error
		;	generate(Frame, Bytes, Tail) ->
			generate_all_bytes(Frames, Tail)
		;	domain_error(ccsds_frame_term, Frame)
		).
	generate_all_bytes([], []).

	take_optional_insert_zone(Bytes, Rest, InsertZone) :-
		(	_InsertZoneLength_ =:= 0 ->
			Rest = Bytes,
			InsertZone = none
		;	take(_InsertZoneLength_, Bytes, InsertZoneBytes, Rest),
			InsertZone = insert_zone(InsertZoneBytes)
		).

	take_optional_ocf(Bytes, Rest, OCF) :-
		(	_HasOCF_ == true ->
			take(4, Bytes, OCFBytes, Rest),
			OCF = ocf(OCFBytes)
		;	Rest = Bytes,
			OCF = none
		).

	take_optional_fecf(Bytes, Rest, FECF) :-
		(	_HasFECF_ == true ->
			take(2, Bytes, FECFBytes, Rest),
			FECF = fecf(FECFBytes)
		;	Rest = Bytes,
			FECF = none
		).

	append_optional_insert_zone(none, Bytes, Bytes).
	append_optional_insert_zone(insert_zone(InsertZoneBytes), Bytes, Rest) :-
		append(InsertZoneBytes, Rest, Bytes).

	append_optional_ocf(OCF, Bytes, Rest) :-
		(	_HasOCF_ == true ->
			OCF = ocf(OCFBytes),
			append(OCFBytes, Rest, Bytes)
		;	OCF = none,
			Rest = Bytes
		).

	append_optional_fecf(FECF, Bytes, Rest) :-
		(	_HasFECF_ == true ->
			FECF = fecf(FECFBytes),
			append(FECFBytes, Rest, Bytes)
		;	FECF = none,
			Rest = Bytes
		).

	ocf_length(4) :-
		_HasOCF_ == true,
		!.
	ocf_length(0).

	fecf_length(2) :-
		_HasFECF_ == true,
		!.
	fecf_length(0).

	valid_parameters :-
		integer(_FrameLength_),
		_FrameLength_ >= 6,
		integer(_InsertZoneLength_),
		_InsertZoneLength_ >= 0,
		valid(boolean, _HasOCF_),
		valid(boolean, _HasFECF_),
		ocf_length(OCFLen),
		fecf_length(FECFLen),
		_FrameLength_ >= 6 + _InsertZoneLength_ + OCFLen + FECFLen.

	valid_virtual_channel_frame_count(Count) :-
		valid(between(integer, 0, 268435455), Count).

	valid_signaling_field(signaling_field(ReplayFlag, FrameCountUsageFlag, SpacecraftIdExtension, FrameCountCycle), SpacecraftId, VirtualChannelFrameCount) :-
		valid(between(integer, 0, 1), ReplayFlag),
		valid(between(integer, 0, 1), FrameCountUsageFlag),
		valid(between(integer, 0, 3), SpacecraftIdExtension),
		valid(between(integer, 0, 15), FrameCountCycle),
		SpacecraftIdExtension =:= (SpacecraftId >> 8),
		(	FrameCountUsageFlag =:= 1 ->
			FrameCountCycle =:= ((VirtualChannelFrameCount >> 24) /\ 0x0F)
		;	VirtualChannelFrameCount =< 16777215
		).

	valid_insert_zone(none) :-
		_InsertZoneLength_ =:= 0,
		!.
	valid_insert_zone(insert_zone(Bytes)) :-
		_InsertZoneLength_ > 0,
		valid(list(byte, _InsertZoneLength_), Bytes).

	valid_ocf(ocf(Bytes), 4) :-
		_HasOCF_ == true,
		!,
		valid(list(byte, 4), Bytes).
	valid_ocf(none, 0) :-
		_HasOCF_ == false.

	valid_template_fecf(fecf(Bytes), 2) :-
		_HasFECF_ == true,
		!,
		valid(list(byte, 2), Bytes).
	valid_template_fecf(none, 2) :-
		_HasFECF_ == true,
		!.
	valid_template_fecf(none, 0) :-
		_HasFECF_ == false.

	compute_fecf(Frame, fecf(Bytes)) :-
		encode_frame_without_fecf(Frame, FrameBytes, []),
		compute_fecf_bytes(FrameBytes, Bytes).

	replace_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, _), FECF, aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, FECF)).

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
		(	CRC0 /\ 0x8000 =:= 0 ->
			CRC1 is (CRC0 << 1) /\ 0xFFFF
		;	CRC1 is xor((CRC0 << 1) /\ 0xFFFF, 0x1021)
		),
		NextBit is Bit - 1,
		compute_fecf_crc_bits(NextBit, CRC1, CRC).

	write_bytes([], _).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

:- end_object.
