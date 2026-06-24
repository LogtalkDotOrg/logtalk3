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


:- object(ccsds_tc_frames(_FrameLength_, _SegmentHeaderLength_, _HasFECF_),
	implements(ccsds_frame_protocol)).

	:- info([
		version is 1:0:2,
		author is 'Paulo Moura',
		date is 2026-06-24,
		comment is 'CCSDS telecommand transfer frame parser and generator.',
		parameters is [
			'FrameLength' - 'Fixed telecommand transfer frame length in octets.',
			'SegmentHeaderLength' - 'Length in octets of the optional segment header when present.',
			'HasFECF' - 'Boolean indicating if frames include the frame error control field.'
		]
	]).

	:- public(bypass_flag/2).
	:- mode(bypass_flag(+compound, -integer), one).
	:- info(bypass_flag/2, [
		comment is 'Extracts the bypass flag value.',
		argnames is ['Frame', 'Flag']
	]).

	:- public(control_command_flag/2).
	:- mode(control_command_flag(+compound, -integer), one).
	:- info(control_command_flag/2, [
		comment is 'Extracts the control command flag value.',
		argnames is ['Frame', 'Flag']
	]).

	:- public(sequence_number/2).
	:- mode(sequence_number(+compound, -integer), one).
	:- info(sequence_number/2, [
		comment is 'Extracts the frame sequence number.',
		argnames is ['Frame', 'SequenceNumber']
	]).

	:- public(segment_header/2).
	:- mode(segment_header(+compound, -compound), one).
	:- info(segment_header/2, [
		comment is 'Extracts the optional segment header. Returns ``none`` when absent.',
		argnames is ['Frame', 'SegmentHeader']
	]).

	:- public(update_fecf/2).
	:- mode(update_fecf(+compound, -compound), one_or_error).
	:- info(update_fecf/2, [
		comment is 'Computes the correct telecommand transfer frame FECF for the selected object configuration and returns the corresponding updated frame term.',
		argnames is ['Frame', 'UpdatedFrame'],
		exceptions is [
			'``Frame`` is a variable' - instantiation_error,
			'``Frame`` is neither a variable nor a valid telecommand transfer frame term' - domain_error(ccsds_frame_term, 'Frame')
		]
	]).

	:- public(verify_fecf/1).
	:- mode(verify_fecf(@compound), zero_or_one).
	:- info(verify_fecf/1, [
		comment is 'True if the telecommand transfer frame term contains the correct FECF for the selected object configuration.',
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
		(	catch(
				generate_all_stream(Frames, Stream),
				Error,
				(	catch(close(Stream), _, true),
					throw(Error)
				)
			) ->
			close(Stream)
		; 	catch(close(Stream), _, true),
			fail
		).
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
		Frame = tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, FECF),
		valid(between(integer, 0, 3), Version),
		valid(between(integer, 0, 1), BypassFlag),
		valid(between(integer, 0, 1), ControlCommandFlag),
		valid(between(integer, 0, 1023), SpacecraftId),
		valid(between(integer, 0, 63), VirtualChannelId),
		valid(between(integer, 0, 255), SequenceNumber),
		valid_segment_header(SegmentHeader, SegmentHeaderLength),
		valid_template_fecf(FECF, FECFLen),
		DataFieldLength is _FrameLength_ - 5 - SegmentHeaderLength - FECFLen,
		DataFieldLength >= 0,
		valid(list(byte, DataFieldLength), DataField).

	refresh_fecf(Frame, UpdatedFrame) :-
		frame_template(Frame),
		(	_HasFECF_ == true ->
			compute_fecf(Frame, FECF),
			replace_fecf(Frame, FECF, UpdatedFrame)
		;	UpdatedFrame = Frame
		).

	frame_type(tc_transfer_frame(_, _, _, _, _, _, _, _, _), tc).

	version(tc_transfer_frame(Version, _, _, _, _, _, _, _, _), Version).

	bypass_flag(tc_transfer_frame(_, Flag, _, _, _, _, _, _, _), Flag).

	control_command_flag(tc_transfer_frame(_, _, Flag, _, _, _, _, _, _), Flag).

	spacecraft_id(tc_transfer_frame(_, _, _, SpacecraftId, _, _, _, _, _), SpacecraftId).

	virtual_channel_id(tc_transfer_frame(_, _, _, _, VirtualChannelId, _, _, _, _), VirtualChannelId).

	sequence_number(tc_transfer_frame(_, _, _, _, _, SequenceNumber, _, _, _), SequenceNumber).

	segment_header(tc_transfer_frame(_, _, _, _, _, _, SegmentHeader, _, _), SegmentHeader).

	data_field(tc_transfer_frame(_, _, _, _, _, _, _, DataField, _), DataField).

	ocf(tc_transfer_frame(_, _, _, _, _, _, _, _, _), none).

	fecf(tc_transfer_frame(_, _, _, _, _, _, _, _, FECF), FECF).

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

	decode_frame([Byte0, Byte1, Byte2, Byte3, Byte4| Tail], tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, FECF)) :-
		FlagsAndSpacecraftId is (Byte0 << 8) + Byte1,
		Version is (FlagsAndSpacecraftId >> 14) /\ 0x03,
		BypassFlag is (FlagsAndSpacecraftId >> 13) /\ 0x01,
		ControlCommandFlag is (FlagsAndSpacecraftId >> 12) /\ 0x01,
		Reserved is (FlagsAndSpacecraftId >> 10) /\ 0x03,
		Reserved =:= 0,
		SpacecraftId is FlagsAndSpacecraftId /\ 0x03FF,
		VirtualChannelIdAndLength is (Byte2 << 8) + Byte3,
		VirtualChannelId is (VirtualChannelIdAndLength >> 10) /\ 0x3F,
		FrameLengthToken is VirtualChannelIdAndLength /\ 0x03FF,
		FrameLength is FrameLengthToken + 1,
		FrameLength =:= _FrameLength_,
		SequenceNumber = Byte4,
		fecf_length(FECFLen),
		DataFieldLength is _FrameLength_ - 5 - _SegmentHeaderLength_ - FECFLen,
		DataFieldLength >= 0,
		take_optional_segment_header(_SegmentHeaderLength_, Tail, Tail1, SegmentHeader),
		take(DataFieldLength, Tail1, DataField, Tail2),
		take_optional_fecf(Tail2, [], FECF).

	encode_frame(tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, FECF), Bytes, Tail) :-
		encode_frame_without_fecf(tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, FECF), Bytes, Tail0),
		append_optional_fecf(FECF, Tail0, Tail).

	encode_frame_without_fecf(tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, _), Bytes, Tail) :-
		FlagsAndSpacecraftId is ((Version /\ 0x03) << 14) \/ ((BypassFlag /\ 0x01) << 13) \/ ((ControlCommandFlag /\ 0x01) << 12) \/ (SpacecraftId /\ 0x03FF),
		VirtualChannelIdAndLength is ((VirtualChannelId /\ 0x3F) << 10) \/ ((_FrameLength_ - 1) /\ 0x03FF),
		Byte0 is (FlagsAndSpacecraftId >> 8) /\ 0xFF,
		Byte1 is FlagsAndSpacecraftId /\ 0xFF,
		Byte2 is (VirtualChannelIdAndLength >> 8) /\ 0xFF,
		Byte3 is VirtualChannelIdAndLength /\ 0xFF,
		Byte4 is SequenceNumber /\ 0xFF,
		Bytes = [Byte0, Byte1, Byte2, Byte3, Byte4| Rest0],
		append_optional_segment_header(SegmentHeader, Rest0, Rest1),
		append(DataField, Tail, Rest1).

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

	take_optional_segment_header(0, Bytes, Bytes, none).
	take_optional_segment_header(Length, Bytes, Rest, segment_header(SegmentHeaderBytes)) :-
		Length > 0,
		take(Length, Bytes, SegmentHeaderBytes, Rest).

	take_optional_fecf(Bytes, Rest, FECF) :-
		(	_HasFECF_ == true ->
			take(2, Bytes, FECFBytes, Rest),
			FECF = fecf(FECFBytes)
		;	Rest = Bytes,
			FECF = none
		).

	append_optional_segment_header(none, Bytes, Bytes).
	append_optional_segment_header(segment_header(SegmentHeaderBytes), Bytes, Rest) :-
		append(SegmentHeaderBytes, Rest, Bytes).

	append_optional_fecf(FECF, Bytes, Rest) :-
		(	_HasFECF_ == true ->
			FECF = fecf(FECFBytes),
			append(FECFBytes, Rest, Bytes)
		;	FECF = none,
			Rest = Bytes
		).

	fecf_length(Length) :-
		(	_HasFECF_ == true ->
			Length = 2
		;	Length = 0
		).

	valid_segment_header(none, 0) :-
		_SegmentHeaderLength_ =:= 0,
		!.
	valid_segment_header(segment_header(Bytes), Length) :-
		_SegmentHeaderLength_ > 0,
		Length = _SegmentHeaderLength_,
		valid(list(byte, Length), Bytes).

	valid_template_fecf(none, Length) :-
		(	_HasFECF_ == true ->
			Length = 2
		;	_HasFECF_ == false ->
			Length = 0
		;	fail
		).
	valid_template_fecf(fecf(Bytes), 2) :-
		_HasFECF_ == true,
		valid(list(byte, 2), Bytes).

	compute_fecf(Frame, fecf(Bytes)) :-
		encode_frame_without_fecf(Frame, FrameBytes, []),
		compute_fecf_bytes(FrameBytes, Bytes).

	replace_fecf(tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, _), FECF, tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, FECF)).

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

	valid_parameters :-
		integer(_FrameLength_),
		_FrameLength_ >= 5,
		_FrameLength_ =< 1024,
		integer(_SegmentHeaderLength_),
		_SegmentHeaderLength_ >= 0,
		(	_HasFECF_ == true ->
			FECFLen = 2
		;	(   _HasFECF_ == false ->
				FECFLen = 0
			;	fail
			)
		),
		_FrameLength_ >= 5 + _SegmentHeaderLength_ + FECFLen.

:- end_object.
