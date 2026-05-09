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


:- object(ccsds_frames).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-09,
		comment is 'Helpers for introspecting CCSDS transfer frame terms.'
	]).

	:- public(valid/1).
	:- mode(valid(@compound), zero_or_one).
	:- info(valid/1, [
		comment is 'True if the argument is a known CCSDS transfer frame term.',
		argnames is ['Frame']
	]).

	:- public(frame_type/2).
	:- mode(frame_type(+compound, -atom), one).
	:- info(frame_type/2, [
		comment is 'Returns the CCSDS transfer frame type for a known frame term.',
		argnames is ['Frame', 'Type']
	]).

	:- public(version/2).
	:- mode(version(+compound, -integer), one).
	:- info(version/2, [
		comment is 'Extracts the transfer frame version number from a known frame term.',
		argnames is ['Frame', 'Version']
	]).

	:- public(spacecraft_id/2).
	:- mode(spacecraft_id(+compound, -integer), one).
	:- info(spacecraft_id/2, [
		comment is 'Extracts the spacecraft identifier from a known frame term.',
		argnames is ['Frame', 'SpacecraftId']
	]).

	:- public(virtual_channel_id/2).
	:- mode(virtual_channel_id(+compound, -integer), one).
	:- info(virtual_channel_id/2, [
		comment is 'Extracts the virtual channel identifier from a known frame term.',
		argnames is ['Frame', 'VirtualChannelId']
	]).

	:- public(data_field/2).
	:- mode(data_field(+compound, -list(byte)), one).
	:- info(data_field/2, [
		comment is 'Extracts the raw data field bytes from a known frame term.',
		argnames is ['Frame', 'DataField']
	]).

	:- public(ocf/2).
	:- mode(ocf(+compound, -compound), one).
	:- info(ocf/2, [
		comment is 'Extracts the operational control field. Returns ``none`` when absent.',
		argnames is ['Frame', 'OCF']
	]).

	:- public(fecf/2).
	:- mode(fecf(+compound, -compound), one).
	:- info(fecf/2, [
		comment is 'Extracts the frame error control field. Returns ``none`` when absent.',
		argnames is ['Frame', 'FECF']
	]).

	:- public(update_fecf/2).
	:- mode(update_fecf(+compound, -compound), one_or_error).
	:- info(update_fecf/2, [
		comment is 'Refreshes a present frame error control field from the remaining frame fields while leaving frames without an FECF unchanged.',
		argnames is ['Frame', 'UpdatedFrame']
	]).

	:- public(verify_fecf/1).
	:- mode(verify_fecf(@compound), zero_or_one).
	:- info(verify_fecf/1, [
		comment is 'True if a present frame error control field matches the remaining frame fields. Frames without an FECF succeed.',
		argnames is ['Frame']
	]).

	:- public(extract_packets/3).
	:- mode(extract_packets(+compound, +integer, -list(compound)), one_or_error).
	:- info(extract_packets/3, [
		comment is 'Parses the raw frame data field as CCSDS space packet bytes using the given packet secondary header length.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'Packets']
	]).

	:- public(insert_packets/4).
	:- mode(insert_packets(+list(compound), +integer, +compound, -compound), one_or_error).
	:- info(insert_packets/4, [
		comment is 'Generates CCSDS packet bytes and replaces the raw data field of a frame term, preserving the remaining header fields unchanged.',
		argnames is ['Packets', 'SecondaryHeaderLength', 'Frame', 'UpdatedFrame']
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(list, [
		append/3, length/2
	]).

	valid(Frame) :-
		known_frame(Frame),
		verify_fecf(Frame).

	known_frame(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, FECF)) :-
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
		valid_secondary_header_term(SecondaryHeader),
		valid(list(byte), DataField),
		valid_ocf_term(OCF),
		valid_fecf_term(FECF).
	known_frame(tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, FECF)) :-
		valid(between(integer, 0, 3), Version),
		valid(between(integer, 0, 1), BypassFlag),
		valid(between(integer, 0, 1), ControlCommandFlag),
		valid(between(integer, 0, 1023), SpacecraftId),
		valid(between(integer, 0, 63), VirtualChannelId),
		valid(between(integer, 0, 255), SequenceNumber),
		valid_segment_header_term(SegmentHeader),
		valid(list(byte), DataField),
		valid_fecf_term(FECF).
	known_frame(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, FECF)) :-
		valid(between(integer, 0, 3), Version),
		valid(between(integer, 0, 1023), SpacecraftId),
		valid(between(integer, 0, 63), VirtualChannelId),
		valid(between(integer, 0, 268435455), VirtualChannelFrameCount),
		valid_signaling_field_term(SignalingField),
		valid_insert_zone_term(InsertZone),
		valid(list(byte), DataField),
		valid_ocf_term(OCF),
		valid_fecf_term(FECF).

	frame_type(tm_transfer_frame(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _), tm).
	frame_type(tc_transfer_frame(_, _, _, _, _, _, _, _, _), tc).
	frame_type(aos_transfer_frame(_, _, _, _, _, _, _, _, _), aos).

	version(tm_transfer_frame(Version, _, _, _, _, _, _, _, _, _, _, _, _, _, _), Version).
	version(tc_transfer_frame(Version, _, _, _, _, _, _, _, _), Version).
	version(aos_transfer_frame(Version, _, _, _, _, _, _, _, _), Version).

	spacecraft_id(tm_transfer_frame(_, SpacecraftId, _, _, _, _, _, _, _, _, _, _, _, _, _), SpacecraftId).
	spacecraft_id(tc_transfer_frame(_, _, _, SpacecraftId, _, _, _, _, _), SpacecraftId).
	spacecraft_id(aos_transfer_frame(_, SpacecraftId, _, _, _, _, _, _, _), SpacecraftId).

	virtual_channel_id(tm_transfer_frame(_, _, VirtualChannelId, _, _, _, _, _, _, _, _, _, _, _, _), VirtualChannelId).
	virtual_channel_id(tc_transfer_frame(_, _, _, _, VirtualChannelId, _, _, _, _), VirtualChannelId).
	virtual_channel_id(aos_transfer_frame(_, _, VirtualChannelId, _, _, _, _, _, _), VirtualChannelId).

	data_field(tm_transfer_frame(_, _, _, _, _, _, _, _, _, _, _, _, DataField, _, _), DataField).
	data_field(tc_transfer_frame(_, _, _, _, _, _, _, DataField, _), DataField).
	data_field(aos_transfer_frame(_, _, _, _, _, _, DataField, _, _), DataField).

	ocf(tm_transfer_frame(_, _, _, _, _, _, _, _, _, _, _, _, _, OCF, _), OCF).
	ocf(tc_transfer_frame(_, _, _, _, _, _, _, _, _), none).
	ocf(aos_transfer_frame(_, _, _, _, _, _, _, OCF, _), OCF).

	fecf(tm_transfer_frame(_, _, _, _, _, _, _, _, _, _, _, _, _, _, FECF), FECF).
	fecf(tc_transfer_frame(_, _, _, _, _, _, _, _, FECF), FECF).
	fecf(aos_transfer_frame(_, _, _, _, _, _, _, _, FECF), FECF).

	extract_packets(Frame, SecondaryHeaderLength, Packets) :-
		(   var(Frame) ->
			instantiation_error
		;   var(SecondaryHeaderLength) ->
			instantiation_error
		;   valid(Frame) ->
			valid_secondary_header_length(SecondaryHeaderLength),
			data_field(Frame, DataField),
			ccsds_packets(SecondaryHeaderLength)::parse(bytes(DataField), Packets)
		;   domain_error(ccsds_frame_term, Frame)
		).

	insert_packets(Packets, SecondaryHeaderLength, Frame, UpdatedFrame) :-
		(   var(Packets) ->
			instantiation_error
		;   var(SecondaryHeaderLength) ->
			instantiation_error
		;   var(Frame) ->
			instantiation_error
		;   valid(Frame) ->
			valid_secondary_header_length(SecondaryHeaderLength),
			ccsds_packets(SecondaryHeaderLength)::generate(bytes(DataField), Packets),
			replace_data_field(Frame, DataField, IntermediateFrame),
			update_fecf(IntermediateFrame, UpdatedFrame)
		;   domain_error(ccsds_frame_term, Frame)
		).

	update_fecf(Frame, UpdatedFrame) :-
		(   var(Frame) ->
			instantiation_error
		;   known_frame(Frame) ->
			refresh_fecf(Frame, UpdatedFrame)
		;   domain_error(ccsds_frame_term, Frame)
		).

	verify_fecf(Frame) :-
		known_frame(Frame),
		valid_frame_fecf(Frame).

	valid_signaling_field_term(signaling_field(ReplayFlag, FrameCountUsageFlag, SpacecraftIdExtension, FrameCountCycle)) :-
		valid(between(integer, 0, 1), ReplayFlag),
		valid(between(integer, 0, 1), FrameCountUsageFlag),
		valid(between(integer, 0, 3), SpacecraftIdExtension),
		valid(between(integer, 0, 15), FrameCountCycle).

	valid_insert_zone_term(none).
	valid_insert_zone_term(insert_zone(Bytes)) :-
		valid(list(byte), Bytes).

	valid_segment_header_term(none).
	valid_segment_header_term(segment_header(Bytes)) :-
		valid(list(byte), Bytes).

	valid_secondary_header_term(none).
	valid_secondary_header_term(secondary_header(Bytes)) :-
		valid(list(byte), Bytes).

	valid_ocf_term(none).
	valid_ocf_term(ocf(Bytes)) :-
		valid(list(byte), Bytes).

	valid_fecf_term(none).
	valid_fecf_term(fecf(Bytes)) :-
		valid(list(byte), Bytes).

	valid_secondary_header_length(SecondaryHeaderLength) :-
		integer(SecondaryHeaderLength),
		SecondaryHeaderLength >= 0,
		!.
	valid_secondary_header_length(SecondaryHeaderLength) :-
		domain_error(ccsds_secondary_header_length, SecondaryHeaderLength).

	valid_frame_fecf(Frame) :-
		fecf(Frame, none),
		!.
	valid_frame_fecf(Frame) :-
		fecf(Frame, fecf(Bytes)),
		compute_fecf(Frame, fecf(Bytes)).

	refresh_fecf(Frame, UpdatedFrame) :-
		fecf(Frame, none),
		!,
		UpdatedFrame = Frame.
	refresh_fecf(Frame, UpdatedFrame) :-
		compute_fecf(Frame, FECF),
		replace_fecf(Frame, FECF, UpdatedFrame).

	compute_fecf(Frame, fecf(Bytes)) :-
		frame_bytes_without_fecf(Frame, FrameBytes),
		compute_fecf_bytes(FrameBytes, Bytes).

	frame_bytes_without_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, _), Bytes) :-
		secondary_header_bytes(SecondaryHeader, SecondaryHeaderBytes),
		ocf_bytes(OCF, OCFBytes),
		Byte0 is ((Version /\ 0x03) << 6) \/ ((SpacecraftId >> 4) /\ 0x3F),
		Byte1 is ((SpacecraftId /\ 0x0F) << 4) \/ ((VirtualChannelId /\ 0x07) << 1) \/ (OCFFlag /\ 0x01),
		Byte2 is MasterChannelFrameCount,
		Byte3 is VirtualChannelFrameCount,
		Byte4 is ((SecondaryHeaderFlag /\ 0x01) << 7) \/ ((SynchronizationFlag /\ 0x01) << 6) \/ ((PacketOrderFlag /\ 0x01) << 5) \/ ((SegmentLengthIdentifier /\ 0x03) << 3) \/ ((FirstHeaderPointer >> 8) /\ 0x07),
		Byte5 is FirstHeaderPointer /\ 0xFF,
		Bytes = [Byte0, Byte1, Byte2, Byte3, Byte4, Byte5| Tail0],
		append(SecondaryHeaderBytes, Tail1, Tail0),
		append(DataField, Tail2, Tail1),
		append(OCFBytes, [], Tail2).
	frame_bytes_without_fecf(tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, _), Bytes) :-
		segment_header_bytes(SegmentHeader, SegmentHeaderBytes),
		length(DataField, DataFieldLength),
		length(SegmentHeaderBytes, SegmentHeaderLength),
		FrameLength is 5 + SegmentHeaderLength + DataFieldLength + 2,
		FlagsAndSpacecraftId is ((Version /\ 0x03) << 14) \/ ((BypassFlag /\ 0x01) << 13) \/ ((ControlCommandFlag /\ 0x01) << 12) \/ (SpacecraftId /\ 0x03FF),
		VirtualChannelIdAndLength is ((VirtualChannelId /\ 0x3F) << 10) \/ ((FrameLength - 1) /\ 0x03FF),
		Byte0 is (FlagsAndSpacecraftId >> 8) /\ 0xFF,
		Byte1 is FlagsAndSpacecraftId /\ 0xFF,
		Byte2 is (VirtualChannelIdAndLength >> 8) /\ 0xFF,
		Byte3 is VirtualChannelIdAndLength /\ 0xFF,
		Byte4 is SequenceNumber /\ 0xFF,
		Bytes = [Byte0, Byte1, Byte2, Byte3, Byte4| Tail0],
		append(SegmentHeaderBytes, Tail1, Tail0),
		append(DataField, [], Tail1).
	frame_bytes_without_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, signaling_field(ReplayFlag, FrameCountUsageFlag, SpacecraftIdExtension, FrameCountCycle), InsertZone, DataField, OCF, _), Bytes) :-
		insert_zone_bytes(InsertZone, InsertZoneBytes),
		ocf_bytes(OCF, OCFBytes),
		BaseFrameCount is VirtualChannelFrameCount /\ 0xFFFFFF,
		Byte0 is ((Version /\ 0x03) << 6) \/ ((SpacecraftId /\ 0x0FF) >> 2),
		Byte1 is ((SpacecraftId /\ 0x03) << 6) \/ (VirtualChannelId /\ 0x3F),
		Byte2 is (BaseFrameCount >> 16) /\ 0xFF,
		Byte3 is (BaseFrameCount >> 8) /\ 0xFF,
		Byte4 is BaseFrameCount /\ 0xFF,
		Byte5 is ((ReplayFlag /\ 0x01) << 7) \/ ((FrameCountUsageFlag /\ 0x01) << 6) \/ ((SpacecraftIdExtension /\ 0x03) << 4) \/ (FrameCountCycle /\ 0x0F),
		Bytes = [Byte0, Byte1, Byte2, Byte3, Byte4, Byte5| Tail0],
		append(InsertZoneBytes, Tail1, Tail0),
		append(DataField, Tail2, Tail1),
		append(OCFBytes, [], Tail2).

	secondary_header_bytes(none, []).
	secondary_header_bytes(secondary_header(Bytes), Bytes).

	segment_header_bytes(none, []).
	segment_header_bytes(segment_header(Bytes), Bytes).

	insert_zone_bytes(none, []).
	insert_zone_bytes(insert_zone(Bytes), Bytes).

	ocf_bytes(none, []).
	ocf_bytes(ocf(Bytes), Bytes).

	replace_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, _), FECF, tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, FECF)).
	replace_fecf(tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, _), FECF, tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, FECF)).
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
		(   CRC0 /\ 0x8000 =:= 0 ->
			CRC1 is (CRC0 << 1) /\ 0xFFFF
		;   CRC1 is xor((CRC0 << 1) /\ 0xFFFF, 0x1021)
		),
		NextBit is Bit - 1,
		compute_fecf_crc_bits(NextBit, CRC1, CRC).

	replace_data_field(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, _, OCF, FECF), DataField, tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, FECF)).
	replace_data_field(tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, _, FECF), DataField, tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, FECF)).
	replace_data_field(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, _, OCF, FECF), DataField, aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, FECF)).

:- end_object.
