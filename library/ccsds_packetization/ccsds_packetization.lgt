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


:- object(ccsds_packetization).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-06-14,
		comment is 'Helpers for packetizing CCSDS space packets into TM and AOS service-data regions with cross-frame carryover and idle fill generation.'
	]).

	:- public(initial_state/1).
	:- mode(initial_state(-compound), one).
	:- info(initial_state/1, [
		comment is 'Returns the initial packetizer state.',
		argnames is ['State']
	]).

	:- public(pending_packets/2).
	:- mode(pending_packets(+compound, -list(compound)), one_or_error).
	:- info(pending_packets/2, [
		comment is 'Extracts the non-empty queued packets and pending trailing packet bytes buffered per frame type, spacecraft identifier, and virtual channel identifier.',
		argnames is ['State', 'PendingPackets'],
		exceptions is [
			'``State`` is a variable' - instantiation_error,
			'``State`` is neither a variable nor a valid packetizer state term' - domain_error(ccsds_packetizer_state, 'State')
		]
	]).

	:- public(packetize_tm_packets/6).
	:- mode(packetize_tm_packets(+compound, +integer, +compound, +list(compound), -compound, -compound), one_or_error).
	:- info(packetize_tm_packets/6, [
		comment is 'Packetizes packets into the packet service region of a TM transfer frame, preserving any still-pending trailing packet bytes and queuing packets that do not fully fit.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedFrame', 'UpdatedState'],
		exceptions is [
			'``Frame``, ``SecondaryHeaderLength``, ``State``, or ``Packets`` is a variable' - instantiation_error,
			'``Frame`` is neither a variable nor a valid TM transfer frame term' - domain_error(ccsds_tm_transfer_frame_term, 'Frame'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``State`` is neither a variable nor a valid packetizer state term' - domain_error(ccsds_packetizer_state, 'State'),
			'``Packets`` is neither a variable nor a valid list of CCSDS packet terms' - domain_error(ccsds_packet_terms, 'Packets')
		]
	]).

	:- public(packetize_tm_packets/7).
	:- mode(packetize_tm_packets(+compound, +integer, +compound, +list(compound), -compound, -compound, -list(compound)), one_or_error).
	:- info(packetize_tm_packets/7, [
		comment is 'Packetizes packets into the packet service region of a TM transfer frame and also returns explicit packetization events.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedFrame', 'UpdatedState', 'Events'],
		exceptions is [
			'``Frame``, ``SecondaryHeaderLength``, ``State``, or ``Packets`` is a variable' - instantiation_error,
			'``Frame`` is neither a variable nor a valid TM transfer frame term' - domain_error(ccsds_tm_transfer_frame_term, 'Frame'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``State`` is neither a variable nor a valid packetizer state term' - domain_error(ccsds_packetizer_state, 'State'),
			'``Packets`` is neither a variable nor a valid list of CCSDS packet terms' - domain_error(ccsds_packet_terms, 'Packets')
		]
	]).

	:- public(packetize_aos_packets/6).
	:- mode(packetize_aos_packets(+compound, +integer, +compound, +list(compound), -compound, -compound), one_or_error).
	:- info(packetize_aos_packets/6, [
		comment is 'Packetizes packets into the packet service region of an AOS transfer frame, preserving any still-pending trailing packet bytes and queuing packets that do not fully fit.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedFrame', 'UpdatedState'],
		exceptions is [
			'``Frame``, ``SecondaryHeaderLength``, ``State``, or ``Packets`` is a variable' - instantiation_error,
			'``Frame`` is neither a variable nor a valid AOS transfer frame term' - domain_error(ccsds_aos_transfer_frame_term, 'Frame'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``State`` is neither a variable nor a valid packetizer state term' - domain_error(ccsds_packetizer_state, 'State'),
			'``Packets`` is neither a variable nor a valid list of CCSDS packet terms' - domain_error(ccsds_packet_terms, 'Packets')
		]
	]).

	:- public(packetize_aos_packets/7).
	:- mode(packetize_aos_packets(+compound, +integer, +compound, +list(compound), -compound, -compound, -list(compound)), one_or_error).
	:- info(packetize_aos_packets/7, [
		comment is 'Packetizes packets into the packet service region of an AOS transfer frame and also returns explicit packetization events.',
		argnames is ['Frame', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedFrame', 'UpdatedState', 'Events'],
		exceptions is [
			'``Frame``, ``SecondaryHeaderLength``, ``State``, or ``Packets`` is a variable' - instantiation_error,
			'``Frame`` is neither a variable nor a valid AOS transfer frame term' - domain_error(ccsds_aos_transfer_frame_term, 'Frame'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``State`` is neither a variable nor a valid packetizer state term' - domain_error(ccsds_packetizer_state, 'State'),
			'``Packets`` is neither a variable nor a valid list of CCSDS packet terms' - domain_error(ccsds_packet_terms, 'Packets')
		]
	]).

	:- public(packetize_tm_frames/7).
	:- mode(packetize_tm_frames(+list(compound), +integer, +compound, +list(compound), -list(compound), -list(compound), -compound), one_or_error).
	:- info(packetize_tm_frames/7, [
		comment is 'Packetizes packets across a sequence of TM transfer frames. Packets that remain queued for the first frame channel are also returned.',
		argnames is ['Frames', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedFrames', 'RemainingPackets', 'UpdatedState'],
		exceptions is [
			'``Frames``, ``SecondaryHeaderLength``, ``State``, or ``Packets`` is a variable' - instantiation_error,
			'``Frames`` is neither a variable nor a valid list of TM transfer frame terms' - domain_error(ccsds_tm_transfer_frame_terms, 'Frames'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``State`` is neither a variable nor a valid packetizer state term' - domain_error(ccsds_packetizer_state, 'State'),
			'``Packets`` is neither a variable nor a valid list of CCSDS packet terms' - domain_error(ccsds_packet_terms, 'Packets')
		]
	]).

	:- public(packetize_tm_frames/8).
	:- mode(packetize_tm_frames(+list(compound), +integer, +compound, +list(compound), -list(compound), -list(compound), -compound, -list(compound)), one_or_error).
	:- info(packetize_tm_frames/8, [
		comment is 'Packetizes packets across a sequence of TM transfer frames and also returns explicit packetization events in frame order.',
		argnames is ['Frames', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedFrames', 'RemainingPackets', 'UpdatedState', 'Events'],
		exceptions is [
			'``Frames``, ``SecondaryHeaderLength``, ``State``, or ``Packets`` is a variable' - instantiation_error,
			'``Frames`` is neither a variable nor a valid list of TM transfer frame terms' - domain_error(ccsds_tm_transfer_frame_terms, 'Frames'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``State`` is neither a variable nor a valid packetizer state term' - domain_error(ccsds_packetizer_state, 'State'),
			'``Packets`` is neither a variable nor a valid list of CCSDS packet terms' - domain_error(ccsds_packet_terms, 'Packets')
		]
	]).

	:- public(packetize_aos_frames/7).
	:- mode(packetize_aos_frames(+list(compound), +integer, +compound, +list(compound), -list(compound), -list(compound), -compound), one_or_error).
	:- info(packetize_aos_frames/7, [
		comment is 'Packetizes packets across a sequence of AOS transfer frames. Packets that remain queued for the first frame channel are also returned.',
		argnames is ['Frames', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedFrames', 'RemainingPackets', 'UpdatedState'],
		exceptions is [
			'``Frames``, ``SecondaryHeaderLength``, ``State``, or ``Packets`` is a variable' - instantiation_error,
			'``Frames`` is neither a variable nor a valid list of AOS transfer frame terms' - domain_error(ccsds_aos_transfer_frame_terms, 'Frames'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``State`` is neither a variable nor a valid packetizer state term' - domain_error(ccsds_packetizer_state, 'State'),
			'``Packets`` is neither a variable nor a valid list of CCSDS packet terms' - domain_error(ccsds_packet_terms, 'Packets')
		]
	]).

	:- public(packetize_aos_frames/8).
	:- mode(packetize_aos_frames(+list(compound), +integer, +compound, +list(compound), -list(compound), -list(compound), -compound, -list(compound)), one_or_error).
	:- info(packetize_aos_frames/8, [
		comment is 'Packetizes packets across a sequence of AOS transfer frames and also returns explicit packetization events in frame order.',
		argnames is ['Frames', 'SecondaryHeaderLength', 'State', 'Packets', 'UpdatedFrames', 'RemainingPackets', 'UpdatedState', 'Events'],
		exceptions is [
			'``Frames``, ``SecondaryHeaderLength``, ``State``, or ``Packets`` is a variable' - instantiation_error,
			'``Frames`` is neither a variable nor a valid list of AOS transfer frame terms' - domain_error(ccsds_aos_transfer_frame_terms, 'Frames'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``State`` is neither a variable nor a valid packetizer state term' - domain_error(ccsds_packetizer_state, 'State'),
			'``Packets`` is neither a variable nor a valid list of CCSDS packet terms' - domain_error(ccsds_packet_terms, 'Packets')
		]
	]).

	:- public(generate_idle_packet/4).
	:- mode(generate_idle_packet(+integer, +integer, +integer, -compound), one_or_error).
	:- info(generate_idle_packet/4, [
		comment is 'Generates a telemetry idle packet using APID 2047, the given packet secondary header length, sequence count, and user-data length.',
		argnames is ['SecondaryHeaderLength', 'SequenceCount', 'UserDataLength', 'Packet'],
		exceptions is [
			'``SecondaryHeaderLength``, ``SequenceCount``, or ``UserDataLength`` is a variable' - instantiation_error,
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``SequenceCount`` is neither a variable nor a valid CCSDS packet sequence count' - domain_error(ccsds_packet_sequence_count, 'SequenceCount'),
			'``UserDataLength`` is neither a variable nor a valid idle-packet user-data length' - domain_error(ccsds_idle_user_data_length, 'UserDataLength')
		]
	]).

	:- uses(list, [
		append/3, length/2, reverse/2, take/4
	]).

	:- uses(type, [
		valid/2
	]).

	initial_state(packetizer_state([])).

	pending_packets(State, PendingPackets) :-
		(	var(State) ->
			instantiation_error
		;	valid_packetizer_state(State) ->
			State = packetizer_state(Channels),
			pending_packets_(Channels, PendingPackets)
		;	domain_error(ccsds_packetizer_state, State)
		).

	packetize_tm_packets(Frame, SecondaryHeaderLength, State, Packets, UpdatedFrame, UpdatedState) :-
		packetize_tm_packets(Frame, SecondaryHeaderLength, State, Packets, UpdatedFrame, UpdatedState, _).

	packetize_tm_packets(Frame, SecondaryHeaderLength, State, Packets, UpdatedFrame, UpdatedState, Events) :-
		(	var(Frame) ->
			instantiation_error
		;	var(SecondaryHeaderLength) ->
			instantiation_error
		;	var(State) ->
			instantiation_error
		;	var(Packets) ->
			instantiation_error
		;	valid_secondary_header_length(SecondaryHeaderLength),
			valid_packetizer_state(State),
			valid_packet_terms(Packets) ->
			tm_frame_context(Frame, SpacecraftId, VirtualChannelId, Capacity),
			packetize_channel(tm, SpacecraftId, VirtualChannelId, Capacity, SecondaryHeaderLength, State, Packets, DataField, FirstHeaderPointer, UpdatedState, Events),
			tm_first_header_pointer(FirstHeaderPointer, NormalizedFirstHeaderPointer),
			replace_tm_packet_zone(Frame, NormalizedFirstHeaderPointer, DataField, UpdatedFrame)
		;	\+ valid_secondary_header_length(SecondaryHeaderLength) ->
			domain_error(ccsds_secondary_header_length, SecondaryHeaderLength)
		;	\+ valid_packetizer_state(State) ->
			domain_error(ccsds_packetizer_state, State)
		;	\+ valid_packet_terms(Packets) ->
			domain_error(ccsds_packet_terms, Packets)
		;	domain_error(ccsds_tm_transfer_frame_term, Frame)
		).

	packetize_aos_packets(Frame, SecondaryHeaderLength, State, Packets, UpdatedFrame, UpdatedState) :-
		packetize_aos_packets(Frame, SecondaryHeaderLength, State, Packets, UpdatedFrame, UpdatedState, _).

	packetize_aos_packets(Frame, SecondaryHeaderLength, State, Packets, UpdatedFrame, UpdatedState, Events) :-
		(	var(Frame) ->
			instantiation_error
		;	var(SecondaryHeaderLength) ->
			instantiation_error
		;	var(State) ->
			instantiation_error
		;	var(Packets) ->
			instantiation_error
		;	valid_secondary_header_length(SecondaryHeaderLength),
			valid_packetizer_state(State),
			valid_packet_terms(Packets) ->
			aos_frame_context(Frame, SpacecraftId, VirtualChannelId, Capacity),
			packetize_channel(aos, SpacecraftId, VirtualChannelId, Capacity, SecondaryHeaderLength, State, Packets, PacketZoneBytes, FirstHeaderPointer, UpdatedState, Events),
			encode_aos_packet_zone(FirstHeaderPointer, PacketZoneBytes, DataField),
			replace_aos_packet_zone(Frame, DataField, UpdatedFrame)
		;	\+ valid_secondary_header_length(SecondaryHeaderLength) ->
			domain_error(ccsds_secondary_header_length, SecondaryHeaderLength)
		;	\+ valid_packetizer_state(State) ->
			domain_error(ccsds_packetizer_state, State)
		;	\+ valid_packet_terms(Packets) ->
			domain_error(ccsds_packet_terms, Packets)
		;	domain_error(ccsds_aos_transfer_frame_term, Frame)
		).

	packetize_tm_frames(Frames, SecondaryHeaderLength, State, Packets, UpdatedFrames, RemainingPackets, UpdatedState) :-
		packetize_tm_frames(Frames, SecondaryHeaderLength, State, Packets, UpdatedFrames, RemainingPackets, UpdatedState, _).

	packetize_tm_frames(Frames, SecondaryHeaderLength, State, Packets, UpdatedFrames, RemainingPackets, UpdatedState, Events) :-
		(	var(Frames) ->
			instantiation_error
		;	var(SecondaryHeaderLength) ->
			instantiation_error
		;	var(State) ->
			instantiation_error
		;	var(Packets) ->
			instantiation_error
		;	valid_secondary_header_length(SecondaryHeaderLength),
			valid_packetizer_state(State),
			valid_packet_terms(Packets),
			valid_tm_frames(Frames) ->
			(	Frames = [FirstFrame| _] ->
				tm_frame_context(FirstFrame, SpacecraftId, VirtualChannelId, _),
				packetize_tm_frames_(Frames, SecondaryHeaderLength, State, Packets, UpdatedFrames, UpdatedState, Events),
				queued_packets_for_channel(UpdatedState, tm, SpacecraftId, VirtualChannelId, RemainingPackets)
			;	UpdatedFrames = [],
				RemainingPackets = Packets,
				UpdatedState = State,
				Events = []
			)
		;	\+ valid_secondary_header_length(SecondaryHeaderLength) ->
			domain_error(ccsds_secondary_header_length, SecondaryHeaderLength)
		;	\+ valid_packetizer_state(State) ->
			domain_error(ccsds_packetizer_state, State)
		;	\+ valid_packet_terms(Packets) ->
			domain_error(ccsds_packet_terms, Packets)
		;	domain_error(ccsds_tm_transfer_frame_terms, Frames)
		).

	packetize_aos_frames(Frames, SecondaryHeaderLength, State, Packets, UpdatedFrames, RemainingPackets, UpdatedState) :-
		packetize_aos_frames(Frames, SecondaryHeaderLength, State, Packets, UpdatedFrames, RemainingPackets, UpdatedState, _).

	packetize_aos_frames(Frames, SecondaryHeaderLength, State, Packets, UpdatedFrames, RemainingPackets, UpdatedState, Events) :-
		(	var(Frames) ->
			instantiation_error
		;	var(SecondaryHeaderLength) ->
			instantiation_error
		;	var(State) ->
			instantiation_error
		;	var(Packets) ->
			instantiation_error
		;	valid_secondary_header_length(SecondaryHeaderLength),
			valid_packetizer_state(State),
			valid_packet_terms(Packets),
			valid_aos_frames(Frames) ->
			(	Frames = [FirstFrame| _] ->
				aos_frame_context(FirstFrame, SpacecraftId, VirtualChannelId, _),
				packetize_aos_frames_(Frames, SecondaryHeaderLength, State, Packets, UpdatedFrames, UpdatedState, Events),
				queued_packets_for_channel(UpdatedState, aos, SpacecraftId, VirtualChannelId, RemainingPackets)
			;	UpdatedFrames = [],
				RemainingPackets = Packets,
				UpdatedState = State,
				Events = []
			)
		;	\+ valid_secondary_header_length(SecondaryHeaderLength) ->
			domain_error(ccsds_secondary_header_length, SecondaryHeaderLength)
		;	\+ valid_packetizer_state(State) ->
			domain_error(ccsds_packetizer_state, State)
		;	\+ valid_packet_terms(Packets) ->
			domain_error(ccsds_packet_terms, Packets)
		;	domain_error(ccsds_aos_transfer_frame_terms, Frames)
		).

	generate_idle_packet(SecondaryHeaderLength, SequenceCount, UserDataLength, Packet) :-
		(	var(SecondaryHeaderLength) ->
			instantiation_error
		;	var(SequenceCount) ->
			instantiation_error
		;	var(UserDataLength) ->
			instantiation_error
		;	valid_secondary_header_length(SecondaryHeaderLength),
			valid_sequence_count(SequenceCount),
			integer(UserDataLength), UserDataLength >= 1 ->
			idle_secondary_header(SecondaryHeaderLength, SecondaryHeader),
			zero_bytes(UserDataLength, UserData),
			Packet = ccsds_packet(0, 0, 0, 2047, 3, SequenceCount, SecondaryHeader, UserData)
		;	\+ valid_secondary_header_length(SecondaryHeaderLength) ->
			domain_error(ccsds_secondary_header_length, SecondaryHeaderLength)
		;	\+ valid_sequence_count(SequenceCount) ->
			domain_error(ccsds_packet_sequence_count, SequenceCount)
		;	domain_error(ccsds_idle_user_data_length, UserDataLength)
		).

	packetize_tm_frames_([Frame| Frames], SecondaryHeaderLength, State, Packets, [UpdatedFrame| UpdatedFrames], UpdatedState, Events) :-
		packetize_tm_packets(Frame, SecondaryHeaderLength, State, Packets, UpdatedFrame, IntermediateState, FrameEvents),
		append(FrameEvents, RemainingEvents, Events),
		packetize_tm_frames_(Frames, SecondaryHeaderLength, IntermediateState, [], UpdatedFrames, UpdatedState, RemainingEvents).
	packetize_tm_frames_([], _, State, _, [], State, []).

	packetize_aos_frames_([Frame| Frames], SecondaryHeaderLength, State, Packets, [UpdatedFrame| UpdatedFrames], UpdatedState, Events) :-
		packetize_aos_packets(Frame, SecondaryHeaderLength, State, Packets, UpdatedFrame, IntermediateState, FrameEvents),
		append(FrameEvents, RemainingEvents, Events),
		packetize_aos_frames_(Frames, SecondaryHeaderLength, IntermediateState, [], UpdatedFrames, UpdatedState, RemainingEvents).
	packetize_aos_frames_([], _, State, _, [], State, []).

	tm_first_header_pointer(none, 2047) :-
		!.
	tm_first_header_pointer(FirstHeaderPointer, FirstHeaderPointer).

	packetize_channel(FrameType, SpacecraftId, VirtualChannelId, Capacity, SecondaryHeaderLength, State, Packets, ZoneBytes, FirstHeaderPointer, UpdatedState, Events) :-
		channel_state(State, FrameType, SpacecraftId, VirtualChannelId, PendingBytes0, QueuedPackets0),
		append(QueuedPackets0, Packets, QueuedPackets),
		fill_packet_zone(Capacity, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, PendingBytes0, QueuedPackets, ZoneBytes, FirstHeaderPointer, PendingBytes, RemainingPackets, Events),
		update_channel_state(State, FrameType, SpacecraftId, VirtualChannelId, PendingBytes, RemainingPackets, UpdatedState).

	fill_packet_zone(Capacity, _, _, _, _, PendingBytes, QueuedPackets, [], none, PendingBytes, QueuedPackets, []) :-
		Capacity =< 0,
		!.
	fill_packet_zone(Capacity, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, PendingBytes, QueuedPackets, ZoneBytes, FirstHeaderPointer, UpdatedPendingBytes, UpdatedQueuedPackets, Events) :-
		consume_pending_bytes(Capacity, PendingBytes, PrefixBytes, RemainingCapacity, PendingRemainder),
		(	PendingRemainder \== [] ->
			ZoneBytes = PrefixBytes,
			FirstHeaderPointer = none,
			UpdatedPendingBytes = PendingRemainder,
			UpdatedQueuedPackets = QueuedPackets,
			Events = []
		;	length(PrefixBytes, Offset),
			emit_started_packets(RemainingCapacity, Offset, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, QueuedPackets, StartedBytes, FirstHeaderPointer, UpdatedPendingBytes, UpdatedQueuedPackets, Events),
			append(PrefixBytes, StartedBytes, ZoneBytes)
		).

	emit_started_packets(0, _, _, _, _, _, QueuedPackets, [], none, [], QueuedPackets, []) :-
		!.
	emit_started_packets(Capacity, Offset, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, [Packet| Packets], Bytes, Offset, UpdatedPendingBytes, UpdatedQueuedPackets, Events) :-
		packet_bytes(Packet, SecondaryHeaderLength, PacketBytes, _),
		length(PacketBytes, PacketBytesLength),
		PacketBytesLength > Capacity,
		!,
		take(Capacity, PacketBytes, Bytes, UpdatedPendingBytes),
		UpdatedQueuedPackets = Packets,
		Events = [buffered_packet_fragment(FrameType, SpacecraftId, VirtualChannelId, UpdatedPendingBytes)].
	emit_started_packets(Capacity, Offset, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, [Packet| Packets], Bytes, Offset, UpdatedPendingBytes, UpdatedQueuedPackets, Events) :-
		packet_bytes(Packet, SecondaryHeaderLength, PacketBytes, SequenceCount),
		length(PacketBytes, PacketBytesLength),
		PacketBytesLength =< Capacity,
		!,
		RemainingCapacity is Capacity - PacketBytesLength,
		NextOffset is Offset + PacketBytesLength,
		append(PacketBytes, RemainingBytes, Bytes),
		idle_sequence_count(SequenceCount, IdleSequenceCount),
		emit_following_packets(RemainingCapacity, NextOffset, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, Packets, IdleSequenceCount, RemainingBytes, UpdatedPendingBytes, UpdatedQueuedPackets, Events).
	emit_started_packets(Capacity, Offset, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, [], Bytes, Offset, UpdatedPendingBytes, [], Events) :-
		idle_sequence_count(none, IdleSequenceCount),
		emit_idle_fill(Capacity, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, IdleSequenceCount, Bytes, UpdatedPendingBytes, Events).

	emit_following_packets(0, _, _, _, _, _, Packets, _, [], [], Packets, []) :-
		!.
	emit_following_packets(Capacity, _, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, [Packet| Packets], _, Bytes, UpdatedPendingBytes, UpdatedQueuedPackets, Events) :-
		packet_bytes(Packet, SecondaryHeaderLength, PacketBytes, _),
		length(PacketBytes, PacketBytesLength),
		PacketBytesLength > Capacity,
		!,
		take(Capacity, PacketBytes, Bytes, UpdatedPendingBytes),
		UpdatedQueuedPackets = Packets,
		Events = [buffered_packet_fragment(FrameType, SpacecraftId, VirtualChannelId, UpdatedPendingBytes)].
	emit_following_packets(Capacity, Offset, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, [Packet| Packets], _, Bytes, UpdatedPendingBytes, UpdatedQueuedPackets, Events) :-
		packet_bytes(Packet, SecondaryHeaderLength, PacketBytes, SequenceCount),
		length(PacketBytes, PacketBytesLength),
		PacketBytesLength =< Capacity,
		!,
		RemainingCapacity is Capacity - PacketBytesLength,
		NextOffset is Offset + PacketBytesLength,
		append(PacketBytes, RemainingBytes, Bytes),
		idle_sequence_count(SequenceCount, IdleSequenceCount),
		emit_following_packets(RemainingCapacity, NextOffset, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, Packets, IdleSequenceCount, RemainingBytes, UpdatedPendingBytes, UpdatedQueuedPackets, Events).
	emit_following_packets(Capacity, _, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, [], IdleSequenceCount, Bytes, UpdatedPendingBytes, [], Events) :-
		emit_idle_fill(Capacity, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, IdleSequenceCount, Bytes, UpdatedPendingBytes, Events).

	emit_idle_fill(0, _, _, _, _, _, [], [], []) :-
		!.
	emit_idle_fill(Capacity, SecondaryHeaderLength, FrameType, SpacecraftId, VirtualChannelId, IdleSequenceCount, Bytes, UpdatedPendingBytes, Events) :-
		minimum_packet_bytes_length(SecondaryHeaderLength, MinimumLength),
		(	Capacity >= MinimumLength ->
			UserDataLength is Capacity - 6 - SecondaryHeaderLength,
			generate_idle_packet(SecondaryHeaderLength, IdleSequenceCount, UserDataLength, IdlePacket),
			packet_bytes(IdlePacket, SecondaryHeaderLength, Bytes, _),
			UpdatedPendingBytes = [],
			Events = [generated_idle_packet(FrameType, SpacecraftId, VirtualChannelId, IdlePacket)]
		;	generate_idle_packet(SecondaryHeaderLength, IdleSequenceCount, 1, IdlePacket),
			packet_bytes(IdlePacket, SecondaryHeaderLength, IdlePacketBytes, _),
			take(Capacity, IdlePacketBytes, Bytes, UpdatedPendingBytes),
			Events = [generated_idle_packet(FrameType, SpacecraftId, VirtualChannelId, IdlePacket), buffered_packet_fragment(FrameType, SpacecraftId, VirtualChannelId, UpdatedPendingBytes)]
		).

	consume_pending_bytes(Capacity, PendingBytes, PrefixBytes, RemainingCapacity, PendingRemainder) :-
		(	PendingBytes == [] ->
			PrefixBytes = [],
			RemainingCapacity = Capacity,
			PendingRemainder = []
		;	take(Capacity, PendingBytes, PrefixBytes, PendingRemainder) ->
			length(PrefixBytes, PrefixLength),
			RemainingCapacity is Capacity - PrefixLength
		;	PrefixBytes = PendingBytes,
			length(PrefixBytes, PrefixLength),
			RemainingCapacity is Capacity - PrefixLength,
			PendingRemainder = []
		).

	channel_state(packetizer_state(Channels), FrameType, SpacecraftId, VirtualChannelId, PendingBytes, QueuedPackets) :-
		(	select_channel_entry(FrameType, SpacecraftId, VirtualChannelId, Channels, packetizer_channel(FrameType, SpacecraftId, VirtualChannelId, PendingBytes, QueuedPackets), _) ->
			true
		;	PendingBytes = [],
			QueuedPackets = []
		).

	queued_packets_for_channel(packetizer_state(Channels), FrameType, SpacecraftId, VirtualChannelId, QueuedPackets) :-
		(	select_channel_entry(FrameType, SpacecraftId, VirtualChannelId, Channels, packetizer_channel(FrameType, SpacecraftId, VirtualChannelId, _, QueuedPackets), _) ->
			true
		;	QueuedPackets = []
		).

	update_channel_state(packetizer_state(Channels), FrameType, SpacecraftId, VirtualChannelId, PendingBytes, QueuedPackets, packetizer_state(UpdatedChannels)) :-
		(	select_channel_entry(FrameType, SpacecraftId, VirtualChannelId, Channels, _, OtherChannels) ->
			true
		;	OtherChannels = Channels
		),
		(	PendingBytes == [], QueuedPackets == [] ->
			UpdatedChannels = OtherChannels
		;	UpdatedChannels = [packetizer_channel(FrameType, SpacecraftId, VirtualChannelId, PendingBytes, QueuedPackets)| OtherChannels]
		).

	select_channel_entry(FrameType, SpacecraftId, VirtualChannelId, [Channel| Channels], Channel, Channels) :-
		Channel = packetizer_channel(FrameType, SpacecraftId, VirtualChannelId, _, _),
		!.
	select_channel_entry(FrameType, SpacecraftId, VirtualChannelId, [Channel| Channels], SelectedChannel, [Channel| RemainingChannels]) :-
		select_channel_entry(FrameType, SpacecraftId, VirtualChannelId, Channels, SelectedChannel, RemainingChannels).

	pending_packets_([Channel| Channels], [Channel| PendingPackets]) :-
		Channel = packetizer_channel(_, _, _, PendingBytes, QueuedPackets),
		( PendingBytes \== [] ; QueuedPackets \== [] ),
		!,
		pending_packets_(Channels, PendingPackets).
	pending_packets_([_| Channels], PendingPackets) :-
		pending_packets_(Channels, PendingPackets).
	pending_packets_([], []).

	tm_frame_context(tm_transfer_frame(_, SpacecraftId, VirtualChannelId, _, _, _, _, _, _, _, _, _, DataField, _, _), SpacecraftId, VirtualChannelId, Capacity) :-
		valid(list(byte), DataField),
		length(DataField, Capacity).

	aos_frame_context(aos_transfer_frame(_, SpacecraftId, VirtualChannelId, _, _, _, DataField, _, _), SpacecraftId, VirtualChannelId, Capacity) :-
		valid(list(byte), DataField),
		length(DataField, DataFieldLength),
		DataFieldLength >= 2,
		Capacity is DataFieldLength - 2.

	replace_tm_packet_zone(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, _, SecondaryHeader, _, OCF, FECF), FirstHeaderPointer, DataField, UpdatedFrame) :-
		refresh_tm_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, FECF), UpdatedFrame).

	replace_aos_packet_zone(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, _, OCF, FECF), DataField, UpdatedFrame) :-
		refresh_aos_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, FECF), UpdatedFrame).

	refresh_tm_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, none), tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, none)) :-
		!.
	refresh_tm_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, _), tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, FECF)) :-
		compute_tm_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, none), FECF).

	refresh_aos_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, none), aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, none)) :-
		!.
	refresh_aos_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, _), aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, FECF)) :-
		compute_aos_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, SignalingField, InsertZone, DataField, OCF, none), FECF).

	compute_tm_fecf(tm_transfer_frame(Version, SpacecraftId, VirtualChannelId, OCFFlag, MasterChannelFrameCount, VirtualChannelFrameCount, SecondaryHeaderFlag, SynchronizationFlag, PacketOrderFlag, SegmentLengthIdentifier, FirstHeaderPointer, SecondaryHeader, DataField, OCF, _), fecf(Bytes)) :-
		secondary_header_bytes(SecondaryHeader, SecondaryHeaderBytes),
		ocf_bytes(OCF, OCFBytes),
		Byte0 is ((Version /\ 0x03) << 6) \/ ((SpacecraftId >> 4) /\ 0x3F),
		Byte1 is ((SpacecraftId /\ 0x0F) << 4) \/ ((VirtualChannelId /\ 0x07) << 1) \/ (OCFFlag /\ 0x01),
		Byte2 is MasterChannelFrameCount,
		Byte3 is VirtualChannelFrameCount,
		Byte4 is ((SecondaryHeaderFlag /\ 0x01) << 7) \/ ((SynchronizationFlag /\ 0x01) << 6) \/ ((PacketOrderFlag /\ 0x01) << 5) \/ ((SegmentLengthIdentifier /\ 0x03) << 3) \/ ((FirstHeaderPointer >> 8) /\ 0x07),
		Byte5 is FirstHeaderPointer /\ 0xFF,
		FrameBytes = [Byte0, Byte1, Byte2, Byte3, Byte4, Byte5| Tail0],
		append(SecondaryHeaderBytes, Tail1, Tail0),
		append(DataField, Tail2, Tail1),
		append(OCFBytes, [], Tail2),
		compute_fecf_bytes(FrameBytes, Bytes).

	compute_aos_fecf(aos_transfer_frame(Version, SpacecraftId, VirtualChannelId, VirtualChannelFrameCount, signaling_field(ReplayFlag, FrameCountUsageFlag, SpacecraftIdExtension, FrameCountCycle), InsertZone, DataField, OCF, _), fecf(Bytes)) :-
		insert_zone_bytes(InsertZone, InsertZoneBytes),
		ocf_bytes(OCF, OCFBytes),
		BaseFrameCount is VirtualChannelFrameCount /\ 0xFFFFFF,
		Byte0 is ((Version /\ 0x03) << 6) \/ ((SpacecraftId /\ 0x0FF) >> 2),
		Byte1 is ((SpacecraftId /\ 0x03) << 6) \/ (VirtualChannelId /\ 0x3F),
		Byte2 is (BaseFrameCount >> 16) /\ 0xFF,
		Byte3 is (BaseFrameCount >> 8) /\ 0xFF,
		Byte4 is BaseFrameCount /\ 0xFF,
		Byte5 is ((ReplayFlag /\ 0x01) << 7) \/ ((FrameCountUsageFlag /\ 0x01) << 6) \/ ((SpacecraftIdExtension /\ 0x03) << 4) \/ (FrameCountCycle /\ 0x0F),
		FrameBytes = [Byte0, Byte1, Byte2, Byte3, Byte4, Byte5| Tail0],
		append(InsertZoneBytes, Tail1, Tail0),
		append(DataField, Tail2, Tail1),
		append(OCFBytes, [], Tail2),
		compute_fecf_bytes(FrameBytes, Bytes).

	secondary_header_bytes(none, []).
	secondary_header_bytes(secondary_header(Bytes), Bytes).

	insert_zone_bytes(none, []).
	insert_zone_bytes(insert_zone(Bytes), Bytes).

	ocf_bytes(none, []).
	ocf_bytes(ocf(Bytes), Bytes).

	compute_fecf_bytes(FrameBytes, [HighByte, LowByte]) :-
		compute_fecf_crc(FrameBytes, 0xFFFF, CRC),
		HighByte is (CRC >> 8) /\ 0xFF,
		LowByte is CRC /\ 0xFF.

	compute_fecf_crc([Byte| Bytes], CRC0, CRC) :-
		CRC1 is xor(CRC0, Byte << 8) /\ 0xFFFF,
		compute_fecf_crc_bits(8, CRC1, CRC2),
		compute_fecf_crc(Bytes, CRC2, CRC).
	compute_fecf_crc([], CRC, CRC).

	compute_fecf_crc_bits(0, CRC, CRC).
	compute_fecf_crc_bits(Bit, CRC0, CRC) :-
		(	CRC0 /\ 0x8000 =:= 0 ->
			CRC1 is (CRC0 << 1) /\ 0xFFFF
		;	CRC1 is xor((CRC0 << 1) /\ 0xFFFF, 0x1021)
		),
		NextBit is Bit - 1,
		compute_fecf_crc_bits(NextBit, CRC1, CRC).

	encode_aos_packet_zone(none, PacketZoneBytes, Bytes) :-
		!,
		encode_aos_first_header_pointer(2047, Prefix),
		append(Prefix, PacketZoneBytes, Bytes).
	encode_aos_packet_zone(FirstHeaderPointer, PacketZoneBytes, Bytes) :-
		encode_aos_first_header_pointer(FirstHeaderPointer, Prefix),
		append(Prefix, PacketZoneBytes, Bytes).

	encode_aos_first_header_pointer(FirstHeaderPointer, [Byte0, Byte1]) :-
		integer(FirstHeaderPointer),
		FirstHeaderPointer >= 0,
		FirstHeaderPointer =< 2047,
		Byte0 is (FirstHeaderPointer >> 8) /\ 0x07,
		Byte1 is FirstHeaderPointer /\ 0xFF.

	packet_bytes(Packet, SecondaryHeaderLength, Bytes, SequenceCount) :-
		Packet = ccsds_packet(_, _, _, _, _, SequenceCount, _, _),
		ccsds_packets(SecondaryHeaderLength)::generate(bytes(Bytes), [Packet]).

	idle_secondary_header(0, none) :-
		!.
	idle_secondary_header(SecondaryHeaderLength, secondary_header(Bytes)) :-
		zero_bytes(SecondaryHeaderLength, Bytes).

	zero_bytes(Length, Bytes) :-
		zero_bytes_(Length, [], ReversedBytes),
		reverse(ReversedBytes, Bytes).

	zero_bytes_(Length, Accumulator, Accumulator) :-
		Length =< 0,
		!.
	zero_bytes_(Length, Accumulator, Bytes) :-
		NextLength is Length - 1,
		zero_bytes_(NextLength, [0| Accumulator], Bytes).

	minimum_packet_bytes_length(SecondaryHeaderLength, MinimumLength) :-
		MinimumLength is 7 + SecondaryHeaderLength.

	idle_sequence_count(none, 0) :-
		!.
	idle_sequence_count(SequenceCount, NextSequenceCount) :-
		NextSequenceCount is (SequenceCount + 1) mod 16384.

	valid_packetizer_state(packetizer_state(Channels)) :-
		valid_packetizer_channels(Channels).

	valid_packetizer_channels([Channel| Channels]) :-
		valid_packetizer_channel(Channel),
		no_duplicate_packetizer_channel(Channel, Channels),
		valid_packetizer_channels(Channels).
	valid_packetizer_channels([]).

	valid_packetizer_channel(packetizer_channel(FrameType, SpacecraftId, VirtualChannelId, PendingBytes, QueuedPackets)) :-
		once(( FrameType == tm ; FrameType == aos )),
		valid_frame_type_limits(FrameType, SpacecraftId, VirtualChannelId),
		valid(list(byte), PendingBytes),
		valid_packet_terms(QueuedPackets).

	valid_frame_type_limits(tm, SpacecraftId, VirtualChannelId) :-
		valid(between(integer, 0, 1023), SpacecraftId),
		valid(between(integer, 0, 7), VirtualChannelId).
	valid_frame_type_limits(aos, SpacecraftId, VirtualChannelId) :-
		valid(between(integer, 0, 1023), SpacecraftId),
		valid(between(integer, 0, 63), VirtualChannelId).

	no_duplicate_packetizer_channel(packetizer_channel(FrameType, SpacecraftId, VirtualChannelId, _, _), [packetizer_channel(FrameType, SpacecraftId, VirtualChannelId, _, _)| _]) :-
		!,
		fail.
	no_duplicate_packetizer_channel(Channel, [_| Channels]) :-
		no_duplicate_packetizer_channel(Channel, Channels).
	no_duplicate_packetizer_channel(_, []).

	valid_packet_terms([Packet| Packets]) :-
		valid_packet_term(Packet),
		valid_packet_terms(Packets).
	valid_packet_terms([]).

	valid_packet_term(ccsds_packet(Version, Type, SecondaryHeaderFlag, APID, SequenceFlags, SequenceCount, SecondaryHeader, UserData)) :-
		valid(between(integer, 0, 7), Version),
		valid(between(integer, 0, 1), Type),
		valid(between(integer, 0, 1), SecondaryHeaderFlag),
		valid(between(integer, 0, 2047), APID),
		valid(between(integer, 0, 3), SequenceFlags),
		valid_sequence_count(SequenceCount),
		valid_secondary_header_term(SecondaryHeader),
		valid(list(byte), UserData).

	valid_secondary_header_term(none).
	valid_secondary_header_term(secondary_header(Bytes)) :-
		valid(list(byte), Bytes).

	valid_sequence_count(SequenceCount) :-
		valid(between(integer, 0, 16383), SequenceCount).

	valid_secondary_header_length(SecondaryHeaderLength) :-
		integer(SecondaryHeaderLength),
		SecondaryHeaderLength >= 0.

	valid_tm_frames([Frame| Frames]) :-
		tm_frame_context(Frame, _, _, _),
		valid_tm_frames(Frames).
	valid_tm_frames([]).

	valid_aos_frames([Frame| Frames]) :-
		aos_frame_context(Frame, _, _, _),
		valid_aos_frames(Frames).
	valid_aos_frames([]).

:- end_object.
