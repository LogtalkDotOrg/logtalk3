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


:- object(ccsds_tc_services).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-05-08,
		comment is 'Helpers for CCSDS telecommand segment extraction, insertion, and cross-frame TC service reassembly.'
	]).

	:- public(valid_segment/1).
	:- mode(valid_segment(@compound), zero_or_one).
	:- info(valid_segment/1, [
		comment is 'True if the argument is a valid CCSDS telecommand segment term.',
		argnames is ['Segment']
	]).

	:- public(sequence_flags/2).
	:- mode(sequence_flags(+compound, -atom), one).
	:- info(sequence_flags/2, [
		comment is 'Extracts the telecommand segment sequence flags.',
		argnames is ['Segment', 'SequenceFlags']
	]).

	:- public(map_id/2).
	:- mode(map_id(+compound, -integer), one).
	:- info(map_id/2, [
		comment is 'Extracts the telecommand service-unit MAP identifier from either a telecommand segment term or a provenance-aware reassembled telecommand service-unit term.',
		argnames is ['ServiceUnit', 'MapId']
	]).

	:- public(valid_map_dispatch/1).
	:- mode(valid_map_dispatch(@compound), zero_or_one).
	:- info(valid_map_dispatch/1, [
		comment is 'True if the argument is a valid MAP-aware dispatch term grouping complete telecommand service units for a single MAP identifier.',
		argnames is ['Dispatch']
	]).

	:- public(dispatch_map_id/2).
	:- mode(dispatch_map_id(+compound, -integer), one).
	:- info(dispatch_map_id/2, [
		comment is 'Extracts the MAP identifier from a MAP-aware dispatch term.',
		argnames is ['Dispatch', 'MapId']
	]).

	:- public(dispatch_service_units/2).
	:- mode(dispatch_service_units(+compound, -list(compound)), one).
	:- info(dispatch_service_units/2, [
		comment is 'Extracts the complete telecommand service units from a MAP-aware dispatch term.',
		argnames is ['Dispatch', 'ServiceUnits']
	]).

	:- public(segment_header_suffix/2).
	:- mode(segment_header_suffix(+compound, -list(byte)), one).
	:- info(segment_header_suffix/2, [
		comment is 'Extracts the mission-specific segment-header suffix bytes. Returns the empty list when the segment has no mission-specific suffix.',
		argnames is ['Segment', 'HeaderSuffix']
	]).

	:- public(valid_reassembled_segment/1).
	:- mode(valid_reassembled_segment(@compound), zero_or_one).
	:- info(valid_reassembled_segment/1, [
		comment is 'True if the argument is a valid provenance-aware reassembled telecommand service-unit term.',
		argnames is ['ReassembledSegment']
	]).

	:- public(segment_header_suffixes/2).
	:- mode(segment_header_suffixes(+compound, -list(list(byte))), one).
	:- info(segment_header_suffixes/2, [
		comment is 'Extracts the ordered list of per-frame mission-specific segment-header suffixes that contributed to a provenance-aware reassembled telecommand service-unit term.',
		argnames is ['ReassembledSegment', 'HeaderSuffixes']
	]).

	:- public(segment_data/2).
	:- mode(segment_data(+compound, -list(byte)), one).
	:- info(segment_data/2, [
		comment is 'Extracts the telecommand segment data bytes.',
		argnames is ['Segment', 'Data']
	]).

	:- public(valid_reassembly_state/1).
	:- mode(valid_reassembly_state(@compound), zero_or_one).
	:- info(valid_reassembly_state/1, [
		comment is 'True if the argument is a valid telecommand segment reassembly state term.',
		argnames is ['State']
	]).

	:- public(initial_reassembly_state/1).
	:- mode(initial_reassembly_state(-compound), one).
	:- info(initial_reassembly_state/1, [
		comment is 'Returns the initial telecommand segment reassembly state.',
		argnames is ['State']
	]).

	:- public(pending_fragments/2).
	:- mode(pending_fragments(+compound, -list(compound)), one).
	:- info(pending_fragments/2, [
		comment is 'Extracts the pending segmented telecommand data fragments keyed by spacecraft, virtual channel, and MAP identifier.',
		argnames is ['State', 'PendingFragments']
	]).

	:- public(valid_discontinuity_policy/1).
	:- mode(valid_discontinuity_policy(@atom), zero_or_one).
	:- info(valid_discontinuity_policy/1, [
		comment is 'True if the argument is a valid discontinuity recovery policy atom. Valid values are ``throw``, ``drop``, and ``resynchronize``.',
		argnames is ['Policy']
	]).

	:- public(extract_tc_segment/2).
	:- mode(extract_tc_segment(+compound, -compound), one_or_error).
	:- info(extract_tc_segment/2, [
		comment is 'Extracts a telecommand segment from a telecommand transfer frame using the representation ``tc_segment(SequenceFlags, MapId, HeaderSuffix, Data)``. When mission-specific segment headers use multiple octets, only the first octet is interpreted for the standard MAP and sequence semantics and the remaining octets are returned as ``HeaderSuffix``.',
		argnames is ['Frame', 'Segment']
	]).

	:- public(insert_tc_segment/3).
	:- mode(insert_tc_segment(+compound, +compound, -compound), one_or_error).
	:- info(insert_tc_segment/3, [
		comment is 'Updates a telecommand transfer frame from a telecommand segment term by encoding the standard MAP and sequence semantics in the first segment-header octet and preserving any remaining mission-specific segment-header octets.',
		argnames is ['Segment', 'Frame', 'UpdatedFrame']
	]).

	:- public(reassemble_tc_frame/4).
	:- mode(reassemble_tc_frame(+compound, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_tc_frame/4, [
		comment is 'Reassembles complete telecommand service units from a single telecommand transfer frame using the default ``throw`` discontinuity recovery policy.',
		argnames is ['Frame', 'State', 'Segments', 'UpdatedState']
	]).

	:- public(reassemble_tc_frame/5).
	:- mode(reassemble_tc_frame(+compound, +atom, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_tc_frame/5, [
		comment is 'Reassembles complete telecommand service units from a single telecommand transfer frame using the selected discontinuity recovery policy.',
		argnames is ['Frame', 'Policy', 'State', 'Segments', 'UpdatedState']
	]).

	:- public(reassemble_tc_frame/6).
	:- mode(reassemble_tc_frame(+compound, +atom, +compound, -list(compound), -compound, -list(compound)), one_or_error).
	:- info(reassemble_tc_frame/6, [
		comment is 'Reassembles complete telecommand service units from a single telecommand transfer frame using the selected discontinuity recovery policy and returns any recovery events.',
		argnames is ['Frame', 'Policy', 'State', 'Segments', 'UpdatedState', 'Events']
	]).

	:- public(reassemble_tc_frames/4).
	:- mode(reassemble_tc_frames(+list(compound), +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_tc_frames/4, [
		comment is 'Reassembles complete telecommand service units across a sequence of telecommand transfer frames using the default ``throw`` discontinuity recovery policy.',
		argnames is ['Frames', 'State', 'Segments', 'UpdatedState']
	]).

	:- public(reassemble_tc_frames/5).
	:- mode(reassemble_tc_frames(+list(compound), +atom, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_tc_frames/5, [
		comment is 'Reassembles complete telecommand service units across a sequence of telecommand transfer frames using the selected discontinuity recovery policy.',
		argnames is ['Frames', 'Policy', 'State', 'Segments', 'UpdatedState']
	]).

	:- public(reassemble_tc_frames/6).
	:- mode(reassemble_tc_frames(+list(compound), +atom, +compound, -list(compound), -compound, -list(compound)), one_or_error).
	:- info(reassemble_tc_frames/6, [
		comment is 'Reassembles complete telecommand service units across a sequence of telecommand transfer frames using the selected discontinuity recovery policy and returns any recovery events in frame order.',
		argnames is ['Frames', 'Policy', 'State', 'Segments', 'UpdatedState', 'Events']
	]).

	:- public(reassemble_tc_frame_with_provenance/5).
	:- mode(reassemble_tc_frame_with_provenance(+compound, +compound, -list(compound), -compound, -list(compound)), one_or_error).
	:- info(reassemble_tc_frame_with_provenance/5, [
		comment is 'Reassembles complete telecommand service units from a single telecommand transfer frame using the default ``throw`` discontinuity recovery policy and returns provenance-aware reassembled service-unit terms.',
		argnames is ['Frame', 'State', 'ReassembledSegments', 'UpdatedState', 'Events']
	]).

	:- public(reassemble_tc_frame_with_provenance/6).
	:- mode(reassemble_tc_frame_with_provenance(+compound, +atom, +compound, -list(compound), -compound, -list(compound)), one_or_error).
	:- info(reassemble_tc_frame_with_provenance/6, [
		comment is 'Reassembles complete telecommand service units from a single telecommand transfer frame using the selected discontinuity recovery policy and returns provenance-aware reassembled service-unit terms plus any recovery events.',
		argnames is ['Frame', 'Policy', 'State', 'ReassembledSegments', 'UpdatedState', 'Events']
	]).

	:- public(reassemble_tc_frames_with_provenance/5).
	:- mode(reassemble_tc_frames_with_provenance(+list(compound), +compound, -list(compound), -compound, -list(compound)), one_or_error).
	:- info(reassemble_tc_frames_with_provenance/5, [
		comment is 'Reassembles complete telecommand service units across a sequence of telecommand transfer frames using the default ``throw`` discontinuity recovery policy and returns provenance-aware reassembled service-unit terms.',
		argnames is ['Frames', 'State', 'ReassembledSegments', 'UpdatedState', 'Events']
	]).

	:- public(reassemble_tc_frames_with_provenance/6).
	:- mode(reassemble_tc_frames_with_provenance(+list(compound), +atom, +compound, -list(compound), -compound, -list(compound)), one_or_error).
	:- info(reassemble_tc_frames_with_provenance/6, [
		comment is 'Reassembles complete telecommand service units across a sequence of telecommand transfer frames using the selected discontinuity recovery policy and returns provenance-aware reassembled service-unit terms plus any recovery events in frame order.',
		argnames is ['Frames', 'Policy', 'State', 'ReassembledSegments', 'UpdatedState', 'Events']
	]).

	:- public(dispatch_service_units_by_map/2).
	:- mode(dispatch_service_units_by_map(+list(compound), -list(compound)), one_or_error).
	:- info(dispatch_service_units_by_map/2, [
		comment is 'Groups complete telecommand service-unit terms by MAP identifier, preserving the original service-unit order within each MAP bucket and the first-seen order of MAP buckets. Accepts both telecommand segment terms and provenance-aware reassembled telecommand service-unit terms.',
		argnames is ['ServiceUnits', 'Dispatches']
	]).

	:- public(dispatch_service_units_by_map/3).
	:- mode(dispatch_service_units_by_map(+list(compound), +integer, -list(compound)), one_or_error).
	:- info(dispatch_service_units_by_map/3, [
		comment is 'Extracts the complete telecommand service units for a specific MAP identifier from a list of complete telecommand service-unit terms.',
		argnames is ['ServiceUnits', 'MapId', 'DispatchedServiceUnits']
	]).

	:- uses(list, [
		append/3
	]).

	:- uses(type, [
		valid/2
	]).

	valid_segment(Segment) :-
		segment_term_parts(Segment, SequenceFlags, MapId, HeaderSuffix, Data),
		valid_sequence_flags(SequenceFlags),
		valid_map_id(MapId),
		valid(list(byte), HeaderSuffix),
		valid(list(byte), Data).

	sequence_flags(Segment, SequenceFlags) :-
		segment_term_parts(Segment, SequenceFlags, _, _, _).

	map_id(ServiceUnit, MapId) :-
		service_unit_map_id(ServiceUnit, MapId).

	valid_map_dispatch(map_dispatch(MapId, ServiceUnits)) :-
		valid_map_id(MapId),
		valid_service_units(ServiceUnits),
		all_service_units_for_map(ServiceUnits, MapId).

	dispatch_map_id(map_dispatch(MapId, _), MapId).

	dispatch_service_units(map_dispatch(_, ServiceUnits), ServiceUnits).

	segment_header_suffix(Segment, HeaderSuffix) :-
		segment_term_parts(Segment, _, _, HeaderSuffix, _).

	valid_reassembled_segment(tc_reassembled_segment(MapId, HeaderSuffixes, Data)) :-
		valid_map_id(MapId),
		valid_header_suffixes_provenance(HeaderSuffixes),
		valid(list(byte), Data).

	segment_header_suffixes(tc_reassembled_segment(_, HeaderSuffixes, _), HeaderSuffixes).

	segment_data(Segment, Data) :-
		(   segment_term_parts(Segment, _, _, _, Data) ->
			true
		;   Segment = tc_reassembled_segment(_, _, Data)
		).

	valid_reassembly_state(tc_reassembly_state(Channels)) :-
		valid_reassembly_channels(Channels).

	initial_reassembly_state(tc_reassembly_state([])).

	pending_fragments(tc_reassembly_state(Channels), PendingFragments) :-
		pending_fragments_(Channels, PendingFragments).

	valid_discontinuity_policy(throw).
	valid_discontinuity_policy(drop).
	valid_discontinuity_policy(resynchronize).

	extract_tc_segment(Frame, Segment) :-
		(   var(Frame) ->
			instantiation_error
		;   tc_segment_context(Frame, _, _, _, Segment)
		).

	insert_tc_segment(Segment, Frame, UpdatedFrame) :-
		(   var(Segment) ->
			instantiation_error
		;   var(Frame) ->
			instantiation_error
		;   Segment = tc_segment(SequenceFlags, MapId, Data),
			valid_segment(Segment) ->
			Frame = tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader0, _, FECF),
			extract_segment_header_suffix(SegmentHeader0, HeaderSuffix),
			encode_segment_header(SequenceFlags, MapId, HeaderSuffix, SegmentHeader),
			UpdatedFrame = tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, Data, FECF)
		;   Segment = tc_segment(SequenceFlags, MapId, HeaderSuffix, Data),
			valid_segment(Segment) ->
			Frame = tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader0, _, FECF),
			(   SegmentHeader0 = none
			;   extract_segment_header_suffix(SegmentHeader0, _)
			),
			encode_segment_header(SequenceFlags, MapId, HeaderSuffix, SegmentHeader),
			UpdatedFrame = tc_transfer_frame(Version, BypassFlag, ControlCommandFlag, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, Data, FECF)
		;   domain_error(ccsds_tc_segment, Segment)
		).

	reassemble_tc_frame(Frame, State, Segments, UpdatedState) :-
		reassemble_tc_frame(Frame, throw, State, Segments, UpdatedState).

	reassemble_tc_frame(Frame, Policy, State, Segments, UpdatedState) :-
		reassemble_tc_frame(Frame, Policy, State, Segments, UpdatedState, _).

	reassemble_tc_frame(Frame, Policy, State, Segments, UpdatedState, Events) :-
		(   var(Frame) ->
			instantiation_error
		;   var(Policy) ->
			instantiation_error
		;   var(State) ->
			instantiation_error
		;   valid_discontinuity_policy_(Policy),
			valid_reassembly_state_(State),
			reassemble_tc_frame_with_provenance(Frame, Policy, State, ReassembledSegments, UpdatedState, Events),
			strip_reassembled_segments(ReassembledSegments, Segments)
		).

	reassemble_tc_frames(Frames, State, Segments, UpdatedState) :-
		reassemble_tc_frames(Frames, throw, State, Segments, UpdatedState).

	reassemble_tc_frames(Frames, Policy, State, Segments, UpdatedState) :-
		reassemble_tc_frames(Frames, Policy, State, Segments, UpdatedState, _).

	reassemble_tc_frames(Frames, Policy, State, Segments, UpdatedState, Events) :-
		(   var(Frames) ->
			instantiation_error
		;   var(Policy) ->
			instantiation_error
		;   var(State) ->
			instantiation_error
		;   valid_discontinuity_policy_(Policy),
			valid_reassembly_state_(State),
			reassemble_tc_frames_with_provenance(Frames, Policy, State, ReassembledSegments, UpdatedState, Events),
			strip_reassembled_segments(ReassembledSegments, Segments)
		).

	reassemble_tc_frame_with_provenance(Frame, State, ReassembledSegments, UpdatedState, Events) :-
		reassemble_tc_frame_with_provenance(Frame, throw, State, ReassembledSegments, UpdatedState, Events).

	reassemble_tc_frame_with_provenance(Frame, Policy, State, ReassembledSegments, UpdatedState, Events) :-
		(   var(Frame) ->
			instantiation_error
		;   var(Policy) ->
			instantiation_error
		;   var(State) ->
			instantiation_error
		;   valid_discontinuity_policy_(Policy),
			valid_reassembly_state_(State),
			tc_segment_context(Frame, SpacecraftId, VirtualChannelId, SequenceNumber, Segment),
			channel_state(State, SpacecraftId, VirtualChannelId, SequenceNumber, Policy, RecoveryMode, PendingSegments, RecoveryEvents),
			reassemble_segment_with_recovery(RecoveryMode, Segment, PendingSegments, ReassembledSegments, UpdatedPendingSegments),
			next_sequence_number(SequenceNumber, ExpectedSequenceNumber),
			update_channel_state(State, SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, UpdatedPendingSegments, UpdatedState),
			Events = RecoveryEvents
		).

	reassemble_tc_frames_with_provenance(Frames, State, ReassembledSegments, UpdatedState, Events) :-
		reassemble_tc_frames_with_provenance(Frames, throw, State, ReassembledSegments, UpdatedState, Events).

	reassemble_tc_frames_with_provenance(Frames, Policy, State, ReassembledSegments, UpdatedState, Events) :-
		(   var(Frames) ->
			instantiation_error
		;   var(Policy) ->
			instantiation_error
		;   var(State) ->
			instantiation_error
		;   valid_discontinuity_policy_(Policy),
			valid_reassembly_state_(State),
			reassemble_tc_frames_with_provenance_(Frames, Policy, State, ReassembledSegments, UpdatedState, Events)
		).

	reassemble_tc_frames_with_provenance_([Frame| Frames], Policy, State, ReassembledSegments, UpdatedState, Events) :-
		reassemble_tc_frame_with_provenance(Frame, Policy, State, FrameSegments, IntermediateState, FrameEvents),
		append(FrameSegments, RemainingSegments, ReassembledSegments),
		append(FrameEvents, RemainingEvents, Events),
		reassemble_tc_frames_with_provenance_(Frames, Policy, IntermediateState, RemainingSegments, UpdatedState, RemainingEvents).
	reassemble_tc_frames_with_provenance_([], _, State, [], State, []).

	dispatch_service_units_by_map(ServiceUnits, Dispatches) :-
		(   var(ServiceUnits) ->
			instantiation_error
		;   valid_service_units(ServiceUnits) ->
			dispatch_service_units_by_map_(ServiceUnits, [], Dispatches)
		;   domain_error(ccsds_tc_service_units, ServiceUnits)
		).

	dispatch_service_units_by_map(ServiceUnits, MapId, DispatchedServiceUnits) :-
		(   var(ServiceUnits) ->
			instantiation_error
		;   var(MapId) ->
			instantiation_error
		;   valid_service_units(ServiceUnits) ->
			check_map_id_(MapId),
			dispatch_service_units_for_map(ServiceUnits, MapId, DispatchedServiceUnits)
		;   domain_error(ccsds_tc_service_units, ServiceUnits)
		).

	dispatch_service_units_by_map_([ServiceUnit| ServiceUnits], Dispatches0, Dispatches) :-
		service_unit_map_id(ServiceUnit, MapId),
		insert_dispatched_service_unit(Dispatches0, MapId, ServiceUnit, Dispatches1),
		dispatch_service_units_by_map_(ServiceUnits, Dispatches1, Dispatches).
	dispatch_service_units_by_map_([], Dispatches, Dispatches).

	dispatch_service_units_for_map([ServiceUnit| ServiceUnits], MapId, [ServiceUnit| DispatchedServiceUnits]) :-
		service_unit_map_id(ServiceUnit, MapId),
		!,
		dispatch_service_units_for_map(ServiceUnits, MapId, DispatchedServiceUnits).
	dispatch_service_units_for_map([_| ServiceUnits], MapId, DispatchedServiceUnits) :-
		!,
		dispatch_service_units_for_map(ServiceUnits, MapId, DispatchedServiceUnits).
	dispatch_service_units_for_map([], _, []).

	insert_dispatched_service_unit([], MapId, ServiceUnit, [map_dispatch(MapId, [ServiceUnit])]).
	insert_dispatched_service_unit([map_dispatch(MapId, ServiceUnits0)| Dispatches], MapId, ServiceUnit, [map_dispatch(MapId, ServiceUnits)| Dispatches]) :-
		!,
		append(ServiceUnits0, [ServiceUnit], ServiceUnits).
	insert_dispatched_service_unit([Dispatch| Dispatches0], MapId, ServiceUnit, [Dispatch| Dispatches]) :-
		insert_dispatched_service_unit(Dispatches0, MapId, ServiceUnit, Dispatches).

	tc_segment_context(tc_transfer_frame(_, _, _, SpacecraftId, VirtualChannelId, SequenceNumber, SegmentHeader, DataField, _), SpacecraftId, VirtualChannelId, SequenceNumber, Segment) :-
		!,
		decode_segment_header(SegmentHeader, SequenceFlags, MapId, HeaderSuffix),
		valid(list(byte), DataField),
		Segment = tc_segment(SequenceFlags, MapId, HeaderSuffix, DataField).
	tc_segment_context(Frame, _, _, _, _) :-
		domain_error(ccsds_tc_transfer_frame_term, Frame).

	decode_segment_header(segment_header([HeaderByte| HeaderSuffix]), SequenceFlags, MapId, HeaderSuffix) :-
		valid(byte, HeaderByte),
		valid(list(byte), HeaderSuffix),
		Code is (HeaderByte >> 6) /\ 0x03,
		MapId is HeaderByte /\ 0x3F,
		decode_sequence_flags(Code, SequenceFlags),
		!.
	decode_segment_header(SegmentHeader, _, _, _) :-
		domain_error(ccsds_tc_segment_header, SegmentHeader).

	encode_segment_header(SequenceFlags, MapId, HeaderSuffix, SegmentHeader) :-
		valid_sequence_flags(SequenceFlags),
		valid_map_id(MapId),
		valid(list(byte), HeaderSuffix),
		encode_sequence_flags(SequenceFlags, Code),
		HeaderByte is ((Code /\ 0x03) << 6) \/ (MapId /\ 0x3F),
		SegmentHeader = segment_header([HeaderByte| HeaderSuffix]).

	extract_segment_header_suffix(segment_header([_| HeaderSuffix]), HeaderSuffix) :-
		!.
	extract_segment_header_suffix(none, []) :-
		!.
	extract_segment_header_suffix(SegmentHeader, _) :-
		domain_error(ccsds_tc_segment_header, SegmentHeader).

	decode_sequence_flags(0, continuation).
	decode_sequence_flags(1, first).
	decode_sequence_flags(2, last).
	decode_sequence_flags(3, unsegmented).

	encode_sequence_flags(continuation, 0).
	encode_sequence_flags(first, 1).
	encode_sequence_flags(last, 2).
	encode_sequence_flags(unsegmented, 3).

	channel_state(tc_reassembly_state(Channels), SpacecraftId, VirtualChannelId, SequenceNumber, Policy, RecoveryMode, PendingSegments, Events) :-
		(   select_channel_entry(SpacecraftId, VirtualChannelId, Channels, tc_reassembly_channel(SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, StoredPendingSegments), _) ->
			channel_recovery_mode(Policy, SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, SequenceNumber, StoredPendingSegments, RecoveryMode, PendingSegments, Events)
		;   RecoveryMode = normal,
			PendingSegments = [],
			Events = []
		).

	channel_recovery_mode(_, _, _, ExpectedSequenceNumber, SequenceNumber, PendingSegments, normal, PendingSegments, []) :-
		ExpectedSequenceNumber =:= SequenceNumber,
		!.
	channel_recovery_mode(throw, SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, SequenceNumber, _, _, _, _) :-
		domain_error(ccsds_tc_transfer_frame_sequence, tc_transfer_frame_sequence(SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, SequenceNumber)).
	channel_recovery_mode(drop, SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, SequenceNumber, PendingSegments, drop, [], Events) :-
		discontinuity_events(drop, SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, SequenceNumber, PendingSegments, Events).
	channel_recovery_mode(resynchronize, SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, SequenceNumber, PendingSegments, resynchronize, [], Events) :-
		discontinuity_events(resynchronize, SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, SequenceNumber, PendingSegments, Events).

	discontinuity_events(Policy, SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, SequenceNumber, PendingSegments, Events) :-
		Discontinuity = discontinuity(ExpectedSequenceNumber, SequenceNumber),
		dropped_fragment_events(SpacecraftId, VirtualChannelId, PendingSegments, Discontinuity, DroppedEvents),
		recovery_action_event(Policy, SpacecraftId, VirtualChannelId, Discontinuity, ActionEvents),
		append(DroppedEvents, ActionEvents, Events).

	dropped_fragment_events(_, _, [], _, []) :-
		!.
	dropped_fragment_events(SpacecraftId, VirtualChannelId, [PendingSegment| PendingSegments], Reason, [dropped_fragment(SpacecraftId, VirtualChannelId, MapId, Reason, PendingData)| Events]) :-
		pending_segment_parts(PendingSegment, MapId, PendingData, _),
		dropped_fragment_events(SpacecraftId, VirtualChannelId, PendingSegments, Reason, Events).

	recovery_action_event(drop, SpacecraftId, VirtualChannelId, Discontinuity, [skipped_frame(SpacecraftId, VirtualChannelId, Discontinuity)]).
	recovery_action_event(resynchronize, SpacecraftId, VirtualChannelId, Discontinuity, [resynchronized(SpacecraftId, VirtualChannelId, Discontinuity)]).

	reassemble_segment_with_recovery(normal, Segment, PendingSegments, Segments, UpdatedPendingSegments) :-
		reassemble_segment(Segment, PendingSegments, Segments, UpdatedPendingSegments).
	reassemble_segment_with_recovery(drop, _, _, [], []).
	reassemble_segment_with_recovery(resynchronize, Segment, _, Segments, UpdatedPendingSegments) :-
		reassemble_segment(Segment, [], Segments, UpdatedPendingSegments).

	reassemble_segment(Segment, PendingSegments, [ReassembledSegment], PendingSegments) :-
		segment_term_parts(Segment, unsegmented, MapId, HeaderSuffix, Data),
		!,
		(   select_pending_segment(MapId, PendingSegments, _, _, _) ->
			domain_error(ccsds_tc_segment_reassembly, existing_pending_segment(MapId))
		;   ReassembledSegment = tc_reassembled_segment(MapId, [HeaderSuffix], Data)
		).
	reassemble_segment(Segment, PendingSegments, [], [tc_pending_segment(MapId, Data, [HeaderSuffix])| PendingSegments]) :-
		segment_term_parts(Segment, first, MapId, HeaderSuffix, Data),
		!,
		(   select_pending_segment(MapId, PendingSegments, _, _, _) ->
			domain_error(ccsds_tc_segment_reassembly, existing_pending_segment(MapId))
		;   true
		).
	reassemble_segment(Segment, PendingSegments, [], UpdatedPendingSegments) :-
		segment_term_parts(Segment, continuation, MapId, HeaderSuffix, Data),
		!,
		(   select_pending_segment(MapId, PendingSegments, PendingData, PendingHeaderSuffixes, OtherPendingSegments) ->
			append(PendingData, Data, UpdatedData),
			append(PendingHeaderSuffixes, [HeaderSuffix], UpdatedHeaderSuffixes),
			UpdatedPendingSegments = [tc_pending_segment(MapId, UpdatedData, UpdatedHeaderSuffixes)| OtherPendingSegments]
		;   domain_error(ccsds_tc_segment_reassembly, missing_first_segment(MapId))
		).
	reassemble_segment(Segment, PendingSegments, [tc_reassembled_segment(MapId, HeaderSuffixes, ReassembledData)], UpdatedPendingSegments) :-
		segment_term_parts(Segment, last, MapId, HeaderSuffix, Data),
		(   select_pending_segment(MapId, PendingSegments, PendingData, PendingHeaderSuffixes, UpdatedPendingSegments) ->
			append(PendingData, Data, ReassembledData),
			append(PendingHeaderSuffixes, [HeaderSuffix], HeaderSuffixes)
		;   domain_error(ccsds_tc_segment_reassembly, missing_first_segment(MapId))
		).

	segment_term_parts(tc_segment(SequenceFlags, MapId, Data), SequenceFlags, MapId, [], Data).
	segment_term_parts(tc_segment(SequenceFlags, MapId, HeaderSuffix, Data), SequenceFlags, MapId, HeaderSuffix, Data).

	service_unit_map_id(ServiceUnit, MapId) :-
		(   segment_term_parts(ServiceUnit, _, MapId, _, _) ->
			true
		;   ServiceUnit = tc_reassembled_segment(MapId, _, _)
		).

	select_pending_segment(MapId, [PendingSegment| PendingSegments], PendingData, PendingHeaderSuffixes, PendingSegments) :-
		pending_segment_parts(PendingSegment, MapId, PendingData, PendingHeaderSuffixes),
		!.
	select_pending_segment(MapId, [PendingSegment| PendingSegments], PendingData, PendingHeaderSuffixes, [PendingSegment| OtherPendingSegments]) :-
		select_pending_segment(MapId, PendingSegments, PendingData, PendingHeaderSuffixes, OtherPendingSegments).

	update_channel_state(tc_reassembly_state(Channels), SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, PendingSegments, tc_reassembly_state(UpdatedChannels)) :-
		(   select_channel_entry(SpacecraftId, VirtualChannelId, Channels, _, OtherChannels) ->
			true
		;   OtherChannels = Channels
		),
		UpdatedChannels = [tc_reassembly_channel(SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, PendingSegments)| OtherChannels].

	select_channel_entry(SpacecraftId, VirtualChannelId, [Entry| Entries], Entry, Entries) :-
		Entry = tc_reassembly_channel(SpacecraftId, VirtualChannelId, _, _),
		!.
	select_channel_entry(SpacecraftId, VirtualChannelId, [Entry| Entries], SelectedEntry, [Entry| Rest]) :-
		select_channel_entry(SpacecraftId, VirtualChannelId, Entries, SelectedEntry, Rest).

	next_sequence_number(SequenceNumber, ExpectedSequenceNumber) :-
		ExpectedSequenceNumber is (SequenceNumber + 1) mod 256.

	pending_fragments_([tc_reassembly_channel(SpacecraftId, VirtualChannelId, _, PendingSegments)| Channels], PendingFragments) :-
		pending_fragments_channel_(PendingSegments, SpacecraftId, VirtualChannelId, ChannelPendingFragments),
		append(ChannelPendingFragments, RemainingPendingFragments, PendingFragments),
		pending_fragments_(Channels, RemainingPendingFragments).
	pending_fragments_([], []).

	pending_fragments_channel_([PendingSegment| PendingSegments], SpacecraftId, VirtualChannelId, [pending_fragment(SpacecraftId, VirtualChannelId, MapId, PendingData)| PendingFragments]) :-
		pending_segment_parts(PendingSegment, MapId, PendingData, _),
		pending_fragments_channel_(PendingSegments, SpacecraftId, VirtualChannelId, PendingFragments).
	pending_fragments_channel_([], _, _, []).

	valid_reassembly_state_(State) :-
		valid_reassembly_state(State),
		!.
	valid_reassembly_state_(State) :-
		domain_error(ccsds_tc_reassembly_state, State).

	valid_discontinuity_policy_(Policy) :-
		valid_discontinuity_policy(Policy),
		!.
	valid_discontinuity_policy_(Policy) :-
		domain_error(ccsds_tc_discontinuity_policy, Policy).

	valid_reassembly_channels([Channel| Channels]) :-
		valid_reassembly_channel(Channel),
		no_duplicate_channel(Channel, Channels),
		valid_reassembly_channels(Channels).
	valid_reassembly_channels([]).

	valid_reassembly_channel(tc_reassembly_channel(SpacecraftId, VirtualChannelId, ExpectedSequenceNumber, PendingSegments)) :-
		valid(between(integer, 0, 1023), SpacecraftId),
		valid(between(integer, 0, 63), VirtualChannelId),
		valid(between(integer, 0, 255), ExpectedSequenceNumber),
		valid_pending_segments(PendingSegments).

	valid_pending_segments([PendingSegment| PendingSegments]) :-
		valid_pending_segment(PendingSegment),
		no_duplicate_pending_segment(PendingSegment, PendingSegments),
		valid_pending_segments(PendingSegments).
	valid_pending_segments([]).

	valid_service_units([ServiceUnit| ServiceUnits]) :-
		valid_service_unit(ServiceUnit),
		valid_service_units(ServiceUnits).
	valid_service_units([]).

	valid_service_unit(ServiceUnit) :-
		(   valid_segment(ServiceUnit) ->
			true
		;   valid_reassembled_segment(ServiceUnit)
		).

	valid_pending_segment(PendingSegment) :-
		pending_segment_parts(PendingSegment, MapId, PendingData, PendingHeaderSuffixes),
		valid_map_id(MapId),
		valid(list(byte), PendingData),
		valid_header_suffixes_provenance(PendingHeaderSuffixes).

	pending_segment_parts(tc_pending_segment(MapId, PendingData), MapId, PendingData, []).
	pending_segment_parts(tc_pending_segment(MapId, PendingData, PendingHeaderSuffixes), MapId, PendingData, PendingHeaderSuffixes).

	no_duplicate_channel(tc_reassembly_channel(SpacecraftId, VirtualChannelId, _, _), [tc_reassembly_channel(SpacecraftId, VirtualChannelId, _, _)| _]) :-
		!,
		fail.
	no_duplicate_channel(Channel, [_| Channels]) :-
		!,
		no_duplicate_channel(Channel, Channels).
	no_duplicate_channel(_, []).

	no_duplicate_pending_segment(PendingSegment, [OtherPendingSegment| _]) :-
		pending_segment_parts(PendingSegment, MapId, _, _),
		pending_segment_parts(OtherPendingSegment, MapId, _, _),
		!,
		fail.
	no_duplicate_pending_segment(PendingSegment, [_| PendingSegments]) :-
		!,
		no_duplicate_pending_segment(PendingSegment, PendingSegments).
	no_duplicate_pending_segment(_, []).

	valid_header_suffixes_provenance([]).
	valid_header_suffixes_provenance([HeaderSuffix| HeaderSuffixes]) :-
		valid(list(byte), HeaderSuffix),
		valid_header_suffixes_provenance_(HeaderSuffixes).

	valid_header_suffixes_provenance_([HeaderSuffix| HeaderSuffixes]) :-
		valid(list(byte), HeaderSuffix),
		valid_header_suffixes_provenance_(HeaderSuffixes).
	valid_header_suffixes_provenance_([]).

	all_service_units_for_map([], _).
	all_service_units_for_map([ServiceUnit| ServiceUnits], MapId) :-
		service_unit_map_id(ServiceUnit, MapId),
		all_service_units_for_map(ServiceUnits, MapId).

	strip_reassembled_segments([tc_reassembled_segment(MapId, [HeaderSuffix], Data)| ReassembledSegments], [tc_segment(unsegmented, MapId, HeaderSuffix, Data)| Segments]) :-
		!,
		strip_reassembled_segments(ReassembledSegments, Segments).
	strip_reassembled_segments([tc_reassembled_segment(MapId, _, Data)| ReassembledSegments], [tc_segment(unsegmented, MapId, [], Data)| Segments]) :-
		strip_reassembled_segments(ReassembledSegments, Segments).
	strip_reassembled_segments([], []).

	check_map_id_(MapId) :-
		valid_map_id(MapId),
		!.
	check_map_id_(MapId) :-
		domain_error(ccsds_tc_map_id, MapId).

	valid_sequence_flags(continuation).
	valid_sequence_flags(first).
	valid_sequence_flags(last).
	valid_sequence_flags(unsegmented).

	valid_map_id(MapId) :-
		valid(between(integer, 0, 63), MapId).

:- end_object.
