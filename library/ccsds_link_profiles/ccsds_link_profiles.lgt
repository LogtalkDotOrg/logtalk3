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


:- object(ccsds_link_profiles).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-20,
		comment is 'Ergonomic wrappers around the CCSDS TM, TC, and AOS frame objects using explicit profile terms.'
	]).

	:- public(valid_profile/1).
	:- mode(valid_profile(@compound), zero_or_one).
	:- info(valid_profile/1, [
		comment is 'True if the argument is a supported CCSDS link profile term.',
		argnames is ['Profile']
	]).

	:- public(parse_frame/3).
	:- mode(parse_frame(+compound, +compound, -compound), one_or_error).
	:- info(parse_frame/3, [
		comment is 'Parses exactly one CCSDS transfer frame from a source using a link profile term. Supported source terms are ``file(File)``, ``stream(Stream)``, and ``bytes(Bytes)``.',
		argnames is ['Source', 'Profile', 'Frame'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Profile`` is a variable' - instantiation_error,
			'``Profile`` is neither a variable nor a supported link profile term' - domain_error(ccsds_link_profile, 'Profile'),
			'``Source`` does not contain exactly one frame for the selected profile' - domain_error(ccsds_single_frame_source, 'Source')
		]
	]).

	:- public(parse_frames/3).
	:- mode(parse_frames(+compound, +compound, -list(compound)), one_or_error).
	:- info(parse_frames/3, [
		comment is 'Parses zero or more CCSDS transfer frames from a source using a link profile term. Supported source terms are ``file(File)``, ``stream(Stream)``, and ``bytes(Bytes)``.',
		argnames is ['Source', 'Profile', 'Frames'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Profile`` is a variable' - instantiation_error,
			'``Profile`` is neither a variable nor a supported link profile term' - domain_error(ccsds_link_profile, 'Profile')
		]
	]).

	:- public(generate_frame/3).
	:- mode(generate_frame(+compound, +compound, +compound), one_or_error).
	:- info(generate_frame/3, [
		comment is 'Generates exactly one CCSDS transfer frame to a sink using a link profile term. Supported sink terms are ``file(File)``, ``stream(Stream)``, and ``bytes(Bytes)``.',
		argnames is ['Sink', 'Profile', 'Frame'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Profile`` is a variable' - instantiation_error,
			'``Frame`` is a variable' - instantiation_error,
			'``Profile`` is neither a variable nor a supported link profile term' - domain_error(ccsds_link_profile, 'Profile'),
			'``Frame`` is neither a variable nor a valid frame term for the selected profile' - domain_error(ccsds_frame_term, 'Frame')
		]
	]).

	:- public(generate_frames/3).
	:- mode(generate_frames(+compound, +compound, +list(compound)), one_or_error).
	:- info(generate_frames/3, [
		comment is 'Generates zero or more CCSDS transfer frames to a sink using a link profile term. Supported sink terms are ``file(File)``, ``stream(Stream)``, and ``bytes(Bytes)``.',
		argnames is ['Sink', 'Profile', 'Frames'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Profile`` is a variable' - instantiation_error,
			'``Frames`` is a variable' - instantiation_error,
			'``Profile`` is neither a variable nor a supported link profile term' - domain_error(ccsds_link_profile, 'Profile'),
			'``Frames`` is neither a variable nor a list of valid frame terms for the selected profile' - domain_error(ccsds_frame_terms, 'Frames')
		]
	]).

	:- public(valid_reassembly_state/1).
	:- mode(valid_reassembly_state(@compound), zero_or_one).
	:- info(valid_reassembly_state/1, [
		comment is 'True if the argument is a valid TM/AOS channel packet reassembly state term for the profile-level packet reassembly predicates.',
		argnames is ['State']
	]).

	:- public(initial_reassembly_state/1).
	:- mode(initial_reassembly_state(-compound), one).
	:- info(initial_reassembly_state/1, [
		comment is 'Returns the initial TM/AOS channel packet reassembly state for the profile-level packet reassembly predicates.',
		argnames is ['State']
	]).

	:- public(pending_fragments/2).
	:- mode(pending_fragments(+compound, -list(compound)), one_or_error).
	:- info(pending_fragments/2, [
		comment is 'Returns the non-empty pending packet fragments currently buffered per TM or AOS virtual channel.',
		argnames is ['State', 'PendingFragments'],
		exceptions is [
			'``State`` is a variable' - instantiation_error,
			'``State`` is not a valid channel reassembly state term' - domain_error(ccsds_channel_reassembly_state, 'State')
		]
	]).

	:- public(valid_discontinuity_policy/1).
	:- mode(valid_discontinuity_policy(@atom), zero_or_one).
	:- info(valid_discontinuity_policy/1, [
		comment is 'True if the argument is a valid packet reassembly discontinuity recovery policy atom. Valid values are ``throw``, ``drop``, and ``resynchronize``.',
		argnames is ['Policy']
	]).

	:- public(extract_packets/4).
	:- mode(extract_packets(+compound, +integer, +compound, -compound), one_or_error).
	:- info(extract_packets/4, [
		comment is 'Extracts the TM first-header-pointer packet zone or the AOS M_PDU packet zone from a TM or AOS frame using the selected link profile and packet secondary header length.',
		argnames is ['Profile', 'SecondaryHeaderLength', 'Frame', 'PacketZone'],
		exceptions is [
			'``Profile`` is a variable' - instantiation_error,
			'``SecondaryHeaderLength`` is a variable' - instantiation_error,
			'``Frame`` is a variable' - instantiation_error,
			'``Profile`` is neither a variable nor a supported link profile term' - domain_error(ccsds_link_profile, 'Profile'),
			'``Profile`` is a telecommand profile and therefore does not support packet extraction' - domain_error(ccsds_packet_link_profile, 'Profile'),
			'``Frame`` is neither a variable nor a valid frame term for the selected profile' - domain_error(ccsds_frame_term, 'Frame')
		]
	]).

	:- public(insert_packets/5).
	:- mode(insert_packets(+compound, +integer, +compound, +compound, -compound), one_or_error).
	:- info(insert_packets/5, [
		comment is 'Regenerates the TM first-header-pointer packet zone or the AOS M_PDU packet zone for a TM or AOS frame using the selected link profile and packet secondary header length.',
		argnames is ['Profile', 'SecondaryHeaderLength', 'PacketZone', 'Frame', 'UpdatedFrame'],
		exceptions is [
			'``Profile`` is a variable' - instantiation_error,
			'``SecondaryHeaderLength`` is a variable' - instantiation_error,
			'``PacketZone`` is a variable' - instantiation_error,
			'``Frame`` is a variable' - instantiation_error,
			'``Profile`` is neither a variable nor a supported link profile term' - domain_error(ccsds_link_profile, 'Profile'),
			'``Profile`` is a telecommand profile and therefore does not support packet insertion' - domain_error(ccsds_packet_link_profile, 'Profile'),
			'``Frame`` is neither a variable nor a valid frame term for the selected profile' - domain_error(ccsds_frame_term, 'Frame')
		]
	]).

	:- public(reassemble_packets/6).
	:- mode(reassemble_packets(+compound, +integer, +compound, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_packets/6, [
		comment is 'Reassembles complete packets from a single TM or AOS transfer frame using the default ``throw`` discontinuity recovery policy.',
		argnames is ['Profile', 'SecondaryHeaderLength', 'Frame', 'State', 'Packets', 'UpdatedState'],
		exceptions is [
			'``Profile``, ``SecondaryHeaderLength``, ``Frame``, or ``State`` is a variable' - instantiation_error,
			'``Profile`` is neither a variable nor a supported link profile term' - domain_error(ccsds_link_profile, 'Profile'),
			'``Profile`` is a telecommand profile and therefore does not support packet reassembly' - domain_error(ccsds_packet_link_profile, 'Profile'),
			'``Frame`` is neither a variable nor a valid TM or AOS frame term for the selected profile' - domain_error(ccsds_frame_term, 'Frame'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``State`` is neither a variable nor a valid channel reassembly state term' - domain_error(ccsds_channel_reassembly_state, 'State'),
			'The delegated packet reassembly detects a discontinuous frame sequence, inconsistent counter modulus, or invalid continuation state' - domain_error(ccsds_packet_reassembly_continuation, packet_reassembly_state('PendingData')),
			'A delegated packet continuation fragment is followed by a new packet before the previous packet is completed' - domain_error(ccsds_packet_reassembly_continuation, incomplete_continuation_before_new_packet)
		]
	]).

	:- public(reassemble_packets/7).
	:- mode(reassemble_packets(+compound, +integer, +compound, +atom, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_packets/7, [
		comment is 'Reassembles complete packets from a single TM or AOS transfer frame using the selected discontinuity recovery policy.',
		argnames is ['Profile', 'SecondaryHeaderLength', 'Frame', 'Policy', 'State', 'Packets', 'UpdatedState'],
		exceptions is [
			'``Profile``, ``SecondaryHeaderLength``, ``Frame``, ``Policy``, or ``State`` is a variable' - instantiation_error,
			'``Profile`` is neither a variable nor a supported link profile term' - domain_error(ccsds_link_profile, 'Profile'),
			'``Profile`` is a telecommand profile and therefore does not support packet reassembly' - domain_error(ccsds_packet_link_profile, 'Profile'),
			'``Frame`` is neither a variable nor a valid TM or AOS frame term for the selected profile' - domain_error(ccsds_frame_term, 'Frame'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``Policy`` is neither a variable nor a valid discontinuity recovery policy' - domain_error(ccsds_discontinuity_policy, 'Policy'),
			'``State`` is neither a variable nor a valid channel reassembly state term' - domain_error(ccsds_channel_reassembly_state, 'State'),
			'The delegated packet reassembly detects a discontinuous frame sequence, inconsistent counter modulus, or invalid continuation state under the ``throw`` policy' - domain_error(ccsds_packet_reassembly_continuation, packet_reassembly_state('PendingData')),
			'A delegated packet continuation fragment is followed by a new packet before the previous packet is completed' - domain_error(ccsds_packet_reassembly_continuation, incomplete_continuation_before_new_packet)
		]
	]).

	:- public(reassemble_packets/8).
	:- mode(reassemble_packets(+compound, +integer, +compound, +atom, +compound, -list(compound), -compound, -list(compound)), one_or_error).
	:- info(reassemble_packets/8, [
		comment is 'Reassembles complete packets from a single TM or AOS transfer frame using the selected discontinuity recovery policy and returns any recovery events.',
		argnames is ['Profile', 'SecondaryHeaderLength', 'Frame', 'Policy', 'State', 'Packets', 'UpdatedState', 'Events'],
		exceptions is [
			'``Profile``, ``SecondaryHeaderLength``, ``Frame``, ``Policy``, or ``State`` is a variable' - instantiation_error,
			'``Profile`` is neither a variable nor a supported link profile term' - domain_error(ccsds_link_profile, 'Profile'),
			'``Profile`` is a telecommand profile and therefore does not support packet reassembly' - domain_error(ccsds_packet_link_profile, 'Profile'),
			'``Frame`` is neither a variable nor a valid TM or AOS frame term for the selected profile' - domain_error(ccsds_frame_term, 'Frame'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``Policy`` is neither a variable nor a valid discontinuity recovery policy' - domain_error(ccsds_discontinuity_policy, 'Policy'),
			'``State`` is neither a variable nor a valid channel reassembly state term' - domain_error(ccsds_channel_reassembly_state, 'State'),
			'The delegated packet reassembly detects a discontinuous frame sequence, inconsistent counter modulus, or invalid continuation state under the ``throw`` policy' - domain_error(ccsds_packet_reassembly_continuation, packet_reassembly_state('PendingData')),
			'A delegated packet continuation fragment is followed by a new packet before the previous packet is completed' - domain_error(ccsds_packet_reassembly_continuation, incomplete_continuation_before_new_packet)
		]
	]).

	:- public(reassemble_frames/6).
	:- mode(reassemble_frames(+compound, +integer, +list(compound), +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_frames/6, [
		comment is 'Reassembles complete packets across a sequence of TM or AOS transfer frames using the default ``throw`` discontinuity recovery policy.',
		argnames is ['Profile', 'SecondaryHeaderLength', 'Frames', 'State', 'Packets', 'UpdatedState'],
		exceptions is [
			'``Profile``, ``SecondaryHeaderLength``, ``Frames``, or ``State`` is a variable' - instantiation_error,
			'``Profile`` is neither a variable nor a supported link profile term' - domain_error(ccsds_link_profile, 'Profile'),
			'``Profile`` is a telecommand profile and therefore does not support packet reassembly' - domain_error(ccsds_packet_link_profile, 'Profile'),
			'``Frames`` is neither a variable nor a list of valid TM or AOS frame terms for the selected profile' - domain_error(ccsds_frame_terms, 'Frames'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``State`` is neither a variable nor a valid channel reassembly state term' - domain_error(ccsds_channel_reassembly_state, 'State'),
			'The delegated packet reassembly detects a discontinuous frame sequence, inconsistent counter modulus, or invalid continuation state' - domain_error(ccsds_packet_reassembly_continuation, packet_reassembly_state('PendingData')),
			'A delegated packet continuation fragment is followed by a new packet before the previous packet is completed' - domain_error(ccsds_packet_reassembly_continuation, incomplete_continuation_before_new_packet)
		]
	]).

	:- public(reassemble_frames/7).
	:- mode(reassemble_frames(+compound, +integer, +list(compound), +atom, +compound, -list(compound), -compound), one_or_error).
	:- info(reassemble_frames/7, [
		comment is 'Reassembles complete packets across a sequence of TM or AOS transfer frames using the selected discontinuity recovery policy.',
		argnames is ['Profile', 'SecondaryHeaderLength', 'Frames', 'Policy', 'State', 'Packets', 'UpdatedState'],
		exceptions is [
			'``Profile``, ``SecondaryHeaderLength``, ``Frames``, ``Policy``, or ``State`` is a variable' - instantiation_error,
			'``Profile`` is neither a variable nor a supported link profile term' - domain_error(ccsds_link_profile, 'Profile'),
			'``Profile`` is a telecommand profile and therefore does not support packet reassembly' - domain_error(ccsds_packet_link_profile, 'Profile'),
			'``Frames`` is neither a variable nor a list of TM or AOS frame terms for the selected profile' - domain_error(ccsds_frame_terms, 'Frames'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``Policy`` is neither a variable nor a valid discontinuity recovery policy' - domain_error(ccsds_discontinuity_policy, 'Policy'),
			'``State`` is neither a variable nor a valid channel reassembly state term' - domain_error(ccsds_channel_reassembly_state, 'State'),
			'The delegated packet reassembly detects a discontinuous frame sequence, inconsistent counter modulus, or invalid continuation state under the ``throw`` policy' - domain_error(ccsds_packet_reassembly_continuation, packet_reassembly_state('PendingData')),
			'A delegated packet continuation fragment is followed by a new packet before the previous packet is completed' - domain_error(ccsds_packet_reassembly_continuation, incomplete_continuation_before_new_packet)
		]
	]).

	:- public(reassemble_frames/8).
	:- mode(reassemble_frames(+compound, +integer, +list(compound), +atom, +compound, -list(compound), -compound, -list(compound)), one_or_error).
	:- info(reassemble_frames/8, [
		comment is 'Reassembles complete packets across a sequence of TM or AOS transfer frames using the selected discontinuity recovery policy and returns any recovery events.',
		argnames is ['Profile', 'SecondaryHeaderLength', 'Frames', 'Policy', 'State', 'Packets', 'UpdatedState', 'Events'],
		exceptions is [
			'``Profile``, ``SecondaryHeaderLength``, ``Frames``, ``Policy``, or ``State`` is a variable' - instantiation_error,
			'``Profile`` is neither a variable nor a supported link profile term' - domain_error(ccsds_link_profile, 'Profile'),
			'``Profile`` is a telecommand profile and therefore does not support packet reassembly' - domain_error(ccsds_packet_link_profile, 'Profile'),
			'``Frames`` is neither a variable nor a list of TM or AOS frame terms for the selected profile' - domain_error(ccsds_frame_terms, 'Frames'),
			'``SecondaryHeaderLength`` is neither a variable nor a valid CCSDS packet secondary header length' - domain_error(ccsds_secondary_header_length, 'SecondaryHeaderLength'),
			'``Policy`` is neither a variable nor a valid discontinuity recovery policy' - domain_error(ccsds_discontinuity_policy, 'Policy'),
			'``State`` is neither a variable nor a valid channel reassembly state term' - domain_error(ccsds_channel_reassembly_state, 'State'),
			'The delegated packet reassembly detects a discontinuous frame sequence, inconsistent counter modulus, or invalid continuation state under the ``throw`` policy' - domain_error(ccsds_packet_reassembly_continuation, packet_reassembly_state('PendingData')),
			'A delegated packet continuation fragment is followed by a new packet before the previous packet is completed' - domain_error(ccsds_packet_reassembly_continuation, incomplete_continuation_before_new_packet)
		]
	]).

	valid_profile(tm_profile(FrameLength, SecondaryHeaderLength, HasFECF)) :-
		valid_frame_length(FrameLength),
		valid_non_negative_length(SecondaryHeaderLength),
		valid_boolean(HasFECF).
	valid_profile(tc_profile(FrameLength, SegmentHeaderLength, HasFECF)) :-
		valid_frame_length(FrameLength),
		valid_non_negative_length(SegmentHeaderLength),
		valid_boolean(HasFECF).
	valid_profile(aos_profile(FrameLength, InsertZoneLength, HasOCF, HasFECF)) :-
		valid_frame_length(FrameLength),
		valid_non_negative_length(InsertZoneLength),
		valid_boolean(HasOCF),
		valid_boolean(HasFECF).

	parse_frame(Source, Profile, Frame) :-
		parse_frames(Source, Profile, Frames),
		(	Frames = [Frame] ->
			true
		;	domain_error(ccsds_single_frame_source, Source)
		).

	parse_frames(Source, Profile, Frames) :-
		(	var(Source) ->
			instantiation_error
		;	var(Profile) ->
			instantiation_error
		;	valid_profile(Profile) ->
			profile_object(Profile, Object),
			Object::parse(Source, Frames)
		;	domain_error(ccsds_link_profile, Profile)
		).

	generate_frame(Sink, Profile, Frame) :-
		(	var(Sink) ->
			instantiation_error
		;	var(Profile) ->
			instantiation_error
		;	var(Frame) ->
			instantiation_error
		;	valid_profile(Profile) ->
			profile_object(Profile, Object),
			(	Object::valid(Frame) ->
				generate_frames(Sink, Profile, [Frame])
			;	domain_error(ccsds_frame_term, Frame)
			)
		;	domain_error(ccsds_link_profile, Profile)
		).

	generate_frames(Sink, Profile, Frames) :-
		(	var(Sink) ->
			instantiation_error
		;	var(Profile) ->
			instantiation_error
		;	var(Frames) ->
			instantiation_error
		;	valid_profile(Profile) ->
			profile_object(Profile, Object),
			(	valid_frames(Object, Frames) ->
				Object::generate(Sink, Frames)
			;	domain_error(ccsds_frame_terms, Frames)
			)
		;	domain_error(ccsds_link_profile, Profile)
		).

	valid_reassembly_state(State) :-
		ccsds_packet_services::valid_channel_reassembly_state(State).

	initial_reassembly_state(State) :-
		ccsds_packet_services::initial_channel_reassembly_state(State).

	pending_fragments(State, PendingFragments) :-
		(	var(State) ->
			instantiation_error
		;	ccsds_packet_services::pending_fragments(State, PendingFragments)
		).

	valid_discontinuity_policy(Policy) :-
		ccsds_packet_services::valid_discontinuity_policy(Policy).

	extract_packets(Profile, SecondaryHeaderLength, Frame, PacketZone) :-
		(	var(Profile) ->
			instantiation_error
		;	var(SecondaryHeaderLength) ->
			instantiation_error
		;	var(Frame) ->
			instantiation_error
		;	packet_profile_object(Profile, Type, _) ->
			(	packet_frame_term(Type, Frame) ->
				extract_profile_packets(Type, Frame, SecondaryHeaderLength, PacketZone)
			;	domain_error(ccsds_frame_term, Frame)
			)
		;	valid_profile(Profile) ->
			domain_error(ccsds_packet_link_profile, Profile)
		;	domain_error(ccsds_link_profile, Profile)
		).

	insert_packets(Profile, SecondaryHeaderLength, PacketZone, Frame, UpdatedFrame) :-
		(	var(Profile) ->
			instantiation_error
		;	var(SecondaryHeaderLength) ->
			instantiation_error
		;	var(PacketZone) ->
			instantiation_error
		;	var(Frame) ->
			instantiation_error
		;	packet_profile_object(Profile, Type, _) ->
			(	packet_frame_term(Type, Frame) ->
				insert_profile_packets(Type, PacketZone, SecondaryHeaderLength, Frame, UpdatedFrame)
			;	domain_error(ccsds_frame_term, Frame)
			)
		;	valid_profile(Profile) ->
			domain_error(ccsds_packet_link_profile, Profile)
		;	domain_error(ccsds_link_profile, Profile)
		).

	reassemble_packets(Profile, SecondaryHeaderLength, Frame, State, Packets, UpdatedState) :-
		reassemble_packets(Profile, SecondaryHeaderLength, Frame, throw, State, Packets, UpdatedState).

	reassemble_packets(Profile, SecondaryHeaderLength, Frame, Policy, State, Packets, UpdatedState) :-
		reassemble_packets(Profile, SecondaryHeaderLength, Frame, Policy, State, Packets, UpdatedState, _).

	reassemble_packets(Profile, SecondaryHeaderLength, Frame, Policy, State, Packets, UpdatedState, Events) :-
		(	var(Profile) ->
			instantiation_error
		;	var(SecondaryHeaderLength) ->
			instantiation_error
		;	var(Frame) ->
			instantiation_error
		;	var(Policy) ->
			instantiation_error
		;	var(State) ->
			instantiation_error
		;	packet_profile_object(Profile, Type, _) ->
			(	packet_frame_term(Type, Frame) ->
				reassemble_profile_packets(Type, Frame, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events)
			;	domain_error(ccsds_frame_term, Frame)
			)
		;	valid_profile(Profile) ->
			domain_error(ccsds_packet_link_profile, Profile)
		;	domain_error(ccsds_link_profile, Profile)
		).

	reassemble_frames(Profile, SecondaryHeaderLength, Frames, State, Packets, UpdatedState) :-
		reassemble_frames(Profile, SecondaryHeaderLength, Frames, throw, State, Packets, UpdatedState).

	reassemble_frames(Profile, SecondaryHeaderLength, Frames, Policy, State, Packets, UpdatedState) :-
		reassemble_frames(Profile, SecondaryHeaderLength, Frames, Policy, State, Packets, UpdatedState, _).

	reassemble_frames(Profile, SecondaryHeaderLength, Frames, Policy, State, Packets, UpdatedState, Events) :-
		(	var(Profile) ->
			instantiation_error
		;	var(SecondaryHeaderLength) ->
			instantiation_error
		;	var(Frames) ->
			instantiation_error
		;	var(Policy) ->
			instantiation_error
		;	var(State) ->
			instantiation_error
		;	packet_profile_object(Profile, Type, _) ->
			(	valid_packet_frames(Type, Frames) ->
				reassemble_profile_frames(Type, Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events)
			;	domain_error(ccsds_frame_terms, Frames)
			)
		;	valid_profile(Profile) ->
			domain_error(ccsds_packet_link_profile, Profile)
		;	domain_error(ccsds_link_profile, Profile)
		).

	profile_object(tm_profile(FrameLength, SecondaryHeaderLength, HasFECF), ccsds_tm_frames(FrameLength, SecondaryHeaderLength, HasFECF)).
	profile_object(tc_profile(FrameLength, SegmentHeaderLength, HasFECF), ccsds_tc_frames(FrameLength, SegmentHeaderLength, HasFECF)).
	profile_object(aos_profile(FrameLength, InsertZoneLength, HasOCF, HasFECF), ccsds_aos_frames(FrameLength, InsertZoneLength, HasOCF, HasFECF)).

	packet_profile_object(tm_profile(FrameLength, SecondaryHeaderLength, HasFECF), tm, ccsds_tm_frames(FrameLength, SecondaryHeaderLength, HasFECF)).
	packet_profile_object(aos_profile(FrameLength, InsertZoneLength, HasOCF, HasFECF), aos, ccsds_aos_frames(FrameLength, InsertZoneLength, HasOCF, HasFECF)).

	extract_profile_packets(tm, Frame, SecondaryHeaderLength, PacketZone) :-
		ccsds_packet_services::extract_tm_packets(Frame, SecondaryHeaderLength, PacketZone).
	extract_profile_packets(aos, Frame, SecondaryHeaderLength, PacketZone) :-
		ccsds_packet_services::extract_aos_packets(Frame, SecondaryHeaderLength, PacketZone).

	insert_profile_packets(tm, PacketZone, SecondaryHeaderLength, Frame, UpdatedFrame) :-
		ccsds_packet_services::insert_tm_packets(PacketZone, SecondaryHeaderLength, Frame, UpdatedFrame).
	insert_profile_packets(aos, PacketZone, SecondaryHeaderLength, Frame, UpdatedFrame) :-
		ccsds_packet_services::insert_aos_packets(PacketZone, SecondaryHeaderLength, Frame, UpdatedFrame).

	reassemble_profile_packets(tm, Frame, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events) :-
		ccsds_packet_services::reassemble_tm_packets(Frame, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events).
	reassemble_profile_packets(aos, Frame, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events) :-
		ccsds_packet_services::reassemble_aos_packets(Frame, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events).

	reassemble_profile_frames(tm, Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events) :-
		ccsds_packet_services::reassemble_tm_frames(Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events).
	reassemble_profile_frames(aos, Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events) :-
		ccsds_packet_services::reassemble_aos_frames(Frames, SecondaryHeaderLength, Policy, State, Packets, UpdatedState, Events).

	valid_frames(_, []).
	valid_frames(Object, [Frame| Frames]) :-
		Object::valid(Frame),
		valid_frames(Object, Frames).

	packet_frame_term(tm, tm_transfer_frame(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)).
	packet_frame_term(aos, aos_transfer_frame(_, _, _, _, _, _, _, _, _)).

	valid_packet_frames(_, []).
	valid_packet_frames(Type, [Frame| Frames]) :-
		packet_frame_term(Type, Frame),
		valid_packet_frames(Type, Frames).

	valid_frame_length(FrameLength) :-
		integer(FrameLength),
		FrameLength > 0.

	valid_non_negative_length(Length) :-
		integer(Length),
		Length >= 0.

	valid_boolean(true).
	valid_boolean(false).

:- end_object.
