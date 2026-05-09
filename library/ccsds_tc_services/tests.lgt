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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-05-08,
		comment is 'Unit tests for the "ccsds_tc_services" library.'
	]).

	:- uses(ccsds_tc_services, [
		valid_segment/1, sequence_flags/2, map_id/2, segment_header_suffix/2, segment_data/2,
		valid_reassembled_segment/1, segment_header_suffixes/2,
		valid_map_dispatch/1, dispatch_map_id/2, dispatch_service_units/2, dispatch_service_units_by_map/2, dispatch_service_units_by_map/3,
		valid_reassembly_state/1, initial_reassembly_state/1, pending_fragments/2,
		valid_discontinuity_policy/1,
		extract_tc_segment/2, insert_tc_segment/3,
		reassemble_tc_frame/4, reassemble_tc_frame/5, reassemble_tc_frame/6,
		reassemble_tc_frames/4, reassemble_tc_frames/5, reassemble_tc_frames/6,
		reassemble_tc_frame_with_provenance/5, reassemble_tc_frame_with_provenance/6,
		reassemble_tc_frames_with_provenance/5, reassemble_tc_frames_with_provenance/6
	]).

	cover(ccsds_tc_services).

	test(ccsds_tc_services_valid_segment_1_01, deterministic) :-
		valid_segment(tc_segment(unsegmented, 42, [0xAA, 0xBB])).

	test(ccsds_tc_services_valid_segment_1_02, deterministic) :-
		valid_segment(tc_segment(unsegmented, 42, [0xAA, 0xBB], [0xCC])).

	test(ccsds_tc_services_sequence_flags_2_01, deterministic(SequenceFlags == first)) :-
		sequence_flags(tc_segment(first, 7, [0x01]), SequenceFlags).

	test(ccsds_tc_services_map_id_2_01, deterministic(MapId == 7)) :-
		map_id(tc_segment(first, 7, [0x01]), MapId).

	test(ccsds_tc_services_map_id_2_02, deterministic(MapId == 7)) :-
		map_id(tc_reassembled_segment(7, [[0xAA], [0xBB]], [0x01, 0x02]), MapId).

	test(ccsds_tc_services_segment_header_suffix_2_01, deterministic(HeaderSuffix == [0xAA, 0xBB])) :-
		segment_header_suffix(tc_segment(first, 7, [0xAA, 0xBB], [0x01]), HeaderSuffix).

	test(ccsds_tc_services_segment_header_suffix_2_02, deterministic(HeaderSuffix == [])) :-
		segment_header_suffix(tc_segment(first, 7, [0x01]), HeaderSuffix).

	test(ccsds_tc_services_valid_reassembled_segment_1_01, deterministic) :-
		valid_reassembled_segment(tc_reassembled_segment(7, [[0xAA], [0xBB]], [0x01, 0x02])).

	test(ccsds_tc_services_segment_header_suffixes_2_01, deterministic(HeaderSuffixes == [[0xAA], [0xBB]])) :-
		segment_header_suffixes(tc_reassembled_segment(7, [[0xAA], [0xBB]], [0x01, 0x02]), HeaderSuffixes).

	test(ccsds_tc_services_valid_map_dispatch_1_01, deterministic) :-
		valid_map_dispatch(map_dispatch(7, [tc_segment(unsegmented, 7, [], [0x01]), tc_reassembled_segment(7, [[0xAA]], [0x02])])).

	test(ccsds_tc_services_dispatch_map_id_2_01, deterministic(MapId == 7)) :-
		dispatch_map_id(map_dispatch(7, [tc_segment(unsegmented, 7, [], [0x01])]), MapId).

	test(ccsds_tc_services_dispatch_service_units_2_01, deterministic(ServiceUnits == [tc_segment(unsegmented, 7, [], [0x01]), tc_reassembled_segment(7, [[0xAA]], [0x02])])) :-
		dispatch_service_units(map_dispatch(7, [tc_segment(unsegmented, 7, [], [0x01]), tc_reassembled_segment(7, [[0xAA]], [0x02])]), ServiceUnits).

	test(ccsds_tc_services_dispatch_service_units_by_map_2_01, deterministic(Dispatches == [map_dispatch(7, [tc_segment(unsegmented, 7, [], [0x01]), tc_reassembled_segment(7, [[0xAA]], [0x02])]), map_dispatch(6, [tc_reassembled_segment(6, [[0xBB]], [0x03])])])) :-
		dispatch_service_units_by_map([
			tc_segment(unsegmented, 7, [], [0x01]),
			tc_reassembled_segment(6, [[0xBB]], [0x03]),
			tc_reassembled_segment(7, [[0xAA]], [0x02])
		], Dispatches).

	test(ccsds_tc_services_dispatch_service_units_by_map_3_01, deterministic(DispatchedServiceUnits == [tc_reassembled_segment(7, [[0xAA]], [0x02]), tc_segment(unsegmented, 7, [], [0x04])])) :-
		dispatch_service_units_by_map([
			tc_reassembled_segment(6, [[0xBB]], [0x03]),
			tc_reassembled_segment(7, [[0xAA]], [0x02]),
			tc_segment(unsegmented, 7, [], [0x04])
		], 7, DispatchedServiceUnits).

	test(ccsds_tc_services_segment_data_2_01, deterministic(Data == [0x01])) :-
		segment_data(tc_segment(first, 7, [0x01]), Data).

	test(ccsds_tc_services_valid_reassembly_state_1_01, deterministic) :-
		valid_reassembly_state(tc_reassembly_state([tc_reassembly_channel(42, 3, 8, [tc_pending_segment(5, [0xAA])])])).

	test(ccsds_tc_services_valid_reassembly_state_1_02, deterministic) :-
		valid_reassembly_state(tc_reassembly_state([tc_reassembly_channel(42, 3, 8, [tc_pending_segment(5, [0xAA], [[0x11], [0x22]])])])).

	test(ccsds_tc_services_initial_reassembly_state_1_01, deterministic(State == tc_reassembly_state([]))) :-
		initial_reassembly_state(State).

	test(ccsds_tc_services_pending_fragments_2_01, deterministic(Pending == [pending_fragment(42, 3, 5, [0xAA]), pending_fragment(42, 3, 6, [0xBB])])) :-
		pending_fragments(tc_reassembly_state([tc_reassembly_channel(42, 3, 8, [tc_pending_segment(5, [0xAA]), tc_pending_segment(6, [0xBB])])]), Pending).

	test(ccsds_tc_services_valid_discontinuity_policy_1_01, deterministic) :-
		valid_discontinuity_policy(throw),
		valid_discontinuity_policy(drop),
		valid_discontinuity_policy(resynchronize).

	test(ccsds_tc_services_extract_tc_segment_2_01, deterministic(Segment == tc_segment(unsegmented, 63, [], [1, 2, 3]))) :-
		extract_tc_segment(tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0xFF]), [1, 2, 3], none), Segment).

	test(ccsds_tc_services_extract_tc_segment_2_02, deterministic(Segment == tc_segment(first, 5, [0xAA, 0xBB], [1, 2, 3]))) :-
		extract_tc_segment(tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x45, 0xAA, 0xBB]), [1, 2, 3], none), Segment).

	test(ccsds_tc_services_insert_tc_segment_3_01, deterministic(UpdatedFrame == tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x6A]), [1, 2, 3], none))) :-
		insert_tc_segment(tc_segment(first, 42, [1, 2, 3]), tc_transfer_frame(0, 1, 0, 42, 3, 7, none, [], none), UpdatedFrame).

	test(ccsds_tc_services_insert_tc_segment_3_02, deterministic(UpdatedFrame == tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x6A, 0xAA, 0xBB]), [1, 2, 3], none))) :-
		insert_tc_segment(tc_segment(first, 42, [1, 2, 3]), tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x00, 0xAA, 0xBB]), [], none), UpdatedFrame).

	test(ccsds_tc_services_insert_tc_segment_3_03, deterministic(UpdatedFrame == tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x6A, 0x11, 0x22]), [1, 2, 3], none))) :-
		insert_tc_segment(tc_segment(first, 42, [0x11, 0x22], [1, 2, 3]), tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x00, 0xAA, 0xBB]), [], none), UpdatedFrame).

	test(ccsds_tc_services_reassemble_tc_frame_4_01, deterministic((Segments == [tc_segment(unsegmented, 5, [], [1, 2, 3])], UpdatedState == tc_reassembly_state([tc_reassembly_channel(42, 3, 8, [])])))) :-
		initial_reassembly_state(State),
		reassemble_tc_frame(tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0xC5]), [1, 2, 3], none), State, Segments, UpdatedState).

	test(ccsds_tc_services_reassemble_tc_frame_4_02, deterministic((Segments == [tc_segment(unsegmented, 5, [0xAA, 0xBB], [1, 2, 3])], UpdatedState == tc_reassembly_state([tc_reassembly_channel(42, 3, 8, [])])))) :-
		initial_reassembly_state(State),
		reassemble_tc_frame(tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0xC5, 0xAA, 0xBB]), [1, 2, 3], none), State, Segments, UpdatedState).

	test(ccsds_tc_services_reassemble_tc_frames_4_01, deterministic((Segments == [tc_segment(unsegmented, 5, [], [1, 2, 3, 4])], Pending == []))) :-
		initial_reassembly_state(State),
		reassemble_tc_frames([
			tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x45]), [1, 2], none),
			tc_transfer_frame(0, 1, 0, 42, 3, 8, segment_header([0x85]), [3, 4], none)
		], State, Segments, UpdatedState),
		pending_fragments(UpdatedState, Pending).

	test(ccsds_tc_services_reassemble_tc_frames_4_02, deterministic((Segments == [tc_segment(unsegmented, 6, [], [9]), tc_segment(unsegmented, 5, [], [1, 2])], Pending == []))) :-
		initial_reassembly_state(State),
		reassemble_tc_frames([
			tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x45]), [1], none),
			tc_transfer_frame(0, 1, 0, 42, 3, 8, segment_header([0xC6]), [9], none),
			tc_transfer_frame(0, 1, 0, 42, 3, 9, segment_header([0x85]), [2], none)
		], State, Segments, UpdatedState),
		pending_fragments(UpdatedState, Pending).

	test(ccsds_tc_services_reassemble_tc_frame_with_provenance_5_01, deterministic((ReassembledSegments == [tc_reassembled_segment(5, [[0xAA, 0xBB]], [1, 2, 3])], UpdatedState == tc_reassembly_state([tc_reassembly_channel(42, 3, 8, [])]), Events == []))) :-
		initial_reassembly_state(State),
		reassemble_tc_frame_with_provenance(tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0xC5, 0xAA, 0xBB]), [1, 2, 3], none), State, ReassembledSegments, UpdatedState, Events).

	test(ccsds_tc_services_reassemble_tc_frames_with_provenance_5_01, deterministic((ReassembledSegments == [tc_reassembled_segment(5, [[0xAA], [0xBB]], [1, 2, 3, 4])], Pending == [], Events == []))) :-
		initial_reassembly_state(State),
		reassemble_tc_frames_with_provenance([
			tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x45, 0xAA]), [1, 2], none),
			tc_transfer_frame(0, 1, 0, 42, 3, 8, segment_header([0x85, 0xBB]), [3, 4], none)
		], State, ReassembledSegments, UpdatedState, Events),
		pending_fragments(UpdatedState, Pending).

	test(ccsds_tc_services_reassemble_tc_frame_with_provenance_6_01, deterministic((ReassembledSegments == [tc_reassembled_segment(6, [[0x33]], [9])], UpdatedState == tc_reassembly_state([tc_reassembly_channel(42, 3, 10, [])]), Events == [dropped_fragment(42, 3, 5, discontinuity(8, 9), [1]), resynchronized(42, 3, discontinuity(8, 9))]))) :-
		initial_reassembly_state(State0),
		reassemble_tc_frame_with_provenance(tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x45, 0x11]), [1], none), State0, [], State1, []),
		reassemble_tc_frame_with_provenance(tc_transfer_frame(0, 1, 0, 42, 3, 9, segment_header([0xC6, 0x33]), [9], none), resynchronize, State1, ReassembledSegments, UpdatedState, Events).

	test(ccsds_tc_services_reassemble_tc_frame_5_01, error(domain_error(ccsds_tc_transfer_frame_sequence, _))) :-
		initial_reassembly_state(State0),
		reassemble_tc_frame(tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x45]), [1], none), State0, [], State1),
		reassemble_tc_frame(tc_transfer_frame(0, 1, 0, 42, 3, 9, segment_header([0x85]), [2], none), throw, State1, _, _).

	test(ccsds_tc_services_reassemble_tc_frame_6_01, deterministic((Segments == [], UpdatedState == tc_reassembly_state([tc_reassembly_channel(42, 3, 10, [])]), Events == [dropped_fragment(42, 3, 5, discontinuity(8, 9), [1]), skipped_frame(42, 3, discontinuity(8, 9))]))) :-
		initial_reassembly_state(State0),
		reassemble_tc_frame(tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x45]), [1], none), State0, [], State1),
		reassemble_tc_frame(tc_transfer_frame(0, 1, 0, 42, 3, 9, segment_header([0x85]), [2], none), drop, State1, Segments, UpdatedState, Events).

	test(ccsds_tc_services_reassemble_tc_frame_6_02, deterministic((Segments == [tc_segment(unsegmented, 6, [], [9])], UpdatedState == tc_reassembly_state([tc_reassembly_channel(42, 3, 10, [])]), Events == [dropped_fragment(42, 3, 5, discontinuity(8, 9), [1]), resynchronized(42, 3, discontinuity(8, 9))]))) :-
		initial_reassembly_state(State0),
		reassemble_tc_frame(tc_transfer_frame(0, 1, 0, 42, 3, 7, segment_header([0x45]), [1], none), State0, [], State1),
		reassemble_tc_frame(tc_transfer_frame(0, 1, 0, 42, 3, 9, segment_header([0xC6]), [9], none), resynchronize, State1, Segments, UpdatedState, Events).

:- end_object.
