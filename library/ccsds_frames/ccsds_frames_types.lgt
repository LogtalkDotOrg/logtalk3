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


:- category(ccsds_frames_types).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-08,
		comment is 'Type definitions for CCSDS transfer frames.'
	]).

	:- multifile(type::type/1).
	type::type(ccsds_tm_frame(_, _, _)).
	type::type(ccsds_tm_frame_term(_, _, _)).
	type::type(ccsds_tc_frame(_, _, _)).
	type::type(ccsds_tc_frame_term(_, _, _)).
	type::type(ccsds_aos_frame(_, _, _, _)).
	type::type(ccsds_aos_frame_term(_, _, _, _)).

	:- multifile(type::check/2).
	type::check(ccsds_tm_frame(FrameLength, SecondaryHeaderLength, HasFECF), Term) :-
		(   valid_ccsds_tm_frame_bytes(Term, FrameLength, SecondaryHeaderLength, HasFECF) ->
			true
		;   var(Term) ->
			throw(instantiation_error)
		;   throw(type_error(ccsds_tm_frame(FrameLength, SecondaryHeaderLength, HasFECF), Term))
		).

	type::check(ccsds_tm_frame_term(FrameLength, SecondaryHeaderLength, HasFECF), Term) :-
		(   ccsds_tm_frames(FrameLength, SecondaryHeaderLength, HasFECF)::valid(Term) ->
			true
		;   var(Term) ->
			throw(instantiation_error)
		;   throw(type_error(ccsds_tm_frame_term(FrameLength, SecondaryHeaderLength, HasFECF), Term))
		).

	type::check(ccsds_tc_frame(FrameLength, SegmentHeaderLength, HasFECF), Term) :-
		(   valid_ccsds_tc_frame_bytes(Term, FrameLength, SegmentHeaderLength, HasFECF) ->
			true
		;   var(Term) ->
			throw(instantiation_error)
		;   throw(type_error(ccsds_tc_frame(FrameLength, SegmentHeaderLength, HasFECF), Term))
		).

	type::check(ccsds_tc_frame_term(FrameLength, SegmentHeaderLength, HasFECF), Term) :-
		(   ccsds_tc_frames(FrameLength, SegmentHeaderLength, HasFECF)::valid(Term) ->
			true
		;   var(Term) ->
			throw(instantiation_error)
		;   throw(type_error(ccsds_tc_frame_term(FrameLength, SegmentHeaderLength, HasFECF), Term))
		).

	type::check(ccsds_aos_frame(FrameLength, InsertZoneLength, HasOCF, HasFECF), Term) :-
		(   valid_ccsds_aos_frame_bytes(Term, FrameLength, InsertZoneLength, HasOCF, HasFECF) ->
			true
		;   var(Term) ->
			throw(instantiation_error)
		;   throw(type_error(ccsds_aos_frame(FrameLength, InsertZoneLength, HasOCF, HasFECF), Term))
		).

	type::check(ccsds_aos_frame_term(FrameLength, InsertZoneLength, HasOCF, HasFECF), Term) :-
		(   ccsds_aos_frames(FrameLength, InsertZoneLength, HasOCF, HasFECF)::valid(Term) ->
			true
		;   var(Term) ->
			throw(instantiation_error)
		;   throw(type_error(ccsds_aos_frame_term(FrameLength, InsertZoneLength, HasOCF, HasFECF), Term))
		).

	valid_ccsds_tm_frame_bytes(Bytes, FrameLength, SecondaryHeaderLength, HasFECF) :-
		type::valid(list(byte, FrameLength), Bytes),
		ccsds_tm_frames(FrameLength, SecondaryHeaderLength, HasFECF)::parse(bytes(Bytes), [_]).

	valid_ccsds_tc_frame_bytes(Bytes, FrameLength, SegmentHeaderLength, HasFECF) :-
		type::valid(list(byte, FrameLength), Bytes),
		ccsds_tc_frames(FrameLength, SegmentHeaderLength, HasFECF)::parse(bytes(Bytes), [_]).

	valid_ccsds_aos_frame_bytes(Bytes, FrameLength, InsertZoneLength, HasOCF, HasFECF) :-
		type::valid(list(byte, FrameLength), Bytes),
		ccsds_aos_frames(FrameLength, InsertZoneLength, HasOCF, HasFECF)::parse(bytes(Bytes), [_]).

:- end_category.
