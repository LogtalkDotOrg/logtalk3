%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:8:0,
		author is 'Paulo Moura',
		date is 2021-12-27,
		comment is 'Unit tests for the ISO Prolog standard get_byte/1-2 built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.13.1.4

	succeeds(iso_get_byte_2_01) :-
		^^set_binary_input([113,119,101,114]),
		{get_byte(Byte)},
		Byte == 113,
		^^check_binary_input([119,101,114]).

	succeeds(iso_get_byte_2_02) :-
		^^set_binary_input(st_i, [113,119,101,114]),
		{get_byte(st_i, Byte)},
		Byte == 113,
		^^check_binary_input(st_i, [119,101,114]).

	succeeds(iso_get_byte_2_03) :-
		^^set_binary_input(st_i, [113,119,101,114,116,121]),
		\+ {get_byte(st_i, 114)},
		^^check_binary_input(st_i, [119,101,114,116,121]).

	succeeds(iso_get_byte_2_04) :-
		^^set_binary_input(st_i, []),
		{get_byte(st_i, Byte)},
		Byte == -1.

	throws(iso_get_byte_2_05, error(permission_error(input,stream,user_output),_)) :-
		{get_byte(user_output, _Byte)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_get_byte_2_06, error(instantiation_error,_)) :-
		{get_byte(_, _)}.

	throws(sics_get_byte_2_07, error(type_error(in_byte,p),_)) :-
		^^set_binary_input([]),
		{get_byte(p)}.

	throws(sics_get_byte_2_08, error(type_error(in_byte,-2),_)) :-
		^^set_binary_input([]),
		{get_byte(-2)}.

	throws(sics_get_byte_2_09, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		^^set_binary_input([]),
		{get_byte(foo,_)}.

	throws(sics_get_byte_2_10, error(existence_error(stream,S),_)) :-
		^^closed_input_stream(S, [type(binary)]),
		{get_byte(S,_)}.

	throws(sics_get_byte_2_11, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, [type(binary)]),
		{get_byte(S,_)}.

	throws(sics_get_byte_2_12, error(permission_error(input,stream,S),_)) :-
		current_output(S),
		{get_byte(S,_)}.

	throws(sics_get_byte_2_13, error(permission_error(input,text_stream,S),_)) :-
		^^set_text_input(''),
		current_input(S),
		{get_byte(_)}.

	succeeds(sics_get_byte_2_14) :-
		^^set_binary_input(st_i, [], [eof_action(error)]),
		catch({get_byte(st_i,_), get_byte(st_i,_)}, error(permission_error(input,past_end_of_stream,_),_), true),
		stream_property(S, alias(st_i)),
		stream_property(S, end_of_stream(past)).

	% tests from the Logtalk portability work

	succeeds(lgt_get_byte_2_15) :-
		^^set_binary_input(st_i, [], [eof_action(eof_code)]),
		{get_byte(st_i, Byte1), get_byte(st_i, Byte1), get_byte(st_i, Byte2)},
		Byte1 == -1, Byte2 == -1.

	throws(lgt_get_byte_2_16, error(permission_error(input,stream,s),_)) :-
		^^set_binary_output(s, []),
		{get_byte(s,_)}.

	throws(lgt_get_byte_2_17, error(permission_error(input,text_stream,_),_)) :-
		^^set_text_input(s, ''),
		{get_byte(s,_)}.

	succeeds(lgt_get_byte_2_18) :-
		^^set_binary_input(st_i, []),
		{get_byte(st_i, -1)}.

	succeeds(lgt_get_byte_2_19) :-
		^^set_binary_input([255,255,255]),
		{get_byte(Byte)},
		Byte == 255.

	succeeds(lgt_get_byte_2_20) :-
		^^set_binary_input(st_i, [255,255,255]),
		{get_byte(st_i, Byte)},
		Byte == 255.

	succeeds(lgt_get_byte_2_21) :-
		^^set_binary_input([255,255,255]),
		{get_byte(255)}.

	succeeds(lgt_get_byte_2_22) :-
		^^set_binary_input(st_i, [255,255,255]),
		{get_byte(st_i, 255)}.

	succeeds(lgt_get_byte_2_23) :-
		^^set_binary_input([0,0,0]),
		{get_byte(Byte)},
		Byte == 0.

	succeeds(lgt_get_byte_2_24) :-
		^^set_binary_input(st_i, [0,0,0]),
		{get_byte(st_i, Byte)},
		Byte == 0.

	succeeds(lgt_get_byte_2_25) :-
		^^set_binary_input([0,0,0]),
		{get_byte(0)}.

	succeeds(lgt_get_byte_2_26) :-
		^^set_binary_input(st_i, [0,0,0]),
		{get_byte(st_i, 0)}.

	throws(lgt_get_byte_2_27, error(type_error(in_byte,256),_)) :-
		^^set_binary_input([]),
		{get_byte(256)}.

	cleanup :-
		^^clean_text_input,
		^^clean_binary_input,
		^^clean_binary_output.

:- end_object.
