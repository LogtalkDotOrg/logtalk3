%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2020-11-06,
		comment is 'Unit tests for the ISO Prolog standard put_byte/1-2 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.13.3.4

	test(iso_put_byte_2_01, true(Assertion)) :-
		^^set_binary_output([113,119,101,114]),
		{put_byte(116)},
		^^binary_output_assertion([113,119,101,114,116], Assertion).

	test(iso_put_byte_2_02, true(Assertion)) :-
		^^set_binary_output(st_o, [113,119,101,114]),
		{put_byte(st_o, 116)},
		^^binary_output_assertion(st_o, [113,119,101,114,116], Assertion).

	test(iso_put_byte_2_03, error(instantiation_error)) :-
		^^set_binary_output(my_file_1, []),
		{put_byte(my_file_1, _C)}.

	test(iso_put_byte_2_04, error(type_error(byte, ty))) :-
		^^set_binary_output(my_file_2, []),
		{put_byte(my_file_2, 'ty')}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_put_byte_2_05, error(instantiation_error)) :-
		{put_byte(_S, 118)}.

	test(sics_put_byte_2_06, error(instantiation_error)) :-
		^^set_binary_output([]),
		{put_byte(_C)}.

	test(sics_put_byte_2_07, error(existence_error(stream,S))) :-
		^^closed_input_stream(S, [type(binary)]),
		{put_byte(S, 77)}.

	test(sics_put_byte_2_08, error(existence_error(stream,S))) :-
		^^closed_output_stream(S, [type(binary)]),
		{put_byte(S, 77)}.

	test(sics_put_byte_2_09, error(permission_error(output,stream,S))) :-
		^^set_binary_input([]),
		current_input(S),
		{put_byte(S, 99)}.

	test(sics_put_byte_2_10, error(permission_error(output,text_stream,S))) :-
		current_output(S),
		{put_byte(99)}.

	test(sics_put_byte_2_11, error(type_error(byte,-1))) :-
		^^set_binary_output([]),
		{put_byte(-1)}.

	test(sics_put_byte_2_12, error(instantiation_error)) :-
		{put_byte(_S, 1)}.

	test(sics_put_byte_2_13, errors([domain_error(stream_or_alias,foo), existence_error(stream,foo)])) :-
		% both exception terms seem to be acceptable in the ISO spec
		{put_byte(foo, 1)}.

	% tests from the Logtalk portability work

	test(lgt_put_byte_2_14, error(permission_error(output,stream,s))) :-
		^^set_binary_input(s, []),
		{put_byte(s, 99)}.

	test(lgt_put_byte_2_15, error(permission_error(output,text_stream,_))) :-
		^^set_text_output(s, ''),
		{put_byte(s, 99)}.

	test(lgt_put_byte_2_16, true(Assertion)) :-
		^^set_binary_output([252,253,254]),
		{put_byte(255)},
		^^binary_output_assertion([252,253,254,255], Assertion).

	test(lgt_put_byte_2_17, true(Assertion)) :-
		^^set_binary_output(st_o, [252,253,254]),
		{put_byte(st_o, 255)},
		^^binary_output_assertion(st_o, [252,253,254,255], Assertion).

	test(lgt_put_byte_2_18, true(Assertion)) :-
		^^set_binary_output([3,2,1]),
		{put_byte(0)},
		^^binary_output_assertion([3,2,1,0], Assertion).

	test(lgt_put_byte_2_19, true(Assertion)) :-
		^^set_binary_output(st_o, [3,2,1]),
		{put_byte(st_o, 0)},
		^^binary_output_assertion(st_o, [3,2,1,0], Assertion).

	cleanup :-
		^^clean_binary_input,
		^^clean_binary_output,
		^^clean_text_output.

:- end_object.
