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
		date is 2021-12-27,
		comment is 'Unit tests for the ISO Prolog standard put_code/1-2 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.12.3.4

	test(iso_put_code_2_01, true(Assertion)) :-
		^^set_text_output('qwer'),
		{put_code(0't)},
		^^text_output_assertion('qwert', Assertion).

	test(iso_put_code_2_02, true(Assertion)) :-
		^^set_text_output(st_o, 'qwer'),
		{put_code(st_o, 0't)},
		^^text_output_assertion(st_o, 'qwert', Assertion).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(iso_put_code_2_03, error(instantiation_error)) :-
		^^set_text_output(my_file, ''),
		{put_code(my_file, _C)},
		^^check_text_output(my_file, '').

	test(iso_put_code_2_04, error(type_error(integer,ty))) :-
		^^set_text_output(st_o, ''),
		{put_code(st_o, 'ty')},
		^^check_text_output(st_o, '').

	test(sics_put_code_2_05, error(instantiation_error)) :-
		{put_code(_, 0't)}.

	test(sics_put_code_2_06, error(instantiation_error)) :-
		{put_code(_)}.

	test(iso_put_code_2_07, error(existence_error(stream,S))) :-
		^^closed_input_stream(S, []),
		{put_code(S, 0'a)}.

	test(iso_put_code_2_08, error(existence_error(stream,S))) :-
		^^closed_output_stream(S, []),
		{put_code(S, 0'a)}.

	test(iso_put_code_2_09, error(permission_error(output,stream,S))) :-
		current_input(S),
		{put_code(S, 0'a)}.

	test(iso_put_code_2_10, error(permission_error(output,binary_stream,S))) :-
		os::absolute_file_name(t, Path),
		open(Path, write, S, [type(binary)]),
		{put_code(S, 0'a)}.

	test(sics_put_code_2_11, error(representation_error(character_code))) :-
		{put_code(-1)}.

	test(sics_put_code_2_12, errors([domain_error(stream_or_alias,foo), existence_error(stream,foo)])) :-
		% both exception terms seem to be acceptable in the ISO spec
		{put_code(foo, 1)}.

	% tests from the Logtalk portability work

	test(lgt_put_code_2_13, error(permission_error(output,stream,s))) :-
		^^set_text_input(s, ''),
		{put_code(s, 1)}.

	test(lgt_put_code_2_14, error(permission_error(output,binary_stream,_))) :-
		^^set_binary_output(s, []),
		{put_code(s, 1)}.

	test(lgt_put_code_2_15, error(type_error(integer,65.0))) :-
		^^set_text_output(''),
		{put_code(65.0)},
		^^check_text_output('').

	test(lgt_put_code_2_16, error(type_error(integer,65.0))) :-
		^^set_text_output(st_o, ''),
		{put_code(st_o, 65.0)},
		^^check_text_output(st_o, '').

	cleanup :-
		^^clean_file(t),
		^^clean_text_output,
		^^clean_text_input,
		^^clean_binary_output.

:- end_object.
