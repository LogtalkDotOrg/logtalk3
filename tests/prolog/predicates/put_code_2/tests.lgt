%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.3,
		author is 'Paulo Moura',
		date is 2017/08/24,
		comment is 'Unit tests for the ISO Prolog standard put_code/1-2 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.12.3.4

	succeeds(iso_put_code_2_01) :-
		^^set_text_output('qwer'),
		{put_code(0't)},
		^^check_text_output('qwert').

	succeeds(iso_put_code_2_02) :-
		^^set_text_output(st_o, 'qwer'),
		{put_code(st_o, 0't)},
		^^check_text_output(st_o, 'qwert').

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(iso_put_code_2_03, error(instantiation_error,_)) :-
		^^set_text_output(my_file, ''),
		{put_code(my_file, _C)},
		^^check_text_output(my_file, '').

	throws(iso_put_code_2_04, error(type_error(integer,ty),_)) :-
		^^set_text_output(st_o, ''),
		{put_code(st_o, 'ty')},
		^^check_text_output(st_o, '').

	throws(sics_put_code_2_05, error(instantiation_error,_)) :-
		{put_code(_, 0't)}.

	throws(sics_put_code_2_06, error(instantiation_error,_)) :-
		{put_code(_)}.
		
	throws(iso_put_code_2_07, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{put_code(S, 0'a)}.
		
	throws(iso_put_code_2_08, error(permission_error(output,stream,S),_)) :-
		current_input(S),
		{put_code(S, 0'a)}.

	throws(iso_put_code_2_09, error(permission_error(output,binary_stream,S),_)) :-
		os::absolute_file_name(t, Path),
		open(Path, write, S, [type(binary)]),
		{put_code(S, 0'a)}.

	throws(sics_put_code_2_10, error(representation_error(character_code),_)) :-
		{put_code(-1)}.

	throws(sics_put_code_2_11, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		{put_code(foo, 1)}.

	% tests from the Logtalk portability work

	throws(lgt_put_code_2_12, error(permission_error(output,stream,s),_)) :-
		^^set_text_input(s, ''),
		{put_code(s, 1)}.

	throws(lgt_put_code_2_13, error(permission_error(output,binary_stream,_),_)) :-
		^^set_binary_output(s, []),
		{put_code(s, 1)}.

	cleanup :-
		^^clean_file(t),
		^^clean_text_output,
		^^clean_text_input,
		^^clean_binary_output.

:- end_object.
