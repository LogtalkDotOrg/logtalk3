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
		version is 1.6,
		author is 'Paulo Moura',
		date is 2017/08/24,
		comment is 'Unit tests for the ISO Prolog standard get_code/1-2 built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.12.1.4

	succeeds(iso_get_code_2_01) :-
		^^set_text_input('qwerty'),
		{get_code(Char)},
		Char == 0'q,
		^^check_text_input('werty').

	succeeds(iso_get_code_2_02) :-
		^^set_text_input(st_i, 'qwerty'),
		{get_code(st_i, Char)},
		Char == 0'q,
		^^check_text_input(st_i, 'werty').

	succeeds(iso_get_code_2_03) :-
		^^set_text_input(st_i, '\'qwerty\''),
		{get_code(st_i, Char)},
		Char == 39,		% 0'\'
		^^check_text_input(st_i, 'qwerty\'').

	succeeds(iso_get_code_2_04) :-
		^^set_text_input(st_i, 'qwerty'),
		\+ {get_code(st_i, 0'p)},
		^^check_text_input(st_i, 'werty').

	succeeds(iso_get_code_2_05) :-
		^^set_text_input(st_i, ''),
		{get_code(st_i, Char)},
		Char == -1,
		stream_property(Stream, alias(st_i)),
		stream_property(Stream, end_of_stream(past)).

	throws(iso_get_code_2_06, error(permission_error(input,stream,user_output),_)) :-
		{get_code(user_output, _)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_get_code_2_07, error(instantiation_error,_)) :-
		{get_code(_, _)}.

	throws(sics_get_code_2_08, error(type_error(integer,p),_)) :-
		^^set_text_input('foo'),
		{get_code(p)}.

	throws(sics_get_code_2_09, error(type_error(integer,p),_)) :-
		^^set_text_input(st_i, 'foo'),
		{get_code(st_i, p)}.

	throws(sics_get_code_2_10, error(representation_error(in_character_code),_)) :-
		^^set_text_input('foo'),
		{get_code(-2)}.

	throws(sics_get_code_2_11, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		{get_code(foo,_)}.

	throws(sics_get_code_2_12, error(existence_error(stream,S),_)) :-
		^^closed_input_stream(S, []),
		{get_code(S,_)}.

	throws(sics_get_code_2_13, error(permission_error(input,stream,S),_)) :-
		current_output(S),
		{get_code(S,_)}.

	throws(sics_get_code_2_14, error(permission_error(input,binary_stream,S),_)) :-
		^^set_binary_input([]),
		current_input(S),
		{get_code(_)}.

	succeeds(sics_get_code_2_15) :-
		^^set_text_input(st_i, '', [eof_action(error)]),
		catch({get_code(st_i,_), get_code(st_i,_)}, error(permission_error(input,past_end_of_stream,_),_), true),
		stream_property(S, alias(st_i)),
		stream_property(S, end_of_stream(past)).

	succeeds(sics_get_code_2_16) :-
		os::absolute_file_name(t, Path),
		^^create_text_file(Path, ''),
		open(Path, read, S, [eof_action(eof_code)]),
		{get_code(S, C1), get_code(S, C2)},
		C1 == -1, C2 == -1,
		stream_property(S, end_of_stream(past)).

	succeeds(sics_get_code_2_17) :-
		os::absolute_file_name(t, Path),
		^^create_binary_file(Path, [0]),
		open(Path, read, S),
		catch({get_code(S, _)}, Error, Error = error(representation_error(character),_)).

	% tests from the Logtalk portability work

	succeeds(lgt_get_code_2_18) :-
		^^set_text_input(st_i, '', [eof_action(eof_code)]),
		{get_code(st_i,_), get_code(st_i,Code)},
		Code == -1.

	throws(lgt_get_code_2_19, error(permission_error(input,stream,s),_)) :-
		^^set_text_output(s, ''),
		{get_code(s, _)}.

	throws(lgt_get_code_2_20, error(permission_error(input,binary_stream,_),_)) :-
		^^set_binary_input(s, []),
		{get_code(s, _)}.

	cleanup :-
		^^clean_file(t),
		^^clean_text_input,
		^^clean_binary_input,
		^^clean_text_output.

:- end_object.
