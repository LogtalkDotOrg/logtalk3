%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.7,
		author is 'Paulo Moura',
		date is 2017/08/24,
		comment is 'Unit tests for the ISO Prolog standard peek_code/1-2 built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.12.2.4

	succeeds(iso_peek_code_2_01) :-
		^^set_text_input('qwerty'),
		{peek_code(Code)},
		Code == 0'q,
		^^check_text_input('qwerty').

	succeeds(iso_peek_code_2_02) :-
		^^set_text_input(st_i, 'qwerty'),
		{peek_code(st_i, Code)},
		Code == 0'q,
		^^check_text_input(st_i, 'qwerty').

	succeeds(iso_peek_code_2_03) :-
		^^set_text_input(st_i, '\'qwerty\''),
		{peek_code(st_i, Code)},
		Code == 39,		% 0'\'
		^^check_text_input(st_i, '\'qwerty\'').

	fails(iso_peek_code_2_04) :-
		^^set_text_input(st_i, 'qwerty'),
		{peek_code(st_i, 0'p)}.

	succeeds(iso_peek_code_2_05) :-
		^^set_text_input(st_i, ''),
		{peek_code(st_i, Code)},
		Code == -1,
		^^check_text_input(st_i, '').

	throws(iso_peek_code_2_06, error(permission_error(input,stream,user_output),_)) :-
		{peek_code(user_output, _)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_peek_code_2_07, error(instantiation_error,_)) :-
		{peek_code(_, _)}.

	throws(sics_peek_code_2_08, error(type_error(integer,p),_)) :-
		^^set_text_input('foo'),
		{peek_code(p)}.

	throws(sics_peek_code_2_09, error(type_error(integer,p),_)) :-
		^^set_text_input(st_i, 'foo'),
		{peek_code(st_i, p)}.

	throws(sics_peek_code_2_10, error(representation_error(in_character_code),_)) :-
		^^set_text_input('foo'),
		{peek_code(-2)}.

	throws(sics_peek_code_2_11, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% the second exception term is a common but not conforming alternative
		{peek_code(foo,_)}.

	throws(sics_peek_code_2_12, error(existence_error(stream,S),_)) :-
		^^closed_input_stream(S, []),
		{peek_code(S, _)}.

	throws(sics_peek_code_2_13, error(permission_error(input,stream,S),_)) :-
		current_output(S),
		{peek_code(S, _)}.

	throws(sics_peek_code_2_14, error(permission_error(input,binary_stream,S),_)) :-
		^^set_binary_input([]),
		current_input(S),
		{peek_code(S, _)}.

	succeeds(sics_peek_code_2_15) :-
		^^set_text_input(st_i, '', [eof_action(error)]),
		catch({get_code(st_i,_), peek_code(st_i,_)}, error(permission_error(input,past_end_of_stream,_),_), true),
		stream_property(S, alias(st_i)),
		stream_property(S, end_of_stream(past)).

	succeeds(sics_peek_code_2_16) :-
		^^set_text_input(''),
		{peek_code(C1), peek_code(C2)},
		C1 == -1, C2 == -1.

	succeeds(sics_peek_code_2_17) :-
		os::absolute_file_name(t, Path),
		^^create_binary_file(Path, [0]),
		open(Path, read, Stream),
		catch({peek_code(Stream, _)}, Error, Error = error(representation_error(character),_)).

	% tests from the Logtalk portability work

	succeeds(lgt_peek_code_2_18) :-
		^^set_text_input(st_i, '', [eof_action(eof_code)]),
		{get_code(st_i,_), peek_code(st_i,Code)},
		Code == -1.

	throws(lgt_peek_code_2_19, error(permission_error(input,stream,s),_)) :-
		^^set_text_output(s, ''),
		{peek_code(s, _)}.

	throws(lgt_peek_code_2_20, error(permission_error(input,binary_stream,_),_)) :-
		^^set_binary_input(s, []),
		{peek_code(s, _)}.

	cleanup :-
		^^clean_file(t),
		^^clean_text_input,
		^^clean_binary_input,
		^^clean_text_output.

:- end_object.
