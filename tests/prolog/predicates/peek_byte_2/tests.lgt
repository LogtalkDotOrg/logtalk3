%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		date is 2015/05/10,
		comment is 'Unit tests for the ISO Prolog standard peek_byte/1-2 built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.13.2.4

	succeeds(iso_peek_byte_2_01) :-
		^^set_binary_input([113,119,101,114]),
		{peek_byte(Byte)},
		Byte == 113,
		^^check_binary_input([113,119,101,114]).

	succeeds(iso_peek_byte_2_02) :-
		^^set_binary_input(st_i, [113,119,101,114]),
		{peek_byte(st_i, Byte)},
		Byte == 113,
		^^check_binary_input(st_i, [113,119,101,114]).

	fails(iso_peek_byte_2_03) :-
		^^set_binary_input(st_i, [113,119,101,114]),
		{peek_byte(st_i, 114)}.

	succeeds(iso_peek_byte_2_04) :-
		^^set_binary_input(st_i, []),
		{peek_byte(st_i, Byte)},
		Byte == -1,
		^^check_binary_input(st_i, []).

	throws(iso_peek_byte_2_05, error(permission_error(input,stream,user_output),_)) :-
		{peek_byte(user_output, _Byte)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_peek_byte_2_06, error(instantiation_error,_)) :-
		{peek_byte(_, _)}.

	throws(sics_peek_byte_2_07, error(type_error(in_byte,p),_)) :-
		^^set_binary_input([]),
		{peek_byte(p)}.

	throws(sics_peek_byte_2_08, error(type_error(in_byte,-2),_)) :-
		^^set_binary_input([]),
		{peek_byte(-2)}.

	throws(sics_peek_byte_2_09, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		{peek_byte(foo,_)}.

	throws(sics_peek_byte_2_10, error(existence_error(stream,S),_)) :-
		^^closed_input_stream(S, [type(binary)]),
		{peek_byte(S,_)}.

	throws(sics_peek_byte_2_11, error(permission_error(input,stream,S),_)) :-
		current_output(S),
		{peek_byte(S,_)}.

	throws(sics_peek_byte_2_12, error(permission_error(input,text_stream,S),_)) :-
		^^set_text_input(''),
		current_input(S),
		{peek_byte(_)}.

	throws(sics_peek_byte_2_13, error(permission_error(input,past_end_of_stream,_),_)) :-
		^^set_binary_input(st_i, [], [eof_action(error)]),
		{get_byte(st_i,_), peek_byte(st_i,_)}.

	% tests from the Logtalk portability work

	succeeds(lgt_peek_byte_2_14) :-
		^^set_binary_input(st_i, [], [eof_action(eof_code)]),
		{get_byte(st_i,_), peek_byte(st_i,Byte)},
		Byte == -1.

	throws(lgt_peek_byte_2_15, error(permission_error(input,stream,s),_)) :-
		^^set_binary_output(s, []),
		{peek_byte(s,_)}.

	throws(lgt_peek_byte_2_16, error(permission_error(input,text_stream,s),_)) :-
		^^set_text_input(s, ''),
		{peek_byte(s,_)}.

	cleanup :-
		^^clean_text_input,
		^^clean_binary_input,
		^^clean_binary_output.

:- end_object.
