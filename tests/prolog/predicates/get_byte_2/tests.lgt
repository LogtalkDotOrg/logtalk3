%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2015/05/10,
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

	throws(sics_get_byte_2_11, error(permission_error(input,stream,S),_)) :-
		current_output(S),
		{get_byte(S,_)}.

	throws(sics_get_byte_2_12, error(permission_error(input,text_stream,S),_)) :-
		^^set_text_input(''),
		current_input(S),
		{get_byte(_)}.

	succeeds(sics_get_byte_2_13) :-
		^^set_binary_input(st_i, [], [eof_action(error)]),
		catch({get_byte(st_i,_), get_byte(st_i,_)}, error(permission_error(input,past_end_of_stream,_),_), true),
		stream_property(S, alias(st_i)),
		stream_property(S, end_of_stream(past)).

	% tests from the Logtalk portability work

	succeeds(lgt_get_byte_2_14) :-
		^^set_binary_input(st_i, [], [eof_action(eof_code)]),
		{get_byte(st_i,_), get_byte(st_i,Byte)},
		Byte = -1.

	throws(lgt_get_byte_2_15, error(permission_error(input,stream,s),_)) :-
		^^set_binary_output(s, []),
		{get_byte(s,_)}.

	throws(lgt_get_byte_2_16, error(permission_error(input,text_stream,s),_)) :-
		^^set_text_input(s, ''),
		{get_byte(s,_)}.

	cleanup :-
		^^clean_text_input,
		^^clean_binary_input,
		^^clean_binary_output.

:- end_object.
