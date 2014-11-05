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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/11/03,
		comment is 'Unit tests for the ISO Prolog standard put_byte/1-2 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.13.3.4

	succeeds(iso_put_byte_2_01) :-
		^^set_binary_output([113,119,101,114]),
		{put_byte(116)},
		^^check_binary_output([113,119,101,114,116]).

	succeeds(iso_put_byte_2_02) :-
		^^set_binary_output(st_o, [113,119,101,114]),
		{put_byte(st_o, 116)},
		^^check_binary_output(st_o, [113,119,101,114,116]).

	throws(iso_put_byte_2_03, error(instantiation_error,_)) :-
		^^set_binary_output(my_file_1, []),
		{put_byte(my_file_1, _C)}.

	throws(iso_put_byte_2_04, error(type_error(byte, ty),_)) :-
		^^set_binary_output(my_file_2, []),
		{put_byte(my_file_2, 'ty')}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_put_byte_2_05, error(instantiation_error,_)) :-
		{put_byte(_S, 118)}.

	throws(sics_put_byte_2_06, error(instantiation_error,_)) :-
		^^set_binary_output([]),
		{put_byte(_C)}.

	throws(sics_put_byte_2_07, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, [type(binary)]),
		{put_byte(S, 77)}.

	throws(sics_put_byte_2_08, error(permission_error(output,stream,S),_)) :-
		^^set_binary_input([]),
		current_input(S),
		{put_byte(S, 99)}.

	throws(sics_put_byte_2_09, error(permission_error(output,text_stream,S),_)) :-
		current_output(S),
		{put_byte(99)}.

	throws(sics_put_byte_2_10, error(type_error(byte,-1),_)) :-
		^^set_binary_output([]),
		{put_byte(-1)}.

	throws(sics_put_byte_2_11, error(instantiation_error,_)) :-
		{put_byte(_S, 1)}.

	throws(sics_put_byte_2_12, error(domain_error(stream_or_alias, foo),_)) :-
		{put_byte(foo, 1)}.

	cleanup :-
		^^clean_binary_input,
		^^clean_binary_output.

:- end_object.
