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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/05/04,
		comment is 'Unit tests for the ISO Prolog standard put_char/1-2 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.12.3.4

	succeeds(iso_put_char_2_01) :-
		^^set_text_output('qwer'),
		{put_char(t)},
		^^check_text_output('qwert').

	succeeds(iso_put_char_2_02) :-
		^^set_text_output(st_o, 'qwer'),
		{put_char(st_o, 'A')},
		^^check_text_output(st_o, 'qwerA').

	succeeds(iso_put_char_2_03) :-
		^^set_text_output('qwer'),
		{nl, put_char(a)},
		^^check_text_output('qwer\na').

	succeeds(iso_put_char_2_04) :-
		^^set_text_output(st_o, 'qwer'),
		{nl(st_o), put_char(st_o, a)},
		^^check_text_output(st_o, 'qwer\na').

	throws(iso_put_char_2_05, error(instantiation_error,_)) :-
		^^set_text_output(my_file, ''),
		{put_char(my_file, _C)}.

	throws(iso_put_char_2_06, error(type_error(character, ty),_)) :-
		^^set_text_output(st_o, ''),
		{put_char(st_o, 'ty')}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_put_char_2_07, error(instantiation_error,_)) :-
		{put_char(_, t)}.

	throws(sics_put_char_2_08, error(instantiation_error,_)) :-
		{put_char(_)}.

	throws(sics_put_char_2_09, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{put_char(S, a)}.

	throws(sics_put_char_2_10, error(permission_error(output,stream,S),_)) :-
		current_input(S),
		{put_char(S, a)}.

	throws(sics_put_char_2_11, error(permission_error(output,binary_stream,S),_)) :-
		^^set_binary_output([]),
		current_output(S),
		{put_char(a)}.

	% tests from the Logtalk portability work

	throws(lgt_put_char_2_12, error(permission_error(output,stream,s),_)) :-
		^^set_text_input(s, ''),
		{put_char(s, a)}.

	throws(lgt_put_char_2_13, error(permission_error(output,binary_stream,s),_)) :-
		^^set_binary_output(s, []),
		{put_char(s, a)}.

	cleanup :-
		^^clean_text_output,
		^^clean_binary_output,
		^^clean_text_input.

:- end_object.
