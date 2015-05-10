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
		version is 1.4,
		author is 'Paulo Moura',
		date is 2015/05/10,
		comment is 'Unit tests for the ISO Prolog standard peek_char/1-2 built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.12.2.4

	succeeds(iso_peek_char_2_01) :-
		^^set_text_input('qwerty'),
		{peek_char(Char)},
		Char == 'q',
		^^check_text_input('qwerty').

	succeeds(iso_peek_char_2_02) :-
		^^set_text_input(st_i, 'qwerty'),
		{peek_char(st_i, Char)},
		Char == 'q',
		^^check_text_input(st_i, 'qwerty').

	succeeds(iso_peek_char_2_03) :-
		^^set_text_input(st_i, '\'qwerty\''),
		{peek_char(st_i, Char)},
		Char == '\'',
		^^check_text_input(st_i, '\'qwerty\'').

	fails(iso_peek_char_2_04) :-
		^^set_text_input(st_i, 'qwerty'),
		{peek_char(st_i, p)}.

	succeeds(iso_peek_char_2_05) :-
		^^set_text_input(st_i, ''),
		{peek_char(st_i, Char)},
		Char == end_of_file,
		^^check_text_input(st_i, '').

	succeeds(iso_peek_char_2_06) :-
		^^set_text_input(s, '', [eof_action(error)]),
		catch({get_char(s, _), peek_char(s, _Char)}, error(permission_error(input,past_end_of_stream,_),_), true),
		stream_property(S, alias(s)),
		stream_property(S, end_of_stream(past)).

	throws(iso_peek_char_2_07, error(permission_error(input,stream,user_output),_)) :-
		{peek_char(user_output, _)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_peek_char_2_08, error(instantiation_error,_)) :-
		{peek_char(_, _)}.

	% skip the next two tests for now as some Prolog systems don't type check the output argument

	- throws(sics_peek_char_2_09, error(type_error(in_character,1),_)) :-
		{peek_char(1)}.

	- throws(sics_peek_char_2_10, error(type_error(in_character,1),_)) :-
		{peek_char(user_input, 1)}.

	throws(sics_peek_char_2_11, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		{peek_char(foo,_)}.

	throws(sics_peek_char_2_12, error(existence_error(stream,S),_)) :-
		^^closed_input_stream(S, []),
		{peek_char(S, _)}.

	throws(sics_peek_char_2_13, error(permission_error(input,stream,S),_)) :-
		current_output(S),
		{peek_char(S, _)}.

	throws(sics_peek_char_2_14, error(permission_error(input,binary_stream,s),_)) :-
		^^set_binary_input(s, []),
		{peek_char(s, _)}.

	succeeds(sics_peek_char_2_15) :-
		os::expand_path(t, Path),
		^^create_text_file(Path, ''),
		open(Path, read, Stream),
		{peek_char(Stream, C1), peek_char(Stream, C1), peek_char(Stream, C2)},
		C1 == end_of_file, C2 == end_of_file.

	throws(sics_peek_char_2_16, error(representation_error(character),_)) :-
		os::expand_path(t, Path),
		^^create_binary_file(Path, [0]),
		open(Path, read, Stream),
		{peek_char(Stream, _)}.

	succeeds(lgt_peek_char_2_17) :-
		^^set_text_input(s, '', [eof_action(eof_code)]),
		{get_char(s, end_of_file), peek_char(s, end_of_file)}.

	throws(lgt_peek_char_2_18, error(permission_error(input,stream,s),_)) :-
		^^set_text_output(s, ''),
		{peek_char(s, _)}.

	cleanup :-
		^^clean_file(t),
		^^clean_text_input,
		^^clean_binary_input,
		^^clean_text_output.

:- end_object.
