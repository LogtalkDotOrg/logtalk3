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
		comment is 'Unit tests for the ISO Prolog standard get_char/1-2 built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.12.1.4

	succeeds(iso_get_char_2_01) :-
		^^set_text_input('qwerty'),
		{get_char(Char)},
		Char == 'q',
		^^check_text_input('werty').

	succeeds(iso_get_char_2_02) :-
		^^set_text_input(st_i, 'qwerty'),
		{get_char(st_i, Char)},
		Char == 'q',
		^^check_text_input(st_i, 'werty').

	succeeds(iso_get_char_2_03) :-
		^^set_text_input(st_i, '\'qwerty\''),
		{get_char(st_i, Char)},
		Char == '\'',
		^^check_text_input(st_i, 'qwerty\'').

	succeeds(iso_get_char_2_04) :-
		^^set_text_input(st_i, 'qwerty'),
		\+ {get_char(st_i, p)},
		^^check_text_input(st_i, 'werty').

	succeeds(iso_get_char_2_05) :-
		^^set_text_input(st_i, ''),
		{get_char(st_i, Char)},
		Char == end_of_file,
		stream_property(Stream, alias(st_i)),
		stream_property(Stream, end_of_stream(past)).

	throws(iso_get_char_2_06, error(permission_error(input,stream,user_output),_)) :-
		{get_char(user_output, _)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_get_char_2_07, error(instantiation_error,_)) :-
		{get_char(_, _)}.

	% skip the next two tests for now as some Prolog systems don't type check the output argument

	- throws(sics_get_char_2_08, error(type_error(in_character,1),_)) :-
		{get_char(1)}.

	- throws(sics_get_char_2_09, error(type_error(in_character,1),_)) :-
		{get_char(user_input, 1)}.

	throws(sics_get_char_2_10, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		{get_char(foo,_)}.

	throws(sics_get_char_2_11, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{get_char(S,_)}.

	throws(sics_get_char_2_12, error(permission_error(input,stream,S),_)) :-
		current_output(S),
		{get_char(S,_)}.

	throws(sics_get_char_2_13, error(permission_error(input,binary_stream,S),_)) :-
		^^set_binary_input([]),
		current_input(S),
		{get_char(_)}.

	succeeds(sics_get_char_2_14) :-
		^^set_text_input(st_i, '', [eof_action(error)]),
		catch({get_char(st_i,_), get_char(st_i,_)}, error(permission_error(input,past_end_of_stream,_),_), true),
		stream_property(S, alias(st_i)),
		stream_property(S, end_of_stream(past)).

	succeeds(sics_get_char_2_15) :-
		os::expand_path(t, Path),
		^^create_text_file(Path, ''),
		open(Path, read, S, [eof_action(eof_code)]),
		{get_char(S, C1), get_char(S, C2)},
		C1 == end_of_file, C2 == end_of_file,
		stream_property(S, end_of_stream(past)).

	throws(sics_get_char_2_16, error(representation_error(character),_)) :-
		os::expand_path(t, Path),
		^^create_binary_file(Path, [0]),
		open(Path, read, S),
		{get_char(S, _)}.

	succeeds(lgt_get_char_2_17) :-
		^^set_text_input(st_i, '', [eof_action(eof_code)]),
		{get_char(st_i,end_of_file), get_char(st_i,end_of_file)},
		stream_property(S, alias(st_i)),
		stream_property(S, end_of_stream(past)).

	throws(lgt_get_char_2_18, error(permission_error(input,stream,s),_)) :-
		^^set_text_output(s, ''),
		{get_char(s, _)}.

	throws(lgt_get_char_2_19, error(permission_error(input,binary_stream,s),_)) :-
		^^set_binary_input(s, []),
		{get_char(s, _)}.

	cleanup :-
		^^clean_file(t),
		^^clean_text_input,
		^^clean_binary_input,
		^^clean_text_output.

:- end_object.
