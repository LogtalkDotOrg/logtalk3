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
		date is 2014/11/05,
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

	% skip the next three tests for now as some Prolog systems don't type check the output argument

	- throws(sics_peek_code_2_08, error(type_error(integer,p),_)) :-
		{peek_code(p)}.

	- throws(sics_peek_code_2_09, error(type_error(integer,p),_)) :-
		{peek_code(user_input,p)}.

	- throws(sics_peek_code_2_10, error(representation_error(in_character_code),_)) :-
		{peek_code(-2)}.

	throws(sics_peek_code_2_11, error(domain_error(stream_or_alias,foo),_)) :-
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

	throws(sics_peek_code_2_15, error(permission_error(input,past_end_of_stream,S),_)) :-
		^^set_text_input(''),
		current_input(S),
		{get_code(_), peek_code(_)}.

	succeeds(sics_peek_code_2_16) :-
		^^set_text_input(''),
		{peek_code(C1), peek_code(C2)},
		C1 == -1, C2 == -1.

	throws(sics_peek_code_2_17, error(representation_error(character),_)) :-
		os::expand_path(t, Path),
		^^create_binary_file(Path, [0]),
		open(Path, read, Stream),
		{peek_code(Stream, _)}.

	cleanup :-
		os::delete_file(t),
		^^clean_text_input,
		^^clean_binary_input.

:- end_object.
