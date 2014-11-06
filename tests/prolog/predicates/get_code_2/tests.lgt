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
		date is 2014/11/06,
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

	% skip the next three tests for now as some Prolog systems don't type check the output argument

	- throws(sics_get_code_2_08, error(type_error(integer,p),_)) :-
		{get_code(p)}.

	- throws(sics_get_code_2_09, error(type_error(integer,p),_)) :-
		{get_code(user_input,p)}.

	- throws(sics_get_code_2_10, error(representation_error(in_character_code),_)) :-
		{get_code(-2)}.

	:- if(current_logtalk_flag(prolog_conformance, iso_strict)).
		throws(sics_get_code_2_11, error(domain_error(stream_or_alias,foo),_)) :-
			{get_code(foo,_)}.
	:- else.
		throws(sics_get_code_2_11, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
			% the second exception term is a common but not conforming alternative
			{get_code(foo,_)}.
	:- endif.

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
		^^set_text_input(''),
		current_input(S),
		catch({get_code(_), get_code(_)}, error(permission_error(input,past_end_of_stream,S),_), true),
		stream_property(S, end_of_stream(past)).

	succeeds(sics_get_code_2_16) :-
		os::expand_path(t, Path),
		^^create_text_file(Path, ''),
		open(Path, read, S, [eof_action(eof_code)]),
		{get_code(S, C1), get_code(S, C2)},
		C1 == -1, C2 == -1,
		stream_property(S, end_of_stream(past)).

	throws(sics_get_code_2_17, error(representation_error(character),_)) :-
		os::expand_path(t, Path),
		^^create_binary_file(Path, [0]),
		open(Path, read, S),
		{get_code(S, _)}.

	cleanup :-
		os::delete_file(t),
		^^clean_text_input,
		^^clean_binary_input.

:- end_object.
