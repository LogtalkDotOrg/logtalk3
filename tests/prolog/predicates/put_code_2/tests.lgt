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
		date is 2014/11/21,
		comment is 'Unit tests for the ISO Prolog standard put_code/1-2 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.12.3.4

	succeeds(iso_put_code_2_01) :-
		^^set_text_output('qwer'),
		{put_code(0't)},
		^^check_text_output('qwert').

	succeeds(iso_put_code_2_02) :-
		^^set_text_output(st_o, 'qwer'),
		{put_code(st_o, 0't)},
		^^check_text_output(st_o, 'qwert').

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(iso_put_code_2_03, error(instantiation_error,_)) :-
		^^set_text_output(my_file, ''),
		{put_code(my_file, _C)},
		^^check_text_output(my_file, '').

	throws(iso_put_code_2_04, error(type_error(integer,ty),_)) :-
		^^set_text_output(st_o, ''),
		{put_code(st_o, 'ty')},
		^^check_text_output(st_o, '').

	throws(sics_put_code_2_05, error(instantiation_error,_)) :-
		{put_code(_, 0't)}.

	throws(sics_put_code_2_06, error(instantiation_error,_)) :-
		{put_code(_)}.
		
	throws(iso_put_code_2_07, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{put_code(S, 0'a)}.
		
	throws(iso_put_code_2_08, error(permission_error(output,stream,S),_)) :-
		current_input(S),
		{put_code(S, 0'a)}.

	throws(iso_put_code_2_09, error(permission_error(output,binary_stream,S),_)) :-
		os::expand_path(t, Path),
		open(Path, write, S, [type(binary)]),
		{put_code(S, 0'a)}.

	throws(sics_put_code_2_10, error(representation_error(character_code),_)) :-
		{put_code(-1)}.

	:- if(current_logtalk_flag(prolog_conformance, iso_strict)).
		throws(sics_put_code_2_11, error(domain_error(stream_or_alias,foo),_)) :-
			{put_code(foo,1)}.
	:- else.
		throws(sics_put_code_2_11, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
			% the second exception term is a common but not conforming alternative
			{put_code(foo,1)}.
	:- endif.

	cleanup :-
		^^clean_file(t),
		^^clean_text_output.

:- end_object.
