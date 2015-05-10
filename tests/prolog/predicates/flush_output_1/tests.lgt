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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/05/10,
		comment is 'Unit tests for the ISO Prolog standard flush_output/0-1 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.7

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	succeeds(sics_flush_output_1_01) :-
		os::expand_path(foo, Path),
		open(Path, write, S), write(S, foo),
		{flush_output(S)},
		^^check_text_file(Path, 'foo').

	throws(sics_flush_output_1_02, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		{flush_output(foo)}.

	throws(sics_flush_output_1_03, error(instantiation_error,_)) :-
		{flush_output(_S)}.

	throws(sics_flush_output_1_04, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{flush_output(S)}.

	throws(sics_flush_output_1_05, error(permission_error(output,stream,S),_)) :-
		os::expand_path(foo, Path),
		^^create_text_file(Path, ''),
		open(Path, read, S),
		{flush_output(S)}.

	succeeds(sics_flush_output_1_06) :-
		^^set_text_output(st_o, ''),
		{flush_output(st_o)},
		^^check_text_output(st_o, '').

	% tests from the ECLiPSe test suite

	throws(eclipse_flush_output_1_07, error(permission_error(output,stream,user_input),_)) :-
		{flush_output(user_input)}.

	succeeds(eclipse_flush_output_1_08) :-
		{flush_output(user_output)}.

	cleanup :-
		^^clean_file(foo).

:- end_object.
