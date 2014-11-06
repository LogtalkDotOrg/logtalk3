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
		comment is 'Unit tests for the ISO Prolog standard at_end_of_stream/0-1 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.8.4

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_at_end_of_stream_1_01, error(instantiation_error,_)) :-
		{at_end_of_stream(_S)}.

	throws(sics_at_end_of_stream_1_02, error(domain_error(stream_or_alias,foo),_)) :-
		{at_end_of_stream(foo)}.

	throws(sics_at_end_of_stream_1_03, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{at_end_of_stream(S)}.

	succeeds(sics_at_end_of_stream_1_04) :-
		^^set_text_input(st_i, ''),
		{at_end_of_stream(st_i)},
		^^check_text_input(st_i, '').

	succeeds(sics_at_end_of_stream_1_05) :-
		^^set_text_input(st_i, 'a'),
		\+ {at_end_of_stream(st_i)},
		^^check_text_input(st_i, 'a').

	succeeds(sics_at_end_of_stream_1_06) :-
		^^set_binary_input(st_i, []),
		{at_end_of_stream(st_i)},
		^^check_binary_input(st_i, []).

	succeeds(sics_at_end_of_stream_1_07) :-
		^^set_binary_input(st_i, [0]),
		\+ {at_end_of_stream(st_i)},
		^^set_binary_input(st_i, [0]).

:- end_object.
