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
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard close/1-2 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.6

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	succeeds(sics_close_1_01) :-
		os::expand_path(foo, Path),
		open(Path, write, S),
		{close(S)},
		^^check_text_file(Path, '').

	throws(sics_close_1_02, error(instantiation_error,_)) :-
		{close(_)}.

	throws(sics_close_1_03, error(instantiation_error,_)) :-
		{current_input(S)},
		{close(S, _)}.

	throws(sics_close_1_04, error(instantiation_error,_)) :-
		{current_input(S)},
		{close(S, [force(true)|_])}.

	throws(sics_close_1_05, error(instantiation_error,_)) :-
		{current_input(S)},
		{close(S, [force(true),_])}.

	throws(sics_close_1_06, error(type_error(list,foo),_)) :-
		{current_input(S)},
		{close(S, foo)}.

	throws(sics_close_1_07, error(domain_error(close_option,foo),_)) :-
		{current_input(S)},
		{close(S, [foo])}.

	:- if(current_logtalk_flag(prolog_conformance, iso_strict)).
		throws(sics_close_1_08, error(domain_error(stream_or_alias,foo),_)) :-
			{close(foo)}.
	:- else.
		throws(sics_close_1_08, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
			% the second exception term is a common but not conforming alternative
			{close(foo)}.
	:- endif.

	throws(sics_close_1_09, error(existence_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{close(S)}.

	% tests from the Logtalk portability work

	succeeds(lgt_close_1_10) :-
		{close(user_input)}.

	succeeds(lgt_close_1_11) :-
		{close(user_output)}.

	succeeds(lgt_close_1_12) :-
		{close(user_error)}.

	succeeds(lgt_close_1_13) :-
		^^set_text_output(''),
		current_output(S),
		{close(S, [force(true)])}.

	succeeds(lgt_close_1_14) :-
		^^set_text_output(s, ''),
		{close(s, [force(true)])}.

	cleanup :-
		^^clean_file(foo).

:- end_object.
