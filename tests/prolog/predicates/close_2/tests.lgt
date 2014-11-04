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
		comment is 'Unit tests for the ISO Prolog standard close/1-2 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.6

	succeeds(sics_close_1_01) :-
		{open('foo', write, S)},
		{close(S)}.

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

	throws(sics_close_1_08, error(domain_error(stream_or_alias,foo),_)) :-
		{close(foo)}.

:- end_object.
