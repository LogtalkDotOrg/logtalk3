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
		comment is 'Unit tests for the ISO Prolog standard current_output/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.2

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	succeeds(sics_current_input_1_1) :-
		{current_output(_S)}.

	throws(sics_current_input_1_2, error(domain_error(stream,foo),_)) :-
		{current_output(foo)}.

	fails(sics_current_input_1_3) :-
		{current_input(S),
		 current_output(S)}.

	throws(sics_current_input_1_4, error(domain_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{current_output(S)}.

	succeeds(sics_current_input_1_5) :-
		{current_output(S),
		 current_output(S)}.

:- end_object.
