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
		comment is 'Unit tests for the ISO Prolog standard set_output/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.4

	succeeds(sics_set_output_1_1) :-
		{current_output(S)},
		{set_output(S)}.

	throws(sics_set_output_1_2, error(instantiation_error,_)) :-
		{set_output(_S)}.

	throws(sics_set_output_1_3, error(domain_error(stream_or_alias,foo),_)) :-
		{set_output(foo)}.

:- end_object.
