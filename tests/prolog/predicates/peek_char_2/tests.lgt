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
		comment is 'Unit tests for the ISO Prolog standard peek_char/1-2 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.12.2.4

	throws(iso_peek_char_2_12, error(permission_error(input,stream,user_output),_)) :-
		{peek_char(user_output, _)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_peek_char_2_14, error(instantiation_error,_)) :-
		{peek_char(_, _)}.

	% skip the next two tests for now as some Prolog systems don't type check the output argument

	- throws(sics_peek_char_2_15, error(type_error(in_character,1),_)) :-
		{peek_char(1)}.

	- throws(sics_peek_char_2_16, error(type_error(in_character,1),_)) :-
		{peek_char(user_input, 1)}.

	throws(sics_peek_char_2_17, error(domain_error(stream_or_alias,foo),_)) :-
		{peek_char(foo,_)}.

:- end_object.
