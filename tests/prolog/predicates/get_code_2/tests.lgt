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
		comment is 'Unit tests for the ISO Prolog standard get_code/1-2 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.12.1.4

	throws(iso_get_code_2_12, error(permission_error(input,stream,user_output),_)) :-
		{get_code(user_output, _)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_get_code_2_23, error(instantiation_error,_)) :-
		{get_code(_, _)}.

	throws(sics_get_code_2_24, error(type_error(integer,p),_)) :-
		{get_code(p)}.

	throws(sics_get_code_2_25, error(type_error(integer,p),_)) :-
		{get_code(user_input,p)}.

	throws(sics_get_code_2_26, error(representation_error(in_character_code),_)) :-
		{get_code(-2)}.

	throws(sics_get_code_2_27, error(domain_error(stream_or_alias,foo),_)) :-
		{get_code(foo,_)}.

:- end_object.
