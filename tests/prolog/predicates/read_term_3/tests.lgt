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
		comment is 'Unit tests for the ISO Prolog standard read_term/3, read_term/2, read/2, and read/1 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.1.4

	throws(sics_read_term_3_08, error(instantiation_error,_)) :-
		{read(_, _)}.

	throws(sics_read_term_3_09, error(instantiation_error,_)) :-
		{read_term(user_input, _, _)}.

	throws(sics_read_term_3_10, error(instantiation_error,_)) :-
		{read_term(user_input,_,[variables(_)|_])}.

	throws(sics_read_term_3_11, error(instantiation_error,_)) :-
		{read_term(user_input,_,[variables(_),_])}.

	throws(sics_read_term_3_12, error(domain_error(stream_or_alias,foo),_)) :-
		{read(foo, _)}.

	throws(sics_read_term_3_13, error(type_error(list,bar),_)) :-
		{read_term(user_input, _, bar)}.

	throws(sics_read_term_3_14, error(domain_error(read_option,bar),_)) :-
		{read_term(user_input, _, [bar])}.

	throws(sics_read_term_3_15, error(permission_error(input,stream,user_output),_)) :-
		{read_term(user_output, _, [])}.

:- end_object.
