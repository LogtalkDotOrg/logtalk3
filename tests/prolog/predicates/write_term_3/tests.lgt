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
		comment is 'Unit tests for the ISO Prolog standard write_term/3, write_term/2, write/2, write/1, writeq/2, writeq/1, write_canonical/2, and write_canonical/1 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.2.4

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_write_term_3_08, error(instantiation_error,_)) :-
		{write(_S, foo)}.

	throws(sics_write_term_3_09, error(instantiation_error,_)) :-
		{write_term(foo, _Opts)}.

	throws(sics_write_term_3_10, error(instantiation_error,_)) :-
		{write_term(user_output, foo, _Opts)}.

	throws(sics_write_term_3_11, error(instantiation_error,_)) :-
		{write_term(foo, [quoted(true)|_Opts])}.

	throws(sics_write_term_3_12, error(instantiation_error,_)) :-
		{write_term(user_output, foo, [quoted(true)|_Opts])}.

	throws(sics_write_term_3_13, error(instantiation_error,_)) :-
		{write_term(foo, [quoted(true),_Opts])}.

	throws(sics_write_term_3_14, error(instantiation_error,_)) :-
		{write_term(user_output, foo, [quoted(true),_Opts])}.

	throws(sics_write_term_3_15, error(type_error(list,2),_)) :-
		{write_term(user_output, 1, 2)}.

	throws(sics_write_term_3_16, error(type_error(list,[quoted(true)|foo]),_)) :-
		{write_term(1, [quoted(true)|foo])}.

	throws(sics_write_term_3_17, error(domain_error(stream_or_alias,foo),_)) :-
		{write(foo, 1)}.

	throws(sics_write_term_3_18, error(domain_error(write_option,foo),_)) :-
		{write_term(1, [quoted(true),foo])}.

:- end_object.
