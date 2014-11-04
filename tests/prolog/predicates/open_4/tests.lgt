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
		comment is 'Unit tests for the ISO Prolog standard open/3-4 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.5.4

	throws(sics_open_4_04, error(instantiation_error,_)) :-
		{open(_, read, _)}.

	throws(sics_open_4_05, error(instantiation_error,_)) :-
		{open(f, _, _)}.

	throws(sics_open_4_06, error(instantiation_error,_)) :-
		{open(f, write, _, _)}.

	throws(sics_open_4_07, error(instantiation_error,_)) :-
		{open(f, write, _, [type(text)|_])}.

	throws(sics_open_4_08, error(instantiation_error,_)) :-
		{open(f, write, _, [type(text),_])}.

	throws(sics_open_4_09, error(type_error(atom,1),_)) :-
		{open(f, 1, _)}.

	throws(sics_open_4_10, error(type_error(list,type(text)),_)) :-
		{open(f, write, _, type(text))}.

	throws(sics_open_4_11, error(uninstantiation_error(bar),_)) :-
		{open(f, write, bar)}.

	throws(sics_open_4_12, error(domain_error(source_sink,foo(1,2)),_)) :-
		{open(foo(1,2), write, _)}.

	throws(sics_open_4_13, error(domain_error(io_mode,red),_)) :-
		{open('foo', red, _)}.

	throws(sics_open_4_14, error(domain_error(stream_option,bar),_)) :-
		{open(foo, write, _, [bar])}.

	throws(sics_open_4_15, error(existence_error(source_sink,'nonexistent'),_)) :-
		{open('nonexistent', read, _)}.

:- end_object.
