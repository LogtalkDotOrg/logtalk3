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
		date is 2014/11/21,
		comment is 'Unit tests for the ISO Prolog standard open/3-4 built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.5.4

	succeeds(iso_open_4_01) :-
		os::expand_path('roger_data', Path),
		^^create_binary_file(Path, []),
		{open(Path, read, D, [type(binary)]),
		 at_end_of_stream(D)}.

	succeeds(iso_open_4_02) :-
		os::expand_path('scowen', Path),
		{open(Path, write, D, [alias(editor)]),
		 stream_property(D, alias(editor))}.

	succeeds(iso_open_4_03) :-
		os::expand_path('dave', Path),
		^^create_text_file(Path, 'foo.'),
		{open(Path, read, DD, []),
		 read(DD, foo),
		 at_end_of_stream(DD)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

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

	throws(sics_open_4_15, error(existence_error(source_sink,Path),_)) :-
		os::expand_path('nonexistent', Path),
		{open(Path, read, _)}.

	throws(sics_open_4_16, error(permission_error(open,source_sink,alias(a)),_)) :-
		os::expand_path(foo, Path),
		{open(Path, write, _, [alias(a)]),
		 open(bar, write, _, [alias(a)])}.

	cleanup :-
		^^clean_file(roger_data),
		^^clean_file(scowen),
		^^clean_file(dave),
		^^clean_file(foo).

:- end_object.
