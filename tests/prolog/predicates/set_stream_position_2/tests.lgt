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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/05/19,
		comment is 'Unit tests for the ISO Prolog standard set_stream_position/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.9

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_set_stream_position_2_01, error(instantiation_error,_)) :-
		^^stream_position(Pos),
		{set_stream_position(_S, Pos)}.

	throws(sics_set_stream_position_2_02, error(instantiation_error,_)) :-
		% the original test used the current input stream but this results in a test that
		% can trigger two different errors depending on the order of argument checking
		% {current_input(S)},
		^^set_text_input(st_i, '', [reposition(true)]),
		{set_stream_position(st_i, _Pos)}.

	throws(sics_set_stream_position_2_03, [error(domain_error(stream_or_alias,foo),_), error(existence_error(stream,foo),_)]) :-
		% both exception terms seem to be acceptable in the ISO spec
		^^stream_position(Pos),
		{set_stream_position(foo,Pos)}.

	throws(sics_set_stream_position_2_04, error(existence_error(stream,S),_)) :-
		^^stream_position(Pos),
		^^closed_output_stream(S, []),
		{set_stream_position(S, Pos)}.

	throws(sics_set_stream_position_2_05, error(domain_error(stream_position,foo),_)) :-
		% the original test used the current input stream but this results in a test that
		% can trigger two different errors depending on the order of argument checking
		% {current_input(S)},
		^^set_text_input(st_i, '', [reposition(true)]),
		{set_stream_position(st_i, foo)}.

	throws(sics_set_stream_position_2_06, error(permission_error(reposition,stream,S),_)) :-
		os::expand_path(foo, Path),
		{open(Path, write, FS), stream_property(FS, position(Pos)), current_input(S)},
		{set_stream_position(S, Pos)}.

	cleanup :-
		^^clean_file(foo).

:- end_object.
