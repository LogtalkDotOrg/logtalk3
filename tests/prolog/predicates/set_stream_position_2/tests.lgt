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
		date is 2014/11/05,
		comment is 'Unit tests for the ISO Prolog standard set_stream_position/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.9

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_set_stream_position_2_01, error(instantiation_error,_)) :-
		^^stream_position(Pos),
		{set_stream_position(_S, Pos)}.

	throws(sics_set_stream_position_2_02, error(instantiation_error,_)) :-
		{current_input(S)},
		{set_stream_position(S, _Pos)}.

	throws(sics_set_stream_position_2_03, error(domain_error(stream_or_alias,foo),_)) :-
		^^stream_position(Pos),
		{set_stream_position(foo,Pos)}.

	throws(sics_set_stream_position_2_04, error(existence_error(stream,S),_)) :-
		^^stream_position(Pos),
		^^closed_output_stream(S, []),
		{set_stream_position(S, Pos)}.

	throws(sics_set_stream_position_2_05, error(domain_error(stream_position,foo),_)) :-
		{current_input(S)},
		{set_stream_position(S,foo)}.

	throws(sics_set_stream_position_2_06, error(permission_error(reposition,stream,S),_)) :-
		os::expand_path(foo, Path),
		{open(Path, write, FS), stream_property(FS, position(Pos)), current_input(S)},
		{set_stream_position(S, Pos)}.

	cleanup :-
		os::expand_path(foo, Path),
		os::delete_file(Path).

:- end_object.
