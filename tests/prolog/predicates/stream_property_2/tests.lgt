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
		comment is 'Unit tests for the ISO Prolog standard stream_property/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.8.4

	succeeds(iso_stream_property_2_01) :-
		os::expand_path(foo, FooPath),
		os::expand_path(bar, BarPath),
		^^create_text_file(FooPath, ''),
		open(FooPath, read, S1),
		open(BarPath, write, S2),
		findall(S-F, {stream_property(S, file_name(F))}, L),
		memberchk(S1-FooPath, L),
		memberchk(S2-BarPath, L).

	succeeds(iso_stream_property_2_02) :-
		os::expand_path(bar, BarPath),
		open(BarPath, write, FOut),
		current_output(COut),
		findall(S, {stream_property(S, output)}, L),
		memberchk(FOut, L),
		memberchk(COut, L).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_stream_property_2_03, error(domain_error(stream,foo),_)) :-
		{stream_property(foo, _S)}.

	throws(sics_stream_property_2_04, error(domain_error(stream_property,foo),_)) :-
		{stream_property(_S, foo)}.

	succeeds(sics_stream_property_2_05) :-
		current_input(S),
		findall(P, {stream_property(S, P)}, L),
		memberchk(input, L),
		memberchk(alias(user_input), L),
		memberchk(eof_action(reset), L),
		memberchk(mode(read), L),
		memberchk(reposition(false), L),
		memberchk(type(text), L).

	succeeds(sics_stream_property_2_06) :-
		current_output(S),
		findall(P, {stream_property(S, P)}, L),
		memberchk(output, L),
		memberchk(alias(user_output), L),
		memberchk(eof_action(reset), L),
		memberchk(mode(append), L),
		memberchk(reposition(false), L),
		memberchk(type(text), L).

	fails(sics_stream_property_2_07) :-
		{stream_property(_S, type(binary))}.

	cleanup :-
		os::delete_file(foo),
		os::delete_file(bar).

	memberchk(Element, [Head| _]) :-
		Element == Head,
		!.
	memberchk(Element, [_| Tail]) :-
		memberchk(Element, Tail).

:- end_object.
