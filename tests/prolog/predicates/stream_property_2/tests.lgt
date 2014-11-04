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
		comment is 'Unit tests for the ISO Prolog standard stream_property/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.11.8.4

	throws(sics_stream_property_2_03, error(domain_error(stream,foo),_)) :-
		{stream_property(foo, _S)}.

	throws(sics_stream_property_2_04, error(domain_error(stream_property,foo),_)) :-
		{stream_property(_S, foo)}.

	fails(sics_stream_property_2_07) :-
		{stream_property(_S, type(binary))}.

:- end_object.
