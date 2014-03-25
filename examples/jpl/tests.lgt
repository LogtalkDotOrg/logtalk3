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
		author is 'Paulo Moura and Sergio Castro',
		date is 2014/03/25,
		comment is 'Unit tests for the "jpl" example.'
	]).

	cover(java(_,_)).
	cover(java(_)).

	test(jpl_1) :-
		java('java.lang.System')::getProperty('java.version').

	test(jpl_2) :-
		java('java.lang.System', Version)::getProperty('java.version'),
		atom(Version).

	test(jpl_3) :-
		java('java.lang.System', Version)::invoke(getProperty, ['java.version']),
		atom(Version).

	test(jpl_4) :-
		java('java.lang.Math')::get_field('PI', Pi),
		float(Pi).

	test(jpl_5) :-
		java('java.awt.Rectangle')::new([100, 20], Rectangle),
		java(Rectangle)::set_field(width, 300),
		java(Rectangle)::get_field(width, Value),
		Value == 300.

	test(jpl_6) :-
		java('java.util.ArrayList')::new(ArrayList),
		java(ArrayList)::(add('Paulo'), add('Carlos'), add('Helena')),
		java(ArrayList, Iterator)::iterator,
		findall(
			Name, 
			(	repeat,
				java(Iterator, HasNext)::hasNext,
				(	HasNext == @(true) ->
					java(Iterator, Name)::next
				;	!,
					fail
				)
			),
			Names
		),
		Names == ['Paulo', 'Carlos', 'Helena'].

:- end_object.
