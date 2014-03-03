%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(bom).

	:- info([
		version is 1.0,
		date is 2004/5/11,
		author is 'Paulo Moura',
		comment is 'Adaptation of the bill of materials DCG example from the Amzi! Prolog manual.'
	]).

	:- public(parts/2).
	:- mode(parts(+atom, -list), one).
	:- info(parts/2, [
		comment is 'Returns the list of parts for building an object.',
		argnames is ['Object', 'Parts']
	]).

	parts(Object, Parts) :-
		phrase(Object, Parts).

	bike --> frame, drivechain, wheel, wheel.

	wheel --> spokes, rim, hub.

	drivechain --> crank, pedal, pedal, chain.

	spokes --> [spokes].
	crank --> [crank].
	pedal --> [pedal].
	chain --> [chain].
	rim --> [rim].
	hub --> [hub].
	frame --> [frame].

:- end_object.
