%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(switch,
	imports(assumptions)).

	:- info([
		version is 1.0,
		author is 'Orginal example by Paul Tarau et al. Adapted to Logtalk by Paulo Moura.',
		date is 2014/06/26,
		comment is 'Example of defining a switch...case control construct using linear assumptions.'
	]).

	:- public(test/1).
	:- mode(test(+integer), one).
	:- info(test/1, [
		comment is 'Test predicate for the switch control construct.',
		argnames is ['Value']
	]).

	switch(Selector,Body) :-
		^^assumel(case(Selector)),
		call(Body).

	default :-
		case(_).

	test(X) :-
		switch(X, (
			case(1) -> write(one) ;
			case(2) -> write(two) ;
			default -> write(unexpected(X))
		)), nl.

	:- private(case/1).
	:- dynamic(case/1).

:- end_object.
