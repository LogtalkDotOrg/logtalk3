%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(assumptions).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/06/26,
		comment is 'Simple implementation of ground linear and intuitionistic assumptions.'
	]).

	:- public(assumel/1).
	:- mode(assumel(+callable), one).
	:- info(assumel/1, [
		comment is 'Assume a ground fact to be used once.',
		argnames is ['Fact']
	]).

	assumel(Fact) :-
		(	assertz((Fact :- retractall(Fact)))
		;	retractall(Fact),
			!,
			fail
		).

	:- public(assumei/1).
	:- mode(assumei(+callable), one).
	:- info(assumei/1, [
		comment is 'Assume a ground fact to be used any number of times.',
		argnames is ['Fact']
	]).

	assumei(Fact) :-
		(	assertz(Fact)
		;	retract(Fact),
			!,
			fail
		).

:- end_category.
