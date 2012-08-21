%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
The description of this puzzle is easily found on the web. For example:

http://www.csci.csusb.edu/dick/cs320/prolog/Potions.htm
*/


:- object(potions).

	:- info([
		version is 1.2,
		date is 2006/3/26,
		author is 'Paulo Moura',
		comment is 'Harry Potter potions logical puzzle.']).

	:- uses(list, [select/3]).

	:- public(potions/7).
	:- mode(potions(?atom, ?atom, ?atom, ?atom, ?atom, ?atom, ?atom), zero_or_one).
	:- info(potions/7, [
		comment is 'Contents of the seven potions.',
		argnames is ['P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7']]).

	contents([wine, wine, poison, poison, poison, forward, backwards]).

	potions(P1, P2, P3, P4, P5, P6, P7) :-
		contents(H1),
		select(P1, H1, H2),
		select(P7, H2, H3),
		P1 \== P7, P1 \== forward, P7 \== forward,				% second clue
		select(P2, H3, H4),
		P2 \== poison,
		select(P3, H4, H5),
		P3 \== poison,											% third clue
		select(P6, H5, H6),										% fourth clue
		P2 == P6,
		select(P4, H6, H7),
		select(P5, H7, []),
		two_pairs_poison_wine([P1, P2, P3, P4, P5, P6, P7]).	% first clue

	two_pairs_poison_wine(S) :-
		poison_wine_pair(S, R),
		poison_wine_pair(R, _).

	poison_wine_pair([poison, wine| R], R) :-
		!.
	poison_wine_pair([_| L], R) :-
		poison_wine_pair(L, R).

:- end_object.
