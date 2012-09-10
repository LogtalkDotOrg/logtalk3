%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(walker).

	:- info([
		version is 1.0,
		date is 2004/4/29,
		author is 'Paulo Moura',
		comment is 'Walker movements.']).

	:- public(walk/2).
	:- mode(walk(@list, -position), one).
	:- info(walk/2, [
		comment is 'Parses a sequence of walker moves, returning ending position.',
		argnames is ['Moves', 'Ending']]).

	walk(Moves, Ending) :-
		phrase(walk(Ending), Moves).

	walk(Ending) -->
		moves((0, 0), Ending).

	moves(Start, Ending) -->
		move(Start, Temp), moves(Temp, Ending). 
	moves(Ending, Ending) -->
		[]. 

	move((X0, Y0), (X, Y)) --> [ n(S)], {X is X0, Y is Y0 + S}.
	move((X0, Y0), (X, Y)) --> [ne(S)], {X is X0 + S / sqrt(2), Y is Y0 + S / sqrt(2)}.
	move((X0, Y0), (X, Y)) --> [ e(S)], {X is X0 + S, Y = Y0}.
	move((X0, Y0), (X, Y)) --> [se(S)], {X is X0 + S / sqrt(2), Y is Y0 - S / sqrt(2)}.
	move((X0, Y0), (X, Y)) --> [ s(S)], {X is X0, Y is Y0 - S}.
	move((X0, Y0), (X, Y)) --> [sw(S)], {X is X0 - S / sqrt(2), Y is Y0 - S / sqrt(2)}.
	move((X0, Y0), (X, Y)) --> [ w(S)], {X is X0 - S, Y = Y0}.
	move((X0, Y0), (X, Y)) --> [nw(S)], {X is X0 - S / sqrt(2), Y is Y0 + S / sqrt(2)}.

:- end_object.
