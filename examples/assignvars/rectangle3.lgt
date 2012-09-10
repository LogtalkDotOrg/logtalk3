%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(rectangle(_Width, _Height, _Position),
	imports(private::assignvars)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2005/1/8,
		comment is 'A simple implementation of a geometric rectangle using assignable variables and parametric objects.',
		parnames is ['Width', 'Height', 'Position']]).

	:- public(init/0).
	:- mode(init, one).
	:- info(init/0,
		[comment is 'Initialize rectangle position.']).

	:- public(area/1).
	:- mode(area(-integer), one).
	:- info(area/1,
		[comment is 'Rectangle area.',
		 argnames is ['Area']]).

	:- public(move/2).
	:- mode(move(+integer, +integer), one).
	:- info(move/2, [
		comment is 'Moves a rectangle to a new position.',
		argnames is ['X', 'Y']]).

	:- public(position/2).
	:- mode(position(?integer, ?integer), zero_or_one).
	:- info(position/2, [
		comment is 'Rectangle current position.',
		argnames is ['X', 'Y']]).

	init :-
		parameter(3, Position),
		::assignable(Position, (0, 0)).

	area(Area) :-
		parameter(1, Width),
		parameter(2, Height),
		Area is Width*Height.

	move(X, Y) :-
		parameter(3, Position),
		::Position <= (X, Y).

	position(X, Y) :-
		parameter(3, Position),
		::Position => (X, Y).

:- end_object.
