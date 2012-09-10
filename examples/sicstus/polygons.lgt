%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(rectangle(_Width, _Height)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric rectangles.',
		parnames is ['Width', 'Height']]).

	:- public(width/1).
	:- mode(width(?number), zero_or_one).
	:- info(width/1, [
		comment is 'Rectangle width.',
		argnames is ['Width']]).

	:- public(height/1).
	:- mode(height(?number), zero_or_one).
	:- info(height/1, [
		comment is 'Rectangle height.',
		argnames is ['Height']]).

	:- public(area/1).
	:- mode(area(-number), one).
	:- info(area/1, [
		comment is 'Rectangle area.',
		argnames is ['Area']]).

	width(Width) :-
		parameter(1, Width).

	height(Height) :-
		parameter(2, Height).

	area(Area) :-
		::width(Width),
		::height(Height),
		Area is Width*Height.

:- end_object.


:- object(square(Side),
	extends(rectangle(Side, Side))).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric squares.',
		parnames is ['Side']]).

	:- public(side/1).
	:- mode(side(?number), zero_or_one).
	:- info(side/1, [
		comment is 'Square side.',
		argnames is ['Side']]).

	side(Side) :-
		parameter(1, Side).

	width(Width) :-
		parameter(1, Width).

	height(Height) :-
		parameter(1, Height).

:- end_object.
