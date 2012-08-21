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
This example illustrates the use of the predicate directive alias/3 for 
defining alternative names for inherited predicates.
*/


% first, we define a simple parametric object for representing rectangles:

:- object(rectangle(_Width, _Height)).

	:- public(width/1).
	:- public(height/1).
	:- public(area/1).

	width(Width) :-
		parameter(1, Width).

	height(Height) :-
		parameter(2, Height).

	area(Area) :-
		::width(Width),
		::height(Height),
		Area is Width*Height.

:- end_object.


% next, we define a square object which adds an alias, side/1, for the 
% inherited predicate width/1:

:- object(square(Side),
	extends(rectangle(Side, Side))).

	:- alias(rectangle(_, _), width/1, side/1).

:- end_object.


% a similar example can be defined using ellipses and circles:

:- object(ellipse(_RX, _RY)).

	:- public(rx/1).
	:- public(ry/1).
	:- public(area/1).

	rx(Rx) :-
		parameter(1, Rx).

	ry(Ry) :-
		parameter(2, Ry).

	area(Area) :-
		::rx(Rx),
		::ry(Ry),
		Area is Rx*Ry*3.1415927.

:- end_object.


% in this case, we define an alias named r/1 for the inherited 
% predicate rx/1:

:- object(circle(Radius),
	extends(ellipse(Radius, Radius))).

	:- alias(ellipse(_, _), rx/1, r/1).

:- end_object.
