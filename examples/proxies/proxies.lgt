%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% we use a parametric object in order to give an interpretation to an 
% object proxy arguments and to encapsulate relevant predicates:

:- object(circle(_Id, _Radius, _Color)).

	:- public([
		id/1, radius/1, color/1,
		area/1, perimeter/1,
		print/0
	]).

	id(Id) :-
		parameter(1, Id).

	radius(Radius) :-
		parameter(2, Radius).

	color(Color) :-
		parameter(3, Color).

	area(Area) :-
		::radius(Radius),
		Area is 3.14159265358979*Radius*Radius.

	perimeter(Perimeter) :-
		::radius(Radius),
		Perimeter is 2*3.14159265358979*Radius.

	print :-
		id(Id), write('id: '), write(Id),
		area(Area), write(', area: '), write(Area),
		perimeter(Perimeter), write(', perimeter: '), write(Perimeter),
		color(Color), write(', color: '), write(Color), nl.

:- end_object.


% parametric object proxies:

circle('#1', 1.23, blue).
circle('#2', 3.71, yellow).
circle('#3', 0.39, green).
circle('#4', 5.74, black).
circle('#5', 8.32, cyan).
