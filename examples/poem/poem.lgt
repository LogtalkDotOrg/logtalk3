%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/* Points are going to be the fundamental quantity.                     */
/* These will be defined in Cartesian co-ordinates.                     */

:- object(point(_X, _Y)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric points.',
		parnames is ['X', 'Y'],
		source is 'Example adapted from the POEM system by Ben Staveley-Taylor.']).

	:- public(identical/1).
	:- mode(identical(+nonvar), one).

	:- public(distance/2).
	:- mode(distance(+nonvar, -number), one).

	identical(point(X1, Y1)) :-
		/* succeeds if the argument and owner points are the same.  */
		parameter(1, X),
		parameter(2, Y),
		X1 = X,
		Y1 = Y.

	distance(point(X1, Y1), Distance) :-
		/* finds the distance between argument and owner points.    */
		parameter(1, X),
		parameter(2, Y),
		Distance is sqrt((X1-X) * (X1-X) + (Y1-Y) * (Y1-Y)).

:- end_object.



/* A line is defined by its end points.                                 */
/* This class shows examples of calling its own and other class         */
/* predicates.                                                          */

:- object(line(_Point1, _Point2)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric lines.',
		parnames is ['Point1', 'Point2'],
		source is 'Example adapted from the POEM system by Ben Staveley-Taylor.']).

	:- public(length/1).
	:- mode(length(-number), one).

	:- public(intersects/1).
	:- mode(intersects(+nonvar), one).

	:- public(signed_distance/2).
	:- mode(signed_distance(+nonvar, -number), one).

	:- public(distance/2).
	:- mode(distance(+nonvar, -number), one).

	length(Length) :-
		/* sets Len to the length of the owner line                 */
		parameter(1, P1),
		parameter(2, P2),
		P1::distance(P2, Length).

    intersects(Line2) :-
		/* succeeds if Line2 intersects the owner line.             */
		/* this isn't necessarily a good method, but shows how to   */
		/* call class procedures from within the class definition.  */
		parameter(1, P1),
		parameter(2, P2),
		Line1 = line(P1, P2),
		Line2 = line(P3, P4),
		Line2::signed_distance(P1, D1),
		Line2::signed_distance(P2, D2),
		opposite_signs(D1, D2),
		Line1::signed_distance(P3, D3),
		Line1::signed_distance(P4, D4),
		opposite_signs(D3, D4).

	signed_distance(Point, Dist) :-
		/* finds the perpendicular distance from point to line.     */
		/* the sign of the answer depends on which side of the      */
		/* line the point is on.                                    */
		parameter(1, P1),
		parameter(2, P2),
		P1 = point(X1, Y1),
		P2 = point(X2, Y2),
		Point = point(X3, Y3),
		A is X2-X1,
		B is Y1-Y2,
		C is X1*Y2-X2*Y1,
		Dist is (A*Y3+B*X3+C) / sqrt(A*A+B*B).

	distance(Point, Dist) :-
		/* as 'signed_distance', but Dist always >= 0               */
		parameter(1, P1),
		parameter(2, P2),
		line(P1, P2)::signed_distance(Point, Temp),
		Dist is abs(Temp).

	/* 'opposite_signs' succeeds if its arguments are of opposite signs.    */
	/* It has a feature in that 'opposite_signs(0,0)' succeeds: this is     */
	/* because 0 is treated as having optional sign.                        */

	opposite_signs(A, B) :-
		o_s_aux(A, B).
	opposite_signs(A, B) :-
		o_s_aux(B, A).

	o_s_aux(A, B) :-
		A >= 0,
		B =< 0.

:- end_object.



/* Ellipses are defined by centre and semi-axes                         */

:- object(ellipse(_Center, _A, _B)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric ellipses.',
		parnames is ['Center', 'Rx', 'Ry'],
		source is 'Example adapted from the POEM system by Ben Staveley-Taylor.']).

	:- public(area/1).
	:- mode(area(-number), one).

	area(Area) :-
		pi(Pi),
		parameter(2, A),
		parameter(3, B),
		Area is Pi*A*B.

	pi(3.14196).

:- end_object.



/* Circle is a special form of ellipse                                  */
/* Subclasses ('circle' here) must have the same number of arguments    */
/* as their superclass ('ellipse') for the superclass predicates to     */
/* be applicable.  The arguments may be renamed for clarity.            */

:- object(circle(Center, Radius),
	extends(ellipse(Center, Radius, Radius))).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric circles.',
		parnames is ['Center', 'Radius'],
		source is 'Example adapted from the POEM system by Ben Staveley-Taylor.']).

	:- public(circumference/1).
	:- mode(circumference(-number), one).

	circumference(Circumference) :-
		pi(Pi),
		parameter(2, Radius),
		Circumference is 2*Pi*Radius.

	pi(3.14196).

:- end_object.



/************************************************************************/
/*                                                                      */
/* Now some examples of using the above definitions ...                 */
/*                                                                      */
/************************************************************************/


run :-
	banner,
	points,
	ellipse,
	lines,
	circle.


setup_points(P45, P66, AnotherP45, Pm4m3, P00) :-
	P45 = point(4, 5),
	P66 = point(6, 6),
    AnotherP45 = P45,
	Pm4m3 = point(-4, -3),
	P00 = point(0, 0).


setup_lines(L1, L2, L3, L4) :-
    setup_points(P45, P66, _, Pm4m3, P00),
	L1 = line(P00, P45),
	L2 = line(Pm4m3, P66),
	L3 = line(Pm4m3, P45),
	L4 = line(P00, P66).


banner :-
    write('Logtalk adaptation of a Ben Staveley-Taylor POEM example.'), nl,
    write('Original banner:'), nl, nl,
    nl,
    write('POEM demonstration file.'), nl, nl,
    write('The example output that follows is produced by Prolog code'), nl,
    write('using the object language enhancement POEM.  Please look'), nl,
    write('through the code file "shapes.pl" provided to understand how'), nl,
    write('the class facilities are being used.'), nl.


points :-
    nl,
    write('(1) point manipulation:'), nl,
    setup_points(P45, P66, AnotherP45, _, P00),
    write('distance from (4,5) to (6,6) is '),
    P45::distance(P66, D),
    write(D), nl,
    P45::identical(AnotherP45),
    write('P45 and AnotherP45 are identical points'), nl,
    \+ P00::identical(P66),
    write('P00 and P66 are different points'), nl.


ellipse :-
    nl,
    write('(2) ellipse manipulation:'), nl,
	P56 = point(5, 6),
	E = ellipse(P56, 3, 5),
    write('Area of ellipse of semi-axes 3 and 5 is '),
    E::area(A),
    write(A), nl.


lines :-
    nl,
    write('(3) line manipulation:'), nl,
    setup_lines( L1, L2, L3, L4 ),
	P33 = point(3, 3),
    write('distance from '),write(L2),write(' to '),write(P33),write(' is '),
    L2::distance(P33, D),
    write(D), nl,
    L1::intersects(L2),
    write(L1), write(' intesects '), write(L2), nl,
    \+ L3::intersects(L4),
    write(L3), write(' does not intersect '), write(L4), nl.


circle :-
    nl,
    write('(4) circle manipulation:'), nl,
    write('  Circles are subsets of ellipses, so the "area" function'), nl,
    write('  is available, and a new "circumference" function.'), nl,
	P22 = point(2, 2),
	C = circle(P22, 3),
    write('Area of circle radius 3 is '),
    C::area(A),
    write(A), nl,
    write('Circumference of circle radius 3 is '),
    C::circumference(Circ),
    write(Circ), nl, nl.
