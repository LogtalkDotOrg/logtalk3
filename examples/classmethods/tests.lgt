%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/06,
		comment is 'Unit tests for the "classmethods" example.']).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	unit(metacircle).
	unit(circle).
	unit(c42).

	test(classmethods_1) :-
		circle::area(1.0, Area),
		Area =~= 3.14159265358979.

	test(classmethods_2) :-
		c42::area(Area),
		Area =~= 24.630086404144.

	test(classmethods_3) :-
		circle::new(1.2, 7.9, 2.0, Circle),
		Circle::area(Area),
		Area =~= 4.5238934211693.

:- end_object.
