%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.11,
		author is 'Parker Jones and Paulo Moura',
		date is 2011/09/20,
		comment is 'Unit tests for the "shapes_ch" example.'
	]).

	throws(shapes_ch_1, error(existence_error(predicate_declaration,nsides/1), logtalk(_,_))) :-
		square::nsides(_).

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(shapes_ch_2) :-
		q1::color(Color), q1::side(Side), q1::position(X, Y),
		Color == red, Side == 1, X == 0, Y == 0.

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(shapes_ch_3) :-
		q2::side(Side), q2::area(Area), q2::perimeter(Perimeter),
		Side == 3, Area == 9, Perimeter == 12.

:- end_object.
