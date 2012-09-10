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
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/03,
		comment is 'Unit tests for the "points" example.']).

	unit(point).
	unit(bounded_point).
	unit(history_point).
	unit(bounded_history_point).

	test(points_1) :-
		point::new(Point, [position-(1, 3)]),
		Point::(print, move(7, 4), print).

	test(points_2) :-
		bounded_point::new(Point, [position-(1, 3), bounds(x)-(0, 13), bounds(y)-(-7, 7)]),
		Point::(print, move(7, 4), print).

	test(points_3) :-
		history_point::new(Point, [position-(1, 3)]),
		Point::(print, move(7, 4), print).

	test(points_4) :-
		bounded_history_point::new(Point, [position-(1, 3), bounds(x)-(0, 13), bounds(y)-(-7, 7)]),
		Point::(print, move(7, 4), print).

	cleanup :-
		point::delete_all,
		bounded_point::delete_all,
		history_point::delete_all,
		bounded_history_point::delete_all.

:- end_object.
