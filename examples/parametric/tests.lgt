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
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "parametric" example.'
	]).

	unit(.(_, _)).
	unit([]).
	unit(date(_, _, _)).
	unit(time(_, _, _)).
	unit(rectangle(_, _, _, _)).
	unit(person(_, _)).
	unit(employee(_, _, _)).
	unit(dress(_)).
	unit(speech(_)).
	unit(speech(_, _)).

	test(parametric_1) :-
		findall(X, [1, 2, 3]::member(X), Solutions),
		Solutions == [1, 2, 3].

	test(parametric_2) :-
		findall(X, [1, 2, 3]::last(X), Solutions),
		Solutions == [3].

	test(parametric_3) :-
		findall(X, [1, 2, 3]::nextto(2,X), Solutions),
		Solutions == [3].

	test(parametric_4) :-
		\+ []::member(_).

	test(parametric_5) :-
		rectangle(W, H, X, Y)::init, rectangle(W, H, X, Y)::move(3, 4, NR), NR::position(X2, Y2),
		W  == 2,
		H  == 1,
		X  == 0,
		Y  == 0,
		NR == rectangle(2, 1, 3, 4),
		X2 == 3,
		Y2 == 4.

	test(parametric_6) :-
		person(sally, 20)::grow_older(NewId),
		NewId == person(sally, 21).

	test(parametric_7) :-
		employee(sally, 21, 1200)::give_raise(250, NewId),
		NewId == employee(sally, 21, 1450).

:- end_object.
