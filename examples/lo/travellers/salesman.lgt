%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(salesman).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		source is 'Example adapted from the Francis G. McCabe L&O documentation.'
	]).

	:- public(route/2).
	:- mode(route(+list, -nonvar), zero_or_more).

:- end_object.



:- object(circular,
	extends(salesman)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		source is 'Example adapted from the Francis G. McCabe L&O documentation.'
	]).

	route([Town| Towns], Route) :-
		route(Towns, Town-Town, Route).

	route([], Route, Route).
	route([Town| Towns], Route, Route2) :-
		best_place(Route, Town, Best),
		split(Best, Route, Town, Split),
		route(Towns, Split, Route2).

	best_place(Route, Town, Best) :-
		best_place(Route, Town, 10000000, 0, 0, Best).

	best_place(R-T1-T2, Town, XD, _, I, Best) :-
		atom(T1),
		atom(T2),
		extra(T1, Town, T2, XT),
		XT < XD,
		I2 is I + 1,
		best_place(R-T1, Town, XT, I2, I2, Best).
	best_place(R-T1-T2, Town, XD, XI, I, Best) :-
		atom(T1),
		atom(T2),
		I2 is I + 1,
		best_place(R-T1, Town, XD, XI, I2, Best).
	best_place(T1-T2, Town, XD, _, I, Best) :-
		atom(T1),
		atom(T2),
		extra(T1, Town, T2, XT),
		XT < XD,
		Best is I + 1.
	best_place(_-_, _, _, XI, _, XI).

	split(0, Route, Town, Route-Town).
	split(IX, Route-Town1, Town, Split-Town1) :-
		IX2 is IX - 1,
		split(IX2, Route, Town, Split).
	split(1, Route, Town, Town-Route).

	extra(T1, T, T2, E) :-
		T1::crow_flies(T, E1),
		T::crow_flies(T2, E2),
		T1::crow_flies(T2, E3),
		E is E1 + E2 - E3.

:- end_object.



:- object(incremental,
	extends(salesman)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		source is 'Example adapted from the Francis G. McCabe L&O documentation.'
	]).

	route([Town| Towns], Route) :-
  		route(Towns, Town, Route).

	route([], Route, Route).
	route([Town| Towns], Route, Route2) :-
		best_place(Route, Town, Best),
		split(Best, Route, Town, NewR),
		route(Towns, NewR, Route2).

	best_place(Route-Town1, Town, Best) :-  % try the back first ...
		atom(Town1),
		Town::crow_flies(Town1, Distance),
		best_place(Route-Town1, Town, Distance, 0, 0, Best).
	best_place(Town, _, 0) :-
		atom(Town).

	best_place(R-T1-T2, T, XD, _, I, Best) :-
		atom(T1),
		atom(T2),
		extra(T1, T, T2, XT),
		XT < XD,
		I2 is I + 1,
		best_place(R-T1, T, XT, I2, I2, Best).
	best_place(R-T1-T2, T, XD, XI, I, Best) :-
		atom(T1),
		atom(T2),
		I2 is I + 1,
		best_place(R-T1, T, XD, XI, I2, Best).
	best_place(T1-T2, T, XD, _, I, Best) :-
		atom(T1),
		atom(T2),
		extra(T1, T, T2, XT),
		XT < XD,
		I2 is I + 1,
		best_place(T1, T, XT, I2, I2, Best).
	best_place(T1-T2, T, XD, XI, I, Best) :-
		atom(T1),
		atom(T2),
		I2 is I + 1,
		best_place(T1, T, XD, XI, I2, Best).
	best_place(T1, T, XD, _, I, Best) :-
		atom(T1),
		T1::crow_flies(T, Distance),
		Distance < XD,
		Best is I + 1.
	best_place(_, _, _, XI, _, XI).

	split(0, Route, Town, Route-Town).
	split(IX, Route-Town1, Town, S-Town1) :-
		IX2 is IX - 1,
		split(IX2, Route, Town, S).
	split(1, Route, Town, Town-Route).

	extra(T1, T, T2, XT) :-
		T1::crow_flies(T, Distance1),
		T::crow_flies(T2, Distance2),
		T1::crow_flies(T2, Distance3),
		XT is Distance1 + Distance2 - Distance3.

:- end_object.



:- object(presort,
	extends(incremental)).

	:- info([
		author is 'Paulo Moura',
		version is 1.1,
		date is 2013/04/23,
		comment is '.',
		source is 'Example adapted from the Francis G. McCabe L&O documentation.'
	]).

	route(Towns, Route) :-
		arrange(Towns, Towns2),
		^^route(Towns2, Route).

	arrange(Towns, Sorted) :-
		centre(Towns, X, Y),
    	quick(geographic(X, Y))::sort(Towns, Sorted).

	centre(Towns, X, Y) :-
		average(Towns, 0, 0, U, V, 0, L),
		X is U/L,
		Y is V/L.

	average([], U, V, U, V, L, L).
	average([Town| Towns], UX, VX, U, V, I, L):-
		Town::at(UT, VT),
		UX2 is UX+UT,
		VX2 is VX+VT,
		I2 is I + 1,
		average(Towns, UX2, VX2, U, V, I2, L).

:- end_object.



:- object(driving,
	extends(salesman)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		source is 'Example adapted from the Francis G. McCabe L&O documentation.'
	]).

	route(Towns, Route) :-
		presort::route(Towns, Presort), 
		drive_around(Presort, Route).

	drive_around(Route-Town1-Town2, Route1-Route2) :-
		!,
		drive_around(Route-Town1, Route1),
		Town1::drive(Town2, Route2).
	drive_around(Town1-Town2, Route) :-
		!,
		Town1::drive(Town2, Route).
	drive_around(Town, Town).

	drive_length(Route, Length) :-
		drive_length(Route, 0, Length).

	drive_length(Route-Town1-Town2, Acc, Length) :-
		!,
		Town1::road_distance(Town2, Length2),
		Acc2 is Acc + Length2,
		drive_length(Route-Town1, Acc2, Length).
	drive_length(Town1-Town2, Acc, Length) :-
		!,
		Town1::road_distance(Town2, Length2),
		Length is Acc + Length2.
	drive_length(_, Length, Length).

:- end_object.



:- object(permute,
	extends(salesman)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is '.',
		source is 'Example adapted from the Francis G. McCabe L&O documentation.'
	]).

	route(Towns, Route) :-
		findall(
			(Towns2, Length),
			(permute(Towns, Towns2), route_length(Towns2, Length)),
			List),
		shortest(List, Route).

	permute([Town], Town).
	permute(Towns, Towns2-Town) :-
		delete(Towns, Town, Towns3),
		permute(Towns3, Towns2).

	delete([Head| Tail], Head, Tail).
	delete([Head| Tail], Element, [Head| Tail2]):-
		delete(Tail, Element, Tail2).

	route_length(Town, 0) :-
		atom(Town), !.
	route_length(Towns-Town1-Town2, Length) :-
		!,
		route_length(Towns-Town1, Length1),
		Town1::crow_flies(Town2, Length2),
		Length is Length1 + Length2.
	route_length(Town1-Town2, Length) :-
		Town1::crow_flies(Town2, Length).

	shortest(List, Shortest) :-
		shortest(List, null, 1000000, Shortest).

	shortest([], Route, Length, (Route, Length)).
	shortest([(Route, Length)| Routes], _, LX, Shortest) :-
		Length < LX, !,
		shortest(Routes, Route, Length, Shortest).
	shortest([_| Routes], RX, LX, Shortest) :-
		shortest(Routes, RX, LX, Shortest).

:- end_object.
