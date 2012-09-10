%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(location(_X, _Y)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		parnames is ['X', 'Y'],
		comment is '.',
		source is 'Example adapted from the Francis G. McCabe L&O documentation.']).

	:- public(at/2).
	:- mode(at(-integer, -integer), one).

	:- public(crow_flies/2).
	:- mode(crow_flies(+atom, -integer), one).

	:- public(drive/2).
	:- mode(drive(+atom, -nonvar), zero_or_more).

	:- public(links/1).
	:- mode(links(-list), one).

	:- public(road_distance/2).
	:- mode(road_distance(?atom, ?integer), zero_or_more).

	at(X, Y) :-
		parameter(1, X),
		parameter(2, Y).

	crow_flies(Town, Distance) :-
		::at(X, Y),
		Town::at(U, V),
		U0 is U-X,
		V0 is V-Y,
		Distance is sqrt(U0*U0+V0*V0).

	road_distance(Town, Distance) :-
		::links(Links),
		member((Town, Distance), Links).

	drive(To, Route) :-  % plan a road journey
		self(Self),
		plan_drive(Self, To, [], _, Route).

	% go directly
	plan_drive(From, To, _, Distance, From-To):-
		To::links(Links),
		member((From, Distance), Links).

	% go indirectly
	plan_drive(From, To, R, D+DI, Route-To):-
		To::links(Links),
		nearest(Links, From, Int, DI),
		\+ member(Int, R),
		plan_drive(From, Int, [To| R], D, Route).

	nearest(Links, To, Int, Distance):-
		quick(metric(To))::sort(Links, Sorted),
		member((Int, Distance), Sorted).

	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

:- end_object.



:- object(aberdeen,
	extends(location(194, 340))).

	links([
		(edinburgh, 115),
		(glasgow, 142)]).

:- end_object.


:- object(aberystwyth,
	extends(location(126, 102))).

	links([
		(birmingham, 114),
		(liverpool, 100),
		(swansea, 75)]).

:- end_object.


:- object(birmingham,
	extends(location(192, 106))).

	links([
		(aberystwyth, 114),
		(bristol, 86),
		(cambridge, 97),
		(liverpool, 99),
		(nottingham, 48),
		(oxford, 63),
		(sheffield, 75)]).

:- end_object.


:- object(brighton,
	extends(location(248, 10))).

	links([
		(dover, 81),
		(portsmouth, 49),
		(london, 52)]).

:- end_object.


:- object(bristol,
	extends(location(168, 47))).

	links([
		(cardiff, 44),
		(exeter, 76),
		(oxford, 71),
		(birmingham, 86)]).

:- end_object.


:- object(cambridge,
	extends(location(254, 92))).

    links([
    	(nottingham, 82),
		(oxford, 80),
		(london, 54),
		(birmingham, 97)]).

:- end_object.


:- object(cardiff,
	extends(location(148, 56))).

    links([
    	(bristol, 44),
		(swansea, 45)]).

:- end_object.


:- object(carlisle,
	extends(location(166, 226))).

	links([
		(glasgow, 94),
		(leeds, 117),
		(newcastle, 58)]).

:- end_object.


:- object(dover,
	extends(location(292, 38))).

	links([
		(brighton, 81),
		(london, 71)]).

:- end_object.


:- object(edinburgh,
	extends(location(162, 282))).

	links([
		(aberdeen, 115),
		(glasgow, 44),
		(newcastle, 104)]).

:- end_object.


:- object(exeter,
	extends(location(138, 18))).

	links([
		(bristol, 76),
		(penzance, 112),
		(portsmouth, 126)]).

:- end_object.


:- object(glasgow,
	extends(location(132, 273))).

	links([
		(aberdeen, 142),
		(carlisle, 94),
		(edinburgh, 44)]).

:- end_object.


:- object(hull,
	extends(location(240, 168))).

	links([
		(leeds, 58),
		(sheffield, 65),
		(york, 37)]).

:- end_object.


:- object(leeds,
	extends(location(208, 170))).

	links([
		(carlisle, 117),
		(hull, 58),
		(sheffield, 34),
		(manchester, 41),
		(york, 23)]).

:- end_object.


:- object(liverpool,
	extends(location(164, 150))).

	links([
		(aberystwyth, 100),
		(birmingham, 99),
		(manchester, 35),
		(sheffield, 70)]).

:- end_object.


:- object(london,
	extends(location(244,54))).

	links([
		(brighton, 52),
		(dover, 71),
		(cambridge, 54),
		(oxford, 57),
		(portsmouth, 72)]).

:- end_object.


:- object(manchester,
	extends(location(180, 156))).

    links([
    	(leeds, 41),
		(liverpool, 35),
		(sheffield, 38)]).

:- end_object.


:- object(newcastle,
	extends(location(210, 230))).

    links([
    	(edinburgh, 104),
		(carlisle, 58),
		(york, 80)]).

:- end_object.


:- object(nottingham,
	extends(location(216, 128))).

    links([
    	(birmingham, 48),
		(cambridge, 82),
		(sheffield, 38)]).

:- end_object.


:- object(oxford,
	extends(location(214, 66))).

    links([
    	(bristol, 71),
		(birmingham, 63),
		(cambridge, 80),
		(london, 57)]).

:- end_object.


:- object(penzance,
	extends(location(10, 0))).

    links([(
    	exeter, 112)]).

:- end_object.


:- object(portsmouth,
	extends(location(216, 22))).

    links([
    	(brighton, 49),
		(exeter, 126),
		(london, 72)]).

:- end_object.


:- object(sheffield,
	extends(location(208, 142))).

    links([
    	(birmingham, 75),
		(hull, 65),
		(leeds, 34),
		(liverpool, 70),
		(manchester, 38),
		(nottingham, 38)]).

:- end_object.


:- object(swansea,
	extends(location(126, 66))).

    links([
    	(cardiff, 45),
		(aberystwyth, 75)]).

:- end_object.


:- object(york,
	extends(location(218, 184))).

    links([
    	(leeds, 23),
		(hull, 37),
		(newcastle, 80)]).

:- end_object.
