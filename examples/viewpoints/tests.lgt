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
		comment is 'Unit tests for the "viewpoints" example.'
	]).

	cover(joe_person).
	cover(joe_sportsman).
	cover(joe_chess_player).
	cover(joe_film_enthusiast).
	cover(joe_employee).

	test(viewpoints_1) :-
		joe_person::age(Age),
		Age == 30.

	test(viewpoints_2) :-
		joe_sportsman::age(Age),
		Age == 30.

	test(viewpoints_3) :-
		joe_person::grow_older,
		joe_chess_player::age(Age),
		Age == 31.

	test(viewpoints_4) :-
		joe_employee::grow_older,
		joe_person::age(Age),
		Age == 32.

	test(viewpoints_5) :-
		joe_person::score(Score),
		Score == 0.

	test(viewpoints_6) :-
		joe_employee::score(Score),
		Score == 0.

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(viewpoints_7) :-
		joe_chess_player::set_score(2200), joe_chess_player::score(Score),
		Score == 2200.

	test(viewpoints_8) :-
		joe_person::score(Score),
		Score == 0.

	test(viewpoints_9) :-
		joe_sportsman::score(Score),
		Score == 0.

:- end_object.
