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
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/03,
		comment is 'Unit tests for the "viewpoints" example.']).

	unit(joePerson).
	unit(joeSportsman).
	unit(joeChessPlayer).
	unit(joeFilmEnthusiast).
	unit(joeEmployee).

	test(viewpoints_1) :-
		joePerson::age(Age),
		Age == 30.

	test(viewpoints_2) :-
		joeSportsman::age(Age),
		Age == 30.

	test(viewpoints_3) :-
		joePerson::getOlder,
		joeChessPlayer::age(Age),
		Age == 31.

	test(viewpoints_4) :-
		joeEmployee::getOlder,
		joePerson::age(Age),
		Age == 32.

	test(viewpoints_5) :-
		joePerson::score(Score),
		Score == 0.

	test(viewpoints_6) :-
		joeEmployee::score(Score),
		Score == 0.

	% don't use message broadcasting syntax in order to workaround a XSB parser bug
	test(viewpoints_7) :-
		joeChessPlayer::setScore(2200), joeChessPlayer::score(Score),
		Score == 2200.

	test(viewpoints_8) :-
		joePerson::score(Score),
		Score == 0.

	test(viewpoints_9) :-
		joeSportsman::score(Score),
		Score == 0.

:- end_object.
