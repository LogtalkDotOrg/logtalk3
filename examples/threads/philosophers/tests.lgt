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
		author is 'Paulo Moura',
		date is 2012/08/06,
		comment is 'Unit tests for the "threads/philosophers" example.'
	]).

	:- threaded.

	unit(chopstick).
	unit(philosopher).

	unit(philosopher(_, _, _)).

	test(philosophers_1) :-
		threaded_ignore(p1::run(5, 5)),
		threaded_ignore(p2::run(5, 5)),
		threaded_ignore(p3::run(5, 5)),
		threaded_ignore(p4::run(5, 5)),
		threaded_ignore(p5::run(5, 5)).

	test(philosophers_2) :-
		threaded_ignore(philosopher(p1,cs1,cs2)::run(5, 5)),
		threaded_ignore(philosopher(p2,cs2,cs3)::run(5, 5)),
		threaded_ignore(philosopher(p3,cs3,cs4)::run(5, 5)),
		threaded_ignore(philosopher(p4,cs4,cs5)::run(5, 5)),
		threaded_ignore(philosopher(p5,cs1,cs5)::run(5, 5)).

:- end_object.
