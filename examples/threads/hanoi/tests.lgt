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
		version is 1.01,
		author is 'Paulo Moura',
		date is 2014/05/12,
		comment is 'Unit tests for the "threads/hanoi" example.'
	]).

	cover(hanoi(_)).

	test(hanoi_1) :-
		hanoi(1)::run(12).

	test(hanoi_2) :-
		hanoi(2)::run(12).

	test(hanoi_3) :-
		hanoi(4)::run(12).

	test(hanoi_4) :-
		hanoi(8)::run(12).

	test(hanoi_5) :-
		hanoi(16)::run(12).

:- end_object.
