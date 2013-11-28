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
		date is 2013/11/28,
		comment is 'Unit tests for the "assertions" tool.'
	]).

	:- uses(assertions, [
		assertion/1,
		assertion/2
	]).

	cover(assertions).

	% assertion/1 tests

	test(assertions_1) :-
		assertion(ground(x)),
		2 is 1 + 1.

	test(assertions_2) :-
		assertion(22 is 2 + 2),
		2 is 1 + 1.

	test(assertions_3) :-
		assertion(1),
		2 is 1 + 1.

	% assertion/2 tests

	test(assertions_4) :-
		assertion(assertions_4, ground(x)),
		2 is 1 + 1.

	test(assertions_5) :-
		assertion(assertions_5, 22 is 2 + 2),
		2 is 1 + 1.

	test(assertions_6) :-
		assertion(assertions_6, 1),
		2 is 1 + 1.

:- end_object.
