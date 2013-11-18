%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(uses_2_test_object).

	:- public(p/1).
	p(1).

	:- public(q/1).
	q(2).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/05/16,
		comment is 'Unit tests for the uses/2 built-in directive.'
	]).

	:- uses(uses_2_test_object, [p/1, q/1::r/1]).

	test(uses_2_1) :-
		p(X),
		X == 1.

	test(uses_2_2) :-
		r(X),
		X == 2.

:- end_object.
