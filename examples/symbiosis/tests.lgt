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
		date is 2013/03/16,
		comment is 'Unit tests for the "symbiosis" example.'
	]).

	test(symbiosis_1) :-
		symbiosis::p.

	test(symbiosis_2) :-
		symbiosis::q(L),
		L == [97, 98, 99].

	test(symbiosis_3) :-
		symbiosis::r(L),
		L == [1, 2, 3].

	test(symbiosis_4) :-
		symbiosis::s(L),
		L == [2,3,4].

	test(symbiosis_5) :-
		symbiosis::t(L),
		L == [2,3,4].

:- end_object.
