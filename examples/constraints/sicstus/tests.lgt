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
		date is 2012/11/26,
		comment is 'Unit tests for the "constraints/sicstus" example.']).

	test(sicstus_clpfd_1) :-
		findall(X, cars_ix::cars_ix([ff], X), Xs),
		Xs == [
			[1,2,6,3,5,4,4,5,3,6],
			[1,3,6,2,5,4,3,5,4,6],
			[1,3,6,2,6,4,5,3,4,5],
			[5,4,3,5,4,6,2,6,3,1],
			[6,3,5,4,4,5,3,6,2,1],
			[6,4,5,3,4,5,2,6,3,1]
		].

	test(sicstus_clpfd_2) :-
		findall(X, cars_ix::cars_ix2([ff], X), Xs),
		Xs == [
			[1,2,6,3,5,4,4,5,3,6],
			[1,3,6,2,5,4,3,5,4,6],
			[1,3,6,2,6,4,5,3,4,5],
			[5,4,3,5,4,6,2,6,3,1],
			[6,3,5,4,4,5,3,6,2,1],
			[6,4,5,3,4,5,2,6,3,1]
		].

	test(sicstus_clpfd_3) :-
		findall(X, cars_ix::cars_ix3([ff], X), Xs),
		Xs == [
			[1,2,6,3,5,4,4,5,3,6],
			[1,3,6,2,5,4,3,5,4,6],
			[1,3,6,2,6,4,5,3,4,5],
			[5,4,3,5,4,6,2,6,3,1],
			[6,3,5,4,4,5,3,6,2,1],
			[6,4,5,3,4,5,2,6,3,1]
		].

:- end_object.
