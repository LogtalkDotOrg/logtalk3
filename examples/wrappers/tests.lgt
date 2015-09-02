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
		date is 2015/09/02,
		comment is 'Unit tests for the "wrappers" example.'
	]).

	test(wrappers_1) :-
		zipper<<(zipper(3, [1,2,3,4,5], Zip, X), next(Zip, Next)),
		Zip == zip([2, 1], 3, [4, 5]),
		X == 3,
		Next == zip([3, 2, 1], 4, [5]).

	test(wrappers_2) :-
		zipper<<(zipper(3, [1,2,3,4,5], Zip, X), next(Zip, Next), previous(Next, Zip)),
		Zip == zip([2, 1], 3, [4, 5]),
		X == 3,
		Next == zip([3, 2, 1], 4, [5]).

	test(wrappers_3) :-
		zipper<<(zipper(3, [1,2,3,4,5], Zip, X), previous(Zip, Previous)),
		Zip == zip([2, 1], 3, [4, 5]),
		X == 3,
		Previous == zip([1], 2, [3, 4, 5]).

	test(wrappers_4) :-
		zipper<<(zipper(3, [1,2,3,4,5], Three, X), next(Three, Four), next(Four, Five), previous(Five, Four), previous(Four, Three), previous(Three, Two), previous(Two, One)),
		Three == zip([2, 1], 3, [4, 5]),
		X == 3,
		Four == zip([3, 2, 1], 4, [5]),
		Five == zip([4, 3, 2, 1], 5, []),
		Two == zip([1], 2, [3, 4, 5]),
		One == zip([], 1, [2, 3, 4, 5]).

:- end_object.
