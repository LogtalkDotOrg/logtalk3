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
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "threads/nondet" example.'
	]).

	:- threaded.

	test(nondet_01) :-
		threaded_call(lists::member(_, [1,2,3])).

	test(nondet_02) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs),
		Xs == [1, 2, 3].

	test(nondet_03) :-
		threaded_once(lists::member(_, [1,2,3])).

	test(nondet_04) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs),
		Xs == [1].

	test(nondet_05) :-
		threaded_call(lists::member(_, [1,2,3])),
		threaded_call(lists::member(_, [1,2,3])).

	test(nondet_06) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs),
		Xs == [1, 2, 3].

	test(nondet_07) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs),
		Xs == [1, 2, 3].

	test(nondet_08) :-
		threaded_call(lists::member(_, [1,2,3]), _),
		threaded_call(lists::member(_, [1,2,3]), Tag),
		findall(X, threaded_exit(lists::member(X, [1,2,3]), Tag), Xs),
		Xs == [1, 2, 3].

	test(nondet_09) :-
		threaded_call(lists::member(_, [1,2,3,2])).

	test(nondet_10) :-
		findall(1, threaded_exit(lists::member(2, [1,2,3,2])), L),
		L == [1, 1].

:- end_object.
