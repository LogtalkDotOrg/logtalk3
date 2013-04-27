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
		version is 1.21,
		author is 'Parker Jones and Paulo Moura',
		date is 2013/04/27,
		comment is 'Unit tests for the "tabling" example.'
	]).
	
	test(tabling_1) :-
		setof(Y, paths::path(1, Y), Ys),
		Ys = [2, 3, 4, 5].
	
	test(tabling_2) :-
		fibonacci::fib(30, F),
		F == 1346269.

	unit(fibonacci).
	unit(paths).

	:- if(current_logtalk_flag(prolog_dialect, yap)).

	unit(mdt_paths_first).
	unit(mdt_paths_min).
	unit(mdt_paths_min_all).

	test(tabling_3) :-
		setof((Z, N), mdt_paths_first::path(a, Z, N), L),
		L == [(a, 2), (b, 1)].

	test(tabling_4) :-
		setof((Z, C), mdt_paths_min::path(a, Z, C), L),
		L == [(b, 1), (c, 2), (d, 3)].

	test(tabling_5) :-
		setof((Z, C, N), mdt_paths_min_all::path(a, Z, C, N), L),
		L == [(b, 2, 1), (b, 2, 2), (c, 1, 1)].

	:- endif.

:- end_object.
