%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(msort(_Threads)).

	:- info([
		version is 1.32,
		author is 'Paulo Moura and Paul Crocker',
		date is 2011/03/28,
		comment is 'Multi-threaded implementation of the merge sort algorithm.',
		parameters is ['Threads'- 'Number of threads to use in sorting. Valid values are 1, 2, 4, 8, etc.']]).

	:- threaded.

	:- public(msort/2).
	:- mode(msort(@list(number), -list(number)), one).
	:- info(msort/2, [
		comment is 'Sorts a list of terms into ascending order.',
		argnames is ['List', 'Sorted']]).

	msort(List, Sorted) :-
		parameter(1, Threads),
		Threads > 0,
		mt_msort(Threads, List, Sorted).

	mt_msort(1, List, Sorted) :-
		!,
		st_msort(List, Sorted).
	mt_msort(Threads, List, Sorted) :-
		Threads2 is Threads//2,
		split(List, List1, List2),
		threaded((
			mt_msort(Threads2, List1, Sorted1),
			mt_msort(Threads2, List2, Sorted2)
		)),
		merge(Sorted1, Sorted2, Sorted).

	st_msort([], []) :- !.
	st_msort([X], [X]) :- !.
	st_msort([X, Y| Xs], Ys) :-
		split([X, Y| Xs], X1s, X2s),
		st_msort(X1s, Y1s),
		st_msort(X2s, Y2s),
		merge(Y1s, Y2s, Ys).

	split([], [], []).
	split([X| Xs], [X| Ys], Zs) :-
		split(Xs, Zs, Ys).

	merge([X| Xs], [Y| Ys], [X| Zs]) :-
		X =< Y, !,
		merge(Xs, [Y| Ys], Zs).
	merge([X| Xs], [Y| Ys], [Y| Zs]) :-
		X > Y, !,
		merge([X | Xs], Ys, Zs).
	merge([], Xs, Xs) :- !.
	merge(Xs, [], Xs).

:- end_object.

