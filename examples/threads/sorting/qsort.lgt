%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(qsort(_Threads)).

	:- info([
		version is 1.21,
		author is 'Paul Crocker and Paulo Moura',
		date is 2007/12/27,
		comment is 'Multi-threaded version of the quick sort algorithm.',
		parameters is ['Threads' - 'Number of threads to use in sorting. Valid values are 1, 2, 4, 8, 16, 32, etc.']
	]).

	:- threaded.

	:- public(qsort/2).
	:- mode(qsort(+list, -list), one).
	:- info(qsort/2, [
		comment is 'Sorts a list of terms into ascending order.',
		argnames is ['List', 'Sorted']
	]).

	qsort(List, Sorted) :-
		parameter(1, Threads),
		Threads > 0,
		qsort(List, [], Sorted, Threads).

	qsort([], Sorted, Sorted, _).
	qsort([Pivot| Rest], Acc, Sorted, Threads) :-
		(	Threads =:= 1 ->
			quicksort([Pivot| Rest], Acc, Sorted)
		;	Threads2 is Threads//2,
			partition(Rest, Pivot, Smaller0, Bigger0),
			threaded((
				qsort(Smaller0, [Pivot| Bigger], Sorted, Threads2),
				qsort(Bigger0, Acc, Bigger, Threads2)
			))
		).

	partition([], _, [], []).
	partition([X| Xs], Pivot, Smalls, Bigs) :-
		(	X < Pivot ->
			Smalls = [X| Rest],
			partition(Xs, Pivot, Rest, Bigs)
		;	Bigs = [X| Rest],
			partition(Xs, Pivot, Smalls, Rest)
		).

	quicksort([], Sorted, Sorted).
	quicksort([Pivot| Rest], Acc, Sorted) :- 
		partition(Rest, Pivot, Smaller0, Bigger0),
		quicksort(Smaller0, [Pivot| Bigger], Sorted),
		quicksort(Bigger0, Acc, Bigger).

:- end_object.
