%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(sorting).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2011/03/22,
		comment is 'Experiment with co-inductive sorting of lists.'
	]).

	:- public(keysort/2).

	:- coinductive(sort/3).
	:- coinductive(partition/4).

	keysort(List, Sorted) :-
		sort(List, [], Sorted).

	sort([], Sorted, Sorted).
	sort([Pivot| Rest], Acc, Sorted) :- 
		partition(Rest, Pivot, Smaller0, Bigger0),
		sort(Smaller0, [Pivot| Bigger], Sorted),
		sort(Bigger0, Acc, Bigger).

	partition([], _, [], []).
	partition([Key-Value| Pairs], PivotKey-PivotValue, Smalls, Bigs) :-
		(	Key @< PivotKey ->
			Smalls = [Key-Value| Rest],
			partition(Pairs, PivotKey-PivotValue, Rest, Bigs)
		;	Key == PivotKey, Value @< PivotValue ->
			Smalls = [Key-Value| Rest],
			partition(Pairs, PivotKey-PivotValue, Rest, Bigs)
		;	Bigs = [Key-Value| Rest],
			partition(Pairs, PivotKey-PivotValue, Smalls, Rest)
		).

:- end_object.
