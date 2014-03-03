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
		date is 2012/08/13,
		comment is 'Unit tests for the "threads/sorting" example.'
	]).

	cover(generator).
	cover(msort(_)).
	cover(qsort(_)).

	test(sorting_1) :-
		generator::list(20000, List),
		msort(1)::msort(List, _Sorted).

	test(sorting_2) :-
		generator::list(20000, List),
		msort(2)::msort(List, _Sorted).

	test(sorting_3) :-
		generator::list(20000, List),
		msort(4)::msort(List, _Sorted).

	test(sorting_4) :-
		generator::list(20000, List),
		qsort(1)::qsort(List, _Sorted).

	test(sorting_5) :-
		generator::list(20000, List),
		qsort(2)::qsort(List, _Sorted).

	test(sorting_6) :-
		generator::list(20000, List),
		qsort(4)::qsort(List, _Sorted).

:- end_object.
