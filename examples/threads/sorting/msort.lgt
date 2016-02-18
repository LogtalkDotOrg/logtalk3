%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(msort(_Threads)).

	:- info([
		version is 1.32,
		author is 'Paulo Moura and Paul Crocker',
		date is 2011/03/28,
		comment is 'Multi-threaded implementation of the merge sort algorithm.',
		parameters is ['Threads' - 'Number of threads to use in sorting. Valid values are 1, 2, 4, 8, etc.']
	]).

	:- threaded.

	:- public(msort/2).
	:- mode(msort(@list(number), -list(number)), one).
	:- info(msort/2, [
		comment is 'Sorts a list of terms into ascending order.',
		argnames is ['List', 'Sorted']
	]).

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

