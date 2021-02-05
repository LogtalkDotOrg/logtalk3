%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(qsort(_Threads)).

	:- info([
		version is 1:21:0,
		author is 'Paul Crocker and Paulo Moura',
		date is 2007-12-27,
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
