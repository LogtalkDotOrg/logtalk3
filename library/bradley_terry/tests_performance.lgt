%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(sparse_cycle_256,
	implements(pairwise_ranking_dataset_protocol)).

	item(Item) :-
		item_(1, 256, Item).

	preference(Item, Next, Weight) :-
		item(Item),
		next_item(Item, 256, Next),
		forward_weight(Item, Weight).
	preference(Next, Item, Weight) :-
		item(Item),
		next_item(Item, 256, Next),
		reverse_weight(Item, Weight).

	item_(Item, _Last, Item).
	item_(Current, Last, Item) :-
		Current < Last,
		Next is Current + 1,
		item_(Next, Last, Item).

	next_item(Item, Last, Next) :-
		(   Item < Last ->
			Next is Item + 1
		;   Next = 1
		).

	forward_weight(Item, Weight) :-
		Weight is 2 + Item mod 3.

	reverse_weight(Item, Weight) :-
		Weight is 1 + Item mod 2.

:- end_object.


:- object(tests_performance,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-26,
		comment is 'Performance and sparse-graph regression benchmarks for the "bradley_terry" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2,
		memberchk/2
	]).

	test(sparse_cycle_256_reference_fit, true, [note(metrics(train_seconds-TrainTime, items-Count, iterations-Iterations, strength_sum-TotalStrength))]) :-
		benchmark(bradley_terry::learn(sparse_cycle_256, _), TrainTime),
		bradley_terry::learn(sparse_cycle_256, Ranker),
		bradley_terry::scores(Ranker, Strengths),
		length(Strengths, Count),
		Count == 256,
		sum_strengths(Strengths, 0.0, TotalStrength),
		abs(TotalStrength - 1.0) =< 1.0e-9,
		memberchk(1-FirstStrength, Strengths),
		memberchk(256-LastStrength, Strengths),
		FirstStrength > 0.0,
		LastStrength > 0.0,
		bradley_terry::diagnostics(Ranker, Diagnostics),
		memberchk(convergence(converged), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics).

	sum_strengths([], TotalStrength, TotalStrength).
	sum_strengths([_-Strength| Strengths], TotalStrength0, TotalStrength) :-
		TotalStrength1 is TotalStrength0 + Strength,
		sum_strengths(Strengths, TotalStrength1, TotalStrength).

:- end_object.
