%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(numberlist,
	implements(numberlistp),
	extends(list)).

	:- info([
		version is 1:16:0,
		author is 'Paulo Moura',
		date is 2025-03-13,
		comment is 'List of numbers predicates.',
		see_also is [list, list(_), varlist, difflist]
	]).

	:- uses(list, [
		msort/2, sort/2, sort/4
	]).

	average([N| Ns], Average) :-
		average(Ns, 1, N, Average).

	average([], Length, Sum, Average) :-
		Average is Sum / Length.
	average([N| Ns], Lacc, Sacc, Average) :-
		Lacc2 is Lacc + 1,
		Sacc2 is Sacc + N,
		average(Ns, Lacc2, Sacc2, Average).

	median([X| Xs], Median) :-
		median([X| Xs], Median, _).

	median([X| Xs], Median, Length) :-
		quicksort([X| Xs], [], Sorted, 0, Length),
		(	Length mod 2 =:= 1 ->
			Middle is Length // 2 + 1,
			middle_element(1, Middle, Sorted, Median)
		;	Left is Length // 2,
			middle_elements(1, Left, Sorted, XLeft, XRight),
			Median is XLeft + (XRight - XLeft) / 2
		).

	quicksort([], Sorted, Sorted, Length, Length).
	quicksort([Pivot| List], Aux, Sorted, Acc, Length) :-
		partition(List, Pivot, Small, Large),
		Acc2 is Acc + 1,
		quicksort(Large, Aux, Sorted1, Acc2, Acc3),
		quicksort(Small, [Pivot| Sorted1], Sorted, Acc3, Length).

	partition([], _, [], []).
	partition([X| Xs], Pivot, Small, Large) :-
		(	X < Pivot ->
			Small = [X| Small1], Large = Large1
		;	Small = Small1, Large = [X| Large1]
		),
		partition(Xs, Pivot, Small1, Large1).

	middle_element(N, N, [X| _], Middle) :-
		!,
		Middle = X.
	middle_element(M, N, [_| Xs], X) :-
		M2 is M + 1,
		middle_element(M2, N, Xs, X).

	middle_elements(N, N, [X1, X2| _], MiddleLeft, MiddleRight) :-
		!,
		MiddleLeft = X1,
		MiddleRight = X2.
	middle_elements(M, N, [_| Xs], X1, X2) :-
		M2 is M + 1,
		middle_elements(M2, N, Xs, X1, X2).

	modes([X| Xs], Modes) :-
		msort([X| Xs], [Y| Ys]),
		count_occurrences(Ys, Y, 1, Occurrences),
		sort(1, (@>=), Occurrences, [K-M| Sorted]),
		occurrences_modes(Sorted, K, M, Modes0),
		sort(Modes0, Modes).

	count_occurrences([], X, N, [N-X]).
	count_occurrences([X| Xs], X, N, Occurrences) :-
		!,
		M is N + 1,
		count_occurrences(Xs, X, M, Occurrences).
	count_occurrences([Y| Ys], X, N, [N-X| Occurrences]) :-
		count_occurrences(Ys, Y, 1, Occurrences).

	occurrences_modes([], _, Mode, [Mode]).
	occurrences_modes([K-Mode1| Ks], K, Mode0, [Mode0| Modes]) :-
		!,
		occurrences_modes(Ks, K, Mode1, Modes).
	occurrences_modes([_| _], _, Mode, [Mode]).

	min([X| Xs], Min) :-
		min(Xs, X, Min).

	min([], Min, Min).
	min([X| Xs], Min0, Min) :-
		(	X < Min0 ->
			min(Xs, X, Min)
		;	min(Xs, Min0, Min)
		).

	max([X| Xs], Max) :-
		max(Xs, X, Max).

	max([], Max, Max).
	max([X| Xs], Max0, Max) :-
		(	X > Max0 ->
			max(Xs, X, Max)
		;	max(Xs, Max0, Max)
		).

	min_max([X| Xs], Min, Max) :-
		min_max(Xs, X, Min, X, Max).

	min_max([], Min, Min, Max, Max).
	min_max([X| Xs], Min0, Min, Max0, Max) :-
		Min1 is min(X, Min0),
		Max1 is max(X, Max0),
		min_max(Xs, Min1, Min, Max1, Max).

	product([X| Xs], Product) :-
		product(Xs, X, Product).

	product([], Product, Product).
	product([X| Xs], Acc, Product) :-
		Acc2 is Acc * X,
		product(Xs, Acc2, Product).

	sum(List, Sum) :-
		sum(List, 0, Sum).

	sum([], Sum, Sum).
	sum([X| Xs], Acc, Sum) :-
		Acc2 is Acc + X,
		sum(Xs, Acc2, Sum).

	euclidean_norm([X| Xs], Norm) :-
		SquareSum0 is X * X,
		euclidean_norm(Xs, SquareSum0, Norm).

	euclidean_norm([], SquareSum, Norm) :-
		Norm is sqrt(SquareSum).
	euclidean_norm([X| Xs], SquareSum0, Norm) :-
		SquareSum1 is SquareSum0 + X * X,
		euclidean_norm(Xs, SquareSum1, Norm).

	chebyshev_norm([X| Xs], Norm) :-
		Norm0 is abs(X),
		chebyshev_norm(Xs, Norm0, Norm).

	chebyshev_norm([], Norm, Norm).
	chebyshev_norm([X| Xs], Norm0, Norm) :-
		(	abs(X) > Norm0 ->
			Norm1 is abs(X)
		;	Norm1 is Norm0
		),
		chebyshev_norm(Xs, Norm1, Norm).

	manhattan_norm([X| Xs], Norm) :-
		Norm0 is abs(X),
		manhattan_norm(Xs, Norm0, Norm).

	manhattan_norm([], Norm, Norm).
	manhattan_norm([X| Xs], Norm0, Norm) :-
		Norm1 is Norm0 + abs(X),
		manhattan_norm(Xs, Norm1, Norm).

	euclidean_distance([X| Xs], [Y| Ys], Distance) :-
		Distance0 is (X - Y) * (X - Y),
		euclidean_distance(Xs, Ys, Distance0, Distance).

	euclidean_distance([], [], Sum, Distance) :-
		Distance is sqrt(Sum).
	euclidean_distance([X| Xs], [Y| Ys], Sum0, Distance) :-
		Sum1 is Sum0 + (X - Y) * (X - Y),
		euclidean_distance(Xs, Ys, Sum1, Distance).

	chebyshev_distance([X| Xs], [Y| Ys], Distance) :-
		Distance0 is abs(X - Y),
		chebyshev_distance(Xs, Ys, Distance0, Distance).

	chebyshev_distance([], [], Distance, Distance).
	chebyshev_distance([X| Xs], [Y| Ys], Distance0, Distance) :-
		(	abs(X - Y) > Distance0 ->
			Distance1 is abs(X - Y)
		;	Distance1 is Distance0
		),
		chebyshev_distance(Xs, Ys, Distance1, Distance).

	manhattan_distance([X| Xs], [Y| Ys], Distance) :-
		Distance0 is abs(X - Y),
		manhattan_distance(Xs, Ys, Distance0, Distance).

	manhattan_distance([], [], Distance, Distance).
	manhattan_distance([X| Xs], [Y| Ys], Distance0, Distance) :-
		Distance1 is Distance0 + abs(X - Y),
		manhattan_distance(Xs, Ys, Distance1, Distance).

	scalar_product([X| Xs], [Y| Ys], Product) :-
		Product0 is X * Y,
		scalar_product(Xs, Ys, Product0, Product).

	scalar_product([], [], Product, Product).
	scalar_product([X| Xs], [Y| Ys], Product0, Product) :-
		Product1 is Product0 + X * Y,
		scalar_product(Xs, Ys, Product1, Product).

	normalize_range([], []).
	normalize_range([X| Xs], [Y| Ys]) :-
		min_max_list([X| Xs], Min, Max),
		Range is Max - Min,
		normalize_range_2([X| Xs], Min, Range, [Y| Ys]).

	normalize_range_2([], _, _, []).
	normalize_range_2([X| Xs], Min, Range, [Y| Ys]) :-
		Y is float((X - Min) / Range),
		normalize_range_2(Xs, Min, Range, Ys).

	normalize_range([], _, _, []).
	normalize_range([X| Xs], NewMin, NewMax, [Y| Ys]) :-
		min_max_list([X| Xs], Min, Max),
		Range is Max - Min,
		NewRange is NewMax - NewMin,
		normalize_range_4([X| Xs], Min, Max, Range, NewMin, NewRange, [Y| Ys]).

	normalize_range_4([], _, _, _, _, _, []).
	normalize_range_4([X| Xs], Min, Max, Range, NewMin, NewRange, [Y| Ys]) :-
		Y is float((X - Min) * NewRange / Range + NewMin),
		normalize_range_4(Xs, Min, Max, Range, NewMin, NewRange, Ys).

	min_max_list([X| Xs], Min, Max) :-
		min_max_list(Xs, X, Min, X, Max).

	min_max_list([], Min, Min, Max, Max).
	min_max_list([X| Xs], Min0, Min, Max0, Max) :-
		(	X < Min0 ->
			min_max_list(Xs, X, Min, Max0, Max)
		;	X > Max0 ->
			min_max_list(Xs, Min0, Min, X, Max)
		;	min_max_list(Xs, Min0, Min, Max0, Max)
		).

	normalize_unit([], []).
	normalize_unit([X| Xs], [Y| Ys]) :-
		euclidean_norm([X| Xs], Norm),
		% as some Prolog backends may return 1/Norm as a rational number,
		% we explicitly convert the term into a float in the next goal
		Factor is float(1 / Norm),
		rescale([X| Xs], Factor, [Y| Ys]).

	normalize_scalar([], []).
	normalize_scalar([X| Xs], [Y| Ys]) :-
		sum([X| Xs], Sum),
		% as some Prolog backends may return 1/Norm as a rational number,
		% we explicitly convert the term into a float in the next goal
		Factor is float(1 / Sum),
		rescale([X| Xs], Factor, [Y| Ys]).

	rescale([], _, []).
	rescale([X| Xs], Factor, [Y| Ys]) :-
		Y is X * Factor,
		rescale(Xs, Factor, Ys).

	softmax(Xs, Ys) :-
		softmax(Xs, 1.0, Ys).

	softmax(Xs, T, Ys) :-
		T > 0.0,
		softmax_exps_sum(Xs, T, Es, 0, Sum),
		softmax_exps_softmax(Es, Sum, Ys).

	softmax_exps_sum([], _, [], Sum, Sum).
	softmax_exps_sum([X| Xs], T, [E| Es], Sum0, Sum) :-
		E is exp(X / T),
		Sum1 is Sum0 + E,
		softmax_exps_sum(Xs, T, Es, Sum1, Sum).

	softmax_exps_softmax([], _, []).
	softmax_exps_softmax([E| Es], Sum, [Y| Ys]) :-
		Y is E / Sum,
		softmax_exps_softmax(Es, Sum, Ys).

	:- if(current_logtalk_flag(prolog_dialect, xsb)).

		:- set_logtalk_flag(left_recursion, silent).

		least_common_multiple([I1, I2| Is], Multiple) :-
			I1 > 0,
			I2 > 0,
			gcd(I1, I2, GCD),
			Multiple0 is (I1 * I2) // GCD,
			least_common_multiple(Is, Multiple0, Multiple).

		least_common_multiple([], Multiple, Multiple).
		least_common_multiple([I| Is], Multiple0, Multiple) :-
			I > 0,
			gcd(Multiple0, I, GCD),
			Multiple1 is (Multiple0 * I) // GCD,
			least_common_multiple(Is, Multiple1, Multiple).

		gcd(X, 0, X) :- !.
		gcd(0, X, X) :- !.
		gcd(X, Y, D) :- X =< Y, !, Z is Y - X, gcd(X, Z, D).
		gcd(X, Y, D) :- gcd(Y, X, D).

	:- else.

		least_common_multiple([I1, I2| Is], Multiple) :-
			I1 > 0,
			I2 > 0,
			Multiple0 is (I1 * I2) // gcd(I1, I2),
			least_common_multiple(Is, Multiple0, Multiple).

		least_common_multiple([], Multiple, Multiple).
		least_common_multiple([I| Is], Multiple0, Multiple) :-
			I > 0,
			Multiple1 is (Multiple0 * I) // gcd(Multiple0, I),
			least_common_multiple(Is, Multiple1, Multiple).

	:- endif.

	valid((-)) :-
		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([Element| List]) :-
		number(Element),
		valid(List).

	check(Term) :-
		(	valid(Term) ->
			true
		;	var(Term) ->
			instantiation_error
		;	type_error(numberlist, Term)
		).

:- end_object.
