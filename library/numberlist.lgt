%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.7,
		author is 'Paulo Moura',
		date is 2017/10/03,
		comment is 'List of numbers predicates.',
		see_also is [list, list(_), varlist, difflist]
	]).

	average([], 0.0).
	average([N| Ns], Average) :-
		average(Ns, 1, N, Average).

	average([], Length, Sum, Average) :-
		Average is Sum / Length.
	average([N| Ns], Lacc, Sacc, Average) :-
		Lacc2 is Lacc + 1,
		Sacc2 is Sacc + N,
		average(Ns, Lacc2, Sacc2, Average).

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
		Y is (X - Min) / Range,
		normalize_range_2(Xs, Min, Range, Ys).

	normalize_range([], _, _, []).
	normalize_range([X| Xs], NewMin, NewMax, [Y| Ys]) :-
		min_max_list([X| Xs], Min, Max),
		Range is Max - Min,
		NewRange is NewMax - NewMin,
		normalize_range_4([X| Xs], Min, Max, Range, NewMin, NewRange, [Y| Ys]).

	normalize_range_4([], _, _, _, _, _, []).
	normalize_range_4([X| Xs], Min, Max, Range, NewMin, NewRange, [Y| Ys]) :-
		Y is (X - Min) * NewRange / Range + NewMin,
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
		Factor is 1 / Norm,
		rescale([X| Xs], Factor, [Y| Ys]).

	normalize_scalar([], []).
	normalize_scalar([X| Xs], [Y| Ys]) :-
		sum([X| Xs], Sum),
		Factor is 1 / Sum,
		rescale([X| Xs], Factor, [Y| Ys]).

	rescale([], _, []).
	rescale([X| Xs], Factor, [Y| Ys]) :-
		Y is X * Factor,
		rescale(Xs, Factor, Ys).

	valid((-)) :-		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([Element| List]) :-
		number(Element),
		valid(List).

	check(Term) :-
		context(Context),
		(	valid(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, Context))
		;	throw(error(type_error(numberlist, Term), Context))
		).

:- end_object.
