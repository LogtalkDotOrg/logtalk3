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



:- object(numberlist,
	implements(numberlistp),
	extends(list)).

	:- info([
		version is 1.5,
		author is 'Paulo Moura',
		date is 2011/12/15,
		comment is 'List of numbers predicates.'
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
	min([X| Xs], Aux, Min) :-
		(	X < Aux ->
			min(Xs, X, Min)
		;	min(Xs, Aux, Min)
		).

	max([X| Xs], Max) :-
		max(Xs, X, Max).

	max([], Max, Max).
	max([X| Xs], Aux, Max) :-
		(	X > Aux ->
			max(Xs, X, Max)
		;	max(Xs, Aux, Max)
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

	valid((-)) :-		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([Element| List]) :-
		number(Element),
		valid(List).

	check(Term) :-
		this(This),
		sender(Sender),
		(	valid(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
