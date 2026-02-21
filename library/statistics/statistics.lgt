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


:- category(statistics,
	implements(statisticsp)).

	:- info([
		version is 1:8:0,
		author is 'Paulo Moura',
		date is 2026-02-20,
		comment is 'Statistical calculations over a list of numbers.'
	]).

	:- private(arithmetic_mean/5).
	:- mode(arithmetic_mean(+list(number), +integer, -integer, +number, -float), one).
	:- info(arithmetic_mean/5, [
		comment is 'Auxiliary predicate for computing the arithmetic mean.',
		argnames is ['List', 'Length0', 'Length', 'Sum', 'Mean']
	]).

	:- private(squares_and_cubes/6).
	:- mode(squares_and_cubes(+list(number), +float, +float, -float, +float, -float), one).
	:- info(squares_and_cubes/6, [
		comment is 'Auxiliary predicate for computing the skewness.',
		argnames is ['List', 'Mean', 'Squares0', 'Squares', 'Cubes0', 'Cubes']
	]).

	:- private(squares_and_hypers/6).
	:- mode(squares_and_hypers(+list(number), +float, +float, -float, +float, -float), one).
	:- info(squares_and_hypers/6, [
		comment is 'Auxiliary predicate for computing the kurtosis.',
		argnames is ['List', 'Mean', 'Squares0', 'Squares', 'Hypers0', 'Hypers']
	]).

	:- private(variance/6).
	:- mode(variance(+list(number), +integer, -integer, +float, +float, -float), one).
	:- info(variance/6, [
		comment is 'Auxiliary predicate for computing the variance.',
		argnames is ['List', 'Length0', 'Length', 'Mean', 'M20', 'M2']
	]).

	:- private(cross_deviation_sum/6).
	:- mode(cross_deviation_sum(+list(number), +list(number), +float, +float, +float, -float), one).
	:- info(cross_deviation_sum/6, [
		comment is 'Auxiliary predicate for computing the cross-deviation sum for covariance.',
		argnames is ['List1', 'List2', 'Mean1', 'Mean2', 'Sum0', 'Sum']
	]).

	:- private(sorted_median/3).
	:- mode(sorted_median(+list(number), +integer, -number), one).
	:- info(sorted_median/3, [
		comment is 'Auxiliary predicate for computing the median of an already sorted list with known length.',
		argnames is ['Sorted', 'Length', 'Median']
	]).

	:- private(sum_of_squares/4).
	:- mode(sum_of_squares(+list(number), +float, +float, -float), one).
	:- info(sum_of_squares/4, [
		comment is 'Auxiliary predicate for computing the sum of squared deviations.',
		argnames is ['List', 'Mean', 'Sum0', 'Sum']
	]).

	:- private(central_moment_sum/7).
	:- mode(central_moment_sum(+list(number), +positive_integer, +float, +integer, +float, -integer, -float), one).
	:- info(central_moment_sum/7, [
		comment is 'Auxiliary predicate for computing central moments.',
		argnames is ['List', 'K', 'Mean', 'N0', 'Sum0', 'N', 'Sum']
	]).

	:- private(min_max_normalize/4).
	:- mode(min_max_normalize(+list(number), +number, +number, -list(float)), one).
	:- info(min_max_normalize/4, [
		comment is 'Auxiliary predicate for min-max normalization.',
		argnames is ['List', 'Min', 'Range', 'NormalizedList']
	]).

	:- private(count_frequencies/4).
	:- mode(count_frequencies(+list(number), +number, +integer, -list(pair(number,integer))), one).
	:- info(count_frequencies/4, [
		comment is 'Auxiliary predicate for computing frequency distribution.',
		argnames is ['List', 'CurrentValue', 'CurrentCount', 'Distribution']
	]).

	:- private(squared_error_sum/6).
	:- mode(squared_error_sum(+list(number), +list(number), +integer, +float, -integer, -float), one).
	:- info(squared_error_sum/6, [
		comment is 'Auxiliary predicate for computing mean squared error.',
		argnames is ['List1', 'List2', 'N0', 'Sum0', 'N', 'Sum']
	]).

	:- private(compute_ranks/3).
	:- mode(compute_ranks(+list(number), +list(number), -list(float)), one).
	:- info(compute_ranks/3, [
		comment is 'Auxiliary predicate for computing ranks of elements in a list.',
		argnames is ['List', 'All', 'Ranks']
	]).

	:- private(count_less_equal/6).
	:- mode(count_less_equal(+list(number), +number, +integer, -integer, +integer, -integer), one).
	:- info(count_less_equal/6, [
		comment is 'Auxiliary predicate for counting elements less than and equal to a value.',
		argnames is ['List', 'Value', 'Less0', 'Less', 'Equal0', 'Equal']
	]).

	:- uses(list, [
		drop/3, nth1/3, take/3
	]).

	:- uses(numberlist, [
		min/2, max/2, min_max/3, modes/2, product/2, sum/2 as sum_list/2
	]).

	arithmetic_mean([X| Xs], Mean) :-
		arithmetic_mean(Xs, 1, _, X, Mean).

	arithmetic_mean([], Length, Length, Sum, Mean) :-
		Mean is float(Sum / Length).
	arithmetic_mean([X| Xs], Length0, Length, Sum0, Mean) :-
		Length1 is Length0 + 1,
		Sum1 is Sum0 + X,
		arithmetic_mean(Xs, Length1, Length, Sum1, Mean).

	geometric_mean([X| Xs], Mean) :-
		geometric_mean(Xs, 1, X, Mean).

	geometric_mean([], Length, Product, Mean) :-
		Mean is float(Product ** (1.0 / Length)).
	geometric_mean([X| Xs], Length0, Pacc, Mean) :-
		Length1 is Length0 + 1,
		Pacc2 is Pacc * X,
		geometric_mean(Xs, Length1, Pacc2, Mean).

	harmonic_mean([X| Xs], Mean) :-
		Sum is 1.0 / X,
		harmonic_mean(Xs, 1, Sum, Mean).

	harmonic_mean([], Length, Sum, Mean) :-
		Mean is float(Length / Sum).
	harmonic_mean([X| Xs], Length0, Sum0, Mean) :-
		Length1 is Length0 + 1,
		Sum1 is Sum0 + 1.0 / X,
		harmonic_mean(Xs, Length1, Sum1, Mean).

	weighted_mean([W| Ws], [X| Xs], Mean) :-
		weighted_mean(Ws, W, SumWs, Xs, X * W, SumXs),
		Mean is SumXs / SumWs.

	weighted_mean([], SumWs, SumWs, [], SumXs, SumXs).
	weighted_mean([W| Ws], SumWs0, SumWs, [X| Xs], SumXs0, SumXs) :-
		SumWs1 is SumWs0 + W,
		SumXs1 is SumXs0 + X * W,
		weighted_mean(Ws, SumWs1, SumWs, Xs, SumXs1, SumXs).

	mean_deviation([X| Xs], Deviation) :-
		arithmetic_mean(Xs, 1, Length, X, Mean),
		average_deviation([X| Xs], Mean, 0, Length, 0, Sum),
		Deviation is float(Sum / Length).

	median_deviation([X| Xs], Deviation) :-
		median([X| Xs], Median, Length),
		average_deviation([X| Xs], Median, 0, Length, 0, Sum),
		Deviation is float(Sum / Length).

	average_deviation([X| Xs], CentralTendency, Deviation) :-
		average_deviation([X| Xs], CentralTendency, 0, Length, 0, Sum),
		Deviation is float(Sum / Length).

	average_deviation([], _, Length, Length, Sum, Sum).
	average_deviation([X| Xs], CentralTendency, Length0, Length, Sum0, Sum) :-
		Length1 is Length0 + 1,
		Sum1 is Sum0 + abs(X - CentralTendency),
		average_deviation(Xs, CentralTendency, Length1, Length, Sum1, Sum).

	coefficient_of_variation([X| Xs], Coefficient) :-
		::standard_deviation([X| Xs], Deviation),
		arithmetic_mean([X| Xs], Mean),
		Coefficient is float(Deviation / Mean).

	relative_standard_deviation(Xs, Percentage) :-
		coefficient_of_variation(Xs, Coefficient),
		Percentage is float(Coefficient * 100).

	variance([], Length, Length, _, M2, M2).
	variance([X| Xs], Length0, Length, Mean, M2acc, M2) :-
		Length1 is Length0 + 1,
		Delta is X - Mean,
		Mean2 is Mean + Delta/Length1,
		M2acc2 is M2acc + Delta * (X - Mean2),
		variance(Xs, Length1, Length, Mean2, M2acc2, M2).

	squares_and_cubes([], _, Squares, Squares, Cubes, Cubes).
	squares_and_cubes([X| Xs], Mean, Squares0, Squares, Cubes0, Cubes) :-
		Squares1 is Squares0 + (X - Mean) ** 2,
		Cubes1 is Cubes0 + (X - Mean) ** 3,
		squares_and_cubes(Xs, Mean, Squares1, Squares, Cubes1, Cubes).

	squares_and_hypers([], _, Squares, Squares, Hypers, Hypers).
	squares_and_hypers([X| Xs], Mean, Squares0, Squares, Hypers0, Hypers) :-
		Squares1 is Squares0 + (X - Mean) ** 2,
		Hypers1 is Hypers0 + (X - Mean) ** 4,
		squares_and_hypers(Xs, Mean, Squares1, Squares, Hypers1, Hypers).

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
	quicksort([Pivot| List], Aux, Sorted, Length0, Length) :-
		partition(List, Pivot, Small, Large),
		Length1 is Length0 + 1,
		quicksort(Large, Aux, Sorted1, Length1, Length2),
		quicksort(Small, [Pivot| Sorted1], Sorted, Length2, Length).

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

	range(Xs, Range) :-
		min_max(Xs, Min, Max),
		Range is Max - Min.

	z_normalization(Xs, Ys) :-
		arithmetic_mean(Xs, Mean),
		::standard_deviation(Xs, Deviation),
		z_normalization(Xs, Mean, Deviation, Ys).

	z_normalization([], _, _, []).
	z_normalization([X| Xs], Mean, Deviation, [Y| Ys]) :-
		Y is float((X - Mean) / Deviation),
		z_normalization(Xs, Mean, Deviation, Ys).

	sum([X| Xs], Sum) :-
		sum_list([X| Xs], Sum).

	fractile(P, [X| Xs], Fractile) :-
		0.0 < P, P < 1.0,
		quicksort([X| Xs], [], Sorted, 0, Length),
		Index is ceiling(P * Length),
		nth1(Index, Sorted, Fractile).

	percentile(P, List, Percentile) :-
		0 < P, P < 100,
		Fraction is P / 100.0,
		fractile(Fraction, List, Percentile).

	quartiles([X| Xs], Q1, Q2, Q3) :-
		quicksort([X| Xs], [], Sorted, 0, Length),
		Length >= 2,
		sorted_median(Sorted, Length, Q2),
		Half is Length // 2,
		sorted_median(Sorted, Half, Q1),
		(	Length mod 2 =:= 1 ->
			Skip is Half + 1
		;	Skip is Half
		),
		drop(Skip, Sorted, Upper),
		UpperLength is Length - Skip,
		sorted_median(Upper, UpperLength, Q3).

	interquartile_range(List, IQR) :-
		quartiles(List, Q1, _, Q3),
		IQR is Q3 - Q1.

	correlation(Xs1, Xs2, Correlation) :-
		::covariance(Xs1, Xs2, Covariance),
		::standard_deviation(Xs1, SD1),
		::standard_deviation(Xs2, SD2),
		Correlation is float(Covariance / (SD1 * SD2)).

	rank_correlation(Xs, Ys, Correlation) :-
		compute_ranks(Xs, Xs, RanksX),
		compute_ranks(Ys, Ys, RanksY),
		::correlation(RanksX, RanksY, Correlation).

	trimmed_mean(Fraction, [X| Xs], Mean) :-
		0.0 =< Fraction, Fraction < 0.5,
		quicksort([X| Xs], [], Sorted, 0, Length),
		Trim is truncate(Fraction * Length),
		TrimmedLength is Length - 2 * Trim,
		TrimmedLength > 0,
		drop(Trim, Sorted, Temp),
		take(TrimmedLength, Temp, Trimmed),
		sum_list(Trimmed, Sum),
		Mean is float(Sum / TrimmedLength).

	sum_of_squares([X| Xs], SumOfSquares) :-
		arithmetic_mean([X| Xs], Mean),
		sum_of_squares([X| Xs], Mean, 0, SumOfSquares).

	central_moment(K, [X| Xs], Moment) :-
		K > 0,
		arithmetic_mean([X| Xs], Mean),
		central_moment_sum([X| Xs], K, Mean, 0, 0, N, Sum),
		Moment is float(Sum / N).

	min_max_normalization([X| Xs], Normalized) :-
		min_max([X| Xs], Min, Max),
		Range is Max - Min,
		Range > 0,
		min_max_normalize([X| Xs], Min, Range, Normalized).

	frequency_distribution([X| Xs], Distribution) :-
		quicksort([X| Xs], [], [First| Rest], 0, _),
		count_frequencies(Rest, First, 1, Distribution).

	standard_error([X| Xs], Error) :-
		arithmetic_mean(Xs, 1, N, X, _),
		::standard_deviation([X| Xs], Deviation),
		Error is float(Deviation / sqrt(N)).

	mean_squared_error([X| Xs], [Y| Ys], MSE) :-
		squared_error_sum([X| Xs], [Y| Ys], 0, 0, N, Sum),
		MSE is float(Sum / N).

	root_mean_squared_error(Xs, Ys, RMSE) :-
		mean_squared_error(Xs, Ys, MSE),
		RMSE is float(sqrt(MSE)).

	% auxiliary predicates for new functions

	sorted_median(Sorted, Length, Median) :-
		Length > 0,
		(	Length mod 2 =:= 1 ->
			Middle is Length // 2 + 1,
			middle_element(1, Middle, Sorted, Median)
		;	Left is Length // 2,
			middle_elements(1, Left, Sorted, XLeft, XRight),
			Median is XLeft + (XRight - XLeft) / 2
		).

	sum_of_squares([], _, Sum, Sum).
	sum_of_squares([X| Xs], Mean, Sum0, Sum) :-
		Sum1 is Sum0 + (X - Mean) ** 2,
		sum_of_squares(Xs, Mean, Sum1, Sum).

	central_moment_sum([], _, _, N, Sum, N, Sum).
	central_moment_sum([X| Xs], K, Mean, N0, Sum0, N, Sum) :-
		N1 is N0 + 1,
		Sum1 is Sum0 + (X - Mean) ** K,
		central_moment_sum(Xs, K, Mean, N1, Sum1, N, Sum).

	min_max_normalize([], _, _, []).
	min_max_normalize([X| Xs], Min, Range, [Y| Ys]) :-
		Y is float((X - Min) / Range),
		min_max_normalize(Xs, Min, Range, Ys).

	count_frequencies([], Value, Count, [Value-Count]).
	count_frequencies([X| Xs], Current, Count, Distribution) :-
		(	X =:= Current ->
			Count1 is Count + 1,
			count_frequencies(Xs, Current, Count1, Distribution)
		;	Distribution = [Current-Count| Rest],
			count_frequencies(Xs, X, 1, Rest)
		).

	squared_error_sum([], [], N, Sum, N, Sum).
	squared_error_sum([X| Xs], [Y| Ys], N0, Sum0, N, Sum) :-
		N1 is N0 + 1,
		Sum1 is Sum0 + (X - Y) ** 2,
		squared_error_sum(Xs, Ys, N1, Sum1, N, Sum).

	cross_deviation_sum([], [], _, _, Sum, Sum).
	cross_deviation_sum([X| Xs], [Y| Ys], MeanX, MeanY, Sum0, Sum) :-
		Sum1 is Sum0 + (X - MeanX) * (Y - MeanY),
		cross_deviation_sum(Xs, Ys, MeanX, MeanY, Sum1, Sum).

	compute_ranks([], _, []).
	compute_ranks([X| Xs], All, [Rank| Ranks]) :-
		count_less_equal(All, X, 0, Less, 0, Equal),
		Rank is Less + (Equal + 1) / 2.0,
		compute_ranks(Xs, All, Ranks).

	count_less_equal([], _, Less, Less, Equal, Equal).
	count_less_equal([Y| Ys], X, Less0, Less, Equal0, Equal) :-
		(	Y < X ->
			Less1 is Less0 + 1,
			count_less_equal(Ys, X, Less1, Less, Equal0, Equal)
		;	Y =:= X ->
			Equal1 is Equal0 + 1,
			count_less_equal(Ys, X, Less0, Less, Equal1, Equal)
		;	count_less_equal(Ys, X, Less0, Less, Equal0, Equal)
		).

	valid((-)) :-		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([Element| List]) :-
		number(Element),
		valid(List).

:- end_category.
