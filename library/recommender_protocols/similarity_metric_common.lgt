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


:- category(similarity_metric_common).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Shared helper predicates for sparse-vector overlap, numeric scaling, normalization, and correlation, reused by the library similarity metric objects.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- protected(common_pairs/3).
	:- mode(common_pairs(+list(pair), +list(pair), -list(pair)), one).
	:- info(common_pairs/3, [
		comment is 'Collects the ``Value1-Value2`` pairs for the keys present in both sparse vectors. Assumes each vector declares at most one value per key.',
		argnames is ['Vector1', 'Vector2', 'Pairs']
	]).

	common_pairs(Vector1, Vector2, Pairs) :-
		findall(
			Value1-Value2,
			(	member(Key-Value1, Vector1),
				memberchk(Key-Value2, Vector2)
			),
			Pairs
		).

	:- protected(vector_norm/2).
	:- mode(vector_norm(+list(pair), -float), one).
	:- info(vector_norm/2, [
		comment is 'Computes the Euclidean norm of the values of a sparse Key-Value vector.',
		argnames is ['Vector', 'Norm']
	]).

	vector_norm(Vector, Norm) :-
		extract_values(Vector, Values),
		sum_of_squares_list(Values, SumSquares),
		Norm is sqrt(SumSquares).

	extract_values([], []).
	extract_values([_Key-Value| Vector], [Value| Values]) :-
		extract_values(Vector, Values).

	:- protected(scale_values/2).
	:- mode(scale_values(+list(number), -list(number)), one).
	:- info(scale_values/2, [
		comment is 'Scales numeric values by their maximum absolute magnitude, leaving empty and all-zero lists unchanged.',
		argnames is ['Values', 'Scaled']
	]).

	scale_values(Values, Scaled) :-
		max_absolute_value(Values, 0, Scale),
		(	Scale =:= 0 ->
			Scaled = Values
		;	divide_values(Values, Scale, Scaled)
		).

	max_absolute_value([], Scale, Scale).
	max_absolute_value([Value| Values], Scale0, Scale) :-
		Magnitude is abs(Value),
		(	Magnitude > Scale0 ->
			Scale1 = Magnitude
		;	Scale1 = Scale0
		),
		max_absolute_value(Values, Scale1, Scale).

	divide_values([], _Divisor, []).
	divide_values([Value| Values], Divisor, [Scaled| ScaledValues]) :-
		Scaled is Value / Divisor,
		divide_values(Values, Divisor, ScaledValues).

	:- protected(normalize_values/2).
	:- mode(normalize_values(+list(number), -list(number)), one).
	:- info(normalize_values/2, [
		comment is 'Unit-normalizes numeric values using bounded intermediate magnitudes, leaving empty and all-zero lists unchanged.',
		argnames is ['Values', 'Normalized']
	]).

	normalize_values(Values, Normalized) :-
		scale_values(Values, Scaled),
		sum_of_squares_list(Scaled, SumSquares),
		(	SumSquares =:= 0 ->
			Normalized = Scaled
		;	Norm is sqrt(SumSquares),
			divide_values(Scaled, Norm, Normalized)
		).

	:- protected(normalize_vector/2).
	:- mode(normalize_vector(+list(pair), -list(pair)), one).
	:- info(normalize_vector/2, [
		comment is 'Unit-normalizes a sparse numeric vector while preserving its keys and their order.',
		argnames is ['Vector', 'Normalized']
	]).

	normalize_vector(Vector, Normalized) :-
		extract_values(Vector, Values),
		normalize_values(Values, NormalizedValues),
		vector_with_values(Vector, NormalizedValues, Normalized).

	vector_with_values([], [], []).
	vector_with_values([Key-_Value| Vector], [Value| Values], [Key-Value| Normalized]) :-
		vector_with_values(Vector, Values, Normalized).

	:- protected(bounded_similarity/2).
	:- mode(bounded_similarity(+number, -number), one).
	:- info(bounded_similarity/2, [
		comment is 'Bounds a computed cosine or correlation score to its mathematical range, removing endpoint roundoff.',
		argnames is ['Score', 'Similarity']
	]).

	bounded_similarity(Score, Similarity) :-
		(	Score > 1.0 ->
			Similarity = 1.0
		;	(	Score < -1.0 ->
				Similarity = -1.0
			;	Similarity = Score
			)
		).

	:- protected(centered_normalized_values/2).
	:- mode(centered_normalized_values(+list(number), -list(number)), one).
	:- info(centered_normalized_values/2, [
		comment is 'Centers and unit-normalizes numeric values in shifted, scaled coordinates. Empty lists remain empty and exactly constant lists become zeros.',
		argnames is ['Values', 'Normalized']
	]).

	centered_normalized_values([], []).
	centered_normalized_values([First| Values], Normalized) :-
		value_bounds(Values, First, First, Min, Max),
		(	Min =:= Max ->
			center([First| Values], First, Normalized)
		;	(	(Min >= 0 ; Max =< 0) ->
				Reference = First
			;	Reference = 0
			),
			center([First| Values], Reference, Shifted),
			scale_values(Shifted, Scaled),
			mean_values(Scaled, Mean),
			center(Scaled, Mean, Centered),
			normalize_values(Centered, Normalized)
		).

	value_bounds([], Min, Max, Min, Max).
	value_bounds([Value| Values], Min0, Max0, Min, Max) :-
		(	Value < Min0 ->
			Min1 = Value
		;	Min1 = Min0
		),
		(	Value > Max0 ->
			Max1 = Value
		;	Max1 = Max0
		),
		value_bounds(Values, Min1, Max1, Min, Max).

	:- protected(split_pairs/3).
	:- mode(split_pairs(+list(pair), -list(number), -list(number)), one).
	:- info(split_pairs/3, [
		comment is 'Splits a list of ``Value1-Value2`` pairs into separate, position-aligned lists of first and second values.',
		argnames is ['Pairs', 'Values1', 'Values2']
	]).

	split_pairs([], [], []).
	split_pairs([Value1-Value2| Pairs], [Value1| Values1], [Value2| Values2]) :-
		split_pairs(Pairs, Values1, Values2).

	:- protected(dot_product/3).
	:- mode(dot_product(+list(number), +list(number), -number), one).
	:- info(dot_product/3, [
		comment is 'Computes the dot product of two position-aligned numeric lists of the same length.',
		argnames is ['Values1', 'Values2', 'Dot']
	]).

	dot_product([], [], 0.0).
	dot_product([Value1| Values1], [Value2| Values2], Dot) :-
		dot_product(Values1, Values2, Dot0),
		Dot is Dot0 + Value1 * Value2.

	:- protected(sum_of_squares_list/2).
	:- mode(sum_of_squares_list(+list(number), -number), one).
	:- info(sum_of_squares_list/2, [
		comment is 'Computes the sum of the squares of a numeric list.',
		argnames is ['Values', 'SumSquares']
	]).

	sum_of_squares_list([], 0.0).
	sum_of_squares_list([Value| Values], SumSquares) :-
		sum_of_squares_list(Values, SumSquares0),
		SumSquares is SumSquares0 + Value * Value.

	:- protected(mean_values/2).
	:- mode(mean_values(+list(number), -float), one_or_error).
	:- info(mean_values/2, [
		comment is 'Computes the arithmetic mean of a non-empty numeric list.',
		argnames is ['Values', 'Mean'],
		exceptions is [
			'``Values`` is empty' - evaluation_error(zero_divisor)
		]
	]).

	mean_values(Values, Mean) :-
		sum(Values, Sum),
		length(Values, Count),
		(	Count =:= 0 ->
			evaluation_error(zero_divisor)
		;	Mean is Sum / Count
		).

	:- protected(center/3).
	:- mode(center(+list(number), +number, -list(number)), one).
	:- info(center/3, [
		comment is 'Subtracts a value (typically the mean) from every element of a numeric list.',
		argnames is ['Values', 'Value', 'Centered']
	]).

	center([], _Value, []).
	center([Value0| Values], Value, [Centered| CenteredValues]) :-
		Centered is Value0 - Value,
		center(Values, Value, CenteredValues).

:- end_category.
