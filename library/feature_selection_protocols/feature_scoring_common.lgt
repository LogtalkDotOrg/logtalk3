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


:- category(feature_scoring_common).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Shared arithmetic, discretization, and sparse contingency helpers for feature scoring.'
	]).

	:- uses(list, [
		length/2, last/2, msort/3
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- uses(type, [
		check/3
	]).

	:- protected(class_scatter/5).
	:- mode(class_scatter(+list(pair), -non_negative_integer, -non_negative_integer, -float, -float), one).
	:- info(class_scatter/5, [
		comment is 'Computes scaled between-class and within-class scatter from complete numeric-value and categorical-target pairs. Empty inputs have zero scatter.',
		argnames is ['Pairs', 'ObservationCount', 'ClassCount', 'Between', 'Within']
	]).

	class_scatter(CompletePairs, ObservationCount, ClassCount, Between, Within) :-
		split_pairs(CompletePairs, CompleteValues, CompleteTargets),
		scale_values(CompleteValues, Values),
		scatter_value_pairs(Values, CompleteTargets, Pairs),
		group_by_target(Pairs, Groups),
		length(Groups, ClassCount),
		length(Pairs, ObservationCount),
		(	ObservationCount =:= 0 ->
			Between = 0.0,
			Within = 0.0
		;	mean_values(Values, OverallMean),
			class_scatter_sums(Groups, OverallMean, 0.0, Between, 0.0, Within)
		).

	scatter_value_pairs([], [], []).
	scatter_value_pairs([Value| Values], [Target| Targets], [Value-Target| Pairs]) :-
		scatter_value_pairs(Values, Targets, Pairs).

	class_scatter_sums([], _OverallMean, Between, Between, Within, Within).
	class_scatter_sums([_Target-Values| Groups], OverallMean, Between0, Between, Within0, Within) :-
		length(Values, GroupSize),
		mean_values(Values, GroupMean),
		Deviation is GroupMean - OverallMean,
		Between1 is Between0 + GroupSize * Deviation * Deviation,
		sum_of_squared_deviations(Values, GroupMean, GroupWithin),
		Within1 is Within0 + GroupWithin,
		class_scatter_sums(Groups, OverallMean, Between1, Between, Within1, Within).

	:- protected(categorical_pairs/4).
	:- mode(categorical_pairs(+term, +list, +list, -list(pair)), one_or_error).
	:- info(categorical_pairs/4, [
		comment is 'Validates aligned scoring inputs and prepares complete categorical pairs, optionally discretizing numeric features.',
		argnames is ['Discretization', 'Values', 'Targets', 'Pairs'],
		exceptions is [
			'An input list or discretization parameter is unbound' - instantiation_error,
			'An input is not a list' - type_error(list, 'Input'),
			'A categorical value or target is not atomic' - type_error(atomic, 'Value'),
			'A continuous feature value is not numeric' - type_error(number, 'Value'),
			'A bin count is not an integer' - type_error(integer, 'Count'),
			'A bin count is not positive' - domain_error(positive_integer, 'Count'),
			'The discretization specification is invalid' - domain_error(discretization, 'Discretization'),
			'The input list lengths differ' - consistency_error(list_length, 'ValueCount', 'TargetCount')
		]
	]).

	categorical_pairs(Specification, Values, Targets, Pairs) :-
		check_discretization(Specification),
		context(Context),
		check(list, Values, Context),
		check(list, Targets, Context),
		length(Values, ValueCount),
		length(Targets, TargetCount),
		(	ValueCount =:= TargetCount ->
			true
		;	consistency_error(list_length, ValueCount, TargetCount)
		),
		complete_pairs(Values, Targets, Complete),
		check_categorical_pairs(Complete, Specification, Context),
		discretize_pairs(Specification, Complete, Pairs).

	check_discretization(Specification) :-
		(	\+ ground(Specification) ->
			instantiation_error
		;	Specification == categorical ->
			true
		;	(	(Specification = equal_width(Count); Specification = equal_frequency(Count)) ->
				context(Context),
				check(positive_integer, Count, Context)
			;	domain_error(discretization, Specification)
			)
		).

	check_categorical_pairs([], _Specification, _Context).
	check_categorical_pairs([Value-Target| Pairs], Specification, Context) :-
		check(atomic, Target, Context),
		(	Specification == categorical ->
			check(atomic, Value, Context)
		;	check(number, Value, Context)
		),
		check_categorical_pairs(Pairs, Specification, Context).

	discretize_pairs(categorical, Pairs, Pairs).
	discretize_pairs(equal_width(Count), Pairs, Binned) :-
		(	Pairs == [] ->
			Binned = []
		;	Pairs = [First-_| _],
			pair_extrema(Pairs, First, First, Minimum, Maximum),
			width_coordinates(Minimum, Maximum, Origin, Range, Scale),
			width_bins(Pairs, Count, Maximum, Origin, Range, Scale, Binned)
		).
	discretize_pairs(equal_frequency(Count), Pairs, Binned) :-
		(	Pairs == [] ->
			Binned = []
		;	number_pairs(Pairs, 0, Numbered),
			msort(compare_numeric_pairs, Numbered, Sorted),
			length(Sorted, Total),
			Bins is min(Count, Total),
			last(Sorted, Maximum-_),
			quantile_cuts(Sorted, 1, 1, Bins, Total, Maximum, Cuts0),
			unique_cuts(Cuts0, Cuts),
			quantile_bins(Sorted, Cuts, 0, Assigned),
			keysort(Assigned, Ordered),
			unnumber_pairs(Ordered, Binned)
		).

	pair_extrema([], Minimum, Maximum, Minimum, Maximum).
	pair_extrema([Value-_| Pairs], Minimum0, Maximum0, Minimum, Maximum) :-
		Minimum1 is min(Minimum0, Value),
		Maximum1 is max(Maximum0, Value),
		pair_extrema(Pairs, Minimum1, Maximum1, Minimum, Maximum).

	width_coordinates(Minimum, Maximum, Origin, Range, Scale) :-
		(	Minimum < 0,
			Maximum > 0 ->
			Scale is max(abs(Minimum), abs(Maximum)),
			Origin is Minimum / Scale,
			Range is Maximum / Scale - Origin
		;	Scale = 1,
			Origin = Minimum,
			Range is Maximum - Minimum
		).

	width_bins([], _Count, _Maximum, _Origin, _Range, _Scale, []).
	width_bins([Value-Target| Pairs], Count, Maximum, Origin, Range, Scale, [Bin-Target| Binned]) :-
		(	Range =:= 0 ->
			Bin = 0
		;	(	Value =:= Maximum ->
				Bin is Count - 1
			;	(	Scale =:= 1 ->
					Offset is Value - Origin
				;	Offset is Value / Scale - Origin
				),
				Fraction is Offset / Range,
				Bin is max(0, min(Count - 1, floor(Fraction * Count)))
			)
		),
		width_bins(Pairs, Count, Maximum, Origin, Range, Scale, Binned).

	number_pairs([], _Position, []).
	number_pairs([Value-Target| Pairs], Position, [Value-(Position-Target)| Numbered]) :-
		Next is Position + 1,
		number_pairs(Pairs, Next, Numbered).

	:- private(compare_numeric_pairs/3).
	:- mode(compare_numeric_pairs(-atom, +pair, +pair), one).
	:- info(compare_numeric_pairs/3, [
		comment is 'Compares numeric feature values arithmetically, keeping numerically equal representations together.',
		argnames is ['Order', 'Pair1', 'Pair2']
	]).

	compare_numeric_pairs(Order, Value1-_, Value2-_) :-
		(	Value1 < Value2 ->
			Order = (<)
		;	(	Value1 > Value2 ->
				Order = (>)
			;	Order = (=)
			)
		).

	quantile_cuts([], _Position, _Index, _Bins, _Total, _Maximum, []).
	quantile_cuts([Value-_| Sorted], Position, Index, Bins, Total, Maximum, Cuts) :-
		(	Index >= Bins ->
			Cuts = []
		;	Rank is (Index * Total + Bins - 1) // Bins,
			(	Position =:= Rank ->
				NextIndex is Index + 1,
				(	Value < Maximum ->
					Cuts = [Value| Rest]
				;	Cuts = Rest
				)
			;	NextIndex = Index,
				Cuts = Rest
			),
			NextPosition is Position + 1,
			quantile_cuts(Sorted, NextPosition, NextIndex, Bins, Total, Maximum, Rest)
		).

	unique_cuts([], []).
	unique_cuts([Cut| Cuts], [Cut| Unique]) :-
		skip_equal_cuts(Cuts, Cut, Rest),
		unique_cuts(Rest, Unique).

	skip_equal_cuts([], _Cut, []).
	skip_equal_cuts([Next| Cuts], Cut, Rest) :-
		(	Next =:= Cut ->
			skip_equal_cuts(Cuts, Cut, Rest)
		;	Rest = [Next| Cuts]
		).

	quantile_bins([], _Cuts, _Bin, []).
	quantile_bins([Value-(Position-Target)| Sorted], Cuts0, Bin0, [Position-(Bin-Target)| Assigned]) :-
		advance_cuts(Value, Cuts0, Bin0, Cuts, Bin),
		quantile_bins(Sorted, Cuts, Bin, Assigned).

	advance_cuts(_Value, [], Bin, [], Bin) :-
		!.
	advance_cuts(Value, [Cut| Cuts0], Bin0, Cuts, Bin) :-
		(	Value > Cut ->
			Next is Bin0 + 1,
			advance_cuts(Value, Cuts0, Next, Cuts, Bin)
		;	Cuts = [Cut| Cuts0],
			Bin = Bin0
		).

	unnumber_pairs([], []).
	unnumber_pairs([_Position-Pair| Ordered], [Pair| Pairs]) :-
		unnumber_pairs(Ordered, Pairs).

	:- protected(contingency_counts/2).
	:- mode(contingency_counts(+list(pair), -compound), one).
	:- info(contingency_counts/2, [
		comment is 'Builds sparse observed cell counts and marginal dictionaries from complete categorical pairs.',
		argnames is ['Pairs', 'Counts']
	]).

	contingency_counts(Pairs, contingency(Total, Rows, Columns, Cells)) :-
		avltree::new(Empty),
		count_pairs(Pairs, Empty, Rows, Empty, Columns, Empty, CellTree, 0, Total),
		avltree::as_list(CellTree, Cells).

	count_pairs([], Rows, Rows, Columns, Columns, Cells, Cells, Total, Total).
	count_pairs([Value-Target| Pairs], Rows0, Rows, Columns0, Columns, Cells0, Cells, Total0, Total) :-
		increment_count(Rows0, Value, Rows1),
		increment_count(Columns0, Target, Columns1),
		increment_count(Cells0, cell(Value, Target), Cells1),
		Total1 is Total0 + 1,
		count_pairs(Pairs, Rows1, Rows, Columns1, Columns, Cells1, Cells, Total1, Total).

	increment_count(Dictionary0, Key, Dictionary) :-
		(	avltree::lookup(Key, Previous, Dictionary0) ->
			Count is Previous + 1
		;	Count = 1
		),
		avltree::insert(Dictionary0, Key, Count, Dictionary).

	:- protected(contingency_score/3).
	:- mode(contingency_score(+atom, +compound, -float), one).
	:- info(contingency_score/3, [
		comment is 'Computes mutual information, Pearson chi-square, symmetrical uncertainty, or Cramer\'s V from sparse contingency counts. Degenerate tables score 0.0.',
		argnames is ['Criterion', 'Counts', 'Score']
	]).

	contingency_score(Criterion, contingency(Total, Rows, Columns, Cells), Score) :-
		avltree::size(Rows, RowCount),
		avltree::size(Columns, ColumnCount),
		(	(Total < 2; RowCount < 2; ColumnCount < 2) ->
			Score = 0.0
		;	contingency_statistic(Criterion, Cells, Total, Rows, Columns, Score)
		).

	contingency_statistic(mutual_information, Cells, Total, Rows, Columns, Score) :-
		mutual_information_cells(Cells, Total, Rows, Columns, 0.0, Information),
		Score is max(0.0, Information).
	contingency_statistic(chi_square, Cells, Total, Rows, Columns, Score) :-
		avltree::new(Empty),
		chi_square_cells(Cells, Total, Rows, Columns, Empty, Covered, 0.0, ObservedScore),
		avltree::as_list(Rows, RowCounts),
		chi_square_empty_cells(RowCounts, Covered, Total, ObservedScore, Score).
	contingency_statistic(symmetrical_uncertainty, Cells, Total, Rows, Columns, Score) :-
		contingency_statistic(mutual_information, Cells, Total, Rows, Columns, Information),
		avltree::as_list(Rows, RowCounts),
		avltree::as_list(Columns, ColumnCounts),
		marginal_entropy(RowCounts, Total, 0.0, RowEntropy),
		marginal_entropy(ColumnCounts, Total, 0.0, ColumnEntropy),
		Entropy is RowEntropy + ColumnEntropy,
		(	Entropy > 0.0 ->
			Score is min(1.0, max(0.0, 2 * (Information / Entropy)))
		;	Score = 0.0
		).
	contingency_statistic(cramers_v, Cells, Total, Rows, Columns, Score) :-
		contingency_statistic(chi_square, Cells, Total, Rows, Columns, ChiSquare),
		avltree::size(Rows, RowCount),
		avltree::size(Columns, ColumnCount),
		Dimension is min(RowCount - 1, ColumnCount - 1),
		Score is min(1.0, sqrt(max(0.0, (ChiSquare / Total) / Dimension))).

	marginal_entropy([], _Total, Entropy, Entropy).
	marginal_entropy([_Category-Count| Counts], Total, Entropy0, Entropy) :-
		Probability is Count / Total,
		Entropy1 is Entropy0 - Probability * (log(Probability) / log(2)),
		marginal_entropy(Counts, Total, Entropy1, Entropy).

	mutual_information_cells([], _Total, _Rows, _Columns, Information, Information).
	mutual_information_cells([cell(Value, Target)-Count| Cells], Total, Rows, Columns, Information0, Information) :-
		avltree::lookup(Value, RowCount, Rows),
		avltree::lookup(Target, ColumnCount, Columns),
		Ratio is (Count / RowCount) / (ColumnCount / Total),
		Information1 is Information0 + (Count / Total) * (log(Ratio) / log(2)),
		mutual_information_cells(Cells, Total, Rows, Columns, Information1, Information).

	chi_square_cells([], _Total, _Rows, _Columns, Covered, Covered, Score, Score).
	chi_square_cells([cell(Value, Target)-Count| Cells], Total, Rows, Columns, Covered0, Covered, Score0, Score) :-
		avltree::lookup(Value, RowCount, Rows),
		avltree::lookup(Target, ColumnCount, Columns),
		Expected is RowCount * (ColumnCount / Total),
		Deviation is Count - Expected,
		Score1 is Score0 + (Deviation / Expected) * Deviation,
		(	avltree::lookup(Value, Previous, Covered0) ->
			ColumnTotal is Previous + ColumnCount
		;	ColumnTotal = ColumnCount
		),
		avltree::insert(Covered0, Value, ColumnTotal, Covered1),
		chi_square_cells(Cells, Total, Rows, Columns, Covered1, Covered, Score1, Score).

	chi_square_empty_cells([], _Covered, _Total, Score, Score).
	chi_square_empty_cells([Value-RowCount| Rows], Covered, Total, Score0, Score) :-
		avltree::lookup(Value, ColumnTotal, Covered),
		Score1 is Score0 + RowCount * ((Total - ColumnTotal) / Total),
		chi_square_empty_cells(Rows, Covered, Total, Score1, Score).

	:- protected(complete_pairs/3).
	:- mode(complete_pairs(+list, +list, -list(pair)), one).
	:- info(complete_pairs/3, [
		comment is 'Collects the ``Value-Target`` pairs for which both the feature value and the target are bound, discarding any example where either is an unbound variable (casewise deletion). ``Values`` and ``Targets`` must have the same length.',
		argnames is ['Values', 'Targets', 'Pairs']
	]).

	complete_pairs([], [], []).
	complete_pairs([Value| Values], [Target| Targets], Pairs) :-
		(	nonvar(Value),
			nonvar(Target) ->
			Pairs = [Value-Target| Pairs0]
		;	Pairs = Pairs0
		),
		complete_pairs(Values, Targets, Pairs0).

	:- protected(complete_values/2).
	:- mode(complete_values(+list, -list(number)), one).
	:- info(complete_values/2, [
		comment is 'Collects the bound (non-missing) values of a list, discarding unbound ones.',
		argnames is ['Values', 'Complete']
	]).

	complete_values([], []).
	complete_values([Value| Values], Complete) :-
		(	nonvar(Value) ->
			Complete = [Value| Complete0]
		;	Complete = Complete0
		),
		complete_values(Values, Complete0).

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
		length(Values, Count),
		(	Count =:= 0 ->
			evaluation_error(zero_divisor)
		;	scale_values(Values, Scaled, Scale),
			sum(Scaled, Sum),
			Mean is min(1.0, max(-1.0, Sum / Count)) * Scale
		).

	:- protected(variance_values/2).
	:- mode(variance_values(+list(number), -float), one_or_error).
	:- info(variance_values/2, [
		comment is 'Computes the population variance of a non-empty numeric list.',
		argnames is ['Values', 'Variance'],
		exceptions is [
			'``Values`` is empty' - evaluation_error(zero_divisor)
		]
	]).

	variance_values(Values, Variance) :-
		(	Values == [] ->
			evaluation_error(zero_divisor)
		;	Values = [Reference| _]
		),
		center(Values, Reference, Offsets),
		scale_values(Offsets, Scaled, Scale),
		centered_values(Scaled, Centered),
		sum_of_squares_list(Centered, SumSquares),
		length(Values, Count),
		Variance is (SumSquares / Count * Scale) * Scale.

	:- protected(centered_values/2).
	:- mode(centered_values(+list(number), -list(number)), one_or_error).
	:- info(centered_values/2, [
		comment is 'Centers non-empty numeric values using offsets from the first value, preserving exact constant inputs.',
		argnames is ['Values', 'Centered'],
		exceptions is ['``Values`` is empty' - evaluation_error(zero_divisor)]
	]).

	centered_values([], _Centered) :-
		evaluation_error(zero_divisor).
	centered_values([Reference| Values], Centered) :-
		center([Reference| Values], Reference, Offsets),
		mean_values(Offsets, MeanOffset),
		center(Offsets, MeanOffset, Centered).

	:- protected(scale_values/2).
	:- mode(scale_values(+list(number), -list(number)), one).
	:- info(scale_values/2, [
		comment is 'Scales numeric values by their largest absolute value, leaving an all-zero list unchanged.',
		argnames is ['Values', 'Scaled']
	]).

	scale_values(Values, Scaled) :-
		scale_values(Values, Scaled, _Scale).

	:- protected(scale_values/3).
	:- mode(scale_values(+list(number), -list(number), -number), one).
	:- info(scale_values/3, [
		comment is 'Scales numeric values and returns the divisor, using 1.0 for an all-zero or empty list.',
		argnames is ['Values', 'Scaled', 'Scale']
	]).

	scale_values(Values, Scaled, Scale) :-
		maximum_absolute(Values, 0.0, Maximum),
		(	Maximum =:= 0 ->
			Scaled = Values,
			Scale = 1.0
		;	Scale = Maximum,
			divide_values(Values, Scale, Scaled)
		).

	maximum_absolute([], Maximum, Maximum).
	maximum_absolute([Value| Values], Maximum0, Maximum) :-
		Maximum1 is max(Maximum0, abs(Value)),
		maximum_absolute(Values, Maximum1, Maximum).

	divide_values([], _Divisor, []).
	divide_values([Value| Values], Divisor, [Scaled| ScaledValues]) :-
		Scaled is Value / Divisor,
		divide_values(Values, Divisor, ScaledValues).

	:- protected(squared_deviations/3).
	:- mode(squared_deviations(+list(number), +number, -list(number)), one).
	:- info(squared_deviations/3, [
		comment is 'Computes the squared deviation of every value from a given reference value (typically the mean).',
		argnames is ['Values', 'Reference', 'SquaredDeviations']
	]).

	squared_deviations([], _Reference, []).
	squared_deviations([Value| Values], Reference, [SquaredDeviation| SquaredDeviations]) :-
		Deviation is Value - Reference,
		SquaredDeviation is Deviation * Deviation,
		squared_deviations(Values, Reference, SquaredDeviations).

	:- protected(sum_of_squared_deviations/3).
	:- mode(sum_of_squared_deviations(+list(number), +number, -float), one).
	:- info(sum_of_squared_deviations/3, [
		comment is 'Computes the sum of the squared deviations of every value from a given reference value (typically the mean); the within-group sum of squares used by anova_f_score is the sum of this over every group.',
		argnames is ['Values', 'Reference', 'SumSquaredDeviations']
	]).

	sum_of_squared_deviations(Values, Reference, SumSquaredDeviations) :-
		squared_deviations(Values, Reference, SquaredDeviations),
		sum(SquaredDeviations, SumSquaredDeviations).

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

	:- protected(dot_product/3).
	:- mode(dot_product(+list(number), +list(number), -number), one).
	:- info(dot_product/3, [
		comment is 'Computes the dot product of two position-aligned numeric lists of the same length.',
		argnames is ['Values1', 'Values2', 'Dot']
	]).

	dot_product(Values1, Values2, Dot) :-
		dot_product(Values1, Values2, 0.0, Dot).

	dot_product([], [], Dot, Dot).
	dot_product([Value1| Values1], [Value2| Values2], Dot0, Dot) :-
		Dot1 is Dot0 + Value1 * Value2,
		dot_product(Values1, Values2, Dot1, Dot).

	:- protected(sum_of_squares_list/2).
	:- mode(sum_of_squares_list(+list(number), -number), one).
	:- info(sum_of_squares_list/2, [
		comment is 'Computes the sum of the squares of a numeric list.',
		argnames is ['Values', 'SumSquares']
	]).

	sum_of_squares_list(Values, SumSquares) :-
		sum_of_squares_list(Values, 0.0, SumSquares).

	sum_of_squares_list([], SumSquares, SumSquares).
	sum_of_squares_list([Value| Values], SumSquares0, SumSquares) :-
		SumSquares1 is SumSquares0 + Value * Value,
		sum_of_squares_list(Values, SumSquares1, SumSquares).

	:- protected(split_pairs/3).
	:- mode(split_pairs(+list(pair), -list(number), -list(number)), one).
	:- info(split_pairs/3, [
		comment is 'Splits a list of ``Value1-Value2`` pairs into separate, position-aligned lists of first and second values.',
		argnames is ['Pairs', 'Values1', 'Values2']
	]).

	split_pairs([], [], []).
	split_pairs([Value1-Value2| Pairs], [Value1| Values1], [Value2| Values2]) :-
		split_pairs(Pairs, Values1, Values2).

	:- protected(group_by_target/2).
	:- mode(group_by_target(+list(pair), -list(pair)), one).
	:- info(group_by_target/2, [
		comment is 'Groups ``Value-Target`` pairs into ``Target-Values`` pairs in standard target order, preserving input value order within each group.',
		argnames is ['Pairs', 'Groups']
	]).

	group_by_target(Pairs, Groups) :-
		target_keys(Pairs, Keyed),
		keysort(Keyed, Sorted),
		group_sorted_targets(Sorted, Groups).

	target_keys([], []).
	target_keys([Value-Target| Pairs], [Target-Value| Keyed]) :-
		target_keys(Pairs, Keyed).

	group_sorted_targets([], []).
	group_sorted_targets([Target-Value| Pairs], [Target-[Value| Values]| Groups]) :-
		take_target_values(Pairs, Target, Values, Rest),
		group_sorted_targets(Rest, Groups).

	take_target_values([], _Target, [], []).
	take_target_values([NextTarget-Value| Pairs], Target, Values, Rest) :-
		(	NextTarget == Target ->
			Values = [Value| Values0],
			take_target_values(Pairs, Target, Values0, Rest)
		;	Values = [],
			Rest = [NextTarget-Value| Pairs]
		).

:- end_category.
