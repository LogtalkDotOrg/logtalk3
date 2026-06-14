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


:- object(regression_tree,
	imports(regressor_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Regression tree regressor supporting continuous and mixed-feature datasets using recursive variance-reduction splits.',
		see_also is [linear_regression, knn_regression, random_forest_regression, gradient_boosting_regression]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		permutation/2 as random_permutation/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, nth1/3
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Regressor, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::target(Target),
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		build_encoders(Attributes, Examples, Options, Encoders, FeatureLabels),
		^^examples_to_rows(Examples, Encoders, Rows),
		build_tree(Rows, FeatureLabels, 0, Options, Tree),
		length(Examples, TrainingExampleCount),
		build_diagnostics(Target, FeatureLabels, TrainingExampleCount, Options, Diagnostics),
		Regressor = regression_tree_regressor(Encoders, FeatureLabels, Tree, Diagnostics).

	predict(Regressor, Instance, Target) :-
		Regressor =.. [_, Encoders, _FeatureLabels, Tree, _Diagnostics],
		^^encode_instance(Encoders, Instance, Features),
		predict_tree(Tree, Features, Target).

	build_diagnostics(Target, FeatureLabels, TrainingExampleCount, Options, Diagnostics) :-
		length(FeatureLabels, FeatureCount),
		^^base_regressor_diagnostics(regression_tree, Target, TrainingExampleCount, Options, [encoded_feature_count(FeatureCount)], Diagnostics).

	build_encoders([], _, _, [], []).
	build_encoders([Attribute-Values| Rest], Examples, Options, [Encoder| Encoders], FeatureLabels) :-
		(	Values == continuous ->
			^^continuous_stats(Attribute, Examples, Options, Mean, Scale),
			Encoder = continuous(Attribute, Mean, Scale),
			AttributeLabels = [feature(Attribute, value), feature(Attribute, missing)]
		;	Encoder = categorical(Attribute, Values),
			categorical_feature_labels(Attribute, Values, AttributeLabels)
		),
		append(AttributeLabels, RestLabels, FeatureLabels),
		build_encoders(Rest, Examples, Options, Encoders, RestLabels).

	categorical_feature_labels(Attribute, Values, Labels) :-
		categorical_value_labels(Values, Attribute, ValueLabels),
		append(ValueLabels, [feature(Attribute, missing)], Labels).

	categorical_value_labels([], _Attribute, []).
	categorical_value_labels([_Baseline], _Attribute, []) :-
		!.
	categorical_value_labels([_Baseline| Values], Attribute, Labels) :-
		categorical_non_baseline_value_labels(Values, Attribute, Labels).

	categorical_non_baseline_value_labels([], _Attribute, []).
	categorical_non_baseline_value_labels([Value| Values], Attribute, [feature(Attribute, category(Value))| Labels]) :-
		categorical_non_baseline_value_labels(Values, Attribute, Labels).

	build_tree(Rows, FeatureLabels, Depth, Options, Tree) :-
		mean_target(Rows, Mean),
		(	stopping_condition(Rows, Depth, Options) ->
			Tree = leaf(Mean)
		;	best_split(Rows, FeatureLabels, Options, BestIndex, BestThreshold, BestReduction, LeftRows, RightRows),
			^^option(minimum_variance_reduction(MinimumVarianceReduction), Options),
			(	(BestIndex == none ; BestReduction =< MinimumVarianceReduction) ->
				Tree = leaf(Mean)
			;	NextDepth is Depth + 1,
				build_tree(LeftRows, FeatureLabels, NextDepth, Options, LeftTree),
				build_tree(RightRows, FeatureLabels, NextDepth, Options, RightTree),
				Tree = node(BestIndex, BestThreshold, Mean, LeftTree, RightTree)
			)
		),
		!.

	stopping_condition(Rows, _Depth, Options) :-
		length(Rows, Count),
		^^option(minimum_samples_leaf(MinimumSamplesLeaf), Options),
		Count =< MinimumSamplesLeaf,
		!.
	stopping_condition(_Rows, Depth, Options) :-
		^^option(maximum_depth(MaximumDepth), Options),
		Depth >= MaximumDepth,
		!.
	stopping_condition(Rows, _Depth, _Options) :-
		target_variance(Rows, Variance),
		Variance =< 0.0.

	best_split(Rows, FeatureLabels, Options, BestIndex, BestThreshold, BestReduction, BestLeftRows, BestRightRows) :-
		target_sse(Rows, ParentSSE),
		split_feature_indexes(FeatureLabels, Options, FeatureIndexes),
		best_split_feature(FeatureIndexes, Rows, ParentSSE, Options, none, 0.0, none, [], [], BestIndex, BestThreshold, BestReduction, BestLeftRows, BestRightRows).

	split_feature_indexes(FeatureLabels, Options, FeatureIndexes) :-
		attribute_feature_indexes(FeatureLabels, AttributeIndexes),
		^^option(maximum_features_per_split(MaximumFeatures), Options),
		select_attribute_feature_indexes(AttributeIndexes, MaximumFeatures, FeatureIndexes).

	attribute_feature_indexes(FeatureLabels, AttributeIndexes) :-
		attribute_feature_indexes(FeatureLabels, 1, AttributeIndexes).
	attribute_feature_indexes([], _, []).
	attribute_feature_indexes([feature(Attribute, _)| FeatureLabels], Index, [Attribute-[Index| Indexes]| AttributeIndexes]) :-
		NextIndex is Index + 1,
		collect_attribute_feature_indexes(FeatureLabels, Attribute, NextIndex, RestIndex, Indexes, RestLabels),
		attribute_feature_indexes(RestLabels, RestIndex, AttributeIndexes).

	collect_attribute_feature_indexes([feature(Attribute, _)| FeatureLabels], Attribute, Index, RestIndex, [Index| Indexes], RestLabels) :-
		!,
		NextIndex is Index + 1,
		collect_attribute_feature_indexes(FeatureLabels, Attribute, NextIndex, RestIndex, Indexes, RestLabels).
	collect_attribute_feature_indexes(FeatureLabels, _Attribute, Index, Index, [], FeatureLabels).

	select_attribute_feature_indexes(AttributeIndexes, MaximumFeatures, FeatureIndexes) :-
		length(AttributeIndexes, AttributeCount),
		(	(MaximumFeatures == all ; MaximumFeatures >= AttributeCount) ->
			flatten_attribute_feature_indexes(AttributeIndexes, FeatureIndexes)
		;	random_permutation(AttributeIndexes, ShuffledAttributeIndexes),
			take_attribute_groups(MaximumFeatures, ShuffledAttributeIndexes, SelectedAttributeIndexes),
			flatten_attribute_feature_indexes(SelectedAttributeIndexes, FeatureIndexes)
		).

	take_attribute_groups(0, _AttributeIndexes, []) :-
		!.
	take_attribute_groups(_Count, [], []) :-
		!.
	take_attribute_groups(Count, [AttributeIndexes| RestAttributeIndexes], [AttributeIndexes| SelectedAttributeIndexes]) :-
		NextCount is Count - 1,
		take_attribute_groups(NextCount, RestAttributeIndexes, SelectedAttributeIndexes).

	flatten_attribute_feature_indexes([], []).
	flatten_attribute_feature_indexes([_-Indexes| AttributeIndexes], FeatureIndexes) :-
		flatten_attribute_feature_indexes(AttributeIndexes, RestFeatureIndexes),
		append(Indexes, RestFeatureIndexes, FeatureIndexes).

	best_split_feature([], _Rows, _ParentSSE, _Options, BestIndex0, BestReduction0, BestThreshold0, BestLeft0, BestRight0, BestIndex, BestThreshold, BestReduction, BestLeft, BestRight) :-
		!,
		BestIndex = BestIndex0,
		BestThreshold = BestThreshold0,
		BestReduction = BestReduction0,
		BestLeft = BestLeft0,
		BestRight = BestRight0.
	best_split_feature([Index| FeatureIndexes], Rows, ParentSSE, Options, BestIndex0, BestReduction0, BestThreshold0, BestLeft0, BestRight0, BestIndex, BestThreshold, BestReduction, BestLeft, BestRight) :-
		candidate_thresholds(Rows, Index, Thresholds),
		best_threshold_for_feature(Thresholds, Rows, Index, ParentSSE, Options, BestThresholdForFeature, BestReductionForFeature, LeftRows, RightRows),
		(	number(BestReductionForFeature), number(BestReduction0), BestReductionForFeature > BestReduction0 ->
			NextBestIndex = Index,
			NextBestReduction = BestReductionForFeature,
			NextBestThreshold = BestThresholdForFeature,
			NextBestLeft = LeftRows,
			NextBestRight = RightRows
		;	NextBestIndex = BestIndex0,
			NextBestReduction = BestReduction0,
			NextBestThreshold = BestThreshold0,
			NextBestLeft = BestLeft0,
			NextBestRight = BestRight0
		),
		best_split_feature(FeatureIndexes, Rows, ParentSSE, Options, NextBestIndex, NextBestReduction, NextBestThreshold, NextBestLeft, NextBestRight, BestIndex, BestThreshold, BestReduction, BestLeft, BestRight).

	candidate_thresholds(Rows, Index, Thresholds) :-
		findall(Value, (member(Features-_, Rows), nth1(Index, Features, Value)), Values0),
		sort(Values0, Values),
		candidate_thresholds_from_values(Values, Thresholds).

	candidate_thresholds_from_values([], []).
	candidate_thresholds_from_values([_], []) :-
		!.
	candidate_thresholds_from_values([Value1, Value2| Values], [Threshold| Thresholds]) :-
		Threshold is (Value1 + Value2) / 2,
		candidate_thresholds_from_values([Value2| Values], Thresholds).

	best_threshold_for_feature(Thresholds, Rows, Index, ParentSSE, Options, BestThreshold, BestReduction, BestLeftRows, BestRightRows) :-
		best_threshold_for_feature(Thresholds, Rows, Index, ParentSSE, Options, none, 0.0, [], [], BestThreshold, BestReduction, BestLeftRows, BestRightRows).

	best_threshold_for_feature([], _Rows, _Index, _ParentSSE, _Options, BestThreshold0, BestReduction0, BestLeftRows0, BestRightRows0, BestThreshold, BestReduction, BestLeftRows, BestRightRows) :-
		BestThreshold = BestThreshold0,
		BestReduction = BestReduction0,
		BestLeftRows = BestLeftRows0,
		BestRightRows = BestRightRows0.
	best_threshold_for_feature([Threshold| Thresholds], Rows, Index, ParentSSE, Options, BestThreshold0, BestReduction0, BestLeftRows0, BestRightRows0, BestThreshold, BestReduction, BestLeftRows, BestRightRows) :-
		evaluate_threshold(Threshold, Rows, Index, ParentSSE, Options, CurrentReduction, CurrentLeftRows, CurrentRightRows),
		(	number(CurrentReduction), number(BestReduction0), CurrentReduction > BestReduction0 ->
			NextBestThreshold = Threshold,
			NextBestReduction = CurrentReduction,
			NextBestLeftRows = CurrentLeftRows,
			NextBestRightRows = CurrentRightRows
		;	NextBestThreshold = BestThreshold0,
			NextBestReduction = BestReduction0,
			NextBestLeftRows = BestLeftRows0,
			NextBestRightRows = BestRightRows0
		),
		best_threshold_for_feature(Thresholds, Rows, Index, ParentSSE, Options, NextBestThreshold, NextBestReduction, NextBestLeftRows, NextBestRightRows, BestThreshold, BestReduction, BestLeftRows, BestRightRows).

	evaluate_threshold(Threshold, Rows, Index, ParentSSE, Options, Reduction, LeftRows, RightRows) :-
		split_rows(Rows, Index, Threshold, LeftRows, RightRows),
		length(LeftRows, LeftCount),
		length(RightRows, RightCount),
		^^option(minimum_samples_leaf(MinimumSamplesLeaf), Options),
		(	(LeftCount < MinimumSamplesLeaf ; RightCount < MinimumSamplesLeaf) ->
			Reduction = 0.0
		;	target_sse(LeftRows, LeftSSE),
			target_sse(RightRows, RightSSE),
			Reduction is ParentSSE - LeftSSE - RightSSE
		), !.

	split_rows([], _Index, _Threshold, [], []).
	split_rows([Features-Target| Rows], Index, Threshold, LeftRows, RightRows) :-
		nth1(Index, Features, Value),
		(	Value =< Threshold ->
			LeftRows = [Features-Target| LeftRest],
			split_rows(Rows, Index, Threshold, LeftRest, RightRows)
		;	LeftRows = LeftRest,
			RightRows = [Features-Target| RightRest],
			split_rows(Rows, Index, Threshold, LeftRows, RightRest)
		).

	mean_target(Rows, Mean) :-
		findall(Target, member(_-Target, Rows), Targets),
		arithmetic_mean(Targets, Mean).

	target_variance([_-_], 0.0) :-
		!.
	target_variance(Rows, Variance) :-
		findall(Target, member(_-Target, Rows), Targets),
		variance(Targets, Variance).

	target_sse(Rows, SSE) :-
		mean_target(Rows, Mean),
		target_sse(Rows, Mean, 0.0, SSE).

	target_sse([], _Mean, SSE, SSE).
	target_sse([_-Target| Rows], Mean, SSE0, SSE) :-
		Error is Target - Mean,
		SSE1 is SSE0 + Error * Error,
		target_sse(Rows, Mean, SSE1, SSE).

	predict_tree(leaf(Target), _Features, Target).
	predict_tree(node(Index, Threshold, Fallback, LeftTree, RightTree), Features, Target) :-
		(	nth1(Index, Features, Value) ->
			(	Value =< Threshold ->
				predict_tree(LeftTree, Features, Target)
			;	predict_tree(RightTree, Features, Target)
			)
		;	Target = Fallback
		).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Encoders', 'FeatureLabels', 'Tree', 'Diagnostics'].

	regressor_term_template(regression_tree_regressor(_Encoders, _FeatureLabels, _Tree, _Diagnostics), regression_tree_regressor('Encoders', 'FeatureLabels', 'Tree', 'Diagnostics')).

	check_regressor(Regressor) :-
		(	Regressor = regression_tree_regressor(Encoders, FeatureLabels, Tree, Diagnostics),
			^^valid_regression_encoders(Encoders),
			^^valid_feature_labels(FeatureLabels),
			^^encoded_feature_count(Encoders, FeatureCount),
			length(FeatureLabels, FeatureCount),
			^^valid_regression_tree(Tree, FeatureCount),
			^^valid_regressor_metadata(regression_tree, Diagnostics),
			^^valid_diagnostic_count(encoded_feature_count, Diagnostics, FeatureCount) ->
			true
		;	domain_error(regressor, Regressor)
		).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor = regression_tree_regressor(Encoders, FeatureLabels, Tree, Diagnostics),
		Clause =.. [Functor, Encoders, FeatureLabels, Tree, Diagnostics].

	print_regressor(Regressor) :-
		Regressor = regression_tree_regressor(_Encoders, FeatureLabels, Tree, Diagnostics),
		format('Regression Tree Regressor~n', []),
		format('=======================~n~n', []),
		^^print_regressor_template(Regressor),
		format('Diagnostics: ~w~n~n', [Diagnostics]),
		print_tree(Tree, FeatureLabels, 0).

	print_tree(leaf(Target), _FeatureLabels, Indent) :-
		print_indent(Indent),
		format('leaf(~4f)~n', [Target]).
	print_tree(node(Index, Threshold, Fallback, LeftTree, RightTree), FeatureLabels, Indent) :-
		nth1(Index, FeatureLabels, Label),
		print_indent(Indent),
		format('if ~w =< ~4f (fallback ~4f)~n', [Label, Threshold, Fallback]),
		NextIndent is Indent + 2,
		print_tree(LeftTree, FeatureLabels, NextIndent),
		print_indent(Indent),
		format('else~n', []),
		print_tree(RightTree, FeatureLabels, NextIndent).

	print_indent(Indent) :-
		(	Indent =< 0 ->
			true
		;	format('~*|', [Indent])
		).

	default_option(maximum_depth(10)).
	default_option(minimum_samples_leaf(1)).
	default_option(minimum_variance_reduction(0.0)).
	default_option(maximum_features_per_split(all)).
	default_option(feature_scaling(false)).

	valid_option(maximum_depth(MaximumDepth)) :-
		valid(positive_integer, MaximumDepth).
	valid_option(minimum_samples_leaf(MinimumSamplesLeaf)) :-
		valid(positive_integer, MinimumSamplesLeaf).
	valid_option(minimum_variance_reduction(MinimumVarianceReduction)) :-
		valid(non_negative_float, MinimumVarianceReduction).
	valid_option(maximum_features_per_split(MaximumFeaturesPerSplit)) :-
		(	MaximumFeaturesPerSplit == all ->
			true
		;	valid(positive_integer, MaximumFeaturesPerSplit)
		).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).

:- end_object.
