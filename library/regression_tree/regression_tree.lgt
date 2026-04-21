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
	imports([options, regressor_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-21,
		comment is 'Regression tree regressor supporting continuous and mixed-feature datasets using recursive variance-reduction splits.',
		remarks is [
			'Algorithm' - 'Builds a binary regression tree by recursively selecting the encoded feature threshold that maximizes variance reduction.',
			'Feature handling' - 'Continuous features may be standardized using z-score scaling. Categorical features are one-hot encoded from the declared dataset attribute values.',
			'Missing values' - 'Missing feature values represented using anonymous variables are encoded using explicit missing-value indicator features.',
			'Regressor representation' - 'The learned regressor is represented by default as ``regression_tree_regressor(Encoders, FeatureLabels, Tree, Options)`` where ``Tree`` is built from ``leaf(Prediction)`` and ``node(Index, Threshold, FallbackPrediction, Left, Right)`` terms.'
		],
		see_also is [linear_regression, knn_regression, random_forest_regression, gradient_boosting_regression]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a regressor from the given dataset object using the specified options.',
		argnames is ['Dataset', 'Regressor', 'Options']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, nth1/3
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
		Dataset::target(_Target),
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		build_encoders(Attributes, Examples, Options, Encoders, FeatureLabels),
		examples_to_rows(Examples, Encoders, Rows),
		build_tree(Rows, 0, Options, Tree),
		Regressor = regression_tree_regressor(Encoders, FeatureLabels, Tree, Options).

	predict(Regressor, Instance, Target) :-
		Regressor =.. [_, Encoders, _FeatureLabels, Tree, _Options],
		encode_instance(Encoders, Instance, Features),
		predict_tree(Tree, Features, Target).

	build_encoders([], _, _, [], []).
	build_encoders([Attribute-Values| Rest], Examples, Options, [Encoder| Encoders], FeatureLabels) :-
		(   Values == continuous ->
			continuous_stats(Attribute, Examples, Options, Mean, Scale),
			Encoder = continuous(Attribute, Mean, Scale),
			AttributeLabels = [feature(Attribute, value), feature(Attribute, missing)]
		;   Encoder = categorical(Attribute, Values),
			categorical_feature_labels(Attribute, Values, AttributeLabels)
		),
		append(AttributeLabels, RestLabels, FeatureLabels),
		build_encoders(Rest, Examples, Options, Encoders, RestLabels).

	continuous_stats(Attribute, Examples, Options, Mean, Scale) :-
		^^option(feature_scaling(FeatureScaling), Options),
		(   FeatureScaling == on ->
			known_attribute_values(Examples, Attribute, Values),
			(   Values == [] ->
				Mean = 0.0,
				Scale = 1.0
			;   arithmetic_mean(Values, Mean),
				length(Values, Count),
				(   Count > 1 ->
					variance(Values, Variance)
				;   Variance = 0.0
				),
				(   Variance > 0.0 ->
					Scale is sqrt(Variance)
				;   Scale = 1.0
				)
			)
		;   Mean = 0.0,
			Scale = 1.0
		).

	known_attribute_values([], _, []).
	known_attribute_values([example(_Id, _Target, AttributeValues)| Examples], Attribute, Values) :-
		(   memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			Values = [Value| Rest]
		;   Values = Rest
		),
		known_attribute_values(Examples, Attribute, Rest).

	categorical_feature_labels(Attribute, Values, Labels) :-
		categorical_value_labels(Values, Attribute, ValueLabels),
		append(ValueLabels, [feature(Attribute, missing)], Labels).

	categorical_value_labels([], _Attribute, []).
	categorical_value_labels([Value| Values], Attribute, [feature(Attribute, category(Value))| Labels]) :-
		categorical_value_labels(Values, Attribute, Labels).

	examples_to_rows([], _, []).
	examples_to_rows([example(_Id, Target, AttributeValues)| Examples], Encoders, [Features-Target| Rows]) :-
		encode_instance(Encoders, AttributeValues, Features),
		examples_to_rows(Examples, Encoders, Rows).

	encode_instance([], _, []).
	encode_instance([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature, Missing| Features]) :-
		!,
		(   memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			normalize_continuous(Value, Mean, Scale, Feature),
			Missing = 0.0
		;   Feature = 0.0,
			Missing = 1.0
		),
		encode_instance(Encoders, AttributeValues, Features).
	encode_instance([categorical(Attribute, Values)| Encoders], AttributeValues, Features) :-
		(   memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			check_categorical_value(Attribute, Values, Value),
			one_hot_encode(Values, Value, Encoded)
		;   missing_one_hot_encode(Values, Encoded)
		),
		append(Encoded, RestFeatures, Features),
		encode_instance(Encoders, AttributeValues, RestFeatures).

	normalize_continuous(Value, Mean, Scale, Feature) :-
		(   number(Value) ->
			true
		;   type_error(number, Value)
		),
		Feature is (Value - Mean) / Scale.

	check_categorical_value(Attribute, Values, Value) :-
		(   member(Value, Values) ->
			true
		;   domain_error(attribute_value(Attribute, Values), Value)
		).

	one_hot_encode(Values, Value, Encoded) :-
		one_hot_encode_(Values, Value, Encoded0),
		append(Encoded0, [0.0], Encoded).

	one_hot_encode_([], _, []).
	one_hot_encode_([Category| Categories], Value, [Feature| Features]) :-
		(   Value == Category ->
			Feature = 1.0
		;   Feature = 0.0
		),
		one_hot_encode_(Categories, Value, Features).

	missing_one_hot_encode(Values, Encoded) :-
		zero_vector_from_values(Values, Zeroes),
		append(Zeroes, [1.0], Encoded).

	zero_vector_from_values([], []).
	zero_vector_from_values([_| Values], [0.0| Zeroes]) :-
		zero_vector_from_values(Values, Zeroes).

	build_tree(Rows, Depth, Options, Tree) :-
		mean_target(Rows, Mean),
		(   stopping_condition(Rows, Depth, Options) ->
			Tree = leaf(Mean)
		;   feature_count(Rows, NumFeatures),
			best_split(Rows, NumFeatures, Options, BestIndex, BestThreshold, BestReduction, LeftRows, RightRows),
			^^option(minimum_variance_reduction(MinimumVarianceReduction), Options),
			(   BestIndex == none ; BestReduction =< MinimumVarianceReduction ->
				Tree = leaf(Mean)
			;   NextDepth is Depth + 1,
				build_tree(LeftRows, NextDepth, Options, LeftTree),
				build_tree(RightRows, NextDepth, Options, RightTree),
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

	feature_count([Features-_| _], Count) :-
		length(Features, Count).

	best_split(Rows, NumFeatures, Options, BestIndex, BestThreshold, BestReduction, BestLeftRows, BestRightRows) :-
		target_sse(Rows, ParentSSE),
		best_split_feature(1, NumFeatures, Rows, ParentSSE, Options, none, 0.0, none, [], [], BestIndex, BestThreshold, BestReduction, BestLeftRows, BestRightRows).

	best_split_feature(Index, NumFeatures, _Rows, _ParentSSE, _Options, BestIndex0, BestReduction0, BestThreshold0, BestLeft0, BestRight0, BestIndex, BestThreshold, BestReduction, BestLeft, BestRight) :-
		Index > NumFeatures,
		!,
		BestIndex = BestIndex0,
		BestThreshold = BestThreshold0,
		BestReduction = BestReduction0,
		BestLeft = BestLeft0,
		BestRight = BestRight0.
	best_split_feature(Index, NumFeatures, Rows, ParentSSE, Options, BestIndex0, BestReduction0, BestThreshold0, BestLeft0, BestRight0, BestIndex, BestThreshold, BestReduction, BestLeft, BestRight) :-
		candidate_thresholds(Rows, Index, Thresholds),
		best_threshold_for_feature(Thresholds, Rows, Index, ParentSSE, Options, BestThresholdForFeature, BestReductionForFeature, LeftRows, RightRows),
		(   number(BestReductionForFeature), number(BestReduction0), BestReductionForFeature > BestReduction0 ->
			NextBestIndex = Index,
			NextBestReduction = BestReductionForFeature,
			NextBestThreshold = BestThresholdForFeature,
			NextBestLeft = LeftRows,
			NextBestRight = RightRows
		;   NextBestIndex = BestIndex0,
			NextBestReduction = BestReduction0,
			NextBestThreshold = BestThreshold0,
			NextBestLeft = BestLeft0,
			NextBestRight = BestRight0
		),
		NextIndex is Index + 1,
		best_split_feature(NextIndex, NumFeatures, Rows, ParentSSE, Options, NextBestIndex, NextBestReduction, NextBestThreshold, NextBestLeft, NextBestRight, BestIndex, BestThreshold, BestReduction, BestLeft, BestRight).

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
		(   number(CurrentReduction), number(BestReduction0), CurrentReduction > BestReduction0 ->
			NextBestThreshold = Threshold,
			NextBestReduction = CurrentReduction,
			NextBestLeftRows = CurrentLeftRows,
			NextBestRightRows = CurrentRightRows
		;   NextBestThreshold = BestThreshold0,
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
		(   LeftCount < MinimumSamplesLeaf ; RightCount < MinimumSamplesLeaf ->
			Reduction = 0.0
		;   target_sse(LeftRows, LeftSSE),
			target_sse(RightRows, RightSSE),
			Reduction is ParentSSE - LeftSSE - RightSSE
		), !.

	split_rows([], _Index, _Threshold, [], []).
	split_rows([Features-Target| Rows], Index, Threshold, LeftRows, RightRows) :-
		nth1(Index, Features, Value),
		(   Value =< Threshold ->
			LeftRows = [Features-Target| LeftRest],
			split_rows(Rows, Index, Threshold, LeftRest, RightRows)
		;   LeftRows = LeftRest,
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
		(   nth1(Index, Features, Value) ->
			(   Value =< Threshold ->
				predict_tree(LeftTree, Features, Target)
			;   predict_tree(RightTree, Features, Target)
			)
		;   Target = Fallback
		).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Encoders', 'FeatureLabels', 'Tree', 'Options'].

	regressor_term_template(regression_tree_regressor(_Encoders, _FeatureLabels, _Tree, _Options), regression_tree_regressor('Encoders', 'FeatureLabels', 'Tree', 'Options')).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor = regression_tree_regressor(Encoders, FeatureLabels, Tree, Options),
		Clause =.. [Functor, Encoders, FeatureLabels, Tree, Options].

	print_regressor(Regressor) :-
		Regressor = regression_tree_regressor(_Encoders, FeatureLabels, Tree, Options),
		format('Regression Tree Regressor~n', []),
		format('=======================~n~n', []),
		^^print_regressor_template(Regressor),
		format('Options: ~w~n~n', [Options]),
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
		(   Indent =< 0 ->
			true
		;   format('~*|', [Indent])
		).

	default_option(maximum_depth(10)).
	default_option(minimum_samples_leaf(1)).
	default_option(minimum_variance_reduction(0.0)).
	default_option(feature_scaling(off)).

	valid_option(maximum_depth(MaximumDepth)) :-
		valid(positive_integer, MaximumDepth).
	valid_option(minimum_samples_leaf(MinimumSamplesLeaf)) :-
		valid(positive_integer, MinimumSamplesLeaf).
	valid_option(minimum_variance_reduction(MinimumVarianceReduction)) :-
		valid(non_negative_float, MinimumVarianceReduction).
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).

:- end_object.
