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


:- object(knn_distance,
	imports(anomaly_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-20,
		comment is 'k-nearest-neighbor distance anomaly detector with multiple distance metrics, mixed-feature support, and missing-value handling. Learns from a dataset object implementing the ``anomaly_dataset_protocol`` protocol and returns a detector term that can be used for scoring, prediction, and export.',
		remarks is [
			'Algorithm' - 'The detector memorizes the training instances and computes an anomaly score from normalized distances to the nearest neighbors. Larger distances indicate more isolated and therefore more anomalous instances.',
			'Score modes' - 'Supports both the distance to the k-th nearest neighbor and the mean distance to the k nearest neighbors.',
			'Feature types' - 'Handles continuous and categorical attributes declared by the dataset object.',
			'Missing values' - 'Missing values are ignored when computing distances. Distances are normalized by the number of comparable dimensions.',
			'Anomaly detector representation' - 'The learned detector is represented as a ``knn_distance_detector(AttributeNames, FeatureTypes, AttributeScales, Instances, Options)`` term.'
		],
		see_also is [anomaly_dataset_protocol, anomaly_detector_protocol, isolation_forest, lof]
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		last/2 as last_distance/2, length/2, member/2, memberchk/2, msort/2, reverse/2, take/3
	]).

	:- uses(numberlist, [
		max/2, min/2, sum/2
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Detector, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		determine_feature_types(Attributes, FeatureTypes),
		compute_attribute_scales(AttributeNames, Attributes, Dataset, AttributeScales),
		findall(
			Id-Class-Values,
			(
				Dataset::example(Id, Class, AttributeValues),
				extract_values(AttributeNames, AttributeValues, RawValues),
				sanitize_training_values(RawValues, Values)
			),
			Instances
		),
		Detector = knn_distance_detector(AttributeNames, FeatureTypes, AttributeScales, Instances, Options).

	score(Detector, Instance, Score) :-
		detector_data(Detector, AttributeNames, FeatureTypes, AttributeScales, Instances, Options),
		extract_values(AttributeNames, Instance, RawValues),
		sanitize_input_values(RawValues, Values),
		instance_raw_score(Values, FeatureTypes, AttributeScales, Instances, Options, RawScore),
		training_raw_scores(FeatureTypes, AttributeScales, Instances, Options, ReferenceScores),
		normalize_against_reference(RawScore, ReferenceScores, Score).

	score_all(Dataset, Detector, Scores) :-
		detector_data(Detector, AttributeNames, FeatureTypes, AttributeScales, Instances, Options),
		raw_score_pairs(Dataset, AttributeNames, FeatureTypes, AttributeScales, Instances, Options, RawPairs),
		reference_scores(RawPairs, ReferenceScores),
		normalize_raw_pairs(RawPairs, ReferenceScores, ScoredPairs),
		msort(ScoredPairs, SortedAscending),
		reverse(SortedAscending, SortedDescending),
		^^extract_scores(SortedDescending, Scores).

	anomaly_detector_to_clauses(_Dataset, Detector, Functor, [Clause]) :-
		Clause =.. [Functor, Detector].

	print_anomaly_detector(Detector) :-
		detector_data(Detector, AttributeNames, FeatureTypes, _AttributeScales, Instances, Options),
		length(Instances, NumInstances),
		format('k-Nearest-Neighbor Distance Anomaly Detector~n', []),
		format('==========================================~n~n', []),
		format('Training instances: ~w~n', [NumInstances]),
		format('Features:           ~w~n', [AttributeNames]),
		format('Feature types:      ~w~n', [FeatureTypes]),
		^^print_anomaly_detector_template(Detector),
		format('Options:            ~w~n', [Options]).

	anomaly_detector_export_template(Functor, Template) :-
		Template =.. [Functor, 'Detector'].

	anomaly_detector_term_template(knn_distance_detector(_AttributeNames, _FeatureTypes, _AttributeScales, _Instances, _Options), knn_distance_detector('AttributeNames', 'FeatureTypes', 'AttributeScales', 'Instances', 'Options')).

	detector_data(Detector, AttributeNames, FeatureTypes, AttributeScales, Instances, Options) :-
		Detector =.. [_Functor, AttributeNames, FeatureTypes, AttributeScales, Instances, Options].

	determine_feature_types([], []).
	determine_feature_types([_-Values| Pairs], [Type| Types]) :-
		(   Values == continuous ->
			Type = numeric
		;   Type = categorical
		),
		determine_feature_types(Pairs, Types).

	compute_attribute_scales([], _, _, []).
	compute_attribute_scales([Attribute| Attributes], AttributePairs, Dataset, [Scale| Scales]) :-
		memberchk(Attribute-Values, AttributePairs),
		(   Values == continuous ->
			findall(
				Value,
				(
					Dataset::example(_Id, _Class, AttributeValues),
					memberchk(Attribute-Value, AttributeValues),
					nonvar(Value),
					number(Value)
				),
				NumericValues
			),
			(   NumericValues == [] ->
				Scale = 1.0
			;   min(NumericValues, Minimum),
				max(NumericValues, Maximum),
				Range is float(Maximum - Minimum),
				(   Range =< 0.0 ->
					Scale = 1.0
				;   Scale = Range
				)
			)
		;   Scale = 1.0
		),
		compute_attribute_scales(Attributes, AttributePairs, Dataset, Scales).

	extract_values([], _, []).
	extract_values([Attribute| Attributes], AttributeValues, [Value| Values]) :-
		memberchk(Attribute-Value, AttributeValues),
		extract_values(Attributes, AttributeValues, Values).

	sanitize_training_values([], []).
	sanitize_training_values([Value| Values], [Sanitized| SanitizedValues]) :-
		(   var(Value) ->
			Sanitized = '$missing'
		;   Sanitized = Value
		),
		sanitize_training_values(Values, SanitizedValues).

	sanitize_input_values([], []).
	sanitize_input_values([Value| Values], [Sanitized| SanitizedValues]) :-
		(   var(Value) ->
			Sanitized = '$missing'
		;   Sanitized = Value
		),
		sanitize_input_values(Values, SanitizedValues).

	instance_raw_score(Values, FeatureTypes, AttributeScales, Instances, Options, Score) :-
		^^option(k(K0), Options),
		length(Instances, MaxK),
		(   MaxK > 0 ->
			(   K0 =< MaxK -> K = K0 ; K = MaxK ),
			findall(
				Distance,
				(
					member(_Id-_Class-OtherValues, Instances),
					compute_distance(Values, OtherValues, FeatureTypes, AttributeScales, Options, Distance)
				),
				Distances
			),
			keysort_distances(Distances, SortedDistances),
			take(K, SortedDistances, NeighborDistances),
				raw_score_from_neighbors(NeighborDistances, Options, Score)
		;   Score = 0.0
		).

		raw_score_pairs(Dataset, AttributeNames, FeatureTypes, AttributeScales, Instances, Options, RawPairs) :-
			findall(
				RawScore-Id-Class,
				(
					Dataset::example(Id, Class, AttributeValues),
					extract_values(AttributeNames, AttributeValues, Values),
					remove_instance(Instances, Id, OtherInstances),
					instance_raw_score(Values, FeatureTypes, AttributeScales, OtherInstances, Options, RawScore)
				),
				RawPairs
			).

		training_raw_scores(FeatureTypes, AttributeScales, Instances, Options, ReferenceScores) :-
			findall(
				RawScore,
				(
					member(Id-_Class-Values, Instances),
					remove_instance(Instances, Id, OtherInstances),
					instance_raw_score(Values, FeatureTypes, AttributeScales, OtherInstances, Options, RawScore)
				),
				ReferenceScores
			).

		reference_scores([], [0.0]).
		reference_scores([RawPair| RawPairs], ReferenceScores) :-
			findall(RawScore, member(RawScore-_-_, [RawPair| RawPairs]), Scores),
			(   Scores == [] ->
				ReferenceScores = [0.0]
			;   ReferenceScores = Scores
			).

		normalize_raw_pairs([], _ReferenceScores, []).
		normalize_raw_pairs([RawScore-Id-Class| RawPairs], ReferenceScores, [Score-Id-Class| ScoredPairs]) :-
			normalize_against_reference(RawScore, ReferenceScores, Score),
			normalize_raw_pairs(RawPairs, ReferenceScores, ScoredPairs).

		normalize_against_reference(RawScore, ReferenceScores, Score) :-
			count_less_or_equal(ReferenceScores, RawScore, 0, Count),
			length(ReferenceScores, Total),
			(   Total > 0 ->
				Score is float(Count / Total)
			;   Score = 0.0
			).

		count_less_or_equal([], _RawScore, Count, Count).
		count_less_or_equal([ReferenceScore| ReferenceScores], RawScore, Count0, Count) :-
			(   ReferenceScore =< RawScore ->
				Count1 is Count0 + 1
			;   Count1 = Count0
			),
			count_less_or_equal(ReferenceScores, RawScore, Count1, Count).

	keysort_distances(Distances, SortedDistances) :-
		findall(Distance-Distance, member(Distance, Distances), Pairs),
		keysort(Pairs, SortedPairs),
		extract_sorted_distances(SortedPairs, SortedDistances).

	extract_sorted_distances([], []).
	extract_sorted_distances([Distance-_| Pairs], [Distance| Distances]) :-
		extract_sorted_distances(Pairs, Distances).

	raw_score_from_neighbors(NeighborDistances, Options, RawScore) :-
		^^option(score_mode(ScoreMode), Options),
		(   ScoreMode == kth_distance ->
			last_distance(NeighborDistances, RawScore)
		;   mean_distance(NeighborDistances, RawScore)
		).

	mean_distance(Distances, Mean) :-
		sum(Distances, Sum),
		length(Distances, Count),
		Mean is float(Sum / Count).

	compute_distance(Values1, Values2, FeatureTypes, AttributeScales, Options, Distance) :-
		^^option(distance_metric(Metric), Options),
		compute_metric_distance(Metric, Values1, Values2, FeatureTypes, AttributeScales, Distance).

	compute_metric_distance(euclidean, Values1, Values2, FeatureTypes, AttributeScales, Distance) :-
		sum_squared_components(Values1, Values2, FeatureTypes, AttributeScales, 0.0, 0, SumSq, Count),
		normalize_component_distance(Count, SumSq, Normalized),
		Distance is sqrt(Normalized).
	compute_metric_distance(manhattan, Values1, Values2, FeatureTypes, AttributeScales, Distance) :-
		sum_absolute_components(Values1, Values2, FeatureTypes, AttributeScales, 0.0, 0, SumAbs, Count),
		normalize_component_distance(Count, SumAbs, Distance).
	compute_metric_distance(chebyshev, Values1, Values2, FeatureTypes, AttributeScales, Distance) :-
		max_component_distance(Values1, Values2, FeatureTypes, AttributeScales, 0.0, 0, Distance, _Count).
	compute_metric_distance(minkowski, Values1, Values2, FeatureTypes, AttributeScales, Distance) :-
		P = 3.0,
		sum_power_components(Values1, Values2, FeatureTypes, AttributeScales, P, 0.0, 0, SumPow, Count),
		normalize_component_distance(Count, SumPow, Normalized),
		Distance is Normalized ** (1.0 / P).

	normalize_component_distance(Count, _Raw, 1.0) :-
		Count =< 0,
		!.
	normalize_component_distance(Count, Raw, Normalized) :-
		Normalized is float(Raw / Count).

	sum_squared_components([], [], [], [], Sum, Count, Sum, Count).
	sum_squared_components([Value1| Values1], [Value2| Values2], [Type| Types], [Scale| Scales], Sum0, Count0, Sum, Count) :-
		(   comparable_component(Type, Value1, Value2, Scale, Delta) ->
			Sum1 is Sum0 + Delta * Delta,
			Count1 is Count0 + 1
		;   Sum1 = Sum0,
			Count1 = Count0
		),
		sum_squared_components(Values1, Values2, Types, Scales, Sum1, Count1, Sum, Count).

	sum_absolute_components([], [], [], [], Sum, Count, Sum, Count).
	sum_absolute_components([Value1| Values1], [Value2| Values2], [Type| Types], [Scale| Scales], Sum0, Count0, Sum, Count) :-
		(   comparable_component(Type, Value1, Value2, Scale, Delta) ->
			Sum1 is Sum0 + abs(Delta),
			Count1 is Count0 + 1
		;   Sum1 = Sum0,
			Count1 = Count0
		),
		sum_absolute_components(Values1, Values2, Types, Scales, Sum1, Count1, Sum, Count).

	max_component_distance([], [], [], [], Maximum, Count, Maximum, Count).
	max_component_distance([Value1| Values1], [Value2| Values2], [Type| Types], [Scale| Scales], Maximum0, Count0, Maximum, Count) :-
		(   comparable_component(Type, Value1, Value2, Scale, Delta) ->
			Component is abs(Delta),
			Maximum1 is max(Maximum0, Component),
			Count1 is Count0 + 1
		;   Maximum1 = Maximum0,
			Count1 = Count0
		),
		max_component_distance(Values1, Values2, Types, Scales, Maximum1, Count1, Maximum, Count).

	sum_power_components([], [], [], [], _, Sum, Count, Sum, Count).
	sum_power_components([Value1| Values1], [Value2| Values2], [Type| Types], [Scale| Scales], P, Sum0, Count0, Sum, Count) :-
		(   comparable_component(Type, Value1, Value2, Scale, Delta) ->
			Component is abs(Delta) ** P,
			Sum1 is Sum0 + Component,
			Count1 is Count0 + 1
		;   Sum1 = Sum0,
			Count1 = Count0
		),
		sum_power_components(Values1, Values2, Types, Scales, P, Sum1, Count1, Sum, Count).

	comparable_component(_Type, Value1, _Value2, _Scale, _Delta) :-
		missing_value(Value1),
		!,
		fail.
	comparable_component(_Type, _Value1, Value2, _Scale, _Delta) :-
		missing_value(Value2),
		!,
		fail.
	comparable_component(numeric, Value1, Value2, Scale, Delta) :-
		Scale > 0.0,
		Delta is float((Value1 - Value2) / Scale).
	comparable_component(categorical, Value1, Value2, _Scale, Delta) :-
		(   Value1 == Value2 ->
			Delta = 0.0
		;   Delta = 1.0
		).

	missing_value(Value) :-
		var(Value).
	missing_value('$missing').

	remove_instance([], _Id, []).
	remove_instance([Id-_-_| Instances], Id, Instances) :-
		!.
	remove_instance([Instance| Instances], Id, [Instance| Others]) :-
		remove_instance(Instances, Id, Others).

	default_option(k(5)).
	default_option(distance_metric(euclidean)).
	default_option(score_mode(kth_distance)).
	default_option(anomaly_threshold(0.5)).

	valid_option(k(K)) :-
		valid(positive_integer, K).
	valid_option(distance_metric(Metric)) :-
		valid(one_of(atom, [euclidean, manhattan, chebyshev, minkowski]), Metric).
	valid_option(score_mode(Mode)) :-
		valid(one_of(atom, [kth_distance, mean_distance]), Mode).
	valid_option(anomaly_threshold(Threshold)) :-
		valid(probability, Threshold).

:- end_object.
