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


:- object(lof,
	imports(anomaly_detector_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'Local Outlier Factor anomaly detector with multiple distance metrics, mixed-feature support, and missing-value handling. Learns from a dataset object implementing the ``anomaly_dataset_protocol`` protocol and returns a detector term that can be used for scoring, prediction, and export.',
		remarks is [
			'Algorithm' - 'The detector memorizes the training instances and computes Local Outlier Factor values by comparing the local reachability density of a query to the densities of its neighbors.',
			'Feature types' - 'Handles continuous and categorical attributes declared by the dataset object.',
			'Missing values' - 'Missing values are ignored when computing distances. Distances are normalized by the number of comparable dimensions.',
			'Normalized scores' - 'Raw LOF values are normalized to the interval ``[0.0, 1.0]`` by ranking them against the training raw scores.',
			'Anomaly detector representation' - 'The learned detector is represented as a ``lof_detector(AttributeNames, FeatureTypes, AttributeScales, Instances, Options)`` term.'
		],
		see_also is [anomaly_dataset_protocol, anomaly_detector_protocol, knn_distance, isolation_forest]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		last/2 as last_neighbor_distance/2, length/2, member/2, memberchk/2, msort/2, nth1/3, reverse/2, take/3
	]).

	:- uses(numberlist, [
		max/2, min/2
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
		Detector = lof_detector(AttributeNames, FeatureTypes, AttributeScales, Instances, Options).

	score(Detector, Instance, Score) :-
		detector_data(Detector, AttributeNames, FeatureTypes, AttributeScales, Instances, Options),
		extract_values(AttributeNames, Instance, RawValues),
		sanitize_input_values(RawValues, Values),
		raw_lof(Values, FeatureTypes, AttributeScales, Instances, Options, RawScore),
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

	export_to_clauses(_Dataset, Detector, Functor, [Clause]) :-
		Clause =.. [Functor, Detector].

	print_anomaly_detector(Detector) :-
		detector_data(Detector, AttributeNames, FeatureTypes, _AttributeScales, Instances, Options),
		length(Instances, NumInstances),
		format('Local Outlier Factor Anomaly Detector~n', []),
		format('====================================~n~n', []),
		format('Training instances: ~w~n', [NumInstances]),
		format('Features:           ~w~n', [AttributeNames]),
		format('Feature types:      ~w~n', [FeatureTypes]),
		^^print_anomaly_detector_template(Detector),
		format('Options:            ~w~n', [Options]).

	anomaly_detector_export_template(Functor, Template) :-
		Template =.. [Functor, 'Detector'].

	anomaly_detector_term_template(lof_detector(_AttributeNames, _FeatureTypes, _AttributeScales, _Instances, _Options), lof_detector('AttributeNames', 'FeatureTypes', 'AttributeScales', 'Instances', 'Options')).

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

	raw_lof(Values, FeatureTypes, AttributeScales, Instances, Options, RawScore) :-
		length(Instances, Count),
		(   Count > 1 ->
			neighbor_count(Options, Count, K),
			k_nearest_neighbors(Values, FeatureTypes, AttributeScales, Instances, Options, K, Neighbors),
			local_reachability_density(Values, FeatureTypes, AttributeScales, Instances, Options, K, Neighbors, QueryLrd),
			neighbor_lrd_ratio_sum(Neighbors, FeatureTypes, AttributeScales, Instances, Options, K, QueryLrd, 0.0, RatioSum),
			RawScore is float(RatioSum / K)
		;   RawScore = 1.0
		).

	neighbor_count(Options, Count, K) :-
		^^option(k(K0), Options),
		MaxK is Count - 1,
		(   MaxK =< 1 ->
			K = 1
		;   K0 =< MaxK ->
			K = K0
		;   K = MaxK
		).

	local_reachability_density(Values, FeatureTypes, AttributeScales, Instances, Options, K, Neighbors, Lrd) :-
		reachability_distance_sum(Values, FeatureTypes, AttributeScales, Instances, Options, K, Neighbors, 0.0, ReachabilitySum),
		(   ReachabilitySum =< 0.0 ->
			Lrd = 1.0e12
		;   Lrd is float(K / ReachabilitySum)
		).

	reachability_distance_sum(_Values, _FeatureTypes, _AttributeScales, _Instances, _Options, _K, [], Sum, Sum) :-
		!.
	reachability_distance_sum(Values, FeatureTypes, AttributeScales, Instances, Options, K, [Distance-NeighborId-_-NeighborValues| Neighbors], Sum0, Sum) :-
		neighbor_k_distance(NeighborId, NeighborValues, FeatureTypes, AttributeScales, Instances, Options, K, NeighborKDistance),
		ReachabilityDistance is max(Distance, NeighborKDistance),
		Sum1 is Sum0 + ReachabilityDistance,
		reachability_distance_sum(Values, FeatureTypes, AttributeScales, Instances, Options, K, Neighbors, Sum1, Sum).

	neighbor_k_distance(NeighborId, NeighborValues, FeatureTypes, AttributeScales, Instances, Options, K, KDistance) :-
		remove_instance(Instances, NeighborId, OtherInstances),
		length(OtherInstances, Count),
		(   Count > 0 ->
			(   K =< Count -> K2 = K ; K2 = Count ),
			k_nearest_neighbors(NeighborValues, FeatureTypes, AttributeScales, OtherInstances, Options, K2, NeighborNeighbors),
			k_distance_from_neighbors(NeighborNeighbors, KDistance)
		;   KDistance = 0.0
		).

	neighbor_lrd_ratio_sum([], _FeatureTypes, _AttributeScales, _Instances, _Options, _K, _QueryLrd, Sum, Sum).
	neighbor_lrd_ratio_sum([_-NeighborId-_-NeighborValues| Neighbors], FeatureTypes, AttributeScales, Instances, Options, K, QueryLrd, Sum0, Sum) :-
		neighbor_local_reachability_density(NeighborId, NeighborValues, FeatureTypes, AttributeScales, Instances, Options, K, NeighborLrd),
		Ratio is NeighborLrd / QueryLrd,
		Sum1 is Sum0 + Ratio,
		neighbor_lrd_ratio_sum(Neighbors, FeatureTypes, AttributeScales, Instances, Options, K, QueryLrd, Sum1, Sum).

	neighbor_local_reachability_density(NeighborId, NeighborValues, FeatureTypes, AttributeScales, Instances, Options, K, Lrd) :-
		remove_instance(Instances, NeighborId, OtherInstances),
		length(OtherInstances, Count),
		(   Count > 0 ->
			(   K =< Count -> K2 = K ; K2 = Count ),
			k_nearest_neighbors(NeighborValues, FeatureTypes, AttributeScales, OtherInstances, Options, K2, Neighbors),
			local_reachability_density(NeighborValues, FeatureTypes, AttributeScales, OtherInstances, Options, K2, Neighbors, Lrd)
		;   Lrd = 1.0e12
		).

	k_nearest_neighbors(Values, FeatureTypes, AttributeScales, Instances, Options, K, Neighbors) :-
		findall(
			Distance-Id-Class-OtherValues,
			(
				member(Id-Class-OtherValues, Instances),
				compute_distance(Values, OtherValues, FeatureTypes, AttributeScales, Options, Distance)
			),
			Distances
		),
		msort(Distances, SortedDistances),
		take(K, SortedDistances, Neighbors).

	k_distance_from_neighbors(Neighbors, KDistance) :-
		findall(Distance, member(Distance-_-_-_, Neighbors), Distances),
		last_neighbor_distance(Distances, KDistance).

	raw_score_pairs(Dataset, AttributeNames, FeatureTypes, AttributeScales, Instances, Options, RawPairs) :-
		findall(
			RawScore-Id-Class,
			(
				Dataset::example(Id, Class, AttributeValues),
				extract_values(AttributeNames, AttributeValues, RawValues),
				sanitize_input_values(RawValues, Values),
				remove_instance(Instances, Id, OtherInstances),
				raw_lof(Values, FeatureTypes, AttributeScales, OtherInstances, Options, RawScore)
			),
			RawPairs
		).

	training_raw_scores(FeatureTypes, AttributeScales, Instances, Options, ReferenceScores) :-
		findall(
			RawScore,
			(
				member(Id-_Class-Values, Instances),
				remove_instance(Instances, Id, OtherInstances),
				raw_lof(Values, FeatureTypes, AttributeScales, OtherInstances, Options, RawScore)
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
		reference_baseline(ReferenceScores, Baseline),
		max(ReferenceScores, Maximum),
		(   RawScore =< Baseline ->
			Score = 0.0
		;   Range is Maximum - Baseline,
			(   Range =< 0.0 ->
				Score = 1.0
			;   Normalized is float((RawScore - Baseline) / Range),
				clamp_score(Normalized, Score)
			)
		).

	reference_baseline([], 1.0).
	reference_baseline([ReferenceScore| ReferenceScores], Baseline) :-
		msort([ReferenceScore| ReferenceScores], SortedScores),
		length(SortedScores, Length),
		Position is (Length + 3) // 4,
		nth1(Position, SortedScores, Baseline).

	clamp_score(Score0, 0.0) :-
		Score0 =< 0.0,
		!.
	clamp_score(Score0, 1.0) :-
		Score0 >= 1.0,
		!.
	clamp_score(Score, Score).

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
	default_option(anomaly_threshold(0.4)).

	valid_option(k(K)) :-
		valid(positive_integer, K).
	valid_option(distance_metric(Metric)) :-
		valid(one_of(atom, [euclidean, manhattan, chebyshev, minkowski]), Metric).
	valid_option(anomaly_threshold(Threshold)) :-
		valid(probability, Threshold).

:- end_object.
