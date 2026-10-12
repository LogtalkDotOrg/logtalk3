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


:- object(lof_anomaly_detector,
	imports(anomaly_detector_common)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Local Outlier Factor anomaly detector with multiple distance metrics, mixed-feature support, and missing-value handling. Learns from baseline training examples selected from a dataset object implementing the ``anomaly_dataset_protocol`` protocol and returns a detector term that can be used for scoring, prediction, and export.',
		see_also is [
			anomaly_dataset_protocol, anomaly_detector_protocol, knn_distance_anomaly_detector,
			isolation_forest_anomaly_detector
		]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, last/2 as last_neighbor_distance/2, length/2, member/2, memberchk/2, msort/2, reverse/2
	]).

	:- uses(numberlist, [
		max/2, min_max/3
	]).

	:- uses(pairs, [
		group_sorted_by_key/2, keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- private(extract_values/3).
	:- mode(extract_values(+list(atom), +list(pair), -list), one_or_error).
	:- info(extract_values/3, [
		comment is 'Extracts feature values in the declared attribute order.',
		argnames is ['Attributes', 'AttributeValues', 'Values'],
		exceptions is [
			'A declared attribute is absent from ``AttributeValues``' - existence_error(attribute, 'Attribute')
		]
	]).

	:- private(check_feature_values/2).
	:- mode(check_feature_values(+list(atom), +list), one_or_error).
	:- info(check_feature_values/2, [
		comment is 'Checks prepared numeric, categorical, and missing feature values.',
		argnames is ['FeatureTypes', 'Values'],
		exceptions is [
			'A bound feature value in ``Values`` is not ground' - instantiation_error,
			'A numeric feature value is not a number' - type_error(number, 'Value'),
			'A categorical feature value is not an atom' - type_error(atom, 'Value')
		]
	]).

	:- private(check_instance_ids/2).
	:- mode(check_instance_ids(+list, +list(integer)), one_or_error).
	:- info(check_instance_ids/2, [
		comment is 'Checks distinct integer identities for training examples.',
		argnames is ['Instances', 'Seen'],
		exceptions is [
			'An example identifier in ``Instances`` is not ground' - instantiation_error,
			'An example identifier is not an integer' - type_error(integer, 'Id'),
			'An example identifier occurs more than once' - domain_error(unique_example_id, 'Id')
		]
	]).

	learn(Dataset, Detector, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^baseline_training_examples(Dataset, Examples, Options),
		^^dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		determine_feature_types(Attributes, FeatureTypes),
		findall(
			Id-Class-Values,
			(
				member(Id-Class-AttributeValues, Examples),
				extract_values(AttributeNames, AttributeValues, RawValues),
				sanitize_training_values(RawValues, Values),
				check_feature_values(FeatureTypes, Values)
			),
			Instances
		),
		^^check_examples_non_empty(Dataset, Instances),
		check_instance_ids(Instances, []),
		compute_attribute_scales(FeatureTypes, Instances, AttributeScales),
		build_training_cache(FeatureTypes, AttributeScales, Instances, Options, ReferenceScores, Cache),
			build_diagnostics(Dataset, AttributeNames, FeatureTypes, Instances, ReferenceScores, Options, Diagnostics),
			Detector = lof_detector(Dataset, AttributeNames, FeatureTypes, AttributeScales, Instances, ReferenceScores, Cache, Diagnostics).

	check_anomaly_detector(Detector) :-
		(	\+ ground(Detector) ->
			instantiation_error
		;	Detector = lof_detector(TrainingDataset, AttributeNames, FeatureTypes, AttributeScales, Instances, ReferenceScores, Cache, Diagnostics),
			valid(object_identifier, TrainingDataset),
			valid_attribute_names(AttributeNames),
			valid_feature_types(FeatureTypes, AttributeNames),
			valid_attribute_scales(AttributeScales, AttributeNames),
			valid_training_instances(Instances, FeatureTypes),
			valid_reference_scores(ReferenceScores, Instances),
			valid_detector_diagnostics(TrainingDataset, AttributeNames, FeatureTypes, Instances, ReferenceScores, Diagnostics),
			memberchk(options(Options), Diagnostics),
			valid_training_cache(Cache, Instances, ReferenceScores, Options) ->
			true
		;	domain_error(anomaly_detector, Detector)
		).

		anomaly_detector_diagnostics_data(lof_detector(_Dataset, _AttributeNames, _FeatureTypes, _AttributeScales, _Instances, _ReferenceScores, _Cache, Diagnostics), Diagnostics).

		build_diagnostics(Dataset, AttributeNames, FeatureTypes, Instances, ReferenceScores, Options, Diagnostics) :-
		length(Instances, ExampleCount),
		length(ReferenceScores, ReferenceScoreCount),
		Diagnostics = [
			model(lof_anomaly_detector),
			training_dataset(Dataset),
			attribute_names(AttributeNames),
			feature_types(FeatureTypes),
			example_count(ExampleCount),
			reference_score_count(ReferenceScoreCount),
			options(Options)
		].

	score(Detector, Instance, Score) :-
		detector_data(Detector, _TrainingDataset, AttributeNames, FeatureTypes, AttributeScales, Instances, _ReferenceScores, Cache, Diagnostics),
		memberchk(options(Options), Diagnostics),
		extract_values(AttributeNames, Instance, RawValues),
		sanitize_input_values(RawValues, Values),
		check_feature_values(FeatureTypes, Values),
		cached_raw_score(Values, FeatureTypes, AttributeScales, Instances, Options, Cache, RawScore),
		Cache = lof_cache(_, _, _, _, Maximum),
		normalize_against_maximum(RawScore, Maximum, Score).

	score_all(Dataset, Detector, Scores) :-
		detector_data(Detector, TrainingDataset, AttributeNames, FeatureTypes, AttributeScales, Instances, ReferenceScores, Cache, Diagnostics),
		Cache = lof_cache(_, _, _, ReferenceIndex, Maximum),
		memberchk(options(Options), Diagnostics),
		memberchk(baseline_selection_policy(Policy), Options),
		(	Dataset == TrainingDataset,
			Policy == reject ->
			raw_score_pairs_training(Instances, ReferenceScores, RawPairs)
		;	Dataset == TrainingDataset ->
			raw_score_pairs_training_dataset(Dataset, AttributeNames, FeatureTypes, AttributeScales, Instances, ReferenceIndex, Options, Cache, RawPairs)
		;	raw_score_pairs(Dataset, AttributeNames, FeatureTypes, AttributeScales, Instances, Options, Cache, RawPairs)
		),
		normalize_raw_pairs_maximum(RawPairs, Maximum, ScoredPairs),
		msort(ScoredPairs, SortedAscending),
		reverse(SortedAscending, SortedDescending),
		^^extract_scores(SortedDescending, Scores).

	export_to_clauses(_Dataset, Detector, Functor, [Clause]) :-
		Clause =.. [Functor, Detector].

	print_anomaly_detector(Detector) :-
		detector_data(Detector, _TrainingDataset, AttributeNames, FeatureTypes, _AttributeScales, Instances, _ReferenceScores, _Cache, Diagnostics),
		length(Instances, NumInstances),
		memberchk(options(Options), Diagnostics),
		format('Local Outlier Factor Anomaly Detector~n', []),
		format('====================================~n~n', []),
		format('Training instances: ~w~n', [NumInstances]),
		format('Features:           ~w~n', [AttributeNames]),
		format('Feature types:      ~w~n', [FeatureTypes]),
		^^print_anomaly_detector_template(Detector),
		format('Options:            ~w~n', [Options]).

	anomaly_detector_export_template(Functor, Template) :-
		Template =.. [Functor, 'Detector'].

	anomaly_detector_term_template(lof_detector(_TrainingDataset, _AttributeNames, _FeatureTypes, _AttributeScales, _Instances, _ReferenceScores, _Cache, _Diagnostics), lof_detector('TrainingDataset', 'AttributeNames', 'FeatureTypes', 'AttributeScales', 'Instances', 'ReferenceScores', 'Cache', 'Diagnostics')).

	detector_data(lof_detector(TrainingDataset, AttributeNames, FeatureTypes, AttributeScales, Instances, ReferenceScores, Cache, Diagnostics), TrainingDataset, AttributeNames, FeatureTypes, AttributeScales, Instances, ReferenceScores, Cache, Diagnostics).

	check_instance_ids([], _Seen).
	check_instance_ids([Id-_-_| Instances], Seen) :-
		(	\+ ground(Id) ->
			instantiation_error
		;	\+ integer(Id) ->
			type_error(integer, Id)
		;	member(Id, Seen) ->
			domain_error(unique_example_id, Id)
		;	check_instance_ids(Instances, [Id| Seen])
		).

	build_training_cache(Types, Scales, Instances, Options, Scores, lof_cache(K, Metric, Graph, ReferenceIndex, Maximum)) :-
		length(Instances, Count),
		Available is max(0, Count - 1),
		neighbor_count(Options, Available, K),
		^^option(distance_metric(Metric), Options),
		(	Count =< 256 ->
			symmetric_distance_pairs(Instances, Types, Scales, Metric, DistancePairs),
			group_sorted_by_key(DistancePairs, Groups),
			avltree::as_dictionary(Groups, Rows)
		;	Rows = none
		),
		findall(
			Id-node(Neighbors, KDistance, pending),
			(	member(Id-_-Values, Instances),
				(	Rows == none ->
					remove_instance(Instances, Id, Candidates),
					compact_neighbors(Values, Types, Scales, Candidates, Metric, K, Neighbors)
				;	(	avltree::lookup(Id, Distances, Rows) ->
						true
					;	Distances = []
					),
					select_compact_distances(Distances, K, Neighbors)
				),
				(	Neighbors == [] ->
					KDistance = 0.0
				;	last_neighbor_distance(Neighbors, KDistance-_)
				)
			),
			Neighborhoods
		),
		avltree::as_dictionary(Neighborhoods, NeighborhoodIndex),
		graph_density_pairs(Neighborhoods, NeighborhoodIndex, DensityPairs),
		avltree::as_dictionary(DensityPairs, Graph),
		graph_reference_scores(Instances, Graph, Scores),
		reference_score_index(Instances, Scores, ReferencePairs),
		avltree::as_dictionary(ReferencePairs, ReferenceIndex),
		max(Scores, Maximum).

	symmetric_distance_pairs([], _Types, _Scales, _Metric, []).
	symmetric_distance_pairs([Id-_-Values| Instances], Types, Scales, Metric, Pairs) :-
		findall(
			Entry,
			(	member(OtherId-_-OtherValues, Instances),
				compute_metric_distance(Metric, Values, OtherValues, Types, Scales, Distance),
				( Entry = Id-(Distance-OtherId) ; Entry = OtherId-(Distance-Id) )
			),
			Head
		),
		symmetric_distance_pairs(Instances, Types, Scales, Metric, Tail),
		append(Head, Tail, Pairs).

	select_compact_distances(Distances, K, Neighbors) :-
		msort(Distances, Sorted),
		take_at_most(K, Sorted, Prefix),
		(	Prefix == [] ->
			Neighbors = []
		;	last_neighbor_distance(Prefix, Boundary-_),
			compact_boundary(Sorted, Boundary, Neighbors)
		).

	graph_density_pairs([], _Index, []).
	graph_density_pairs([Id-node(Neighbors, KDistance, _)| Entries], Index, [Id-node(Neighbors, KDistance, Lrd)| Pairs]) :-
		cached_density(Neighbors, Index, Lrd),
		graph_density_pairs(Entries, Index, Pairs).

	cached_density([], _Graph, 1.0) :-
		!.
	cached_density(Neighbors, Graph, Lrd) :-
		cached_reachability_sum(Neighbors, Graph, 0.0, Sum),
		length(Neighbors, Count),
		(	Sum =< 0.0 ->
			Lrd = 1.0e12
		;	Lrd is float(Count / Sum)
		).

	cached_reachability_sum([], _Graph, Sum, Sum).
	cached_reachability_sum([Distance-Id| Neighbors], Graph, Sum0, Sum) :-
		avltree::lookup(Id, node(_, KDistance, _), Graph),
		Sum1 is Sum0 + max(Distance, KDistance),
		cached_reachability_sum(Neighbors, Graph, Sum1, Sum).

	cached_lof([], _Graph, _Lrd, 1.0) :-
		!.
	cached_lof(Neighbors, Graph, Lrd, Raw) :-
		cached_ratio_sum(Neighbors, Graph, Lrd, 0.0, Sum),
		length(Neighbors, Count),
		Raw is float(Sum / Count).

	cached_ratio_sum([], _Graph, _Lrd, Sum, Sum).
	cached_ratio_sum([_-Id| Neighbors], Graph, Lrd, Sum0, Sum) :-
		avltree::lookup(Id, node(_, _, NeighborLrd), Graph),
		Sum1 is Sum0 + NeighborLrd / Lrd,
		cached_ratio_sum(Neighbors, Graph, Lrd, Sum1, Sum).

	graph_reference_scores([], _Graph, []).
	graph_reference_scores([Id-_-_| Instances], Graph, [Raw| Scores]) :-
		avltree::lookup(Id, node(Neighbors, _, Lrd), Graph),
		cached_lof(Neighbors, Graph, Lrd, Raw),
		graph_reference_scores(Instances, Graph, Scores).

	cached_raw_score(Values, Types, Scales, Instances, Options, lof_cache(_, Metric, Graph, _, _), Raw) :-
		length(Instances, Count),
		neighbor_count(Options, Count, K),
		compact_neighbors(Values, Types, Scales, Instances, Metric, K, Neighbors),
		cached_density(Neighbors, Graph, Lrd),
		cached_lof(Neighbors, Graph, Lrd, Raw).

	valid_training_cache(lof_cache(K, Metric, Graph, Index, Maximum), Instances, Scores, Options) :-
		length(Instances, Count),
		Available is max(0, Count - 1),
		neighbor_count(Options, Available, K),
		^^option(distance_metric(Metric), Options),
		max(Scores, ExpectedMaximum),
		Maximum == ExpectedMaximum,
		reference_score_index(Instances, Scores, Pairs),
		avltree::as_dictionary(Pairs, ExpectedIndex),
		Index == ExpectedIndex,
		avltree::valid(Graph),
		avltree::size(Graph, Count),
		valid_graph_entries(Instances, Graph).

	valid_graph_entries([], _Graph).
	valid_graph_entries([Id-_-_| Instances], Graph) :-
		avltree::lookup(Id, node(Neighbors, KDistance, Lrd), Graph),
		valid(non_negative_number, KDistance),
		number(Lrd),
		Lrd > 0.0,
		valid(list(pair), Neighbors),
		valid_graph_neighbors(Neighbors, Id, Graph),
		valid_graph_entries(Instances, Graph).

	valid_graph_neighbors([], _Self, _Graph).
	valid_graph_neighbors([Distance-Id| Neighbors], Self, Graph) :-
		Id \== Self,
		number(Distance),
		Distance >= 0.0,
		avltree::lookup(Id, _, Graph),
		valid_graph_neighbors(Neighbors, Self, Graph).

	compact_neighbors(Values, Types, Scales, Instances, Metric, K, Neighbors) :-
		length(Instances, Available),
		(	K =:= 0 ->
			Neighbors = []
		;	Available < 128 ->
			findall(
				Distance-Id,
				(	member(Id-_-OtherValues, Instances),
					compute_metric_distance(Metric, Values, OtherValues, Types, Scales, Distance)
				),
				Distances
			),
			select_compact_distances(Distances, K, Neighbors)
		;	K * 2 >= Available ->
			findall(
				Distance-Id,
				(	member(Id-_-OtherValues, Instances),
					compute_metric_distance(Metric, Values, OtherValues, Types, Scales, Distance)
				),
				Distances
			),
			msort(Distances, Sorted),
			take_at_most(K, Sorted, Prefix),
			(	Prefix == [] ->
				Neighbors = []
			;	last_neighbor_distance(Prefix, Boundary-_),
				compact_boundary(Sorted, Boundary, Neighbors)
			)
		; binary_heap_max::as_heap([], Empty),
			neighbor_cutoff(Instances, Values, Types, Scales, Metric, K, Empty, 0, Heap, Size),
			(	Size =:= 0 ->
				Neighbors = []
			;	binary_heap_max::top(Heap, Boundary, _),
				findall(
					Distance-Id,
					(	member(Id-_-OtherValues, Instances),
						compute_metric_distance(Metric, Values, OtherValues, Types, Scales, Distance),
						Distance =< Boundary
					),
					Retained
				),
				msort(Retained, Neighbors)
			)
		).

	compact_boundary([], _Boundary, []).
	compact_boundary([Distance-Id| Distances], Boundary, Neighbors) :-
		(	Distance =< Boundary ->
			Neighbors = [Distance-Id| Rest],
			compact_boundary(Distances, Boundary, Rest)
		; 	Neighbors = []
		).

	neighbor_cutoff([], _Values, _Types, _Scales, _Metric, _K, Heap, Size, Heap, Size).
	neighbor_cutoff([Id-_-OtherValues| Instances], Values, Types, Scales, Metric, K, Heap0, Size0, Heap, Size) :-
		(	compute_metric_distance(Metric, Values, OtherValues, Types, Scales, Distance) ->
			(	Size0 < K ->
				binary_heap_max::insert(Distance, Id, Heap0, Heap1),
				Size1 is Size0 + 1
			;	binary_heap_max::top(Heap0, Worst, _),
				(	Distance < Worst ->
					binary_heap_max::delete(Heap0, _, _, Reduced),
					binary_heap_max::insert(Distance, Id, Reduced, Heap1)
				;	Heap1 = Heap0
				),
				Size1 = Size0
			)
		;	Heap1 = Heap0,
			Size1 = Size0
		),
		neighbor_cutoff(Instances, Values, Types, Scales, Metric, K, Heap1, Size1, Heap, Size).

	valid_attribute_names(AttributeNames) :-
		valid(list(atom), AttributeNames),
		AttributeNames \== [].

	valid_feature_types(FeatureTypes, AttributeNames) :-
		valid(list(atom), FeatureTypes),
		length(FeatureTypes, Count),
		length(AttributeNames, Count),
		valid_feature_types_(FeatureTypes).

	valid_feature_types_([]).
	valid_feature_types_([FeatureType| FeatureTypes]) :-
		memberchk(FeatureType, [numeric, categorical]),
		valid_feature_types_(FeatureTypes).

	valid_attribute_scales(AttributeScales, AttributeNames) :-
		valid(list(number), AttributeScales),
		length(AttributeScales, Count),
		length(AttributeNames, Count),
		valid_attribute_scales_(AttributeScales).

	valid_attribute_scales_([]).
	valid_attribute_scales_([AttributeScale| AttributeScales]) :-
		AttributeScale > 0.0,
		valid_attribute_scales_(AttributeScales).

	valid_training_instances(Instances, FeatureTypes) :-
		valid(list(compound), Instances),
		Instances \== [],
		valid_training_instances_(Instances, FeatureTypes).

	valid_training_instances_([], _).
	valid_training_instances_([Id-Class-Values| Instances], FeatureTypes) :-
		valid(integer, Id),
		valid(atom, Class),
		valid_feature_values(FeatureTypes, Values),
		valid_training_instances_(Instances, FeatureTypes).

	valid_feature_values([], []).
	valid_feature_values([Type| Types], [Value| Values]) :-
		(	Value == '$missing' ->
			true
		;	Type == numeric ->
			number(Value)
		;	atom(Value)
		),
		valid_feature_values(Types, Values).

	check_feature_values([], []).
	check_feature_values([Type| Types], [Value| Values]) :-
		(	\+ ground(Value) ->
			instantiation_error
		;	Value == '$missing' ->
			true
		; Type == numeric ->
			(	number(Value) ->
				true
			;	type_error(number, Value)
			)
		;	(	atom(Value) ->
				true
			;	type_error(atom, Value)
			)
		),
		check_feature_values(Types, Values).

	valid_reference_scores(ReferenceScores, Instances) :-
		valid(list(non_negative_number), ReferenceScores),
		length(ReferenceScores, Count),
		length(Instances, Count).

	valid_detector_options(Options) :-
		valid(list(compound), Options),
		catch(^^check_options(Options), _Error, fail).

	valid_detector_diagnostics(Dataset, AttributeNames, FeatureTypes, Instances, ReferenceScores, Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(lof_anomaly_detector), Diagnostics),
		memberchk(training_dataset(Dataset), Diagnostics),
		memberchk(attribute_names(AttributeNames), Diagnostics),
		memberchk(feature_types(FeatureTypes), Diagnostics),
		length(Instances, ExampleCount),
		memberchk(example_count(ExampleCount), Diagnostics),
		length(ReferenceScores, ReferenceScoreCount),
		memberchk(reference_score_count(ReferenceScoreCount), Diagnostics),
		memberchk(options(Options), Diagnostics),
		valid_detector_options(Options).

	determine_feature_types([], []).
	determine_feature_types([_-Values| Pairs], [Type| Types]) :-
		(	Values == continuous ->
			Type = numeric
		;	Type = categorical
		),
		determine_feature_types(Pairs, Types).

	compute_attribute_scales([], _Instances, []).
	compute_attribute_scales([Type| Types], Instances, [Scale| Scales]) :-
		instance_column(Instances, NumericValues, Tails),
		(	Type == numeric, NumericValues \== [] ->
			min_max(NumericValues, Minimum, Maximum),
			Range is float(Maximum - Minimum),
			(	Range > 0.0 ->
				Scale = Range
			;	Scale = 1.0
			)
		;	Scale = 1.0
		),
		compute_attribute_scales(Types, Tails, Scales).

	instance_column([], [], []).
	instance_column([Id-Class-[Value| Values]| Instances], NumericValues, [Id-Class-Values| Tails]) :-
		(	number(Value) ->
			NumericValues = [Value| Rest]
		;	NumericValues = Rest
		),
		instance_column(Instances, Rest, Tails).

	extract_values([], _, []).
	extract_values([Attribute| Attributes], AttributeValues, [Value| Values]) :-
		(	member(Attribute-Value, AttributeValues) ->
			true
		;	existence_error(attribute, Attribute)
		),
		extract_values(Attributes, AttributeValues, Values).

	sanitize_training_values([], []).
	sanitize_training_values([Value| Values], [Sanitized| SanitizedValues]) :-
		(	var(Value) ->
			Sanitized = '$missing'
		;	Sanitized = Value
		),
		sanitize_training_values(Values, SanitizedValues).

	sanitize_input_values([], []).
	sanitize_input_values([Value| Values], [Sanitized| SanitizedValues]) :-
		(	var(Value) ->
			Sanitized = '$missing'
		;	Sanitized = Value
		),
		sanitize_input_values(Values, SanitizedValues).

	raw_lof(Values, FeatureTypes, AttributeScales, Instances, Options, RawScore) :-
		raw_lof_population(Values, FeatureTypes, AttributeScales, Instances, Instances, Options, RawScore).

	raw_lof_population(Values, FeatureTypes, AttributeScales, Instances, Candidates, Options, RawScore) :-
		length(Candidates, AvailableCount),
		(	AvailableCount > 0 ->
			neighbor_count(Options, AvailableCount, RequestedK),
			length(Instances, TrainingCount),
			GraphCount is max(0, TrainingCount - 1),
			neighbor_count(Options, GraphCount, GraphK),
			k_nearest_neighbors(Values, FeatureTypes, AttributeScales, Candidates, Options, RequestedK, Neighbors),
			length(Neighbors, NeighborCount),
			(	NeighborCount > 0 ->
				local_reachability_density(Values, FeatureTypes, AttributeScales, Instances, Options, GraphK, Neighbors, QueryLrd),
				neighbor_lrd_ratio_sum(Neighbors, FeatureTypes, AttributeScales, Instances, Options, GraphK, QueryLrd, 0.0, RatioSum),
				RawScore is float(RatioSum / NeighborCount)
			;	RawScore = 1.0
			)
		;	RawScore = 1.0
		).

	neighbor_count(Options, AvailableCount, K) :-
		^^option(k(K0), Options),
		(	AvailableCount =< 1 ->
			K = AvailableCount
		;	K0 =< AvailableCount ->
			K = K0
		;	K = AvailableCount
		).

	local_reachability_density(Values, FeatureTypes, AttributeScales, Instances, Options, K, Neighbors, Lrd) :-
		reachability_distance_sum(Values, FeatureTypes, AttributeScales, Instances, Options, K, Neighbors, 0.0, ReachabilitySum),
		length(Neighbors, NeighborCount),
		(	ReachabilitySum =< 0.0 ->
			Lrd = 1.0e12
		;	Lrd is float(NeighborCount / ReachabilitySum)
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
		(	Count > 0 ->
			(	K =< Count -> K2 = K ; K2 = Count ),
			k_nearest_neighbors(NeighborValues, FeatureTypes, AttributeScales, OtherInstances, Options, K2, NeighborNeighbors),
			(	NeighborNeighbors == [] ->
				KDistance = 0.0
			;	k_distance_from_neighbors(NeighborNeighbors, KDistance)
			)
		;	KDistance = 0.0
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
		(	Count > 0 ->
			(	K =< Count -> K2 = K ; K2 = Count ),
			k_nearest_neighbors(NeighborValues, FeatureTypes, AttributeScales, OtherInstances, Options, K2, Neighbors),
			length(Neighbors, NeighborCount),
			(	NeighborCount > 0 ->
				local_reachability_density(NeighborValues, FeatureTypes, AttributeScales, Instances, Options, K, Neighbors, Lrd)
			;	Lrd = 1.0
			)
		;	Lrd = 1.0
		).

	k_nearest_neighbors(Values, FeatureTypes, AttributeScales, Instances, Options, K, Neighbors) :-
		^^option(distance_metric(Metric), Options),
		findall(
			Distance-Id-Class-OtherValues,
			(	member(Id-Class-OtherValues, Instances),
				compute_metric_distance(Metric, Values, OtherValues, FeatureTypes, AttributeScales, Distance)
			),
			Distances
		),
		msort(Distances, SortedDistances),
		take_at_most(K, SortedDistances, Prefix),
		(	Prefix == [] ->
			Neighbors = []
		;	k_distance_from_neighbors(Prefix, Boundary),
			neighbors_within_boundary(SortedDistances, Boundary, Neighbors)
		).

	neighbors_within_boundary([], _Boundary, []).
	neighbors_within_boundary([Distance-Id-Class-Values| Candidates], Boundary, Neighbors) :-
		(	Distance =< Boundary ->
			Neighbors = [Distance-Id-Class-Values| Rest],
			neighbors_within_boundary(Candidates, Boundary, Rest)
		;	Neighbors = []
		).

	take_at_most(_Count, [], []) :-
		!.
	take_at_most(0, _List, []) :-
		!.
	take_at_most(Count, [Item| Items], [Item| Taken]) :-
		Count > 0,
		Remaining is Count - 1,
		take_at_most(Remaining, Items, Taken).

	k_distance_from_neighbors(Neighbors, KDistance) :-
		findall(Distance, member(Distance-_-_-_, Neighbors), Distances),
		last_neighbor_distance(Distances, KDistance).

	raw_score_pairs(Dataset, AttributeNames, FeatureTypes, AttributeScales, Instances, Options, Cache, RawPairs) :-
		findall(
			RawScore-Id-Class,
			(	Dataset::example(Id, Class, AttributeValues),
				extract_values(AttributeNames, AttributeValues, RawValues),
				sanitize_input_values(RawValues, Values),
				check_feature_values(FeatureTypes, Values),
				cached_raw_score(Values, FeatureTypes, AttributeScales, Instances, Options, Cache, RawScore)
			),
			RawPairs
		).

	raw_score_pairs_training([], [], []).
	raw_score_pairs_training([Id-Class-_Values| Instances], [RawScore| ReferenceScores], [RawScore-Id-Class| RawPairs]) :-
		raw_score_pairs_training(Instances, ReferenceScores, RawPairs).

	raw_score_pairs_training_dataset(Dataset, AttributeNames, FeatureTypes, AttributeScales, Instances, ReferenceIndex, Options, Cache, RawPairs) :-
		findall(
			RawScore-Id-Class,
			(	Dataset::example(Id, Class, AttributeValues),
				(	avltree::lookup(Id-Class, RawScore, ReferenceIndex) ->
					true
				;	extract_values(AttributeNames, AttributeValues, RawValues),
					sanitize_input_values(RawValues, Values),
					check_feature_values(FeatureTypes, Values),
					cached_raw_score(Values, FeatureTypes, AttributeScales, Instances, Options, Cache, RawScore)
				)
			),
			RawPairs
		).

	reference_score_index([], [], []).
	reference_score_index([Id-Class-_| Instances], [RawScore| ReferenceScores], [Id-Class-RawScore| Pairs]) :-
		reference_score_index(Instances, ReferenceScores, Pairs).

	training_raw_scores(FeatureTypes, AttributeScales, Instances, Options, ReferenceScores) :-
		findall(
			RawScore,
			(
				member(Id-_Class-Values, Instances),
				remove_instance(Instances, Id, OtherInstances),
				raw_lof_population(Values, FeatureTypes, AttributeScales, Instances, OtherInstances, Options, RawScore)
			),
			ReferenceScores
		).

	normalize_raw_pairs_maximum([], _Maximum, []).
	normalize_raw_pairs_maximum([RawScore-Id-Class| RawPairs], Maximum, [Score-Id-Class| ScoredPairs]) :-
		normalize_against_maximum(RawScore, Maximum, Score),
		normalize_raw_pairs_maximum(RawPairs, Maximum, ScoredPairs).

	normalize_against_maximum(RawScore, MaximumReferenceScore, Score) :-
		(	RawScore =< 1.0 ->
			Score = 0.0
		;	MaximumReferenceScore =< 1.0 ->
			Score = 1.0
		;	Scale is MaximumReferenceScore - 1.0,
			Normalized is float((RawScore - 1.0) / Scale),
			clamp_score(Normalized, Score)
		).

	clamp_score(Score0, 0.0) :-
		Score0 =< 0.0,
		!.
	clamp_score(Score0, 1.0) :-
		Score0 >= 1.0,
		!.
	clamp_score(Score, Score).

	compute_metric_distance(euclidean, Values1, Values2, FeatureTypes, AttributeScales, Distance) :-
		sum_squared_components(Values1, Values2, FeatureTypes, AttributeScales, 0.0, 0, SumSq, Count),
		Count > 0,
		normalize_component_distance(Count, SumSq, Normalized),
		Distance is sqrt(Normalized).
	compute_metric_distance(manhattan, Values1, Values2, FeatureTypes, AttributeScales, Distance) :-
		sum_absolute_components(Values1, Values2, FeatureTypes, AttributeScales, 0.0, 0, SumAbs, Count),
		Count > 0,
		normalize_component_distance(Count, SumAbs, Distance).
	compute_metric_distance(chebyshev, Values1, Values2, FeatureTypes, AttributeScales, Distance) :-
		max_component_distance(Values1, Values2, FeatureTypes, AttributeScales, 0.0, 0, Distance, Count),
		Count > 0.
	compute_metric_distance(minkowski, Values1, Values2, FeatureTypes, AttributeScales, Distance) :-
		P = 3.0,
		sum_power_components(Values1, Values2, FeatureTypes, AttributeScales, P, 0.0, 0, SumPow, Count),
		Count > 0,
		normalize_component_distance(Count, SumPow, Normalized),
		Distance is Normalized ** (1.0 / P).

	normalize_component_distance(Count, Raw, Normalized) :-
		Normalized is float(Raw / Count).

	sum_squared_components([], [], [], [], Sum, Count, Sum, Count).
	sum_squared_components([Value1| Values1], [Value2| Values2], [Type| Types], [Scale| Scales], Sum0, Count0, Sum, Count) :-
		(	comparable_component(Type, Value1, Value2, Scale, Delta) ->
			Sum1 is Sum0 + Delta * Delta,
			Count1 is Count0 + 1
		;	Sum1 = Sum0,
			Count1 = Count0
		),
		sum_squared_components(Values1, Values2, Types, Scales, Sum1, Count1, Sum, Count).

	sum_absolute_components([], [], [], [], Sum, Count, Sum, Count).
	sum_absolute_components([Value1| Values1], [Value2| Values2], [Type| Types], [Scale| Scales], Sum0, Count0, Sum, Count) :-
		(	comparable_component(Type, Value1, Value2, Scale, Delta) ->
			Sum1 is Sum0 + abs(Delta),
			Count1 is Count0 + 1
		;	Sum1 = Sum0,
			Count1 = Count0
		),
		sum_absolute_components(Values1, Values2, Types, Scales, Sum1, Count1, Sum, Count).

	max_component_distance([], [], [], [], Maximum, Count, Maximum, Count).
	max_component_distance([Value1| Values1], [Value2| Values2], [Type| Types], [Scale| Scales], Maximum0, Count0, Maximum, Count) :-
		(	comparable_component(Type, Value1, Value2, Scale, Delta) ->
			Component is abs(Delta),
			Maximum1 is max(Maximum0, Component),
			Count1 is Count0 + 1
		;	Maximum1 = Maximum0,
			Count1 = Count0
		),
		max_component_distance(Values1, Values2, Types, Scales, Maximum1, Count1, Maximum, Count).

	sum_power_components([], [], [], [], _, Sum, Count, Sum, Count).
	sum_power_components([Value1| Values1], [Value2| Values2], [Type| Types], [Scale| Scales], P, Sum0, Count0, Sum, Count) :-
		(	comparable_component(Type, Value1, Value2, Scale, Delta) ->
			Component is abs(Delta) ** P,
			Sum1 is Sum0 + Component,
			Count1 is Count0 + 1
		;	Sum1 = Sum0,
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
		(	Value1 == Value2 ->
			Delta = 0.0
		;	Delta = 1.0
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
	default_option(baseline_class_values([normal])).
	default_option(baseline_selection_policy(reject)).

	valid_option(k(K)) :-
		integer(K),
		K > 0.
	valid_option(distance_metric(Metric)) :-
		once((Metric == euclidean; Metric == manhattan; Metric == chebyshev; Metric == minkowski)).
	valid_option(anomaly_threshold(Threshold)) :-
		valid(probability, Threshold).
	valid_option(baseline_class_values(BaselineClassValues)) :-
		^^valid_baseline_class_values(BaselineClassValues).
	valid_option(baseline_selection_policy(Policy)) :-
		once((Policy == reject; Policy == filter)).

:- end_object.
