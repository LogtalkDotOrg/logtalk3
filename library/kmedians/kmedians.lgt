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


:- object(kmedians,
	imports(clusterer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-23,
		comment is 'k-Medians clusterer for continuous datasets. Learns from a dataset object implementing the ``clustering_dataset_protocol`` protocol and returns a clusterer term that can be used for assigning new instances to clusters and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses an iterative median-update algorithm with deterministic initialization.',
			'Feature handling' - 'Supports continuous attributes only. Continuous attributes can be standardized using z-score scaling before clustering.',
			'Distance metric' - 'Uses Manhattan distance for assignment and convergence checks.',
			'Initialization' - 'Supports ``first_k`` initialization and a deterministic ``spread`` initialization that repeatedly chooses the farthest example from the medians selected so far.',
			'Empty clusters' - 'If an iteration leaves a cluster empty, its median is kept unchanged from the previous iteration.',
			'Clusterer representation' - 'The learned clusterer is represented by default as ``kmedians_clusterer(Encoders, Medians, Options)`` where ``Encoders`` stores the feature encoding metadata, ``Medians`` stores the learned median vectors in cluster-id order, and ``Options`` stores the effective training options.'
		],
		see_also is [clusterer_protocol, clustering_dataset_protocol, kmeans, kmedoids]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, nth1/3
	]).

	:- uses(numberlist, [
		manhattan_distance/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Clusterer, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		findall(
			Id-AttributeValues,
			Dataset::example(Id, AttributeValues),
			Examples
		),
		^^check_examples(Dataset, AttributeNames, Examples),
		^^build_encoders(AttributeNames, Examples, Options, Encoders),
		^^examples_to_rows(Examples, Encoders, Rows),
		length(Rows, Count),
		^^option(k(K), Options),
		^^check_cluster_count(K, Count),
		^^option(initialization(Initialization), Options),
		initialize_medians(Initialization, K, Rows, InitialMedians),
		optimize_medians(Rows, Options, 0, InitialMedians, Medians),
		Clusterer = kmedians_clusterer(Encoders, Medians, Options),
		!.

	cluster(Clusterer, Instance, Cluster) :-
		Clusterer =.. [_, Encoders, Medians, _Options],
		^^encode_instance(Encoders, Instance, Features),
		nearest_median(Medians, Features, Cluster, _Distance).

	clusterer_diagnostics_data(kmedians_clusterer(_Encoders, Medians, Options), Diagnostics) :-
		length(Medians, MedianCount),
		Diagnostics = [
			model(kmedians),
			median_count(MedianCount),
			options(Options)
		].

	initialize_medians(first_k, K, Rows, Medians) :-
		^^take_first_k(K, Rows, Medians).
	initialize_medians(spread, K, [_-First| Rows], [First| Medians]) :-
		Remaining is K - 1,
		select_spread_medians(Remaining, Rows, [First], Medians).

	select_spread_medians(0, _, _, []) :-
		!.
	select_spread_medians(Count, Candidates, Selected, [Vector| Medians]) :-
		Count > 0,
		farthest_candidate(Candidates, Selected, BestCandidate, RemainingCandidates),
		BestCandidate = _-Vector,
		NextCount is Count - 1,
		select_spread_medians(NextCount, RemainingCandidates, [Vector| Selected], Medians).

	farthest_candidate([Candidate| Candidates], Selected, BestCandidate, RemainingCandidates) :-
		Candidate = _-Vector,
		closest_median_distance(Vector, Selected, Distance),
		farthest_candidate(Candidates, Selected, Candidate, Distance, BestCandidate),
		^^remove_candidate(BestCandidate, [Candidate| Candidates], RemainingCandidates).

	farthest_candidate([], _, BestCandidate, _BestDistance, BestCandidate).
	farthest_candidate([Candidate| Candidates], Selected, BestCandidate0, BestDistance0, BestCandidate) :-
		Candidate = _-Vector,
		closest_median_distance(Vector, Selected, Distance),
		(   Distance > BestDistance0 ->
			BestCandidate1 = Candidate,
			BestDistance1 = Distance
		;   BestCandidate1 = BestCandidate0,
			BestDistance1 = BestDistance0
		),
		farthest_candidate(Candidates, Selected, BestCandidate1, BestDistance1, BestCandidate).

	optimize_medians(Rows, Options, Iteration, Medians0, Medians) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(   Iteration >= MaximumIterations ->
			Medians = Medians0
		;   assign_rows(Rows, Medians0, Assignments),
			recompute_medians(Medians0, Assignments, 1, Medians1),
			max_median_shift(Medians0, Medians1, Shift),
			^^option(tolerance(Tolerance), Options),
			(   Shift =< Tolerance ->
				Medians = Medians1
			;   NextIteration is Iteration + 1,
				optimize_medians(Rows, Options, NextIteration, Medians1, Medians)
			)
		).

	assign_rows([], _, []).
	assign_rows([_-Vector| Rows], Medians, [Cluster-Vector| Assignments]) :-
		nearest_median(Medians, Vector, Cluster, _Distance),
		assign_rows(Rows, Medians, Assignments).

	nearest_median([Median| Medians], Vector, Cluster, Distance) :-
		manhattan_distance(Vector, Median, InitialDistance),
		nearest_median(Medians, Vector, 2, 1, InitialDistance, Cluster, Distance).

	nearest_median([], _Vector, _Index, BestCluster, BestDistance, BestCluster, BestDistance).
	nearest_median([Median| Medians], Vector, Index, BestCluster0, BestDistance0, BestCluster, BestDistance) :-
		manhattan_distance(Vector, Median, Distance0),
		(   Distance0 < BestDistance0 ->
			BestCluster1 = Index,
			BestDistance1 = Distance0
		;   BestCluster1 = BestCluster0,
			BestDistance1 = BestDistance0
		),
		NextIndex is Index + 1,
		nearest_median(Medians, Vector, NextIndex, BestCluster1, BestDistance1, BestCluster, BestDistance).

	closest_median_distance(Vector, [Median| Medians], Distance) :-
		manhattan_distance(Vector, Median, InitialDistance),
		closest_median_distance(Medians, Vector, InitialDistance, Distance).

	closest_median_distance([], _Vector, BestDistance, BestDistance).
	closest_median_distance([Median| Medians], Vector, BestDistance0, BestDistance) :-
		manhattan_distance(Vector, Median, Distance),
		(   Distance < BestDistance0 ->
			BestDistance1 = Distance
		;   BestDistance1 = BestDistance0
		),
		closest_median_distance(Medians, Vector, BestDistance1, BestDistance).

	recompute_medians([], _, _, []).
	recompute_medians([Median0| Medians0], Assignments, Cluster, [Median| Medians]) :-
		assigned_vectors(Cluster, Assignments, Vectors),
		(   Vectors == [] ->
			Median = Median0
		;   median_vectors(Vectors, Median)
		),
		NextCluster is Cluster + 1,
		recompute_medians(Medians0, Assignments, NextCluster, Medians).

	assigned_vectors(_, [], []) :-
		!.
	assigned_vectors(Cluster, [Cluster-Vector| Assignments], [Vector| Vectors]) :-
		!,
		assigned_vectors(Cluster, Assignments, Vectors).
	assigned_vectors(Cluster, [_OtherCluster-_| Assignments], Vectors) :-
		assigned_vectors(Cluster, Assignments, Vectors).

	median_vectors(Vectors, Medians) :-
		transpose(Vectors, Columns),
		medians_of_columns(Columns, Medians).

	transpose([[]| _], []) :-
		!.
	transpose(Vectors, [Column| Columns]) :-
		first_column(Vectors, Column, RemainingVectors),
		transpose(RemainingVectors, Columns).

	first_column([], [], []).
	first_column([[Value| Values]| Vectors], [Value| Column], [Values| RemainingVectors]) :-
		first_column(Vectors, Column, RemainingVectors).

	medians_of_columns([], []).
	medians_of_columns([Column| Columns], [Median| Medians]) :-
		numeric_sort(Column, SortedColumn),
		median_of_sorted(SortedColumn, Median),
		medians_of_columns(Columns, Medians).

	numeric_sort([], []).
	numeric_sort([Value| Values], Sorted) :-
		numeric_sort(Values, SortedValues),
		insert_sorted(Value, SortedValues, Sorted).

	insert_sorted(Value, [], [Value]) :-
		!.
	insert_sorted(Value, [Head| Tail], [Value, Head| Tail]) :-
		Value =< Head,
		!.
	insert_sorted(Value, [Head| Tail], [Head| SortedTail]) :-
		insert_sorted(Value, Tail, SortedTail).

	median_of_sorted(Sorted, Median) :-
		length(Sorted, Count),
		Middle is Count // 2,
		(   1 is Count mod 2 ->
			Index is Middle + 1,
			nth1(Index, Sorted, Median)
		;   LeftIndex is Middle,
			RightIndex is Middle + 1,
			nth1(LeftIndex, Sorted, LeftValue),
			nth1(RightIndex, Sorted, RightValue),
			Median is (LeftValue + RightValue) / 2.0
		).

	max_median_shift([], [], 0.0).
	max_median_shift([Median0| Medians0], [Median1| Medians1], MaxShift) :-
		manhattan_distance(Median0, Median1, Shift),
		max_median_shift(Medians0, Medians1, RestMaxShift),
		MaxShift is max(Shift, RestMaxShift).

	print_clusterer(kmedians_clusterer(Encoders, Medians, Options)) :-
		format('k-Medians Clusterer~n', []),
		format('===================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nMedians:~n', []),
		print_medians(Medians, 1).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		format('  ~w (continuous, mean=~4f, scale=~4f)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).

	print_medians([], _).
	print_medians([Median| Medians], Cluster) :-
		format('  cluster ~d: ~w~n', [Cluster, Median]),
		NextCluster is Cluster + 1,
		print_medians(Medians, NextCluster).

	default_option(k(2)).
	default_option(maximum_iterations(100)).
	default_option(tolerance(1.0e-6)).
	default_option(initialization(spread)).
	default_option(feature_scaling(on)).

	valid_option(k(K)) :-
		valid(positive_integer, K).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance >= 0.0.
	valid_option(initialization(Initialization)) :-
		once((Initialization == first_k; Initialization == spread)).
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).

:- end_object.
