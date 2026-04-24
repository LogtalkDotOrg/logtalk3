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


:- object(kmedoids,
	imports(clusterer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-23,
		comment is 'k-Medoids clusterer for continuous datasets. Learns from a dataset object implementing the ``clustering_dataset_protocol`` protocol and returns a clusterer term that can be used for assigning new instances to clusters and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses an iterative medoid-update algorithm with deterministic initialization and deterministic cluster assignments.',
			'Feature handling' - 'Supports continuous attributes only. Continuous attributes can be standardized using z-score scaling before clustering.',
			'Distance metrics' - 'Supports Euclidean and Manhattan distances.',
			'Initialization' - 'Supports ``first_k`` initialization and a deterministic ``spread`` initialization that repeatedly chooses the farthest example from the medoids selected so far.',
			'Empty clusters' - 'If an iteration leaves a cluster empty, its medoid is kept unchanged from the previous iteration.',
			'Clusterer representation' - 'The learned clusterer is represented by default as ``kmedoids_clusterer(Encoders, Medoids, Options)`` where ``Encoders`` stores the feature encoding metadata, ``Medoids`` stores the learned medoid vectors in cluster-id order, and ``Options`` stores the effective training options.'
		],
		see_also is [clusterer_protocol, clustering_dataset_protocol, kmeans]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(numberlist, [
		euclidean_distance/3, manhattan_distance/3
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
		initialize_medoids(Initialization, K, Rows, Options, InitialMedoids),
		optimize_medoids(Rows, Options, 0, InitialMedoids, Medoids),
		Clusterer = kmedoids_clusterer(Encoders, Medoids, Options).

	cluster(Clusterer, Instance, Cluster) :-
		Clusterer =.. [_, Encoders, Medoids, Options],
		^^encode_instance(Encoders, Instance, Features),
		nearest_medoid(Medoids, Features, Options, Cluster, _Distance).

	clusterer_diagnostics_data(kmedoids_clusterer(_Encoders, Medoids, Options), Diagnostics) :-
		length(Medoids, MedoidCount),
		Diagnostics = [
			model(kmedoids),
			medoid_count(MedoidCount),
			options(Options)
		].

	initialize_medoids(first_k, K, Rows, _Options, Medoids) :-
		^^take_first_k(K, Rows, Medoids).
	initialize_medoids(spread, K, [_-First| Rows], Options, [First| Medoids]) :-
		Remaining is K - 1,
		select_spread_medoids(Remaining, Rows, [First], Options, Medoids).

	select_spread_medoids(0, _, _, _, []) :-
		!.
	select_spread_medoids(Count, Candidates, Selected, Options, [Vector| Medoids]) :-
		Count > 0,
		farthest_candidate(Candidates, Selected, Options, BestCandidate, RemainingCandidates),
		BestCandidate = _-Vector,
		NextCount is Count - 1,
		select_spread_medoids(NextCount, RemainingCandidates, [Vector| Selected], Options, Medoids).

	farthest_candidate([Candidate| Candidates], Selected, Options, BestCandidate, RemainingCandidates) :-
		Candidate = _-Vector,
		closest_medoid_distance(Vector, Selected, Options, Distance),
		farthest_candidate(Candidates, Selected, Options, Candidate, Distance, BestCandidate),
		^^remove_candidate(BestCandidate, [Candidate| Candidates], RemainingCandidates).

	farthest_candidate([], _, _Options, BestCandidate, _BestDistance, BestCandidate).
	farthest_candidate([Candidate| Candidates], Selected, Options, BestCandidate0, BestDistance0, BestCandidate) :-
		Candidate = _-Vector,
		closest_medoid_distance(Vector, Selected, Options, Distance),
		(   Distance > BestDistance0 ->
			BestCandidate1 = Candidate,
			BestDistance1 = Distance
		;   BestCandidate1 = BestCandidate0,
			BestDistance1 = BestDistance0
		),
		farthest_candidate(Candidates, Selected, Options, BestCandidate1, BestDistance1, BestCandidate).

	optimize_medoids(Rows, Options, Iteration, Medoids0, Medoids) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(   Iteration >= MaximumIterations ->
			Medoids = Medoids0
		;   assign_rows(Rows, Medoids0, Options, Assignments),
			recompute_medoids(Medoids0, Assignments, Options, 1, Medoids1),
			max_medoid_shift(Medoids0, Medoids1, Options, Shift),
			^^option(tolerance(Tolerance), Options),
			(   Shift =< Tolerance ->
				Medoids = Medoids1
			;   NextIteration is Iteration + 1,
				optimize_medoids(Rows, Options, NextIteration, Medoids1, Medoids)
			)
		).

	assign_rows([], _, _, []).
	assign_rows([_-Vector| Rows], Medoids, Options, [Cluster-Vector| Assignments]) :-
		nearest_medoid(Medoids, Vector, Options, Cluster, _Distance),
		assign_rows(Rows, Medoids, Options, Assignments).

	nearest_medoid([Medoid| Medoids], Vector, Options, Cluster, Distance) :-
		distance(Options, Vector, Medoid, InitialDistance),
		nearest_medoid(Medoids, Vector, Options, 2, 1, InitialDistance, Cluster, Distance).

	nearest_medoid([], _Vector, _Options, _Index, BestCluster, BestDistance, BestCluster, BestDistance).
	nearest_medoid([Medoid| Medoids], Vector, Options, Index, BestCluster0, BestDistance0, BestCluster, BestDistance) :-
		distance(Options, Vector, Medoid, Distance0),
		(   Distance0 < BestDistance0 ->
			BestCluster1 = Index,
			BestDistance1 = Distance0
		;   BestCluster1 = BestCluster0,
			BestDistance1 = BestDistance0
		),
		NextIndex is Index + 1,
		nearest_medoid(Medoids, Vector, Options, NextIndex, BestCluster1, BestDistance1, BestCluster, BestDistance).

	closest_medoid_distance(Vector, [Medoid| Medoids], Options, Distance) :-
		distance(Options, Vector, Medoid, InitialDistance),
		closest_medoid_distance(Medoids, Vector, Options, InitialDistance, Distance).

	closest_medoid_distance([], _Vector, _Options, BestDistance, BestDistance).
	closest_medoid_distance([Medoid| Medoids], Vector, Options, BestDistance0, BestDistance) :-
		distance(Options, Vector, Medoid, Distance0),
		(   Distance0 < BestDistance0 ->
			BestDistance1 = Distance0
		;   BestDistance1 = BestDistance0
		),
		closest_medoid_distance(Medoids, Vector, Options, BestDistance1, BestDistance).

	recompute_medoids([], _, _, _, []).
	recompute_medoids([Medoid0| Medoids0], Assignments, Options, Cluster, [Medoid| Medoids]) :-
		assigned_vectors(Cluster, Assignments, Vectors),
		(   Vectors == [] ->
			Medoid = Medoid0
		;   best_medoid(Vectors, Options, Medoid)
		),
		NextCluster is Cluster + 1,
		recompute_medoids(Medoids0, Assignments, Options, NextCluster, Medoids).

	assigned_vectors(_, [], []) :-
		!.
	assigned_vectors(Cluster, [Cluster-Vector| Assignments], [Vector| Vectors]) :-
		!,
		assigned_vectors(Cluster, Assignments, Vectors).
	assigned_vectors(Cluster, [_OtherCluster-_| Assignments], Vectors) :-
		assigned_vectors(Cluster, Assignments, Vectors).

	best_medoid([Vector| Vectors], Options, Medoid) :-
		total_distance(Vector, [Vector| Vectors], Options, InitialCost),
		best_medoid(Vectors, [Vector| Vectors], Options, Vector, InitialCost, Medoid).

	best_medoid([], _Candidates, _Options, BestMedoid, _BestCost, BestMedoid).
	best_medoid([Candidate| Candidates], AllVectors, Options, BestMedoid0, BestCost0, BestMedoid) :-
		total_distance(Candidate, AllVectors, Options, Cost),
		(   Cost < BestCost0 ->
			BestMedoid1 = Candidate,
			BestCost1 = Cost
		;   BestMedoid1 = BestMedoid0,
			BestCost1 = BestCost0
		),
		best_medoid(Candidates, AllVectors, Options, BestMedoid1, BestCost1, BestMedoid).

	total_distance(_Candidate, [], _Options, 0.0) :-
		!.
	total_distance(Candidate, [Vector| Vectors], Options, Cost) :-
		distance(Options, Candidate, Vector, Distance0),
		total_distance(Candidate, Vectors, Options, RestCost),
		Cost is Distance0 + RestCost.

	max_medoid_shift([], [], _Options, 0.0).
	max_medoid_shift([Medoid0| Medoids0], [Medoid1| Medoids1], Options, MaxShift) :-
		distance(Options, Medoid0, Medoid1, Shift),
		max_medoid_shift(Medoids0, Medoids1, Options, RestMaxShift),
		MaxShift is max(Shift, RestMaxShift).

	distance(Options, Vector1, Vector2, Distance) :-
		^^option(distance_metric(Metric), Options),
		distance_metric(Metric, Vector1, Vector2, Distance).

	distance_metric(euclidean, Vector1, Vector2, Distance) :-
		euclidean_distance(Vector1, Vector2, Distance).
	distance_metric(manhattan, Vector1, Vector2, Distance) :-
		manhattan_distance(Vector1, Vector2, Distance).

	print_clusterer(kmedoids_clusterer(Encoders, Medoids, Options)) :-
		format('k-Medoids Clusterer~n', []),
		format('===================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nMedoids:~n', []),
		print_medoids(Medoids, 1).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		format('  ~w (continuous, mean=~4f, scale=~4f)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).

	print_medoids([], _).
	print_medoids([Medoid| Medoids], Cluster) :-
		format('  cluster ~d: ~w~n', [Cluster, Medoid]),
		NextCluster is Cluster + 1,
		print_medoids(Medoids, NextCluster).

	default_option(k(2)).
	default_option(maximum_iterations(100)).
	default_option(tolerance(1.0e-6)).
	default_option(initialization(spread)).
	default_option(distance_metric(euclidean)).
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
	valid_option(distance_metric(Metric)) :-
		once((Metric == euclidean; Metric == manhattan)).
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).

:- end_object.
