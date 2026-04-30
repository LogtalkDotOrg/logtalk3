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


:- object(kmeans,
	imports(clusterer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-30,
		comment is 'k-Means clusterer for continuous datasets. Learns from a dataset object implementing the ``clustering_dataset_protocol`` protocol and returns a clusterer term that can be used for assigning new instances to clusters and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses Lloyd''s algorithm with deterministic initialization.',
			'Feature handling' - 'Supports continuous attributes only. Continuous attributes can be standardized using z-score scaling before clustering.',
			'Initialization' - 'Supports ``first_k`` initialization and a deterministic ``spread`` initialization that repeatedly chooses the farthest example from the centroids selected so far.',
			'Empty clusters' - 'If an iteration leaves a cluster empty, its centroid is kept unchanged from the previous iteration.',
			'Training diagnostics' - 'Exposes training metadata including example count, convergence status, performed iterations, and final centroid shift.',
			'Clusterer representation' - 'The learned clusterer is represented by default as ``kmeans_clusterer(Encoders, Centroids, Options, Diagnostics)`` where ``Encoders`` stores the feature encoding metadata, ``Centroids`` stores the learned centroid vectors in cluster-id order, ``Options`` stores the effective training options, and ``Diagnostics`` stores training metadata.'
		],
		see_also is [clusterer_protocol, clustering_dataset_protocol, nearest_centroid]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(numberlist, [
		rescale/3
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
		initialize_centroids(Initialization, K, Rows, InitialCentroids),
		optimize_centroids(Rows, Options, 0, 0.0, InitialCentroids, Centroids, Convergence, Iterations, FinalShift),
		build_diagnostics(Count, Centroids, Options, Convergence, Iterations, FinalShift, Diagnostics),
		Clusterer = kmeans_clusterer(Encoders, Centroids, Options, Diagnostics).

	cluster(Clusterer, Instance, Cluster) :-
		clusterer_data(Clusterer, Encoders, Centroids, _Options, _Diagnostics),
		^^encode_instance(Encoders, Instance, Features),
		nearest_centroid(Centroids, Features, Cluster, _DistanceSquared).

	clusterer_data(Clusterer, Encoders, Centroids, Options, Diagnostics) :-
		Clusterer =.. [_Functor, Encoders, Centroids, Options, Diagnostics].

	build_diagnostics(TrainingExampleCount, Centroids, Options, Convergence, Iterations, FinalShift, Diagnostics) :-
		length(Centroids, CentroidCount),
		Diagnostics = [
			model(kmeans),
			centroid_count(CentroidCount),
			training_example_count(TrainingExampleCount),
			convergence(Convergence),
			iterations(Iterations),
			final_shift(FinalShift),
			options(Options)
		].

	clusterer_diagnostics_data(Clusterer, Diagnostics) :-
		clusterer_data(Clusterer, _Encoders, _Centroids, _Options, Diagnostics).

	check_clusterer(Clusterer) :-
		(   clusterer_data(Clusterer, Encoders, Centroids, Options, Diagnostics),
			length(Encoders, FeatureCount),
			^^valid_continuous_encoders(Encoders),
			valid(list(list(float, FeatureCount)), Centroids),
			^^valid_clusterer_metadata(kmeans, Options, Diagnostics),
			length(Centroids, CentroidCount),
			^^valid_diagnostic_count(centroid_count, Diagnostics, CentroidCount) ->
			true
		;   domain_error(clusterer, Clusterer)
		).

	initialize_centroids(first_k, K, Rows, Centroids) :-
		^^take_first_k(K, Rows, Centroids).
	initialize_centroids(spread, K, [_-First| Rows], [First| Centroids]) :-
		Remaining is K - 1,
		select_spread_centroids(Remaining, Rows, [First], Centroids).

	select_spread_centroids(0, _, _, []) :-
		!.
	select_spread_centroids(Count, Candidates, Selected, [Vector| Centroids]) :-
		Count > 0,
		farthest_candidate(Candidates, Selected, BestCandidate, RemainingCandidates),
		BestCandidate = _-Vector,
		NextCount is Count - 1,
		select_spread_centroids(NextCount, RemainingCandidates, [Vector| Selected], Centroids).

	farthest_candidate([Candidate| Candidates], Selected, BestCandidate, RemainingCandidates) :-
		Candidate = _-Vector,
		closest_centroid_distance_squared(Vector, Selected, Distance),
		farthest_candidate(Candidates, Selected, Candidate, Distance, BestCandidate),
		^^remove_candidate(BestCandidate, [Candidate| Candidates], RemainingCandidates).

	farthest_candidate([], _, BestCandidate, _BestDistance, BestCandidate).
	farthest_candidate([Candidate| Candidates], Selected, BestCandidate0, BestDistance0, BestCandidate) :-
		Candidate = _-Vector,
		closest_centroid_distance_squared(Vector, Selected, Distance),
		(   Distance > BestDistance0 ->
			BestCandidate1 = Candidate,
			BestDistance1 = Distance
		;   BestCandidate1 = BestCandidate0,
			BestDistance1 = BestDistance0
		),
		farthest_candidate(Candidates, Selected, BestCandidate1, BestDistance1, BestCandidate).

	optimize_centroids(Rows, Options, Iteration, PreviousShift, Centroids0, Centroids, Convergence, Iterations, FinalShift) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(   Iteration >= MaximumIterations ->
			Centroids = Centroids0,
			Convergence = maximum_iterations,
			Iterations = Iteration,
			FinalShift = PreviousShift
		;   assign_rows(Rows, Centroids0, Assignments),
			recompute_centroids(Centroids0, Assignments, 1, Centroids1),
			max_centroid_shift(Centroids0, Centroids1, 0.0, Shift),
			^^option(tolerance(Tolerance), Options),
			NextIteration is Iteration + 1,
			(   Shift =< Tolerance ->
				Centroids = Centroids1,
				Convergence = tolerance,
				Iterations = NextIteration,
				FinalShift = Shift
			;   optimize_centroids(Rows, Options, NextIteration, Shift, Centroids1, Centroids, Convergence, Iterations, FinalShift)
			)
		).

	assign_rows([], _, []).
	assign_rows([_-Vector| Rows], Centroids, [Cluster-Vector| Assignments]) :-
		nearest_centroid(Centroids, Vector, Cluster, _DistanceSquared),
		assign_rows(Rows, Centroids, Assignments).

	nearest_centroid([Centroid| Centroids], Vector, Cluster, DistanceSquared) :-
		squared_euclidean_distance(Vector, Centroid, 0.0, InitialDistance),
		nearest_centroid(Centroids, Vector, 2, 1, InitialDistance, Cluster, DistanceSquared).

	nearest_centroid([], _Vector, _Index, BestCluster, BestDistance, BestCluster, BestDistance) :-
		!.
	nearest_centroid([Centroid| Centroids], Vector, Index, BestCluster0, BestDistance0, BestCluster, BestDistance) :-
		squared_euclidean_distance(Vector, Centroid, 0.0, Distance),
		(   Distance < BestDistance0 ->
			BestCluster1 = Index,
			BestDistance1 = Distance
		;   BestCluster1 = BestCluster0,
			BestDistance1 = BestDistance0
		),
		NextIndex is Index + 1,
		nearest_centroid(Centroids, Vector, NextIndex, BestCluster1, BestDistance1, BestCluster, BestDistance).

	closest_centroid_distance_squared(Vector, [Centroid| Centroids], DistanceSquared) :-
		squared_euclidean_distance(Vector, Centroid, 0.0, InitialDistance),
		closest_centroid_distance_squared(Centroids, Vector, InitialDistance, DistanceSquared).

	closest_centroid_distance_squared([], _Vector, BestDistance, BestDistance).
	closest_centroid_distance_squared([Centroid| Centroids], Vector, BestDistance0, BestDistance) :-
		squared_euclidean_distance(Vector, Centroid, 0.0, Distance),
		(   Distance < BestDistance0 ->
			BestDistance1 = Distance
		;   BestDistance1 = BestDistance0
		),
		closest_centroid_distance_squared(Centroids, Vector, BestDistance1, BestDistance).

	squared_euclidean_distance([], [], DistanceSquared, DistanceSquared).
	squared_euclidean_distance([Value| Values], [Centroid| Centroids], DistanceSquared0, DistanceSquared) :-
		Delta is Value - Centroid,
		DistanceSquared1 is DistanceSquared0 + Delta * Delta,
		squared_euclidean_distance(Values, Centroids, DistanceSquared1, DistanceSquared).

	recompute_centroids([], _, _, []).
	recompute_centroids([Centroid0| Centroids0], Assignments, Cluster, [Centroid| Centroids]) :-
		assigned_vectors(Cluster, Assignments, Vectors),
		(   Vectors == [] ->
			Centroid = Centroid0
		;   average_vectors(Vectors, Centroid)
		),
		NextCluster is Cluster + 1,
		recompute_centroids(Centroids0, Assignments, NextCluster, Centroids).

	assigned_vectors(_, [], []) :-
		!.
	assigned_vectors(Cluster, [Cluster-Vector| Assignments], [Vector| Vectors]) :-
		!,
		assigned_vectors(Cluster, Assignments, Vectors).
	assigned_vectors(Cluster, [_OtherCluster-_| Assignments], Vectors) :-
		assigned_vectors(Cluster, Assignments, Vectors).

	average_vectors([Vector| Vectors], Average) :-
		sum_vectors(Vectors, Vector, 1, Sum, Count),
		Factor is 1.0 / Count,
		rescale(Sum, Factor, Average).

	sum_vectors([], Sum, Count, Sum, Count) :-
		!.
	sum_vectors([Vector| Vectors], Sum0, Count0, Sum, Count) :-
		add_vectors(Sum0, Vector, Sum1),
		Count1 is Count0 + 1,
		sum_vectors(Vectors, Sum1, Count1, Sum, Count).

	add_vectors([], [], []).
	add_vectors([Left| Lefts], [Right| Rights], [Sum| Sums]) :-
		Sum is Left + Right,
		add_vectors(Lefts, Rights, Sums).

	max_centroid_shift([], [], MaxShift, MaxShift).
	max_centroid_shift([Centroid0| Centroids0], [Centroid1| Centroids1], MaxShift0, MaxShift) :-
		squared_euclidean_distance(Centroid0, Centroid1, 0.0, DistanceSquared),
		Shift is sqrt(DistanceSquared),
		MaxShift1 is max(MaxShift0, Shift),
		max_centroid_shift(Centroids0, Centroids1, MaxShift1, MaxShift).

	print_clusterer(Clusterer) :-
		clusterer_data(Clusterer, Encoders, Centroids, Options, Diagnostics),
		format('k-Means Clusterer~n', []),
		format('=================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Diagnostics: ~w~n~n', [Diagnostics]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nCentroids:~n', []),
		print_centroids(Centroids, 1).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		format('  ~w (continuous, mean=~4f, scale=~4f)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).

	print_centroids([], _).
	print_centroids([Centroid| Centroids], Cluster) :-
		format('  cluster ~d: ~w~n', [Cluster, Centroid]),
		NextCluster is Cluster + 1,
		print_centroids(Centroids, NextCluster).

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
