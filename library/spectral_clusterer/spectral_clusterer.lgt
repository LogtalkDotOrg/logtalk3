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


:- object(spectral_clusterer,
	imports(clusterer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-01,
		comment is 'Normalized spectral clusterer for continuous datasets using a Gaussian RBF affinity and a Nystroem extension for assigning new instances to clusters.',
		see_also is [clusterer_protocol, clustering_dataset_protocol, kmeans_clusterer, linear_algebra]
	]).

	:- protected(clusterer_diagnostics_data/2).
	:- mode(clusterer_diagnostics_data(+compound, -list(compound)), one).
	:- info(clusterer_diagnostics_data/2, [
		comment is 'Extracts the diagnostics metadata from a learned spectral clusterer.',
		argnames is ['Clusterer', 'Diagnostics']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2, nth0/3
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- uses(pairs, [
		keys/2, values/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(linear_algebra, [
		symmetric_eigen/5, transpose_matrix/2
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
		values(Rows, TrainingRows),
		length(TrainingRows, Count),
		^^option(k(K), Options),
		^^check_cluster_count(K, Count),
		resolve_sigma(TrainingRows, Options, ResolvedSigma),
		Gamma is 1.0 / (2.0 * ResolvedSigma * ResolvedSigma),
		build_affinity_matrix(TrainingRows, Gamma, AffinityMatrix),
		row_sums(AffinityMatrix, Degrees),
		build_normalized_adjacency(AffinityMatrix, Degrees, NormalizedAdjacency),
		^^option(tolerance(Tolerance), Options),
		^^option(maximum_iterations(MaximumIterations), Options),
		symmetric_eigen(NormalizedAdjacency, Tolerance, MaximumIterations, AllComponents, AllEigenvalues),
		select_components(K, Tolerance, AllComponents, AllEigenvalues, Components, Eigenvalues),
		transpose_matrix(Components, EmbeddingRows0),
		normalize_embedding_rows(EmbeddingRows0, Tolerance, EmbeddingRows),
		^^option(initialization(Initialization), Options),
		initialize_centroids(Initialization, K, EmbeddingRows, InitialCentroids),
		optimize_centroids(EmbeddingRows, Options, 0, 0.0, InitialCentroids, Centroids, Convergence, Iterations, FinalShift),
		build_diagnostics(Count, K, ResolvedSigma, Eigenvalues, Options, Convergence, Iterations, FinalShift, Diagnostics),
		Clusterer = spectral_clusterer(Encoders, TrainingRows, Degrees, Components, Eigenvalues, Centroids, ResolvedSigma, Options, Diagnostics).

	cluster(Clusterer, Instance, Cluster) :-
		clusterer_data(Clusterer, Encoders, TrainingRows, Degrees, Components, Eigenvalues, Centroids, ResolvedSigma, Options, _Diagnostics),
		^^encode_instance(Encoders, Instance, Features),
		Gamma is 1.0 / (2.0 * ResolvedSigma * ResolvedSigma),
		affinity_vector(TrainingRows, Features, Gamma, Affinities),
		sum(Affinities, Degree),
		^^option(tolerance(Tolerance), Options),
		check_positive_value(instance_degree, Degree, Tolerance),
		normalized_affinity_vector(Affinities, Degrees, Degree, NormalizedAffinities),
		project_components(Components, Eigenvalues, NormalizedAffinities, Embedding0),
		normalize_embedding_row(Embedding0, Tolerance, Embedding),
		nearest_centroid(Centroids, Embedding, Cluster, _DistanceSquared).

	clusterer_data(Clusterer, Encoders, TrainingRows, Degrees, Components, Eigenvalues, Centroids, ResolvedSigma, Options, Diagnostics) :-
		Clusterer =.. [_Functor, Encoders, TrainingRows, Degrees, Components, Eigenvalues, Centroids, ResolvedSigma, Options, Diagnostics].

	clusterer_diagnostics_data(Clusterer, Diagnostics) :-
		clusterer_data(Clusterer, _Encoders, _TrainingRows, _Degrees, _Components, _Eigenvalues, _Centroids, _ResolvedSigma, _Options, Diagnostics).

	check_clusterer(Clusterer) :-
		(	clusterer_data(Clusterer, Encoders, TrainingRows, Degrees, Components, Eigenvalues, Centroids, ResolvedSigma, Options, Diagnostics),
			length(Encoders, FeatureCount),
			^^valid_continuous_encoders(Encoders),
			valid(list(list(number, FeatureCount)), TrainingRows),
			length(TrainingRows, TrainingExampleCount),
			valid(list(positive_number, TrainingExampleCount), Degrees),
			length(Components, ClusterCount),
			valid(list(list(number, TrainingExampleCount), ClusterCount), Components),
			valid(list(positive_number, ClusterCount), Eigenvalues),
			valid(list(list(number, ClusterCount), ClusterCount), Centroids),
			number(ResolvedSigma), ResolvedSigma > 0.0,
			^^valid_clusterer_metadata(spectral_clusterer, Options, Diagnostics),
			^^valid_diagnostic_count(cluster_count, Diagnostics, ClusterCount),
			^^valid_diagnostic_count(training_example_count, Diagnostics, TrainingExampleCount),
			memberchk(resolved_sigma(ResolvedSigma), Diagnostics),
			memberchk(eigenvalues(Eigenvalues), Diagnostics) ->
			true
		;	domain_error(clusterer, Clusterer)
		).

	resolve_sigma(_Rows, Options, Sigma) :-
		^^option(sigma(Sigma), Options),
		number(Sigma),
		!.
	resolve_sigma(Rows, _Options, Sigma) :-
		pairwise_positive_distances(Rows, Distances),
		(	Distances == [] ->
			domain_error(positive_pairwise_distance, 0.0)
		;	sort_numbers(Distances, SortedDistances),
			median_sorted(SortedDistances, Sigma)
		).

	sort_numbers([], []).
	sort_numbers([Number| Numbers], Sorted) :-
		sort_numbers(Numbers, Sorted0),
		insert_number(Sorted0, Number, Sorted).

	insert_number([], Number, [Number]).
	insert_number([Head| Tail], Number, Sorted) :-
		(	Number =< Head ->
			Sorted = [Number, Head| Tail]
		;	Sorted = [Head| SortedTail],
			insert_number(Tail, Number, SortedTail)
		).

	pairwise_positive_distances([], []).
	pairwise_positive_distances([Row| Rows], Distances) :-
		distances_to_rows(Rows, Row, HeadDistances),
		pairwise_positive_distances(Rows, TailDistances),
		append(HeadDistances, TailDistances, Distances).

	distances_to_rows([], _Row, []).
	distances_to_rows([OtherRow| Rows], Row, Distances) :-
		squared_euclidean_distance(Row, OtherRow, 0.0, DistanceSquared),
		(	DistanceSquared > 0.0 ->
			Distance is sqrt(DistanceSquared),
			Distances = [Distance| Tail]
		;	Distances = Tail
		),
		distances_to_rows(Rows, Row, Tail).

	median_sorted(Sorted, Median) :-
		length(Sorted, Count),
		Middle is Count // 2,
		(	Count mod 2 =:= 1 ->
			nth0(Middle, Sorted, Median)
		;	LeftIndex is Middle - 1,
			nth0(LeftIndex, Sorted, Left),
			nth0(Middle, Sorted, Right),
			Median is (Left + Right) / 2.0
		).

	build_affinity_matrix([], _Gamma, []).
	build_affinity_matrix([Row| Rows], Gamma, [AffinityRow| AffinityMatrix]) :-
		build_affinity_row([Row| Rows], Row, Gamma, AffinityRow0),
		build_affinity_prefix(Rows, Row, Gamma, AffinityRow0, AffinityRow),
		build_affinity_matrix(Rows, Gamma, AffinityMatrix0),
		prepend_affinities(AffinityMatrix0, Rows, Row, Gamma, AffinityMatrix).

	build_affinity_prefix(_Rows, _Row, _Gamma, AffinityRow, AffinityRow).

	prepend_affinities([], [], _Row, _Gamma, []).
	prepend_affinities([AffinityRow0| Matrix0], [OtherRow| Rows], Row, Gamma, [[Affinity| AffinityRow0]| Matrix]) :-
		rbf_affinity(Row, OtherRow, Gamma, Affinity),
		prepend_affinities(Matrix0, Rows, Row, Gamma, Matrix).

	build_affinity_row([], _Row, _Gamma, []).
	build_affinity_row([OtherRow| Rows], Row, Gamma, [Affinity| Affinities]) :-
		rbf_affinity(Row, OtherRow, Gamma, Affinity),
		build_affinity_row(Rows, Row, Gamma, Affinities).

	rbf_affinity(Left, Right, Gamma, Affinity) :-
		squared_euclidean_distance(Left, Right, 0.0, DistanceSquared),
		Affinity is exp(-Gamma * DistanceSquared).

	row_sums([], []).
	row_sums([Row| Rows], [Sum| Sums]) :-
		sum(Row, Sum),
		row_sums(Rows, Sums).

	build_normalized_adjacency(AffinityMatrix, Degrees, NormalizedAdjacency) :-
		build_normalized_rows(AffinityMatrix, Degrees, Degrees, NormalizedAdjacency).

	build_normalized_rows([], [], _AllDegrees, []).
	build_normalized_rows([AffinityRow| AffinityRows], [Degree| Degrees], AllDegrees, [NormalizedRow| NormalizedRows]) :-
		normalize_affinity_row(AffinityRow, AllDegrees, Degree, NormalizedRow),
		build_normalized_rows(AffinityRows, Degrees, AllDegrees, NormalizedRows).

	normalize_affinity_row([], [], _Degree, []).
	normalize_affinity_row([Affinity| Affinities], [OtherDegree| Degrees], Degree, [Normalized| NormalizedValues]) :-
		Normalized is Affinity / sqrt(Degree * OtherDegree),
		normalize_affinity_row(Affinities, Degrees, Degree, NormalizedValues).

	select_components(K, Tolerance, AllComponents, AllEigenvalues, Components, Eigenvalues) :-
		select_positive_components(K, Tolerance, AllComponents, AllEigenvalues, Components, Eigenvalues),
		length(Components, Available),
		(	Available =:= K ->
			true
		;	domain_error(component_count, K-Available)
		).

	select_positive_components(0, _Tolerance, _Components, _Eigenvalues, [], []) :-
		!.
	select_positive_components(_K, _Tolerance, [], [], [], []).
	select_positive_components(K, Tolerance, [Component| Components], [Eigenvalue| Eigenvalues], SelectedComponents, SelectedEigenvalues) :-
		(	Eigenvalue > Tolerance ->
			SelectedComponents = [Component| RemainingComponents],
			SelectedEigenvalues = [Eigenvalue| RemainingEigenvalues],
			Remaining is K - 1
		;	SelectedComponents = RemainingComponents,
			SelectedEigenvalues = RemainingEigenvalues,
			Remaining = K
		),
		select_positive_components(Remaining, Tolerance, Components, Eigenvalues, RemainingComponents, RemainingEigenvalues).

	normalize_embedding_rows([], _Tolerance, []).
	normalize_embedding_rows([Row| Rows], Tolerance, [Normalized| NormalizedRows]) :-
		normalize_embedding_row(Row, Tolerance, Normalized),
		normalize_embedding_rows(Rows, Tolerance, NormalizedRows).

	normalize_embedding_row(Row, Tolerance, Normalized) :-
		squared_norm(Row, 0.0, SquaredNorm),
		Norm is sqrt(SquaredNorm),
		check_positive_value(embedding_norm, Norm, Tolerance),
		Scale is 1.0 / Norm,
		scale_vector(Row, Scale, Normalized).

	check_positive_value(_Kind, Value, Tolerance) :-
		Value > Tolerance,
		!.
	check_positive_value(Kind, Value, _Tolerance) :-
		domain_error(positive_spectral_value(Kind), Value).

	normalized_affinity_vector([], [], _Degree, []).
	normalized_affinity_vector([Affinity| Affinities], [TrainingDegree| TrainingDegrees], Degree, [Normalized| NormalizedAffinities]) :-
		Normalized is Affinity / sqrt(Degree * TrainingDegree),
		normalized_affinity_vector(Affinities, TrainingDegrees, Degree, NormalizedAffinities).

	project_components([], [], _NormalizedAffinities, []).
	project_components([Component| Components], [Eigenvalue| Eigenvalues], NormalizedAffinities, [Coordinate| Coordinates]) :-
		dot_product(Component, NormalizedAffinities, 0.0, Product),
		Coordinate is Product / Eigenvalue,
		project_components(Components, Eigenvalues, NormalizedAffinities, Coordinates).

	affinity_vector([], _Features, _Gamma, []).
	affinity_vector([TrainingRow| TrainingRows], Features, Gamma, [Affinity| Affinities]) :-
		rbf_affinity(TrainingRow, Features, Gamma, Affinity),
		affinity_vector(TrainingRows, Features, Gamma, Affinities).

	dot_product([], [], Product, Product).
	dot_product([Left| Lefts], [Right| Rights], Product0, Product) :-
		Product1 is Product0 + Left * Right,
		dot_product(Lefts, Rights, Product1, Product).

	squared_norm([], SquaredNorm, SquaredNorm).
	squared_norm([Value| Values], SquaredNorm0, SquaredNorm) :-
		SquaredNorm1 is SquaredNorm0 + Value * Value,
		squared_norm(Values, SquaredNorm1, SquaredNorm).

	scale_vector([], _Scale, []).
	scale_vector([Value| Values], Scale, [Scaled| ScaledValues]) :-
		Scaled is Scale * Value,
		scale_vector(Values, Scale, ScaledValues).

	initialize_centroids(first_k, K, Rows, Centroids) :-
		take_vectors(K, Rows, Centroids).
	initialize_centroids(spread, K, [First| Rows], [First| Centroids]) :-
		Remaining is K - 1,
		select_spread_centroids(Remaining, Rows, [First], Centroids).

	take_vectors(0, _Rows, []) :-
		!.
	take_vectors(K, [Row| Rows], [Row| Selected]) :-
		Remaining is K - 1,
		take_vectors(Remaining, Rows, Selected).

	select_spread_centroids(0, _Candidates, _Selected, []) :-
		!.
	select_spread_centroids(Count, Candidates, Selected, [Vector| Centroids]) :-
		farthest_candidate(Candidates, Selected, Vector, RemainingCandidates),
		NextCount is Count - 1,
		select_spread_centroids(NextCount, RemainingCandidates, [Vector| Selected], Centroids).

	farthest_candidate([Candidate| Candidates], Selected, BestCandidate, RemainingCandidates) :-
		closest_centroid_distance_squared(Candidate, Selected, Distance),
		farthest_candidate(Candidates, Selected, Candidate, Distance, BestCandidate),
		remove_vector(BestCandidate, [Candidate| Candidates], RemainingCandidates).

	farthest_candidate([], _Selected, BestCandidate, _BestDistance, BestCandidate).
	farthest_candidate([Candidate| Candidates], Selected, BestCandidate0, BestDistance0, BestCandidate) :-
		closest_centroid_distance_squared(Candidate, Selected, Distance),
		(	Distance > BestDistance0 ->
			BestCandidate1 = Candidate,
			BestDistance1 = Distance
		;	BestCandidate1 = BestCandidate0,
			BestDistance1 = BestDistance0
		),
		farthest_candidate(Candidates, Selected, BestCandidate1, BestDistance1, BestCandidate).

	remove_vector(Vector, [Candidate| Candidates], Remaining) :-
		(	Vector == Candidate ->
			Remaining = Candidates
		;	Remaining = [Candidate| Tail],
			remove_vector(Vector, Candidates, Tail)
		).

	optimize_centroids(Rows, Options, Iteration, PreviousShift, Centroids0, Centroids, Convergence, Iterations, FinalShift) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(	Iteration >= MaximumIterations ->
			Centroids = Centroids0,
			Convergence = maximum_iterations,
			Iterations = Iteration,
			FinalShift = PreviousShift
		;	assign_rows(Rows, Centroids0, Assignments),
			recompute_centroids(Centroids0, Assignments, 1, Centroids1),
			max_centroid_shift(Centroids0, Centroids1, 0.0, Shift),
			^^option(tolerance(Tolerance), Options),
			NextIteration is Iteration + 1,
			(	Shift =< Tolerance ->
				Centroids = Centroids1,
				Convergence = tolerance,
				Iterations = NextIteration,
				FinalShift = Shift
			;	optimize_centroids(Rows, Options, NextIteration, Shift, Centroids1, Centroids, Convergence, Iterations, FinalShift)
			)
		).

	assign_rows([], _Centroids, []).
	assign_rows([Vector| Rows], Centroids, [Cluster-Vector| Assignments]) :-
		nearest_centroid(Centroids, Vector, Cluster, _DistanceSquared),
		assign_rows(Rows, Centroids, Assignments).

	nearest_centroid([Centroid| Centroids], Vector, Cluster, DistanceSquared) :-
		squared_euclidean_distance(Vector, Centroid, 0.0, InitialDistance),
		nearest_centroid(Centroids, Vector, 2, 1, InitialDistance, Cluster, DistanceSquared).

	nearest_centroid([], _Vector, _Index, BestCluster, BestDistance, BestCluster, BestDistance).
	nearest_centroid([Centroid| Centroids], Vector, Index, BestCluster0, BestDistance0, BestCluster, BestDistance) :-
		squared_euclidean_distance(Vector, Centroid, 0.0, Distance),
		(	Distance < BestDistance0 ->
			BestCluster1 = Index,
			BestDistance1 = Distance
		;	BestCluster1 = BestCluster0,
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
		BestDistance1 is min(BestDistance0, Distance),
		closest_centroid_distance_squared(Centroids, Vector, BestDistance1, BestDistance).

	squared_euclidean_distance([], [], DistanceSquared, DistanceSquared).
	squared_euclidean_distance([Left| Lefts], [Right| Rights], DistanceSquared0, DistanceSquared) :-
		Delta is Left - Right,
		DistanceSquared1 is DistanceSquared0 + Delta * Delta,
		squared_euclidean_distance(Lefts, Rights, DistanceSquared1, DistanceSquared).

	recompute_centroids([], _Assignments, _Cluster, []).
	recompute_centroids([Centroid0| Centroids0], Assignments, Cluster, [Centroid| Centroids]) :-
		assigned_vectors(Assignments, Cluster, Vectors),
		(	Vectors == [] ->
			Centroid = Centroid0
		;	average_vectors(Vectors, Centroid)
		),
		NextCluster is Cluster + 1,
		recompute_centroids(Centroids0, Assignments, NextCluster, Centroids).

	assigned_vectors([], _Cluster, []).
	assigned_vectors([AssignedCluster-Vector| Assignments], Cluster, Vectors) :-
		(	Cluster =:= AssignedCluster ->
			Vectors = [Vector| Tail]
		;	Vectors = Tail
		),
		assigned_vectors(Assignments, Cluster, Tail).

	average_vectors([Vector| Vectors], Average) :-
		sum_vectors(Vectors, Vector, 1, Sum, Count),
		Scale is 1.0 / Count,
		scale_vector(Sum, Scale, Average).

	sum_vectors([], Sum, Count, Sum, Count).
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

	build_diagnostics(TrainingExampleCount, ClusterCount, ResolvedSigma, Eigenvalues, Options, Convergence, Iterations, FinalShift, Diagnostics) :-
		Diagnostics = [
			model(spectral_clusterer),
			cluster_count(ClusterCount),
			training_example_count(TrainingExampleCount),
			resolved_sigma(ResolvedSigma),
			eigenvalues(Eigenvalues),
			convergence(Convergence),
			iterations(Iterations),
			final_shift(FinalShift),
			options(Options)
		].

	print_clusterer(Clusterer) :-
		clusterer_data(Clusterer, Encoders, _TrainingRows, _Degrees, _Components, Eigenvalues, Centroids, ResolvedSigma, Options, Diagnostics),
		format('Spectral Clusterer~n', []),
		format('==================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Diagnostics: ~w~n~n', [Diagnostics]),
		format('Resolved sigma: ~w~n', [ResolvedSigma]),
		format('Eigenvalues: ~w~n', [Eigenvalues]),
		format('Encoders: ~w~n', [Encoders]),
		format('Embedding centroids: ~w~n', [Centroids]).

	default_option(k(2)).
	default_option(sigma(auto)).
	default_option(maximum_iterations(1000)).
	default_option(tolerance(1.0e-8)).
	default_option(initialization(spread)).
	default_option(feature_scaling(on)).

	valid_option(k(K)) :-
		valid(positive_integer, K).
	valid_option(sigma(Sigma)) :-
		(	Sigma == auto ->
			true
		;	number(Sigma),
			Sigma > 0.0
		).
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
