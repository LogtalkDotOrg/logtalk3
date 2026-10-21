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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
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
		append/3, length/2, memberchk/2, msort/2, nth0/3
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
		symmetric_eigen_partial/7, transpose_matrix/2, matrix_vector_product/3, add_scaled_outer_product/4, add_scaled_vector/4
	]).

	learn(Dataset, Clusterer, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		(	Attributes == [] ->
			domain_error(non_empty_attributes, Dataset)
		;	true
		),
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
		build_affinity_matrix(TrainingRows, ResolvedSigma, AffinityMatrix),
		row_sums(AffinityMatrix, Degrees),
		build_normalized_adjacency(AffinityMatrix, Degrees, NormalizedAdjacency),
		spectral_components(NormalizedAdjacency, Degrees, K, Options, Components, Eigenvalues, SpectralDiagnostics),
		transpose_matrix(Components, EmbeddingRows0),
		normalize_embedding_rows(EmbeddingRows0, 0.0, EmbeddingRows),
		^^option(initialization(Initialization), Options),
		initialize_centroids(Initialization, K, EmbeddingRows, InitialCentroids),
		optimize_centroids(EmbeddingRows, Options, 0, 0.0, InitialCentroids, Centroids, Convergence, Iterations, FinalShift),
		build_diagnostics(Count, K, ResolvedSigma, Eigenvalues, Options, Convergence, Iterations, FinalShift, Diagnostics),
		append(SpectralDiagnostics, Diagnostics, AllDiagnostics),
		build_projection_coefficients(Components, Eigenvalues, Degrees, Projection),
		Clusterer = spectral_clusterer(Encoders, TrainingRows, Degrees, Components, Eigenvalues, Projection, Centroids, ResolvedSigma, Options, AllDiagnostics).

	cluster(Clusterer, Instance, Cluster) :-
		clusterer_data(Clusterer, Encoders, TrainingRows, _Degrees, _Components, _Eigenvalues, Projection, Centroids, ResolvedSigma, _Options, _Diagnostics),
		^^encode_instance(Encoders, Instance, Features),
		affinity_vector(TrainingRows, Features, ResolvedSigma, Affinities, 0.0, Maximum),
		check_positive_value(instance_degree, Maximum, 0.0),
		projection_embedding(Projection, Affinities, Maximum, Embedding),
		nearest_centroid(Centroids, Embedding, Cluster, _DistanceSquared).

	clusterer_data(Clusterer, Encoders, TrainingRows, Degrees, Components, Eigenvalues, Projection, Centroids, ResolvedSigma, Options, Diagnostics) :-
		Clusterer =.. [_Functor, Encoders, TrainingRows, Degrees, Components, Eigenvalues, Projection, Centroids, ResolvedSigma, Options, Diagnostics].

	clusterer_diagnostics_data(Clusterer, Diagnostics) :-
		clusterer_data(Clusterer, _Encoders, _TrainingRows, _Degrees, _Components, _Eigenvalues, _Projection, _Centroids, _ResolvedSigma, _Options, Diagnostics).

	check_clusterer(Clusterer) :-
		(	\+ ground(Clusterer) ->
			instantiation_error
		;	compound(Clusterer),
			clusterer_data(Clusterer, Encoders, TrainingRows, Degrees, Components, Eigenvalues, Projection, Centroids, ResolvedSigma, Options, Diagnostics),
			length(Encoders, FeatureCount),
			FeatureCount > 0,
			^^valid_continuous_encoders(Encoders),
			valid(list(list(number, FeatureCount)), TrainingRows),
			length(TrainingRows, TrainingExampleCount),
			TrainingExampleCount > 0,
			valid(list(positive_number, TrainingExampleCount), Degrees),
			length(Components, ClusterCount),
			ClusterCount > 0,
			ClusterCount =< TrainingExampleCount,
			valid(list(list(number, TrainingExampleCount), ClusterCount), Components),
			valid(list(positive_number, ClusterCount), Eigenvalues),
			valid(list(list(number, TrainingExampleCount), ClusterCount), Projection),
			valid(list(list(number, ClusterCount), ClusterCount), Centroids),
			number(ResolvedSigma), ResolvedSigma > 0.0,
			^^valid_clusterer_metadata(spectral_clusterer, Options, Diagnostics),
			^^option(k(ClusterCount), Options),
			^^option(sigma(Sigma), Options),
			(Sigma == auto; Sigma == ResolvedSigma),
			^^option(eigenvalue_cutoff(Cutoff), Options),
			usable_eigenvalues(Eigenvalues, Cutoff),
			memberchk(eigensolver_convergence(tolerance), Diagnostics),
			memberchk(eigensolver_iterations(SpectralIterations), Diagnostics),
			valid(list(non_negative_integer, ClusterCount), SpectralIterations),
			memberchk(eigenpair_residuals(Residuals), Diagnostics),
			valid(list(non_negative_number, ClusterCount), Residuals),
			memberchk(maximum_eigenpair_residual(MaximumResidual), Diagnostics),
			valid(non_negative_number, MaximumResidual),
			memberchk(orthogonality_error(Orthogonality), Diagnostics),
			valid(non_negative_number, Orthogonality),
			^^valid_diagnostic_count(cluster_count, Diagnostics, ClusterCount),
			^^valid_diagnostic_count(training_example_count, Diagnostics, TrainingExampleCount),
			memberchk(resolved_sigma(ResolvedSigma), Diagnostics),
			memberchk(eigenvalues(Eigenvalues), Diagnostics) ->
			true
		;	domain_error(clusterer, Clusterer)
		).

	usable_eigenvalues([], _Cutoff).
	usable_eigenvalues([Value| Values], Cutoff) :-
		Value > Cutoff,
		usable_eigenvalues(Values, Cutoff).

	spectral_components(Matrix, Degrees, K, Options, Components, Eigenvalues, Diagnostics) :-
		sum(Degrees, Total),
		perron_vector(Degrees, Total, Perron),
		^^option(eigensolver_tolerance(Tolerance), Options),
		^^option(eigensolver_maximum_iterations(MaximumIterations), Options),
		Remaining is K - 1,
		(	Remaining =:= 0 ->
			OtherComponents0 = [],
			OtherValues = [],
			OtherIterations = []
		;	add_scaled_outer_product(Perron, -1.0, Matrix, Deflated),
			SolverTolerance is Tolerance / 4.0,
			symmetric_eigen_partial(Deflated, Remaining, SolverTolerance, MaximumIterations, OtherComponents0, OtherValues, SolverDiagnostics),
			memberchk(component_iterations(OtherIterations), SolverDiagnostics)
		),
		^^option(eigenvalue_cutoff(Cutoff), Options),
		select_components(K, Cutoff, [Perron| OtherComponents0], [1.0| OtherValues], _, _),
		reorthogonalize_components(OtherComponents0, [Perron], OtherComponents),
		AllComponents = [Perron| OtherComponents],
		measure_components(AllComponents, Matrix, AllValues, Residuals, 0.0, MaximumResidual),
		component_orthogonality(AllComponents, 0.0, Orthogonality),
		row_sums(Matrix, Sums),
		maximum_absolute(Sums, 1.0, MatrixScale),
		Threshold is Tolerance * MatrixScale,
		(	MaximumResidual =< Threshold,
			Orthogonality =< Tolerance ->
			SpectralConvergence = tolerance
		;	SpectralConvergence = maximum_iterations
		),
		Diagnostics = [eigensolver_convergence(SpectralConvergence), eigensolver_iterations([0| OtherIterations]), eigenpair_residuals(Residuals), maximum_eigenpair_residual(MaximumResidual), orthogonality_error(Orthogonality)],
		(	SpectralConvergence == tolerance ->
			true
		;	domain_error(spectral_convergence, Diagnostics)
		),
		select_components(K, Cutoff, AllComponents, AllValues, Components, Eigenvalues).

	perron_vector([], _Total, []).
	perron_vector([Degree| Degrees], Total, [Value| Values]) :-
		Value is sqrt(Degree / Total),
		perron_vector(Degrees, Total, Values).

	reorthogonalize_components([], _Selected, []).
	reorthogonalize_components([Vector0| Vectors], Selected, [Vector| Components]) :-
		orthogonalize_component(Selected, Vector0, Vector1),
		orthogonalize_component(Selected, Vector1, Vector2),
		normalize_embedding_row(Vector2, 0.0, Vector3),
		linear_algebra::stabilize_vector_sign(Vector3, 0.0, Vector),
		reorthogonalize_components(Vectors, [Vector| Selected], Components).

	orthogonalize_component([], Vector, Vector).
	orthogonalize_component([Basis| Bases], Vector0, Vector) :-
		dot_product(Basis, Vector0, 0.0, Coefficient),
		Negative is -Coefficient,
		add_scaled_vector(Basis, Negative, Vector0, Vector1),
		orthogonalize_component(Bases, Vector1, Vector).

	measure_components([], _Matrix, [], [], Maximum, Maximum).
	measure_components([Vector| Vectors], Matrix, [Value| Values], [Residual| Residuals], Maximum0, Maximum) :-
		matrix_vector_product(Matrix, Vector, Product),
		dot_product(Vector, Product, 0.0, Value),
		scale_vector(Vector, Value, Expected),
		squared_euclidean_distance(Product, Expected, 0.0, SquaredResidual),
		Residual is sqrt(SquaredResidual),
		Maximum1 is max(Maximum0, Residual),
		measure_components(Vectors, Matrix, Values, Residuals, Maximum1, Maximum).

	component_orthogonality([], Error, Error).
	component_orthogonality([Vector| Vectors], Error0, Error) :-
		dot_product(Vector, Vector, 0.0, SquaredNorm),
		Error1 is max(Error0, abs(SquaredNorm - 1.0)),
		cross_component_error(Vectors, Vector, Error1, Error2),
		component_orthogonality(Vectors, Error2, Error).

	cross_component_error([], _Vector, Error, Error).
	cross_component_error([Other| Vectors], Vector, Error0, Error) :-
		dot_product(Vector, Other, 0.0, Product),
		Error1 is max(Error0, abs(Product)),
		cross_component_error(Vectors, Vector, Error1, Error).

	resolve_sigma(_Rows, Options, Sigma) :-
		^^option(sigma(Sigma), Options),
		number(Sigma),
		!.
	resolve_sigma(Rows, _Options, Sigma) :-
		pairwise_distance_keys(Rows, Kind, Distances),
		(	Distances == [] ->
			domain_error(positive_pairwise_distance, 0.0)
		;	median_distance_keys(Distances, Kind, Sigma)
		).

	pairwise_distance_keys(Rows, Kind, Distances) :-
		catch(
			(	pairwise_distances(Rows, squared, Distances, []),
				Kind = squared
			),
			error(evaluation_error(_), _),
			(	pairwise_distances(Rows, ordinary, Distances, []),
				Kind = ordinary
			)
		).

	pairwise_distances([], _Kind, Tail, Tail).
	pairwise_distances([Row| Rows], Kind, Distances, Tail) :-
		distances_to_rows(Rows, Row, Kind, Distances, Remaining),
		pairwise_distances(Rows, Kind, Remaining, Tail).

	distances_to_rows([], _Row, _Kind, Tail, Tail).
	distances_to_rows([OtherRow| Rows], Row, Kind, Distances, Tail) :-
		distance_key(Row, OtherRow, Kind, Distance),
		(	Distance > 0.0 ->
			Distances = [Distance| Remaining]
		;	Distances = Remaining
		),
		distances_to_rows(Rows, Row, Kind, Remaining, Tail).

	distance_key(Left, Right, squared, Distance) :-
		!,
		squared_euclidean_distance(Left, Right, 0.0, Distance),
		(	Distance > 0.0 ->
			true
		;	Left == Right ->
			true
		;	evaluation_error(float_underflow)
		).
	distance_key(Left, Right, ordinary, Distance) :-
		vector_difference(Left, Right, Difference),
		maximum_absolute(Difference, 0.0, Maximum),
		(	Maximum > 0.0 ->
			divide_vector(Difference, Maximum, Scaled),
			squared_norm(Scaled, 0.0, SquaredNorm),
			Distance is Maximum * sqrt(SquaredNorm)
		;	Distance = 0.0
		).

	vector_difference([], [], []).
	vector_difference([Left| Lefts], [Right| Rights], [Delta| Deltas]) :-
		Delta is Left - Right,
		vector_difference(Lefts, Rights, Deltas).

	median_distance_keys(Keys, Kind, Median) :-
		length(Keys, Count),
		Middle is Count // 2,
		order_statistic(Keys, Middle, RightKey),
		distance_from_key(Kind, RightKey, Right),
		(	Count mod 2 =:= 1 ->
			Median = Right
		;	LeftIndex is Middle - 1,
			order_statistic(Keys, LeftIndex, LeftKey),
			distance_from_key(Kind, LeftKey, Left),
			Median is Left + (Right - Left) / 2.0
		).

	distance_from_key(squared, Key, Distance) :-
		!,
		Distance is sqrt(Key).
	distance_from_key(ordinary, Distance, Distance).

	order_statistic(Keys, Index, Value) :-
		length(Keys, Count),
		(	Count =< 5 ->
			msort(Keys, Sorted),
			nth0(Index, Sorted, Value)
		;	group_medians(Keys, Medians),
			length(Medians, MedianCount),
			PivotIndex is MedianCount // 2,
			order_statistic(Medians, PivotIndex, Pivot),
			partition_keys(Keys, Pivot, Less, Equal, Greater),
			length(Less, LessCount),
			length(Equal, EqualCount),
			Upper is LessCount + EqualCount,
			(	Index < LessCount ->
				order_statistic(Less, Index, Value)
			;	(	Index < Upper ->
					Value = Pivot
				;	GreaterIndex is Index - Upper,
					order_statistic(Greater, GreaterIndex, Value)
				)
			)
		).

	group_medians([], []).
	group_medians([Key| Keys], [Median| Medians]) :-
		take_group(4, Keys, Group, Remaining),
		msort([Key| Group], Sorted),
		length(Sorted, Count),
		Index is Count // 2,
		nth0(Index, Sorted, Median),
		group_medians(Remaining, Medians).

	take_group(0, Keys, [], Keys) :-
		!.
	take_group(_Count, [], [], []) :-
		!.
	take_group(Count, [Key| Keys], [Key| Group], Remaining) :-
		Next is Count - 1,
		take_group(Next, Keys, Group, Remaining).

	partition_keys([], _Pivot, [], [], []).
	partition_keys([Key| Keys], Pivot, Less, Equal, Greater) :-
		(	Key < Pivot ->
			Less = [Key| LessTail],
			Equal = EqualTail,
			Greater = GreaterTail
		;	Key > Pivot ->
			Less = LessTail,
			Equal = EqualTail,
			Greater = [Key| GreaterTail]
		;	Less = LessTail,
			Equal = [Key| EqualTail],
			Greater = GreaterTail
		),
		partition_keys(Keys, Pivot, LessTail, EqualTail, GreaterTail).

	build_affinity_matrix([], _Sigma, []).
	build_affinity_matrix([Row| Rows], Sigma, [[1.0| Affinities]| AffinityMatrix]) :-
		build_affinity_row(Rows, Row, Sigma, Affinities),
		build_affinity_matrix(Rows, Sigma, AffinityMatrix0),
		prepend_affinities(AffinityMatrix0, Affinities, AffinityMatrix).

	prepend_affinities([], [], []).
	prepend_affinities([AffinityRow| Matrix0], [Affinity| Affinities], [[Affinity| AffinityRow]| Matrix]) :-
		prepend_affinities(Matrix0, Affinities, Matrix).

	build_affinity_row([], _Row, _Sigma, []).
	build_affinity_row([OtherRow| Rows], Row, Sigma, [Affinity| Affinities]) :-
		rbf_affinity(Row, OtherRow, Sigma, Affinity),
		build_affinity_row(Rows, Row, Sigma, Affinities).

	rbf_affinity(Left, Right, Sigma, Affinity) :-
		catch(
			(	scaled_distance_squared(Left, Right, Sigma, 0.0, DistanceSquared),
				Affinity is exp(-0.5 * DistanceSquared)
			),
			error(evaluation_error(_), _),
			logarithmic_affinity(Left, Right, Sigma, Affinity)
		).

	scaled_distance_squared([], [], _Sigma, Sum, Sum).
	scaled_distance_squared([Left| Lefts], [Right| Rights], Sigma, Sum0, Sum) :-
		Ratio is (Left - Right) / Sigma,
		Sum1 is Sum0 + Ratio * Ratio,
		scaled_distance_squared(Lefts, Rights, Sigma, Sum1, Sum).

	logarithmic_affinity(Left, Right, Sigma, Affinity) :-
		LogSigma is log(Sigma),
		logarithmic_distance(Left, Right, LogSigma, none, Distance),
		(	Distance == none ->
			Affinity = 1.0
		;	Distance = some(LogDistance),
			LogExponent is LogDistance - log(2.0),
			reduce_log_exponent(LogExponent, 0, Exponent, Squarings),
			Initial is exp(-Exponent),
			square_affinity(Squarings, Initial, Affinity)
		).

	logarithmic_distance([], [], _LogSigma, Sum, Sum).
	logarithmic_distance([Left| Lefts], [Right| Rights], LogSigma, Sum0, Sum) :-
		(	Left =:= Right ->
			Sum1 = Sum0
		;	Scale is max(abs(Left), abs(Right)),
			Delta is abs(Left / Scale - Right / Scale),
			LogSquared is 2.0 * (log(Scale) + log(Delta) - LogSigma),
			add_log_distance(Sum0, LogSquared, Sum1)
		),
		logarithmic_distance(Lefts, Rights, LogSigma, Sum1, Sum).

	add_log_distance(none, Value, some(Value)).
	add_log_distance(some(Previous), Value, some(Sum)) :-
		Maximum is max(Previous, Value),
		Sum is Maximum + log(exp(Previous - Maximum) + exp(Value - Maximum)).

	reduce_log_exponent(LogExponent, Count, Exponent, Squarings) :-
		(	LogExponent =< 0.0 ->
			Exponent is exp(LogExponent),
			Squarings = Count
		;	Reduced is LogExponent - log(2.0),
			Next is Count + 1,
			reduce_log_exponent(Reduced, Next, Exponent, Squarings)
		).

	square_affinity(Count, Value, Affinity) :-
		(	(Count =:= 0; Value =< 0.0) ->
			Affinity = Value
		;	Squared is Value * Value,
			Remaining is Count - 1,
			square_affinity(Remaining, Squared, Affinity)
		).

	row_sums([], []).
	row_sums([Row| Rows], [Sum| Sums]) :-
		sum(Row, Sum),
		row_sums(Rows, Sums).

	build_normalized_adjacency(AffinityMatrix, Degrees, NormalizedAdjacency) :-
		inverse_sqrt_degrees(Degrees, Inverses),
		build_normalized_rows(AffinityMatrix, Inverses, Inverses, NormalizedAdjacency).

	inverse_sqrt_degrees([], []).
	inverse_sqrt_degrees([Degree| Degrees], [Inverse| Inverses]) :-
		Inverse is 1.0 / sqrt(Degree),
		inverse_sqrt_degrees(Degrees, Inverses).

	build_normalized_rows([], [], _AllDegrees, []).
	build_normalized_rows([AffinityRow| AffinityRows], [Degree| Degrees], AllDegrees, [NormalizedRow| NormalizedRows]) :-
		normalize_affinity_row(AffinityRow, AllDegrees, Degree, NormalizedRow),
		build_normalized_rows(AffinityRows, Degrees, AllDegrees, NormalizedRows).

	normalize_affinity_row([], [], _Degree, []).
	normalize_affinity_row([Affinity| Affinities], [OtherDegree| Degrees], Degree, [Normalized| NormalizedValues]) :-
		Normalized is Affinity * (Degree * OtherDegree),
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
		maximum_absolute(Row, 0.0, Maximum),
		check_positive_value(embedding_norm, Maximum, 0.0),
		divide_vector(Row, Maximum, Scaled),
		squared_norm(Scaled, 0.0, SquaredNorm),
		Norm is sqrt(SquaredNorm),
		Threshold is Tolerance / Norm,
		check_positive_value(embedding_norm, Maximum, Threshold),
		Scale is 1.0 / Norm,
		scale_vector(Scaled, Scale, Normalized).

	maximum_absolute([], Maximum, Maximum).
	maximum_absolute([Value| Values], Maximum0, Maximum) :-
		Maximum1 is max(Maximum0, abs(Value)),
		maximum_absolute(Values, Maximum1, Maximum).

	divide_vector([], _Divisor, []).
	divide_vector([Value| Values], Divisor, [Quotient| Quotients]) :-
		Quotient is Value / Divisor,
		divide_vector(Values, Divisor, Quotients).

	check_positive_value(_Kind, Value, Tolerance) :-
		Value > Tolerance,
		!.
	check_positive_value(Kind, Value, _Tolerance) :-
		domain_error(positive_spectral_value(Kind), Value).

	build_projection_coefficients(Components, Eigenvalues, Degrees, Projection) :-
		inverse_sqrt_degrees(Degrees, Inverses),
		projection_rows(Components, Eigenvalues, Inverses, Projection).

	projection_rows([], [], _Inverses, []).
	projection_rows([Component| Components], [Eigenvalue| Eigenvalues], Inverses, [Row| Rows]) :-
		projection_row(Component, Inverses, Eigenvalue, Row),
		projection_rows(Components, Eigenvalues, Inverses, Rows).

	projection_row([], [], _Eigenvalue, []).
	projection_row([Value| Values], [Inverse| Inverses], Eigenvalue, [Coefficient| Coefficients]) :-
		Coefficient is (Value / Eigenvalue) * Inverse,
		projection_row(Values, Inverses, Eigenvalue, Coefficients).

	projection_embedding(Projection, Affinities, Embedding) :-
		maximum_absolute(Affinities, 0.0, Maximum),
		projection_embedding(Projection, Affinities, Maximum, Embedding).

	projection_embedding(Projection, Affinities, Maximum, Embedding) :-
		(	Maximum >= 1.0 ->
			ScaledAffinities = Affinities
		;	divide_vector(Affinities, Maximum, ScaledAffinities)
		),
		project_prepared(Projection, ScaledAffinities, Embedding0),
		normalize_embedding_row(Embedding0, 0.0, Embedding).

	project_prepared([], _Affinities, []).
	project_prepared([Row| Rows], Affinities, [Coordinate| Coordinates]) :-
		dot_product(Row, Affinities, 0.0, Coordinate),
		project_prepared(Rows, Affinities, Coordinates).

	affinity_vector([], _Features, _Sigma, [], Maximum, Maximum).
	affinity_vector([TrainingRow| TrainingRows], Features, Sigma, [Affinity| Affinities], Maximum0, Maximum) :-
		rbf_affinity(TrainingRow, Features, Sigma, Affinity),
		Maximum1 is max(Maximum0, Affinity),
		affinity_vector(TrainingRows, Features, Sigma, Affinities, Maximum1, Maximum).

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
		(	Remaining =:= 0 ->
			Centroids = []
		;	prepare_spread_candidates(Rows, First, Candidates),
			select_spread_centroids(Remaining, Candidates, Centroids)
		).

	take_vectors(0, _Rows, []) :-
		!.
	take_vectors(K, [Row| Rows], [Row| Selected]) :-
		Remaining is K - 1,
		take_vectors(Remaining, Rows, Selected).

	prepare_spread_candidates([], _Centroid, []).
	prepare_spread_candidates([Row| Rows], Centroid, [Row-Distance| Candidates]) :-
		squared_euclidean_distance(Row, Centroid, 0.0, Distance),
		prepare_spread_candidates(Rows, Centroid, Candidates).

	select_spread_centroids(Count, [Candidate-Distance| Candidates], [Vector| Centroids]) :-
		farthest_cached_candidate(Candidates, Candidate, Distance, Vector),
		(	Count =:= 1 ->
			Centroids = []
		;	remove_spread_candidate(Vector, [Candidate-Distance| Candidates], RemainingCandidates),
			update_spread_candidates(RemainingCandidates, Vector, UpdatedCandidates),
			NextCount is Count - 1,
			select_spread_centroids(NextCount, UpdatedCandidates, Centroids)
		).

	farthest_cached_candidate([], BestCandidate, _BestDistance, BestCandidate).
	farthest_cached_candidate([Candidate-Distance| Candidates], BestCandidate0, BestDistance0, BestCandidate) :-
		(	Distance > BestDistance0 ->
			BestCandidate1 = Candidate,
			BestDistance1 = Distance
		;	BestCandidate1 = BestCandidate0,
			BestDistance1 = BestDistance0
		),
		farthest_cached_candidate(Candidates, BestCandidate1, BestDistance1, BestCandidate).

	remove_spread_candidate(Vector, [Candidate-Distance| Candidates], Remaining) :-
		(	Vector == Candidate ->
			Remaining = Candidates
		;	Remaining = [Candidate-Distance| Tail],
			remove_spread_candidate(Vector, Candidates, Tail)
		).

	update_spread_candidates([], _Centroid, []).
	update_spread_candidates([Row-Previous| Candidates], Centroid, [Row-Minimum| Updated]) :-
		squared_euclidean_distance(Row, Centroid, 0.0, Distance),
		Minimum is min(Previous, Distance),
		update_spread_candidates(Candidates, Centroid, Updated).

	optimize_centroids(Rows, Options, Iteration, PreviousShift, Centroids0, Centroids, Convergence, Iterations, FinalShift) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(	Iteration >= MaximumIterations ->
			Centroids = Centroids0,
			Convergence = maximum_iterations,
			Iterations = Iteration,
			FinalShift = PreviousShift
		;	update_centroids(Rows, Centroids0, Centroids1),
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

	update_centroids(Rows, Centroids, Updated) :-
		centroid_accumulators(Centroids, 1, Pairs),
		avltree::as_dictionary(Pairs, Tree0),
		assign_rows(Rows, Centroids, Tree0, Tree),
		recompute_centroids(Centroids, Tree, 1, Updated).

	centroid_accumulators([], _Index, []).
	centroid_accumulators([_| Centroids], Index, [Index-accumulator(0, [])| Pairs]) :-
		Next is Index + 1,
		centroid_accumulators(Centroids, Next, Pairs).

	assign_rows([], _Centroids, Tree, Tree).
	assign_rows([Vector| Rows], Centroids, Tree0, Tree) :-
		nearest_centroid(Centroids, Vector, Cluster, _DistanceSquared),
		avltree::lookup(Cluster, accumulator(Count0, Sum0), Tree0),
		(	Count0 =:= 0 ->
			Sum = Vector
		;	add_vectors(Sum0, Vector, Sum)
		),
		Count is Count0 + 1,
		avltree::update(Tree0, Cluster, accumulator(Count, Sum), Tree1),
		assign_rows(Rows, Centroids, Tree1, Tree).

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

	squared_euclidean_distance([], [], DistanceSquared, DistanceSquared).
	squared_euclidean_distance([Left| Lefts], [Right| Rights], DistanceSquared0, DistanceSquared) :-
		Delta is Left - Right,
		DistanceSquared1 is DistanceSquared0 + Delta * Delta,
		squared_euclidean_distance(Lefts, Rights, DistanceSquared1, DistanceSquared).

	recompute_centroids([], _Tree, _Cluster, []).
	recompute_centroids([Centroid0| Centroids0], Tree, Cluster, [Centroid| Centroids]) :-
		avltree::lookup(Cluster, accumulator(Count, Sum), Tree),
		(	Count =:= 0 ->
			Centroid = Centroid0
		;	Scale is 1.0 / Count,
			scale_vector(Sum, Scale, Centroid)
		),
		NextCluster is Cluster + 1,
		recompute_centroids(Centroids0, Tree, NextCluster, Centroids).

	add_vectors([], [], []).
	add_vectors([Left| Lefts], [Right| Rights], [Sum| Sums]) :-
		Sum is Left + Right,
		add_vectors(Lefts, Rights, Sums).

	max_centroid_shift(Centroids0, Centroids1, Initial, Shift) :-
		InitialSquared is Initial * Initial,
		max_centroid_shift_squared(Centroids0, Centroids1, InitialSquared, Squared),
		Shift is sqrt(Squared).

	max_centroid_shift_squared([], [], Maximum, Maximum).
	max_centroid_shift_squared([Centroid0| Centroids0], [Centroid1| Centroids1], Maximum0, Maximum) :-
		squared_euclidean_distance(Centroid0, Centroid1, 0.0, DistanceSquared),
		Maximum1 is max(Maximum0, DistanceSquared),
		max_centroid_shift_squared(Centroids0, Centroids1, Maximum1, Maximum).

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
		clusterer_data(Clusterer, Encoders, _TrainingRows, _Degrees, _Components, Eigenvalues, _Projection, Centroids, ResolvedSigma, Options, Diagnostics),
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
	default_option(eigensolver_tolerance(1.0e-8)).
	default_option(eigensolver_maximum_iterations(1000)).
	default_option(eigenvalue_cutoff(1.0e-8)).
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
		integer(MaximumIterations),
		MaximumIterations > 0.
	valid_option(tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance >= 0.0.
	valid_option(eigensolver_tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance >= 0.0.
	valid_option(eigenvalue_cutoff(Cutoff)) :-
		number(Cutoff),
		Cutoff >= 0.0.
	valid_option(eigensolver_maximum_iterations(Iterations)) :-
		integer(Iterations),
		Iterations > 0.
	valid_option(initialization(Initialization)) :-
		once((Initialization == first_k; Initialization == spread)).
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).

:- end_object.
