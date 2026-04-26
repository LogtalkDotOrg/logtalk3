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


:- object(gaussian_mixture,
	imports(clusterer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-26,
		comment is 'Gaussian mixture model clusterer for continuous datasets. Learns from a dataset object implementing the ``clustering_dataset_protocol`` protocol and returns a clusterer term that can be used for assigning new instances to clusters and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses deterministic expectation-maximization with diagonal covariance matrices.',
			'Dead components' - 'Supports explicit dead-component handling policies: ``zero_weight`` preserves the previous component with zero weight, while ``reseed`` relocates the component to the least-confident training row.',
			'Feature handling' - 'Supports continuous attributes only. Continuous attributes can be standardized using z-score scaling before clustering.',
			'Initialization' - 'Supports ``first_k`` initialization and deterministic ``spread`` initialization for component means.',
			'Prediction' - 'Assigns new instances to the component with the largest posterior score and can also return per-component posterior probabilities.',
			'Clusterer representation' - 'The learned clusterer is represented by default as ``gaussian_mixture_clusterer(Encoders, Components, Weights, Options, Diagnostics)`` where ``Encoders`` stores the feature encoding metadata, ``Components`` stores the learned means and diagonal variances, ``Weights`` stores the mixture weights, ``Options`` stores the effective training options, and ``Diagnostics`` stores convergence metadata.'
		],
		see_also is [clusterer_protocol, clustering_dataset_protocol, kmeans]
	]).

	:- public(cluster_probabilities/3).
	:- mode(cluster_probabilities(+compound, +list, -list(pair)), one).
	:- info(cluster_probabilities/3, [
		comment is 'Returns posterior component probabilities for a new instance as ``Cluster-Probability`` pairs in component-id order.',
		argnames is ['Clusterer', 'Instance', 'Probabilities']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, nth1/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(population, [
		variance/2
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
		initialize_means(Initialization, K, Rows, InitialMeans),
		global_variances(Rows, InitialVariances),
		initialize_components(InitialMeans, InitialVariances, InitialComponents),
		initialize_weights(K, InitialWeights),
		optimize_em(Rows, Count, Options, 0, none, InitialComponents, InitialWeights, Components, Weights, Convergence, Iterations, AverageLogLikelihood, FinalDelta),
		build_diagnostics(Components, Count, Options, Convergence, Iterations, AverageLogLikelihood, FinalDelta, Diagnostics),
		Clusterer = gaussian_mixture_clusterer(Encoders, Components, Weights, Options, Diagnostics),
		!.

	cluster(Clusterer, Instance, Cluster) :-
		clusterer_data(Clusterer, Encoders, Components, Weights, _Options, _Diagnostics),
		^^encode_instance(Encoders, Instance, Features),
		best_component(Features, Components, Weights, Cluster).

	cluster_probabilities(Clusterer, Instance, Probabilities) :-
		clusterer_data(Clusterer, Encoders, Components, Weights, _Options, _Diagnostics),
		^^encode_instance(Encoders, Instance, Features),
		row_responsibilities(Features, Components, Weights, Responsibilities, _LogLikelihood),
		responsibility_probabilities(Responsibilities, 1, Probabilities).

	clusterer_diagnostics_data(Clusterer, Diagnostics) :-
		clusterer_data(Clusterer, _Encoders, _Components, _Weights, _Options, Diagnostics).

	initialize_means(first_k, K, Rows, Means) :-
		^^take_first_k(K, Rows, Means).
	initialize_means(spread, K, Rows, [First| Means]) :-
		canonical_spread_candidate(Rows, FirstCandidate, Candidates),
		FirstCandidate = _-First,
		Remaining is K - 1,
		select_spread_means(Remaining, Candidates, [First], Means).

	take_first_k(0, _, []).
	take_first_k(K, [_-Vector| Rows], [Vector| Means]) :-
		K > 0,
		NextK is K - 1,
		take_first_k(NextK, Rows, Means).

	select_spread_means(0, _, _, []) :-
		!.
	select_spread_means(Count, Candidates, Selected, [Vector| Means]) :-
		Count > 0,
		farthest_candidate(Candidates, Selected, BestCandidate, RemainingCandidates),
		BestCandidate = _-Vector,
		NextCount is Count - 1,
		select_spread_means(NextCount, RemainingCandidates, [Vector| Selected], Means).

	canonical_spread_candidate([Candidate| Candidates], BestCandidate, RemainingCandidates) :-
		canonical_spread_candidate(Candidates, Candidate, BestCandidate),
		remove_candidate(BestCandidate, [Candidate| Candidates], RemainingCandidates).

	canonical_spread_candidate([], BestCandidate, BestCandidate).
	canonical_spread_candidate([Candidate| Candidates], BestCandidate0, BestCandidate) :-
		(   preferred_spread_candidate(Candidate, BestCandidate0) ->
			BestCandidate1 = Candidate
		;   BestCandidate1 = BestCandidate0
		),
		canonical_spread_candidate(Candidates, BestCandidate1, BestCandidate).

	farthest_candidate([Candidate| Candidates], Selected, BestCandidate, RemainingCandidates) :-
		Candidate = _-Vector,
		closest_mean_distance_squared(Vector, Selected, Distance),
		farthest_candidate(Candidates, Selected, Candidate, Distance, BestCandidate),
		remove_candidate(BestCandidate, [Candidate| Candidates], RemainingCandidates).

	farthest_candidate([], _Selected, BestCandidate, _BestDistance, BestCandidate).
	farthest_candidate([Candidate| Candidates], Selected, BestCandidate0, BestDistance0, BestCandidate) :-
		Candidate = _-Vector,
		closest_mean_distance_squared(Vector, Selected, Distance),
		(   Distance > BestDistance0 ->
			BestCandidate1 = Candidate,
			BestDistance1 = Distance
		;   Distance =:= BestDistance0,
			preferred_spread_candidate(Candidate, BestCandidate0) ->
			BestCandidate1 = Candidate,
			BestDistance1 = Distance
		;   BestCandidate1 = BestCandidate0,
			BestDistance1 = BestDistance0
		),
		farthest_candidate(Candidates, Selected, BestCandidate1, BestDistance1, BestCandidate).

	preferred_spread_candidate(Id1-Vector1, Id2-Vector2) :-
		(   Vector1 @< Vector2 ->
			true
		;   Vector1 == Vector2,
			Id1 < Id2
		).

	closest_mean_distance_squared(Vector, [Mean| Means], DistanceSquared) :-
		squared_distance(Vector, Mean, 0.0, InitialDistance),
		closest_mean_distance_squared(Means, Vector, InitialDistance, DistanceSquared).

	closest_mean_distance_squared([], _Vector, BestDistance, BestDistance).
	closest_mean_distance_squared([Mean| Means], Vector, BestDistance0, BestDistance) :-
		squared_distance(Vector, Mean, 0.0, Distance),
		(   Distance < BestDistance0 ->
			BestDistance1 = Distance
		;   BestDistance1 = BestDistance0
		),
		closest_mean_distance_squared(Means, Vector, BestDistance1, BestDistance).

	remove_candidate(Id-Vector, [Id-Vector| Candidates], Candidates) :-
		!.
	remove_candidate(BestCandidate, [Candidate| Candidates], [Candidate| RemainingCandidates]) :-
		^^remove_candidate(BestCandidate, Candidates, RemainingCandidates).

	global_variances([_-Features| Rows], Variances) :-
		transpose_rows([Features| MoreFeatures], Columns),
		member_rows_features(Rows, MoreFeatures),
		column_variances(Columns, Variances).

	member_rows_features([], []).
	member_rows_features([_-Features| Rows], [Features| MoreFeatures]) :-
		member_rows_features(Rows, MoreFeatures).

	transpose_rows([], []).
	transpose_rows([[]| _], []).
	transpose_rows(Rows, [Column| Columns]) :-
		pluck_column(Rows, Column, RestRows),
		transpose_rows(RestRows, Columns).

	pluck_column([], [], []).
	pluck_column([[Value| Values]| Rows], [Value| Column], [Values| RestRows]) :-
		pluck_column(Rows, Column, RestRows).

	column_variances([], []).
	column_variances([Column| Columns], [VarianceValue| Variances]) :-
		length(Column, Count),
		(   Count > 1 ->
			variance(Column, Variance0)
		;   Variance0 = 1.0
		),
		(   Variance0 > 0.0 ->
			VarianceValue = Variance0
		;   VarianceValue = 1.0
		),
		column_variances(Columns, Variances).

	initialize_components([], _Variances, []).
	initialize_components([Mean| Means], Variances, [component(Mean, Variances)| Components]) :-
		initialize_components(Means, Variances, Components).

	initialize_weights(K, Weights) :-
		Weight is 1.0 / K,
		initialize_weights(K, Weight, Weights).

	initialize_weights(0, _Weight, []).
	initialize_weights(K, Weight, [Weight| Weights]) :-
		K > 0,
		NextK is K - 1,
		initialize_weights(NextK, Weight, Weights).

	optimize_em(Rows, Count, Options, Iteration, PreviousAverageLogLikelihood, Components0, Weights0, Components, Weights, Convergence, Iterations, AverageLogLikelihood, FinalDelta) :-
		expectation(Rows, Components0, Weights0, Responsibilities, 0.0, TotalLogLikelihood),
		CurrentAverageLogLikelihood is TotalLogLikelihood / Count,
		average_log_likelihood_delta(PreviousAverageLogLikelihood, CurrentAverageLogLikelihood, CurrentDelta),
		(   PreviousAverageLogLikelihood \== none,
			^^option(tolerance(Tolerance), Options),
			CurrentDelta =< Tolerance ->
			Components = Components0,
			Weights = Weights0,
			Convergence = tolerance,
			Iterations = Iteration,
			AverageLogLikelihood = CurrentAverageLogLikelihood,
			FinalDelta = CurrentDelta
		;   ^^option(maximum_iterations(MaximumIterations), Options),
			Iteration >= MaximumIterations ->
			Components = Components0,
			Weights = Weights0,
			Convergence = maximum_iterations,
			Iterations = Iteration,
			AverageLogLikelihood = CurrentAverageLogLikelihood,
			FinalDelta = CurrentDelta
		;   maximization(Rows, Responsibilities, Components0, Options, Components1, Weights1),
			NextIteration is Iteration + 1,
			optimize_em(Rows, Count, Options, NextIteration, CurrentAverageLogLikelihood, Components1, Weights1, Components, Weights, Convergence, Iterations, AverageLogLikelihood, FinalDelta)
		).

	average_log_likelihood_delta(none, _AverageLogLikelihood, 0.0) :-
		!.
	average_log_likelihood_delta(PreviousAverageLogLikelihood, AverageLogLikelihood, Delta) :-
		Delta is abs(AverageLogLikelihood - PreviousAverageLogLikelihood).

	expectation([], _Components, _Weights, [], LogLikelihood, LogLikelihood).
	expectation([_-Features| Rows], Components, Weights, [Responsibilities| ResponsibilitiesRows], LogLikelihood0, LogLikelihood) :-
		row_responsibilities(Features, Components, Weights, Responsibilities, RowLogLikelihood),
		LogLikelihood1 is LogLikelihood0 + RowLogLikelihood,
		expectation(Rows, Components, Weights, ResponsibilitiesRows, LogLikelihood1, LogLikelihood).

	row_responsibilities(Features, Components, Weights, Responsibilities, LogLikelihood) :-
		row_log_scores_summary(Features, Components, Weights, LogScores, LogLikelihood, _BestCluster),
		normalize_log_scores(LogScores, LogLikelihood, Responsibilities).

	row_log_scores_summary(Features, [Component| Components], [Weight| Weights], [FirstLogScore| LogScores], LogLikelihood, BestCluster) :-
		component_log_score(Features, Component, Weight, FirstLogScore),
		row_log_scores_summary(Features, Components, Weights, 2, FirstLogScore, 1, FirstLogScore, LogScores, LogLikelihood, BestCluster).

	row_log_scores_summary(_Features, [], [], _Index, _BestLogScore, BestCluster, LogLikelihood, [], LogLikelihood, BestCluster) :-
		!.
	row_log_scores_summary(Features, [Component| Components], [Weight| Weights], Index, BestLogScore0, BestCluster0, LogSumExp0, [LogScore| LogScores], LogLikelihood, BestCluster) :-
		component_log_score(Features, Component, Weight, LogScore),
		update_best_component(Index, LogScore, BestLogScore0, BestCluster0, BestLogScore1, BestCluster1),
		log_sum_exp(LogSumExp0, LogScore, LogSumExp1),
		NextIndex is Index + 1,
		row_log_scores_summary(Features, Components, Weights, NextIndex, BestLogScore1, BestCluster1, LogSumExp1, LogScores, LogLikelihood, BestCluster).

	row_log_score_summary(Features, [Component| Components], [Weight| Weights], LogLikelihood, BestCluster) :-
		component_log_score(Features, Component, Weight, FirstLogScore),
		row_log_score_summary(Features, Components, Weights, 2, FirstLogScore, 1, FirstLogScore, LogLikelihood, BestCluster).

	row_log_score_summary(_Features, [], [], _Index, _BestLogScore, BestCluster, LogLikelihood, LogLikelihood, BestCluster) :-
		!.
	row_log_score_summary(Features, [Component| Components], [Weight| Weights], Index, BestLogScore0, BestCluster0, LogSumExp0, LogLikelihood, BestCluster) :-
		component_log_score(Features, Component, Weight, LogScore),
		update_best_component(Index, LogScore, BestLogScore0, BestCluster0, BestLogScore1, BestCluster1),
		log_sum_exp(LogSumExp0, LogScore, LogSumExp1),
		NextIndex is Index + 1,
		row_log_score_summary(Features, Components, Weights, NextIndex, BestLogScore1, BestCluster1, LogSumExp1, LogLikelihood, BestCluster).

	update_best_component(Index, LogScore, BestLogScore0, _BestCluster0, LogScore, Index) :-
		LogScore > BestLogScore0,
		!.
	update_best_component(_Index, _LogScore, BestLogScore, BestCluster, BestLogScore, BestCluster).

	log_sum_exp(LogSumExp0, LogScore, LogSumExp) :-
		(   LogScore > LogSumExp0 ->
			LogSumExp is LogScore + log(1.0 + exp(LogSumExp0 - LogScore))
		;   LogSumExp is LogSumExp0 + log(1.0 + exp(LogScore - LogSumExp0))
		).

	normalize_log_scores([], _LogLikelihood, []).
	normalize_log_scores([LogScore| LogScores], LogLikelihood, [Responsibility| Responsibilities]) :-
		Responsibility is exp(LogScore - LogLikelihood),
		normalize_log_scores(LogScores, LogLikelihood, Responsibilities).

	responsibility_probabilities([], _Index, []).
	responsibility_probabilities([Responsibility| Responsibilities], Index, [Index-Responsibility| Probabilities]) :-
		NextIndex is Index + 1,
		responsibility_probabilities(Responsibilities, NextIndex, Probabilities).

	component_log_score(_Features, _Component, Weight, LogScore) :-
		Weight =< 0.0,
		!,
		LogScore = -1.0e300.
	component_log_score(Features, component(Mean, Variances), Weight, LogScore) :-
		gaussian_log_density(Features, Mean, Variances, 0.0, LogDensity),
		LogScore is log(Weight) + LogDensity.

	gaussian_log_density([], [], [], LogDensity, LogDensity).
	gaussian_log_density([Value| Values], [Mean| Means], [Variance| Variances], LogDensity0, LogDensity) :-
		Delta is Value - Mean,
		LogDensity1 is LogDensity0 - 0.5 * log(6.283185307179586 * Variance) - (Delta * Delta) / (2.0 * Variance),
		gaussian_log_density(Values, Means, Variances, LogDensity1, LogDensity).

	maximization(Rows, ResponsibilitiesRows, Components0, Options, Components, Weights) :-
		length(Components0, K),
		update_components(1, K, Rows, ResponsibilitiesRows, Components0, Options, Components, RawWeights),
		normalize_weights(RawWeights, Weights).

	update_components(Index, K, _Rows, _ResponsibilitiesRows, _Components0, _Options, [], []) :-
		Index > K,
		!.
	update_components(Index, K, Rows, ResponsibilitiesRows, Components0, Options, [Component| Components], [Weight| Weights]) :-
		nth1(Index, Components0, Component0),
		update_component(Index, Rows, ResponsibilitiesRows, Component0, Options, Component, Weight),
		NextIndex is Index + 1,
		update_components(NextIndex, K, Rows, ResponsibilitiesRows, Components0, Options, Components, Weights).

	update_component(Index, Rows, ResponsibilitiesRows, Component0, Options, Component, Weight) :-
		component_statistics(Rows, Index, ResponsibilitiesRows, ResponsibilitySum, WeightedSum, WeightedSquares),
		(   ResponsibilitySum =< 1.0e-9 ->
			dead_component_update(Rows, ResponsibilitiesRows, Component0, Options, Component, Weight)
		;   divide_vector(WeightedSum, ResponsibilitySum, Mean),
			length(Rows, Count),
			divide_vector(WeightedSquares, ResponsibilitySum, SecondMoments),
			variance_from_moments(SecondMoments, Mean, Options, Variances),
			Component = component(Mean, Variances),
			Weight is ResponsibilitySum / Count
		).

	dead_component_update(Rows, ResponsibilitiesRows, Component0, Options, Component, Weight) :-
		^^option(dead_component_policy(Policy), Options),
		dead_component_update(Policy, Rows, ResponsibilitiesRows, Component0, Options, Component, Weight).

	dead_component_update(zero_weight, _Rows, _ResponsibilitiesRows, Component0, _Options, Component, Weight) :-
		Component = Component0,
		Weight = 0.0.
	dead_component_update(reseed, Rows, ResponsibilitiesRows, component(_PreviousMean, Variances), _Options, Component, Weight) :-
		reseed_component_mean(Rows, ResponsibilitiesRows, Mean),
		length(Rows, Count),
		Component = component(Mean, Variances),
		Weight is 1.0 / Count.

	reseed_component_mean([_-Features| Rows], [Responsibilities| ResponsibilitiesRows], Mean) :-
		maximum_responsibility(Responsibilities, LowestConfidence0),
		reseed_component_mean(Rows, ResponsibilitiesRows, Features, LowestConfidence0, Mean).

	reseed_component_mean([], [], Mean, _LowestConfidence, Mean).
	reseed_component_mean([_-Features| Rows], [Responsibilities| ResponsibilitiesRows], BestFeatures0, LowestConfidence0, Mean) :-
		maximum_responsibility(Responsibilities, Confidence),
		(   Confidence < LowestConfidence0 ->
			BestFeatures1 = Features,
			LowestConfidence1 = Confidence
		;   BestFeatures1 = BestFeatures0,
			LowestConfidence1 = LowestConfidence0
		),
		reseed_component_mean(Rows, ResponsibilitiesRows, BestFeatures1, LowestConfidence1, Mean).

	maximum_responsibility([Responsibility| Responsibilities], Maximum) :-
		maximum_responsibility(Responsibilities, Responsibility, Maximum).

	maximum_responsibility([], Maximum, Maximum).
	maximum_responsibility([Responsibility| Responsibilities], CurrentMaximum, Maximum) :-
		(   Responsibility > CurrentMaximum ->
			NextMaximum = Responsibility
		;   NextMaximum = CurrentMaximum
		),
		maximum_responsibility(Responsibilities, NextMaximum, Maximum).

	component_statistics(Rows, Index, ResponsibilitiesRows, ResponsibilitySum, WeightedSum, WeightedSquares) :-
		component_statistics(Rows, Index, ResponsibilitiesRows, 0.0, ResponsibilitySum, [], WeightedSum, [], WeightedSquares).

	component_statistics([], _Index, [], ResponsibilitySum, ResponsibilitySum, WeightedSum, WeightedSum, WeightedSquares, WeightedSquares).
	component_statistics([_-Features| Rows], Index, [Responsibilities| ResponsibilitiesRows], ResponsibilitySum0, ResponsibilitySum, WeightedSum0, WeightedSum, WeightedSquares0, WeightedSquares) :-
		nth1(Index, Responsibilities, Responsibility),
		scale_vector(Features, Responsibility, RowWeightedFeatures),
		scale_squares(Features, Responsibility, RowWeightedSquares),
		ResponsibilitySum1 is ResponsibilitySum0 + Responsibility,
		add_optional_vectors(RowWeightedFeatures, WeightedSum0, WeightedSum1),
		add_optional_vectors(RowWeightedSquares, WeightedSquares0, WeightedSquares1),
		component_statistics(Rows, Index, ResponsibilitiesRows, ResponsibilitySum1, ResponsibilitySum, WeightedSum1, WeightedSum, WeightedSquares1, WeightedSquares).

	scale_squares([], _Responsibility, []).
	scale_squares([Value| Values], Responsibility, [WeightedSquare| WeightedSquares]) :-
		WeightedSquare is Responsibility * Value * Value,
		scale_squares(Values, Responsibility, WeightedSquares).

	scale_vector([], _Responsibility, []).
	scale_vector([Value| Values], Responsibility, [WeightedValue| WeightedValues]) :-
		WeightedValue is Responsibility * Value,
		scale_vector(Values, Responsibility, WeightedValues).

	add_optional_vectors(Vector, [], Vector) :-
		!.
	add_optional_vectors([], Vector, Vector) :-
		!.
	add_optional_vectors([Left| Lefts], [Right| Rights], [Sum| Sums]) :-
		Sum is Left + Right,
		add_optional_vectors(Lefts, Rights, Sums).

	divide_vector([], _Denominator, []).
	divide_vector([Value| Values], Denominator, [Quotient| Quotients]) :-
		Quotient is Value / Denominator,
		divide_vector(Values, Denominator, Quotients).

	build_diagnostics(Components, Count, Options, Convergence, Iterations, AverageLogLikelihood, FinalDelta, Diagnostics) :-
		length(Components, ComponentCount),
		Diagnostics = [
			model(gaussian_mixture),
			components(ComponentCount),
			training_examples(Count),
			convergence(Convergence),
			iterations(Iterations),
			average_log_likelihood(AverageLogLikelihood),
			final_delta(FinalDelta),
			options(Options)
		].

	clusterer_data(Clusterer, Encoders, Components, Weights, Options, Diagnostics) :-
		Clusterer =.. [_, Encoders, Components, Weights, Options, Diagnostics].

	variance_from_moments([], [], _Options, []).
	variance_from_moments([SecondMoment| SecondMoments], [Mean| Means], Options, [Variance| Variances]) :-
		Variance0 is SecondMoment - Mean * Mean,
		^^option(covariance_regularization(Regularization), Options),
		(   Variance0 > 0.0 ->
			Variance is Variance0 + Regularization
		;   Variance is Regularization
		),
		variance_from_moments(SecondMoments, Means, Options, Variances).

	normalize_weights(Weights0, Weights) :-
		sum_weights(Weights0, 0.0, Total),
		divide_vector(Weights0, Total, Weights).

	sum_weights([], Total, Total).
	sum_weights([Weight| Weights], Total0, Total) :-
		Total1 is Total0 + Weight,
		sum_weights(Weights, Total1, Total).

	best_component(Features, Components, Weights, Cluster) :-
		row_log_score_summary(Features, Components, Weights, _LogLikelihood, Cluster).

	squared_distance([], [], DistanceSquared, DistanceSquared).
	squared_distance([Value1| Values1], [Value2| Values2], DistanceSquared0, DistanceSquared) :-
		Delta is Value1 - Value2,
		DistanceSquared01 is DistanceSquared0 + Delta * Delta,
		squared_distance(Values1, Values2, DistanceSquared01, DistanceSquared).

	print_clusterer(Clusterer) :-
		clusterer_data(Clusterer, Encoders, Components, Weights, Options, Diagnostics),
		format('Gaussian Mixture Clusterer~n', []),
		format('==========================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Diagnostics: ~w~n~n', [Diagnostics]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nComponents:~n', []),
		print_components(Components, Weights, 1).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		format('  ~w (continuous, mean=~4f, scale=~4f)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).

	print_components([], [], _Index).
	print_components([component(Mean, Variances)| Components], [Weight| Weights], Index) :-
		format('  component ~d: weight=~4f, mean=~w, variances=~w~n', [Index, Weight, Mean, Variances]),
		NextIndex is Index + 1,
		print_components(Components, Weights, NextIndex).

	default_option(k(2)).
	default_option(initialization(spread)).
	default_option(feature_scaling(on)).
	default_option(maximum_iterations(100)).
	default_option(tolerance(0.0001)).
	default_option(covariance_regularization(0.001)).
	default_option(dead_component_policy(zero_weight)).

	valid_option(k(K)) :-
		valid(positive_integer, K).
	valid_option(initialization(Initialization)) :-
		once((Initialization == first_k; Initialization == spread)).
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		valid(positive_number, Tolerance).
	valid_option(covariance_regularization(Regularization)) :-
		valid(positive_number, Regularization).
	valid_option(dead_component_policy(Policy)) :-
		once((Policy == zero_weight; Policy == reseed)).

:- end_object.
